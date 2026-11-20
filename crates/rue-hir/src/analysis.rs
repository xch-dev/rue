//! Finds whether evaluating a value may fail, and whether a reference to a value is always
//! evaluated, so that a value that may fail is never dropped, or moved somewhere that's only
//! evaluated some of the time.
//!
//! The same checks are used while lowering, where they depend on what's inlined, and by the
//! dependency graph and lints, which only know what's written in the source (see
//! [`FailureAnalysis`]).
//!
//! # What this guarantees, and what it trusts
//!
//! Automatic inlining, and evaluating unused bindings (and the values of type guards with nothing
//! to check at runtime) for their effect, never skip a check that a build without them would run,
//! as long as every value has the shape its type says it has. A value has the wrong shape if it's
//! an atom where its type says a pair, or a pair where its type says an atom, at any depth. The
//! compiler can't check that for these values, so it trusts it:
//!
//! - The solution, which must match the parameter types of `main`, including every element of a
//!   list and every field of a struct.
//! - The result of calling a function value that comes from the solution (or from anywhere else
//!   the compiler can't see the body of), which must match the function type's return type. The
//!   call itself is treated as able to fail, but what it returns isn't checked.
//! - The result of an `extern fn`, which must match its declared return type. Calling it is
//!   treated as able to fail.
//! - The result of `unchecked_cast`, which must really have the type it's cast to.
//!
//! Everything else follows from type checking: `as` can't change a value's shape, a type guard
//! checks enough of the shape to rule out every other type its value could have (so a guard from
//! `Any` checks the whole shape), and `==` only compiles for atoms. Given those shapes,
//! the operations treated as unable to fail (reading a field, addition and subtraction,
//! comparisons, `==`, hashing, `concat`, `strlen`, the bitwise and logical operators, shifts by
//! suitable constants, and multiplication and division of suitable constants) can't fail in CLVM.
//! Multiplication and division of values that aren't known at compile time can fail, since the
//! `LIMITS` flag that mainnet enforces rejects operands that are too long (see
//! [`rue_lir::CLVM_FLAGS`]). Nothing relies on narrower facts about atoms, such as a
//! `Bytes32` having 32 bytes, a `Bool` being 0 or 1, or a `PublicKey` being a valid point, since
//! the operations that need those (`coinid`, the BLS and secp operations, and so on) are treated
//! as able to fail.
//!
//! If one of the trusted values has the wrong shape, a release build can return a value where a
//! debug build fails, such as when reading a field of it was moved into a branch that isn't taken.
//! Checking solutions against `main`'s types is part 2 of
//! [#142](https://github.com/xch-dev/rue/issues/142).
//!
//! This is only about where the compiler decides to evaluate values. Checks can still be skipped,
//! in debug and release alike, where the language says a value isn't evaluated: an argument of an
//! `inline fn` whose parameter isn't referenced (or an `inline let` or `inline const` that isn't),
//! which the `inline_value_may_fail` lint reports; and a constant that's never referenced. A type
//! guard with nothing to check at runtime is a constant, but its value is evaluated for its effect
//! if it may fail, by the same rule as a binding that's never referenced, so a parameter of an
//! `inline fn` that's only the value of such a guard counts as not referenced. `debug` statements
//! are only evaluated in debug builds. Running out of cost isn't counted as failing, since
//! evaluating a value in a different place changes how much the program costs.

mod evaluation;
mod may_fail;

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::{
    Database, Hir, HirId, Statement, Symbol, SymbolId, reachable_body, reachable_statements,
};

pub(crate) use evaluation::*;
pub(crate) use may_fail::*;

/// What the checks need to know about how the HIR is lowered.
pub(crate) trait AnalysisContext {
    fn db(&self) -> &Database;

    /// The value that's evaluated where the symbol is referenced, if it's substituted there rather
    /// than read from the environment.
    fn substituted_value(&self, symbol: SymbolId) -> Option<HirId>;

    /// Whether the value of the binding, which isn't `inline`, may be evaluated, either at its
    /// let statement or where it's referenced. This is used to check whether the value may fail,
    /// so it can include bindings whose values can't fail.
    fn is_evaluated(&self, binding: SymbolId) -> bool;

    /// Whether the value of the binding, which isn't `inline`, is evaluated whenever its let
    /// statement is, given that it references a value that may fail. The statements and body are
    /// the rest of the block after the let statement.
    fn let_evaluates(&self, binding: SymbolId, rest: &[Statement], body: Option<HirId>) -> bool;

    /// Whether the value of the type guard, whose check doesn't use it, may be evaluated for its
    /// effect. This is used to check whether the guard may fail, so it can include guards whose
    /// values can't fail.
    fn is_guard_evaluated(&self, guard: HirId) -> bool;

    /// Whether the value of the type guard, whose check doesn't use it, is evaluated for its
    /// effect, given that it references a value that may fail.
    fn guard_evaluates(&self, guard: HirId, value: HirId) -> bool;

    /// Whether creating a closure for the function, or calling it directly, evaluates an argument
    /// of an inlined function that may fail, since the arguments it captures are evaluated then.
    fn captures_may_fail(&self, function: SymbolId) -> bool;

    /// Whether calling the function value of the parameter may fail, where it isn't substituted,
    /// given what's passed for it wherever its function is called.
    fn parameter_values_may_fail(&self, parameter: SymbolId) -> bool;

    /// Whether the body of the `inline fn` may reference the parameter, which is the only way its
    /// argument is evaluated.
    fn is_parameter_used(&self, function: SymbolId, parameter: SymbolId) -> bool;

    /// Whether creating a closure for the function evaluates the parameter, which is the case for
    /// an argument of an inlined function that the closure captures by value.
    fn captures(&self, function: SymbolId, parameter: SymbolId) -> bool;

    /// Whether calling each function may fail, which is shared between checks.
    fn function_failures(&self) -> &RefCell<HashMap<SymbolId, FunctionFailure>>;

    /// Whether the value substituted for each symbol may fail, inside of a function body or not,
    /// for the values whose result doesn't depend on where they're referenced, which is shared
    /// between checks.
    fn substitution_failures(&self) -> &RefCell<HashMap<(SymbolId, bool), bool>>;

    /// Whether the result for the value substituted for the symbol can be shared between checks.
    fn shares_substitution(&self, symbol: SymbolId, fails: bool) -> bool;
}

/// The checks for code as it's written, without knowing what's inlined automatically.
///
/// An `inline` binding or constant is evaluated where it's referenced. Any other binding whose
/// value may fail is always evaluated when its let statement is reached, so a reference to it
/// can't fail. A constant is checked where it's referenced, since it's only evaluated if it's
/// referenced. The parameters of a function can't fail either, since a check in an argument is
/// kept at the call.
///
/// Like the checks used while lowering, this trusts that values have the shapes their types say
/// (atoms where atoms are expected, and pairs where pairs are) wherever the compiler can't check
/// it: the solution passed to `main`, the results of calling a function value from the solution
/// or an `extern fn`, and the results of `unchecked_cast`. Reading a field of one of these, or
/// doing arithmetic on it, is treated as unable to fail. If a value breaks that trust, a check
/// can be moved or skipped.
#[derive(Debug, Default, Clone)]
pub struct FailureAnalysis {
    function_failures: RefCell<HashMap<SymbolId, FunctionFailure>>,
    substitution_failures: RefCell<HashMap<(SymbolId, bool), bool>>,
    let_evaluations: RefCell<HashMap<SymbolId, bool>>,
    body_uses: RefCell<HashMap<SymbolId, Rc<HashSet<SymbolId>>>>,
    /// The constants that are known to be evaluated whenever the program runs, so that reading
    /// them again can't fail.
    evaluated_constants: HashSet<SymbolId>,
}

impl FailureAnalysis {
    pub fn new() -> Self {
        Self::default()
    }

    /// An analysis that treats reading any of the constants as unable to fail, since they're
    /// evaluated whenever the program runs anyway.
    pub(crate) fn with_evaluated_constants(evaluated_constants: HashSet<SymbolId>) -> Self {
        Self {
            evaluated_constants,
            ..Self::default()
        }
    }

    /// Whether evaluating the HIR may fail for values that have the shapes their types say.
    pub fn may_fail(&self, db: &Database, hir: HirId) -> bool {
        may_fail(&self.context(db), hir)
    }

    /// Whether evaluating the statements and body always evaluates a reference to the symbol,
    /// unless it fails first.
    pub fn always_evaluated_in_block(
        &self,
        db: &Database,
        target: SymbolId,
        statements: &[Statement],
        body: Option<HirId>,
    ) -> bool {
        Evaluation::new(&self.context(db), target).block(statements, body)
    }

    /// Whether evaluating the HIR always evaluates a reference to the symbol, unless it fails
    /// first.
    pub fn always_evaluated(&self, db: &Database, target: SymbolId, hir: HirId) -> bool {
        Evaluation::new(&self.context(db), target).hir(hir)
    }

    /// Whether the symbol is a binding that may be evaluated for its effect even if nothing
    /// references it, because it isn't `inline` and its value may fail. A binding whose value only
    /// fails by calling the function values of parameters counts, so this is conservative.
    pub fn may_be_effect(&self, db: &Database, symbol: SymbolId) -> bool {
        !matches!(self.effect(db, symbol), Effect::Never)
    }

    /// Whether the binding is evaluated for its effect if nothing references it. This is the one
    /// rule that the dependency graph, the lints and the recursion check all decide effects by.
    pub(crate) fn effect(&self, db: &Database, symbol: SymbolId) -> Effect {
        match db.symbol(symbol) {
            Symbol::Binding(binding) if !binding.inline => self.value_effect(db, binding.value.hir),
            _ => Effect::Never,
        }
    }

    /// Whether a value that isn't referenced, such as that of a type guard whose check doesn't use
    /// it, is evaluated for its effect, by the same rule as [`Self::effect`].
    pub(crate) fn value_effect(&self, db: &Database, hir: HirId) -> Effect {
        effect(&self.context(db), hir)
    }

    /// Whether calling the function value with arguments that aren't known may fail.
    pub(crate) fn value_call_may_fail(&self, db: &Database, hir: HirId) -> bool {
        value_call_may_fail(&self.context(db), hir)
    }

    fn context<'a>(&'a self, db: &'a Database) -> WrittenContext<'a> {
        WrittenContext { db, analysis: self }
    }
}

struct WrittenContext<'a> {
    db: &'a Database,
    analysis: &'a FailureAnalysis,
}

impl WrittenContext<'_> {
    /// Whether a binding that's never referenced is evaluated for its effect. One that only
    /// fails by calling the value of a parameter depends on what the calls of the function pass,
    /// so it's treated as not evaluated.
    fn evaluated_unreferenced(&self, value: HirId) -> bool {
        matches!(effect(self, value), Effect::Always)
    }

    /// The symbols that evaluating the statements and body may reference, deciding along the way
    /// which of the bindings they declare are evaluated (see [`Uses`]).
    fn block_uses(&self, statements: &[Statement], body: Option<HirId>) -> HashSet<SymbolId> {
        let mut uses = Uses::new(self);
        uses.block(statements, body);
        uses.symbols
    }
}

impl WrittenContext<'_> {
    /// The symbols that the body of the function references where it's evaluated.
    fn body_uses(&self, function: SymbolId) -> Option<Rc<HashSet<SymbolId>>> {
        if let Some(uses) = self.analysis.body_uses.borrow().get(&function) {
            return Some(uses.clone());
        }

        let Symbol::Function(symbol) = self.db.symbol(function) else {
            return None;
        };

        // An `inline fn` that calls itself is an error, but it's checked before that's reported,
        // so every parameter counts as used while its body is walked
        self.analysis.body_uses.borrow_mut().insert(
            function,
            Rc::new(symbol.parameters.iter().copied().collect()),
        );

        let mut uses = Uses::new(self);
        uses.hir(symbol.body);
        let uses = Rc::new(uses.symbols);

        self.analysis
            .body_uses
            .borrow_mut()
            .insert(function, uses.clone());

        Some(uses)
    }
}

impl AnalysisContext for WrittenContext<'_> {
    fn db(&self) -> &Database {
        self.db
    }

    fn substituted_value(&self, symbol: SymbolId) -> Option<HirId> {
        match self.db.symbol(symbol) {
            Symbol::Constant(_) if self.analysis.evaluated_constants.contains(&symbol) => None,
            Symbol::Constant(constant) => Some(constant.value.hir),
            symbol => symbol.inline_value(),
        }
    }

    // A binding that's referenced is evaluated, and one that isn't is evaluated for its effect if
    // it may fail, so it's evaluated whenever that matters
    fn is_evaluated(&self, _binding: SymbolId) -> bool {
        true
    }

    // The dependency graph only evaluates a binding that's never referenced if it may fail, which
    // it might not as written, such as one that only references an argument of an `inline fn`
    fn let_evaluates(&self, binding: SymbolId, rest: &[Statement], body: Option<HirId>) -> bool {
        if let Some(&evaluated) = self.analysis.let_evaluations.borrow().get(&binding) {
            return evaluated;
        }

        let Symbol::Binding(symbol) = self.db.symbol(binding) else {
            return false;
        };

        // This also decides the bindings in the rest of the block, so each block is only walked
        // once
        let evaluated = self.evaluated_unreferenced(symbol.value.hir)
            || self.block_uses(rest, body).contains(&binding);

        self.analysis
            .let_evaluations
            .borrow_mut()
            .insert(binding, evaluated);
        evaluated
    }

    fn captures_may_fail(&self, _function: SymbolId) -> bool {
        false
    }

    // The calls of a function aren't known from the code around one value
    fn parameter_values_may_fail(&self, _parameter: SymbolId) -> bool {
        true
    }

    // A guard's value is evaluated for its effect if it may fail, so it's evaluated whenever that
    // matters
    fn is_guard_evaluated(&self, _guard: HirId) -> bool {
        true
    }

    // The dependency graph only evaluates a guard's value if it may fail as written, the same
    // way as a binding that's never referenced
    fn guard_evaluates(&self, _guard: HirId, value: HirId) -> bool {
        self.evaluated_unreferenced(value)
    }

    fn is_parameter_used(&self, function: SymbolId, parameter: SymbolId) -> bool {
        self.body_uses(function)
            .is_none_or(|uses| uses.contains(&parameter))
    }

    fn captures(&self, function: SymbolId, parameter: SymbolId) -> bool {
        self.body_uses(function)
            .is_some_and(|uses| uses.contains(&parameter))
    }

    fn function_failures(&self) -> &RefCell<HashMap<SymbolId, FunctionFailure>> {
        &self.analysis.function_failures
    }

    fn substitution_failures(&self) -> &RefCell<HashMap<(SymbolId, bool), bool>> {
        &self.analysis.substitution_failures
    }

    // What's substituted as written never changes
    fn shares_substitution(&self, _symbol: SymbolId, _fails: bool) -> bool {
        true
    }
}

/// Finds the symbols that evaluating some HIR may reference, including in the lambdas it creates
/// and the values of the inline bindings and constants it references, which are evaluated there.
/// Other bindings are evaluated where they're defined, so a closure captures their values instead,
/// and only the values of the ones that are evaluated count.
///
/// The statements of a block are walked from the end, so that whether each binding is referenced
/// by what's evaluated after it is known when its let statement is reached. That's cached for
/// [`WrittenContext::let_evaluates`].
struct Uses<'a, 'c> {
    cx: &'c WrittenContext<'a>,
    symbols: HashSet<SymbolId>,
    visited: HashSet<SymbolId>,
}

impl<'a, 'c> Uses<'a, 'c> {
    fn new(cx: &'c WrittenContext<'a>) -> Self {
        Self {
            cx,
            symbols: HashSet::new(),
            visited: HashSet::new(),
        }
    }

    fn hir(&mut self, hir: HirId) {
        // This follows nested lambdas and inline values, which can be as deep as the program
        rue_types::grow_stack(|| self.hir_inner(hir));
    }

    fn hir_inner(&mut self, hir: HirId) {
        let db = self.cx.db;
        let node = db.hir(hir);

        match node {
            Hir::Reference(symbol) => {
                self.symbols.insert(*symbol);

                if let Some(value) = db.symbol(*symbol).inline_value()
                    && self.visited.insert(*symbol)
                {
                    self.hir(value);
                }
            }
            Hir::Lambda(lambda) => {
                if let Symbol::Function(function) = db.symbol(*lambda)
                    && self.visited.insert(*lambda)
                {
                    self.hir(function.body);
                }
            }
            Hir::Block(block) => self.block(&block.statements, block.body),
            // A guard whose check doesn't use its value only evaluates it for its effect, the
            // same way as a binding that's never referenced
            Hir::Guard(guard) => {
                if guard.value_uses() > 0 || self.cx.guard_evaluates(hir, guard.value) {
                    self.hir(guard.value);
                }
            }
            _ => {
                for child in node.children() {
                    self.hir(child);
                }
            }
        }
    }

    fn block(&mut self, statements: &[Statement], body: Option<HirId>) {
        // Nothing after a `return` or `raise` is lowered
        if let Some(body) = reachable_body(statements, body) {
            self.hir(body);
        }

        for statement in reachable_statements(statements).iter().rev() {
            let Statement::Let(symbol, _) = *statement else {
                // Debug statements aren't lowered in release builds
                if !matches!(statement, Statement::Debug(..)) {
                    for hir in statement.hirs() {
                        self.hir(hir);
                    }
                }

                continue;
            };

            let Symbol::Binding(binding) = self.cx.db.symbol(symbol) else {
                continue;
            };

            // An inline binding's value is followed where it's referenced instead
            if binding.inline {
                continue;
            }

            let evaluated =
                self.symbols.contains(&symbol) || self.cx.evaluated_unreferenced(binding.value.hir);

            self.cx
                .analysis
                .let_evaluations
                .borrow_mut()
                .insert(symbol, evaluated);

            if evaluated {
                self.hir(binding.value.hir);
            }
        }
    }
}
