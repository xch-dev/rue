//! Finds whether evaluating a value may fail, and whether a reference to a value is always
//! evaluated, so that a value that may fail is never dropped, or moved somewhere that's only
//! evaluated some of the time.
//!
//! The same checks are used while lowering, where they depend on what's inlined, and by the
//! dependency graph and lints, which only know what's written in the source (see
//! [`FailureAnalysis`]).

mod evaluation;
mod may_fail;

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use indexmap::IndexSet;

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

    /// Whether creating a closure for the function, or calling it directly, evaluates an argument
    /// of an inlined function that may fail, since the arguments it captures are evaluated then.
    fn captures_may_fail(&self, function: SymbolId) -> bool;

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
#[derive(Debug, Default, Clone)]
pub struct FailureAnalysis {
    function_failures: RefCell<HashMap<SymbolId, FunctionFailure>>,
    substitution_failures: RefCell<HashMap<(SymbolId, bool), bool>>,
    let_evaluations: RefCell<HashMap<SymbolId, bool>>,
    parameter_uses: RefCell<HashMap<SymbolId, Rc<HashSet<SymbolId>>>>,
}

impl FailureAnalysis {
    pub fn new() -> Self {
        Self::default()
    }

    /// Whether evaluating the HIR may fail for well-typed values (see [`may_fail`]).
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

    /// Whether evaluating the HIR, which is in a function body, may fail other than by calling the
    /// function values of parameters, and if not, which parameters' values it calls.
    pub(crate) fn body_calls(&self, db: &Database, hir: HirId) -> Option<IndexSet<SymbolId>> {
        body_calls(&self.context(db), hir)
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
    /// Whether a binding that's never referenced is evaluated for its effect. The dependency graph
    /// only does that if its value may fail, other than by calling the value of a parameter,
    /// which depends on what the calls of the function pass (so it's treated as not evaluated).
    fn evaluated_unreferenced(&self, value: HirId) -> bool {
        body_calls(self, value).is_none()
    }

    /// The symbols that evaluating the statements and body may reference, deciding along the way
    /// which of the bindings they declare are evaluated (see [`Uses`]).
    fn block_uses(&self, statements: &[Statement], body: Option<HirId>) -> HashSet<SymbolId> {
        let mut uses = Uses::new(self);
        uses.block(statements, body);
        uses.symbols
    }
}

impl AnalysisContext for WrittenContext<'_> {
    fn db(&self) -> &Database {
        self.db
    }

    fn substituted_value(&self, symbol: SymbolId) -> Option<HirId> {
        match self.db.symbol(symbol) {
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

    fn is_parameter_used(&self, function: SymbolId, parameter: SymbolId) -> bool {
        if let Some(uses) = self.analysis.parameter_uses.borrow().get(&function) {
            return uses.contains(&parameter);
        }

        let Symbol::Function(symbol) = self.db.symbol(function) else {
            return true;
        };

        let mut uses = Uses::new(self);
        uses.hir(symbol.body);
        let used = uses.symbols.contains(&parameter);

        self.analysis
            .parameter_uses
            .borrow_mut()
            .insert(function, Rc::new(uses.symbols));

        used
    }

    fn captures(&self, function: SymbolId, parameter: SymbolId) -> bool {
        let Symbol::Function(symbol) = self.db.symbol(function) else {
            return false;
        };

        let mut uses = Uses::new(self);
        uses.hir(symbol.body);
        uses.symbols.contains(&parameter)
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
