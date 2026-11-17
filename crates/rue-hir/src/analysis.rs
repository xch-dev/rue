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
};

use indexmap::IndexSet;

use crate::{Database, Hir, HirId, Statement, Symbol, SymbolId};

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
        WrittenContext {
            db,
            function_failures: &self.function_failures,
            substitution_failures: &self.substitution_failures,
        }
    }
}

struct WrittenContext<'a> {
    db: &'a Database,
    function_failures: &'a RefCell<HashMap<SymbolId, FunctionFailure>>,
    substitution_failures: &'a RefCell<HashMap<(SymbolId, bool), bool>>,
}

impl AnalysisContext for WrittenContext<'_> {
    fn db(&self) -> &Database {
        self.db
    }

    fn substituted_value(&self, symbol: SymbolId) -> Option<HirId> {
        match self.db.symbol(symbol) {
            Symbol::Constant(constant) => Some(constant.value.hir),
            _ => inline_value(self.db, symbol),
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
        let Symbol::Binding(symbol) = self.db.symbol(binding) else {
            return false;
        };

        if may_fail(self, symbol.value.hir) {
            return true;
        }

        let mut search = References {
            db: self.db,
            target: binding,
            visited: HashSet::new(),
        };

        search.block(rest, body)
    }

    fn captures_may_fail(&self, _function: SymbolId) -> bool {
        false
    }

    fn captures(&self, function: SymbolId, parameter: SymbolId) -> bool {
        let Symbol::Function(symbol) = self.db.symbol(function) else {
            return false;
        };

        let mut search = References {
            db: self.db,
            target: parameter,
            visited: HashSet::new(),
        };

        search.hir(symbol.body)
    }

    fn function_failures(&self) -> &RefCell<HashMap<SymbolId, FunctionFailure>> {
        self.function_failures
    }

    fn substitution_failures(&self) -> &RefCell<HashMap<(SymbolId, bool), bool>> {
        self.substitution_failures
    }

    // What's substituted as written never changes
    fn shares_substitution(&self, _symbol: SymbolId, _fails: bool) -> bool {
        true
    }
}

/// The value of an `inline` binding or constant, which is evaluated where it's referenced.
fn inline_value(db: &Database, symbol: SymbolId) -> Option<HirId> {
    match db.symbol(symbol) {
        Symbol::Binding(binding) if binding.inline => Some(binding.value.hir),
        Symbol::Constant(constant) if constant.inline => Some(constant.value.hir),
        Symbol::Binding(_)
        | Symbol::Constant(_)
        | Symbol::Function(_)
        | Symbol::Parameter(_)
        | Symbol::Unresolved
        | Symbol::Module(_)
        | Symbol::Builtin(_) => None,
    }
}

/// Finds whether some HIR references the target, including in the lambdas it creates and the
/// values of the inline bindings and constants it references, which are evaluated there. Other
/// bindings are evaluated where they're defined, so a closure captures their values instead.
struct References<'a> {
    db: &'a Database,
    target: SymbolId,
    visited: HashSet<SymbolId>,
}

impl References<'_> {
    fn hir(&mut self, hir: HirId) -> bool {
        // This follows nested lambdas and inline values, which can be as deep as the program
        rue_types::grow_stack(|| self.hir_inner(hir))
    }

    fn hir_inner(&mut self, hir: HirId) -> bool {
        let db = self.db;
        let node = db.hir(hir);

        match node {
            Hir::Reference(symbol) => {
                if *symbol == self.target {
                    return true;
                }

                inline_value(db, *symbol)
                    .is_some_and(|value| self.visited.insert(*symbol) && self.hir(value))
            }
            Hir::Lambda(lambda) => {
                let Symbol::Function(function) = db.symbol(*lambda) else {
                    return false;
                };

                self.visited.insert(*lambda) && self.hir(function.body)
            }
            Hir::Block(block) => self.block(&block.statements, block.body),
            _ => node.children().any(|child| self.hir(child)),
        }
    }

    fn block(&mut self, statements: &[Statement], body: Option<HirId>) -> bool {
        statements.iter().any(|statement| match *statement {
            Statement::Let(symbol, _) => match self.db.symbol(symbol) {
                Symbol::Binding(binding) => self.hir(binding.value.hir),
                Symbol::Constant(_)
                | Symbol::Function(_)
                | Symbol::Parameter(_)
                | Symbol::Unresolved
                | Symbol::Module(_)
                | Symbol::Builtin(_) => false,
            },
            _ => statement.hirs().any(|hir| self.hir(hir)),
        }) || body.is_some_and(|body| self.hir(body))
    }
}
