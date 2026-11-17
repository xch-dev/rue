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

use crate::{Database, Hir, HirId, Statement, Symbol, SymbolId};

pub(crate) use evaluation::*;
pub(crate) use may_fail::*;

/// What the checks need to know about how the HIR is lowered.
pub(crate) trait AnalysisContext {
    fn db(&self) -> &Database;

    /// The value that's evaluated where the symbol is referenced, if it's substituted there rather
    /// than read from the environment.
    fn substituted_value(&self, symbol: SymbolId) -> Option<HirId>;

    /// Whether the value of the binding, which isn't `inline`, is evaluated, either at its let
    /// statement or where it's referenced.
    fn is_evaluated(&self, binding: SymbolId) -> bool;

    /// Whether creating a closure for the function, or calling it directly, evaluates an argument
    /// of an inlined function that may fail, since the arguments it captures are evaluated then.
    fn captures_may_fail(&self, function: SymbolId) -> bool;

    /// Whether creating a closure for the function evaluates the parameter, which is the case for
    /// an argument of an inlined function that the closure captures by value.
    fn captures(&self, function: SymbolId, parameter: SymbolId) -> bool;

    /// Whether calling each function may fail, which is shared between checks.
    fn function_failures(&self) -> &RefCell<HashMap<SymbolId, FunctionFailure>>;
}

/// The checks for code as it's written, without knowing what's inlined automatically.
///
/// An `inline` binding or constant is evaluated where it's referenced. Any other binding or
/// constant whose value may fail is always evaluated when its let statement is reached, or when
/// the program starts, so a reference to it can't fail. The parameters of a function can't fail
/// either, since a check in an argument is kept at the call.
#[derive(Debug, Default, Clone)]
pub struct FailureAnalysis {
    function_failures: RefCell<HashMap<SymbolId, FunctionFailure>>,
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

    fn context<'a>(&'a self, db: &'a Database) -> WrittenContext<'a> {
        WrittenContext {
            db,
            function_failures: &self.function_failures,
        }
    }
}

struct WrittenContext<'a> {
    db: &'a Database,
    function_failures: &'a RefCell<HashMap<SymbolId, FunctionFailure>>,
}

impl AnalysisContext for WrittenContext<'_> {
    fn db(&self) -> &Database {
        self.db
    }

    fn substituted_value(&self, symbol: SymbolId) -> Option<HirId> {
        match self.db.symbol(symbol) {
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

    // A binding that's referenced is evaluated, and one that isn't is evaluated for its effect if
    // it may fail, so it's evaluated whenever that matters
    fn is_evaluated(&self, _binding: SymbolId) -> bool {
        true
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

                let value = match db.symbol(*symbol) {
                    Symbol::Binding(binding) if binding.inline => binding.value.hir,
                    Symbol::Constant(constant) if constant.inline => constant.value.hir,
                    _ => return false,
                };

                self.visited.insert(*symbol) && self.hir(value)
            }
            Hir::Lambda(lambda) => {
                let Symbol::Function(function) = db.symbol(*lambda) else {
                    return false;
                };

                self.visited.insert(*lambda) && self.hir(function.body)
            }
            Hir::Block(block) => {
                block.statements.iter().any(|statement| match statement {
                    Statement::Let(symbol, _) => match db.symbol(*symbol) {
                        Symbol::Binding(binding) => self.hir(binding.value.hir),
                        _ => false,
                    },
                    _ => statement.hirs().any(|hir| self.hir(hir)),
                }) || block.body.is_some_and(|body| self.hir(body))
            }
            _ => node.children().any(|child| self.hir(child)),
        }
    }
}
