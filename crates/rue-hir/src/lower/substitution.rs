//! Decides when a value that may fail can be inlined, so that inlining never drops a check, or
//! moves it somewhere that's only evaluated some of the time.
//!
//! A binding, constant or argument that's referenced once is normally evaluated where it's
//! referenced. That's only done for a value that may fail (see [`may_fail`]) if the reference is
//! always evaluated whenever the value would have been, which is at the let statement for a
//! binding, at the start of the program for a constant, and at the call for an argument.
//! Otherwise, it's bound like it would be if it were referenced more than once, or evaluated for
//! its effect if it's never referenced.
//!
//! A binding that's never referenced is evaluated for its effect if it may fail, which the
//! dependency graph decides (see [`crate::DependencyGraph::is_effect`]). So is the value of a type
//! guard whose check doesn't use it (see [`crate::DependencyGraph::is_guard_effect`]).
//!
//! This is sound as long as values have the shapes their types say, which the compiler trusts for
//! some of them (see the [`crate::analysis`] module docs).

use std::{cell::RefCell, collections::HashMap};

use indexmap::IndexSet;

use crate::{
    Database, FunctionKind, Hir, HirId, Statement, Symbol, SymbolId,
    analysis::{AnalysisContext, Evaluation, FunctionFailure, may_fail},
};

use super::Lowerer;

/// What was decided for a binding before its let statement was lowered again.
#[derive(Debug)]
pub(super) struct SavedDecisions {
    symbol: SymbolId,
    substitute: Option<bool>,
    is_path: Option<bool>,
}

fn restore(decided: &mut HashMap<SymbolId, bool>, symbol: SymbolId, value: Option<bool>) {
    match value {
        Some(value) => decided.insert(symbol, value),
        None => decided.remove(&symbol),
    };
}

impl Lowerer<'_, '_, '_> {
    /// Whether the binding or constant, which is referenced at most once and isn't `inline`, can
    /// be evaluated where it's referenced rather than bound. This is cached, since it's needed
    /// each time the symbol is grouped, captured or referenced.
    pub(super) fn can_substitute(&self, symbol: SymbolId) -> bool {
        if self.graph.references(symbol) == 0 {
            return true;
        }

        if let Some(&substitute) = self.substitutions.borrow().get(&symbol) {
            return substitute;
        }

        let substitute = self.decide_substitution(symbol);
        self.substitutions.borrow_mut().insert(symbol, substitute);
        substitute
    }

    /// Forgets what was decided for the bindings (whether they can be substituted, and whether
    /// they're read where they're referenced as paths), since their let statements are being
    /// lowered again, which might be in a call to an `inline fn` with different arguments than
    /// before. They're decided again when the bindings are grouped. Returns what was decided
    /// before, which is restored with [`Self::restore_decisions`] once the statements are
    /// lowered, since a call to the same `inline fn` can be lowered inside another one.
    pub(super) fn forget_decisions(&self, symbols: &IndexSet<SymbolId>) -> Vec<SavedDecisions> {
        let mut substitutions = self.substitutions.borrow_mut();
        let mut path_bindings = self.path_bindings.borrow_mut();

        symbols
            .iter()
            .map(|&symbol| SavedDecisions {
                symbol,
                substitute: substitutions.remove(&symbol),
                is_path: path_bindings.remove(&symbol),
            })
            .collect()
    }

    /// Restores what was decided before [`Self::forget_decisions`].
    pub(super) fn restore_decisions(&self, saved: Vec<SavedDecisions>) {
        let mut substitutions = self.substitutions.borrow_mut();
        let mut path_bindings = self.path_bindings.borrow_mut();

        for decisions in saved {
            restore(&mut substitutions, decisions.symbol, decisions.substitute);
            restore(&mut path_bindings, decisions.symbol, decisions.is_path);
        }
    }

    fn decide_substitution(&self, symbol: SymbolId) -> bool {
        match self.db.symbol(symbol) {
            Symbol::Binding(binding) => {
                !may_fail(self, binding.value.hir) || self.binding_always_evaluated(symbol)
            }
            Symbol::Constant(constant) => {
                !may_fail(self, constant.value.hir) || self.constant_always_evaluated(symbol)
            }
            Symbol::Function(_)
            | Symbol::Parameter(_)
            | Symbol::Unresolved
            | Symbol::Module(_)
            | Symbol::Builtin(_) => true,
        }
    }

    /// Whether the rest of the block after the binding's let statement always evaluates its
    /// single reference, or fails.
    fn binding_always_evaluated(&self, symbol: SymbolId) -> bool {
        let Some((block, index)) = self.graph.let_statement(symbol) else {
            return false;
        };

        let Hir::Block(block) = self.db.hir(block) else {
            return false;
        };

        // The walk stops at a `return` or `raise`, so it doesn't reach what isn't lowered
        Evaluation::new(self, symbol).block(&block.statements[index + 1..], block.body)
    }

    /// Whether the constant's single reference is always evaluated when the program is run, or
    /// fails. Constants that are referenced more than once are evaluated at the start of the
    /// program, so a reference in one of their values counts too.
    fn constant_always_evaluated(&self, symbol: SymbolId) -> bool {
        let roots = self
            .constant_roots
            .get_or_init(|| self.find_constant_roots());
        let mut evaluation = Evaluation::new(self, symbol);
        roots.iter().any(|&root| evaluation.hir(root))
    }

    /// What's always evaluated when the program is run: the body of `main`, and the values of the
    /// constants that are referenced more than once, since they're bound before it.
    fn find_constant_roots(&self) -> Vec<HirId> {
        let Symbol::Function(main) = self.db.symbol(self.main) else {
            return Vec::new();
        };

        if main.kind == FunctionKind::External {
            return Vec::new();
        }

        let mut roots = vec![main.body];

        for &dependency in self.dependencies(self.main, false).iter() {
            if let Symbol::Constant(constant) = self.db.symbol(dependency)
                && !constant.inline
                && self.graph.references(dependency) > 1
            {
                roots.push(constant.value.hir);
            }
        }

        roots
    }

    /// Whether the body of a function that's being inlined always evaluates the parameter's
    /// single reference, or fails, so the argument can be substituted for it.
    pub(super) fn parameter_always_evaluated(&self, body: HirId, parameter: SymbolId) -> bool {
        self.graph.references(parameter) == 1 && Evaluation::new(self, parameter).hir(body)
    }
}

impl Lowerer<'_, '_, '_> {
    /// Whether the binding is evaluated for its effect where it's lowered. The dependency graph
    /// decides which bindings may need to be from how the code is written, but it can't know the
    /// arguments of an inlined function, which might make the value unable to fail in this call.
    pub(super) fn is_lowered_effect(&self, binding: SymbolId) -> bool {
        let Symbol::Binding(symbol) = self.db.symbol(binding) else {
            return false;
        };

        self.graph.is_effect(binding) && may_fail(self, symbol.value.hir)
    }

    /// Whether the value of the type guard, whose check doesn't use it, is evaluated for its
    /// effect where it's lowered. Like a binding that's never referenced, the dependency graph
    /// decides which guards may need to be, and the value must still be able to fail here.
    pub(super) fn is_lowered_guard_effect(&self, guard: HirId, value: HirId) -> bool {
        self.graph.is_guard_effect(guard) && may_fail(self, value)
    }
}

impl Lowerer<'_, '_, '_> {
    /// Whether a binding or constant that isn't `inline` is evaluated where it's referenced, as
    /// long as that's only once and it can't keep it from failing (see
    /// [`Self::can_substitute`]).
    pub(super) fn is_substitution_candidate(&self, symbol: SymbolId, value: HirId) -> bool {
        // The result of a fold for the rest of the list is evaluated once, before the body,
        // unless the body only gets to it in one place, at most once. It's evaluated there then,
        // like any other binding, so that a body that doesn't get to it stops
        if self.db.is_loop_rest_call(value) && !self.db.is_single_use_loop_rest(symbol) {
            return false;
        }

        // A value that a loop refers to from outside of it is only substituted there if it's a
        // literal, since anything else would be evaluated again for each item. This can't depend
        // on the arguments of the calls being inlined, since the loop is lowered once (see
        // `group_symbols`), so a value that's only static with them isn't substituted either
        if self.graph.is_loop_capture(symbol) && !self.db.is_literal_hir(value) {
            return false;
        }

        self.options.auto_inline
    }
}

impl AnalysisContext for Lowerer<'_, '_, '_> {
    fn db(&self) -> &Database {
        self.db
    }

    fn operand_limits(&self) -> bool {
        self.options.enforces_operand_limits()
    }

    /// This is the value of an inline binding or constant, or of one that's inlined automatically,
    /// or the argument for a parameter of an inlined function.
    fn substituted_value(&self, symbol: SymbolId) -> Option<HirId> {
        if let Some(hir) = self.inline_argument(symbol) {
            return Some(hir);
        }

        let (value, inline) = match self.db.symbol(symbol) {
            Symbol::Binding(binding) => (binding.value.hir, binding.inline),
            Symbol::Constant(constant) => (constant.value.hir, constant.inline),
            Symbol::Function(_)
            | Symbol::Parameter(_)
            | Symbol::Unresolved
            | Symbol::Module(_)
            | Symbol::Builtin(_) => return None,
        };

        // A binding or constant whose single reference hasn't been decided yet might be
        // substituted, so its value is checked where it's referenced
        let substituted = inline
            || (self.is_substitution_candidate(symbol, value)
                && self.graph.references(symbol) <= 1
                && self
                    .substitutions
                    .borrow()
                    .get(&symbol)
                    .copied()
                    .unwrap_or(true));

        substituted.then_some(value)
    }

    fn captures_may_fail(&self, function: SymbolId) -> bool {
        if self.inline_symbols.iter().all(HashMap::is_empty) {
            return false;
        }

        self.dependencies(function, true).iter().any(|&capture| {
            self.inline_argument(capture)
                .is_some_and(|hir| !self.is_static_hir(hir) && may_fail(self, hir))
        })
    }

    fn parameter_values_may_fail(&self, parameter: SymbolId) -> bool {
        self.graph.parameter_values_may_fail(self.db, parameter)
    }

    fn is_evaluated(&self, binding: SymbolId) -> bool {
        self.graph.references(binding) > 0 || self.graph.is_effect(binding)
    }

    // An effect is only lowered if its value may fail where it's lowered (see
    // [`Lowerer::is_lowered_effect`])
    fn let_evaluates(&self, binding: SymbolId, _rest: &[Statement], _body: Option<HirId>) -> bool {
        self.graph.references(binding) > 0 || self.is_lowered_effect(binding)
    }

    fn is_guard_evaluated(&self, guard: HirId) -> bool {
        self.graph.is_guard_effect(guard)
    }

    fn guard_evaluates(&self, guard: HirId, value: HirId) -> bool {
        self.is_lowered_guard_effect(guard, value)
    }

    fn is_parameter_used(&self, _function: SymbolId, parameter: SymbolId) -> bool {
        self.graph.references(parameter) > 0
    }

    fn captures(&self, function: SymbolId, parameter: SymbolId) -> bool {
        self.dependencies(function, true).contains(&parameter)
    }

    fn function_failures(&self) -> &RefCell<HashMap<SymbolId, FunctionFailure>> {
        &self.function_failures
    }

    fn substitution_failures(&self) -> &RefCell<HashMap<(SymbolId, bool), bool>> {
        &self.substitution_failures
    }

    // A constant's value can't reference the arguments of an inlined function, so whether it may
    // fail only depends on which of the bindings and constants it references are substituted.
    // Those are only ever decided not to be, which can't make it fail, so a constant that can't
    // fail never will. Bindings are decided again in each call to an `inline fn`.
    fn shares_substitution(&self, symbol: SymbolId, fails: bool) -> bool {
        !fails && matches!(self.db.symbol(symbol), Symbol::Constant(_))
    }
}
