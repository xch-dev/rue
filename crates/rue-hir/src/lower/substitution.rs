//! Decides when a value that's only referenced once is evaluated where it's referenced, so that
//! inlining never drops a check, or moves it somewhere that's only evaluated some of the time,
//! and never evaluates a value over and over.
//!
//! A binding, constant or argument that's referenced once is normally evaluated where it's
//! referenced. That's only done for a value that may fail (see [`may_fail`]) if the reference is
//! always evaluated whenever the value would have been, which is at the let statement for a
//! binding, at the start of the program for a constant, and at the call for an argument.
//! Otherwise, it's bound like it would be if it were referenced more than once, or evaluated for
//! its effect if it's never referenced.
//!
//! A binding or constant is also bound if its reference can be evaluated more than once for each
//! time the value would be, such as in a lambda that's called for each item of a list, unless
//! evaluating it again is cheap, or it's compiled to a constant (see
//! [`Lowerer::decide_repeats`]). A function that captures a value that's always bound only needs
//! the value, rather than what it's evaluated from (see [`Lowerer::is_always_bound`]).
//!
//! A binding that only a loop refers to is captured by it, and never substituted inside of it.
//! Its value is evaluated where the loop is called, which builds what the loop captures, rather
//! than bound before, if that's the loop's only use and the call is evaluated once whenever the
//! value would have been (see [`Lowerer::loop_call_substitution`]).
//!
//! A binding that's never referenced is evaluated for its effect if it may fail, which the
//! dependency graph decides (see [`crate::DependencyGraph::is_effect`]). So is the value of a type
//! guard whose check doesn't use it (see [`crate::DependencyGraph::is_guard_effect`]).
//!
//! This is sound as long as values have the shapes their types say, which the compiler trusts for
//! some of them (see the [`crate::analysis`] module docs).

use std::{
    cell::{OnceCell, RefCell},
    collections::{HashMap, HashSet},
};

use indexmap::IndexSet;

use crate::{
    Database, FunctionCall, FunctionKind, FunctionSymbol, Hir, HirId, Statement, Symbol, SymbolId,
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

/// The walk of where the result of a fold would be substituted (see
/// [`Lowerer::is_loop_rest_rebound`]).
struct Rebinding {
    /// Whether the result may fail. An inlined call binds an argument that may fail unless its
    /// body always evaluates the parameter.
    fails: bool,
    /// What the result would be substituted for: its binding, a binding whose value it's in, or
    /// the parameter of the call being followed whose argument it's in.
    targets: Vec<SymbolId>,
    /// The functions whose bodies are being followed, which are inlined into the calls, and the
    /// argument for each of their parameters, innermost last. A function can be followed inside
    /// of a call to itself, such as an `inline fn` that's called with a lambda that calls it
    /// again, so the parameters of the inner call hide the outer one's.
    frames: Vec<(SymbolId, HashMap<SymbolId, HirId>)>,
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
                self.substitutes_repeat(symbol)
                    && (!self.db.is_loop_rest_call(binding.value.hir)
                        || !self.is_loop_rest_rebound(symbol, binding.value.hir))
                    && (!may_fail(self, binding.value.hir) || self.binding_always_evaluated(symbol))
            }
            Symbol::Constant(constant) => {
                self.substitutes_repeat(symbol)
                    && (!may_fail(self, constant.value.hir)
                        || self.constant_always_evaluated(symbol))
            }
            Symbol::Function(_)
            | Symbol::Parameter(_)
            | Symbol::Unresolved
            | Symbol::Module(_)
            | Symbol::Builtin(_) => true,
        }
    }

    /// Whether the binding or constant is substituted where it's referenced, as far as how often
    /// that's evaluated goes. Its only reference can be somewhere that's evaluated more than
    /// once for each time the value is bound, such as a lambda that's called for each item of a
    /// list, and its value isn't cheap (see [`crate::DependencyGraph::is_repeated`]). Then it's
    /// only substituted if it's compiled to a constant (see [`Self::decide_repeats`]).
    fn substitutes_repeat(&self, symbol: SymbolId) -> bool {
        !self.graph.is_repeated(symbol) || self.is_folded_repeat(symbol)
    }

    /// Whether the value was found to be compiled to a constant (see [`Self::decide_repeats`]).
    fn is_folded_repeat(&self, symbol: SymbolId) -> bool {
        self.folded_repeats.contains(&symbol)
    }

    /// Decides which of the bindings and constants whose only reference can be evaluated more
    /// than once are substituted there anyway (see [`Self::substitutes_repeat`]): the ones whose
    /// value is compiled to a constant, although it isn't written as something that's cheap to
    /// evaluate, such as a hash of constants. It's then folded with what uses it.
    ///
    /// This is decided before the entrypoint is lowered (see [`Self::lower_main`]), rather than
    /// each time a let statement is. Finding out whether a value is a constant lowers it, and
    /// what a function captures depends on which values are bound (see
    /// [`Self::is_always_bound`]), which is only found once for each function. So it doesn't
    /// depend on the arguments of the calls being inlined: a value that's only a constant with
    /// them is bound. A program that's only run while compiling isn't lowered that way, so all
    /// of these values are bound in it.
    ///
    /// It does depend on which functions are inlined, since a call that isn't inlined isn't
    /// folded, so it's decided again when that changes. What was found with the values that
    /// were substituted before is only forgotten if the decision changed, which it rarely does.
    pub(super) fn decide_repeats(&mut self) {
        if !self.options.auto_inline {
            return;
        }

        let undecided: Vec<(SymbolId, HirId)> = self
            .dependencies(self.main, false)
            .iter()
            .filter(|&&symbol| self.graph.is_repeated(symbol))
            .filter_map(|&symbol| match self.db.symbol(symbol) {
                Symbol::Binding(binding) => Some((symbol, binding.value.hir)),
                Symbol::Constant(constant) => Some((symbol, constant.value.hir)),
                Symbol::Function(_)
                | Symbol::Parameter(_)
                | Symbol::Unresolved
                | Symbol::Module(_)
                | Symbol::Builtin(_) => None,
            })
            .collect();

        let folded: HashSet<SymbolId> = undecided
            .into_iter()
            .filter(|&(_, value)| self.folds_to_constant(value))
            .map(|(symbol, _)| symbol)
            .collect();

        if folded == self.folded_repeats {
            return;
        }

        self.folded_repeats = folded;

        // What a function captures, and whether a value can fail, depend on which values are
        // substituted
        self.dependencies.borrow_mut().clear();
        self.argument_captures.borrow_mut().clear();
        self.substitutions.borrow_mut().clear();
        self.substitution_failures.borrow_mut().clear();
        self.function_failures.borrow_mut().clear();
        self.constant_roots = OnceCell::new();
    }

    /// Whether the binding or constant is bound wherever it's lowered, rather than substituted
    /// where it's referenced, so that a function that refers to it from outside only needs its
    /// value (see [`crate::DependencyGraph::captures`]). This has to hold whatever the arguments
    /// of the calls being inlined are, since what a function captures is only found once.
    ///
    /// A value that's referenced more than once is only substituted if it's a path (see
    /// [`Self::is_path_binding`]), which a constant never is, and which a binding can only be if
    /// it's written as one. A value whose only reference can be evaluated more than once, and
    /// that isn't cheap to evaluate, is bound unless it's compiled to a constant (see
    /// [`Self::decide_repeats`]).
    pub(super) fn is_always_bound(&self, symbol: SymbolId) -> bool {
        let (value, inline, is_constant) = match self.db.symbol(symbol) {
            Symbol::Binding(binding) => (binding.value.hir, binding.inline, false),
            Symbol::Constant(constant) => (constant.value.hir, constant.inline, true),
            Symbol::Function(_)
            | Symbol::Parameter(_)
            | Symbol::Unresolved
            | Symbol::Module(_)
            | Symbol::Builtin(_) => return false,
        };

        if inline {
            return false;
        }

        // Nothing else is substituted unless values are inlined automatically
        if !self.options.auto_inline {
            return true;
        }

        if self.graph.is_repeated(symbol) {
            return !self.is_folded_repeat(symbol);
        }

        self.graph.references(symbol) > 1
            && (is_constant
                || !matches!(
                    self.db.hir(self.db.without_casts(value)),
                    Hir::Reference(_) | Hir::FieldAccess(_)
                ))
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

    /// The call of the loop that the binding is evaluated at, rather than bound where its let
    /// statement is, if there is one. That's a binding that's only referenced inside of one loop,
    /// which captures it (see [`crate::DependencyGraph::sole_loop`]). The call evaluates what the
    /// loop captures, so the value is evaluated there, as recursion written by hand evaluates the
    /// argument it passes the value as, and it isn't bound before. The loop still reads it from
    /// what it's called with, so it's evaluated once, whatever the loop does with it.
    ///
    /// The call has to be evaluated at most once for each time the let statement is, so it has
    /// to be in the rest of the block itself: not inside of a lambda, another loop, a type guard
    /// that uses its value more than once, the value of a `let`, or an argument of a call that's
    /// inlined, which can each be evaluated any number of times.
    ///
    /// A value that may fail is only evaluated at the call if the rest of the block always gets
    /// there, as for any binding that's substituted where it's referenced (see
    /// [`Self::can_substitute`]), with the call of the loop counting as that reference. A value
    /// made of literals is substituted inside of the loop instead.
    ///
    /// This is decided where the let statement is lowered (see [`Self::lower_let_run`]), which
    /// the call is lowered inside of.
    pub(super) fn loop_call_substitution(&self, symbol: SymbolId) -> Option<HirId> {
        let Symbol::Binding(binding) = self.db.symbol(symbol) else {
            return None;
        };

        if !self.options.auto_inline || binding.inline || self.db.is_literal_hir(binding.value.hir)
        {
            return None;
        }

        let function = self.graph.sole_loop(symbol)?;
        let (block, index) = self.graph.let_statement(symbol)?;

        let Hir::Block(block) = self.db.hir(block) else {
            return None;
        };

        let statements = &block.statements[index + 1..];
        let mut calls = Vec::new();

        for hir in statements
            .iter()
            .flat_map(|statement| statement.lowered_hirs(self.options.debug_symbols))
            .chain(block.body)
        {
            self.direct_loop_calls(function, hir, &mut calls);
        }

        let [call] = calls[..] else {
            return None;
        };

        (!may_fail(self, binding.value.hir)
            || Evaluation::new(self, function).block(statements, block.body))
        .then_some(call)
    }

    /// Finds the calls of the loop that the node evaluates at most once each time it's
    /// evaluated (see [`Self::loop_call_substitution`]).
    fn direct_loop_calls(&self, function: SymbolId, hir: HirId, calls: &mut Vec<HirId>) {
        rue_types::grow_stack(|| match self.db.hir(hir) {
            Hir::FunctionCall(call) => {
                if matches!(
                    self.db.hir(self.db.without_casts(call.function)),
                    Hir::Reference(callee) if *callee == function
                ) {
                    calls.push(hir);
                }

                // An inlined call evaluates its arguments wherever it refers to its parameters
                if self.inlined_callee(call.function).is_none() {
                    for &arg in &call.args {
                        self.direct_loop_calls(function, arg, calls);
                    }
                }
            }
            Hir::Guard(guard) if guard.value_uses() > 1 => {}
            // A let statement's value is evaluated wherever the binding is substituted
            Hir::Block(block) => {
                for hir in block
                    .statements
                    .iter()
                    .flat_map(|statement| statement.lowered_hirs(self.options.debug_symbols))
                    .chain(block.body)
                {
                    self.direct_loop_calls(function, hir, calls);
                }
            }
            node => {
                for child in node.children() {
                    self.direct_loop_calls(function, child, calls);
                }
            }
        });
    }

    /// Whether the binding for the result of a fold for the rest of the list (see
    /// [`Database::is_loop_rest_call`]), which is referenced once, would only be bound again if
    /// it were substituted there: its reference is in the argument of a call that's inlined into
    /// a body that binds the argument (see [`Self::lower_inline_call`]), directly or through
    /// other calls that are inlined. Binding it there is no smaller than binding it before the
    /// body, and costs more to run, so it's bound before the body then. Otherwise it's evaluated
    /// in place, as recursion written by hand evaluates it: at a call that isn't inlined, or
    /// where an inlined body refers to the parameter.
    ///
    /// This only decides which of the two is smaller, since neither goes through the rest of the
    /// list more than once for the item. A reference that can be evaluated more than once, such
    /// as one in a lambda, another `for`, a type guard that uses its value more than once, or the
    /// argument of an `inline fn` that refers to its parameter more than once, is counted as
    /// more than one, or as repeated, by the dependency graph, so the binding isn't substituted
    /// there at all (see [`crate::DependencyGraph::is_repeated`]). And any other inlined call
    /// binds an argument whose parameter is referenced more than once.
    fn is_loop_rest_rebound(&self, symbol: SymbolId, value: HirId) -> bool {
        let Some((block, index)) = self.graph.let_statement(symbol) else {
            return false;
        };

        let Hir::Block(block) = self.db.hir(block) else {
            return false;
        };

        let mut walk = Rebinding {
            fails: may_fail(self, value),
            targets: vec![symbol],
            frames: Vec::new(),
        };

        self.rebinds_block(&mut walk, &block.statements[index + 1..], block.body)
    }

    /// Whether evaluating the node would bind the result again at an inlined call that the
    /// result is substituted into (see [`Self::is_loop_rest_rebound`]).
    fn rebinds(&self, walk: &mut Rebinding, hir: HirId) -> bool {
        rue_types::grow_stack(|| match self.db.hir(hir) {
            Hir::Block(block) => self.rebinds_block(walk, &block.statements, block.body),
            Hir::FunctionCall(call) => self.rebinds_call(walk, call),
            node => node.children().any(|child| self.rebinds(walk, child)),
        })
    }

    fn rebinds_block(
        &self,
        walk: &mut Rebinding,
        statements: &[Statement],
        body: Option<HirId>,
    ) -> bool {
        for statement in statements {
            let Statement::Let(binding, _) = statement else {
                if statement.hirs().any(|hir| self.rebinds(walk, hir)) {
                    return true;
                }

                continue;
            };

            let Symbol::Binding(symbol) = self.db.symbol(*binding) else {
                continue;
            };

            if self.rebinds(walk, symbol.value.hir) {
                return true;
            }

            if symbol.inline || !self.mentions(walk, symbol.value.hir) {
                continue;
            }

            // A binding that's referenced once is substituted where it's referenced, along with
            // the result in its value. One that's referenced more than once is bound, which
            // binds the result again if that's all its value is
            if self.graph.references(*binding) == 1 {
                walk.targets.push(*binding);
            } else if matches!(
                self.db.hir(self.db.without_casts(symbol.value.hir)),
                Hir::Reference(reference) if walk.targets.contains(reference)
            ) {
                return true;
            }
        }

        body.is_some_and(|body| self.rebinds(walk, body))
    }

    /// Whether the call binds the result again, or something it's substituted into does. An
    /// argument that the result is in is evaluated at a call that isn't inlined, and where the
    /// body refers to the parameter in one that's inlined, unless that binds it.
    fn rebinds_call(&self, walk: &mut Rebinding, call: &FunctionCall) -> bool {
        if self.rebinds(walk, call.function) || call.args.iter().any(|&arg| self.rebinds(walk, arg))
        {
            return true;
        }

        // The result is referenced once, so it's in one argument at most
        let Some(index) = call.args.iter().position(|&arg| self.mentions(walk, arg)) else {
            return false;
        };

        let Some(callee) = self.walked_callee(walk, call.function) else {
            return false;
        };

        let Symbol::Function(function) = self.db.symbol(callee) else {
            return false;
        };

        // The items of a spread argument, and the arguments that are put in a list for a rest
        // parameter, aren't followed to their parameters
        let spread = !call.nil_terminated && index + 1 == call.args.len();

        let Some(&parameter) = function.fixed_parameters().get(index).filter(|_| !spread) else {
            return true;
        };

        // The result is never trivial, since it's a call
        if function.kind != FunctionKind::Inline
            && self.binds_inline_argument(function, parameter, || false, || walk.fails)
        {
            return true;
        }

        let arguments = function
            .fixed_parameters()
            .iter()
            .copied()
            .zip(call.args.iter().copied())
            .collect();

        let targets = std::mem::replace(&mut walk.targets, vec![parameter]);
        walk.frames.push((callee, arguments));

        let rebinds = self.rebinds(walk, function.body);

        walk.frames.pop();
        walk.targets = targets;

        rebinds
    }

    /// Whether an inlined call to the function, which isn't an `inline fn`, binds the argument
    /// for the parameter before the body, like a let statement, rather than substituting it where
    /// the body refers to the parameter (see [`Self::lower_inline_call`]). It's evaluated for its
    /// effect instead if the parameter is never referenced.
    ///
    /// That's the case for an argument whose parameter is referenced more than once, or by a
    /// loop, which evaluates it for each item, unless it's as cheap to evaluate each time (see
    /// [`Self::is_trivial_argument`]). And for one that may fail, unless the body always
    /// evaluates the parameter, so that it's still evaluated at the call.
    pub(super) fn binds_inline_argument(
        &self,
        function: &FunctionSymbol,
        parameter: SymbolId,
        is_trivial: impl FnOnce() -> bool,
        may_fail: impl FnOnce() -> bool,
    ) -> bool {
        let repeated = (self.graph.references(parameter) > 1
            || self.graph.is_loop_capture(parameter))
            && !is_trivial();

        repeated || (may_fail() && !self.parameter_always_evaluated(function.body, parameter))
    }

    /// The function that a call to the callee is inlined into, as [`Self::inlined_callee`] finds
    /// it where the call is lowered: a function called by name that's inlined, or a lambda that's
    /// known there, which can be the argument of a call being followed, such as a lambda that's
    /// passed to an `inline fn` and called in it. As when lowering (see [`Self::known_lambda`]),
    /// a lambda isn't inlined inside a call of itself or of a function it depends on.
    fn walked_callee(&self, walk: &Rebinding, mut callee: HirId) -> Option<SymbolId> {
        let mut frames = walk.frames.len();
        let mut followed = false;

        // An argument is written outside of the call it's passed to, so the parameters it
        // refers to are those of the calls around that one
        while let Hir::Reference(symbol) = self.db.hir(self.db.without_casts(callee))
            && let Some((frame, argument)) = walk.frames[..frames]
                .iter()
                .enumerate()
                .rev()
                .find_map(|(frame, (_, arguments))| Some((frame, *arguments.get(symbol)?)))
        {
            callee = argument;
            frames = frame;
            followed = true;
        }

        if !followed
            && let Hir::Reference(symbol) = self.db.hir(self.db.without_casts(callee))
            && let Symbol::Function(_) = self.db.symbol(*symbol)
        {
            return self.inlined_callee(callee);
        }

        let lambda = self.known_lambda(callee)?;
        let dependencies = self.dependencies(lambda, false);

        let reenters = walk
            .frames
            .iter()
            .any(|&(function, _)| function == lambda || dependencies.contains(&function));

        (!reenters).then_some(lambda)
    }

    /// Whether the node refers to what the result is substituted for, outside of a lambda, which
    /// captures it.
    fn mentions(&self, walk: &Rebinding, hir: HirId) -> bool {
        rue_types::grow_stack(|| match self.db.hir(hir) {
            Hir::Reference(symbol) => walk.targets.contains(symbol),
            Hir::Block(block) => {
                block.statements.iter().any(|statement| match statement {
                    Statement::Let(binding, _) => matches!(
                        self.db.symbol(*binding),
                        Symbol::Binding(binding) if self.mentions(walk, binding.value.hir)
                    ),
                    statement => statement.hirs().any(|hir| self.mentions(walk, hir)),
                }) || block.body.is_some_and(|body| self.mentions(walk, body))
            }
            node => node.children().any(|child| self.mentions(walk, child)),
        })
    }

    /// Whether the constant's single reference is always evaluated when the program is run, or
    /// fails. Constants that are bound are evaluated at the start of the program, so a reference
    /// in one of their values counts too (see [`Self::find_constant_roots`]).
    fn constant_always_evaluated(&self, symbol: SymbolId) -> bool {
        let roots = self
            .constant_roots
            .get_or_init(|| self.find_constant_roots());
        let mut evaluation = Evaluation::new(self, symbol);
        roots.iter().any(|&root| evaluation.hir(root))
    }

    /// What's always evaluated when the program is run: the body of `main`, and the values of the
    /// constants that are bound before it (see [`Self::is_always_bound`]): the ones that are
    /// referenced more than once, or whose only reference can be evaluated more than once.
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
                && self.is_always_bound(dependency)
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
    // fail never will. Bindings are decided again in each call to an `inline fn`. The exception
    // is a value that's found to be a constant once another function is inlined, and substituted
    // from then on, which is why `decide_repeats` forgets all of this when that changes.
    fn shares_substitution(&self, symbol: SymbolId, fails: bool) -> bool {
        !fails && matches!(self.db.symbol(symbol), Symbol::Constant(_))
    }
}
