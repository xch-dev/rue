//! Decides which of the functions that aren't always inlined are inlined into each call anyway.
//!
//! A function that's called once is always inlined, since its body is only there once either
//! way, unless one of its parameters is referenced more than once. One that's called more than
//! once is only smaller inlined if its body is about as small as a call to it, such as
//! `fn twice(x: Int) -> Int { x + x }`, and one whose parameter is referenced more than once if
//! its argument is cheap enough to repeat or to bind. Whether it is depends on what
//! the optimizer makes of the body in each place it's called from, which estimates of the HIR
//! have gotten wrong in direction before. So the whole program is lowered with and without each
//! such function inlined, and it's only inlined if the program it compiles to is smaller (see
//! [`super::measure`]).
//!
//! A function is inlined into every call or none: if only some calls were inlined, the function
//! would still be in the program for the others, along with a copy of its body in each of the
//! ones that were.
//!
//! Inlining doesn't change what's evaluated. An argument that may fail is still evaluated at
//! the call, and one whose parameter is referenced more than once is still only evaluated
//! once, unless it's as cheap to evaluate each time (see [`Lowerer::lower_inline_call`]), which
//! is how `inline fn` and functions that are called once are already lowered. What it costs to
//! run only changes by the call that's no longer made.
//!
//! Each candidate takes another pass over the program, so only functions with a small body are
//! measured (see [`MAX_INLINE_BODY_SIZE`]), and no more than [`MAX_INLINE_CANDIDATES`] of them.
//! Const expressions aren't measured at all, since the programs they're lowered to are only
//! run while compiling (see [`Lowerer::lower_main`]). This is decided again for each way the
//! environments are laid out (see [`super::layout`]).

use crate::{Environment, FunctionKind, Hir, HirId, Statement, Symbol, SymbolId};

use super::{Lowerer, measure::Trial};

/// How many nodes of HIR the body of a function can have and still be measured. A call takes
/// about eight bytes, and the function itself a few more, so a body that's repeated in each call
/// is rarely smaller once it's much larger than that. This isn't what decides whether a function
/// is inlined, which the measurement does. It keeps functions that are unlikely to win from
/// taking a pass over the program each, and gives up the few larger ones that would.
const MAX_INLINE_BODY_SIZE: usize = 32;

/// How many functions are measured in one program, in the order the dependency graph finds them
/// from the entrypoint. The ones after that are called as they were.
const MAX_INLINE_CANDIDATES: usize = 8;

impl Lowerer<'_, '_, '_> {
    /// Whether the function was measured, and found to make the program smaller when it's
    /// inlined into each call.
    pub(super) fn is_inlined_function(&self, function: SymbolId) -> bool {
        self.inlined_functions.contains(&function)
    }

    /// Whether the function is only inlined if the program is measured to be smaller that way:
    /// it's called more than once, or one of its parameters is referenced more than once, so
    /// that its argument might have to be bound in each call. Any other function that's called
    /// once is always inlined.
    pub(super) fn is_measured_function(&self, function: SymbolId) -> bool {
        if self.graph.references(function) > 1 {
            return true;
        }

        let Symbol::Function(symbol) = self.db.symbol(function) else {
            return false;
        };

        symbol
            .parameters
            .iter()
            .any(|&parameter| self.graph.references(parameter) > 1)
    }

    /// Lowers the entrypoint with the current layout, after deciding which of the functions that
    /// are measured are inlined: each one that makes the program smaller, given the ones before
    /// it. This is decided again for each layout (see [`super::layout`]).
    pub(super) fn decide_inlining(&mut self, env: &Environment) -> Trial {
        let main = self.main;
        let candidates = self.inline_candidates();

        self.prepare_trial();
        let mut best = self.trial(|this| this.lower_symbol_value(env, main));

        for function in candidates {
            self.inlined_functions.insert(function);
            self.prepare_trial();
            let inlined = self.trial(|this| this.lower_symbol_value(env, main));

            if inlined.size < best.size {
                best = inlined;
            } else {
                self.inlined_functions.remove(&function);
            }
        }

        best
    }

    /// The functions that could make the program smaller if they were inlined into each of their
    /// calls: ones that aren't inlined already (see [`Self::is_measured_function`]), are never
    /// used as a value, don't call themselves, and have a small body. A function that's used as
    /// a value has to be in the program anyway, and one that calls itself can't be inlined.
    fn inline_candidates(&self) -> Vec<SymbolId> {
        if !self.options.auto_inline {
            return Vec::new();
        }

        self.dependencies(self.main, false)
            .iter()
            .copied()
            .filter(|&symbol| {
                let Symbol::Function(function) = self.db.symbol(symbol) else {
                    return false;
                };

                let mut budget = MAX_INLINE_BODY_SIZE;

                symbol != self.main
                    && matches!(
                        function.kind,
                        FunctionKind::Sequential | FunctionKind::BinaryTree
                    )
                    && self.is_measured_function(symbol)
                    && !self.graph.is_closure(symbol)
                    && !self.is_recursive(symbol, function)
                    && self.fits(function.body, &mut budget)
            })
            .take(MAX_INLINE_CANDIDATES)
            .collect()
    }

    /// Whether the HIR has no more nodes than the budget, which is reduced by them. The values of
    /// bindings count, but the bodies of the functions and lambdas it references don't.
    fn fits(&self, hir: HirId, budget: &mut usize) -> bool {
        let Some(rest) = budget.checked_sub(1) else {
            return false;
        };

        *budget = rest;

        let node = self.db.hir(hir);

        if let Hir::Block(block) = node {
            for statement in &block.statements {
                if let Statement::Let(symbol, _) = statement
                    && let Symbol::Binding(binding) = self.db.symbol(*symbol)
                    && !self.fits(binding.value.hir, budget)
                {
                    return false;
                }
            }
        }

        node.children().all(|child| self.fits(child, budget))
    }
}
