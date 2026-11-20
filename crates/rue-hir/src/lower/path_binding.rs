//! Decides which bindings that are referenced more than once are evaluated where they're
//! referenced anyway, since their value is only a path into the environment.
//!
//! A path, such as a field of a parameter, costs the same to evaluate anywhere, and it can't fail
//! as far as the failure analysis trusts the shapes of values (see the [`crate::analysis`] module
//! docs), which is the same trust that lets a binding that's referenced once be evaluated where
//! it's referenced. It's usually a single byte, so reading it at each reference is smaller than
//! binding it, which takes at least five bytes more than the value itself (`(c VALUE REST)`).
//!
//! It isn't always smaller: a path that's deep enough needs a longer atom at each reference, and
//! a bound value can end up being folded into something shorter, such as a pair of two bindings
//! that's the pair they're bound in. So the rest of the block is lowered both ways, and the
//! smaller one is kept (see [`super::measure`]).
//!
//! The value can go through bindings and arguments that are substituted, as long as it's still
//! only a path, so nothing that may fail or that's expensive to evaluate is ever repeated or
//! moved.

use indexmap::IndexSet;
use rue_lir::LirId;

use crate::{FieldAccess, FieldKind, Hir, HirId, Symbol, SymbolId};

use super::Lowerer;

impl Lowerer<'_, '_, '_> {
    /// Whether the binding, which is referenced more than once and isn't `inline`, is evaluated
    /// where it's referenced rather than bound. It isn't until its let statement is lowered.
    pub(super) fn is_path_binding(&self, symbol: SymbolId) -> bool {
        self.path_bindings
            .borrow()
            .get(&symbol)
            .copied()
            .unwrap_or(false)
    }

    /// Lowers a run of let statements with `lower`, after deciding which of their bindings are
    /// evaluated where they're referenced since they're only paths.
    ///
    /// Each such binding is decided the first time its let statement is lowered, by lowering the
    /// run with all of them evaluated where they're referenced, and then with each one bound
    /// instead, keeping it bound if that's smaller. The decision is remembered, so that the run
    /// isn't measured again when it's lowered again, which it is when an enclosing run is
    /// measured, or the function it's in is inlined somewhere else. It's only applied if the
    /// value is still a path there, since the arguments of an inlined call can differ.
    pub(super) fn lower_path_bindings(
        &mut self,
        symbols: &IndexSet<SymbolId>,
        mut lower: impl FnMut(&mut Self) -> LirId,
    ) -> LirId {
        let mut candidates = Vec::new();

        if self.options.auto_inline {
            // These are found in order, since a path can go through an earlier binding
            for &symbol in symbols {
                if let Symbol::Binding(binding) = self.db.symbol(symbol)
                    && !binding.inline
                    && self.graph.references(symbol) > 1
                    && self.is_path(binding.value.hir)
                {
                    self.path_bindings.borrow_mut().insert(symbol, true);
                    candidates.push(symbol);
                }
            }
        }

        let mut undecided = Vec::new();

        for symbol in candidates {
            match self.path_choices.get(&symbol) {
                Some(&is_path) => {
                    self.path_bindings.borrow_mut().insert(symbol, is_path);
                }
                None => undecided.push(symbol),
            }
        }

        if undecided.is_empty() {
            return lower(self);
        }

        let mut best = self.trial(&mut lower);

        for symbol in undecided {
            self.path_bindings.borrow_mut().insert(symbol, false);
            let bound = self.trial(&mut lower);
            let is_path = bound.size >= best.size;

            if is_path {
                self.path_bindings.borrow_mut().insert(symbol, true);
            } else {
                best = bound;
            }

            self.path_choices.insert(symbol, is_path);
        }

        self.keep(best)
    }

    /// Whether the value is only a path: fields of a value that's read from the environment,
    /// through any casts, and through the bindings, constants and arguments that are substituted
    /// where they're referenced.
    fn is_path(&self, hir: HirId) -> bool {
        // This follows the values that are substituted, which can go as deep as a chain of them
        rue_types::grow_stack(|| self.is_path_inner(hir))
    }

    fn is_path_inner(&self, hir: HirId) -> bool {
        match self.db.hir(hir) {
            Hir::Cast(value)
            | Hir::FieldAccess(FieldAccess {
                value,
                kind: FieldKind::Path(_),
                ..
            }) => self.is_path(*value),
            Hir::Reference(symbol) => {
                if let Some(value) = self.substituted_value(*symbol) {
                    return self.is_path(value);
                }

                // A function that's referenced as a value is a closure
                matches!(
                    self.db.symbol(*symbol),
                    Symbol::Parameter(_) | Symbol::Binding(_) | Symbol::Constant(_)
                )
            }
            _ => false,
        }
    }

    /// Forgets which of the bindings are paths, since their let statements are being lowered
    /// again, which might be in a call to an `inline fn` with different arguments than before.
    /// Returns what was decided before, which is restored with [`Self::restore_path_bindings`]
    /// once the statements are lowered.
    pub(super) fn forget_path_bindings(
        &self,
        symbols: &IndexSet<SymbolId>,
    ) -> Vec<(SymbolId, Option<bool>)> {
        let mut path_bindings = self.path_bindings.borrow_mut();

        symbols
            .iter()
            .map(|&symbol| (symbol, path_bindings.remove(&symbol)))
            .collect()
    }

    /// Restores what was decided before [`Self::forget_path_bindings`].
    pub(super) fn restore_path_bindings(&self, saved: Vec<(SymbolId, Option<bool>)>) {
        let mut path_bindings = self.path_bindings.borrow_mut();

        for (symbol, decided) in saved {
            match decided {
                Some(decided) => path_bindings.insert(symbol, decided),
                None => path_bindings.remove(&symbol),
            };
        }
    }
}
