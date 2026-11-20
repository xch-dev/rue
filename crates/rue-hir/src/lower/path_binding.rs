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
//! smaller one is kept (see [`super::measure`]). Lowering them all bound is one of the ways
//! that's measured, so a block is never larger than it was when they always were.
//!
//! The value can go through bindings and arguments that are substituted, as long as it's still
//! only a path, so nothing that may fail or that's expensive to evaluate is ever repeated or
//! moved.

use indexmap::IndexSet;
use rue_lir::LirId;

use crate::{FieldAccess, FieldKind, Hir, HirId, Symbol, SymbolId};

use super::Lowerer;

/// How many bindings of one run of let statements are measured (see
/// [`Lowerer::lower_path_bindings`]).
const MAX_PATH_CANDIDATES: usize = 8;

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
    /// Each such binding is decided the first time its let statement is lowered. The run is
    /// lowered with all of them read where they're referenced, and with all of them bound, which
    /// is how they were lowered before, so the smaller of the two is never larger than that.
    /// Then each one is switched on its own, and kept that way if that's smaller still. Binding
    /// the first value of a run takes more bytes than binding another one next to it, so
    /// several can be smaller bound together although none is on its own.
    ///
    /// The decision is remembered, so that the run isn't measured again when it's lowered again,
    /// which it is when an enclosing run is measured, or the function it's in is inlined
    /// somewhere else. It's only applied if the value is still a path there, since the
    /// arguments of an inlined call can differ.
    ///
    /// Each measurement lowers the rest of the block again, so no more than
    /// [`MAX_PATH_CANDIDATES`] bindings of a run are measured, and the rest are bound.
    pub(super) fn lower_path_bindings(
        &mut self,
        symbols: &IndexSet<SymbolId>,
        mut lower: impl FnMut(&mut Self) -> LirId,
    ) -> LirId {
        let mut undecided = Vec::new();

        if self.options.auto_inline {
            for &symbol in symbols {
                let Symbol::Binding(binding) = self.db.symbol(symbol) else {
                    continue;
                };

                if binding.inline
                    || self.graph.references(symbol) <= 1
                    || !self.is_path(binding.value.hir)
                {
                    continue;
                }

                let is_path = match self.path_choices.get(&symbol) {
                    Some(&is_path) => is_path,
                    None if undecided.len() < MAX_PATH_CANDIDATES => {
                        undecided.push(symbol);
                        true
                    }
                    None => false,
                };

                self.path_bindings.borrow_mut().insert(symbol, is_path);
            }
        }

        if undecided.is_empty() {
            return lower(self);
        }

        let mut best = self.trial(&mut lower);
        let mut are_paths = true;

        if undecided.len() > 1 {
            self.set_path_bindings(&undecided, false);
            let bound = self.trial(&mut lower);

            if bound.size < best.size {
                best = bound;
                are_paths = false;
            } else {
                self.set_path_bindings(&undecided, true);
            }
        }

        for &symbol in &undecided {
            self.set_path_bindings(&[symbol], !are_paths);
            let switched = self.trial(&mut lower);

            if switched.size < best.size {
                best = switched;
                self.path_choices.insert(symbol, !are_paths);
            } else {
                self.set_path_bindings(&[symbol], are_paths);
                self.path_choices.insert(symbol, are_paths);
            }
        }

        self.keep(best)
    }

    fn set_path_bindings(&self, symbols: &[SymbolId], is_path: bool) {
        let mut path_bindings = self.path_bindings.borrow_mut();

        for &symbol in symbols {
            path_bindings.insert(symbol, is_path);
        }
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
}
