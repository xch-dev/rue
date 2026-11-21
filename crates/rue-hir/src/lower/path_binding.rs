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
use rue_lir::{LirId, first_path, path_to_atom, rest_path};

use crate::{Environment, FieldAccess, FieldKind, Hir, HirId, Symbol, SymbolId, TypePath};

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
    /// somewhere else. It's remembered for the size of the path where the run is lowered, since
    /// the same binding can be a longer path somewhere else, such as in another call of an
    /// `inline fn`, or once a binding of an enclosing run is bound, and it's measured again
    /// there. It's only applied if the value is still a path there at all.
    ///
    /// Each measurement lowers the rest of the block again, so no more than
    /// [`MAX_PATH_CANDIDATES`] bindings of a run are measured, and the rest are bound.
    pub(super) fn lower_path_bindings(
        &mut self,
        env: &Environment,
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

                let key = (symbol, self.path_size(env, binding.value.hir));

                let is_path = match self.path_choices.get(&key) {
                    Some(&is_path) => is_path,
                    None if undecided.len() < MAX_PATH_CANDIDATES => {
                        undecided.push(key);
                        true
                    }
                    None => {
                        self.path_choices.insert(key, false);
                        false
                    }
                };

                self.path_bindings.borrow_mut().insert(symbol, is_path);
            }
        }

        if undecided.is_empty() {
            return lower(self);
        }

        let symbols: Vec<SymbolId> = undecided.iter().map(|&(symbol, _)| symbol).collect();
        let mut best = self.trial(&mut lower);
        let mut are_paths = true;

        if symbols.len() > 1 {
            self.set_path_bindings(&symbols, false);
            let bound = self.trial(&mut lower);

            if bound.size < best.size {
                best = bound;
                are_paths = false;
            } else {
                self.set_path_bindings(&symbols, true);
            }
        }

        let mut choices = Vec::with_capacity(undecided.len());

        for &(symbol, size) in &undecided {
            self.set_path_bindings(&[symbol], !are_paths);
            let switched = self.trial(&mut lower);

            if switched.size < best.size {
                best = switched;
                choices.push(((symbol, size), !are_paths));
            } else {
                self.set_path_bindings(&[symbol], are_paths);
                choices.push(((symbol, size), are_paths));
            }
        }

        // If every way of lowering the run went too deep, nothing was measured, which can be
        // because of what an enclosing run is trying. The bindings are measured again the next
        // time the run is lowered.
        if best.size != usize::MAX {
            self.path_choices.extend(choices);
        }

        self.keep(best)
    }

    fn set_path_bindings(&self, symbols: &[SymbolId], is_path: bool) {
        let mut path_bindings = self.path_bindings.borrow_mut();

        for &symbol in symbols {
            path_bindings.insert(symbol, is_path);
        }
    }

    /// Whether the value is only a path (see [`Self::path_read`]).
    fn is_path(&self, hir: HirId) -> bool {
        self.path_read(hir).is_some()
    }

    /// How many bytes the path that the value is lowered to takes up in the program, where the
    /// run of let statements it's in is lowered, or zero if what it's read from isn't in the environment
    /// yet, which is the case for a binding of the same run.
    fn path_size(&self, env: &Environment, hir: HirId) -> usize {
        let Some((symbol, steps)) = self.path_read(hir) else {
            return 0;
        };

        let Ok(mut path) = env.path(symbol) else {
            return 0;
        };

        for step in steps {
            path = match step {
                TypePath::First => first_path(&path),
                TypePath::Rest => rest_path(&path),
            };
        }

        // An atom of one byte below 0x80 is serialized as it is, and a longer one after its
        // length
        match path_to_atom(&path).as_slice() {
            [byte] if *byte < 0x80 => 1,
            atom => atom.len() + 1,
        }
    }

    /// The symbol that the value is read from, and the path to the field of it that's read, if
    /// the value is only a path: fields of a value that's read from the environment, through any
    /// casts, and through the bindings, constants and arguments that are substituted where
    /// they're referenced.
    pub(super) fn path_read(&self, hir: HirId) -> Option<(SymbolId, Vec<TypePath>)> {
        // This follows the values that are substituted, which can go as deep as a chain of them
        rue_types::grow_stack(|| self.path_read_inner(hir))
    }

    fn path_read_inner(&self, hir: HirId) -> Option<(SymbolId, Vec<TypePath>)> {
        match self.db.hir(hir) {
            Hir::Cast(value) => self.path_read(*value),
            Hir::FieldAccess(FieldAccess {
                value,
                kind: FieldKind::Path(steps),
                ..
            }) => {
                let (symbol, mut path) = self.path_read(*value)?;
                path.extend(steps.iter().copied());
                Some((symbol, path))
            }
            Hir::Reference(symbol) => {
                if let Some(value) = self.substituted_value(*symbol) {
                    return self.path_read(value);
                }

                // A function that's referenced as a value is a closure
                matches!(
                    self.db.symbol(*symbol),
                    Symbol::Parameter(_) | Symbol::Binding(_) | Symbol::Constant(_)
                )
                .then(|| (*symbol, Vec::new()))
            }
            _ => None,
        }
    }
}
