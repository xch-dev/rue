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
//! smaller one is kept (see [`super::measure`]). Lowering it without any path bindings is one
//! of the ways that's measured, so a block is never larger than it was before there were any.
//!
//! The value can go through bindings and arguments that are substituted, as long as it's still
//! only a path, so nothing that may fail or that's expensive to evaluate is ever repeated or
//! moved.

use indexmap::IndexSet;
use rue_lir::{LirId, first_path, path_to_atom, rest_path};

use crate::{Environment, FieldAccess, FieldKind, Hir, HirId, Symbol, SymbolId, TypePath};

use super::{Lowerer, measure::Trial};

/// How many bindings of one run of let statements are measured (see
/// [`Lowerer::lower_path_bindings`]).
const MAX_PATH_CANDIDATES: usize = 8;

/// How many runs of let statements with path bindings a run can be inside of and still be
/// measured (see [`Lowerer::lower_path_bindings`]).
const MAX_MEASURED_RUN_DEPTH: usize = 2;

/// Whether reading a path of the size at each of the references can't take more bytes than
/// binding it: binding a value takes at least five bytes more than the value itself
/// (`(c VALUE REST)`), and each reference to it at least one. A path of one byte never does.
fn is_small_path(references: usize, size: usize) -> bool {
    references
        .saturating_sub(1)
        .saturating_mul(size.saturating_sub(1))
        <= 6
}

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
    /// A run that isn't inside another one that's being measured is first lowered with no
    /// binding read as a path, in it or in any run inside of it, which is how it was lowered
    /// before there were path bindings. Whatever is measured after that is only kept if it's
    /// smaller, so a run is never larger than it was.
    ///
    /// Each binding is then decided the first time its let statement is lowered. The run is
    /// lowered with all of them bound, and with all of them read where they're referenced, and
    /// then each one is switched on its own, and kept that way if that's smaller still. Binding
    /// the first value of a run takes more bytes than binding another one next to it, so
    /// several can be smaller bound together although none is on its own.
    ///
    /// The decision is remembered, so that the run isn't measured again when it's lowered again,
    /// which it is when an enclosing run is measured, or the function it's in is inlined
    /// somewhere else. It's remembered for the size of the path where the run is lowered, since
    /// the same binding can be a longer path somewhere else. What's smaller also depends on the
    /// other paths around it, so a remembered decision can be the larger one where it's reused,
    /// which is what the first lowering is there for. It's only applied if the value is still a
    /// path there at all.
    ///
    /// Reading a path where it's referenced nests no less deeply than binding it. So if lowering
    /// the run with its bindings bound goes too deep, every way of lowering it does, and nothing
    /// else is tried, which keeps a program that's too deep from being lowered over and over.
    ///
    /// Each measurement lowers the rest of the block again, and a run after another one is
    /// inside of it, so measuring every run of a long block would lower its end once for each
    /// run before it, for each of their measurements. So no more than [`MAX_PATH_CANDIDATES`]
    /// bindings of a run are measured, and the rest are bound, and only the runs that are fewer
    /// than [`MAX_MEASURED_RUN_DEPTH`] deep in other runs with path bindings are measured. The
    /// ones inside of those are decided by the size of the path alone (see
    /// [`is_small_path`]), which is usually right, and can't make the outermost run larger than
    /// it was either way.
    pub(super) fn lower_path_bindings(
        &mut self,
        env: &Environment,
        symbols: &IndexSet<SymbolId>,
        mut lower: impl FnMut(&mut Self) -> LirId,
    ) -> LirId {
        let mut decided = Vec::new();
        let mut undecided = Vec::new();

        let measured_depth = self.path_run_depth < MAX_MEASURED_RUN_DEPTH;

        if self.options.auto_inline && !self.paths_disabled {
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

                let size = self.path_size(env, binding.value.hir);
                let key = (symbol, size);

                if !measured_depth {
                    let references = self.graph.references(symbol);
                    decided.push((symbol, is_small_path(references, size)));
                    continue;
                }

                match self.path_choices.get(&key) {
                    Some(&is_path) => decided.push((symbol, is_path)),
                    None if undecided.len() < MAX_PATH_CANDIDATES => undecided.push(key),
                    None => {
                        self.path_choices.insert(key, false);
                        decided.push((symbol, false));
                    }
                }
            }
        }

        if undecided.is_empty() && !decided.iter().any(|&(_, is_path)| is_path) {
            return lower(self);
        }

        // Nothing is a path until it's set to be below
        let symbols: Vec<SymbolId> = undecided.iter().map(|&(symbol, _)| symbol).collect();
        let outermost = self.path_run_depth == 0;
        self.path_run_depth += 1;

        // A run inside another one that has nothing left to measure is lowered once
        if !outermost && undecided.is_empty() {
            for &(symbol, is_path) in &decided {
                self.set_path_bindings(&[symbol], is_path);
            }

            let lir = lower(self);
            self.path_run_depth -= 1;
            return lir;
        }

        let mut best = if outermost {
            self.paths_disabled = true;
            let before = self.trial(&mut lower);
            self.paths_disabled = false;
            Some(before)
        } else {
            None
        };

        if !best.as_ref().is_some_and(Trial::is_too_deep) {
            for &(symbol, is_path) in &decided {
                self.set_path_bindings(&[symbol], is_path);
            }

            let bound = self.trial(&mut lower);
            let too_deep = bound.is_too_deep();
            let mut are_paths = false;
            let mut measured = bound;

            if !too_deep && !symbols.is_empty() {
                self.set_path_bindings(&symbols, true);
                let paths = self.trial(&mut lower);

                if paths.size <= measured.size {
                    measured = paths;
                    are_paths = true;
                } else {
                    self.set_path_bindings(&symbols, false);
                }

                for &(symbol, size) in &undecided {
                    self.set_path_bindings(&[symbol], !are_paths);
                    let switched = self.trial(&mut lower);

                    if switched.size < measured.size {
                        measured = switched;
                        self.path_choices.insert((symbol, size), !are_paths);
                    } else {
                        self.set_path_bindings(&[symbol], are_paths);
                        self.path_choices.insert((symbol, size), are_paths);
                    }
                }
            }

            if best
                .as_ref()
                .is_none_or(|before| measured.size < before.size)
            {
                best = Some(measured);
            }
        }

        self.path_run_depth -= 1;
        self.keep(best.expect("the run was lowered at least once"))
    }

    fn set_path_bindings(&self, symbols: &[SymbolId], is_path: bool) {
        let mut path_bindings = self.path_bindings.borrow_mut();

        for &symbol in symbols {
            path_bindings.insert(symbol, is_path);
        }
    }

    /// Whether the value is only a path (see [`Self::path_read`]).
    pub(super) fn is_path(&self, hir: HirId) -> bool {
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
