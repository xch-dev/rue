//! Follows where a value goes from the expression it's in, for lints that care whether a value
//! reaches the output of a function, or reaches a comparison.

use std::collections::{HashMap, HashSet};

use rue_diagnostic::SrcLoc;
use rue_hir::{Database, Hir, HirId, Statement, Symbol, SymbolId};

use crate::LintContext;

/// Where a value goes, from the expression it's in.
#[derive(Debug, Clone)]
pub(crate) enum Fate {
    /// It reaches what the lint is looking for, such as the output of the function.
    Reached,
    /// It's the value of a binding, so it goes wherever the binding does.
    Bound(SymbolId),
    /// It's used here, in a way that doesn't reach what the lint is looking for.
    Used(SrcLoc),
    /// It isn't used.
    Dropped,
}

/// Follows the value of the current node outward through the nodes that enclose it. `step` is
/// called with each enclosing node and the child that the value is in, and returns the fate of the
/// value if that node decides it, or `None` if the value goes on to the enclosing node.
///
/// Some nodes are handled the same way for every lint: a block passes its body's value on, and a
/// value bound by `let` goes wherever the binding does. A value that's returned, or that's the body
/// of the function, a lambda, or a constant, reaches the output.
pub(crate) fn follow(
    cx: &LintContext<'_>,
    hir: HirId,
    mut step: impl FnMut(&Database, HirId, HirId) -> Option<Fate>,
) -> Fate {
    let db = cx.db();
    let mut child = hir;

    for &parent in cx.parents().iter().rev() {
        match db.hir(parent) {
            Hir::Block(block) => {
                if block.body == Some(child) {
                    child = parent;
                    continue;
                }

                for statement in &block.statements {
                    match statement {
                        Statement::Let(symbol, _) => {
                            if let Symbol::Binding(binding) = db.symbol(*symbol)
                                && binding.value.hir == child
                            {
                                return Fate::Bound(*symbol);
                            }
                        }
                        Statement::Return(value, _) if *value == child => return Fate::Reached,
                        _ => {}
                    }
                }

                // Any other statement discards the value, or only reads it (such as `assert`)
                return Fate::Dropped;
            }
            // The value of a lambda's body is returned when it's called, and a constant's value is
            // evaluated at compile time
            Hir::Lambda(_) | Hir::Const(_) => return Fate::Reached,
            _ => {
                if let Some(fate) = step(db, parent, child) {
                    return fate;
                }
            }
        }

        child = parent;
    }

    // The value is what the function returns
    Fate::Reached
}

/// The fate of a value that's used by a node, at the node's location.
pub(crate) fn used(db: &Database, hir: HirId) -> Fate {
    db.hir_srcloc(hir)
        .cloned()
        .map_or(Fate::Dropped, Fate::Used)
}

/// The fates of the references to each binding, so that a value that's bound can be followed to
/// wherever the binding is used.
#[derive(Debug, Default)]
pub(crate) struct Bindings {
    references: HashMap<SymbolId, Vec<Fate>>,
}

impl Bindings {
    /// Records the fate of the value at a reference, if it's a reference to a binding.
    pub(crate) fn reference(
        &mut self,
        db: &Database,
        symbol: SymbolId,
        fate: impl FnOnce() -> Fate,
    ) {
        if matches!(db.symbol(symbol), Symbol::Binding(_)) {
            self.references.entry(symbol).or_default().push(fate());
        }
    }

    /// Whether a value with the given fate reaches what the lint is looking for, along with the
    /// places it's used on the way.
    pub(crate) fn reaches(&self, fate: &Fate, uses: &mut Vec<SrcLoc>) -> bool {
        self.reaches_impl(fate, &mut HashSet::new(), uses)
    }

    fn reaches_impl(
        &self,
        fate: &Fate,
        visited: &mut HashSet<SymbolId>,
        uses: &mut Vec<SrcLoc>,
    ) -> bool {
        match fate {
            Fate::Reached => true,
            Fate::Bound(symbol) => {
                if !visited.insert(*symbol) {
                    return false;
                }

                let fates = self.references.get(symbol).map_or(&[][..], Vec::as_slice);

                // Every reference is followed, so that all of the places it's used are collected
                let mut reached = false;

                for fate in fates {
                    reached |= self.reaches_impl(fate, visited, uses);
                }

                reached
            }
            Fate::Used(srcloc) => {
                uses.push(srcloc.clone());
                false
            }
            Fate::Dropped => false,
        }
    }
}
