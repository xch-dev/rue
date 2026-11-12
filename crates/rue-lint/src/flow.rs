//! Follows where a value goes from the expression it's in, for lints that care whether a value
//! reaches the output of a function, or reaches a comparison.

use std::collections::{HashMap, HashSet};

use rue_diagnostic::SrcLoc;
use rue_hir::{Database, FieldAccess, Hir, HirId, Statement, Symbol, SymbolId, UnaryOp};

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

/// What it means for a value to decide something, as the condition of an `assert` or an `if`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Checks {
    /// Deciding something is what the lint is looking for, such as a check of a value.
    Reached,
    /// Deciding something only uses the value.
    Used,
}

impl Checks {
    fn fate(self, srcloc: Option<&SrcLoc>) -> Fate {
        match self {
            Self::Reached => Fate::Reached,
            Self::Used => srcloc.cloned().map_or(Fate::Dropped, Fate::Used),
        }
    }
}

/// Follows the value of the current node outward through the nodes that enclose it. `step` is
/// called with each enclosing node and the child that the value is in, and returns the fate of the
/// value if that node decides it, or `None` if the value goes on to the enclosing node.
///
/// Some nodes are handled the same way for every lint: a block passes its body's value on, a value
/// bound by `let` goes wherever the binding does, and an `if` passes its branches' values on. A
/// value that's returned, or that's the body of the function, a lambda, or a constant, reaches the
/// output. A value that's an `if` condition or that's called is used there.
pub(crate) fn follow(
    cx: &LintContext<'_>,
    hir: HirId,
    checks: Checks,
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
                        Statement::Assert(value, srcloc) if *value == child => {
                            return checks.fate(Some(srcloc));
                        }
                        Statement::If(statement) if statement.condition == child => {
                            return checks.fate(Some(&statement.srcloc));
                        }
                        _ => {}
                    }
                }

                // Any other statement discards the value
                return Fate::Dropped;
            }
            // The value of a lambda's body is returned when it's called (unless the lint decides
            // otherwise), and a constant's value is evaluated at compile time
            Hir::Lambda(_) => return step(db, parent, child).unwrap_or(Fate::Reached),
            Hir::Const(_) => return Fate::Reached,
            // The condition of an `if` only decides which branch is taken, and the value of a
            // branch is the value of the `if`
            Hir::If(condition, ..) if *condition == child => {
                return checks.fate(db.hir_srcloc(parent));
            }
            Hir::If(..) => {}
            // Calling the value uses it, rather than passing it on
            Hir::FunctionCall(call) if call.function == child => return used(db, parent),
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

/// The bindings that each binding is destructured into, such as `a` and `b` in
/// `let (a, b) = value`, whose values are parts of it.
pub(crate) type Parts = HashMap<SymbolId, Vec<SymbolId>>;

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

    /// Whether a value with the given fate reaches what the lint is looking for, collecting the
    /// places it's used on the way if `uses` is given.
    pub(crate) fn reaches(
        &self,
        fate: &Fate,
        parts: &Parts,
        mut uses: Option<&mut Vec<SrcLoc>>,
    ) -> bool {
        self.reaches_impl(fate, parts, &mut HashSet::new(), &mut uses)
    }

    fn reaches_impl(
        &self,
        fate: &Fate,
        parts: &Parts,
        visited: &mut HashSet<SymbolId>,
        uses: &mut Option<&mut Vec<SrcLoc>>,
    ) -> bool {
        match fate {
            Fate::Reached => true,
            Fate::Bound(symbol) => {
                if !visited.insert(*symbol) {
                    return false;
                }

                let fates = self.references.get(symbol).map_or(&[][..], Vec::as_slice);
                let symbol_parts = parts.get(symbol).map_or(&[][..], Vec::as_slice);

                // Every reference is followed, so that all of the places it's used are collected,
                // and so is every binding it's destructured into
                let mut reached = false;

                for fate in fates {
                    reached |= self.reaches_impl(fate, parts, visited, uses);
                }

                for &part in symbol_parts {
                    reached |= self.reaches_impl(&Fate::Bound(part), parts, visited, uses);
                }

                reached
            }
            Fate::Used(srcloc) => {
                if let Some(uses) = uses {
                    uses.push(srcloc.clone());
                }
                false
            }
            Fate::Dropped => false,
        }
    }
}

/// Finds the bindings that other bindings are destructured into. The values of the parts are
/// generated, so they aren't walked.
pub(crate) fn binding_parts(db: &Database) -> Parts {
    let mut parts = Parts::new();

    for (symbol, value) in db.symbols().filter_map(|(symbol, binding)| match binding {
        Symbol::Binding(binding) => Some((symbol, binding.value.hir)),
        _ => None,
    }) {
        let mut hir = value;

        // A part is the value, a field of it, or the first or rest of it. The value of a named
        // binding in a destructuring is a reference to the part it names.
        let whole = loop {
            match db.hir(hir) {
                Hir::Cast(inner)
                | Hir::FieldAccess(FieldAccess { value: inner, .. })
                | Hir::Unary(UnaryOp::First | UnaryOp::Rest, inner) => hir = *inner,
                Hir::Reference(whole) => break Some(*whole),
                _ => break None,
            }
        };

        if let Some(whole) = whole
            && matches!(db.symbol(whole), Symbol::Binding(_))
        {
            parts.entry(whole).or_default().push(symbol);
        }
    }

    parts
}
