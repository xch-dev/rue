use std::collections::{HashMap, HashSet};

use rue_diagnostic::{Lint, Note, SrcLoc};
use rue_hir::{Declaration, Hir, HirId, Statement, Symbol, SymbolId, UnaryOp};
use rue_types::TypeId;

use crate::{HirLintPass, LintContext};

/// Reports conditions (values of the standard library's `Condition` type) that are built and used,
/// but never reach the output of the function they're built in. A condition only has an effect if
/// it's output by the puzzle, so one that's only read from (or only used in a branch that drops
/// it) is likely a mistake. Conditions that aren't used at all are already reported by
/// `unused_binding` and `unused_value`.
#[derive(Debug, Default)]
pub(crate) struct DiscardedCondition {
    /// Each condition that's built, along with where its value goes.
    conditions: Vec<(SrcLoc, Fate)>,
    /// Where the value goes at each reference to a binding.
    references: HashMap<SymbolId, Vec<Fate>>,
}

/// Where a value goes, from the expression it's in.
#[derive(Debug, Clone)]
enum Fate {
    /// It's part of what the function returns, or it's passed to another function.
    Output,
    /// It's the value of a binding, so it goes wherever the binding does.
    Bound(SymbolId),
    /// It's only read from, such as by a field access or a comparison.
    Read(SrcLoc),
    /// It isn't used.
    Dropped,
}

impl HirLintPass for DiscardedCondition {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::DiscardedCondition]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        match node {
            Hir::Reference(symbol) => {
                if matches!(cx.db().symbol(*symbol), Symbol::Binding(_)) {
                    let fate = fate(cx, hir);
                    self.references.entry(*symbol).or_default().push(fate);
                }
            }
            Hir::Struct(initializer) => {
                let Some(srcloc) = cx.db().hir_srcloc(hir).cloned() else {
                    return;
                };

                // Tests don't output anything, and often build conditions only to check them
                if let Some(Declaration::Symbol(symbol)) = cx.enclosing_declaration()
                    && cx.db().tests().any(|test| test.symbol == symbol)
                {
                    return;
                }

                let Some(condition) = Self::condition_type(cx, hir) else {
                    return;
                };

                if !cx.contains_unresolved(initializer.ty) && cx.is_assignable(initializer.ty, condition)
                {
                    let fate = fate(cx, hir);
                    self.conditions.push((srcloc, fate));
                }
            }
            _ => {}
        }
    }

    fn finish(&mut self, cx: &mut LintContext<'_>) {
        for (srcloc, fate) in std::mem::take(&mut self.conditions) {
            let mut reads = Vec::new();

            if self.is_output(&fate, &mut HashSet::new(), &mut reads) {
                continue;
            }

            // A condition that isn't used at all is reported by other lints
            let Some(read) = reads.into_iter().next() else {
                continue;
            };

            if let Some(diagnostic) = cx.report(
                Lint::DiscardedCondition,
                srcloc,
                "This condition is used, but it's never output, so it has no effect",
            ) {
                diagnostic
                    .notes
                    .push(Note::at(read, "It's only read from here"));
                diagnostic.notes.push(Note::new(
                    "Add it to the conditions that are returned, or remove it if it isn't needed",
                ));
            }
        }
    }
}

impl DiscardedCondition {
    /// The `Condition` type as it's named in the scope of the node, which is the standard
    /// library's unless it's shadowed.
    fn condition_type(cx: &LintContext<'_>, hir: HirId) -> Option<TypeId> {
        let db = cx.db();
        let mut scope = db.hir_scope(hir);

        while let Some(current) = scope {
            if let Some(ty) = db.scope(current).ty("Condition") {
                return Some(ty);
            }

            scope = db.scope(current).parent();
        }

        None
    }

    /// Whether a value with the given fate reaches the output, collecting where it's read from.
    fn is_output(
        &self,
        fate: &Fate,
        visited: &mut HashSet<SymbolId>,
        reads: &mut Vec<SrcLoc>,
    ) -> bool {
        match fate {
            Fate::Output => true,
            Fate::Bound(symbol) => {
                if !visited.insert(*symbol) {
                    return false;
                }

                let fates = self.references.get(symbol).map_or(&[][..], Vec::as_slice);

                // Every reference is checked, so that the places it's read from are all collected
                let mut output = false;

                for fate in fates {
                    output |= self.is_output(fate, visited, reads);
                }

                output
            }
            Fate::Read(srcloc) => {
                reads.push(srcloc.clone());
                false
            }
            Fate::Dropped => false,
        }
    }
}

/// Where the value of the current node goes, from the nodes that enclose it.
fn fate(cx: &LintContext<'_>, hir: HirId) -> Fate {
    let db = cx.db();
    let mut child = hir;

    for &parent in cx.parents().iter().rev() {
        let read = || {
            db.hir_srcloc(parent)
                .cloned()
                .map_or(Fate::Dropped, Fate::Read)
        };

        match db.hir(parent) {
            // The value is part of a larger value, which goes wherever that does, or part of it is
            // taken out (which may be the condition itself, such as `pair.first`)
            Hir::Pair(..)
            | Hir::List(_)
            | Hir::Struct(_)
            | Hir::Cast(_)
            | Hir::FieldAccess(_)
            | Hir::Unary(UnaryOp::First | UnaryOp::Rest, _) => {}
            // The value of a branch is the value of the `if`
            Hir::If(condition, ..) => {
                if *condition == child {
                    return read();
                }
            }
            Hir::Block(block) => {
                if block.body == Some(child) {
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
                        Statement::Return(value, _) if *value == child => return Fate::Output,
                        _ => {}
                    }
                }

                // Any other statement discards the value, or only reads it (such as `assert`)
                return Fate::Dropped;
            }
            // Passing the value to a function could output it, which isn't followed
            Hir::FunctionCall(call) => {
                if call.function == child {
                    return read();
                }

                return Fate::Output;
            }
            // The value of a lambda's body is returned when it's called, and a constant's value is
            // evaluated at compile time
            Hir::Lambda(_) | Hir::Const(_) => return Fate::Output,
            // Anything else reads the value, such as a field access, a guard or a comparison
            _ => return read(),
        }

        child = parent;
    }

    // The value is what the function returns
    Fate::Output
}
