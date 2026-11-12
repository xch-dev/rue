use rue_diagnostic::{Lint, Note, SrcLoc};
use rue_hir::{Declaration, Hir, HirId, UnaryOp};
use rue_types::TypeId;

use crate::{
    HirLintPass, LintContext,
    flow::{Bindings, Fate, follow, used},
};

/// Reports conditions (values of the standard library's `Condition` type) that are built and used,
/// but never reach the output of the function they're built in. A condition only has an effect if
/// it's output by the puzzle, so one that's only read from (or only used in a branch that drops
/// it) is likely a mistake. Conditions that aren't used at all are already reported by
/// `unused_binding` and `unused_value`.
#[derive(Debug, Default)]
pub(crate) struct DiscardedCondition {
    /// Each condition that's built, along with where its value goes.
    conditions: Vec<(SrcLoc, Fate)>,
    bindings: Bindings,
}

impl HirLintPass for DiscardedCondition {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::DiscardedCondition]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        match node {
            Hir::Reference(symbol) => {
                self.bindings
                    .reference(cx.db(), *symbol, || output_fate(cx, hir));
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

                let Some(condition) = condition_type(cx, hir) else {
                    return;
                };

                if !cx.contains_unresolved(initializer.ty)
                    && cx.is_assignable(initializer.ty, condition)
                {
                    self.conditions.push((srcloc, output_fate(cx, hir)));
                }
            }
            _ => {}
        }
    }

    fn finish(&mut self, cx: &mut LintContext<'_>) {
        for (srcloc, fate) in std::mem::take(&mut self.conditions) {
            let mut uses = Vec::new();

            if self.bindings.reaches(&fate, &mut uses) {
                continue;
            }

            // A condition that isn't used at all is reported by other lints
            let Some(read) = uses.into_iter().next() else {
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

/// Whether the value of the current node reaches the output of the function.
fn output_fate(cx: &LintContext<'_>, hir: HirId) -> Fate {
    follow(cx, hir, |db, parent, child| match db.hir(parent) {
        // The value is part of a larger value, which goes wherever that does, or part of it is
        // taken out (which may be the condition itself, such as `pair.first`)
        Hir::Pair(..)
        | Hir::List(_)
        | Hir::Struct(_)
        | Hir::Cast(_)
        | Hir::FieldAccess(_)
        | Hir::Unary(UnaryOp::First | UnaryOp::Rest, _) => None,
        // The value of a branch is the value of the `if`
        Hir::If(condition, ..) => (*condition == child).then(|| used(db, parent)),
        // Passing the value to a function could output it, which isn't followed
        Hir::FunctionCall(call) => Some(if call.function == child {
            used(db, parent)
        } else {
            Fate::Reached
        }),
        // Anything else reads the value, such as a guard or a comparison
        _ => Some(used(db, parent)),
    })
}

/// The `Condition` type as it's named in the scope of the node, which is the standard library's
/// unless it's shadowed.
fn condition_type(cx: &LintContext<'_>, hir: HirId) -> Option<TypeId> {
    let db = cx.db();
    let (ty, _) = db.resolve_type_in(db.hir_scope(hir)?, "Condition")?;
    Some(ty)
}
