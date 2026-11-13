use rue_diagnostic::{Applicability, Lint, Note, SrcLoc, Suggestion};
use rue_hir::{Block, Hir, HirId, Statement, Symbol};

use crate::{HirLintPass, LintContext, text::encloses};

/// Code after a statement that never finishes, such as a `return` or `raise`, is never evaluated.
#[derive(Debug, Default)]
pub(crate) struct UnreachableCode {
    /// The code that has been reported as unreachable, so that blocks inside of it aren't
    /// reported again.
    reported: Vec<SrcLoc>,
}

impl HirLintPass for UnreachableCode {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::UnreachableCode]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        let Hir::Block(block) = node else {
            return;
        };

        if let Some(srcloc) = cx.db().hir_srcloc(hir)
            && self
                .reported
                .iter()
                .any(|reported| encloses(reported, srcloc))
        {
            return;
        }

        let Some(index) = block
            .statements
            .iter()
            .position(|statement| diverges(cx, statement))
        else {
            return;
        };

        let Some((first, last)) = unreachable_range(cx, block, index) else {
            return;
        };

        let diverging = &block.statements[index];
        let diverging_srcloc = diverging.srcloc().clone();

        // Removing the code after a `return` or `raise` leaves a block that still ends correctly,
        // but removing it after any other statement could leave a block without a value
        let removable = matches!(diverging, Statement::Return(..) | Statement::Raise(..));

        self.reported.push(SrcLoc::new(
            first.source.clone(),
            first.span.start..last.span.end,
        ));

        // This is reported at the first unreachable statement or expression, as in Rust, so that
        // an attribute on it can change the level
        let Some(diagnostic) = cx.report(Lint::UnreachableCode, first, "Unreachable code") else {
            return;
        };

        diagnostic.notes.push(Note::at(
            diverging_srcloc.clone(),
            "Any code after this is never evaluated",
        ));

        // Removing code might not be what was intended, such as if the `return` was left in by
        // mistake, so this isn't applied automatically
        if removable {
            diagnostic.suggestions.push(Suggestion::replace(
                "Remove the unreachable code",
                SrcLoc::new(
                    diverging_srcloc.source,
                    diverging_srcloc.span.end..last.span.end,
                ),
                "",
                Applicability::MaybeIncorrect,
            ));
        }
    }
}

/// Whether evaluating the statement never finishes, so nothing after it is evaluated.
///
/// A `let` whose value never finishes is included, unless the binding is `inline`, since only
/// inline bindings are evaluated where they're referenced rather than at their statement.
fn diverges(cx: &mut LintContext<'_>, statement: &Statement) -> bool {
    match statement {
        Statement::Return(..) | Statement::Raise(..) => true,
        Statement::Expr(statement) => cx
            .db()
            .hir_type(statement.hir)
            .is_some_and(|ty| cx.is_never(ty)),
        // A binding that isn't inline is evaluated at its let statement
        Statement::Let(symbol, _) => match cx.db().symbol(*symbol) {
            Symbol::Binding(binding) if !binding.inline => {
                let ty = binding.value.ty;
                cx.is_never(ty)
            }
            _ => false,
        },
        Statement::If(_) | Statement::Assert(..) | Statement::Debug(..) => false,
    }
}

/// The locations of the first and last statement or expression after the statement at the index.
fn unreachable_range(
    cx: &LintContext<'_>,
    block: &Block,
    index: usize,
) -> Option<(SrcLoc, SrcLoc)> {
    let mut srclocs = block.statements[index + 1..]
        .iter()
        .map(|statement| statement.srcloc().clone())
        .chain(
            block
                .body
                .and_then(|body| cx.db().hir_srcloc(body).cloned()),
        );

    let first = srclocs.next()?;
    let last = srclocs.last().unwrap_or_else(|| first.clone());

    Some((first, last))
}
