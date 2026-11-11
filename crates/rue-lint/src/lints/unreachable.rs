use rue_diagnostic::{Applicability, Lint, Note, SrcLoc, Suggestion};
use rue_hir::{Block, Hir, HirId, Statement, Symbol};

use crate::{HirLintPass, LintContext};

/// Code after a statement that never finishes, such as a `return` or `raise`, is never evaluated.
#[derive(Debug, Default)]
pub(crate) struct UnreachableCode;

impl HirLintPass for UnreachableCode {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::UnreachableCode]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, _hir: HirId, node: &Hir) {
        let Hir::Block(block) = node else {
            return;
        };

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

        let diverging = block.statements[index].srcloc().clone();
        let srcloc = SrcLoc::new(first.source.clone(), first.span.start..last.span.end);

        let Some(diagnostic) = cx.report(Lint::UnreachableCode, srcloc, "Unreachable code") else {
            return;
        };

        diagnostic.notes.push(Note::at(
            diverging.clone(),
            "Any code after this is never evaluated",
        ));

        // Removing code might not be what was intended, such as if the `return` was left in by
        // mistake, so this isn't applied automatically
        diagnostic.suggestions.push(Suggestion::replace(
            "Remove the unreachable code",
            SrcLoc::new(diverging.source, diverging.span.end..last.span.end),
            "",
            Applicability::MaybeIncorrect,
        ));
    }
}

/// Whether evaluating the statement never finishes, so nothing after it is evaluated.
fn diverges(cx: &mut LintContext<'_>, statement: &Statement) -> bool {
    let hir = match statement {
        Statement::Return(..) | Statement::Raise(..) => return true,
        Statement::Expr(statement) => statement.hir,
        // Inline bindings are evaluated where they're referenced instead
        Statement::Let(symbol, _) => match cx.db().symbol(*symbol) {
            Symbol::Binding(binding) if !binding.inline => binding.value.hir,
            _ => return false,
        },
        Statement::If(_) | Statement::Assert(..) | Statement::Debug(..) => return false,
    };

    cx.db().hir_type(hir).is_some_and(|ty| cx.is_never(ty))
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
