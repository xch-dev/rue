use rue_diagnostic::{Applicability, Lint, SrcLoc, Suggestion};
use rue_hir::{Block, Hir, HirId, Statement, Symbol};

use crate::{
    HirLintPass, LintContext,
    text::{has_comment, remove_line},
};

/// Expression statements discard their value, so it should always be nil.
#[derive(Debug, Default)]
pub(crate) struct UnusedValue;

impl HirLintPass for UnusedValue {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::UnusedValue]
    }

    fn check_statement(&mut self, cx: &mut LintContext<'_>, block: &Block, index: usize) {
        if let Statement::Expr(statement) = &block.statements[index]
            && !statement.always_nil
        {
            cx.report(
                Lint::UnusedValue,
                statement.srcloc.clone(),
                "Value returned by statement is unused",
            );
        }
    }
}

/// `debug` statements print a value while debugging, and are usually left in by mistake.
#[derive(Debug, Default)]
pub(crate) struct DebugStatement;

impl HirLintPass for DebugStatement {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::DebugStatement]
    }

    fn check_statement(&mut self, cx: &mut LintContext<'_>, block: &Block, index: usize) {
        let Statement::Debug(_, srcloc) = &block.statements[index] else {
            return;
        };

        if let Some(diagnostic) =
            cx.report(Lint::DebugStatement, srcloc.clone(), "`debug` statement")
        {
            diagnostic.suggestions.push(remove_line(
                "Remove the `debug` statement",
                srcloc,
                Applicability::MachineApplicable,
            ));
        }
    }
}

/// A block that ends by returning the binding declared directly before can end in its value.
#[derive(Debug, Default)]
pub(crate) struct NeedlessLetReturn;

impl HirLintPass for NeedlessLetReturn {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::NeedlessLetReturn]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, _hir: HirId, node: &Hir) {
        let Hir::Block(block) = node else {
            return;
        };

        let (Some(Statement::Let(symbol, let_srcloc)), Some(body)) =
            (block.statements.last(), block.body)
        else {
            return;
        };

        let db = cx.db();

        if !matches!(db.hir(body), Hir::Reference(reference) if reference == symbol) {
            return;
        }

        let Symbol::Binding(binding) = db.symbol(*symbol) else {
            return;
        };

        let (Some(name), Some(body_srcloc), Some(value_srcloc)) = (
            binding.name.clone(),
            db.hir_srcloc(body),
            db.hir_srcloc(binding.value.hir),
        ) else {
            return;
        };

        let span = SrcLoc::new(
            let_srcloc.source.clone(),
            let_srcloc.span.start..body_srcloc.span.end,
        );

        // A type annotation can change the type of the block, so it can't be removed, and neither
        // can any attributes on the binding
        let annotated = db.hir_type(binding.value.hir) != Some(binding.value.ty);
        let has_attributes = let_srcloc.text().starts_with('#');
        let replacement = value_srcloc.text().to_string();
        let applicability = if has_comment(span.text()) {
            Applicability::MaybeIncorrect
        } else {
            Applicability::MachineApplicable
        };

        let Some(diagnostic) = cx.report(
            Lint::NeedlessLetReturn,
            let_srcloc.clone(),
            format!(
                "Binding `{}` is returned directly after it's declared",
                name.text()
            ),
        ) else {
            return;
        };

        if !annotated && !has_attributes {
            diagnostic.suggestions.push(Suggestion::replace(
                "Return the value directly",
                span,
                replacement,
                applicability,
            ));
        }
    }
}
