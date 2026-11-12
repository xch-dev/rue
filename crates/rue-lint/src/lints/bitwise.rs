use std::collections::HashMap;

use rue_diagnostic::{Applicability, Lint, Note, SrcLoc, Suggestion};
use rue_hir::{BinaryOp, Hir, HirId, SymbolId, may_raise};
use rue_lexer::{Lexer, TokenKind};

use crate::{
    HirLintPass, LintContext,
    cost::{COST_PER_BYTE, Cost, CostModel},
};

/// The size of short-circuiting with `&&` or `||` rather than evaluating both sides with `&` or
/// `|`, since it branches on the left side. The right side must cost more than this to run for
/// short-circuiting to be cheaper.
const SHORT_CIRCUIT_SIZE: u64 = 17;

/// `&` and `|` on `Bool` evaluate both sides, which is cheaper than branching when the right side
/// is cheap. That's often on purpose, so this is only reported if the right side can raise (which
/// it does even when the left side decides the result), or costs more to evaluate than branching.
#[derive(Debug, Default)]
pub(crate) struct BoolBitwiseOperator {
    costs: HashMap<SymbolId, Option<Cost>>,
}

impl HirLintPass for BoolBitwiseOperator {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::BoolBitwiseOperator]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, _hir: HirId, node: &Hir) {
        let Hir::Binary(op @ (BinaryOp::All | BinaryOp::Any), left, right) = *node else {
            return;
        };

        let db = cx.db();

        let (Some(left_srcloc), Some(right_srcloc)) = (db.hir_srcloc(left), db.hir_srcloc(right))
        else {
            return;
        };

        let Some(operator) = operator_srcloc(left_srcloc, right_srcloc) else {
            return;
        };

        let (symbol, short_circuit, decides) = if op == BinaryOp::All {
            ("&", "&&", "false")
        } else {
            ("|", "||", "true")
        };

        let reason = if may_raise(db, right) {
            "it can raise".to_string()
        } else {
            let runtime = CostModel::new(db, &mut self.costs).estimate(right).runtime;

            if runtime <= SHORT_CIRCUIT_SIZE * COST_PER_BYTE {
                return;
            }

            format!("it has an estimated cost of {runtime}")
        };

        if let Some(diagnostic) = cx.report(
            Lint::BoolBitwiseOperator,
            operator.clone(),
            format!(
                "The right side of `{symbol}` is evaluated even when the left side is {decides}, and {reason}"
            ),
        ) {
            diagnostic.notes.push(Note::new(format!(
                "`{short_circuit}` only evaluates the right side when it's needed"
            )));
            diagnostic.suggestions.push(Suggestion::replace(
                format!("Use `{short_circuit}` instead"),
                operator,
                short_circuit,
                // The right side no longer raises or runs when the left side decides the result
                Applicability::MaybeIncorrect,
            ));
        }
    }
}

/// The location of the operator between two operands, which is the first token after the left
/// operand that isn't whitespace or a comment.
fn operator_srcloc(left: &SrcLoc, right: &SrcLoc) -> Option<SrcLoc> {
    if left.source.kind != right.source.kind || left.span.end > right.span.start {
        return None;
    }

    let start = left.span.end;
    let text = &left.source.text[start..right.span.start];

    let token = Lexer::new(text).find(|token| {
        !matches!(
            token.kind,
            TokenKind::Whitespace | TokenKind::LineComment | TokenKind::BlockComment { .. }
        )
    })?;

    if !matches!(token.kind, TokenKind::And | TokenKind::Or) {
        return None;
    }

    Some(SrcLoc::new(
        left.source.clone(),
        start + token.span.start..start + token.span.end,
    ))
}
