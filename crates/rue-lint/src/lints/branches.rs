use rue_diagnostic::{Applicability, Lint, Note, Suggestion};
use rue_hir::{BinaryOp, Block, Hir, HirId, Statement, UnaryOp};

use crate::{
    HirLintPass, LintContext,
    equality::hir_eq,
    text::{has_comment, negated_text, negated_text_at, position, text_at},
};

/// An `if` whose branches are the same evaluates to the same value whatever the condition is.
#[derive(Debug, Default)]
pub(crate) struct IdenticalBranches;

impl HirLintPass for IdenticalBranches {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::IdenticalBranches]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        let Hir::If(_, then, else_, _) = *node else {
            return;
        };

        let db = cx.db();

        let (Some(srcloc), Some(then_srcloc), Some(else_srcloc)) = (
            db.hir_srcloc(hir),
            db.hir_srcloc(then),
            db.hir_srcloc(else_),
        ) else {
            return;
        };

        if !hir_eq(db, then, else_) {
            return;
        }

        let srcloc = srcloc.clone();
        let then_text = then_srcloc.text().to_string();
        let else_srcloc = else_srcloc.clone();

        let Some(diagnostic) = cx.report(
            Lint::IdenticalBranches,
            srcloc.clone(),
            "Both branches of this `if` are the same, so the condition has no effect",
        ) else {
            return;
        };

        diagnostic
            .notes
            .push(Note::at(else_srcloc, "The `else` branch is the same"));

        // The condition could raise, so removing it might not be what was intended
        diagnostic.suggestions.push(Suggestion::replace(
            "Replace the `if` with one of its branches",
            srcloc,
            then_text,
            Applicability::MaybeIncorrect,
        ));
    }
}

/// A condition that repeats an earlier one in the same `else if` chain is never true when it's
/// checked, and one that repeats an earlier operand of `&&` or `||` has no effect.
#[derive(Debug, Default)]
pub(crate) struct DuplicateCondition;

impl HirLintPass for DuplicateCondition {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::DuplicateCondition]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        match *node {
            Hir::If(..) => check_else_if_chain(cx, hir),
            Hir::Binary(op, ..) if is_logical(op) => check_logical_chain(cx, hir, op),
            _ => {}
        }
    }
}

fn check_else_if_chain(cx: &mut LintContext<'_>, hir: HirId) {
    // Only the first `if` of a chain is checked, which includes the rest of it
    if let Some(parent) = cx.parent()
        && let Hir::If(_, _, else_, _) = *cx.db().hir(parent)
        && else_ == hir
    {
        return;
    }

    let mut conditions = Vec::new();
    let mut current = hir;

    while let Hir::If(condition, _, else_, _) = *cx.db().hir(current) {
        conditions.push(condition);
        current = else_;
    }

    report_duplicates(
        cx,
        &conditions,
        "This condition is the same as an earlier one in the `else if` chain, so its branch is never taken",
    );
}

fn check_logical_chain(cx: &mut LintContext<'_>, hir: HirId, op: BinaryOp) {
    // Only the outermost operator of a chain is checked, which includes the rest of it
    if let Some(parent) = cx.parent()
        && let Hir::Binary(parent_op, ..) = *cx.db().hir(parent)
        && parent_op == op
    {
        return;
    }

    let mut operands = Vec::new();
    flatten_logical(cx, hir, op, &mut operands);

    report_duplicates(
        cx,
        &operands,
        &format!("This operand of `{op}` is the same as an earlier one, so it has no effect"),
    );
}

fn flatten_logical(cx: &LintContext<'_>, hir: HirId, op: BinaryOp, operands: &mut Vec<HirId>) {
    match *cx.db().hir(hir) {
        // A parenthesized chain is still the same chain, but anything else isn't
        Hir::Binary(inner, left, right) if inner == op => {
            flatten_logical(cx, left, op, operands);
            flatten_logical(cx, right, op, operands);
        }
        _ => operands.push(hir),
    }
}

fn report_duplicates(cx: &mut LintContext<'_>, hirs: &[HirId], message: &str) {
    for (index, &hir) in hirs.iter().enumerate() {
        let Some(srcloc) = cx.db().hir_srcloc(hir).cloned() else {
            continue;
        };

        let Some(earlier) = hirs[..index]
            .iter()
            .find(|&&earlier| hir_eq(cx.db(), earlier, hir))
            .and_then(|&earlier| cx.db().hir_srcloc(earlier).cloned())
        else {
            continue;
        };

        if let Some(diagnostic) = cx.report(Lint::DuplicateCondition, srcloc, message) {
            diagnostic.notes.push(Note::at(
                earlier,
                "The same condition is checked here first",
            ));
        }
    }
}

fn is_logical(op: BinaryOp) -> bool {
    matches!(
        op,
        BinaryOp::And | BinaryOp::Or | BinaryOp::All | BinaryOp::Any
    )
}

/// An `if` that evaluates to `true` in one branch and `false` in the other is the same as its
/// condition (or its negation).
#[derive(Debug, Default)]
pub(crate) struct NeedlessBool;

impl HirLintPass for NeedlessBool {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::NeedlessBool]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        let Hir::If(condition, then, else_, _) = *node else {
            return;
        };

        let (Some(then), Some(else_)) = (bool_value(cx, then), bool_value(cx, else_)) else {
            return;
        };

        if then == else_ {
            return;
        }

        let Some(srcloc) = cx.db().hir_srcloc(hir).cloned() else {
            return;
        };

        let position = position(cx, hir);

        let replacement = if then {
            text_at(cx.db(), condition, position)
        } else {
            negated_text_at(cx.db(), condition, position)
        };

        let Some(replacement) = replacement else {
            return;
        };

        let applicability = if has_comment(srcloc.text()) {
            Applicability::MaybeIncorrect
        } else {
            Applicability::MachineApplicable
        };

        let Some(diagnostic) = cx.report(
            Lint::NeedlessBool,
            srcloc.clone(),
            "This `if` evaluates to a boolean that's the same as its condition",
        ) else {
            return;
        };

        diagnostic.suggestions.push(Suggestion::replace(
            if then {
                "Use the condition directly"
            } else {
                "Use the negation of the condition"
            },
            srcloc,
            replacement,
            applicability,
        ));
    }
}

/// The value of a `true` or `false` literal, or a block that only contains one.
fn bool_value(cx: &LintContext<'_>, hir: HirId) -> Option<bool> {
    let db = cx.db();
    let hir = db.without_casts(hir);

    match db.hir(hir) {
        Hir::Bool(value) if db.hir_srcloc(hir).is_some() => Some(*value),
        Hir::Block(Block {
            statements,
            body: Some(body),
        }) if statements.is_empty() => bool_value(cx, *body),
        _ => None,
    }
}

/// An `if` with a negated condition and an `else` branch is easier to read with the branches
/// swapped and the negation removed.
#[derive(Debug, Default)]
pub(crate) struct NegatedCondition;

impl HirLintPass for NegatedCondition {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::NegatedCondition]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        let Hir::If(condition, then, else_, inline) = *node else {
            return;
        };

        let db = cx.db();

        // Swapping the branches of an `else if` chain would reorder the chain
        if matches!(db.hir(else_), Hir::If(..)) {
            return;
        }

        if let Some(parent) = cx.parent()
            && let Hir::If(_, _, parent_else, _) = *db.hir(parent)
            && parent_else == hir
        {
            return;
        }

        let Some(condition_srcloc) = db.hir_srcloc(condition) else {
            return;
        };

        let negated = match *db.hir(condition) {
            Hir::Unary(UnaryOp::Not, _) => condition_srcloc.text().starts_with('!'),
            Hir::Binary(BinaryOp::Ne, ..) => true,
            _ => false,
        };

        if !negated {
            return;
        }

        let (Some(srcloc), Some(then_srcloc), Some(else_srcloc), Some(condition_text)) = (
            db.hir_srcloc(hir),
            db.hir_srcloc(then),
            db.hir_srcloc(else_),
            negated_text(db, condition),
        ) else {
            return;
        };

        let srcloc = srcloc.clone();
        let condition_srcloc = condition_srcloc.clone();
        let inline = if inline { "inline " } else { "" };
        let replacement = format!(
            "{inline}if {condition_text} {} else {}",
            else_srcloc.text(),
            then_srcloc.text()
        );

        let Some(diagnostic) = cx.report(
            Lint::NegatedCondition,
            condition_srcloc,
            "Negated condition in an `if` with an `else` branch",
        ) else {
            return;
        };

        diagnostic.suggestions.push(Suggestion::replace(
            "Remove the negation and swap the branches",
            srcloc,
            replacement,
            Applicability::MaybeIncorrect,
        ));
    }
}

/// An `if` statement that only raises is the same as an `assert` of the negated condition.
#[derive(Debug, Default)]
pub(crate) struct ManualAssert;

impl HirLintPass for ManualAssert {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::ManualAssert]
    }

    fn check_statement(&mut self, cx: &mut LintContext<'_>, block: &Block, index: usize) {
        let Statement::If(statement) = &block.statements[index] else {
            return;
        };

        // Both branches of an `inline if` are evaluated, so it isn't the same as an `assert`
        if statement.inline {
            return;
        }

        let db = cx.db();

        let Hir::Block(then) = db.hir(statement.then) else {
            return;
        };

        // A `raise` with a value can't be replaced, since an `assert` doesn't have one
        if then.body.is_some() || !matches!(then.statements.as_slice(), [Statement::Raise(None, _)])
        {
            return;
        }

        let srcloc = statement.srcloc.clone();
        let replacement = negated_text(db, statement.condition);

        let Some(diagnostic) = cx.report(
            Lint::ManualAssert,
            srcloc.clone(),
            "This `if` statement only raises, so it can be written as an `assert`",
        ) else {
            return;
        };

        // A statement's location starts at its attributes, which an `assert` can keep, but the
        // suggestion would have to move them
        let Some(replacement) = replacement.filter(|_| !srcloc.text().starts_with('#')) else {
            return;
        };

        let applicability = if has_comment(srcloc.text()) {
            Applicability::MaybeIncorrect
        } else {
            Applicability::MachineApplicable
        };

        diagnostic.suggestions.push(Suggestion::replace(
            "Replace it with an `assert`",
            srcloc,
            format!("assert {replacement};"),
            applicability,
        ));
    }
}
