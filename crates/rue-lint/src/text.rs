//! Helpers for writing the code in suggestions, from the locations of the HIR nodes that are
//! being replaced.

use rue_diagnostic::{SrcLoc, Suggestion};
use rue_hir::{BinaryOp, Database, Hir, HirId, UnaryOp};

/// The code that an expression was compiled from, without any parentheses around it, so that it
/// can be used where any expression is allowed (such as an `assert` or a `let` value).
pub(crate) fn expr_text(db: &Database, hir: HirId) -> Option<String> {
    let text = db.hir_srcloc(hir)?.text();
    Some(strip_parens(text).to_string())
}

/// The code that an expression was compiled from, in parentheses if it could otherwise be split
/// up by the operators around it, so that it can be used as an operand.
pub(crate) fn operand_text(db: &Database, hir: HirId) -> Option<String> {
    let text = db.hir_srcloc(hir)?.text();

    if is_parenthesized(text) || is_atomic(db, hir) {
        Some(text.to_string())
    } else {
        Some(format!("({text})"))
    }
}

/// The code for the negation of a boolean expression, which removes a `!` or flips `==` and `!=`
/// rather than adding another `!` if possible.
pub(crate) fn negated_text(db: &Database, hir: HirId) -> Option<String> {
    let srcloc = db.hir_srcloc(hir)?;

    if !is_parenthesized(srcloc.text()) {
        match *db.hir(hir) {
            Hir::Unary(UnaryOp::Not, inner) if srcloc.text().starts_with('!') => {
                return expr_text(db, inner);
            }
            Hir::Binary(op @ (BinaryOp::Eq | BinaryOp::Ne), left, right) => {
                let op = if op == BinaryOp::Eq { "!=" } else { "==" };
                let left = operand_text(db, left)?;
                let right = operand_text(db, right)?;
                return Some(format!("{left} {op} {right}"));
            }
            _ => {}
        }
    }

    Some(format!("!{}", operand_text(db, hir)?))
}

/// Whether nothing can bind more tightly to an expression than it already does, so that it never
/// needs parentheses as an operand.
fn is_atomic(db: &Database, hir: HirId) -> bool {
    matches!(
        db.hir(hir),
        Hir::Nil
            | Hir::String(_)
            | Hir::Int(_)
            | Hir::Bytes(_)
            | Hir::Bool(_)
            | Hir::Pair(..)
            | Hir::List(_)
            | Hir::Struct(_)
            | Hir::FieldAccess(_)
            | Hir::Reference(_)
            | Hir::Block(_)
            | Hir::Const(_)
            | Hir::FunctionCall(_)
            | Hir::Unary(..)
            | Hir::CoinId(..)
            | Hir::Substr(..)
            | Hir::G1Map(..)
            | Hir::G2Map(..)
            | Hir::Modpow(..)
            | Hir::BlsPairingIdentity(_)
            | Hir::BlsVerify(..)
            | Hir::Secp256K1Verify(..)
            | Hir::Secp256R1Verify(..)
            | Hir::ClvmOp(..)
    )
}

/// Whether the whole text is wrapped in one pair of parentheses, such as `(a + b)` but not
/// `(a) + (b)`.
pub(crate) fn is_parenthesized(text: &str) -> bool {
    if !text.starts_with('(') || !text.ends_with(')') {
        return false;
    }

    let mut depth = 0;
    let mut in_string = false;
    let mut escaped = false;

    for (index, char) in text.char_indices() {
        if in_string {
            match char {
                _ if escaped => escaped = false,
                '\\' => escaped = true,
                '"' => in_string = false,
                _ => {}
            }
            continue;
        }

        match char {
            '"' => in_string = true,
            '(' => depth += 1,
            ')' => {
                depth -= 1;

                if depth == 0 {
                    return index == text.len() - 1;
                }
            }
            _ => {}
        }
    }

    false
}

fn strip_parens(mut text: &str) -> &str {
    while is_parenthesized(text) {
        text = text[1..text.len() - 1].trim();
    }
    text
}

/// Whether there's a comment in the code, which a suggestion that replaces it would remove.
pub(crate) fn has_comment(text: &str) -> bool {
    text.contains("//") || text.contains("/*")
}

/// A suggestion that removes a statement or item, along with the rest of its line if nothing else
/// is on it, so that an empty line isn't left behind.
pub(crate) fn remove_line(
    message: &str,
    srcloc: &SrcLoc,
    applicability: rue_diagnostic::Applicability,
) -> Suggestion {
    let text = &srcloc.source.text;
    let mut start = srcloc.span.start;
    let mut end = srcloc.span.end;

    let line_start = text[..start].rfind('\n').map_or(0, |index| index + 1);
    let line_end = text[end..]
        .find('\n')
        .map_or(text.len(), |index| end + index + 1);

    if text[line_start..start].trim().is_empty() && text[end..line_end].trim().is_empty() {
        start = line_start;
        end = line_end;
    }

    Suggestion::replace(
        message,
        SrcLoc::new(srcloc.source.clone(), start..end),
        "",
        applicability,
    )
}

/// The location of the keyword directly before the name of a declaration (along with the space
/// after it), such as the `inline` in `inline const NAME`, where `after` is the keyword between
/// them (such as `const`).
pub(crate) fn keyword_before(name: &SrcLoc, keyword: &str, after: &str) -> Option<SrcLoc> {
    let text = &name.source.text[..name.span.start];
    let before_name = text.trim_end().strip_suffix(after)?.trim_end();
    let start = before_name.strip_suffix(keyword)?.len();

    if before_name[..start]
        .chars()
        .next_back()
        .is_some_and(|char| char.is_alphanumeric() || char == '_')
    {
        return None;
    }

    let end = start + keyword.len();
    let end = end + (text.len() - end - text[end..].trim_start().len());

    Some(SrcLoc::new(name.source.clone(), start..end))
}
