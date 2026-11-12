//! Helpers for writing the code in suggestions, from the locations of the HIR nodes that are
//! being replaced.

use rue_diagnostic::{SrcLoc, Suggestion};
use rue_hir::{BinaryOp, Database, Hir, HirId, Statement, UnaryOp};
use rue_lexer::{Lexer, TokenKind};

use crate::LintContext;

/// Where an expression is, which decides whether the code that replaces it needs parentheses.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Position {
    /// A whole expression by itself, such as an argument or the value of a binding.
    Whole,
    /// The condition of an `if`, which is a whole expression, except that a struct initializer in
    /// it must be in parentheses.
    Condition,
    /// An operand of a prefix or binary operator.
    Operand,
    /// The operand of a postfix operator (such as `as`, `is`, a field access or a call), which
    /// binds more tightly than anything else, including a prefix operator such as `!`.
    Postfix,
}

/// Where the current node is, from the node that directly encloses it.
pub(crate) fn position(cx: &LintContext<'_>, hir: HirId) -> Position {
    let Some(parent) = cx.parent() else {
        return Position::Whole;
    };

    let db = cx.db();

    match db.hir(parent) {
        Hir::Cast(_) | Hir::Guard(_) | Hir::FieldAccess(_) => Position::Postfix,
        Hir::FunctionCall(call) if call.function == hir => Position::Postfix,
        Hir::If(condition, ..) if *condition == hir => Position::Condition,
        Hir::If(..) => Position::Operand,
        Hir::Block(block)
            if block.statements.iter().any(
                |statement| matches!(statement, Statement::If(statement) if statement.condition == hir),
            ) =>
        {
            Position::Condition
        }
        Hir::Block(_)
        | Hir::List(_)
        | Hir::Struct(_)
        | Hir::Pair(..)
        | Hir::Lambda(_)
        | Hir::FunctionCall(_)
        | Hir::CoinId(..)
        | Hir::Substr(..)
        | Hir::G1Map(..)
        | Hir::G2Map(..)
        | Hir::Modpow(..)
        | Hir::BlsPairingIdentity(_)
        | Hir::BlsVerify(..)
        | Hir::Secp256K1Verify(..)
        | Hir::Secp256R1Verify(..)
        | Hir::ClvmOp(..) => Position::Whole,
        _ => Position::Operand,
    }
}

/// The code that an expression was compiled from, with parentheses if it needs them at the
/// position, and without any that it doesn't need as a whole expression.
pub(crate) fn text_at(db: &Database, hir: HirId, position: Position) -> Option<String> {
    let text = db.hir_srcloc(hir)?.text();

    let needs_parens = match position {
        Position::Whole => return Some(strip_parens(text).to_string()),
        Position::Condition => return Some(condition(strip_parens(text).to_string())),
        Position::Operand => !is_atomic(db, hir),
        Position::Postfix => !is_atomic(db, hir) || matches!(db.hir(hir), Hir::Unary(..)),
    };

    if needs_parens && !is_parenthesized(text) {
        Some(format!("({text})"))
    } else {
        Some(text.to_string())
    }
}

/// The code for the negation of a boolean expression as a whole expression. See
/// [`negated_text_at`].
pub(crate) fn negated_text(db: &Database, hir: HirId) -> Option<String> {
    negated_text_at(db, hir, Position::Whole)
}

/// The code for the negation of a boolean expression, with parentheses if it needs them at the
/// position. A `!` is removed or `==` and `!=` are flipped, rather than adding another `!` if
/// possible.
pub(crate) fn negated_text_at(db: &Database, hir: HirId, position: Position) -> Option<String> {
    if position == Position::Condition {
        return negated_text_at(db, hir, Position::Whole).map(condition);
    }

    let text = strip_parens(db.hir_srcloc(hir)?.text());

    match *db.hir(hir) {
        Hir::Unary(UnaryOp::Not, inner) if text.starts_with('!') => {
            return text_at(db, inner, position);
        }
        Hir::Binary(op @ (BinaryOp::Eq | BinaryOp::Ne), left, right) => {
            let op = if op == BinaryOp::Eq { "!=" } else { "==" };
            let left = text_at(db, left, Position::Operand)?;
            let right = text_at(db, right, Position::Operand)?;
            let text = format!("{left} {op} {right}");

            return Some(if position == Position::Whole {
                text
            } else {
                format!("({text})")
            });
        }
        _ => {}
    }

    let text = format!("!{}", text_at(db, hir, Position::Postfix)?);

    Some(if position == Position::Postfix {
        format!("({text})")
    } else {
        text
    })
}

/// Code that's written as the condition of an `if`, in parentheses if it could contain a struct
/// initializer, which can't be written there without them.
fn condition(text: String) -> String {
    if has_code_brace(&text) && !is_parenthesized(&text) {
        format!("({text})")
    } else {
        text
    }
}

/// Whether one location is inside of another (or the same as it).
pub(crate) fn encloses(outer: &SrcLoc, inner: &SrcLoc) -> bool {
    outer.source.kind == inner.source.kind
        && outer.span.start <= inner.span.start
        && inner.span.end <= outer.span.end
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

    // The text is tokenized like the compiler would, so parentheses in strings and comments
    // aren't counted
    for token in Lexer::new(text) {
        match token.kind {
            TokenKind::OpenParen => depth += 1,
            TokenKind::CloseParen => {
                depth -= 1;

                if depth == 0 {
                    return token.span.end == text.len();
                }
            }
            _ => {}
        }
    }

    false
}

/// Whether the code has a `{` outside of strings and comments, which could be a struct
/// initializer.
fn has_code_brace(text: &str) -> bool {
    Lexer::new(text).any(|token| token.kind == TokenKind::OpenBrace)
}

/// The text without any parentheses around the whole of it.
pub(crate) fn strip_parens(mut text: &str) -> &str {
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
