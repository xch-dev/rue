mod binding;
mod block;
mod document;
mod expr;
mod generics;
mod item;
#[cfg(test)]
mod recovery;
mod stmt;
mod ty;

pub(crate) use document::document;

use crate::{Parser, SyntaxKind, T};

/// Tokens that start a statement and can't appear anywhere else.
const STATEMENT_KEYWORDS: &[SyntaxKind] = &[T![let], T![return], T![assert], T![raise], T![debug]];

/// Tokens that start an item and can't appear anywhere else.
const ITEM_KEYWORDS: &[SyntaxKind] = &[
    T![import],
    T![export],
    T![extern],
    T![test],
    T![mod],
    T![type],
    T![struct],
];

/// Whether the parser is at the start of an item, as opposed to something that could be an
/// expression (such as a lambda or `const` block).
pub(crate) fn at_item_start(p: &mut Parser) -> bool {
    if p.at_any_quiet(ITEM_KEYWORDS).is_some() {
        return true;
    }

    let mut n = 0;

    if p.at_quiet(T![inline]) {
        n += 1;
    }

    matches!(p.lookahead(n), T![fn] | T![const]) && p.lookahead(n + 1) == SyntaxKind::Ident
}

/// Whether the parser is at a token that error recovery should stop at rather than skipping past.
/// These are tokens that close a construct, separate list elements or statements, or start a new
/// statement or item. Stopping here lets the enclosing construct resume parsing normally, rather
/// than reporting a cascade of errors for the rest of the file.
pub(crate) fn at_recovery_point(p: &mut Parser) -> bool {
    p.at_any_quiet(&[T![')'], T![']'], T!['}'], T![;], T![,], SyntaxKind::Eof])
        .is_some()
        || p.at_any_quiet(STATEMENT_KEYWORDS).is_some()
        || at_item_start(p)
}

/// Reports that `name` was expected, then skips tokens as errors until a recovery point or one of
/// the additional `stops` is reached.
pub(crate) fn recover(p: &mut Parser, name: &str, stops: &[SyntaxKind]) {
    p.expected(name);
    p.error_unexpected();
    skip_until(p, stops);
}

/// Reports an unexpected token error, then skips tokens until the next recovery point.
pub(crate) fn skip_unexpected(p: &mut Parser) {
    p.error_unexpected();
    skip_until(p, &[]);
}

/// Skips tokens as errors until a recovery point or one of the additional `stops` is reached.
pub(crate) fn skip_until(p: &mut Parser, stops: &[SyntaxKind]) {
    skip_balanced(p, |p| {
        at_recovery_point(p) || p.at_any_quiet(stops).is_some()
    });
}

/// Skips tokens as errors until `stop` returns true. Delimited groups such as `(...)` are skipped
/// as a whole, so that tokens nested inside of them (such as commas) don't stop the skipping.
/// The start of an item always stops, since items can't be nested inside of other constructs.
pub(crate) fn skip_balanced(p: &mut Parser, mut stop: impl FnMut(&mut Parser) -> bool) {
    let mut depth = 0usize;

    while !p.at_quiet(SyntaxKind::Eof) && !at_item_start(p) {
        if depth == 0 && stop(p) {
            break;
        }

        if p.at_any_quiet(&[T!['('], T!['['], T!['{']]).is_some() {
            depth += 1;
        } else if p.at_any_quiet(&[T![')'], T![']'], T!['}']]).is_some() {
            depth = depth.saturating_sub(1);
        }

        p.bump_as_error();
    }
}

/// Parses a comma separated list of elements, up to and including the closing delimiter. The
/// opening delimiter must already have been consumed. If an element is followed by something
/// other than a comma or the closing delimiter, the unexpected tokens are skipped until the next
/// comma so that the rest of the list can still be parsed. Only the first such error in the list
/// is reported, since it's often the cause of the others.
pub(crate) fn delimited_list(p: &mut Parser, close: SyntaxKind, element: impl FnMut(&mut Parser)) {
    delimited_list_with_stops(p, close, &[], element);
}

/// Like [`delimited_list`], but error recovery also stops at any of the given tokens, rather than
/// skipping them. This is useful for tokens that are known to come after the list, in case the
/// closing delimiter is missing.
pub(crate) fn delimited_list_with_stops(
    p: &mut Parser,
    close: SyntaxKind,
    stops: &[SyntaxKind],
    mut element: impl FnMut(&mut Parser),
) {
    let mut reported = false;

    while !p.at(close) && !p.at_quiet(SyntaxKind::Eof) {
        let start = p.pos();

        element(p);

        if p.try_eat(T![,]) {
            continue;
        }

        if p.at(close) {
            break;
        }

        if reported {
            p.clear_expected();
        } else {
            p.error_unexpected();
            reported = true;
        }

        if p.pos() == start {
            if at_recovery_point(p) || p.at_any_quiet(stops).is_some() {
                break;
            }

            // The element didn't consume anything, so skip at least one token to make progress
            p.bump_as_error();
        }

        skip_until(p, &[&[close], stops].concat());

        if !p.try_eat(T![,]) {
            break;
        }
    }

    p.expect(close);
}

#[cfg(test)]
mod tests {
    use std::{path::Path, sync::Arc};

    use expect_test::Expect;
    use rue_diagnostic::{Source, SourceKind};
    use rue_lexer::Lexer;

    use crate::Parser;

    #[allow(clippy::needless_pass_by_value)]
    pub fn check(f: impl FnOnce(&mut Parser), source: &str, expect: Expect, errors: Expect) {
        let source = Source::new(Arc::from(source), SourceKind::File("main.rue".to_string()));

        let tokens = Lexer::new(&source.text).collect::<Vec<_>>();

        let mut parser = Parser::new(source, tokens);
        f(&mut parser);

        let result = parser.parse_raw();

        let output = format!("{:#?}", result.node);

        let mut error_output = String::new();

        for (i, error) in result.diagnostics.into_iter().enumerate() {
            if i != 0 {
                error_output.push('\n');
            }
            error_output.push_str(&error.message(Path::new(".")));
        }

        expect.assert_eq(&output);
        errors.assert_eq(&error_output);
    }
}
