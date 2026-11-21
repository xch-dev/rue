use rowan::{NodeOrToken, TextRange, TextSize};
use rue_ast::AstNode;
use rue_diagnostic::{Applicability, Lint, SrcLoc, Suggestion};
use rue_parser::SyntaxKind;

use super::syntax::is_operand;
use crate::{
    LintContext, LintInput,
    text::{comment_count, separate_words},
};

pub(crate) const LINTS: &[Lint] = &[Lint::BracketedFor];

/// Reports the lists whose only item is a `for` that makes a list. Whether it does is only known
/// while the list is compiled, so these are recorded as sites then.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    for site in &input.sites.bracketed_fors {
        let Some(diagnostic) = cx.report(
            Lint::BracketedFor,
            site.list.clone(),
            "A `for` expression is a list already, so the brackets around it make a list with one list in it",
        ) else {
            continue;
        };

        let source = &site.list.source;
        let text = &source.text;
        let list = site.list.span.clone();
        let for_expr = site.for_expr.span.clone();

        // Neither suggestion is applied automatically, since each changes the type of the value,
        // which may be what the code around it needs to change for. A comment in the brackets
        // would be removed with them
        if comment_count(&text[list.clone()]) == comment_count(&text[for_expr.clone()]) {
            // Without the brackets, the `for` is where the list was
            let mut replacement = if is_operand_list(input, &site.list) {
                format!("({})", &text[for_expr])
            } else {
                text[for_expr].to_string()
            };

            // The `for` would otherwise join the word before the bracket, as in `return[…]`
            separate_words(text, list, &mut replacement);

            diagnostic.suggestions.push(Suggestion::replace(
                "Remove the brackets",
                site.list.clone(),
                replacement,
                Applicability::MaybeIncorrect,
            ));
        }

        let start = site.for_expr.span.start;

        diagnostic.suggestions.push(Suggestion::replace(
            "Spread the `for` into the list with `...`",
            SrcLoc::new(source.clone(), start..start),
            "...",
            Applicability::MaybeIncorrect,
        ));
    }
}

/// Whether the list at the location is an operand, which a `for` in its place needs parentheses
/// as (see [`is_operand`]).
fn is_operand_list(input: &LintInput<'_>, list: &SrcLoc) -> bool {
    let (Ok(start), Ok(end)) = (
        TextSize::try_from(list.span.start),
        TextSize::try_from(list.span.end),
    ) else {
        return false;
    };

    input
        .documents
        .iter()
        .find(|(source, _)| source.kind == list.source.kind)
        .and_then(|(_, document)| {
            let element = document
                .syntax()
                .covering_element(TextRange::new(start, end));

            match element {
                NodeOrToken::Node(node) => Some(node),
                NodeOrToken::Token(token) => token.parent(),
            }
        })
        .is_some_and(|node| node.kind() == SyntaxKind::ListExpr && is_operand(&node))
}
