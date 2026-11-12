//! Lints that only depend on the syntax tree.

use std::{ops::Range, sync::Arc};

use rowan::{NodeOrToken, TextRange};
use rue_ast::{
    AstBinding, AstExpr, AstGenericArguments, AstGenericParameters, AstGroupExpr, AstIfExpr,
    AstNode, AstPrefixExpr, AstStmtOrExpr, AstStructFieldBinding, AstStructInitializerField,
};
use rue_diagnostic::{Applicability, Lint, Source, Suggestion};
use rue_lexer::Lexer;
use rue_parser::{Parser, SyntaxKind, SyntaxNode, T};

use crate::{LintContext, LintInput, srcloc, text::separate_words};

pub(crate) const LINTS: &[Lint] = &[
    Lint::EmptyGenerics,
    Lint::DoubleNegation,
    Lint::RedundantFieldNames,
    Lint::UnnecessaryParens,
    Lint::CollapsibleIf,
];

pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    for &(source, document) in &input.documents {
        for node in document.syntax().descendants() {
            if let Some(parameters) = AstGenericParameters::cast(node.clone()) {
                if parameters.names().next().is_none() {
                    cx.report(
                        Lint::EmptyGenerics,
                        srcloc(source, node.text_range()),
                        "Unnecessary empty generic parameter list specified",
                    );
                }
            } else if let Some(arguments) = AstGenericArguments::cast(node.clone()) {
                if arguments.types().next().is_none() {
                    cx.report(
                        Lint::EmptyGenerics,
                        srcloc(source, node.text_range()),
                        "Unnecessary empty generic argument list specified",
                    );
                }
            } else if let Some(prefix) = AstPrefixExpr::cast(node.clone())
                && let Some(op) = prefix.op()
                && op.kind() == T![-]
                && let Some(AstExpr::PrefixExpr(inner)) = prefix.expr()
                && let Some(inner_op) = inner.op()
                && inner_op.kind() == T![-]
            {
                cx.report(
                    Lint::DoubleNegation,
                    srcloc(source, node.text_range()),
                    "Use of `--` in an expression will perform a double negation, but could be confused with the 'prefix decrement' operator in other languages",
                );
            } else if let Some(field) = AstStructInitializerField::cast(node.clone()) {
                check_initializer_field(cx, source, &field);
            } else if let Some(field) = AstStructFieldBinding::cast(node.clone()) {
                check_field_binding(cx, source, &field);
            } else if let Some(group) = AstGroupExpr::cast(node.clone()) {
                check_parens(cx, source, &group);
            } else if let Some(if_expr) = AstIfExpr::cast(node.clone()) {
                check_collapsible_if(cx, source, &if_expr);
            }
        }
    }
}

/// A struct initializer field like `name: name` can be written as `name`.
fn check_initializer_field(
    cx: &mut LintContext<'_>,
    source: &Source,
    field: &AstStructInitializerField,
) {
    let (Some(name), Some(AstExpr::PathExpr(path))) = (field.name(), field.expr()) else {
        return;
    };

    let mut segments = path.segments();

    let (Some(segment), None) = (segments.next(), segments.next()) else {
        return;
    };

    if segment.separator().is_some()
        || segment.generic_arguments().is_some()
        || segment
            .name()
            .is_none_or(|value| value.text() != name.text())
    {
        return;
    }

    report_redundant_field_name(cx, source, field.syntax(), name.text());
}

/// A struct binding field like `name: name` can be written as `name`.
fn check_field_binding(cx: &mut LintContext<'_>, source: &Source, field: &AstStructFieldBinding) {
    let (Some(name), Some(AstBinding::NamedBinding(binding))) = (field.name(), field.binding())
    else {
        return;
    };

    if field.spread().is_some()
        || binding
            .name()
            .is_none_or(|value| value.text() != name.text())
    {
        return;
    }

    report_redundant_field_name(cx, source, field.syntax(), name.text());
}

fn report_redundant_field_name(
    cx: &mut LintContext<'_>,
    source: &Source,
    field: &SyntaxNode,
    name: &str,
) {
    let srcloc = srcloc(source, trimmed_range(field));

    if let Some(diagnostic) = cx.report(
        Lint::RedundantFieldNames,
        srcloc.clone(),
        format!("Redundant field name, since `{name}: {name}` can be written as `{name}`"),
    ) {
        diagnostic.suggestions.push(Suggestion::replace(
            "Use the shorthand instead",
            srcloc,
            name,
            Applicability::MachineApplicable,
        ));
    }
}

/// Parentheses are unnecessary around an expression that nothing could split up, or that's
/// already a whole expression by itself (such as a condition, or the value of a binding).
fn check_parens(cx: &mut LintContext<'_>, source: &Source, group: &AstGroupExpr) {
    let Some(inner) = group.expr() else {
        return;
    };

    // Code that failed to parse might need the parentheses once it's fixed
    if group
        .syntax()
        .descendants_with_tokens()
        .any(|element| element.kind() == SyntaxKind::Error)
    {
        return;
    }

    // The inner parentheses of `((a))` are reported as being in a whole expression
    if group
        .syntax()
        .parent()
        .and_then(AstGroupExpr::cast)
        .is_some()
    {
        return;
    }

    if !is_atomic(&inner) && !is_whole_expression(group.syntax()) {
        return;
    }

    let group_range = trimmed_range(group.syntax());
    let inner_range = trimmed_range(inner.syntax());
    let text = &source.text;

    let start: usize = group_range.start().into();
    let end: usize = group_range.end().into();

    // The code is parsed again with the replacement below, so it's separated from the words
    // around it here rather than when the suggestion is reported
    let mut replacement =
        text[usize::from(inner_range.start())..usize::from(inner_range.end())].to_string();
    separate_words(text, start..end, &mut replacement);

    let srcloc = srcloc(source, group_range);

    // Parsing the code again is expensive, so it's skipped where nothing would be reported
    if !cx.is_enabled_at(Lint::UnnecessaryParens, &srcloc) {
        return;
    }

    // The parser has rules that depend on where an expression is (for example, a struct
    // initializer can't be written in an `if` condition without parentheses), so the code is
    // parsed again without them, to make sure that only the parentheses were removed
    if !parses_without_parens(source, group.syntax(), start..end, &replacement) {
        return;
    }

    if let Some(diagnostic) = cx.report(
        Lint::UnnecessaryParens,
        srcloc.clone(),
        "Unnecessary parentheses",
    ) {
        diagnostic.suggestions.push(Suggestion::replace(
            "Remove the parentheses",
            srcloc,
            replacement,
            Applicability::MachineApplicable,
        ));
    }
}

/// Whether nothing around an expression can bind more tightly to part of it than to the whole of
/// it, so it never needs parentheses.
fn is_atomic(expr: &AstExpr) -> bool {
    matches!(
        expr,
        AstExpr::PathExpr(_)
            | AstExpr::LiteralExpr(_)
            | AstExpr::FunctionCallExpr(_)
            | AstExpr::FieldAccessExpr(_)
            | AstExpr::ListExpr(_)
            | AstExpr::PairExpr(_)
            | AstExpr::GroupExpr(_)
            | AstExpr::ConstExpr(_)
            | AstExpr::StructInitializerExpr(_)
    )
}

/// Whether the node is the condition of the `if` expression or statement.
fn is_condition_of(if_node: &SyntaxNode, node: &SyntaxNode) -> bool {
    if_node
        .children()
        .find_map(AstExpr::cast)
        .is_some_and(|condition| condition.syntax() == node)
}

/// Whether the code parses to the same syntax tree once the parentheses are replaced, apart from
/// the parenthesized expression itself.
///
/// Only the statement or item that the parentheses are in is parsed again, since the rest of the
/// document can't change how it's parsed. A statement is parsed in a block of its own, so that
/// it's parsed by the same rules.
fn parses_without_parens(
    source: &Source,
    group: &SyntaxNode,
    range: Range<usize>,
    replacement: &str,
) -> bool {
    let Some(unit) = group.ancestors().find(|node| {
        node.parent().is_some_and(|parent| {
            matches!(
                parent.kind(),
                SyntaxKind::Block | SyntaxKind::Document | SyntaxKind::ModuleItem
            )
        })
    }) else {
        return false;
    };

    let unit_range: Range<usize> = unit.text_range().into();
    let mut text = source.text[unit_range.clone()].to_string();
    text.replace_range(
        range.start - unit_range.start..range.end - unit_range.start,
        replacement,
    );

    let in_block = unit
        .parent()
        .is_some_and(|parent| parent.kind() == SyntaxKind::Block);

    if in_block {
        text = format!("fn lint() {{\n{text}\n}}");
    }

    let tokens = Lexer::new(&text).collect::<Vec<_>>();
    let reparsed = Parser::new(
        Source::new(Arc::from(text.as_str()), source.kind.clone()),
        tokens,
    )
    .parse()
    .node;

    let container = if in_block {
        reparsed
            .descendants()
            .find(|node| node.kind() == SyntaxKind::Block)
    } else {
        Some(reparsed)
    };

    let Some(container) = container else {
        return false;
    };

    let mut children = container.children();

    let (Some(reparsed), None) = (children.next(), children.next()) else {
        return false;
    };

    shape(&unit, Some(group)) == shape(&reparsed, None)
}

/// The kinds of the nodes and tokens of a syntax tree, without whitespace and comments, and
/// without the given parenthesized expression (but with the expression inside of it).
fn shape(root: &SyntaxNode, without: Option<&SyntaxNode>) -> Vec<SyntaxKind> {
    root.descendants_with_tokens()
        .filter_map(|element| match element {
            NodeOrToken::Node(node) => (Some(&node) != without).then(|| node.kind()),
            NodeOrToken::Token(token) => {
                let is_paren = matches!(token.kind(), T!['('] | T![')'])
                    && without.is_some_and(|group| token.parent().as_ref() == Some(group));

                let is_comment = matches!(
                    token.kind(),
                    SyntaxKind::Whitespace | SyntaxKind::LineComment | SyntaxKind::BlockComment
                );

                (!is_paren && !is_comment).then(|| token.kind())
            }
        })
        .collect()
}

/// Whether the expression is already a whole expression by itself, rather than an operand of
/// something else.
fn is_whole_expression(node: &SyntaxNode) -> bool {
    let Some(parent) = node.parent() else {
        return false;
    };

    match parent.kind() {
        SyntaxKind::LetStmt
        | SyntaxKind::ExprStmt
        | SyntaxKind::ReturnStmt
        | SyntaxKind::AssertStmt
        | SyntaxKind::RaiseStmt
        | SyntaxKind::DebugStmt
        | SyntaxKind::ConstantItem
        | SyntaxKind::StructField
        | SyntaxKind::StructInitializerField
        | SyntaxKind::ListItem
        | SyntaxKind::Block
        | SyntaxKind::GroupExpr
        | SyntaxKind::LambdaExpr => true,
        // Only the condition of an `if` is a whole expression, since a branch must be a block
        SyntaxKind::IfStmt | SyntaxKind::IfExpr => is_condition_of(&parent, node),
        _ => false,
    }
}

/// An `else` block that only contains an `if` can be written as `else if`.
fn check_collapsible_if(cx: &mut LintContext<'_>, source: &Source, if_expr: &AstIfExpr) {
    let Some(AstExpr::Block(block)) = if_expr.else_expr() else {
        return;
    };

    let mut items = block.items();

    let (Some(AstStmtOrExpr::Expr(AstExpr::IfExpr(inner))), None) = (items.next(), items.next())
    else {
        return;
    };

    // An `inline if` can't follow `else`
    if inner.inline().is_some() {
        return;
    }

    let block_range = trimmed_range(block.syntax());
    let inner_range = trimmed_range(inner.syntax());

    let text = &source.text;

    let srcloc = srcloc(source, block_range);
    let replacement =
        text[usize::from(inner_range.start())..usize::from(inner_range.end())].to_string();

    if let Some(diagnostic) = cx.report(
        Lint::CollapsibleIf,
        srcloc.clone(),
        "This `else` block only contains an `if`, so it can be written as `else if`",
    ) {
        diagnostic.suggestions.push(Suggestion::replace(
            "Collapse it into an `else if`",
            srcloc,
            replacement,
            Applicability::MachineApplicable,
        ));
    }
}

/// The range of a node without any whitespace or comments at its start or end, which the parser
/// can include in it.
fn trimmed_range(node: &SyntaxNode) -> TextRange {
    let mut tokens = node
        .descendants_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .filter(|token| !token.kind().is_trivia());

    let Some(first) = tokens.next() else {
        return node.text_range();
    };

    let last = tokens.last().unwrap_or_else(|| first.clone());

    TextRange::new(first.text_range().start(), last.text_range().end())
}
