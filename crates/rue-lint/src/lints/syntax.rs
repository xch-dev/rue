//! Lints that only depend on the syntax tree.

use rowan::TextRange;
use rue_ast::{
    AstBinding, AstExpr, AstGenericArguments, AstGenericParameters, AstGroupExpr, AstIfExpr,
    AstNode, AstPrefixExpr, AstStmtOrExpr, AstStructFieldBinding, AstStructInitializerField,
};
use rue_diagnostic::{Applicability, Lint, Source, Suggestion};
use rue_parser::{SyntaxKind, SyntaxNode, T};

use crate::{LintContext, LintInput, srcloc, text::has_comment};

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

    let applicability = if has_comment(srcloc.text()) {
        Applicability::MaybeIncorrect
    } else {
        Applicability::MachineApplicable
    };

    if let Some(diagnostic) = cx.report(
        Lint::RedundantFieldNames,
        srcloc.clone(),
        format!("Redundant field name, since `{name}: {name}` can be written as `{name}`"),
    ) {
        diagnostic.suggestions.push(Suggestion::replace(
            "Use the shorthand instead",
            srcloc,
            name,
            applicability,
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

    let is_condition = group.syntax().parent().is_some_and(|parent| {
        matches!(parent.kind(), SyntaxKind::IfExpr | SyntaxKind::IfStmt)
            && parent
                .children()
                .find_map(AstExpr::cast)
                .is_some_and(|condition| condition.syntax() == group.syntax())
    });

    // A condition can't start with a struct initializer, block or `if` without parentheses,
    // since it would be parsed as the body of the `if` instead
    if is_condition
        && matches!(
            inner,
            AstExpr::StructInitializerExpr(_)
                | AstExpr::Block(_)
                | AstExpr::IfExpr(_)
                | AstExpr::LambdaExpr(_)
        )
    {
        return;
    }

    if !is_atomic(&inner) && !is_whole_expression(group.syntax()) {
        return;
    }

    let group_range = trimmed_range(group.syntax());
    let inner_range = trimmed_range(inner.syntax());
    let text = &source.text;

    // Removing the parentheses can't join the expression to the tokens around it
    let start: usize = group_range.start().into();
    let end: usize = group_range.end().into();
    let inner_text = &text[usize::from(inner_range.start())..usize::from(inner_range.end())];

    let mut replacement = inner_text.to_string();

    if text[..start].chars().next_back().is_some_and(is_word_char)
        && inner_text.chars().next().is_some_and(is_word_char)
    {
        replacement.insert(0, ' ');
    }

    if text[end..].chars().next().is_some_and(is_word_char)
        && inner_text.chars().next_back().is_some_and(is_word_char)
    {
        replacement.push(' ');
    }

    let srcloc = srcloc(source, group_range);

    // Anything between the parentheses and the expression (such as a comment) would be removed
    let outside = format!(
        "{}{}",
        &text[start..inner_range.start().into()],
        &text[inner_range.end().into()..end]
    );

    let applicability = if has_comment(&outside) {
        Applicability::MaybeIncorrect
    } else {
        Applicability::MachineApplicable
    };

    if let Some(diagnostic) = cx.report(
        Lint::UnnecessaryParens,
        srcloc.clone(),
        "Unnecessary parentheses",
    ) {
        diagnostic.suggestions.push(Suggestion::replace(
            "Remove the parentheses",
            srcloc,
            replacement,
            applicability,
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
        SyntaxKind::IfStmt | SyntaxKind::IfExpr => parent
            .children()
            .find_map(AstExpr::cast)
            .is_some_and(|condition| condition.syntax() == node),
        _ => false,
    }
}

fn is_word_char(char: char) -> bool {
    char.is_alphanumeric() || char == '_'
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
    let outside = format!(
        "{}{}",
        &text[usize::from(block_range.start())..usize::from(inner_range.start())],
        &text[usize::from(inner_range.end())..usize::from(block_range.end())]
    );

    let applicability = if has_comment(&outside) {
        Applicability::MaybeIncorrect
    } else {
        Applicability::MachineApplicable
    };

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
            applicability,
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
