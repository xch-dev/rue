use rowan::Checkpoint;

use crate::{
    Parser, SyntaxKind, T,
    grammar::{
        binding::{at_struct_binding, binding},
        block::block,
        delimited_list,
        generics::{generic_arguments, generic_parameters},
        recover, skip_unexpected, skip_until_statement_end,
        ty::{at_type_start, ty},
    },
};

#[derive(Debug, Clone, Copy)]
#[allow(clippy::struct_excessive_bools)]
pub struct ExprOptions {
    pub minimum_binding_power: u8,
    pub allow_statement: bool,
    pub allow_struct_initializer: bool,
    pub inline: bool,
    /// Whether the expression is the start of a slice (`value[start..end]`) if `..` follows it,
    /// which a `.` there would otherwise be a field access without a name.
    pub allow_range: bool,
}

impl Default for ExprOptions {
    fn default() -> Self {
        Self {
            minimum_binding_power: 0,
            allow_statement: false,
            allow_struct_initializer: true,
            inline: false,
            allow_range: false,
        }
    }
}

pub fn expr(p: &mut Parser) {
    let checkpoint = p.checkpoint();
    expr_with(p, checkpoint, ExprOptions::default());
}

/// Returns whether the expression was parsed as a statement: an `if` statement without an `else`,
/// or an `if` with an `else` or a `match` at the start of the statement, which ends at its closing
/// brace like in Rust (see [`ends_statement`]).
pub fn expr_with(p: &mut Parser, checkpoint: Checkpoint, options: ExprOptions) -> bool {
    p.nested(|p| expr_with_inner(p, checkpoint, options))
        .unwrap_or(false)
}

/// At the start of a statement, an `if` with an `else` or a `match` ends at its closing brace, rather
/// than continuing with an operator, so it can be followed by more statements without a `;`. With a
/// `;` after it, it's an expression statement like any other expression.
fn ends_statement(p: &mut Parser) -> bool {
    !p.at_quiet(T![;])
}

fn expr_with_inner(p: &mut Parser, checkpoint: Checkpoint, options: ExprOptions) -> bool {
    if !options.inline
        && let Some(kind) = p.at_any_quiet(SyntaxKind::PREFIX_OPS)
    {
        p.start_at(checkpoint, SyntaxKind::PrefixExpr);
        p.expect(kind);
        let cp = p.checkpoint();
        expr_with(
            p,
            cp,
            ExprOptions {
                minimum_binding_power: 255,
                allow_statement: false,
                allow_struct_initializer: options.allow_struct_initializer,
                inline: false,
                allow_range: options.allow_range,
            },
        );
        p.finish();
    } else if !options.inline && p.at_quiet(T![::])
        || p.at_quiet(SyntaxKind::Ident)
        || p.at_quiet(T![super])
        || p.at_quiet(T![project])
    {
        path_expr(p, checkpoint);

        if p.at(T!['{']) && options.allow_struct_initializer {
            p.start_at(checkpoint, SyntaxKind::StructInitializerExpr);
            p.expect(T!['{']);
            delimited_list(p, T!['}'], |p| {
                // Leave anything that can't start a field to the list's recovery, which only
                // reports the first error, so a missing `}` before more of an enclosing list
                // doesn't report every element after it
                if !p.at(SyntaxKind::Ident) {
                    return;
                }

                p.start(SyntaxKind::StructInitializerField);
                p.expect(SyntaxKind::Ident);
                if p.try_eat(T![:]) {
                    expr(p);
                }
                p.finish();
            });
            p.finish();
        }
    } else if !options.inline
        && let Some(kind) = p.at_any_quiet(SyntaxKind::LITERAL)
    {
        p.start(SyntaxKind::LiteralExpr);
        p.expect(kind);
        p.finish();
    } else if !options.inline && p.at_quiet(T![const]) && p.lookahead(1) != SyntaxKind::Ident {
        p.start_at(checkpoint, SyntaxKind::ConstExpr);
        p.expect(T![const]);
        block(p);
        p.finish();
    } else if !options.inline && p.at_quiet(T!['(']) {
        p.expect(T!['(']);
        expr(p);
        if !p.at(T![,]) && !p.at(T![')']) {
            skip_unexpected(p);
        }
        if p.try_eat(T![,]) {
            p.start_at(checkpoint, SyntaxKind::PairExpr);
            expr(p);
            p.try_eat(T![,]);
            if !p.at(T![')']) {
                skip_unexpected(p);
            }
        } else {
            p.start_at(checkpoint, SyntaxKind::GroupExpr);
        }
        p.expect(T![')']);
        p.finish();
    } else if !options.inline && p.at_quiet(T!['[']) {
        p.start_at(checkpoint, SyntaxKind::ListExpr);
        p.expect(T!['[']);
        delimited_list(p, T![']'], list_item);
        p.finish();
    } else if !options.inline && p.at_quiet(T!['{']) {
        block(p);
    } else if p.at_quiet(T![match])
        || (!options.inline && p.at_quiet(T![inline]) && p.lookahead(1) == T![match])
    {
        match_expr(p, checkpoint, !options.inline);

        if options.allow_statement {
            return ends_statement(p);
        }
    } else if p.at_quiet(T![if]) || (!options.inline && p.at_quiet(T![inline])) {
        if if_expr(p, checkpoint, options.allow_statement, !options.inline) {
            return true;
        }

        if options.allow_statement {
            return ends_statement(p);
        }
    } else if !options.inline && p.at_quiet(T![fn]) && p.lookahead(1) != SyntaxKind::Ident {
        p.start_at(checkpoint, SyntaxKind::LambdaExpr);
        p.expect(T![fn]);
        if p.at(T![<]) {
            generic_parameters(p);
        }
        if p.expect(T!['(']) {
            delimited_list(p, T![')'], |p| {
                p.start(SyntaxKind::FunctionParameter);
                p.try_eat(T![...]);
                binding(p);
                if p.try_eat(T![:]) {
                    ty(p);
                }
                p.finish();
            });
        }
        if p.try_eat(T![:]) {
            ty(p);
        }
        p.expect(T![=>]);
        expr(p);
        p.finish();
    } else {
        // An expression is often followed by a block (such as in the condition of an if
        // expression), so stop there. However, as a statement it's better to skip the block as
        // part of the error (such as in the case of a stray `else` branch).
        if options.allow_statement {
            recover(p, "statement", &[]);
        } else {
            recover(p, "expression", &[T!['{']]);
        }
        return false;
    }

    // Each of these wraps the expression before it, so they nest as deeply as if they recursed
    loop {
        let (kind, token) = if p.at(T!['(']) {
            (SyntaxKind::FunctionCallExpr, T!['('])
        } else if p.at(T![is]) {
            (SyntaxKind::GuardExpr, T![is])
        } else if p.at(T![as]) {
            (SyntaxKind::CastExpr, T![as])
        } else if p.at(T!['[']) {
            (SyntaxKind::IndexExpr, T!['['])
        } else if options.allow_range && p.at_quiet(T![..]) && !p.at_quiet(T![...]) {
            // The `..` of a slice, rather than a field access. Three dots are a field access
            // whose name is still missing followed by the `..`, as in `value[range...end]` while
            // `range.start` is being typed.
            break;
        } else if p.at(T![.]) {
            (SyntaxKind::FieldAccessExpr, T![.])
        } else {
            break;
        };

        if !p.deepen() {
            return false;
        }

        if kind == SyntaxKind::IndexExpr {
            index_or_slice(p, checkpoint);
            continue;
        }

        p.start_at(checkpoint, kind);
        p.expect(token);

        let found = match kind {
            SyntaxKind::FunctionCallExpr => {
                delimited_list(p, T![')'], list_item);
                true
            }
            SyntaxKind::GuardExpr | SyntaxKind::CastExpr => {
                ty(p);
                true
            }
            SyntaxKind::FieldAccessExpr => p.expect(SyntaxKind::Ident),
            _ => unreachable!(),
        };

        p.finish();

        // Without a field name, the rest is unlikely to be a continuation of this expression
        if !found {
            break;
        }
    }

    loop {
        let Some(op) = p.at_any(SyntaxKind::BINARY_OPS) else {
            // The operators that could have continued the expression are too noisy to include in
            // diagnostics about what's expected after it
            p.clear_expected();
            return false;
        };

        let (left_binding_power, right_binding_power) = op
            .binary_binding_power()
            .expect("BINARY_OPS have binding power");

        if left_binding_power < options.minimum_binding_power {
            p.clear_expected();
            return false;
        }

        // Each operator in a chain wraps the expression before it
        if !p.deepen() {
            return false;
        }

        p.expect(op);

        p.start_at(checkpoint, SyntaxKind::BinaryExpr);
        let cp = p.checkpoint();
        expr_with(
            p,
            cp,
            ExprOptions {
                minimum_binding_power: right_binding_power,
                allow_statement: false,
                allow_struct_initializer: options.allow_struct_initializer,
                inline: false,
                allow_range: options.allow_range,
            },
        );
        p.finish();
    }
}

/// Parses the brackets of an index (`value[index]`) or a slice (`value[start..end]`, where either
/// bound can be left out) of the expression that starts at the checkpoint.
fn index_or_slice(p: &mut Parser, checkpoint: Checkpoint) {
    p.expect(T!['[']);

    // A `.` that's followed by another one isn't a field access in either bound, so that it's
    // reported as unexpected rather than as a field without a name
    let bound = |p: &mut Parser| {
        let cp = p.checkpoint();
        expr_with(
            p,
            cp,
            ExprOptions {
                allow_range: true,
                ..ExprOptions::default()
            },
        );
    };

    if !p.at(T![..]) {
        bound(p);
    }

    let kind = if p.try_eat(T![..]) {
        if !p.at(T![']']) {
            bound(p);
        }
        SyntaxKind::SliceExpr
    } else {
        SyntaxKind::IndexExpr
    };

    p.start_at(checkpoint, kind);
    // Unlike in a list, a comma doesn't separate anything here, so it's skipped along with the
    // rest of what's inside of the brackets
    if !p.at(T![']']) {
        p.error_unexpected();
        skip_until_statement_end(p);
    }
    p.expect(T![']']);
    p.finish();
}

fn list_item(p: &mut Parser) {
    p.start(SyntaxKind::ListItem);
    p.try_eat(T![...]);
    expr(p);
    p.finish();
}

fn if_expr(
    p: &mut Parser,
    checkpoint: Checkpoint,
    allow_statement: bool,
    allow_inline: bool,
) -> bool {
    if_expr_with(p, checkpoint, allow_statement, allow_inline, false)
}

/// Parses an `if`, which is an `if` statement if it's at the start of a statement and has no
/// `else`. The `else if` expressions of a chain at the start of a statement can leave out the
/// `else` too, since the chain is a statement then (it's an error later if its value is used).
fn if_expr_with(
    p: &mut Parser,
    checkpoint: Checkpoint,
    allow_statement: bool,
    allow_inline: bool,
    optional_else: bool,
) -> bool {
    // An `inline match` is parsed before getting here, but it's still one of the things that can
    // come after `inline`
    if allow_inline && p.try_eat(T![inline]) {
        p.eat_trivia();
        p.expected(&T![match].to_string());
    }
    p.expect(T![if]);
    let child = p.checkpoint();
    expr_with(
        p,
        child,
        ExprOptions {
            minimum_binding_power: 0,
            allow_statement: false,
            allow_struct_initializer: false,
            inline: false,
            allow_range: false,
        },
    );
    block(p);
    if allow_statement && !p.at(T![else]) {
        p.start_at(checkpoint, SyntaxKind::IfStmt);
        p.finish();
        true
    } else {
        p.start_at(checkpoint, SyntaxKind::IfExpr);
        if optional_else && !p.at(T![else]) {
            p.finish();
            return false;
        }
        p.expect(T![else]);
        if p.at(T![if]) {
            let child = p.checkpoint();
            let optional_else = allow_statement || optional_else;
            p.nested(|p| if_expr_with(p, child, false, false, optional_else));
        } else {
            block(p);
        }
        p.finish();
        false
    }
}

/// A `match` expression, such as `match value { Int => 1, _ => 2 }`, or an `inline match`. Each
/// arm's pattern is a type, which can be followed by a binding that names or destructures the
/// value, such as `Point { x, y } => x + y` or `Point point => point.x`.
/// The arms are separated by commas, except that an arm whose value is a block, `if` or `match`
/// ends at its closing brace, so the comma after it can be left out.
fn match_expr(p: &mut Parser, checkpoint: Checkpoint, allow_inline: bool) {
    p.start_at(checkpoint, SyntaxKind::MatchExpr);
    if allow_inline {
        p.try_eat(T![inline]);
    }
    p.expect(T![match]);
    let child = p.checkpoint();
    expr_with(
        p,
        child,
        ExprOptions {
            minimum_binding_power: 0,
            allow_statement: false,
            allow_struct_initializer: false,
            inline: false,
            allow_range: false,
        },
    );
    if p.expect(T!['{']) {
        delimited_list(p, T!['}'], |p| {
            let mut ends_with_brace = match_arm(p);

            while !p.at(T![,]) && !p.at(T!['}']) && at_type_start(p) {
                if !ends_with_brace {
                    // An arm on the next line most likely only needs the comma, so it's parsed as
                    // the next arm rather than skipped as part of the error
                    if !p.at_line_start() {
                        break;
                    }
                    p.expect(T![,]);
                }
                ends_with_brace = match_arm(p);
            }
        });
    }
    p.finish();
}

/// Parses a match arm, and returns whether its value is a block, `if` or `match`, which ends at its
/// closing brace rather than continuing with an operator, so no comma is needed after it.
fn match_arm(p: &mut Parser) -> bool {
    p.start(SyntaxKind::MatchArm);
    ty(p);

    // The arm's value comes after the `=>`, so a brace here can only start a binding. One that
    // doesn't look like a binding is most likely a block whose `=>` is missing. A name binds the
    // whole value, since nothing in a type continues with one.
    if p.at_quiet(SyntaxKind::Ident)
        || p.at_quiet(T!['('])
        || p.at_quiet(T!['['])
        || at_struct_binding(p)
    {
        binding(p);
    }

    p.expect(T![=>]);

    let checkpoint = p.checkpoint();
    let ends_with_brace = if p.at_quiet(T!['{']) {
        p.nested(block);
        true
    } else if p.at_quiet(T![if]) || p.at_quiet(T![inline]) && p.lookahead(1) == T![if] {
        p.nested(|p| if_expr(p, checkpoint, false, true));
        true
    } else if p.at_quiet(T![match]) || p.at_quiet(T![inline]) && p.lookahead(1) == T![match] {
        p.nested(|p| match_expr(p, checkpoint, true));
        true
    } else {
        expr(p);
        false
    };

    p.finish();
    ends_with_brace
}

fn path_expr(p: &mut Parser, checkpoint: Checkpoint) {
    p.start_at(checkpoint, SyntaxKind::PathExpr);
    path_expr_segment(p, true);
    while p.at(T![::]) {
        path_expr_segment(p, false);
    }
    p.finish();
}

fn path_expr_segment(p: &mut Parser, first: bool) {
    p.start(SyntaxKind::PathSegment);
    if first {
        p.try_eat(T![::]);
    } else {
        p.expect(T![::]);
    }
    if !p.try_eat(T![super]) && !p.try_eat(T![project]) {
        p.expect(SyntaxKind::Ident);
    }
    if p.at(SyntaxKind::TurboFish) {
        p.expect(T![::]);
        generic_arguments(p);
    }
    p.finish();
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::grammar::tests::check;

    use super::*;

    #[test]
    fn test_literal_expr() {
        check(
            expr,
            "hello",
            expect![[r#"
                PathExpr@0..5
                  PathSegment@0..5
                    Ident@0..5 "hello"
            "#]],
            expect![],
        );

        check(
            expr,
            "42",
            expect![[r#"
                LiteralExpr@0..2
                  Integer@0..2 "42"
            "#]],
            expect![],
        );

        check(
            expr,
            "0x42",
            expect![[r#"
                LiteralExpr@0..4
                  Hex@0..4 "0x42"
            "#]],
            expect![],
        );

        check(
            expr,
            "0b1011",
            expect![[r#"
                LiteralExpr@0..6
                  Binary@0..6 "0b1011"
            "#]],
            expect![],
        );

        check(
            expr,
            "0o123",
            expect![[r#"
                LiteralExpr@0..5
                  Octal@0..5 "0o123"
            "#]],
            expect![],
        );

        check(
            expr,
            "\"hello\"",
            expect![[r#"
                LiteralExpr@0..7
                  String@0..7 "\"hello\""
            "#]],
            expect![],
        );

        check(
            expr,
            "true",
            expect![[r#"
                LiteralExpr@0..4
                  True@0..4 "true"
            "#]],
            expect![],
        );

        check(
            expr,
            "false",
            expect![[r#"
                LiteralExpr@0..5
                  False@0..5 "false"
            "#]],
            expect![],
        );

        check(
            expr,
            "nil",
            expect![[r#"
                LiteralExpr@0..3
                  Nil@0..3 "nil"
            "#]],
            expect![],
        );
    }

    #[test]
    fn test_group_expr() {
        check(
            expr,
            "(32 + 6) * 8",
            expect![[r#"
                BinaryExpr@0..12
                  GroupExpr@0..8
                    OpenParen@0..1 "("
                    BinaryExpr@1..7
                      LiteralExpr@1..3
                        Integer@1..3 "32"
                      Whitespace@3..4 " "
                      Plus@4..5 "+"
                      Whitespace@5..6 " "
                      LiteralExpr@6..7
                        Integer@6..7 "6"
                    CloseParen@7..8 ")"
                  Whitespace@8..9 " "
                  Star@9..10 "*"
                  Whitespace@10..11 " "
                  LiteralExpr@11..12
                    Integer@11..12 "8"
            "#]],
            expect![],
        );
    }

    #[test]
    fn test_prefix_expr() {
        check(
            expr,
            "!~+-42",
            expect![[r#"
                PrefixExpr@0..6
                  Not@0..1 "!"
                  PrefixExpr@1..6
                    BitwiseNot@1..2 "~"
                    PrefixExpr@2..6
                      Plus@2..3 "+"
                      PrefixExpr@3..6
                        Minus@3..4 "-"
                        LiteralExpr@4..6
                          Integer@4..6 "42"
            "#]],
            expect![],
        );
    }

    #[test]
    fn test_binary_expr() {
        check(
            expr,
            "1 + 2 * 3 > 4",
            expect![[r#"
                BinaryExpr@0..13
                  BinaryExpr@0..10
                    LiteralExpr@0..1
                      Integer@0..1 "1"
                    Whitespace@1..2 " "
                    Plus@2..3 "+"
                    Whitespace@3..4 " "
                    BinaryExpr@4..10
                      LiteralExpr@4..5
                        Integer@4..5 "2"
                      Whitespace@5..6 " "
                      Star@6..7 "*"
                      Whitespace@7..8 " "
                      LiteralExpr@8..9
                        Integer@8..9 "3"
                      Whitespace@9..10 " "
                  GreaterThan@10..11 ">"
                  Whitespace@11..12 " "
                  LiteralExpr@12..13
                    Integer@12..13 "4"
            "#]],
            expect![],
        );

        check(
            expr,
            "1 && 2 && 3",
            expect![[r#"
                BinaryExpr@0..11
                  BinaryExpr@0..7
                    LiteralExpr@0..1
                      Integer@0..1 "1"
                    Whitespace@1..2 " "
                    And@2..4 "&&"
                    Whitespace@4..5 " "
                    LiteralExpr@5..6
                      Integer@5..6 "2"
                    Whitespace@6..7 " "
                  And@7..9 "&&"
                  Whitespace@9..10 " "
                  LiteralExpr@10..11
                    Integer@10..11 "3"
            "#]],
            expect![],
        );
    }

    #[test]
    fn test_block_expr() {
        check(
            expr,
            "{42}",
            expect![[r#"
                Block@0..4
                  OpenBrace@0..1 "{"
                  LiteralExpr@1..3
                    Integer@1..3 "42"
                  CloseBrace@3..4 "}"
            "#]],
            expect![],
        );
    }

    #[test]
    fn test_const_expr() {
        check(
            expr,
            "const { let value = 1; value + 2 }",
            expect![[r#"
                ConstExpr@0..34
                  Const@0..5 "const"
                  Whitespace@5..6 " "
                  Block@6..34
                    OpenBrace@6..7 "{"
                    Whitespace@7..8 " "
                    LetStmt@8..22
                      Let@8..11 "let"
                      Whitespace@11..12 " "
                      NamedBinding@12..17
                        Ident@12..17 "value"
                      Whitespace@17..18 " "
                      Assign@18..19 "="
                      Whitespace@19..20 " "
                      LiteralExpr@20..21
                        Integer@20..21 "1"
                      Semicolon@21..22 ";"
                    Whitespace@22..23 " "
                    BinaryExpr@23..33
                      PathExpr@23..29
                        PathSegment@23..29
                          Ident@23..28 "value"
                          Whitespace@28..29 " "
                      Plus@29..30 "+"
                      Whitespace@30..31 " "
                      LiteralExpr@31..32
                        Integer@31..32 "2"
                      Whitespace@32..33 " "
                    CloseBrace@33..34 "}"
            "#]],
            expect![""],
        );

        check(
            expr,
            "const { const { 1 + 2 } }",
            expect![[r#"
                ConstExpr@0..25
                  Const@0..5 "const"
                  Whitespace@5..6 " "
                  Block@6..25
                    OpenBrace@6..7 "{"
                    Whitespace@7..8 " "
                    ConstExpr@8..23
                      Const@8..13 "const"
                      Whitespace@13..14 " "
                      Block@14..23
                        OpenBrace@14..15 "{"
                        Whitespace@15..16 " "
                        BinaryExpr@16..22
                          LiteralExpr@16..17
                            Integer@16..17 "1"
                          Whitespace@17..18 " "
                          Plus@18..19 "+"
                          Whitespace@19..20 " "
                          LiteralExpr@20..21
                            Integer@20..21 "2"
                          Whitespace@21..22 " "
                        CloseBrace@22..23 "}"
                    Whitespace@23..24 " "
                    CloseBrace@24..25 "}"
            "#]],
            expect![""],
        );

        check(
            expr,
            "const 42",
            expect![[r#"
            ConstExpr@0..6
              Const@0..5 "const"
              Whitespace@5..6 " "
              Block@6..6
        "#]],
            expect!["Expected `{`, found integer literal at main.rue:1:7"],
        );
    }

    #[test]
    fn test_function_call_expr() {
        check(
            expr,
            "hello(42)",
            expect![[r#"
                FunctionCallExpr@0..9
                  PathExpr@0..5
                    PathSegment@0..5
                      Ident@0..5 "hello"
                  OpenParen@5..6 "("
                  ListItem@6..8
                    LiteralExpr@6..8
                      Integer@6..8 "42"
                  CloseParen@8..9 ")"
            "#]],
            expect![],
        );

        check(
            expr,
            "hello(42,)",
            expect![[r#"
                FunctionCallExpr@0..10
                  PathExpr@0..5
                    PathSegment@0..5
                      Ident@0..5 "hello"
                  OpenParen@5..6 "("
                  ListItem@6..8
                    LiteralExpr@6..8
                      Integer@6..8 "42"
                  Comma@8..9 ","
                  CloseParen@9..10 ")"
            "#]],
            expect![],
        );

        check(
            expr,
            "hello(42, 13)",
            expect![[r#"
                FunctionCallExpr@0..13
                  PathExpr@0..5
                    PathSegment@0..5
                      Ident@0..5 "hello"
                  OpenParen@5..6 "("
                  ListItem@6..8
                    LiteralExpr@6..8
                      Integer@6..8 "42"
                  Comma@8..9 ","
                  Whitespace@9..10 " "
                  ListItem@10..12
                    LiteralExpr@10..12
                      Integer@10..12 "13"
                  CloseParen@12..13 ")"
            "#]],
            expect![],
        );
    }

    #[test]
    fn test_inline_match_expr() {
        check(
            expr,
            "inline match x { nil => 1, _ => inline match y { 0 => 2, _ => 3 } }",
            expect![[r#"
                MatchExpr@0..67
                  Inline@0..6 "inline"
                  Whitespace@6..7 " "
                  Match@7..12 "match"
                  Whitespace@12..13 " "
                  PathExpr@13..15
                    PathSegment@13..15
                      Ident@13..14 "x"
                      Whitespace@14..15 " "
                  OpenBrace@15..16 "{"
                  Whitespace@16..17 " "
                  MatchArm@17..25
                    LiteralType@17..20
                      Nil@17..20 "nil"
                    Whitespace@20..21 " "
                    FatArrow@21..23 "=>"
                    Whitespace@23..24 " "
                    LiteralExpr@24..25
                      Integer@24..25 "1"
                  Comma@25..26 ","
                  Whitespace@26..27 " "
                  MatchArm@27..65
                    PathType@27..29
                      PathSegment@27..29
                        Ident@27..28 "_"
                        Whitespace@28..29 " "
                    FatArrow@29..31 "=>"
                    Whitespace@31..32 " "
                    MatchExpr@32..65
                      Inline@32..38 "inline"
                      Whitespace@38..39 " "
                      Match@39..44 "match"
                      Whitespace@44..45 " "
                      PathExpr@45..47
                        PathSegment@45..47
                          Ident@45..46 "y"
                          Whitespace@46..47 " "
                      OpenBrace@47..48 "{"
                      Whitespace@48..49 " "
                      MatchArm@49..55
                        LiteralType@49..50
                          Integer@49..50 "0"
                        Whitespace@50..51 " "
                        FatArrow@51..53 "=>"
                        Whitespace@53..54 " "
                        LiteralExpr@54..55
                          Integer@54..55 "2"
                      Comma@55..56 ","
                      Whitespace@56..57 " "
                      MatchArm@57..64
                        PathType@57..59
                          PathSegment@57..59
                            Ident@57..58 "_"
                            Whitespace@58..59 " "
                        FatArrow@59..61 "=>"
                        Whitespace@61..62 " "
                        LiteralExpr@62..63
                          Integer@62..63 "3"
                        Whitespace@63..64 " "
                      CloseBrace@64..65 "}"
                  Whitespace@65..66 " "
                  CloseBrace@66..67 "}"
            "#]],
            expect![],
        );

        // Something other than `if` or `match` after `inline`
        check(
            expr,
            "inline x",
            expect![[r#"
            IfExpr@0..8
              Inline@0..6 "inline"
              Whitespace@6..7 " "
              PathExpr@7..8
                PathSegment@7..8
                  Ident@7..8 "x"
              Block@8..8
              Block@8..8
        "#]],
            expect![[r#"
            Expected one of `match`, `if`, found identifier at main.rue:1:8
            Expected `{`, found eof at main.rue:1:9"#]],
        );

        // The `inline` of a statement is part of the match, like it is for an `inline if`
        check(
            crate::grammar::block::block,
            "{ inline match x { _ => {} } }",
            expect![[r#"
                Block@0..30
                  OpenBrace@0..1 "{"
                  Whitespace@1..2 " "
                  MatchExpr@2..28
                    Inline@2..8 "inline"
                    Whitespace@8..9 " "
                    Match@9..14 "match"
                    Whitespace@14..15 " "
                    PathExpr@15..17
                      PathSegment@15..17
                        Ident@15..16 "x"
                        Whitespace@16..17 " "
                    OpenBrace@17..18 "{"
                    Whitespace@18..19 " "
                    MatchArm@19..26
                      PathType@19..21
                        PathSegment@19..21
                          Ident@19..20 "_"
                          Whitespace@20..21 " "
                      FatArrow@21..23 "=>"
                      Whitespace@23..24 " "
                      Block@24..26
                        OpenBrace@24..25 "{"
                        CloseBrace@25..26 "}"
                    Whitespace@26..27 " "
                    CloseBrace@27..28 "}"
                  Whitespace@28..29 " "
                  CloseBrace@29..30 "}"
            "#]],
            expect![],
        );
    }

    #[test]
    fn test_match_expr() {
        check(
            expr,
            "match x { A | nil => 1, _ => {} }",
            expect![[r#"
                MatchExpr@0..33
                  Match@0..5 "match"
                  Whitespace@5..6 " "
                  PathExpr@6..8
                    PathSegment@6..8
                      Ident@6..7 "x"
                      Whitespace@7..8 " "
                  OpenBrace@8..9 "{"
                  Whitespace@9..10 " "
                  MatchArm@10..22
                    UnionType@10..18
                      PathType@10..12
                        PathSegment@10..12
                          Ident@10..11 "A"
                          Whitespace@11..12 " "
                      BitwiseOr@12..13 "|"
                      Whitespace@13..14 " "
                      LiteralType@14..17
                        Nil@14..17 "nil"
                      Whitespace@17..18 " "
                    FatArrow@18..20 "=>"
                    Whitespace@20..21 " "
                    LiteralExpr@21..22
                      Integer@21..22 "1"
                  Comma@22..23 ","
                  Whitespace@23..24 " "
                  MatchArm@24..31
                    PathType@24..26
                      PathSegment@24..26
                        Ident@24..25 "_"
                        Whitespace@25..26 " "
                    FatArrow@26..28 "=>"
                    Whitespace@28..29 " "
                    Block@29..31
                      OpenBrace@29..30 "{"
                      CloseBrace@30..31 "}"
                  Whitespace@31..32 " "
                  CloseBrace@32..33 "}"
            "#]],
            expect![],
        );

        // An arm that ends with a brace doesn't need a comma, and the pair pattern after it isn't
        // parsed as a call of the block
        check(
            expr,
            "match x { nil => {} (Int, Int) => if a { 1 } else { 2 } _ => match y { _ => 3 } }",
            expect![[r#"
                MatchExpr@0..81
                  Match@0..5 "match"
                  Whitespace@5..6 " "
                  PathExpr@6..8
                    PathSegment@6..8
                      Ident@6..7 "x"
                      Whitespace@7..8 " "
                  OpenBrace@8..9 "{"
                  Whitespace@9..10 " "
                  MatchArm@10..19
                    LiteralType@10..13
                      Nil@10..13 "nil"
                    Whitespace@13..14 " "
                    FatArrow@14..16 "=>"
                    Whitespace@16..17 " "
                    Block@17..19
                      OpenBrace@17..18 "{"
                      CloseBrace@18..19 "}"
                  Whitespace@19..20 " "
                  MatchArm@20..55
                    PairType@20..30
                      OpenParen@20..21 "("
                      PathType@21..24
                        PathSegment@21..24
                          Ident@21..24 "Int"
                      Comma@24..25 ","
                      Whitespace@25..26 " "
                      PathType@26..29
                        PathSegment@26..29
                          Ident@26..29 "Int"
                      CloseParen@29..30 ")"
                    Whitespace@30..31 " "
                    FatArrow@31..33 "=>"
                    Whitespace@33..34 " "
                    IfExpr@34..55
                      If@34..36 "if"
                      Whitespace@36..37 " "
                      PathExpr@37..39
                        PathSegment@37..39
                          Ident@37..38 "a"
                          Whitespace@38..39 " "
                      Block@39..44
                        OpenBrace@39..40 "{"
                        Whitespace@40..41 " "
                        LiteralExpr@41..42
                          Integer@41..42 "1"
                        Whitespace@42..43 " "
                        CloseBrace@43..44 "}"
                      Whitespace@44..45 " "
                      Else@45..49 "else"
                      Whitespace@49..50 " "
                      Block@50..55
                        OpenBrace@50..51 "{"
                        Whitespace@51..52 " "
                        LiteralExpr@52..53
                          Integer@52..53 "2"
                        Whitespace@53..54 " "
                        CloseBrace@54..55 "}"
                  Whitespace@55..56 " "
                  MatchArm@56..79
                    PathType@56..58
                      PathSegment@56..58
                        Ident@56..57 "_"
                        Whitespace@57..58 " "
                    FatArrow@58..60 "=>"
                    Whitespace@60..61 " "
                    MatchExpr@61..79
                      Match@61..66 "match"
                      Whitespace@66..67 " "
                      PathExpr@67..69
                        PathSegment@67..69
                          Ident@67..68 "y"
                          Whitespace@68..69 " "
                      OpenBrace@69..70 "{"
                      Whitespace@70..71 " "
                      MatchArm@71..78
                        PathType@71..73
                          PathSegment@71..73
                            Ident@71..72 "_"
                            Whitespace@72..73 " "
                        FatArrow@73..75 "=>"
                        Whitespace@75..76 " "
                        LiteralExpr@76..77
                          Integer@76..77 "3"
                        Whitespace@77..78 " "
                      CloseBrace@78..79 "}"
                  Whitespace@79..80 " "
                  CloseBrace@80..81 "}"
            "#]],
            expect![],
        );

        // An arm on the next line is still parsed when only its comma is missing
        check(
            expr,
            "match x {\n1 => 2\n_ => 3\n}",
            expect![[r#"
                MatchExpr@0..25
                  Match@0..5 "match"
                  Whitespace@5..6 " "
                  PathExpr@6..8
                    PathSegment@6..8
                      Ident@6..7 "x"
                      Whitespace@7..8 " "
                  OpenBrace@8..9 "{"
                  Whitespace@9..10 "\n"
                  MatchArm@10..17
                    LiteralType@10..11
                      Integer@10..11 "1"
                    Whitespace@11..12 " "
                    FatArrow@12..14 "=>"
                    Whitespace@14..15 " "
                    LiteralExpr@15..16
                      Integer@15..16 "2"
                    Whitespace@16..17 "\n"
                  MatchArm@17..24
                    PathType@17..19
                      PathSegment@17..19
                        Ident@17..18 "_"
                        Whitespace@18..19 " "
                    FatArrow@19..21 "=>"
                    Whitespace@21..22 " "
                    LiteralExpr@22..23
                      Integer@22..23 "3"
                    Whitespace@23..24 "\n"
                  CloseBrace@24..25 "}"
            "#]],
            expect!["Expected one of `,`, `}`, found identifier at main.rue:3:1"],
        );
    }

    #[test]
    fn test_match_arm_bindings() {
        // A binding comes between the arm's type and its `=>`, so a brace there isn't a block
        check(
            expr,
            "match x { A { a, b: c, ...d } => a, (B, C) (e, f) => { e } [D] [g, ...h] => g }",
            expect![[r#"
                MatchExpr@0..79
                  Match@0..5 "match"
                  Whitespace@5..6 " "
                  PathExpr@6..8
                    PathSegment@6..8
                      Ident@6..7 "x"
                      Whitespace@7..8 " "
                  OpenBrace@8..9 "{"
                  Whitespace@9..10 " "
                  MatchArm@10..34
                    PathType@10..12
                      PathSegment@10..12
                        Ident@10..11 "A"
                        Whitespace@11..12 " "
                    StructBinding@12..29
                      OpenBrace@12..13 "{"
                      Whitespace@13..14 " "
                      StructFieldBinding@14..15
                        Ident@14..15 "a"
                      Comma@15..16 ","
                      Whitespace@16..17 " "
                      StructFieldBinding@17..21
                        Ident@17..18 "b"
                        Colon@18..19 ":"
                        Whitespace@19..20 " "
                        NamedBinding@20..21
                          Ident@20..21 "c"
                      Comma@21..22 ","
                      Whitespace@22..23 " "
                      StructFieldBinding@23..28
                        Spread@23..26 "..."
                        Ident@26..27 "d"
                        Whitespace@27..28 " "
                      CloseBrace@28..29 "}"
                    Whitespace@29..30 " "
                    FatArrow@30..32 "=>"
                    Whitespace@32..33 " "
                    PathExpr@33..34
                      PathSegment@33..34
                        Ident@33..34 "a"
                  Comma@34..35 ","
                  Whitespace@35..36 " "
                  MatchArm@36..58
                    PairType@36..42
                      OpenParen@36..37 "("
                      PathType@37..38
                        PathSegment@37..38
                          Ident@37..38 "B"
                      Comma@38..39 ","
                      Whitespace@39..40 " "
                      PathType@40..41
                        PathSegment@40..41
                          Ident@40..41 "C"
                      CloseParen@41..42 ")"
                    Whitespace@42..43 " "
                    PairBinding@43..49
                      OpenParen@43..44 "("
                      NamedBinding@44..45
                        Ident@44..45 "e"
                      Comma@45..46 ","
                      Whitespace@46..47 " "
                      NamedBinding@47..48
                        Ident@47..48 "f"
                      CloseParen@48..49 ")"
                    Whitespace@49..50 " "
                    FatArrow@50..52 "=>"
                    Whitespace@52..53 " "
                    Block@53..58
                      OpenBrace@53..54 "{"
                      Whitespace@54..55 " "
                      PathExpr@55..57
                        PathSegment@55..57
                          Ident@55..56 "e"
                          Whitespace@56..57 " "
                      CloseBrace@57..58 "}"
                  Whitespace@58..59 " "
                  MatchArm@59..78
                    ListType@59..62
                      OpenBracket@59..60 "["
                      ListTypeItem@60..61
                        PathType@60..61
                          PathSegment@60..61
                            Ident@60..61 "D"
                      CloseBracket@61..62 "]"
                    Whitespace@62..63 " "
                    ListBinding@63..72
                      OpenBracket@63..64 "["
                      ListBindingItem@64..65
                        NamedBinding@64..65
                          Ident@64..65 "g"
                      Comma@65..66 ","
                      Whitespace@66..67 " "
                      ListBindingItem@67..71
                        Spread@67..70 "..."
                        NamedBinding@70..71
                          Ident@70..71 "h"
                      CloseBracket@71..72 "]"
                    Whitespace@72..73 " "
                    FatArrow@73..75 "=>"
                    Whitespace@75..76 " "
                    PathExpr@76..78
                      PathSegment@76..78
                        Ident@76..77 "g"
                        Whitespace@77..78 " "
                  CloseBrace@78..79 "}"
            "#]],
            expect![],
        );

        // The binding of a union and of `_`, in an `inline match`
        check(
            expr,
            "inline match x { A | B { a } => a, _ {} => 1 }",
            expect![[r#"
                MatchExpr@0..46
                  Inline@0..6 "inline"
                  Whitespace@6..7 " "
                  Match@7..12 "match"
                  Whitespace@12..13 " "
                  PathExpr@13..15
                    PathSegment@13..15
                      Ident@13..14 "x"
                      Whitespace@14..15 " "
                  OpenBrace@15..16 "{"
                  Whitespace@16..17 " "
                  MatchArm@17..33
                    UnionType@17..23
                      PathType@17..19
                        PathSegment@17..19
                          Ident@17..18 "A"
                          Whitespace@18..19 " "
                      BitwiseOr@19..20 "|"
                      Whitespace@20..21 " "
                      PathType@21..23
                        PathSegment@21..23
                          Ident@21..22 "B"
                          Whitespace@22..23 " "
                    StructBinding@23..28
                      OpenBrace@23..24 "{"
                      Whitespace@24..25 " "
                      StructFieldBinding@25..27
                        Ident@25..26 "a"
                        Whitespace@26..27 " "
                      CloseBrace@27..28 "}"
                    Whitespace@28..29 " "
                    FatArrow@29..31 "=>"
                    Whitespace@31..32 " "
                    PathExpr@32..33
                      PathSegment@32..33
                        Ident@32..33 "a"
                  Comma@33..34 ","
                  Whitespace@34..35 " "
                  MatchArm@35..45
                    PathType@35..37
                      PathSegment@35..37
                        Ident@35..36 "_"
                        Whitespace@36..37 " "
                    StructBinding@37..39
                      OpenBrace@37..38 "{"
                      CloseBrace@38..39 "}"
                    Whitespace@39..40 " "
                    FatArrow@40..42 "=>"
                    Whitespace@42..43 " "
                    LiteralExpr@43..44
                      Integer@43..44 "1"
                    Whitespace@44..45 " "
                  CloseBrace@45..46 "}"
            "#]],
            expect![],
        );

        // A name after the type binds the whole value
        check(
            expr,
            "match x { A a => a, _ rest => rest }",
            expect![[r#"
                MatchExpr@0..36
                  Match@0..5 "match"
                  Whitespace@5..6 " "
                  PathExpr@6..8
                    PathSegment@6..8
                      Ident@6..7 "x"
                      Whitespace@7..8 " "
                  OpenBrace@8..9 "{"
                  Whitespace@9..10 " "
                  MatchArm@10..18
                    PathType@10..12
                      PathSegment@10..12
                        Ident@10..11 "A"
                        Whitespace@11..12 " "
                    NamedBinding@12..13
                      Ident@12..13 "a"
                    Whitespace@13..14 " "
                    FatArrow@14..16 "=>"
                    Whitespace@16..17 " "
                    PathExpr@17..18
                      PathSegment@17..18
                        Ident@17..18 "a"
                  Comma@18..19 ","
                  Whitespace@19..20 " "
                  MatchArm@20..35
                    PathType@20..22
                      PathSegment@20..22
                        Ident@20..21 "_"
                        Whitespace@21..22 " "
                    NamedBinding@22..26
                      Ident@22..26 "rest"
                    Whitespace@26..27 " "
                    FatArrow@27..29 "=>"
                    Whitespace@29..30 " "
                    PathExpr@30..35
                      PathSegment@30..35
                        Ident@30..34 "rest"
                        Whitespace@34..35 " "
                  CloseBrace@35..36 "}"
            "#]],
            expect![],
        );

        // In a statement
        check(
            block,
            "{ match x { A { a } => { return a; } _ => {} } x }",
            expect![[r#"
                Block@0..50
                  OpenBrace@0..1 "{"
                  Whitespace@1..2 " "
                  MatchExpr@2..46
                    Match@2..7 "match"
                    Whitespace@7..8 " "
                    PathExpr@8..10
                      PathSegment@8..10
                        Ident@8..9 "x"
                        Whitespace@9..10 " "
                    OpenBrace@10..11 "{"
                    Whitespace@11..12 " "
                    MatchArm@12..36
                      PathType@12..14
                        PathSegment@12..14
                          Ident@12..13 "A"
                          Whitespace@13..14 " "
                      StructBinding@14..19
                        OpenBrace@14..15 "{"
                        Whitespace@15..16 " "
                        StructFieldBinding@16..18
                          Ident@16..17 "a"
                          Whitespace@17..18 " "
                        CloseBrace@18..19 "}"
                      Whitespace@19..20 " "
                      FatArrow@20..22 "=>"
                      Whitespace@22..23 " "
                      Block@23..36
                        OpenBrace@23..24 "{"
                        Whitespace@24..25 " "
                        ReturnStmt@25..34
                          Return@25..31 "return"
                          Whitespace@31..32 " "
                          PathExpr@32..33
                            PathSegment@32..33
                              Ident@32..33 "a"
                          Semicolon@33..34 ";"
                        Whitespace@34..35 " "
                        CloseBrace@35..36 "}"
                    Whitespace@36..37 " "
                    MatchArm@37..44
                      PathType@37..39
                        PathSegment@37..39
                          Ident@37..38 "_"
                          Whitespace@38..39 " "
                      FatArrow@39..41 "=>"
                      Whitespace@41..42 " "
                      Block@42..44
                        OpenBrace@42..43 "{"
                        CloseBrace@43..44 "}"
                    Whitespace@44..45 " "
                    CloseBrace@45..46 "}"
                  Whitespace@46..47 " "
                  PathExpr@47..49
                    PathSegment@47..49
                      Ident@47..48 "x"
                      Whitespace@48..49 " "
                  CloseBrace@49..50 "}"
            "#]],
            expect![],
        );
    }

    #[test]
    fn test_match_arm_binding_errors() {
        // A brace that doesn't look like a binding is a block that's missing its `=>`
        check(
            expr,
            "match x { A { 1 } _ => 2 }",
            expect![[r#"
                MatchExpr@0..26
                  Match@0..5 "match"
                  Whitespace@5..6 " "
                  PathExpr@6..8
                    PathSegment@6..8
                      Ident@6..7 "x"
                      Whitespace@7..8 " "
                  OpenBrace@8..9 "{"
                  Whitespace@9..10 " "
                  MatchArm@10..17
                    PathType@10..12
                      PathSegment@10..12
                        Ident@10..11 "A"
                        Whitespace@11..12 " "
                    Block@12..17
                      OpenBrace@12..13 "{"
                      Whitespace@13..14 " "
                      LiteralExpr@14..15
                        Integer@14..15 "1"
                      Whitespace@15..16 " "
                      CloseBrace@16..17 "}"
                  Whitespace@17..18 " "
                  MatchArm@18..25
                    PathType@18..20
                      PathSegment@18..20
                        Ident@18..19 "_"
                        Whitespace@19..20 " "
                    FatArrow@20..22 "=>"
                    Whitespace@22..23 " "
                    LiteralExpr@23..24
                      Integer@23..24 "2"
                    Whitespace@24..25 " "
                  CloseBrace@25..26 "}"
            "#]],
            expect!["Expected `=>`, found `{` at main.rue:1:13"],
        );

        // A binding can't check anything, so it has no literals or types in it
        check(
            expr,
            "match x { (A, B) (1, b) => b }",
            expect![[r#"
                MatchExpr@0..30
                  Match@0..5 "match"
                  Whitespace@5..6 " "
                  PathExpr@6..8
                    PathSegment@6..8
                      Ident@6..7 "x"
                      Whitespace@7..8 " "
                  OpenBrace@8..9 "{"
                  Whitespace@9..10 " "
                  MatchArm@10..29
                    PairType@10..16
                      OpenParen@10..11 "("
                      PathType@11..12
                        PathSegment@11..12
                          Ident@11..12 "A"
                      Comma@12..13 ","
                      Whitespace@13..14 " "
                      PathType@14..15
                        PathSegment@14..15
                          Ident@14..15 "B"
                      CloseParen@15..16 ")"
                    Whitespace@16..17 " "
                    PairBinding@17..23
                      OpenParen@17..18 "("
                      Error@18..19 "1"
                      Comma@19..20 ","
                      Whitespace@20..21 " "
                      NamedBinding@21..22
                        Ident@21..22 "b"
                      CloseParen@22..23 ")"
                    Whitespace@23..24 " "
                    FatArrow@24..26 "=>"
                    Whitespace@26..27 " "
                    PathExpr@27..29
                      PathSegment@27..29
                        Ident@27..28 "b"
                        Whitespace@28..29 " "
                  CloseBrace@29..30 "}"
            "#]],
            expect!["Expected binding, found integer literal at main.rue:1:19"],
        );
    }

    #[test]
    fn test_if_expr() {
        check(
            expr,
            "if true { 42 } else { 84 }",
            expect![[r#"
                IfExpr@0..26
                  If@0..2 "if"
                  Whitespace@2..3 " "
                  LiteralExpr@3..7
                    True@3..7 "true"
                  Whitespace@7..8 " "
                  Block@8..14
                    OpenBrace@8..9 "{"
                    Whitespace@9..10 " "
                    LiteralExpr@10..12
                      Integer@10..12 "42"
                    Whitespace@12..13 " "
                    CloseBrace@13..14 "}"
                  Whitespace@14..15 " "
                  Else@15..19 "else"
                  Whitespace@19..20 " "
                  Block@20..26
                    OpenBrace@20..21 "{"
                    Whitespace@21..22 " "
                    LiteralExpr@22..24
                      Integer@22..24 "84"
                    Whitespace@24..25 " "
                    CloseBrace@25..26 "}"
            "#]],
            expect![""],
        );

        check(
            expr,
            "if true { 42 } else if false { 84 } else { 91 }",
            expect![[r#"
                IfExpr@0..47
                  If@0..2 "if"
                  Whitespace@2..3 " "
                  LiteralExpr@3..7
                    True@3..7 "true"
                  Whitespace@7..8 " "
                  Block@8..14
                    OpenBrace@8..9 "{"
                    Whitespace@9..10 " "
                    LiteralExpr@10..12
                      Integer@10..12 "42"
                    Whitespace@12..13 " "
                    CloseBrace@13..14 "}"
                  Whitespace@14..15 " "
                  Else@15..19 "else"
                  Whitespace@19..20 " "
                  IfExpr@20..47
                    If@20..22 "if"
                    Whitespace@22..23 " "
                    LiteralExpr@23..28
                      False@23..28 "false"
                    Whitespace@28..29 " "
                    Block@29..35
                      OpenBrace@29..30 "{"
                      Whitespace@30..31 " "
                      LiteralExpr@31..33
                        Integer@31..33 "84"
                      Whitespace@33..34 " "
                      CloseBrace@34..35 "}"
                    Whitespace@35..36 " "
                    Else@36..40 "else"
                    Whitespace@40..41 " "
                    Block@41..47
                      OpenBrace@41..42 "{"
                      Whitespace@42..43 " "
                      LiteralExpr@43..45
                        Integer@43..45 "91"
                      Whitespace@45..46 " "
                      CloseBrace@46..47 "}"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_field_access_expr() {
        check(
            expr,
            "hello.world",
            expect![[r#"
                FieldAccessExpr@0..11
                  PathExpr@0..5
                    PathSegment@0..5
                      Ident@0..5 "hello"
                  Dot@5..6 "."
                  Ident@6..11 "world"
            "#]],
            expect![""],
        );

        check(
            expr,
            "hello.",
            expect![[r#"
                FieldAccessExpr@0..6
                  PathExpr@0..5
                    PathSegment@0..5
                      Ident@0..5 "hello"
                  Dot@5..6 "."
            "#]],
            expect!["Expected identifier, found eof at main.rue:1:7"],
        );
    }

    #[test]
    fn test_guard_expr_logical_or() {
        check(
            expr,
            "value is Int || other",
            expect![[r#"
            BinaryExpr@0..21
              GuardExpr@0..13
                PathExpr@0..6
                  PathSegment@0..6
                    Ident@0..5 "value"
                    Whitespace@5..6 " "
                Is@6..8 "is"
                Whitespace@8..9 " "
                PathType@9..13
                  PathSegment@9..13
                    Ident@9..12 "Int"
                    Whitespace@12..13 " "
              Or@13..15 "||"
              Whitespace@15..16 " "
              PathExpr@16..21
                PathSegment@16..21
                  Ident@16..21 "other"
        "#]],
            expect![],
        );
        check(
            expr,
            "value as Int || other",
            expect![[r#"
            BinaryExpr@0..21
              CastExpr@0..13
                PathExpr@0..6
                  PathSegment@0..6
                    Ident@0..5 "value"
                    Whitespace@5..6 " "
                As@6..8 "as"
                Whitespace@8..9 " "
                PathType@9..13
                  PathSegment@9..13
                    Ident@9..12 "Int"
                    Whitespace@12..13 " "
              Or@13..15 "||"
              Whitespace@15..16 " "
              PathExpr@16..21
                PathSegment@16..21
                  Ident@16..21 "other"
        "#]],
            expect![],
        );
        check(
            expr,
            "value is Int | Bytes || other",
            expect![[r#"
            BinaryExpr@0..29
              GuardExpr@0..21
                PathExpr@0..6
                  PathSegment@0..6
                    Ident@0..5 "value"
                    Whitespace@5..6 " "
                Is@6..8 "is"
                Whitespace@8..9 " "
                UnionType@9..21
                  PathType@9..13
                    PathSegment@9..13
                      Ident@9..12 "Int"
                      Whitespace@12..13 " "
                  BitwiseOr@13..14 "|"
                  Whitespace@14..15 " "
                  PathType@15..21
                    PathSegment@15..21
                      Ident@15..20 "Bytes"
                      Whitespace@20..21 " "
              Or@21..23 "||"
              Whitespace@23..24 " "
              PathExpr@24..29
                PathSegment@24..29
                  Ident@24..29 "other"
        "#]],
            expect![],
        );
        check(
            expr,
            "value is Int | Bytes",
            expect![[r#"
            GuardExpr@0..20
              PathExpr@0..6
                PathSegment@0..6
                  Ident@0..5 "value"
                  Whitespace@5..6 " "
              Is@6..8 "is"
              Whitespace@8..9 " "
              UnionType@9..20
                PathType@9..13
                  PathSegment@9..13
                    Ident@9..12 "Int"
                    Whitespace@12..13 " "
                BitwiseOr@13..14 "|"
                Whitespace@14..15 " "
                PathType@15..20
                  PathSegment@15..20
                    Ident@15..20 "Bytes"
        "#]],
            expect![],
        );
    }

    #[test]
    fn test_path_expr() {
        check(
            expr,
            "hello::world",
            expect![[r#"
                PathExpr@0..12
                  PathSegment@0..5
                    Ident@0..5 "hello"
                  PathSegment@5..12
                    PathSeparator@5..7 "::"
                    Ident@7..12 "world"
            "#]],
            expect![""],
        );

        check(
            expr,
            "::absolute",
            expect![[r#"
                PathExpr@0..10
                  PathSegment@0..10
                    PathSeparator@0..2 "::"
                    Ident@2..10 "absolute"
            "#]],
            expect![""],
        );

        check(
            expr,
            "super::value",
            expect![[r#"
                PathExpr@0..12
                  PathSegment@0..5
                    Super@0..5 "super"
                  PathSegment@5..12
                    PathSeparator@5..7 "::"
                    Ident@7..12 "value"
            "#]],
            expect![""],
        );

        check(
            expr,
            "project::value",
            expect![[r#"
                PathExpr@0..14
                  PathSegment@0..7
                    Project@0..7 "project"
                  PathSegment@7..14
                    PathSeparator@7..9 "::"
                    Ident@9..14 "value"
            "#]],
            expect![""],
        );

        check(
            expr,
            "identity::<Int>",
            expect![[r#"
                PathExpr@0..15
                  PathSegment@0..15
                    Ident@0..8 "identity"
                    PathSeparator@8..10 "::"
                    GenericArguments@10..15
                      LessThan@10..11 "<"
                      PathType@11..14
                        PathSegment@11..14
                          Ident@11..14 "Int"
                      GreaterThan@14..15 ">"
            "#]],
            expect![""],
        );

        check(
            expr,
            "a::b::<Int, Bytes>::c",
            expect![[r#"
                PathExpr@0..21
                  PathSegment@0..1
                    Ident@0..1 "a"
                  PathSegment@1..18
                    PathSeparator@1..3 "::"
                    Ident@3..4 "b"
                    PathSeparator@4..6 "::"
                    GenericArguments@6..18
                      LessThan@6..7 "<"
                      PathType@7..10
                        PathSegment@7..10
                          Ident@7..10 "Int"
                      Comma@10..11 ","
                      Whitespace@11..12 " "
                      PathType@12..17
                        PathSegment@12..17
                          Ident@12..17 "Bytes"
                      GreaterThan@17..18 ">"
                  PathSegment@18..21
                    PathSeparator@18..20 "::"
                    Ident@20..21 "c"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_struct_initializer_expr() {
        check(
            expr,
            "Point {}",
            expect![[r#"
                StructInitializerExpr@0..8
                  PathExpr@0..6
                    PathSegment@0..6
                      Ident@0..5 "Point"
                      Whitespace@5..6 " "
                  OpenBrace@6..7 "{"
                  CloseBrace@7..8 "}"
            "#]],
            expect![""],
        );

        check(
            expr,
            "Point { x: 1, y: 2 }",
            expect![[r#"
                StructInitializerExpr@0..20
                  PathExpr@0..6
                    PathSegment@0..6
                      Ident@0..5 "Point"
                      Whitespace@5..6 " "
                  OpenBrace@6..7 "{"
                  Whitespace@7..8 " "
                  StructInitializerField@8..12
                    Ident@8..9 "x"
                    Colon@9..10 ":"
                    Whitespace@10..11 " "
                    LiteralExpr@11..12
                      Integer@11..12 "1"
                  Comma@12..13 ","
                  Whitespace@13..14 " "
                  StructInitializerField@14..19
                    Ident@14..15 "y"
                    Colon@15..16 ":"
                    Whitespace@16..17 " "
                    LiteralExpr@17..18
                      Integer@17..18 "2"
                    Whitespace@18..19 " "
                  CloseBrace@19..20 "}"
            "#]],
            expect![""],
        );

        check(
            expr,
            "Point { x, y, }",
            expect![[r#"
                StructInitializerExpr@0..15
                  PathExpr@0..6
                    PathSegment@0..6
                      Ident@0..5 "Point"
                      Whitespace@5..6 " "
                  OpenBrace@6..7 "{"
                  Whitespace@7..8 " "
                  StructInitializerField@8..9
                    Ident@8..9 "x"
                  Comma@9..10 ","
                  Whitespace@10..11 " "
                  StructInitializerField@11..12
                    Ident@11..12 "y"
                  Comma@12..13 ","
                  Whitespace@13..14 " "
                  CloseBrace@14..15 "}"
            "#]],
            expect![""],
        );

        check(
            expr,
            "module::Point { x }",
            expect![[r#"
                StructInitializerExpr@0..19
                  PathExpr@0..14
                    PathSegment@0..6
                      Ident@0..6 "module"
                    PathSegment@6..14
                      PathSeparator@6..8 "::"
                      Ident@8..13 "Point"
                      Whitespace@13..14 " "
                  OpenBrace@14..15 "{"
                  Whitespace@15..16 " "
                  StructInitializerField@16..18
                    Ident@16..17 "x"
                    Whitespace@17..18 " "
                  CloseBrace@18..19 "}"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_pair_expr() {
        check(
            expr,
            "(1, 2)",
            expect![[r#"
                PairExpr@0..6
                  OpenParen@0..1 "("
                  LiteralExpr@1..2
                    Integer@1..2 "1"
                  Comma@2..3 ","
                  Whitespace@3..4 " "
                  LiteralExpr@4..5
                    Integer@4..5 "2"
                  CloseParen@5..6 ")"
            "#]],
            expect![""],
        );

        check(
            expr,
            "(1, 2,)",
            expect![[r#"
                PairExpr@0..7
                  OpenParen@0..1 "("
                  LiteralExpr@1..2
                    Integer@1..2 "1"
                  Comma@2..3 ","
                  Whitespace@3..4 " "
                  LiteralExpr@4..5
                    Integer@4..5 "2"
                  Comma@5..6 ","
                  CloseParen@6..7 ")"
            "#]],
            expect![""],
        );

        check(
            expr,
            "(1, (2, 3))",
            expect![[r#"
                PairExpr@0..11
                  OpenParen@0..1 "("
                  LiteralExpr@1..2
                    Integer@1..2 "1"
                  Comma@2..3 ","
                  Whitespace@3..4 " "
                  PairExpr@4..10
                    OpenParen@4..5 "("
                    LiteralExpr@5..6
                      Integer@5..6 "2"
                    Comma@6..7 ","
                    Whitespace@7..8 " "
                    LiteralExpr@8..9
                      Integer@8..9 "3"
                    CloseParen@9..10 ")"
                  CloseParen@10..11 ")"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_list_expr() {
        check(
            expr,
            "[]",
            expect![[r#"
                ListExpr@0..2
                  OpenBracket@0..1 "["
                  CloseBracket@1..2 "]"
            "#]],
            expect![""],
        );

        check(
            expr,
            "[1, 2, 3]",
            expect![[r#"
                ListExpr@0..9
                  OpenBracket@0..1 "["
                  ListItem@1..2
                    LiteralExpr@1..2
                      Integer@1..2 "1"
                  Comma@2..3 ","
                  Whitespace@3..4 " "
                  ListItem@4..5
                    LiteralExpr@4..5
                      Integer@4..5 "2"
                  Comma@5..6 ","
                  Whitespace@6..7 " "
                  ListItem@7..8
                    LiteralExpr@7..8
                      Integer@7..8 "3"
                  CloseBracket@8..9 "]"
            "#]],
            expect![""],
        );

        check(
            expr,
            "[1, 2,]",
            expect![[r#"
                ListExpr@0..7
                  OpenBracket@0..1 "["
                  ListItem@1..2
                    LiteralExpr@1..2
                      Integer@1..2 "1"
                  Comma@2..3 ","
                  Whitespace@3..4 " "
                  ListItem@4..5
                    LiteralExpr@4..5
                      Integer@4..5 "2"
                  Comma@5..6 ","
                  CloseBracket@6..7 "]"
            "#]],
            expect![""],
        );

        check(
            expr,
            "[1, ...rest]",
            expect![[r#"
                ListExpr@0..12
                  OpenBracket@0..1 "["
                  ListItem@1..2
                    LiteralExpr@1..2
                      Integer@1..2 "1"
                  Comma@2..3 ","
                  Whitespace@3..4 " "
                  ListItem@4..11
                    Spread@4..7 "..."
                    PathExpr@7..11
                      PathSegment@7..11
                        Ident@7..11 "rest"
                  CloseBracket@11..12 "]"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_lambda_expr() {
        check(
            expr,
            "fn(x: Int) => x",
            expect![[r#"
                LambdaExpr@0..15
                  Fn@0..2 "fn"
                  OpenParen@2..3 "("
                  FunctionParameter@3..9
                    NamedBinding@3..4
                      Ident@3..4 "x"
                    Colon@4..5 ":"
                    Whitespace@5..6 " "
                    PathType@6..9
                      PathSegment@6..9
                        Ident@6..9 "Int"
                  CloseParen@9..10 ")"
                  Whitespace@10..11 " "
                  FatArrow@11..13 "=>"
                  Whitespace@13..14 " "
                  PathExpr@14..15
                    PathSegment@14..15
                      Ident@14..15 "x"
            "#]],
            expect![""],
        );

        check(
            expr,
            "fn() => 42",
            expect![[r#"
                LambdaExpr@0..10
                  Fn@0..2 "fn"
                  OpenParen@2..3 "("
                  CloseParen@3..4 ")"
                  Whitespace@4..5 " "
                  FatArrow@5..7 "=>"
                  Whitespace@7..8 " "
                  LiteralExpr@8..10
                    Integer@8..10 "42"
            "#]],
            expect![""],
        );

        check(
            expr,
            "fn(x) => x",
            expect![[r#"
                LambdaExpr@0..10
                  Fn@0..2 "fn"
                  OpenParen@2..3 "("
                  FunctionParameter@3..4
                    NamedBinding@3..4
                      Ident@3..4 "x"
                  CloseParen@4..5 ")"
                  Whitespace@5..6 " "
                  FatArrow@6..8 "=>"
                  Whitespace@8..9 " "
                  PathExpr@9..10
                    PathSegment@9..10
                      Ident@9..10 "x"
            "#]],
            expect![""],
        );

        check(
            expr,
            "fn<T>(x: T): T => x",
            expect![[r#"
                LambdaExpr@0..19
                  Fn@0..2 "fn"
                  GenericParameters@2..5
                    LessThan@2..3 "<"
                    Ident@3..4 "T"
                    GreaterThan@4..5 ">"
                  OpenParen@5..6 "("
                  FunctionParameter@6..10
                    NamedBinding@6..7
                      Ident@6..7 "x"
                    Colon@7..8 ":"
                    Whitespace@8..9 " "
                    PathType@9..10
                      PathSegment@9..10
                        Ident@9..10 "T"
                  CloseParen@10..11 ")"
                  Colon@11..12 ":"
                  Whitespace@12..13 " "
                  PathType@13..15
                    PathSegment@13..15
                      Ident@13..14 "T"
                      Whitespace@14..15 " "
                  FatArrow@15..17 "=>"
                  Whitespace@17..18 " "
                  PathExpr@18..19
                    PathSegment@18..19
                      Ident@18..19 "x"
            "#]],
            expect![""],
        );

        check(
            expr,
            "fn(...args: List<Int>) => args",
            expect![[r#"
                LambdaExpr@0..30
                  Fn@0..2 "fn"
                  OpenParen@2..3 "("
                  FunctionParameter@3..21
                    Spread@3..6 "..."
                    NamedBinding@6..10
                      Ident@6..10 "args"
                    Colon@10..11 ":"
                    Whitespace@11..12 " "
                    PathType@12..21
                      PathSegment@12..21
                        Ident@12..16 "List"
                        GenericArguments@16..21
                          LessThan@16..17 "<"
                          PathType@17..20
                            PathSegment@17..20
                              Ident@17..20 "Int"
                          GreaterThan@20..21 ">"
                  CloseParen@21..22 ")"
                  Whitespace@22..23 " "
                  FatArrow@23..25 "=>"
                  Whitespace@25..26 " "
                  PathExpr@26..30
                    PathSegment@26..30
                      Ident@26..30 "args"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_cast_expr() {
        check(
            expr,
            "value as Int",
            expect![[r#"
                CastExpr@0..12
                  PathExpr@0..6
                    PathSegment@0..6
                      Ident@0..5 "value"
                      Whitespace@5..6 " "
                  As@6..8 "as"
                  Whitespace@8..9 " "
                  PathType@9..12
                    PathSegment@9..12
                      Ident@9..12 "Int"
            "#]],
            expect![""],
        );

        check(
            expr,
            "value as Int + 1",
            expect![[r#"
                BinaryExpr@0..16
                  CastExpr@0..13
                    PathExpr@0..6
                      PathSegment@0..6
                        Ident@0..5 "value"
                        Whitespace@5..6 " "
                    As@6..8 "as"
                    Whitespace@8..9 " "
                    PathType@9..13
                      PathSegment@9..13
                        Ident@9..12 "Int"
                        Whitespace@12..13 " "
                  Plus@13..14 "+"
                  Whitespace@14..15 " "
                  LiteralExpr@15..16
                    Integer@15..16 "1"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_function_call_spread() {
        check(
            expr,
            "hello(...args)",
            expect![[r#"
                FunctionCallExpr@0..14
                  PathExpr@0..5
                    PathSegment@0..5
                      Ident@0..5 "hello"
                  OpenParen@5..6 "("
                  ListItem@6..13
                    Spread@6..9 "..."
                    PathExpr@9..13
                      PathSegment@9..13
                        Ident@9..13 "args"
                  CloseParen@13..14 ")"
            "#]],
            expect![""],
        );

        check(
            expr,
            "hello(1, ...rest)",
            expect![[r#"
                FunctionCallExpr@0..17
                  PathExpr@0..5
                    PathSegment@0..5
                      Ident@0..5 "hello"
                  OpenParen@5..6 "("
                  ListItem@6..7
                    LiteralExpr@6..7
                      Integer@6..7 "1"
                  Comma@7..8 ","
                  Whitespace@8..9 " "
                  ListItem@9..16
                    Spread@9..12 "..."
                    PathExpr@12..16
                      PathSegment@12..16
                        Ident@12..16 "rest"
                  CloseParen@16..17 ")"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_index_expr() {
        check(
            expr,
            "a[0]",
            expect![[r#"
            IndexExpr@0..4
              PathExpr@0..1
                PathSegment@0..1
                  Ident@0..1 "a"
              OpenBracket@1..2 "["
              LiteralExpr@2..3
                Integer@2..3 "0"
              CloseBracket@3..4 "]"
        "#]],
            expect![],
        );
    }

    #[test]
    fn test_slice_expr() {
        check(
            expr,
            "a[1..b]",
            expect![[r#"
            SliceExpr@0..7
              PathExpr@0..1
                PathSegment@0..1
                  Ident@0..1 "a"
              OpenBracket@1..2 "["
              LiteralExpr@2..3
                Integer@2..3 "1"
              Range@3..5 ".."
              PathExpr@5..6
                PathSegment@5..6
                  Ident@5..6 "b"
              CloseBracket@6..7 "]"
        "#]],
            expect![],
        );
    }

    #[test]
    fn test_slice_expr_without_start() {
        check(
            expr,
            "a[..2]",
            expect![[r#"
            SliceExpr@0..6
              PathExpr@0..1
                PathSegment@0..1
                  Ident@0..1 "a"
              OpenBracket@1..2 "["
              Range@2..4 ".."
              LiteralExpr@4..5
                Integer@4..5 "2"
              CloseBracket@5..6 "]"
        "#]],
            expect![],
        );
    }

    #[test]
    fn test_slice_expr_without_end() {
        check(
            expr,
            "a[b + 1..]",
            expect![[r#"
            SliceExpr@0..10
              PathExpr@0..1
                PathSegment@0..1
                  Ident@0..1 "a"
              OpenBracket@1..2 "["
              BinaryExpr@2..7
                PathExpr@2..4
                  PathSegment@2..4
                    Ident@2..3 "b"
                    Whitespace@3..4 " "
                Plus@4..5 "+"
                Whitespace@5..6 " "
                LiteralExpr@6..7
                  Integer@6..7 "1"
              Range@7..9 ".."
              CloseBracket@9..10 "]"
        "#]],
            expect![],
        );
    }

    #[test]
    fn test_slice_expr_without_bounds() {
        check(
            expr,
            "a[..]",
            expect![[r#"
            SliceExpr@0..5
              PathExpr@0..1
                PathSegment@0..1
                  Ident@0..1 "a"
              OpenBracket@1..2 "["
              Range@2..4 ".."
              CloseBracket@4..5 "]"
        "#]],
            expect![],
        );
    }

    /// A `.` that's followed by a name is still a field access in a bound.
    #[test]
    fn test_slice_expr_field_bounds() {
        check(
            expr,
            "a[b.c..d.e]",
            expect![[r#"
            SliceExpr@0..11
              PathExpr@0..1
                PathSegment@0..1
                  Ident@0..1 "a"
              OpenBracket@1..2 "["
              FieldAccessExpr@2..5
                PathExpr@2..3
                  PathSegment@2..3
                    Ident@2..3 "b"
                Dot@3..4 "."
                Ident@4..5 "c"
              Range@5..7 ".."
              FieldAccessExpr@7..10
                PathExpr@7..8
                  PathSegment@7..8
                    Ident@7..8 "d"
                Dot@8..9 "."
                Ident@9..10 "e"
              CloseBracket@10..11 "]"
        "#]],
            expect![],
        );
    }

    /// An index binds like the other postfix operators, more tightly than a prefix operator.
    #[test]
    fn test_chained_index_expr() {
        check(
            expr,
            "-a.b[0][1..](2) as Int",
            expect![[r#"
            PrefixExpr@0..22
              Minus@0..1 "-"
              CastExpr@1..22
                FunctionCallExpr@1..15
                  SliceExpr@1..12
                    IndexExpr@1..7
                      FieldAccessExpr@1..4
                        PathExpr@1..2
                          PathSegment@1..2
                            Ident@1..2 "a"
                        Dot@2..3 "."
                        Ident@3..4 "b"
                      OpenBracket@4..5 "["
                      LiteralExpr@5..6
                        Integer@5..6 "0"
                      CloseBracket@6..7 "]"
                    OpenBracket@7..8 "["
                    LiteralExpr@8..9
                      Integer@8..9 "1"
                    Range@9..11 ".."
                    CloseBracket@11..12 "]"
                  OpenParen@12..13 "("
                  ListItem@13..14
                    LiteralExpr@13..14
                      Integer@13..14 "2"
                  CloseParen@14..15 ")"
                Whitespace@15..16 " "
                As@16..18 "as"
                Whitespace@18..19 " "
                PathType@19..22
                  PathSegment@19..22
                    Ident@19..22 "Int"
        "#]],
            expect![],
        );
    }

    /// Like a call, an index continues the expression before it on the next line too.
    #[test]
    fn test_index_expr_on_next_line() {
        check(
            expr,
            "a\n[0]",
            expect![[r#"
            IndexExpr@0..5
              PathExpr@0..2
                PathSegment@0..2
                  Ident@0..1 "a"
                  Whitespace@1..2 "\n"
              OpenBracket@2..3 "["
              LiteralExpr@3..4
                Integer@3..4 "0"
              CloseBracket@4..5 "]"
        "#]],
            expect![],
        );
    }

    /// The brackets after a cast index the cast value, since a type doesn't end with brackets.
    #[test]
    fn test_index_expr_after_cast() {
        check(
            expr,
            "a as Bytes[0]",
            expect![[r#"
            IndexExpr@0..13
              CastExpr@0..10
                PathExpr@0..2
                  PathSegment@0..2
                    Ident@0..1 "a"
                    Whitespace@1..2 " "
                As@2..4 "as"
                Whitespace@4..5 " "
                PathType@5..10
                  PathSegment@5..10
                    Ident@5..10 "Bytes"
              OpenBracket@10..11 "["
              LiteralExpr@11..12
                Integer@11..12 "0"
              CloseBracket@12..13 "]"
        "#]],
            expect![],
        );
    }

    #[test]
    fn test_index_expr_missing_index() {
        check(
            expr,
            "a[]",
            expect![[r#"
            IndexExpr@0..3
              PathExpr@0..1
                PathSegment@0..1
                  Ident@0..1 "a"
              OpenBracket@1..2 "["
              CloseBracket@2..3 "]"
        "#]],
            expect!["Expected one of `..`, expression, found `]` at main.rue:1:3"],
        );
    }

    /// Three dots are a field access that's missing its name, followed by the `..` of a slice,
    /// rather than a spread.
    #[test]
    fn test_slice_expr_with_spread() {
        check(
            expr,
            "a[1...2]",
            expect![[r#"
                SliceExpr@0..8
                  PathExpr@0..1
                    PathSegment@0..1
                      Ident@0..1 "a"
                  OpenBracket@1..2 "["
                  FieldAccessExpr@2..4
                    LiteralExpr@2..3
                      Integer@2..3 "1"
                    Dot@3..4 "."
                  Range@4..6 ".."
                  LiteralExpr@6..7
                    Integer@6..7 "2"
                  CloseBracket@7..8 "]"
            "#]],
            expect!["Expected identifier, found `.` at main.rue:1:5"],
        );
    }

    #[test]
    fn test_slice_expr_with_two_ranges() {
        check(
            expr,
            "a[1..2..3]",
            expect![[r#"
                SliceExpr@0..10
                  PathExpr@0..1
                    PathSegment@0..1
                      Ident@0..1 "a"
                  OpenBracket@1..2 "["
                  LiteralExpr@2..3
                    Integer@2..3 "1"
                  Range@3..5 ".."
                  LiteralExpr@5..6
                    Integer@5..6 "2"
                  Error@6..7 "."
                  Error@7..8 "."
                  Error@8..9 "3"
                  CloseBracket@9..10 "]"
            "#]],
            expect!["Expected `]`, found `.` at main.rue:1:7"],
        );
    }

    #[test]
    fn test_index_expr_unclosed() {
        check(
            expr,
            "a[1",
            expect![[r#"
            IndexExpr@0..3
              PathExpr@0..1
                PathSegment@0..1
                  Ident@0..1 "a"
              OpenBracket@1..2 "["
              LiteralExpr@2..3
                Integer@2..3 "1"
        "#]],
            expect!["Expected one of `..`, `]`, found eof at main.rue:1:4"],
        );
    }

    /// Without the brackets of a slice, a `.` is a field access even if another one follows it.
    #[test]
    fn test_range_outside_of_index() {
        check(
            expr,
            "a..b",
            expect![[r#"
            FieldAccessExpr@0..2
              PathExpr@0..1
                PathSegment@0..1
                  Ident@0..1 "a"
              Dot@1..2 "."
        "#]],
            expect!["Expected identifier, found `.` at main.rue:1:3"],
        );
    }

    #[test]
    fn test_chained_postfix_expr() {
        check(
            expr,
            "a.b(1).c as Int is Bytes",
            expect![[r#"
                GuardExpr@0..24
                  CastExpr@0..16
                    FieldAccessExpr@0..8
                      FunctionCallExpr@0..6
                        FieldAccessExpr@0..3
                          PathExpr@0..1
                            PathSegment@0..1
                              Ident@0..1 "a"
                          Dot@1..2 "."
                          Ident@2..3 "b"
                        OpenParen@3..4 "("
                        ListItem@4..5
                          LiteralExpr@4..5
                            Integer@4..5 "1"
                        CloseParen@5..6 ")"
                      Dot@6..7 "."
                      Ident@7..8 "c"
                    Whitespace@8..9 " "
                    As@9..11 "as"
                    Whitespace@11..12 " "
                    PathType@12..16
                      PathSegment@12..16
                        Ident@12..15 "Int"
                        Whitespace@15..16 " "
                  Is@16..18 "is"
                  Whitespace@18..19 " "
                  PathType@19..24
                    PathSegment@19..24
                      Ident@19..24 "Bytes"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_shift_and_comparison_operators() {
        check(
            expr,
            "1 >> 2",
            expect![[r#"
                BinaryExpr@0..6
                  LiteralExpr@0..1
                    Integer@0..1 "1"
                  Whitespace@1..2 " "
                  RightArithmeticShift@2..4 ">>"
                  Whitespace@4..5 " "
                  LiteralExpr@5..6
                    Integer@5..6 "2"
            "#]],
            expect![""],
        );

        check(
            expr,
            "1 >>> 2",
            expect![[r#"
                BinaryExpr@0..7
                  LiteralExpr@0..1
                    Integer@0..1 "1"
                  Whitespace@1..2 " "
                  RightLogicalShift@2..5 ">>>"
                  Whitespace@5..6 " "
                  LiteralExpr@6..7
                    Integer@6..7 "2"
            "#]],
            expect![""],
        );

        check(
            expr,
            "1 << 2",
            expect![[r#"
                BinaryExpr@0..6
                  LiteralExpr@0..1
                    Integer@0..1 "1"
                  Whitespace@1..2 " "
                  LeftArithmeticShift@2..4 "<<"
                  Whitespace@4..5 " "
                  LiteralExpr@5..6
                    Integer@5..6 "2"
            "#]],
            expect![""],
        );

        check(
            expr,
            "1 <= 2 && 3 >= 4",
            expect![[r#"
                BinaryExpr@0..16
                  BinaryExpr@0..7
                    LiteralExpr@0..1
                      Integer@0..1 "1"
                    Whitespace@1..2 " "
                    LessThanEquals@2..4 "<="
                    Whitespace@4..5 " "
                    LiteralExpr@5..6
                      Integer@5..6 "2"
                    Whitespace@6..7 " "
                  And@7..9 "&&"
                  Whitespace@9..10 " "
                  BinaryExpr@10..16
                    LiteralExpr@10..11
                      Integer@10..11 "3"
                    Whitespace@11..12 " "
                    GreaterThanEquals@12..14 ">="
                    Whitespace@14..15 " "
                    LiteralExpr@15..16
                      Integer@15..16 "4"
            "#]],
            expect![""],
        );

        check(
            expr,
            "1 != 2 == true",
            expect![[r#"
                BinaryExpr@0..14
                  BinaryExpr@0..7
                    LiteralExpr@0..1
                      Integer@0..1 "1"
                    Whitespace@1..2 " "
                    NotEquals@2..4 "!="
                    Whitespace@4..5 " "
                    LiteralExpr@5..6
                      Integer@5..6 "2"
                    Whitespace@6..7 " "
                  Equals@7..9 "=="
                  Whitespace@9..10 " "
                  LiteralExpr@10..14
                    True@10..14 "true"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_bitwise_operator_precedence() {
        check(
            expr,
            "1 | 2 ^ 3 & 4",
            expect![[r#"
                BinaryExpr@0..13
                  LiteralExpr@0..1
                    Integer@0..1 "1"
                  Whitespace@1..2 " "
                  BitwiseOr@2..3 "|"
                  Whitespace@3..4 " "
                  BinaryExpr@4..13
                    LiteralExpr@4..5
                      Integer@4..5 "2"
                    Whitespace@5..6 " "
                    BitwiseXor@6..7 "^"
                    Whitespace@7..8 " "
                    BinaryExpr@8..13
                      LiteralExpr@8..9
                        Integer@8..9 "3"
                      Whitespace@9..10 " "
                      BitwiseAnd@10..11 "&"
                      Whitespace@11..12 " "
                      LiteralExpr@12..13
                        Integer@12..13 "4"
            "#]],
            expect![""],
        );

        check(
            expr,
            "1 & 2 + 3",
            expect![[r#"
                BinaryExpr@0..9
                  LiteralExpr@0..1
                    Integer@0..1 "1"
                  Whitespace@1..2 " "
                  BitwiseAnd@2..3 "&"
                  Whitespace@3..4 " "
                  BinaryExpr@4..9
                    LiteralExpr@4..5
                      Integer@4..5 "2"
                    Whitespace@5..6 " "
                    Plus@6..7 "+"
                    Whitespace@7..8 " "
                    LiteralExpr@8..9
                      Integer@8..9 "3"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_inline_if_expr() {
        check(
            expr,
            "inline if true { 1 } else { 2 }",
            expect![[r#"
                IfExpr@0..31
                  Inline@0..6 "inline"
                  Whitespace@6..7 " "
                  If@7..9 "if"
                  Whitespace@9..10 " "
                  LiteralExpr@10..14
                    True@10..14 "true"
                  Whitespace@14..15 " "
                  Block@15..20
                    OpenBrace@15..16 "{"
                    Whitespace@16..17 " "
                    LiteralExpr@17..18
                      Integer@17..18 "1"
                    Whitespace@18..19 " "
                    CloseBrace@19..20 "}"
                  Whitespace@20..21 " "
                  Else@21..25 "else"
                  Whitespace@25..26 " "
                  Block@26..31
                    OpenBrace@26..27 "{"
                    Whitespace@27..28 " "
                    LiteralExpr@28..29
                      Integer@28..29 "2"
                    Whitespace@29..30 " "
                    CloseBrace@30..31 "}"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_struct_initializer_not_allowed_in_condition() {
        check(
            expr,
            "if value { 1 } else { 2 }",
            expect![[r#"
                IfExpr@0..25
                  If@0..2 "if"
                  Whitespace@2..3 " "
                  PathExpr@3..9
                    PathSegment@3..9
                      Ident@3..8 "value"
                      Whitespace@8..9 " "
                  Block@9..14
                    OpenBrace@9..10 "{"
                    Whitespace@10..11 " "
                    LiteralExpr@11..12
                      Integer@11..12 "1"
                    Whitespace@12..13 " "
                    CloseBrace@13..14 "}"
                  Whitespace@14..15 " "
                  Else@15..19 "else"
                  Whitespace@19..20 " "
                  Block@20..25
                    OpenBrace@20..21 "{"
                    Whitespace@21..22 " "
                    LiteralExpr@22..23
                      Integer@22..23 "2"
                    Whitespace@23..24 " "
                    CloseBrace@24..25 "}"
            "#]],
            expect![""],
        );
    }
}
