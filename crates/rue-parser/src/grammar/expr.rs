use rowan::Checkpoint;

use crate::{
    Parser, SyntaxKind, T,
    grammar::{
        binding::binding,
        block::block,
        delimited_list,
        generics::{generic_arguments, generic_parameters},
        recover, skip_unexpected,
        ty::ty,
    },
};

#[derive(Debug, Clone, Copy)]
pub struct ExprOptions {
    pub minimum_binding_power: u8,
    pub allow_statement: bool,
    pub allow_struct_initializer: bool,
    pub inline: bool,
}

impl Default for ExprOptions {
    fn default() -> Self {
        Self {
            minimum_binding_power: 0,
            allow_statement: false,
            allow_struct_initializer: true,
            inline: false,
        }
    }
}

pub fn expr(p: &mut Parser) {
    let checkpoint = p.checkpoint();
    expr_with(p, checkpoint, ExprOptions::default());
}

pub fn expr_with(p: &mut Parser, checkpoint: Checkpoint, options: ExprOptions) -> bool {
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
            },
        );
        p.finish();
    } else if !options.inline && p.at_quiet(T![::])
        || p.at_quiet(SyntaxKind::Ident)
        || p.at_quiet(T![super])
    {
        path_expr(p, checkpoint);

        if p.at(T!['{']) && options.allow_struct_initializer {
            p.start_at(checkpoint, SyntaxKind::StructInitializerExpr);
            p.expect(T!['{']);
            delimited_list(p, T!['}'], |p| {
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
    } else if p.at_quiet(T![if]) || (!options.inline && p.at_quiet(T![inline])) {
        if if_expr(p, checkpoint, options.allow_statement, !options.inline) {
            return true;
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

    loop {
        if p.at(T!['(']) {
            p.start_at(checkpoint, SyntaxKind::FunctionCallExpr);
            p.expect(T!['(']);
            delimited_list(p, T![')'], list_item);
            p.finish();
        } else if p.at(T![is]) {
            p.start_at(checkpoint, SyntaxKind::GuardExpr);
            p.expect(T![is]);
            ty(p);
            p.finish();
        } else if p.at(T![as]) {
            p.start_at(checkpoint, SyntaxKind::CastExpr);
            p.expect(T![as]);
            ty(p);
            p.finish();
        } else if p.at(T![.]) {
            p.start_at(checkpoint, SyntaxKind::FieldAccessExpr);
            p.expect(T![.]);
            let found = p.expect(SyntaxKind::Ident);
            p.finish();

            // Without a field name, the rest is unlikely to be a continuation of this expression
            if !found {
                break;
            }
        } else {
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
            },
        );
        p.finish();
    }
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
    if allow_inline {
        p.try_eat(T![inline]);
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
        },
    );
    block(p);
    if allow_statement && !p.at(T![else]) {
        p.start_at(checkpoint, SyntaxKind::IfStmt);
        p.finish();
        true
    } else {
        p.start_at(checkpoint, SyntaxKind::IfExpr);
        p.expect(T![else]);
        if p.at(T![if]) {
            let child = p.checkpoint();
            if_expr(p, child, false, false);
        } else {
            block(p);
        }
        p.finish();
        false
    }
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
    if !p.try_eat(T![super]) {
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
