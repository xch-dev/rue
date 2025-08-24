use rowan::Checkpoint;

use crate::{
    Parser, SyntaxKind, T,
    grammar::{block::block, generics::generic_arguments, ty::ty},
};

#[derive(Debug, Clone, Copy)]
pub struct ExprOptions {
    pub minimum_binding_power: u8,
    pub allow_statement: bool,
    pub allow_struct_initializer: bool,
}

impl Default for ExprOptions {
    fn default() -> Self {
        Self {
            minimum_binding_power: 0,
            allow_statement: false,
            allow_struct_initializer: true,
        }
    }
}

pub fn expr(p: &mut Parser<'_>) {
    expr_with(p, ExprOptions::default());
}

pub fn expr_with(p: &mut Parser<'_>, options: ExprOptions) -> bool {
    let checkpoint = p.checkpoint();

    if let Some(kind) = p.at_any(SyntaxKind::PREFIX_OPS) {
        p.start_at(checkpoint, SyntaxKind::PrefixExpr);
        p.expect(kind);
        expr_with(
            p,
            ExprOptions {
                minimum_binding_power: 255,
                allow_statement: false,
                allow_struct_initializer: options.allow_struct_initializer,
            },
        );
        p.finish();
    } else if p.at(T![::]) || p.at(SyntaxKind::Ident) {
        p.start_at(checkpoint, SyntaxKind::PathExpr);
        let mut separated = path_expr_segment(p, true);
        while separated || p.at(T![::]) {
            separated = path_expr_segment(p, false);
        }
        p.finish();

        if p.at(T!['{']) && options.allow_struct_initializer {
            p.start_at(checkpoint, SyntaxKind::StructInitializerExpr);
            p.expect(T!['{']);
            while !p.at(T!['}']) {
                p.start(SyntaxKind::StructInitializerField);
                p.expect(SyntaxKind::Ident);
                p.expect(T![:]);
                expr(p);
                p.finish();
                if !p.try_eat(T![,]) {
                    break;
                }
            }
            p.expect(T!['}']);
            p.finish();
        }
    } else if let Some(kind) = p.at_any(SyntaxKind::LITERAL) {
        p.start(SyntaxKind::LiteralExpr);
        p.expect(kind);
        p.finish();
    } else if p.at(T!['(']) {
        p.expect(T!['(']);
        expr(p);
        if p.try_eat(T![,]) {
            p.start_at(checkpoint, SyntaxKind::PairExpr);
            expr(p);
            p.try_eat(T![,]);
        } else {
            p.start_at(checkpoint, SyntaxKind::GroupExpr);
        }
        p.expect(T![')']);
        p.finish();
    } else if p.at(T!['[']) {
        p.start_at(checkpoint, SyntaxKind::ListExpr);
        p.expect(T!['[']);
        while !p.at(T![']']) {
            p.start(SyntaxKind::ListItem);
            p.try_eat(T![...]);
            expr(p);
            p.finish();
            if !p.try_eat(T![,]) {
                break;
            }
        }
        p.expect(T![']']);
        p.finish();
    } else if p.at(T!['{']) {
        block(p);
    } else if p.at(T![if]) {
        if if_expr(p, checkpoint, options.allow_statement) {
            return true;
        }
    } else if p.at(T![fn]) {
        p.start_at(checkpoint, SyntaxKind::LambdaExpr);
        p.expect(T![fn]);
        p.expect(T!['(']);
        while !p.at(T![')']) {
            p.start(SyntaxKind::FunctionParameter);
            p.try_eat(T![...]);
            p.expect(SyntaxKind::Ident);
            if p.try_eat(T![:]) {
                ty(p);
            }
            p.finish();
            if !p.try_eat(T![,]) {
                break;
            }
        }
        p.expect(T![')']);
        if p.try_eat(T![:]) {
            ty(p);
        }
        p.expect(T![=>]);
        expr(p);
        p.finish();
    } else {
        p.skip();
        return false;
    }

    loop {
        if p.at(T!['(']) {
            p.start_at(checkpoint, SyntaxKind::FunctionCallExpr);
            p.expect(T!['(']);
            while !p.at(T![')']) {
                p.start(SyntaxKind::ListItem);
                p.try_eat(T![...]);
                expr(p);
                p.finish();
                if !p.try_eat(T![,]) {
                    break;
                }
            }
            p.expect(T![')']);
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
            p.expect(SyntaxKind::Ident);
            p.finish();
        } else {
            break;
        }
    }

    loop {
        let Some(op) = p.at_any(SyntaxKind::BINARY_OPS) else {
            return false;
        };

        let (left_binding_power, right_binding_power) = match op {
            T![||] => (1, 2),
            T![&&] => (3, 4),
            T![==] | T![!=] => (5, 6),
            T![<] | T![>] | T![<=] | T![>=] => (7, 8),
            T![|] => (9, 10),
            T![^] => (11, 12),
            T![&] => (13, 14),
            T![<<] | T![>>] => (15, 16),
            T![+] | T![-] => (17, 18),
            T![*] | T![/] | T![%] => (19, 20),
            _ => unreachable!(),
        };

        if left_binding_power < options.minimum_binding_power {
            return false;
        }

        p.expect(op);

        p.start_at(checkpoint, SyntaxKind::BinaryExpr);
        expr_with(
            p,
            ExprOptions {
                minimum_binding_power: right_binding_power,
                allow_statement: false,
                allow_struct_initializer: options.allow_struct_initializer,
            },
        );
        p.finish();
    }
}

fn path_expr_segment(p: &mut Parser<'_>, first: bool) -> bool {
    p.start(SyntaxKind::PathSegment);
    if first {
        p.try_eat(T![::]);
    } else {
        p.expect(T![::]);
    }
    p.expect(SyntaxKind::Ident);
    let mut separated = p.try_eat(T![::]);
    if separated && p.at(T![<]) {
        generic_arguments(p);
        separated = false;
    }
    p.finish();
    separated
}

fn if_expr(p: &mut Parser<'_>, checkpoint: Checkpoint, allow_statement: bool) -> bool {
    p.expect(T![if]);
    expr_with(
        p,
        ExprOptions {
            minimum_binding_power: 0,
            allow_statement: false,
            allow_struct_initializer: false,
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
            let checkpoint = p.checkpoint();
            if_expr(p, checkpoint, false);
        } else {
            block(p);
        }
        p.finish();
        false
    }
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
                  PathExprSegment@0..5
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
                  GroupExpr@0..9
                    OpenParen@0..1 "("
                    BinaryExpr@1..7
                      LiteralExpr@1..4
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
                    LiteralExpr@0..2
                      Integer@0..1 "1"
                      Whitespace@1..2 " "
                    Plus@2..3 "+"
                    BinaryExpr@3..10
                      Whitespace@3..4 " "
                      LiteralExpr@4..6
                        Integer@4..5 "2"
                        Whitespace@5..6 " "
                      Star@6..7 "*"
                      Whitespace@7..8 " "
                      LiteralExpr@8..10
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
                    LiteralExpr@0..2
                      Integer@0..1 "1"
                      Whitespace@1..2 " "
                    And@2..4 "&&"
                    Whitespace@4..5 " "
                    LiteralExpr@5..7
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
    fn test_function_call_expr() {
        check(
            expr,
            "hello(42)",
            expect![[r#"
                FunctionCallExpr@0..9
                  PathExpr@0..5
                    PathExprSegment@0..5
                      Ident@0..5 "hello"
                  OpenParen@5..6 "("
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
                    PathExprSegment@0..5
                      Ident@0..5 "hello"
                  OpenParen@5..6 "("
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
                    PathExprSegment@0..5
                      Ident@0..5 "hello"
                  OpenParen@5..6 "("
                  LiteralExpr@6..8
                    Integer@6..8 "42"
                  Comma@8..9 ","
                  Whitespace@9..10 " "
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
              LiteralExpr@3..8
                True@3..7 "true"
                Whitespace@7..8 " "
              Block@8..15
                OpenBrace@8..9 "{"
                Whitespace@9..10 " "
                LiteralExpr@10..13
                  Integer@10..12 "42"
                  Whitespace@12..13 " "
                CloseBrace@13..14 "}"
                Whitespace@14..15 " "
              Else@15..19 "else"
              Whitespace@19..20 " "
              Block@20..26
                OpenBrace@20..21 "{"
                Whitespace@21..22 " "
                LiteralExpr@22..25
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
                  LiteralExpr@3..8
                    True@3..7 "true"
                    Whitespace@7..8 " "
                  Block@8..15
                    OpenBrace@8..9 "{"
                    Whitespace@9..10 " "
                    LiteralExpr@10..13
                      Integer@10..12 "42"
                      Whitespace@12..13 " "
                    CloseBrace@13..14 "}"
                    Whitespace@14..15 " "
                  Else@15..19 "else"
                  Whitespace@19..20 " "
                  IfExpr@20..47
                    If@20..22 "if"
                    Whitespace@22..23 " "
                    LiteralExpr@23..29
                      False@23..28 "false"
                      Whitespace@28..29 " "
                    Block@29..36
                      OpenBrace@29..30 "{"
                      Whitespace@30..31 " "
                      LiteralExpr@31..34
                        Integer@31..33 "84"
                        Whitespace@33..34 " "
                      CloseBrace@34..35 "}"
                      Whitespace@35..36 " "
                    Else@36..40 "else"
                    Whitespace@40..41 " "
                    Block@41..47
                      OpenBrace@41..42 "{"
                      Whitespace@42..43 " "
                      LiteralExpr@43..46
                        Integer@43..45 "91"
                        Whitespace@45..46 " "
                      CloseBrace@46..47 "}"
            "#]],
            expect![""],
        );
    }
}
