use crate::{
    Parser, SyntaxKind, T,
    grammar::{block::block, generics::generic_arguments},
};

pub fn expr(p: &mut Parser<'_>) {
    expr_binding_power(p, 0);
}

fn expr_binding_power(p: &mut Parser<'_>, minimum_binding_power: u8) {
    let checkpoint = p.checkpoint();

    if let Some(kind) = p.at_any(SyntaxKind::PREFIX_OPS) {
        p.start_at(checkpoint, SyntaxKind::PrefixExpr);
        p.expect(kind);
        expr_binding_power(p, 255);
        p.finish();
    } else if p.at(T![::]) || p.at(SyntaxKind::Ident) {
        p.start_at(checkpoint, SyntaxKind::PathExpr);
        let mut separated = path_expr_segment(p, true);
        while separated || p.at(T![::]) {
            separated = path_expr_segment(p, false);
        }
        p.finish();
    } else if let Some(kind) = p.at_any(SyntaxKind::LITERAL_EXPR) {
        p.start(SyntaxKind::LiteralExpr);
        p.expect(kind);
        p.finish();
    } else if p.at(T!['(']) {
        p.start(SyntaxKind::GroupExpr);
        p.expect(T!['(']);
        expr(p);
        p.expect(T![')']);
        p.finish();
    } else if p.at(T!['{']) {
        block(p);
    } else {
        p.skip();
        return;
    }

    loop {
        if p.at(T!['(']) {
            p.start_at(checkpoint, SyntaxKind::FunctionCallExpr);
            p.expect(T!['(']);
            while !p.at(T![')']) {
                expr(p);
                if !p.try_eat(T![,]) {
                    break;
                }
            }
            p.expect(T![')']);
            p.finish();
        } else {
            break;
        }
    }

    loop {
        let Some(op) = p.at_any(SyntaxKind::BINARY_OPS) else {
            return;
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

        if left_binding_power < minimum_binding_power {
            return;
        }

        p.expect(op);

        p.start_at(checkpoint, SyntaxKind::BinaryExpr);
        expr_binding_power(p, right_binding_power);
        p.finish();
    }
}

fn path_expr_segment(p: &mut Parser<'_>, first: bool) -> bool {
    p.start(SyntaxKind::PathExprSegment);
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
}
