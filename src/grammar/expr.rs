use crate::{BinaryOp, Parser, SyntaxKind, T, grammar::block::block};

pub fn expr(p: &mut Parser<'_>) {
    expr_binding_power(p, 0);
}

fn expr_binding_power(p: &mut Parser<'_>, minimum_binding_power: u8) {
    let checkpoint = p.checkpoint();

    if let Some(kind) = p.at_any(&[T![!], T![-], T![+], T![~]]) {
        p.start_at(checkpoint, SyntaxKind::PrefixExpr);
        p.expect(kind);
        expr_binding_power(p, 255);
        p.finish();
    } else if let Some(kind) = p.at_any(&[
        SyntaxKind::String,
        SyntaxKind::Hex,
        SyntaxKind::Integer,
        SyntaxKind::Ident,
        T![nil],
        T![true],
        T![false],
    ]) {
        p.start(SyntaxKind::LiteralExpr);
        p.expect(kind);
        p.finish();
    } else if p.at(T!['{']) {
        block(p);
    } else {
        p.skip();
        return;
    }

    loop {
        let Some(op_kind) = p.at_any(&[
            T![+],
            T![-],
            T![*],
            T![/],
            T![%],
            T![<],
            T![>],
            T![<=],
            T![>=],
            T![==],
            T![!=],
            T![&&],
            T![||],
            T![&],
            T![|],
            T![^],
            T![<<],
            T![>>],
        ]) else {
            return;
        };

        let op = match op_kind {
            T![+] => BinaryOp::Add,
            T![-] => BinaryOp::Subtract,
            T![*] => BinaryOp::Multiply,
            T![/] => BinaryOp::Divide,
            T![%] => BinaryOp::Remainder,
            T![<] => BinaryOp::LessThan,
            T![>] => BinaryOp::GreaterThan,
            T![<=] => BinaryOp::LessThanEquals,
            T![>=] => BinaryOp::GreaterThanEquals,
            T![==] => BinaryOp::Equals,
            T![!=] => BinaryOp::NotEquals,
            T![&&] => BinaryOp::And,
            T![||] => BinaryOp::Or,
            T![&] => BinaryOp::BitwiseAnd,
            T![|] => BinaryOp::BitwiseOr,
            T![^] => BinaryOp::BitwiseXor,
            T![<<] => BinaryOp::LeftShift,
            T![>>] => BinaryOp::RightShift,
            _ => unreachable!(),
        };

        let (left_binding_power, right_binding_power) = binding_power(op);

        if left_binding_power < minimum_binding_power {
            return;
        }

        p.expect(op_kind);

        p.start_at(checkpoint, SyntaxKind::BinaryExpr);
        expr_binding_power(p, right_binding_power);
        p.finish();
    }
}

fn binding_power(op: BinaryOp) -> (u8, u8) {
    match op {
        BinaryOp::Or => (1, 2),
        BinaryOp::And => (3, 4),
        BinaryOp::Equals | BinaryOp::NotEquals => (5, 6),
        BinaryOp::LessThan
        | BinaryOp::GreaterThan
        | BinaryOp::LessThanEquals
        | BinaryOp::GreaterThanEquals => (7, 8),
        BinaryOp::BitwiseOr => (9, 10),
        BinaryOp::BitwiseXor => (11, 12),
        BinaryOp::BitwiseAnd => (13, 14),
        BinaryOp::LeftShift | BinaryOp::RightShift => (15, 16),
        BinaryOp::Add | BinaryOp::Subtract => (17, 18),
        BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Remainder => (19, 20),
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
                LiteralExpr@0..5
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
}
