use crate::{
    Parser, SyntaxKind, T,
    grammar::{expr::expr, ty::ty},
};

#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StatementKind {
    Normal,
    End,
}

pub fn stmt(p: &mut Parser<'_>) -> StatementKind {
    if p.at(T![let]) {
        let_stmt(p);
    } else {
        expr(p);
        if !p.try_eat(T![;]) {
            return StatementKind::End;
        }
    }
    StatementKind::Normal
}

fn let_stmt(p: &mut Parser<'_>) {
    p.start(SyntaxKind::LetStmt);
    p.expect(T![let]);
    p.expect(SyntaxKind::Ident);
    if p.try_eat(T![:]) {
        ty(p);
    }
    p.expect(T![=]);
    expr(p);
    p.expect(T![;]);
    p.finish();
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};

    use crate::grammar::tests::check;

    use super::*;

    fn check_stmt(kind: StatementKind, source: &str, expect: Expect, errors: Expect) {
        check(
            |p| {
                assert_eq!(stmt(p), kind);
            },
            source,
            expect,
            errors,
        )
    }

    #[test]
    fn test_let_stmt() {
        check_stmt(
            StatementKind::Normal,
            "let x = 5;",
            expect![[r#"
                LetStmt@0..10
                  Let@0..3 "let"
                  Whitespace@3..4 " "
                  Ident@4..5 "x"
                  Whitespace@5..6 " "
                  Assign@6..7 "="
                  Whitespace@7..8 " "
                  LiteralExpr@8..9
                    Integer@8..9 "5"
                  Semicolon@9..10 ";"
            "#]],
            expect![],
        );

        check_stmt(
            StatementKind::Normal,
            "let thing: Int = 42 + 3;",
            expect![[r#"
                LetStmt@0..24
                  Let@0..3 "let"
                  Whitespace@3..4 " "
                  Ident@4..9 "thing"
                  Colon@9..10 ":"
                  LiteralType@10..15
                    Whitespace@10..11 " "
                    Ident@11..14 "Int"
                    Whitespace@14..15 " "
                  Assign@15..16 "="
                  BinaryExpr@16..23
                    Whitespace@16..17 " "
                    LiteralExpr@17..20
                      Integer@17..19 "42"
                      Whitespace@19..20 " "
                    Plus@20..21 "+"
                    Whitespace@21..22 " "
                    LiteralExpr@22..23
                      Integer@22..23 "3"
                  Semicolon@23..24 ";"
            "#]],
            expect![],
        );
    }

    #[test]
    fn test_expr_stmt() {
        check_stmt(
            StatementKind::End,
            "(42 + 3)",
            expect![[r#"
                GroupExpr@0..8
                  OpenParen@0..1 "("
                  BinaryExpr@1..7
                    LiteralExpr@1..4
                      Integer@1..3 "42"
                      Whitespace@3..4 " "
                    Plus@4..5 "+"
                    Whitespace@5..6 " "
                    LiteralExpr@6..7
                      Integer@6..7 "3"
                  CloseParen@7..8 ")"
            "#]],
            expect![],
        );
    }
}
