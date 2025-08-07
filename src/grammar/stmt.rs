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
                LetStmt@0..7
                  Let@0..3 "let"
                  Ident@3..4 "x"
                  Assign@4..5 "="
                  LiteralExpr@5..6
                    Integer@5..6 "5"
                  Semicolon@6..7 ";"
            "#]],
            expect![],
        );

        check_stmt(
            StatementKind::Normal,
            "let thing: Int = 42 + 3;",
            expect![[r#"
                LetStmt@0..18
                  Let@0..3 "let"
                  Ident@3..8 "thing"
                  Colon@8..9 ":"
                  LiteralType@9..12
                    Ident@9..12 "Int"
                  Assign@12..13 "="
                  BinaryExpr@13..17
                    LiteralExpr@13..15
                      Integer@13..15 "42"
                    Plus@15..16 "+"
                    LiteralExpr@16..17
                      Integer@16..17 "3"
                  Semicolon@17..18 ";"
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
                GroupExpr@0..6
                  OpenParen@0..1 "("
                  BinaryExpr@1..5
                    LiteralExpr@1..3
                      Integer@1..3 "42"
                    Plus@3..4 "+"
                    LiteralExpr@4..5
                      Integer@4..5 "3"
                  CloseParen@5..6 ")"
            "#]],
            expect![],
        );
    }
}
