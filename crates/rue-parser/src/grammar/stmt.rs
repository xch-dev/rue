use crate::{
    Parser, SyntaxKind, T,
    grammar::{
        expr::{ExprOptions, expr, expr_with},
        ty::ty,
    },
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
    } else if p.at(T![return]) {
        return_stmt(p);
    } else if p.at(T![assert]) {
        assert_stmt(p);
    } else if p.at(T![raise]) {
        raise_stmt(p);
    } else {
        let cp = p.checkpoint();
        if expr_with(
            p,
            ExprOptions {
                minimum_binding_power: 0,
                allow_statement: true,
                allow_struct_initializer: true,
            },
        ) {
            return StatementKind::Normal;
        }
        if p.at(T![;]) {
            p.start_at(cp, SyntaxKind::ExprStmt);
            p.expect(T![;]);
            p.finish();
        } else {
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
    if p.try_eat(T![=]) {
        expr(p);
    }
    p.expect(T![;]);
    p.finish();
}

fn return_stmt(p: &mut Parser<'_>) {
    p.start(SyntaxKind::ReturnStmt);
    p.expect(T![return]);
    expr(p);
    p.expect(T![;]);
    p.finish();
}

fn assert_stmt(p: &mut Parser<'_>) {
    p.start(SyntaxKind::AssertStmt);
    p.expect(T![assert]);
    expr(p);
    p.expect(T![;]);
    p.finish();
}

fn raise_stmt(p: &mut Parser<'_>) {
    p.start(SyntaxKind::RaiseStmt);
    p.expect(T![raise]);
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
                  Whitespace@10..11 " "
                  PathType@11..15
                    PathTypeSegment@11..15
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
    fn test_expr_stmt_without_semicolon() {
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

    #[test]
    fn test_expr_stmt_with_semicolon() {
        check_stmt(
            StatementKind::Normal,
            "(42 + 3);",
            expect![[r#"
                ExprStmt@0..9
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
                  Semicolon@8..9 ";"
            "#]],
            expect![],
        );
    }

    #[test]
    fn test_if_stmt() {
        check_stmt(
            StatementKind::Normal,
            "if true { 42 }",
            expect![[r#"
                IfStmt@0..14
                  If@0..2 "if"
                  Whitespace@2..3 " "
                  LiteralExpr@3..8
                    True@3..7 "true"
                    Whitespace@7..8 " "
                  Block@8..14
                    OpenBrace@8..9 "{"
                    Whitespace@9..10 " "
                    LiteralExpr@10..13
                      Integer@10..12 "42"
                      Whitespace@12..13 " "
                    CloseBrace@13..14 "}"
            "#]],
            expect![],
        );
    }

    #[test]
    fn test_return_stmt() {
        check_stmt(
            StatementKind::Normal,
            "return 42;",
            expect![[r#"
                ReturnStmt@0..10
                  Return@0..6 "return"
                  Whitespace@6..7 " "
                  LiteralExpr@7..9
                    Integer@7..9 "42"
                  Semicolon@9..10 ";"
            "#]],
            expect![],
        );
    }

    #[test]
    fn test_assert_stmt() {
        check_stmt(
            StatementKind::Normal,
            "return 42;",
            expect![[r#"
                ReturnStmt@0..10
                  Return@0..6 "return"
                  Whitespace@6..7 " "
                  LiteralExpr@7..9
                    Integer@7..9 "42"
                  Semicolon@9..10 ";"
            "#]],
            expect![],
        );
    }

    #[test]
    fn test_raise_stmt() {
        check_stmt(
            StatementKind::Normal,
            "return 42;",
            expect![[r#"
                ReturnStmt@0..10
                  Return@0..6 "return"
                  Whitespace@6..7 " "
                  LiteralExpr@7..9
                    Integer@7..9 "42"
                  Semicolon@9..10 ";"
            "#]],
            expect![],
        );
    }
}
