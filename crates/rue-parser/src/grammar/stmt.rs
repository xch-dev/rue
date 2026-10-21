use rowan::Checkpoint;

use crate::{
    Parser, SyntaxKind, T,
    grammar::{
        binding::binding,
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

pub fn stmt(p: &mut Parser) -> StatementKind {
    let cp = p.checkpoint();

    let inline = p.at_quiet(T![inline]) && p.expect(T![inline]);

    if p.at_quiet(T![let]) {
        let_stmt(p, cp);
    } else if p.at_quiet(T![return]) && !inline {
        return_stmt(p);
    } else if p.at_quiet(T![assert]) && !inline {
        assert_stmt(p);
    } else if p.at_quiet(T![raise]) && !inline {
        raise_stmt(p);
    } else if p.at_quiet(T![debug]) && !inline {
        debug_stmt(p);
    } else {
        if expr_with(
            p,
            cp,
            ExprOptions {
                minimum_binding_power: 0,
                allow_statement: true,
                allow_struct_initializer: true,
                inline,
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

fn let_stmt(p: &mut Parser, cp: Checkpoint) {
    p.start_at(cp, SyntaxKind::LetStmt);
    p.try_eat(T![inline]);
    p.expect(T![let]);
    binding(p);
    if p.try_eat(T![:]) {
        ty(p);
    }
    if p.try_eat(T![=]) {
        expr(p);
    }
    p.expect(T![;]);
    p.finish();
}

fn return_stmt(p: &mut Parser) {
    p.start(SyntaxKind::ReturnStmt);
    p.expect(T![return]);
    if !p.at(T![;]) {
        expr(p);
    }
    p.expect(T![;]);
    p.finish();
}

fn assert_stmt(p: &mut Parser) {
    p.start(SyntaxKind::AssertStmt);
    p.expect(T![assert]);
    expr(p);
    p.expect(T![;]);
    p.finish();
}

fn raise_stmt(p: &mut Parser) {
    p.start(SyntaxKind::RaiseStmt);
    p.expect(T![raise]);
    if !p.at(T![;]) {
        expr(p);
    }
    p.expect(T![;]);
    p.finish();
}

fn debug_stmt(p: &mut Parser) {
    p.start(SyntaxKind::DebugStmt);
    p.expect(T![debug]);
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
        );
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
                  NamedBinding@4..5
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
                  NamedBinding@4..9
                    Ident@4..9 "thing"
                  Colon@9..10 ":"
                  Whitespace@10..11 " "
                  PathType@11..15
                    PathSegment@11..15
                      Ident@11..14 "Int"
                      Whitespace@14..15 " "
                  Assign@15..16 "="
                  Whitespace@16..17 " "
                  BinaryExpr@17..23
                    LiteralExpr@17..19
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
                    LiteralExpr@1..3
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
                      LiteralExpr@1..3
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

    #[test]
    fn test_debug_stmt() {
        check_stmt(
            StatementKind::Normal,
            "debug 42;",
            expect![[r#"
                DebugStmt@0..9
                  Debug@0..5 "debug"
                  Whitespace@5..6 " "
                  LiteralExpr@6..8
                    Integer@6..8 "42"
                  Semicolon@8..9 ";"
            "#]],
            expect![],
        );
    }

    #[test]
    fn test_let_stmt_without_value() {
        check(
            |p| { let _ = stmt(p); },
            "let x: Int;",
            expect![[r#"
                LetStmt@0..11
                  Let@0..3 "let"
                  Whitespace@3..4 " "
                  NamedBinding@4..5
                    Ident@4..5 "x"
                  Colon@5..6 ":"
                  Whitespace@6..7 " "
                  PathType@7..10
                    PathSegment@7..10
                      Ident@7..10 "Int"
                  Semicolon@10..11 ";"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_inline_let_stmt() {
        check(
            |p| { let _ = stmt(p); },
            "inline let x = 5;",
            expect![[r#"
                LetStmt@0..17
                  Inline@0..6 "inline"
                  Whitespace@6..7 " "
                  Let@7..10 "let"
                  Whitespace@10..11 " "
                  NamedBinding@11..12
                    Ident@11..12 "x"
                  Whitespace@12..13 " "
                  Assign@13..14 "="
                  Whitespace@14..15 " "
                  LiteralExpr@15..16
                    Integer@15..16 "5"
                  Semicolon@16..17 ";"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_let_stmt_destructuring() {
        check(
            |p| { let _ = stmt(p); },
            "let (a, b) = pair;",
            expect![[r#"
                LetStmt@0..18
                  Let@0..3 "let"
                  Whitespace@3..4 " "
                  PairBinding@4..10
                    OpenParen@4..5 "("
                    NamedBinding@5..6
                      Ident@5..6 "a"
                    Comma@6..7 ","
                    Whitespace@7..8 " "
                    NamedBinding@8..9
                      Ident@8..9 "b"
                    CloseParen@9..10 ")"
                  Whitespace@10..11 " "
                  Assign@11..12 "="
                  Whitespace@12..13 " "
                  PathExpr@13..17
                    PathSegment@13..17
                      Ident@13..17 "pair"
                  Semicolon@17..18 ";"
            "#]],
            expect![""],
        );

        check(
            |p| { let _ = stmt(p); },
            "let { x, y } = point;",
            expect![[r#"
                LetStmt@0..21
                  Let@0..3 "let"
                  Whitespace@3..4 " "
                  StructBinding@4..12
                    OpenBrace@4..5 "{"
                    Whitespace@5..6 " "
                    StructFieldBinding@6..7
                      Ident@6..7 "x"
                    Comma@7..8 ","
                    Whitespace@8..9 " "
                    StructFieldBinding@9..11
                      Ident@9..10 "y"
                      Whitespace@10..11 " "
                    CloseBrace@11..12 "}"
                  Whitespace@12..13 " "
                  Assign@13..14 "="
                  Whitespace@14..15 " "
                  PathExpr@15..20
                    PathSegment@15..20
                      Ident@15..20 "point"
                  Semicolon@20..21 ";"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_return_stmt_without_value() {
        check(
            |p| { let _ = stmt(p); },
            "return;",
            expect![[r#"
                ReturnStmt@0..7
                  Return@0..6 "return"
                  Semicolon@6..7 ";"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_raise_stmt_without_value() {
        check(
            |p| { let _ = stmt(p); },
            "raise;",
            expect![[r#"
                RaiseStmt@0..6
                  Raise@0..5 "raise"
                  Semicolon@5..6 ";"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_inline_if_stmt() {
        check(
            |p| { let _ = stmt(p); },
            "inline if true { 42 }",
            expect![[r#"
                IfStmt@0..21
                  Inline@0..6 "inline"
                  Whitespace@6..7 " "
                  If@7..9 "if"
                  Whitespace@9..10 " "
                  LiteralExpr@10..14
                    True@10..14 "true"
                  Whitespace@14..15 " "
                  Block@15..21
                    OpenBrace@15..16 "{"
                    Whitespace@16..17 " "
                    LiteralExpr@17..19
                      Integer@17..19 "42"
                    Whitespace@19..20 " "
                    CloseBrace@20..21 "}"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_if_else_expr_stmt() {
        check(
            |p| { let _ = stmt(p); },
            "if true { 1 } else { 2 }",
            expect![[r#"
                IfExpr@0..24
                  If@0..2 "if"
                  Whitespace@2..3 " "
                  LiteralExpr@3..7
                    True@3..7 "true"
                  Whitespace@7..8 " "
                  Block@8..13
                    OpenBrace@8..9 "{"
                    Whitespace@9..10 " "
                    LiteralExpr@10..11
                      Integer@10..11 "1"
                    Whitespace@11..12 " "
                    CloseBrace@12..13 "}"
                  Whitespace@13..14 " "
                  Else@14..18 "else"
                  Whitespace@18..19 " "
                  Block@19..24
                    OpenBrace@19..20 "{"
                    Whitespace@20..21 " "
                    LiteralExpr@21..22
                      Integer@21..22 "2"
                    Whitespace@22..23 " "
                    CloseBrace@23..24 "}"
            "#]],
            expect![""],
        );
    }
}
