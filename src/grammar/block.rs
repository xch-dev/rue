use crate::{
    Parser, SyntaxKind, T,
    grammar::stmt::{StatementKind, stmt},
};

pub fn block(p: &mut Parser<'_>) {
    p.start(SyntaxKind::Block);
    p.expect(T!['{']);
    while !p.at(T!['}']) {
        if stmt(p) == StatementKind::End {
            break;
        }
    }
    p.expect(T!['}']);
    p.finish();
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::grammar::tests::check;

    use super::*;

    #[test]
    fn test_block() {
        check(
            block,
            "{let x = 5;}",
            expect![[r#"
                Block@0..9
                  OpenBrace@0..1 "{"
                  LetStmt@1..8
                    Let@1..4 "let"
                    Ident@4..5 "x"
                    Assign@5..6 "="
                    LiteralExpr@6..7
                      Integer@6..7 "5"
                    Semicolon@7..8 ";"
                  CloseBrace@8..9 "}"
            "#]],
            expect![],
        );

        check(
            block,
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

        check(
            block,
            "{let x = 5; 42}",
            expect![[r#"
                Block@0..11
                  OpenBrace@0..1 "{"
                  LetStmt@1..8
                    Let@1..4 "let"
                    Ident@4..5 "x"
                    Assign@5..6 "="
                    LiteralExpr@6..7
                      Integer@6..7 "5"
                    Semicolon@7..8 ";"
                  LiteralExpr@8..10
                    Integer@8..10 "42"
                  CloseBrace@10..11 "}"
            "#]],
            expect![],
        );
    }
}
