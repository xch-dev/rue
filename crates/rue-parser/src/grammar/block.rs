use crate::{
    Parser, SyntaxKind, T,
    grammar::stmt::{StatementKind, stmt},
};

pub fn block(p: &mut Parser) {
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
                Block@0..12
                  OpenBrace@0..1 "{"
                  LetStmt@1..11
                    Let@1..4 "let"
                    Whitespace@4..5 " "
                    Binding@5..6
                      NamedBinding@5..6
                        Ident@5..6 "x"
                    Whitespace@6..7 " "
                    Assign@7..8 "="
                    Whitespace@8..9 " "
                    LiteralExpr@9..10
                      Integer@9..10 "5"
                    Semicolon@10..11 ";"
                  CloseBrace@11..12 "}"
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
                Block@0..15
                  OpenBrace@0..1 "{"
                  LetStmt@1..11
                    Let@1..4 "let"
                    Whitespace@4..5 " "
                    Binding@5..6
                      NamedBinding@5..6
                        Ident@5..6 "x"
                    Whitespace@6..7 " "
                    Assign@7..8 "="
                    Whitespace@8..9 " "
                    LiteralExpr@9..10
                      Integer@9..10 "5"
                    Semicolon@10..11 ";"
                  Whitespace@11..12 " "
                  LiteralExpr@12..14
                    Integer@12..14 "42"
                  CloseBrace@14..15 "}"
            "#]],
            expect![],
        );
    }
}
