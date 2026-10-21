use crate::{
    Parser, SyntaxKind, T,
    grammar::{
        STATEMENT_KEYWORDS, at_item_start, skip_balanced,
        stmt::{StatementKind, stmt},
    },
};

pub fn block(p: &mut Parser) {
    p.start(SyntaxKind::Block);

    // If the block isn't opened, don't try to parse its contents since they're likely not there
    if !p.expect(T!['{']) {
        p.finish();
        return;
    }

    while !p.at(T!['}']) && !p.at_quiet(SyntaxKind::Eof) {
        // An item can't appear inside of a block, so it's likely that the closing brace is missing
        if at_item_start(p) {
            break;
        }

        let start = p.pos();

        if stmt(p) == StatementKind::Normal {
            continue;
        }

        if p.at(T!['}']) {
            break;
        }

        // The statement ended with an expression that isn't followed by a semicolon or the
        // closing brace, so report the error and try to resume parsing at the next statement
        p.error_unexpected();

        // If the next token is on a new line, it's most likely that only the semicolon is
        // missing. Otherwise, skip ahead to the end of the statement.
        if p.pos() != start && p.at_line_start() {
            continue;
        }

        skip_balanced(p, |p| {
            p.at_any_quiet(&[T![;], T!['}']]).is_some()
                || p.at_any_quiet(STATEMENT_KEYWORDS).is_some()
        });

        if p.at_quiet(T![;]) {
            p.bump_as_error();
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

    #[test]
    fn test_empty_block() {
        check(
            block,
            "{}",
            expect![[r#"
                Block@0..2
                  OpenBrace@0..1 "{"
                  CloseBrace@1..2 "}"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_block_with_statements() {
        check(
            block,
            "{\n    let x = 1;\n    assert x == 1;\n    debug x;\n    x\n}",
            expect![[r#"
                Block@0..56
                  OpenBrace@0..1 "{"
                  Whitespace@1..6 "\n    "
                  LetStmt@6..16
                    Let@6..9 "let"
                    Whitespace@9..10 " "
                    NamedBinding@10..11
                      Ident@10..11 "x"
                    Whitespace@11..12 " "
                    Assign@12..13 "="
                    Whitespace@13..14 " "
                    LiteralExpr@14..15
                      Integer@14..15 "1"
                    Semicolon@15..16 ";"
                  Whitespace@16..21 "\n    "
                  AssertStmt@21..35
                    Assert@21..27 "assert"
                    Whitespace@27..28 " "
                    BinaryExpr@28..34
                      PathExpr@28..30
                        PathSegment@28..30
                          Ident@28..29 "x"
                          Whitespace@29..30 " "
                      Equals@30..32 "=="
                      Whitespace@32..33 " "
                      LiteralExpr@33..34
                        Integer@33..34 "1"
                    Semicolon@34..35 ";"
                  Whitespace@35..40 "\n    "
                  DebugStmt@40..48
                    Debug@40..45 "debug"
                    Whitespace@45..46 " "
                    PathExpr@46..47
                      PathSegment@46..47
                        Ident@46..47 "x"
                    Semicolon@47..48 ";"
                  Whitespace@48..53 "\n    "
                  PathExpr@53..55
                    PathSegment@53..55
                      Ident@53..54 "x"
                      Whitespace@54..55 "\n"
                  CloseBrace@55..56 "}"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_block_with_comments() {
        check(
            block,
            "{\n    // comment\n    /* block */ 42\n}",
            expect![[r#"
                Block@0..37
                  OpenBrace@0..1 "{"
                  Whitespace@1..6 "\n    "
                  LineComment@6..17 "// comment\n"
                  Whitespace@17..21 "    "
                  BlockComment@21..32 "/* block */"
                  Whitespace@32..33 " "
                  LiteralExpr@33..35
                    Integer@33..35 "42"
                  Whitespace@35..36 "\n"
                  CloseBrace@36..37 "}"
            "#]],
            expect![""],
        );
    }
}
