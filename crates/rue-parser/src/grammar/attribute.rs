use crate::{Parser, SyntaxKind, T, grammar::delimited_list};

/// Parses the attributes (such as `#[allow(unused_binding)]`) before an item or statement.
pub fn attributes(p: &mut Parser) {
    while p.at_quiet(T![#]) {
        attribute(p);
    }
}

fn attribute(p: &mut Parser) {
    p.start(SyntaxKind::Attribute);
    p.expect(T![#]);
    if p.expect(T!['[']) {
        p.expect(SyntaxKind::Ident);
        if p.try_eat(T!['(']) {
            delimited_list(p, T![')'], |p| {
                p.expect(SyntaxKind::Ident);
            });
        }
        p.expect(T![']']);
    }
    p.finish();
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::grammar::{block::block, item::item, tests::check};

    #[test]
    fn test_item_attributes() {
        check(
            item,
            "#[allow(a, b)] #[deny(c)] const X: Int = 1;",
            expect![[r##"
                ConstantItem@0..43
                  Attribute@0..14
                    Pound@0..1 "#"
                    OpenBracket@1..2 "["
                    Ident@2..7 "allow"
                    OpenParen@7..8 "("
                    Ident@8..9 "a"
                    Comma@9..10 ","
                    Whitespace@10..11 " "
                    Ident@11..12 "b"
                    CloseParen@12..13 ")"
                    CloseBracket@13..14 "]"
                  Whitespace@14..15 " "
                  Attribute@15..25
                    Pound@15..16 "#"
                    OpenBracket@16..17 "["
                    Ident@17..21 "deny"
                    OpenParen@21..22 "("
                    Ident@22..23 "c"
                    CloseParen@23..24 ")"
                    CloseBracket@24..25 "]"
                  Whitespace@25..26 " "
                  Const@26..31 "const"
                  Whitespace@31..32 " "
                  Ident@32..33 "X"
                  Colon@33..34 ":"
                  Whitespace@34..35 " "
                  PathType@35..39
                    PathSegment@35..39
                      Ident@35..38 "Int"
                      Whitespace@38..39 " "
                  Assign@39..40 "="
                  Whitespace@40..41 " "
                  LiteralExpr@41..42
                    Integer@41..42 "1"
                  Semicolon@42..43 ";"
            "##]],
            expect![""],
        );
    }

    #[test]
    fn test_statement_attributes() {
        check(
            block,
            "{ #[allow(a)] let x = 1; #[deny(b)] x + 1; }",
            expect![[r##"
                Block@0..44
                  OpenBrace@0..1 "{"
                  Whitespace@1..2 " "
                  LetStmt@2..24
                    Attribute@2..13
                      Pound@2..3 "#"
                      OpenBracket@3..4 "["
                      Ident@4..9 "allow"
                      OpenParen@9..10 "("
                      Ident@10..11 "a"
                      CloseParen@11..12 ")"
                      CloseBracket@12..13 "]"
                    Whitespace@13..14 " "
                    Let@14..17 "let"
                    Whitespace@17..18 " "
                    NamedBinding@18..19
                      Ident@18..19 "x"
                    Whitespace@19..20 " "
                    Assign@20..21 "="
                    Whitespace@21..22 " "
                    LiteralExpr@22..23
                      Integer@22..23 "1"
                    Semicolon@23..24 ";"
                  Whitespace@24..25 " "
                  ExprStmt@25..42
                    Attribute@25..35
                      Pound@25..26 "#"
                      OpenBracket@26..27 "["
                      Ident@27..31 "deny"
                      OpenParen@31..32 "("
                      Ident@32..33 "b"
                      CloseParen@33..34 ")"
                      CloseBracket@34..35 "]"
                    Whitespace@35..36 " "
                    BinaryExpr@36..41
                      PathExpr@36..38
                        PathSegment@36..38
                          Ident@36..37 "x"
                          Whitespace@37..38 " "
                      Plus@38..39 "+"
                      Whitespace@39..40 " "
                      LiteralExpr@40..41
                        Integer@40..41 "1"
                    Semicolon@41..42 ";"
                  Whitespace@42..43 " "
                  CloseBrace@43..44 "}"
            "##]],
            expect![""],
        );
    }

    #[test]
    fn test_malformed_attribute() {
        check(
            item,
            "#[allow(a] fn main() {}",
            expect![[r##"
                FunctionItem@0..23
                  Attribute@0..10
                    Pound@0..1 "#"
                    OpenBracket@1..2 "["
                    Ident@2..7 "allow"
                    OpenParen@7..8 "("
                    Ident@8..9 "a"
                    CloseBracket@9..10 "]"
                  Whitespace@10..11 " "
                  Fn@11..13 "fn"
                  Whitespace@13..14 " "
                  Ident@14..18 "main"
                  OpenParen@18..19 "("
                  CloseParen@19..20 ")"
                  Whitespace@20..21 " "
                  Block@21..23
                    OpenBrace@21..22 "{"
                    CloseBrace@22..23 "}"
            "##]],
            expect!["Expected one of `,`, `)`, found `]` at main.rue:1:10"],
        );
    }
}
