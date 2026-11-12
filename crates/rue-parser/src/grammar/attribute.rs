use crate::{Parser, SyntaxKind, T, grammar::delimited_list};

/// Parses the attributes (such as `#[allow(unused_binding)]`) before an item or statement.
pub fn attributes(p: &mut Parser) {
    while p.at_quiet(T![#]) {
        attribute(p);
    }
}

/// Parses the inner attributes (such as `#![allow(unused_binding)]`) at the start of a file or
/// module, which apply to all of it rather than to the next item.
pub fn inner_attributes(p: &mut Parser) {
    while p.at_quiet(T![#]) && p.lookahead(1) == T![!] {
        attribute(p);
    }
}

/// Parses an outer or inner attribute. Inner attributes in other places are parsed the same way,
/// so that they can be reported as misplaced rather than as syntax errors.
fn attribute(p: &mut Parser) {
    p.start(SyntaxKind::Attribute);
    p.expect(T![#]);
    if p.at_quiet(T![!]) {
        p.try_eat(T![!]);
    }
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

    use crate::grammar::{block::block, document::document, item::item, tests::check};

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
    fn test_parameter_attributes() {
        check(
            item,
            "fn f(#[curried] a: Int) {}",
            expect![[r##"
                FunctionItem@0..26
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  Ident@3..4 "f"
                  OpenParen@4..5 "("
                  FunctionParameter@5..22
                    Attribute@5..15
                      Pound@5..6 "#"
                      OpenBracket@6..7 "["
                      Ident@7..14 "curried"
                      CloseBracket@14..15 "]"
                    Whitespace@15..16 " "
                    NamedBinding@16..17
                      Ident@16..17 "a"
                    Colon@17..18 ":"
                    Whitespace@18..19 " "
                    PathType@19..22
                      PathSegment@19..22
                        Ident@19..22 "Int"
                  CloseParen@22..23 ")"
                  Whitespace@23..24 " "
                  Block@24..26
                    OpenBrace@24..25 "{"
                    CloseBrace@25..26 "}"
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

    #[test]
    fn test_inner_attributes() {
        check(
            document,
            "// Header\n#![allow(a)] #![deny(b)]\nimport x::y;\n",
            expect![[r##"
                Document@0..48
                  LineComment@0..10 "// Header\n"
                  Attribute@10..22
                    Pound@10..11 "#"
                    Not@11..12 "!"
                    OpenBracket@12..13 "["
                    Ident@13..18 "allow"
                    OpenParen@18..19 "("
                    Ident@19..20 "a"
                    CloseParen@20..21 ")"
                    CloseBracket@21..22 "]"
                  Whitespace@22..23 " "
                  Attribute@23..34
                    Pound@23..24 "#"
                    Not@24..25 "!"
                    OpenBracket@25..26 "["
                    Ident@26..30 "deny"
                    OpenParen@30..31 "("
                    Ident@31..32 "b"
                    CloseParen@32..33 ")"
                    CloseBracket@33..34 "]"
                  Whitespace@34..35 "\n"
                  ImportItem@35..47
                    Import@35..41 "import"
                    Whitespace@41..42 " "
                    ImportPath@42..46
                      ImportPathSegment@42..43
                        Ident@42..43 "x"
                      ImportPathSegment@43..46
                        PathSeparator@43..45 "::"
                        Ident@45..46 "y"
                    Semicolon@46..47 ";"
                  Whitespace@47..48 "\n"
            "##]],
            expect![""],
        );
    }

    #[test]
    fn test_module_inner_attributes() {
        check(
            item,
            "mod m { #![allow(a)] fn f() {} }",
            expect![[r##"
                ModuleItem@0..32
                  Mod@0..3 "mod"
                  Whitespace@3..4 " "
                  Ident@4..5 "m"
                  Whitespace@5..6 " "
                  OpenBrace@6..7 "{"
                  Whitespace@7..8 " "
                  Attribute@8..20
                    Pound@8..9 "#"
                    Not@9..10 "!"
                    OpenBracket@10..11 "["
                    Ident@11..16 "allow"
                    OpenParen@16..17 "("
                    Ident@17..18 "a"
                    CloseParen@18..19 ")"
                    CloseBracket@19..20 "]"
                  Whitespace@20..21 " "
                  FunctionItem@21..30
                    Fn@21..23 "fn"
                    Whitespace@23..24 " "
                    Ident@24..25 "f"
                    OpenParen@25..26 "("
                    CloseParen@26..27 ")"
                    Whitespace@27..28 " "
                    Block@28..30
                      OpenBrace@28..29 "{"
                      CloseBrace@29..30 "}"
                  Whitespace@30..31 " "
                  CloseBrace@31..32 "}"
            "##]],
            expect![""],
        );
    }

    #[test]
    fn test_misplaced_inner_attributes() {
        check(
            document,
            "fn f() {}\n#![allow(a)] fn g() {}",
            expect![[r##"
                Document@0..32
                  FunctionItem@0..9
                    Fn@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..4 "f"
                    OpenParen@4..5 "("
                    CloseParen@5..6 ")"
                    Whitespace@6..7 " "
                    Block@7..9
                      OpenBrace@7..8 "{"
                      CloseBrace@8..9 "}"
                  Whitespace@9..10 "\n"
                  FunctionItem@10..32
                    Attribute@10..22
                      Pound@10..11 "#"
                      Not@11..12 "!"
                      OpenBracket@12..13 "["
                      Ident@13..18 "allow"
                      OpenParen@18..19 "("
                      Ident@19..20 "a"
                      CloseParen@20..21 ")"
                      CloseBracket@21..22 "]"
                    Whitespace@22..23 " "
                    Fn@23..25 "fn"
                    Whitespace@25..26 " "
                    Ident@26..27 "g"
                    OpenParen@27..28 "("
                    CloseParen@28..29 ")"
                    Whitespace@29..30 " "
                    Block@30..32
                      OpenBrace@30..31 "{"
                      CloseBrace@31..32 "}"
            "##]],
            expect![""],
        );
    }
}
