use crate::{Parser, SyntaxKind, grammar::item::item};

pub fn document(p: &mut Parser) {
    p.start_including_trivia(SyntaxKind::Document);
    while !p.at_quiet(SyntaxKind::Eof) {
        item(p);
    }
    p.eat_trivia();
    p.finish();
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::grammar::tests::check;

    use super::*;

    #[test]
    fn test_document() {
        check(
            document,
            "// A file comment\n\nimport foo::bar;\n\nconst VALUE: Int = 42;\n\nfn main() -> Int {\n    VALUE\n}\n",
            expect![[r#"
                Document@0..92
                  LineComment@0..18 "// A file comment\n"
                  Whitespace@18..19 "\n"
                  ImportItem@19..35
                    Import@19..25 "import"
                    Whitespace@25..26 " "
                    ImportPath@26..34
                      ImportPathSegment@26..29
                        Ident@26..29 "foo"
                      ImportPathSegment@29..34
                        PathSeparator@29..31 "::"
                        Ident@31..34 "bar"
                    Semicolon@34..35 ";"
                  Whitespace@35..37 "\n\n"
                  ConstantItem@37..59
                    Const@37..42 "const"
                    Whitespace@42..43 " "
                    Ident@43..48 "VALUE"
                    Colon@48..49 ":"
                    Whitespace@49..50 " "
                    PathType@50..54
                      PathSegment@50..54
                        Ident@50..53 "Int"
                        Whitespace@53..54 " "
                    Assign@54..55 "="
                    Whitespace@55..56 " "
                    LiteralExpr@56..58
                      Integer@56..58 "42"
                    Semicolon@58..59 ";"
                  Whitespace@59..61 "\n\n"
                  FunctionItem@61..91
                    Fn@61..63 "fn"
                    Whitespace@63..64 " "
                    Ident@64..68 "main"
                    OpenParen@68..69 "("
                    CloseParen@69..70 ")"
                    Whitespace@70..71 " "
                    Arrow@71..73 "->"
                    Whitespace@73..74 " "
                    PathType@74..78
                      PathSegment@74..78
                        Ident@74..77 "Int"
                        Whitespace@77..78 " "
                    Block@78..91
                      OpenBrace@78..79 "{"
                      Whitespace@79..84 "\n    "
                      PathExpr@84..90
                        PathSegment@84..90
                          Ident@84..89 "VALUE"
                          Whitespace@89..90 "\n"
                      CloseBrace@90..91 "}"
                  Whitespace@91..92 "\n"
            "#]],
            expect![""],
        );
    }
}
