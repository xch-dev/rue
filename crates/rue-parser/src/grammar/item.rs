use rowan::Checkpoint;
use rue_diagnostic::DiagnosticKind;

use crate::{
    Parser, SyntaxKind, T,
    grammar::{
        binding::binding, block::block, delimited_list, delimited_list_with_stops,
        expect_semicolon, expr::expr, generics::generic_parameters, skip_balanced, ty::ty,
    },
};

/// Parses a top level item. This always consumes at least one token, unless at the end of the file.
pub fn item(p: &mut Parser) {
    item_with(p, true);
}

/// At the top level, a stray closing brace is skipped as part of an error rather than ending the
/// item, since it's likely left over from a mistake earlier in the file. Inside of a module, it
/// ends the module instead.
fn item_with(p: &mut Parser, top_level: bool) {
    let start = p.pos();
    let cp = p.checkpoint();

    let export = p.try_eat(T![export]);
    let inline = p.try_eat(T![inline]);
    let extern_kw = if inline { false } else { p.try_eat(T![extern]) };
    let test = p.try_eat(T![test]);

    if p.at_quiet(T![mod]) && !inline && !extern_kw && !test {
        module_item(p, cp);
    } else if p.at_quiet(T![fn]) {
        function_item(p, cp, extern_kw);
    } else if p.at_quiet(T![const]) && !extern_kw && !test {
        constant_item(p, cp);
    } else if p.at_quiet(T![type]) && !inline && !extern_kw && !test {
        type_alias_item(p, cp);
    } else if p.at_quiet(T![struct]) && !inline && !extern_kw && !test {
        struct_item(p, cp);
    } else if p.at_quiet(T![import]) || export {
        import_item(p, cp, export);
    } else {
        p.clear_expected();
        p.expected("item");
        p.error_unexpected();

        // Skip everything up until the next item, so that a stray statement or expression only
        // results in a single error rather than one for every token
        if p.pos() == start {
            p.bump_as_error();
        }

        skip_balanced(p, |p| !top_level && p.at_quiet(T!['}']));
    }
}

fn module_item(p: &mut Parser, cp: Checkpoint) {
    p.start_at(cp, SyntaxKind::ModuleItem);
    p.expect(T![mod]);
    p.expect(SyntaxKind::Ident);
    if p.expect(T!['{']) {
        while !p.at(T!['}']) && !p.at_quiet(SyntaxKind::Eof) {
            item_with(p, false);
        }
        p.expect(T!['}']);
    }
    p.finish();
}

fn function_item(p: &mut Parser, cp: Checkpoint, extern_kw: bool) {
    p.start_at(cp, SyntaxKind::FunctionItem);
    p.expect(T![fn]);
    p.expect(SyntaxKind::Ident);
    if p.at(T![<]) {
        generic_parameters(p);
    }
    if p.expect(T!['(']) {
        delimited_list_with_stops(p, T![')'], &[T![->], T!['{']], function_parameter);
    }
    if p.try_eat(T![->]) {
        ty(p);
    }
    if extern_kw && p.at(T![from]) {
        p.expect(T![from]);
        p.expect(SyntaxKind::String);
        expect_semicolon(p);
    } else if p.at(T!['{']) {
        block(p);
    } else {
        p.error(DiagnosticKind::MissingFunctionBody);
    }
    p.finish();
}

fn function_parameter(p: &mut Parser) {
    // Every parameter has a type, so a brace that isn't followed by one is the function body
    // rather than a struct binding (for example, if the closing parenthesis is missing)
    if p.at_quiet(T!['{']) && p.lookahead_past_group() != T![:] {
        return;
    }

    p.start(SyntaxKind::FunctionParameter);
    p.try_eat(T![...]);
    binding(p);
    p.expect(T![:]);
    ty(p);
    p.finish();
}

fn constant_item(p: &mut Parser, cp: Checkpoint) {
    p.start_at(cp, SyntaxKind::ConstantItem);
    p.expect(T![const]);
    p.expect(SyntaxKind::Ident);
    p.expect(T![:]);
    ty(p);
    p.expect(T![=]);
    expr(p);
    expect_semicolon(p);
    p.finish();
}

fn type_alias_item(p: &mut Parser, cp: Checkpoint) {
    p.start_at(cp, SyntaxKind::TypeAliasItem);
    p.expect(T![type]);
    p.expect(SyntaxKind::Ident);
    if p.at(T![<]) {
        generic_parameters(p);
    }
    p.expect(T![=]);
    ty(p);
    expect_semicolon(p);
    p.finish();
}

fn struct_item(p: &mut Parser, cp: Checkpoint) {
    p.start_at(cp, SyntaxKind::StructItem);
    p.expect(T![struct]);
    p.expect(SyntaxKind::Ident);
    if p.at(T![<]) {
        generic_parameters(p);
    }
    if p.expect(T!['{']) {
        delimited_list(p, T!['}'], struct_field);
    }
    p.finish();
}

fn struct_field(p: &mut Parser) {
    p.start(SyntaxKind::StructField);
    p.try_eat(T![...]);
    p.expect(SyntaxKind::Ident);
    if p.try_eat(T![:]) {
        ty(p);
        if p.try_eat(T![=]) {
            expr(p);
        }
    } else {
        p.expect(T![=]);
        expr(p);
    }
    p.finish();
}

fn import_item(p: &mut Parser, cp: Checkpoint, export: bool) {
    p.start_at(cp, SyntaxKind::ImportItem);
    if !export {
        p.expect(T![import]);
    }
    import_path(p, true);
    expect_semicolon(p);
    p.finish();
}

fn import_path(p: &mut Parser, mut first: bool) {
    p.start(SyntaxKind::ImportPath);
    let mut required = true;
    while p.at(T![::]) || required {
        if import_path_segment(p, first) {
            break;
        }
        first = false;
        required = false;
    }
    p.finish();
}

fn import_path_segment(p: &mut Parser, first: bool) -> bool {
    p.start(SyntaxKind::ImportPathSegment);
    p.try_eat(T![::]);
    let last = if !first && p.at(T![*]) {
        p.expect(T![*]);
        true
    } else if !first && p.at(T!['{']) {
        p.expect(T!['{']);
        delimited_list(p, T!['}'], |p| import_path(p, false));
        true
    } else if p.at(T![super]) {
        p.expect(T![super]);
        false
    } else {
        p.expect(SyntaxKind::Ident);
        false
    };
    p.finish();
    last
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::grammar::tests::check;

    use super::*;

    #[test]
    fn test_function_item() {
        check(
            item,
            "fn main() -> Int {}",
            expect![[r#"
                FunctionItem@0..19
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  Ident@3..7 "main"
                  OpenParen@7..8 "("
                  CloseParen@8..9 ")"
                  Whitespace@9..10 " "
                  Arrow@10..12 "->"
                  Whitespace@12..13 " "
                  PathType@13..17
                    PathSegment@13..17
                      Ident@13..16 "Int"
                      Whitespace@16..17 " "
                  Block@17..19
                    OpenBrace@17..18 "{"
                    CloseBrace@18..19 "}"
            "#]],
            expect![""],
        );

        check(
            item,
            "fn main<T>() -> T {}",
            expect![[r#"
                FunctionItem@0..20
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  Ident@3..7 "main"
                  GenericParameters@7..10
                    LessThan@7..8 "<"
                    Ident@8..9 "T"
                    GreaterThan@9..10 ">"
                  OpenParen@10..11 "("
                  CloseParen@11..12 ")"
                  Whitespace@12..13 " "
                  Arrow@13..15 "->"
                  Whitespace@15..16 " "
                  PathType@16..18
                    PathSegment@16..18
                      Ident@16..17 "T"
                      Whitespace@17..18 " "
                  Block@18..20
                    OpenBrace@18..19 "{"
                    CloseBrace@19..20 "}"
            "#]],
            expect![""],
        );

        check(
            item,
            "fn main(value: Int) -> Int { value + 42 }",
            expect![[r#"
                FunctionItem@0..41
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  Ident@3..7 "main"
                  OpenParen@7..8 "("
                  FunctionParameter@8..18
                    NamedBinding@8..13
                      Ident@8..13 "value"
                    Colon@13..14 ":"
                    Whitespace@14..15 " "
                    PathType@15..18
                      PathSegment@15..18
                        Ident@15..18 "Int"
                  CloseParen@18..19 ")"
                  Whitespace@19..20 " "
                  Arrow@20..22 "->"
                  Whitespace@22..23 " "
                  PathType@23..27
                    PathSegment@23..27
                      Ident@23..26 "Int"
                      Whitespace@26..27 " "
                  Block@27..41
                    OpenBrace@27..28 "{"
                    Whitespace@28..29 " "
                    BinaryExpr@29..40
                      PathExpr@29..35
                        PathSegment@29..35
                          Ident@29..34 "value"
                          Whitespace@34..35 " "
                      Plus@35..36 "+"
                      Whitespace@36..37 " "
                      LiteralExpr@37..39
                        Integer@37..39 "42"
                      Whitespace@39..40 " "
                    CloseBrace@40..41 "}"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_external_function_item() {
        check(
            item,
            "extern fn add(a: Int, b: Int) -> Int\n    from \"./add.hex\";",
            expect![[r#"
                FunctionItem@0..58
                  Extern@0..6 "extern"
                  Whitespace@6..7 " "
                  Fn@7..9 "fn"
                  Whitespace@9..10 " "
                  Ident@10..13 "add"
                  OpenParen@13..14 "("
                  FunctionParameter@14..20
                    NamedBinding@14..15
                      Ident@14..15 "a"
                    Colon@15..16 ":"
                    Whitespace@16..17 " "
                    PathType@17..20
                      PathSegment@17..20
                        Ident@17..20 "Int"
                  Comma@20..21 ","
                  Whitespace@21..22 " "
                  FunctionParameter@22..28
                    NamedBinding@22..23
                      Ident@22..23 "b"
                    Colon@23..24 ":"
                    Whitespace@24..25 " "
                    PathType@25..28
                      PathSegment@25..28
                        Ident@25..28 "Int"
                  CloseParen@28..29 ")"
                  Whitespace@29..30 " "
                  Arrow@30..32 "->"
                  Whitespace@32..33 " "
                  PathType@33..41
                    PathSegment@33..41
                      Ident@33..36 "Int"
                      Whitespace@36..41 "\n    "
                  From@41..45 "from"
                  Whitespace@45..46 " "
                  String@46..57 "\"./add.hex\""
                  Semicolon@57..58 ";"
            "#]],
            expect![""],
        );

        check(
            item,
            "extern fn add(a: Int, b: Int) -> Int { a + b }",
            expect![[r#"
                FunctionItem@0..46
                  Extern@0..6 "extern"
                  Whitespace@6..7 " "
                  Fn@7..9 "fn"
                  Whitespace@9..10 " "
                  Ident@10..13 "add"
                  OpenParen@13..14 "("
                  FunctionParameter@14..20
                    NamedBinding@14..15
                      Ident@14..15 "a"
                    Colon@15..16 ":"
                    Whitespace@16..17 " "
                    PathType@17..20
                      PathSegment@17..20
                        Ident@17..20 "Int"
                  Comma@20..21 ","
                  Whitespace@21..22 " "
                  FunctionParameter@22..28
                    NamedBinding@22..23
                      Ident@22..23 "b"
                    Colon@23..24 ":"
                    Whitespace@24..25 " "
                    PathType@25..28
                      PathSegment@25..28
                        Ident@25..28 "Int"
                  CloseParen@28..29 ")"
                  Whitespace@29..30 " "
                  Arrow@30..32 "->"
                  Whitespace@32..33 " "
                  PathType@33..37
                    PathSegment@33..37
                      Ident@33..36 "Int"
                      Whitespace@36..37 " "
                  Block@37..46
                    OpenBrace@37..38 "{"
                    Whitespace@38..39 " "
                    BinaryExpr@39..45
                      PathExpr@39..41
                        PathSegment@39..41
                          Ident@39..40 "a"
                          Whitespace@40..41 " "
                      Plus@41..42 "+"
                      Whitespace@42..43 " "
                      PathExpr@43..45
                        PathSegment@43..45
                          Ident@43..44 "b"
                          Whitespace@44..45 " "
                    CloseBrace@45..46 "}"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_external_source_requires_extern() {
        check(
            item,
            "fn add() from \"./add.hex\";",
            expect![[r#"
                FunctionItem@0..9
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  Ident@3..6 "add"
                  OpenParen@6..7 "("
                  CloseParen@7..8 ")"
                  Whitespace@8..9 " "
            "#]],
            expect!["Missing function body at main.rue:1:10"],
        );
    }

    #[test]
    fn test_constant_item() {
        check(
            item,
            "inline const VALUE: Int = 42;",
            expect![[r#"
                ConstantItem@0..29
                  Inline@0..6 "inline"
                  Whitespace@6..7 " "
                  Const@7..12 "const"
                  Whitespace@12..13 " "
                  Ident@13..18 "VALUE"
                  Colon@18..19 ":"
                  Whitespace@19..20 " "
                  PathType@20..24
                    PathSegment@20..24
                      Ident@20..23 "Int"
                      Whitespace@23..24 " "
                  Assign@24..25 "="
                  Whitespace@25..26 " "
                  LiteralExpr@26..28
                    Integer@26..28 "42"
                  Semicolon@28..29 ";"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_type_alias_item() {
        check(
            item,
            "type Hello = World;",
            expect![[r#"
                TypeAliasItem@0..19
                  Type@0..4 "type"
                  Whitespace@4..5 " "
                  Ident@5..10 "Hello"
                  Whitespace@10..11 " "
                  Assign@11..12 "="
                  Whitespace@12..13 " "
                  PathType@13..18
                    PathSegment@13..18
                      Ident@13..18 "World"
                  Semicolon@18..19 ";"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_import_item() {
        check(
            item,
            "import clvm::quote;",
            expect![[r#"
                ImportItem@0..19
                  Import@0..6 "import"
                  Whitespace@6..7 " "
                  ImportPath@7..18
                    ImportPathSegment@7..11
                      Ident@7..11 "clvm"
                    ImportPathSegment@11..18
                      PathSeparator@11..13 "::"
                      Ident@13..18 "quote"
                  Semicolon@18..19 ";"
            "#]],
            expect![""],
        );

        check(
            item,
            "import hello_world;",
            expect![[r#"
                ImportItem@0..19
                  Import@0..6 "import"
                  Whitespace@6..7 " "
                  ImportPath@7..18
                    ImportPathSegment@7..18
                      Ident@7..18 "hello_world"
                  Semicolon@18..19 ";"
            "#]],
            expect![""],
        );

        check(
            item,
            "import;",
            expect![[r#"
                ImportItem@0..7
                  Import@0..6 "import"
                  ImportPath@6..6
                    ImportPathSegment@6..6
                  Semicolon@6..7 ";"
            "#]],
            expect!["Expected one of `::`, `super`, identifier, found `;` at main.rue:1:7"],
        );

        check(
            item,
            "import *;",
            expect![[r#"
                ImportItem@0..9
                  Import@0..6 "import"
                  Whitespace@6..7 " "
                  ImportPath@7..7
                    ImportPathSegment@7..7
                  Error@7..8 "*"
                  Semicolon@8..9 ";"
            "#]],
            expect!["Expected one of `::`, `super`, identifier, found `*` at main.rue:1:8"],
        );

        check(
            item,
            "import {};",
            expect![[r#"
                ImportItem@0..10
                  Import@0..6 "import"
                  Whitespace@6..7 " "
                  ImportPath@7..7
                    ImportPathSegment@7..7
                  Error@7..8 "{"
                  Error@8..9 "}"
                  Semicolon@9..10 ";"
            "#]],
            expect!["Expected one of `::`, `super`, identifier, found `{` at main.rue:1:8"],
        );

        check(
            item,
            "import {x, y};",
            expect![[r#"
                ImportItem@0..14
                  Import@0..6 "import"
                  Whitespace@6..7 " "
                  ImportPath@7..7
                    ImportPathSegment@7..7
                  Error@7..8 "{"
                  Error@8..9 "x"
                  Error@9..10 ","
                  Whitespace@10..11 " "
                  Error@11..12 "y"
                  Error@12..13 "}"
                  Semicolon@13..14 ";"
            "#]],
            expect!["Expected one of `::`, `super`, identifier, found `{` at main.rue:1:8"],
        );

        check(
            item,
            "import hello::{};",
            expect![[r#"
                ImportItem@0..17
                  Import@0..6 "import"
                  Whitespace@6..7 " "
                  ImportPath@7..16
                    ImportPathSegment@7..12
                      Ident@7..12 "hello"
                    ImportPathSegment@12..16
                      PathSeparator@12..14 "::"
                      OpenBrace@14..15 "{"
                      CloseBrace@15..16 "}"
                  Semicolon@16..17 ";"
            "#]],
            expect![""],
        );

        check(
            item,
            "import hello::{world, there,};",
            expect![[r#"
                ImportItem@0..30
                  Import@0..6 "import"
                  Whitespace@6..7 " "
                  ImportPath@7..29
                    ImportPathSegment@7..12
                      Ident@7..12 "hello"
                    ImportPathSegment@12..29
                      PathSeparator@12..14 "::"
                      OpenBrace@14..15 "{"
                      ImportPath@15..20
                        ImportPathSegment@15..20
                          Ident@15..20 "world"
                      Comma@20..21 ","
                      Whitespace@21..22 " "
                      ImportPath@22..27
                        ImportPathSegment@22..27
                          Ident@22..27 "there"
                      Comma@27..28 ","
                      CloseBrace@28..29 "}"
                  Semicolon@29..30 ";"
            "#]],
            expect![""],
        );

        check(
            item,
            "import hello::*;",
            expect![[r#"
                ImportItem@0..16
                  Import@0..6 "import"
                  Whitespace@6..7 " "
                  ImportPath@7..15
                    ImportPathSegment@7..12
                      Ident@7..12 "hello"
                    ImportPathSegment@12..15
                      PathSeparator@12..14 "::"
                      Star@14..15 "*"
                  Semicolon@15..16 ";"
            "#]],
            expect![""],
        );

        check(
            item,
            "export clvm::quote;",
            expect![[r#"
                ImportItem@0..19
                  Export@0..6 "export"
                  Whitespace@6..7 " "
                  ImportPath@7..18
                    ImportPathSegment@7..11
                      Ident@7..11 "clvm"
                    ImportPathSegment@11..18
                      PathSeparator@11..13 "::"
                      Ident@13..18 "quote"
                  Semicolon@18..19 ";"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_function_item_modifiers() {
        check(
            item,
            "inline fn main() {}",
            expect![[r#"
                FunctionItem@0..19
                  Inline@0..6 "inline"
                  Whitespace@6..7 " "
                  Fn@7..9 "fn"
                  Whitespace@9..10 " "
                  Ident@10..14 "main"
                  OpenParen@14..15 "("
                  CloseParen@15..16 ")"
                  Whitespace@16..17 " "
                  Block@17..19
                    OpenBrace@17..18 "{"
                    CloseBrace@18..19 "}"
            "#]],
            expect![""],
        );

        check(
            item,
            "test fn main() {}",
            expect![[r#"
                FunctionItem@0..17
                  Test@0..4 "test"
                  Whitespace@4..5 " "
                  Fn@5..7 "fn"
                  Whitespace@7..8 " "
                  Ident@8..12 "main"
                  OpenParen@12..13 "("
                  CloseParen@13..14 ")"
                  Whitespace@14..15 " "
                  Block@15..17
                    OpenBrace@15..16 "{"
                    CloseBrace@16..17 "}"
            "#]],
            expect![""],
        );

        check(
            item,
            "export fn main() {}",
            expect![[r#"
                FunctionItem@0..19
                  Export@0..6 "export"
                  Whitespace@6..7 " "
                  Fn@7..9 "fn"
                  Whitespace@9..10 " "
                  Ident@10..14 "main"
                  OpenParen@14..15 "("
                  CloseParen@15..16 ")"
                  Whitespace@16..17 " "
                  Block@17..19
                    OpenBrace@17..18 "{"
                    CloseBrace@18..19 "}"
            "#]],
            expect![""],
        );

        check(
            item,
            "export inline fn main() {}",
            expect![[r#"
                FunctionItem@0..26
                  Export@0..6 "export"
                  Whitespace@6..7 " "
                  Inline@7..13 "inline"
                  Whitespace@13..14 " "
                  Fn@14..16 "fn"
                  Whitespace@16..17 " "
                  Ident@17..21 "main"
                  OpenParen@21..22 "("
                  CloseParen@22..23 ")"
                  Whitespace@23..24 " "
                  Block@24..26
                    OpenBrace@24..25 "{"
                    CloseBrace@25..26 "}"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_function_item_spread_parameter() {
        check(
            item,
            "fn main(...args: List<Int>) {}",
            expect![[r#"
                FunctionItem@0..30
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  Ident@3..7 "main"
                  OpenParen@7..8 "("
                  FunctionParameter@8..26
                    Spread@8..11 "..."
                    NamedBinding@11..15
                      Ident@11..15 "args"
                    Colon@15..16 ":"
                    Whitespace@16..17 " "
                    PathType@17..26
                      PathSegment@17..26
                        Ident@17..21 "List"
                        GenericArguments@21..26
                          LessThan@21..22 "<"
                          PathType@22..25
                            PathSegment@22..25
                              Ident@22..25 "Int"
                          GreaterThan@25..26 ">"
                  CloseParen@26..27 ")"
                  Whitespace@27..28 " "
                  Block@28..30
                    OpenBrace@28..29 "{"
                    CloseBrace@29..30 "}"
            "#]],
            expect![""],
        );

        check(
            item,
            "fn main(a: Int, b: Int,) {}",
            expect![[r#"
                FunctionItem@0..27
                  Fn@0..2 "fn"
                  Whitespace@2..3 " "
                  Ident@3..7 "main"
                  OpenParen@7..8 "("
                  FunctionParameter@8..14
                    NamedBinding@8..9
                      Ident@8..9 "a"
                    Colon@9..10 ":"
                    Whitespace@10..11 " "
                    PathType@11..14
                      PathSegment@11..14
                        Ident@11..14 "Int"
                  Comma@14..15 ","
                  Whitespace@15..16 " "
                  FunctionParameter@16..22
                    NamedBinding@16..17
                      Ident@16..17 "b"
                    Colon@17..18 ":"
                    Whitespace@18..19 " "
                    PathType@19..22
                      PathSegment@19..22
                        Ident@19..22 "Int"
                  Comma@22..23 ","
                  CloseParen@23..24 ")"
                  Whitespace@24..25 " "
                  Block@25..27
                    OpenBrace@25..26 "{"
                    CloseBrace@26..27 "}"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_struct_item() {
        check(
            item,
            "struct Empty {}",
            expect![[r#"
                StructItem@0..15
                  Struct@0..6 "struct"
                  Whitespace@6..7 " "
                  Ident@7..12 "Empty"
                  Whitespace@12..13 " "
                  OpenBrace@13..14 "{"
                  CloseBrace@14..15 "}"
            "#]],
            expect![""],
        );

        check(
            item,
            "struct Point { x: Int, y: Int }",
            expect![[r#"
                StructItem@0..31
                  Struct@0..6 "struct"
                  Whitespace@6..7 " "
                  Ident@7..12 "Point"
                  Whitespace@12..13 " "
                  OpenBrace@13..14 "{"
                  Whitespace@14..15 " "
                  StructField@15..21
                    Ident@15..16 "x"
                    Colon@16..17 ":"
                    Whitespace@17..18 " "
                    PathType@18..21
                      PathSegment@18..21
                        Ident@18..21 "Int"
                  Comma@21..22 ","
                  Whitespace@22..23 " "
                  StructField@23..30
                    Ident@23..24 "y"
                    Colon@24..25 ":"
                    Whitespace@25..26 " "
                    PathType@26..30
                      PathSegment@26..30
                        Ident@26..29 "Int"
                        Whitespace@29..30 " "
                  CloseBrace@30..31 "}"
            "#]],
            expect![""],
        );

        check(
            item,
            "struct Point { x: Int, y: Int, }",
            expect![[r#"
                StructItem@0..32
                  Struct@0..6 "struct"
                  Whitespace@6..7 " "
                  Ident@7..12 "Point"
                  Whitespace@12..13 " "
                  OpenBrace@13..14 "{"
                  Whitespace@14..15 " "
                  StructField@15..21
                    Ident@15..16 "x"
                    Colon@16..17 ":"
                    Whitespace@17..18 " "
                    PathType@18..21
                      PathSegment@18..21
                        Ident@18..21 "Int"
                  Comma@21..22 ","
                  Whitespace@22..23 " "
                  StructField@23..29
                    Ident@23..24 "y"
                    Colon@24..25 ":"
                    Whitespace@25..26 " "
                    PathType@26..29
                      PathSegment@26..29
                        Ident@26..29 "Int"
                  Comma@29..30 ","
                  Whitespace@30..31 " "
                  CloseBrace@31..32 "}"
            "#]],
            expect![""],
        );

        check(
            item,
            "struct Wrapper<T> { value: T }",
            expect![[r#"
                StructItem@0..30
                  Struct@0..6 "struct"
                  Whitespace@6..7 " "
                  Ident@7..14 "Wrapper"
                  GenericParameters@14..17
                    LessThan@14..15 "<"
                    Ident@15..16 "T"
                    GreaterThan@16..17 ">"
                  Whitespace@17..18 " "
                  OpenBrace@18..19 "{"
                  Whitespace@19..20 " "
                  StructField@20..29
                    Ident@20..25 "value"
                    Colon@25..26 ":"
                    Whitespace@26..27 " "
                    PathType@27..29
                      PathSegment@27..29
                        Ident@27..28 "T"
                        Whitespace@28..29 " "
                  CloseBrace@29..30 "}"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_struct_item_default_fields() {
        check(
            item,
            "struct Config { value: Int = 42, flag = true, ...rest: Any }",
            expect![[r#"
                StructItem@0..60
                  Struct@0..6 "struct"
                  Whitespace@6..7 " "
                  Ident@7..13 "Config"
                  Whitespace@13..14 " "
                  OpenBrace@14..15 "{"
                  Whitespace@15..16 " "
                  StructField@16..31
                    Ident@16..21 "value"
                    Colon@21..22 ":"
                    Whitespace@22..23 " "
                    PathType@23..27
                      PathSegment@23..27
                        Ident@23..26 "Int"
                        Whitespace@26..27 " "
                    Assign@27..28 "="
                    Whitespace@28..29 " "
                    LiteralExpr@29..31
                      Integer@29..31 "42"
                  Comma@31..32 ","
                  Whitespace@32..33 " "
                  StructField@33..44
                    Ident@33..37 "flag"
                    Whitespace@37..38 " "
                    Assign@38..39 "="
                    Whitespace@39..40 " "
                    LiteralExpr@40..44
                      True@40..44 "true"
                  Comma@44..45 ","
                  Whitespace@45..46 " "
                  StructField@46..59
                    Spread@46..49 "..."
                    Ident@49..53 "rest"
                    Colon@53..54 ":"
                    Whitespace@54..55 " "
                    PathType@55..59
                      PathSegment@55..59
                        Ident@55..58 "Any"
                        Whitespace@58..59 " "
                  CloseBrace@59..60 "}"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_module_item() {
        check(
            item,
            "mod empty {}",
            expect![[r#"
                ModuleItem@0..12
                  Mod@0..3 "mod"
                  Whitespace@3..4 " "
                  Ident@4..9 "empty"
                  Whitespace@9..10 " "
                  OpenBrace@10..11 "{"
                  CloseBrace@11..12 "}"
            "#]],
            expect![""],
        );

        check(
            item,
            "mod outer { fn main() {} mod inner { const X: Int = 1; } }",
            expect![[r#"
                ModuleItem@0..58
                  Mod@0..3 "mod"
                  Whitespace@3..4 " "
                  Ident@4..9 "outer"
                  Whitespace@9..10 " "
                  OpenBrace@10..11 "{"
                  Whitespace@11..12 " "
                  FunctionItem@12..24
                    Fn@12..14 "fn"
                    Whitespace@14..15 " "
                    Ident@15..19 "main"
                    OpenParen@19..20 "("
                    CloseParen@20..21 ")"
                    Whitespace@21..22 " "
                    Block@22..24
                      OpenBrace@22..23 "{"
                      CloseBrace@23..24 "}"
                  Whitespace@24..25 " "
                  ModuleItem@25..56
                    Mod@25..28 "mod"
                    Whitespace@28..29 " "
                    Ident@29..34 "inner"
                    Whitespace@34..35 " "
                    OpenBrace@35..36 "{"
                    Whitespace@36..37 " "
                    ConstantItem@37..54
                      Const@37..42 "const"
                      Whitespace@42..43 " "
                      Ident@43..44 "X"
                      Colon@44..45 ":"
                      Whitespace@45..46 " "
                      PathType@46..50
                        PathSegment@46..50
                          Ident@46..49 "Int"
                          Whitespace@49..50 " "
                      Assign@50..51 "="
                      Whitespace@51..52 " "
                      LiteralExpr@52..53
                        Integer@52..53 "1"
                      Semicolon@53..54 ";"
                    Whitespace@54..55 " "
                    CloseBrace@55..56 "}"
                  Whitespace@56..57 " "
                  CloseBrace@57..58 "}"
            "#]],
            expect![""],
        );

        check(
            item,
            "export mod public {}",
            expect![[r#"
                ModuleItem@0..20
                  Export@0..6 "export"
                  Whitespace@6..7 " "
                  Mod@7..10 "mod"
                  Whitespace@10..11 " "
                  Ident@11..17 "public"
                  Whitespace@17..18 " "
                  OpenBrace@18..19 "{"
                  CloseBrace@19..20 "}"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_type_alias_item_generic() {
        check(
            item,
            "type Pair<A, B> = (A, B);",
            expect![[r#"
                TypeAliasItem@0..25
                  Type@0..4 "type"
                  Whitespace@4..5 " "
                  Ident@5..9 "Pair"
                  GenericParameters@9..15
                    LessThan@9..10 "<"
                    Ident@10..11 "A"
                    Comma@11..12 ","
                    Whitespace@12..13 " "
                    Ident@13..14 "B"
                    GreaterThan@14..15 ">"
                  Whitespace@15..16 " "
                  Assign@16..17 "="
                  Whitespace@17..18 " "
                  PairType@18..24
                    OpenParen@18..19 "("
                    PathType@19..20
                      PathSegment@19..20
                        Ident@19..20 "A"
                    Comma@20..21 ","
                    Whitespace@21..22 " "
                    PathType@22..23
                      PathSegment@22..23
                        Ident@22..23 "B"
                    CloseParen@23..24 ")"
                  Semicolon@24..25 ";"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_import_item_super() {
        check(
            item,
            "import super::value;",
            expect![[r#"
                ImportItem@0..20
                  Import@0..6 "import"
                  Whitespace@6..7 " "
                  ImportPath@7..19
                    ImportPathSegment@7..12
                      Super@7..12 "super"
                    ImportPathSegment@12..19
                      PathSeparator@12..14 "::"
                      Ident@14..19 "value"
                  Semicolon@19..20 ";"
            "#]],
            expect![""],
        );

        check(
            item,
            "import ::root::value;",
            expect![[r#"
                ImportItem@0..21
                  Import@0..6 "import"
                  Whitespace@6..7 " "
                  ImportPath@7..20
                    ImportPathSegment@7..13
                      PathSeparator@7..9 "::"
                      Ident@9..13 "root"
                    ImportPathSegment@13..20
                      PathSeparator@13..15 "::"
                      Ident@15..20 "value"
                  Semicolon@20..21 ";"
            "#]],
            expect![""],
        );

        check(
            item,
            "import a::{b::{c, d}, e::*};",
            expect![[r#"
                ImportItem@0..28
                  Import@0..6 "import"
                  Whitespace@6..7 " "
                  ImportPath@7..27
                    ImportPathSegment@7..8
                      Ident@7..8 "a"
                    ImportPathSegment@8..27
                      PathSeparator@8..10 "::"
                      OpenBrace@10..11 "{"
                      ImportPath@11..20
                        ImportPathSegment@11..12
                          Ident@11..12 "b"
                        ImportPathSegment@12..20
                          PathSeparator@12..14 "::"
                          OpenBrace@14..15 "{"
                          ImportPath@15..16
                            ImportPathSegment@15..16
                              Ident@15..16 "c"
                          Comma@16..17 ","
                          Whitespace@17..18 " "
                          ImportPath@18..19
                            ImportPathSegment@18..19
                              Ident@18..19 "d"
                          CloseBrace@19..20 "}"
                      Comma@20..21 ","
                      Whitespace@21..22 " "
                      ImportPath@22..26
                        ImportPathSegment@22..23
                          Ident@22..23 "e"
                        ImportPathSegment@23..26
                          PathSeparator@23..25 "::"
                          Star@25..26 "*"
                      CloseBrace@26..27 "}"
                  Semicolon@27..28 ";"
            "#]],
            expect![""],
        );
    }
}
