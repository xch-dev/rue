use crate::{
    Parser, SyntaxKind, T,
    grammar::{block::block, expr::expr, generics::generic_parameters, ty::ty},
};

pub fn item(p: &mut Parser<'_>) {
    if p.at(T![fn]) {
        function_item(p);
    } else if p.at(T![const]) {
        constant_item(p);
    } else if p.at(T![type]) {
        type_alias_item(p);
    } else if p.at(T![struct]) {
        struct_item(p);
    } else {
        p.skip();
    }
}

fn function_item(p: &mut Parser<'_>) {
    p.start(SyntaxKind::FunctionItem);
    p.expect(T![fn]);
    p.expect(SyntaxKind::Ident);
    if p.at(T![<]) {
        generic_parameters(p);
    }
    p.expect(T!['(']);
    while !p.at(T![')']) {
        function_parameter(p);
        if !p.try_eat(T![,]) {
            break;
        }
    }
    p.expect(T![')']);
    p.expect(T![->]);
    ty(p);
    block(p);
    p.finish();
}

fn function_parameter(p: &mut Parser<'_>) {
    p.start(SyntaxKind::FunctionParameter);
    p.expect(SyntaxKind::Ident);
    p.expect(T![:]);
    ty(p);
    p.finish();
}

fn constant_item(p: &mut Parser<'_>) {
    p.start(SyntaxKind::ConstantItem);
    p.expect(T![const]);
    p.expect(SyntaxKind::Ident);
    p.expect(T![:]);
    ty(p);
    p.expect(T![=]);
    expr(p);
    p.expect(T![;]);
    p.finish();
}

fn type_alias_item(p: &mut Parser<'_>) {
    p.start(SyntaxKind::TypeAliasItem);
    p.expect(T![type]);
    p.expect(SyntaxKind::Ident);
    if p.at(T![<]) {
        generic_parameters(p);
    }
    p.expect(T![=]);
    ty(p);
    p.expect(T![;]);
    p.finish();
}

fn struct_item(p: &mut Parser<'_>) {
    p.start(SyntaxKind::StructItem);
    p.expect(T![struct]);
    p.expect(SyntaxKind::Ident);
    if p.at(T![<]) {
        generic_parameters(p);
    }
    p.expect(T!['{']);
    while !p.at(T!['}']) {
        struct_field(p);
        if !p.try_eat(T![,]) {
            break;
        }
    }
    p.expect(T!['}']);
    p.finish();
}

fn struct_field(p: &mut Parser<'_>) {
    p.start(SyntaxKind::StructField);
    p.expect(SyntaxKind::Ident);
    p.expect(T![:]);
    ty(p);
    p.finish();
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
                    PathTypeSegment@13..17
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
                    PathTypeSegment@16..18
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
                    Ident@8..13 "value"
                    Colon@13..14 ":"
                    Whitespace@14..15 " "
                    PathType@15..18
                      PathTypeSegment@15..18
                        Ident@15..18 "Int"
                  CloseParen@18..19 ")"
                  Whitespace@19..20 " "
                  Arrow@20..22 "->"
                  Whitespace@22..23 " "
                  PathType@23..27
                    PathTypeSegment@23..27
                      Ident@23..26 "Int"
                      Whitespace@26..27 " "
                  Block@27..41
                    OpenBrace@27..28 "{"
                    Whitespace@28..29 " "
                    BinaryExpr@29..40
                      PathExpr@29..35
                        PathExprSegment@29..35
                          Ident@29..34 "value"
                          Whitespace@34..35 " "
                      Plus@35..36 "+"
                      Whitespace@36..37 " "
                      LiteralExpr@37..40
                        Integer@37..39 "42"
                        Whitespace@39..40 " "
                    CloseBrace@40..41 "}"
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
                    PathTypeSegment@13..18
                      Ident@13..18 "World"
                  Semicolon@18..19 ";"
            "#]],
            expect![""],
        );
    }
}
