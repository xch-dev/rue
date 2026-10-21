use crate::{
    Parser, SyntaxKind, T,
    grammar::{
        delimited_list,
        generics::{generic_arguments, generic_parameters},
        recover, skip_unexpected,
    },
};

pub fn ty(p: &mut Parser) {
    ty_inner(p, true);

    // The tokens that could have continued the type are too noisy to include in diagnostics
    // about what's expected after it
    p.clear_expected();
}

fn ty_inner(p: &mut Parser, allow_union: bool) {
    let cp = p.checkpoint();

    if p.at_quiet(SyntaxKind::Ident) || p.at_quiet(T![super]) || p.at_quiet(T![::]) {
        path_type(p);
    } else if let Some(kind) = p.at_any_quiet(SyntaxKind::LITERAL) {
        p.start(SyntaxKind::LiteralType);
        p.expect(kind);
        p.finish();
    } else if p.at_quiet(T!['(']) {
        p.expect(T!['(']);
        ty(p);
        if !p.at(T![,]) && !p.at(T![')']) {
            skip_unexpected(p);
        }
        if p.try_eat(T![,]) {
            p.start_at(cp, SyntaxKind::PairType);
            ty(p);
            p.try_eat(T![,]);
            if !p.at(T![')']) {
                skip_unexpected(p);
            }
        } else {
            p.start_at(cp, SyntaxKind::GroupType);
        }
        p.expect(T![')']);
        p.finish();
    } else if p.at_quiet(T!['[']) {
        p.start_at(cp, SyntaxKind::ListType);
        p.expect(T!['[']);
        delimited_list(p, T![']'], |p| {
            p.start(SyntaxKind::ListTypeItem);
            p.try_eat(T![...]);
            ty(p);
            p.finish();
        });
        p.finish();
    } else if p.at_quiet(T![fn]) {
        p.start(SyntaxKind::LambdaType);
        p.expect(T![fn]);
        if p.at(T![<]) {
            generic_parameters(p);
        }
        if p.expect(T!['(']) {
            delimited_list(p, T![')'], |p| {
                p.start(SyntaxKind::LambdaParameter);
                p.try_eat(T![...]);
                p.expect(SyntaxKind::Ident);
                p.expect(T![:]);
                ty(p);
                p.finish();
            });
        }
        p.expect(T![->]);
        ty(p);
        p.finish();
    } else {
        // A type is often followed by `=` (in a type alias or constant), `{` (as the return type
        // of a function), or `>` (as a generic argument), so stop there instead of skipping them
        recover(p, "type", &[T![=], T!['{'], T![>], T![|]]);
        return;
    }

    // A union member can't be empty, so `||` is always a logical or after the type (for example,
    // in `value is Int || other`) rather than the start of another union member
    if allow_union && at_union_separator(p) {
        p.start_at(cp, SyntaxKind::UnionType);
        while at_union_separator(p) {
            p.expect(T![|]);
            ty_inner(p, false);
        }
        p.finish();
    }
}

fn at_union_separator(p: &mut Parser) -> bool {
    p.at(T![|]) && !p.at(T![||])
}

fn path_type(p: &mut Parser) {
    p.start(SyntaxKind::PathType);
    path_type_segment(p, true);
    while p.at(T![::]) {
        path_type_segment(p, false);
    }
    p.finish();
}

fn path_type_segment(p: &mut Parser, first: bool) {
    p.start(SyntaxKind::PathSegment);
    if first {
        p.try_eat(T![::]);
    } else {
        p.expect(T![::]);
    }
    if !p.try_eat(T![super]) {
        p.expect(SyntaxKind::Ident);
    }
    if p.at(T![<]) {
        generic_arguments(p);
    }
    p.finish();
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::grammar::tests::check;

    use super::*;

    #[test]
    fn test_path_type() {
        check(
            ty,
            "String",
            expect![[r#"
                PathType@0..6
                  PathSegment@0..6
                    Ident@0..6 "String"
            "#]],
            expect![],
        );
    }

    #[test]
    fn test_union_type() {
        check(
            ty,
            "String | Int",
            expect![[r#"
                UnionType@0..12
                  PathType@0..7
                    PathSegment@0..7
                      Ident@0..6 "String"
                      Whitespace@6..7 " "
                  BitwiseOr@7..8 "|"
                  Whitespace@8..9 " "
                  PathType@9..12
                    PathSegment@9..12
                      Ident@9..12 "Int"
            "#]],
            expect![],
        );

        check(
            ty,
            "String | Int | Bool",
            expect![[r#"
                UnionType@0..19
                  PathType@0..7
                    PathSegment@0..7
                      Ident@0..6 "String"
                      Whitespace@6..7 " "
                  BitwiseOr@7..8 "|"
                  Whitespace@8..9 " "
                  PathType@9..13
                    PathSegment@9..13
                      Ident@9..12 "Int"
                      Whitespace@12..13 " "
                  BitwiseOr@13..14 "|"
                  Whitespace@14..15 " "
                  PathType@15..19
                    PathSegment@15..19
                      Ident@15..19 "Bool"
            "#]],
            expect![],
        );
    }

    #[test]
    fn test_literal_type() {
        check(
            ty,
            "nil",
            expect![[r#"
                LiteralType@0..3
                  Nil@0..3 "nil"
            "#]],
            expect![""],
        );

        check(
            ty,
            "42",
            expect![[r#"
                LiteralType@0..2
                  Integer@0..2 "42"
            "#]],
            expect![""],
        );

        check(
            ty,
            "\"hello\"",
            expect![[r#"
                LiteralType@0..7
                  String@0..7 "\"hello\""
            "#]],
            expect![""],
        );

        check(
            ty,
            "0xff",
            expect![[r#"
                LiteralType@0..4
                  Hex@0..4 "0xff"
            "#]],
            expect![""],
        );

        check(
            ty,
            "true",
            expect![[r#"
                LiteralType@0..4
                  True@0..4 "true"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_group_type() {
        check(
            ty,
            "(Int)",
            expect![[r#"
                GroupType@0..5
                  OpenParen@0..1 "("
                  PathType@1..4
                    PathSegment@1..4
                      Ident@1..4 "Int"
                  CloseParen@4..5 ")"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_pair_type() {
        check(
            ty,
            "(Int, Bytes)",
            expect![[r#"
                PairType@0..12
                  OpenParen@0..1 "("
                  PathType@1..4
                    PathSegment@1..4
                      Ident@1..4 "Int"
                  Comma@4..5 ","
                  Whitespace@5..6 " "
                  PathType@6..11
                    PathSegment@6..11
                      Ident@6..11 "Bytes"
                  CloseParen@11..12 ")"
            "#]],
            expect![""],
        );

        check(
            ty,
            "(Int, Bytes,)",
            expect![[r#"
                PairType@0..13
                  OpenParen@0..1 "("
                  PathType@1..4
                    PathSegment@1..4
                      Ident@1..4 "Int"
                  Comma@4..5 ","
                  Whitespace@5..6 " "
                  PathType@6..11
                    PathSegment@6..11
                      Ident@6..11 "Bytes"
                  Comma@11..12 ","
                  CloseParen@12..13 ")"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_list_type() {
        check(
            ty,
            "[]",
            expect![[r#"
                ListType@0..2
                  OpenBracket@0..1 "["
                  CloseBracket@1..2 "]"
            "#]],
            expect![""],
        );

        check(
            ty,
            "[Int, Bytes]",
            expect![[r#"
                ListType@0..12
                  OpenBracket@0..1 "["
                  ListTypeItem@1..4
                    PathType@1..4
                      PathSegment@1..4
                        Ident@1..4 "Int"
                  Comma@4..5 ","
                  Whitespace@5..6 " "
                  ListTypeItem@6..11
                    PathType@6..11
                      PathSegment@6..11
                        Ident@6..11 "Bytes"
                  CloseBracket@11..12 "]"
            "#]],
            expect![""],
        );

        check(
            ty,
            "[Int, ...List<Int>]",
            expect![[r#"
                ListType@0..19
                  OpenBracket@0..1 "["
                  ListTypeItem@1..4
                    PathType@1..4
                      PathSegment@1..4
                        Ident@1..4 "Int"
                  Comma@4..5 ","
                  Whitespace@5..6 " "
                  ListTypeItem@6..18
                    Spread@6..9 "..."
                    PathType@9..18
                      PathSegment@9..18
                        Ident@9..13 "List"
                        GenericArguments@13..18
                          LessThan@13..14 "<"
                          PathType@14..17
                            PathSegment@14..17
                              Ident@14..17 "Int"
                          GreaterThan@17..18 ">"
                  CloseBracket@18..19 "]"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_lambda_type() {
        check(
            ty,
            "fn() -> Int",
            expect![[r#"
                LambdaType@0..11
                  Fn@0..2 "fn"
                  OpenParen@2..3 "("
                  CloseParen@3..4 ")"
                  Whitespace@4..5 " "
                  Arrow@5..7 "->"
                  Whitespace@7..8 " "
                  PathType@8..11
                    PathSegment@8..11
                      Ident@8..11 "Int"
            "#]],
            expect![""],
        );

        check(
            ty,
            "fn(x: Int, ...rest: List<Int>) -> Bool",
            expect![[r#"
                LambdaType@0..38
                  Fn@0..2 "fn"
                  OpenParen@2..3 "("
                  LambdaParameter@3..9
                    Ident@3..4 "x"
                    Colon@4..5 ":"
                    Whitespace@5..6 " "
                    PathType@6..9
                      PathSegment@6..9
                        Ident@6..9 "Int"
                  Comma@9..10 ","
                  Whitespace@10..11 " "
                  LambdaParameter@11..29
                    Spread@11..14 "..."
                    Ident@14..18 "rest"
                    Colon@18..19 ":"
                    Whitespace@19..20 " "
                    PathType@20..29
                      PathSegment@20..29
                        Ident@20..24 "List"
                        GenericArguments@24..29
                          LessThan@24..25 "<"
                          PathType@25..28
                            PathSegment@25..28
                              Ident@25..28 "Int"
                          GreaterThan@28..29 ">"
                  CloseParen@29..30 ")"
                  Whitespace@30..31 " "
                  Arrow@31..33 "->"
                  Whitespace@33..34 " "
                  PathType@34..38
                    PathSegment@34..38
                      Ident@34..38 "Bool"
            "#]],
            expect![""],
        );

        check(
            ty,
            "fn<T>(value: T) -> T",
            expect![[r#"
                LambdaType@0..20
                  Fn@0..2 "fn"
                  GenericParameters@2..5
                    LessThan@2..3 "<"
                    Ident@3..4 "T"
                    GreaterThan@4..5 ">"
                  OpenParen@5..6 "("
                  LambdaParameter@6..14
                    Ident@6..11 "value"
                    Colon@11..12 ":"
                    Whitespace@12..13 " "
                    PathType@13..14
                      PathSegment@13..14
                        Ident@13..14 "T"
                  CloseParen@14..15 ")"
                  Whitespace@15..16 " "
                  Arrow@16..18 "->"
                  Whitespace@18..19 " "
                  PathType@19..20
                    PathSegment@19..20
                      Ident@19..20 "T"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_generic_path_type() {
        check(
            ty,
            "List<Int>",
            expect![[r#"
                PathType@0..9
                  PathSegment@0..9
                    Ident@0..4 "List"
                    GenericArguments@4..9
                      LessThan@4..5 "<"
                      PathType@5..8
                        PathSegment@5..8
                          Ident@5..8 "Int"
                      GreaterThan@8..9 ">"
            "#]],
            expect![""],
        );

        check(
            ty,
            "Map<Bytes, List<Int>>",
            expect![[r#"
                PathType@0..21
                  PathSegment@0..21
                    Ident@0..3 "Map"
                    GenericArguments@3..21
                      LessThan@3..4 "<"
                      PathType@4..9
                        PathSegment@4..9
                          Ident@4..9 "Bytes"
                      Comma@9..10 ","
                      Whitespace@10..11 " "
                      PathType@11..20
                        PathSegment@11..20
                          Ident@11..15 "List"
                          GenericArguments@15..20
                            LessThan@15..16 "<"
                            PathType@16..19
                              PathSegment@16..19
                                Ident@16..19 "Int"
                            GreaterThan@19..20 ">"
                      GreaterThan@20..21 ">"
            "#]],
            expect![""],
        );

        check(
            ty,
            "::module::Type",
            expect![[r#"
                PathType@0..14
                  PathSegment@0..8
                    PathSeparator@0..2 "::"
                    Ident@2..8 "module"
                  PathSegment@8..14
                    PathSeparator@8..10 "::"
                    Ident@10..14 "Type"
            "#]],
            expect![""],
        );

        check(
            ty,
            "super::Type",
            expect![[r#"
                PathType@0..11
                  PathSegment@0..5
                    Super@0..5 "super"
                  PathSegment@5..11
                    PathSeparator@5..7 "::"
                    Ident@7..11 "Type"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_union_with_complex_members() {
        check(
            ty,
            "nil | (Int, Bytes) | fn() -> Int",
            expect![[r#"
                UnionType@0..32
                  LiteralType@0..3
                    Nil@0..3 "nil"
                  Whitespace@3..4 " "
                  BitwiseOr@4..5 "|"
                  Whitespace@5..6 " "
                  PairType@6..18
                    OpenParen@6..7 "("
                    PathType@7..10
                      PathSegment@7..10
                        Ident@7..10 "Int"
                    Comma@10..11 ","
                    Whitespace@11..12 " "
                    PathType@12..17
                      PathSegment@12..17
                        Ident@12..17 "Bytes"
                    CloseParen@17..18 ")"
                  Whitespace@18..19 " "
                  BitwiseOr@19..20 "|"
                  Whitespace@20..21 " "
                  LambdaType@21..32
                    Fn@21..23 "fn"
                    OpenParen@23..24 "("
                    CloseParen@24..25 ")"
                    Whitespace@25..26 " "
                    Arrow@26..28 "->"
                    Whitespace@28..29 " "
                    PathType@29..32
                      PathSegment@29..32
                        Ident@29..32 "Int"
            "#]],
            expect![""],
        );
    }
}
