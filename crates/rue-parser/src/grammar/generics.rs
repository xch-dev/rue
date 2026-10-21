use crate::{
    Parser, SyntaxKind, T,
    grammar::{delimited_list_with_stops, ty::ty},
};

pub fn generic_parameters(p: &mut Parser) {
    p.start(SyntaxKind::GenericParameters);
    p.expect(T![<]);
    delimited_list_with_stops(p, T![>], &[T!['('], T![=], T!['{']], |p| {
        p.expect(SyntaxKind::Ident);
    });
    p.finish();
}

pub fn generic_arguments(p: &mut Parser) {
    p.start(SyntaxKind::GenericArguments);
    p.expect(T![<]);
    delimited_list_with_stops(p, T![>], &[T!['('], T![=], T!['{'], T![->]], ty);
    p.finish();
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::grammar::tests::check;

    use super::*;

    #[test]
    fn test_generic_parameters() {
        check(
            generic_parameters,
            "<>",
            expect![[r#"
                GenericParameters@0..2
                  LessThan@0..1 "<"
                  GreaterThan@1..2 ">"
            "#]],
            expect![""],
        );

        check(
            generic_parameters,
            "<T>",
            expect![[r#"
                GenericParameters@0..3
                  LessThan@0..1 "<"
                  Ident@1..2 "T"
                  GreaterThan@2..3 ">"
            "#]],
            expect![""],
        );

        check(
            generic_parameters,
            "<A, B,>",
            expect![[r#"
                GenericParameters@0..7
                  LessThan@0..1 "<"
                  Ident@1..2 "A"
                  Comma@2..3 ","
                  Whitespace@3..4 " "
                  Ident@4..5 "B"
                  Comma@5..6 ","
                  GreaterThan@6..7 ">"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_generic_arguments() {
        check(
            generic_arguments,
            "<Int>",
            expect![[r#"
                GenericArguments@0..5
                  LessThan@0..1 "<"
                  PathType@1..4
                    PathSegment@1..4
                      Ident@1..4 "Int"
                  GreaterThan@4..5 ">"
            "#]],
            expect![""],
        );

        check(
            generic_arguments,
            "<Int, List<Bytes>>",
            expect![[r#"
                GenericArguments@0..18
                  LessThan@0..1 "<"
                  PathType@1..4
                    PathSegment@1..4
                      Ident@1..4 "Int"
                  Comma@4..5 ","
                  Whitespace@5..6 " "
                  PathType@6..17
                    PathSegment@6..17
                      Ident@6..10 "List"
                      GenericArguments@10..17
                        LessThan@10..11 "<"
                        PathType@11..16
                          PathSegment@11..16
                            Ident@11..16 "Bytes"
                        GreaterThan@16..17 ">"
                  GreaterThan@17..18 ">"
            "#]],
            expect![""],
        );

        check(
            generic_arguments,
            "<(Int, Int)>",
            expect![[r#"
                GenericArguments@0..12
                  LessThan@0..1 "<"
                  PairType@1..11
                    OpenParen@1..2 "("
                    PathType@2..5
                      PathSegment@2..5
                        Ident@2..5 "Int"
                    Comma@5..6 ","
                    Whitespace@6..7 " "
                    PathType@7..10
                      PathSegment@7..10
                        Ident@7..10 "Int"
                    CloseParen@10..11 ")"
                  GreaterThan@11..12 ">"
            "#]],
            expect![""],
        );
    }
}
