use crate::{
    Parser, SyntaxKind, T,
    grammar::{delimited_list, recover, skip_unexpected},
};

pub fn binding(p: &mut Parser) {
    if p.at_quiet(SyntaxKind::Ident) {
        p.start(SyntaxKind::NamedBinding);
        p.expect(SyntaxKind::Ident);
        p.finish();
    } else if p.at_quiet(T!['(']) {
        p.start(SyntaxKind::PairBinding);
        p.expect(T!['(']);
        binding(p);
        if !p.at(T![,]) {
            skip_unexpected(p);
        }
        if p.try_eat(T![,]) {
            binding(p);
            p.try_eat(T![,]);
            if !p.at(T![')']) {
                skip_unexpected(p);
            }
        }
        p.expect(T![')']);
        p.finish();
    } else if p.at_quiet(T!['[']) {
        p.start(SyntaxKind::ListBinding);
        p.expect(T!['[']);
        delimited_list(p, T![']'], |p| {
            p.start(SyntaxKind::ListBindingItem);
            p.try_eat(T![...]);
            binding(p);
            p.finish();
        });
        p.finish();
    } else if p.at_quiet(T!['{'])
        && matches!(
            p.lookahead(1),
            SyntaxKind::Ident | T!['}'] | T![.] | SyntaxKind::Eof
        )
    {
        // Only treat this as a struct binding if it looks like one, since a missing closing
        // parenthesis in a function's parameters would otherwise treat its body as a binding
        p.start(SyntaxKind::StructBinding);
        p.expect(T!['{']);
        delimited_list(p, T!['}'], |p| {
            p.start(SyntaxKind::StructFieldBinding);
            let spread = p.try_eat(T![...]);
            p.expect(SyntaxKind::Ident);
            if p.try_eat(T![:]) && !spread {
                binding(p);
            }
            p.finish();
        });
        p.finish();
    } else {
        recover(p, "binding", &[T![=], T![:], T!['{']]);
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::grammar::tests::check;

    use super::*;

    #[test]
    fn test_named_binding() {
        check(
            binding,
            "value",
            expect![[r#"
                NamedBinding@0..5
                  Ident@0..5 "value"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_pair_binding() {
        check(
            binding,
            "(a, b)",
            expect![[r#"
                PairBinding@0..6
                  OpenParen@0..1 "("
                  NamedBinding@1..2
                    Ident@1..2 "a"
                  Comma@2..3 ","
                  Whitespace@3..4 " "
                  NamedBinding@4..5
                    Ident@4..5 "b"
                  CloseParen@5..6 ")"
            "#]],
            expect![""],
        );

        check(
            binding,
            "(a, b,)",
            expect![[r#"
                PairBinding@0..7
                  OpenParen@0..1 "("
                  NamedBinding@1..2
                    Ident@1..2 "a"
                  Comma@2..3 ","
                  Whitespace@3..4 " "
                  NamedBinding@4..5
                    Ident@4..5 "b"
                  Comma@5..6 ","
                  CloseParen@6..7 ")"
            "#]],
            expect![""],
        );

        check(
            binding,
            "((a, b), c)",
            expect![[r#"
                PairBinding@0..11
                  OpenParen@0..1 "("
                  PairBinding@1..7
                    OpenParen@1..2 "("
                    NamedBinding@2..3
                      Ident@2..3 "a"
                    Comma@3..4 ","
                    Whitespace@4..5 " "
                    NamedBinding@5..6
                      Ident@5..6 "b"
                    CloseParen@6..7 ")"
                  Comma@7..8 ","
                  Whitespace@8..9 " "
                  NamedBinding@9..10
                    Ident@9..10 "c"
                  CloseParen@10..11 ")"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_list_binding() {
        check(
            binding,
            "[]",
            expect![[r#"
                ListBinding@0..2
                  OpenBracket@0..1 "["
                  CloseBracket@1..2 "]"
            "#]],
            expect![""],
        );

        check(
            binding,
            "[a, b]",
            expect![[r#"
                ListBinding@0..6
                  OpenBracket@0..1 "["
                  ListBindingItem@1..2
                    NamedBinding@1..2
                      Ident@1..2 "a"
                  Comma@2..3 ","
                  Whitespace@3..4 " "
                  ListBindingItem@4..5
                    NamedBinding@4..5
                      Ident@4..5 "b"
                  CloseBracket@5..6 "]"
            "#]],
            expect![""],
        );

        check(
            binding,
            "[first, ...rest]",
            expect![[r#"
                ListBinding@0..16
                  OpenBracket@0..1 "["
                  ListBindingItem@1..6
                    NamedBinding@1..6
                      Ident@1..6 "first"
                  Comma@6..7 ","
                  Whitespace@7..8 " "
                  ListBindingItem@8..15
                    Spread@8..11 "..."
                    NamedBinding@11..15
                      Ident@11..15 "rest"
                  CloseBracket@15..16 "]"
            "#]],
            expect![""],
        );
    }

    #[test]
    fn test_struct_binding() {
        check(
            binding,
            "{}",
            expect![[r#"
                StructBinding@0..2
                  OpenBrace@0..1 "{"
                  CloseBrace@1..2 "}"
            "#]],
            expect![""],
        );

        check(
            binding,
            "{ x, y }",
            expect![[r#"
                StructBinding@0..8
                  OpenBrace@0..1 "{"
                  Whitespace@1..2 " "
                  StructFieldBinding@2..3
                    Ident@2..3 "x"
                  Comma@3..4 ","
                  Whitespace@4..5 " "
                  StructFieldBinding@5..7
                    Ident@5..6 "y"
                    Whitespace@6..7 " "
                  CloseBrace@7..8 "}"
            "#]],
            expect![""],
        );

        check(
            binding,
            "{ x: renamed, ...rest }",
            expect![[r#"
                StructBinding@0..23
                  OpenBrace@0..1 "{"
                  Whitespace@1..2 " "
                  StructFieldBinding@2..12
                    Ident@2..3 "x"
                    Colon@3..4 ":"
                    Whitespace@4..5 " "
                    NamedBinding@5..12
                      Ident@5..12 "renamed"
                  Comma@12..13 ","
                  Whitespace@13..14 " "
                  StructFieldBinding@14..22
                    Spread@14..17 "..."
                    Ident@17..21 "rest"
                    Whitespace@21..22 " "
                  CloseBrace@22..23 "}"
            "#]],
            expect![""],
        );

        check(
            binding,
            "{ inner: { x } }",
            expect![[r#"
                StructBinding@0..16
                  OpenBrace@0..1 "{"
                  Whitespace@1..2 " "
                  StructFieldBinding@2..14
                    Ident@2..7 "inner"
                    Colon@7..8 ":"
                    Whitespace@8..9 " "
                    StructBinding@9..14
                      OpenBrace@9..10 "{"
                      Whitespace@10..11 " "
                      StructFieldBinding@11..13
                        Ident@11..12 "x"
                        Whitespace@12..13 " "
                      CloseBrace@13..14 "}"
                  Whitespace@14..15 " "
                  CloseBrace@15..16 "}"
            "#]],
            expect![""],
        );
    }
}
