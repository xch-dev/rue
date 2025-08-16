use crate::{Parser, SyntaxKind, T, grammar::generics::generic_arguments};

pub fn ty(p: &mut Parser<'_>) {
    ty_inner(p, true);
}

fn ty_inner(p: &mut Parser<'_>, allow_union: bool) {
    let cp = p.checkpoint();

    if p.at(SyntaxKind::Ident) || p.at(T![::]) {
        path_type(p);
    } else if p.at(T!['(']) {
        p.expect(T!['(']);
        ty(p);
        if p.try_eat(T![,]) {
            p.start_at(cp, SyntaxKind::PairType);
            ty(p);
            p.expect(T![,]);
            p.expect(T![')']);
        } else {
            p.start_at(cp, SyntaxKind::GroupType);
        }
        p.finish()
    } else {
        p.skip();
        return;
    }

    if allow_union && p.at(T![|]) {
        p.start_at(cp, SyntaxKind::UnionType);
        while p.try_eat(T![|]) {
            ty_inner(p, false);
        }
        p.finish();
    }
}

fn path_type(p: &mut Parser<'_>) {
    p.start(SyntaxKind::PathType);
    path_type_segment(p, true);
    while p.at(T![::]) {
        path_type_segment(p, false);
    }
    p.finish();
}

fn path_type_segment(p: &mut Parser<'_>, first: bool) {
    p.start(SyntaxKind::PathTypeSegment);
    if first {
        p.try_eat(T![::]);
    } else {
        p.expect(T![::]);
    }
    p.expect(SyntaxKind::Ident);
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
    fn test_literal_type() {
        check(
            ty,
            "String",
            expect![[r#"
                PathType@0..6
                  PathTypeSegment@0..6
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
                    PathTypeSegment@0..7
                      Ident@0..6 "String"
                      Whitespace@6..7 " "
                  BitwiseOr@7..8 "|"
                  Whitespace@8..9 " "
                  PathType@9..12
                    PathTypeSegment@9..12
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
                    PathTypeSegment@0..7
                      Ident@0..6 "String"
                      Whitespace@6..7 " "
                  BitwiseOr@7..8 "|"
                  Whitespace@8..9 " "
                  PathType@9..13
                    PathTypeSegment@9..13
                      Ident@9..12 "Int"
                      Whitespace@12..13 " "
                  BitwiseOr@13..14 "|"
                  Whitespace@14..15 " "
                  PathType@15..19
                    PathTypeSegment@15..19
                      Ident@15..19 "Bool"
            "#]],
            expect![],
        );
    }
}
