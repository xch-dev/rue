use crate::{
    Parser, SyntaxKind, T,
    grammar::generics::{generic_arguments, generic_parameters},
};

pub fn ty(p: &mut Parser) {
    ty_inner(p, true);
}

fn ty_inner(p: &mut Parser, allow_union: bool) {
    let cp = p.checkpoint();

    if p.at(SyntaxKind::Ident) || p.at(T![::]) {
        path_type(p);
    } else if let Some(kind) = p.at_any(SyntaxKind::LITERAL) {
        p.start(SyntaxKind::LiteralType);
        p.expect(kind);
        p.finish();
    } else if p.at(T!['(']) {
        p.expect(T!['(']);
        ty(p);
        if p.try_eat(T![,]) {
            p.start_at(cp, SyntaxKind::PairType);
            ty(p);
            p.try_eat(T![,]);
        } else {
            p.start_at(cp, SyntaxKind::GroupType);
        }
        p.expect(T![')']);
        p.finish();
    } else if p.at(T!['[']) {
        p.start_at(cp, SyntaxKind::ListType);
        p.expect(T!['[']);
        while !p.at(T![']']) {
            p.start(SyntaxKind::ListTypeItem);
            p.try_eat(T![...]);
            ty(p);
            p.finish();
            if !p.try_eat(T![,]) {
                break;
            }
        }
        p.expect(T![']']);
        p.finish();
    } else if p.at(T![fn]) {
        p.start(SyntaxKind::LambdaType);
        p.expect(T![fn]);
        if p.at(T![<]) {
            generic_parameters(p);
        }
        p.expect(T!['(']);
        while !p.at(T![')']) {
            p.start(SyntaxKind::LambdaParameter);
            p.try_eat(T![...]);
            p.expect(SyntaxKind::Ident);
            p.expect(T![:]);
            ty(p);
            p.finish();
            if !p.try_eat(T![,]) {
                break;
            }
        }
        p.expect(T![')']);
        p.expect(T![->]);
        ty(p);
        p.finish();
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
}
