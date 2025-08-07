use crate::{Parser, SyntaxKind};

pub fn ty(p: &mut Parser<'_>) {
    p.start(SyntaxKind::LiteralType);
    p.expect(SyntaxKind::Ident);
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
                LiteralType@0..6
                  Ident@0..6 "String"
            "#]],
            expect![],
        );
    }
}
