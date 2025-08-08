use crate::{Parser, SyntaxKind, T, grammar::ty::ty};

pub fn generic_parameters(p: &mut Parser<'_>) {
    p.start(SyntaxKind::GenericParameters);
    p.expect(T![<]);
    while !p.at(T![>]) {
        p.expect(SyntaxKind::Ident);
        if !p.try_eat(T![,]) {
            break;
        }
    }
    p.expect(T![>]);
    p.finish();
}

pub fn subtype_generic_parameters(p: &mut Parser<'_>) {
    p.start(SyntaxKind::SubtypeGenericParameters);
    p.expect(T![<]);
    while !p.at(T![>]) {
        p.start(SyntaxKind::SubtypeGenericParameter);
        p.expect(SyntaxKind::Ident);
        p.expect(T![=]);
        p.expect(SyntaxKind::Ident);
        p.finish();
        if !p.try_eat(T![,]) {
            break;
        }
    }
    p.expect(T![>]);
    p.finish();
}

pub fn generic_arguments(p: &mut Parser<'_>) {
    p.start(SyntaxKind::GenericParameters);
    p.expect(T![<]);
    while !p.at(T![>]) {
        ty(p);
        if !p.try_eat(T![,]) {
            break;
        }
    }
    p.expect(T![>]);
    p.finish();
}
