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

pub fn generic_arguments(p: &mut Parser<'_>) {
    p.start(SyntaxKind::GenericArguments);
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
