use crate::{
    Parser, SyntaxKind, T,
    grammar::{block::block, ty::ty},
};

pub fn item(p: &mut Parser<'_>) {
    if p.at(T![fn]) {
        function(p);
    } else {
        p.skip();
    }
}

fn function(p: &mut Parser<'_>) {
    p.start(SyntaxKind::Function);
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
    p.expect(T![:]);
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

fn generic_parameters(p: &mut Parser<'_>) {
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
