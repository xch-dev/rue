use crate::{parser::Parser, SyntaxKind};

pub fn root(p: &mut Parser) {
    p.start(SyntaxKind::Root);
    while !p.at_end() {
        function_item(p);
    }
    p.finish();
}

fn function_item(p: &mut Parser) {
    p.start(SyntaxKind::FunctionItem);
    p.eat(SyntaxKind::Fun);
    p.eat(SyntaxKind::Ident);
    function_params(p);
    p.eat(SyntaxKind::Arrow);
    p.eat(SyntaxKind::Ident);
    block(p);
    p.finish();
}

fn function_params(p: &mut Parser) {
    p.start(SyntaxKind::FunctionParamList);
    p.eat(SyntaxKind::OpenParen);
    while p.peek() != SyntaxKind::CloseParen {
        function_param(p);
        if !p.eat(SyntaxKind::Comma) {
            break;
        }
    }
    p.eat(SyntaxKind::CloseParen);
    p.finish();
}

fn function_param(p: &mut Parser) {
    p.start(SyntaxKind::FunctionParam);
    p.eat(SyntaxKind::Ident);
    p.eat(SyntaxKind::Colon);
    p.eat(SyntaxKind::Ident);
    p.finish();
}

fn block(p: &mut Parser) {
    p.start(SyntaxKind::Block);
    p.eat(SyntaxKind::OpenBrace);
    p.eat(SyntaxKind::Int);
    p.eat(SyntaxKind::CloseBrace);
    p.finish();
}
