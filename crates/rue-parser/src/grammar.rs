use crate::{parser::Parser, ParserError, SyntaxKind};

pub fn root(p: &mut Parser) {
    p.start(SyntaxKind::Root);
    while !p.at_end() {
        item(p);
    }
    p.finish();
}

fn item(p: &mut Parser) {
    if p.peek() == SyntaxKind::Fun {
        function_item(p);
    } else {
        let peek = p.peek();
        p.error(ParserError::UnexpectedToken {
            expected: SyntaxKind::Fun,
            found: peek,
        });
    }
}

fn function_item(p: &mut Parser) {
    p.start(SyntaxKind::FunctionItem);
    p.eat(SyntaxKind::Fun);
    p.eat(SyntaxKind::Ident);
    function_params(p);
    p.eat(SyntaxKind::Arrow);
    ty(p);
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
    ty(p);
    p.finish();
}

fn block(p: &mut Parser) {
    p.start(SyntaxKind::Block);
    p.eat(SyntaxKind::OpenBrace);
    expr(p);
    p.eat(SyntaxKind::CloseBrace);
    p.finish();
}

enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Gt,
}

impl Op {
    fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Lt | Self::Gt => (1, 2),
            Self::Add | Self::Sub => (3, 4),
            Self::Mul | Self::Div => (5, 6),
        }
    }
}

fn expr(p: &mut Parser) {
    expr_binding_power(p, 0);
}

fn expr_binding_power(p: &mut Parser, minimum_binding_power: u8) {
    let checkpoint = p.checkpoint();

    match p.peek() {
        SyntaxKind::Int | SyntaxKind::Ident => {
            p.start(SyntaxKind::LiteralExpr);
            p.bump();
            p.finish();
        }
        SyntaxKind::If => {
            if_expr(p);
        }
        _ => {}
    }

    while p.peek() == SyntaxKind::OpenParen {
        p.start_at(checkpoint, SyntaxKind::FunctionCall);
        p.start(SyntaxKind::FunctionCallArgs);
        p.bump();
        while !p.at_end() {
            expr(p);
            if !p.eat(SyntaxKind::Comma) {
                break;
            }
        }
        p.eat(SyntaxKind::CloseParen);
        p.finish();
        p.finish();
    }

    loop {
        let op = match p.peek() {
            SyntaxKind::Plus => Op::Add,
            SyntaxKind::Minus => Op::Sub,
            SyntaxKind::Star => Op::Mul,
            SyntaxKind::Slash => Op::Div,
            SyntaxKind::LessThan => Op::Lt,
            SyntaxKind::GreaterThan => Op::Gt,
            _ => return,
        };

        let (left_binding_power, right_binding_power) = op.binding_power();

        if left_binding_power < minimum_binding_power {
            return;
        }

        p.bump();

        p.start_at(checkpoint, SyntaxKind::BinaryExpr);
        expr_binding_power(p, right_binding_power);
        p.finish();
    }
}

fn if_expr(p: &mut Parser) {
    p.start(SyntaxKind::IfExpr);
    p.eat(SyntaxKind::If);
    expr(p);
    block(p);
    p.eat(SyntaxKind::Else);
    block(p);
    p.finish();
}

fn ty(p: &mut Parser) {
    match p.peek() {
        SyntaxKind::Ident => {
            p.start(SyntaxKind::LiteralType);
            p.bump();
            p.finish();
        }
        SyntaxKind::Fun => {
            p.start(SyntaxKind::FunctionType);
            p.bump();
            function_type_params(p);
            p.eat(SyntaxKind::Arrow);
            ty(p);
            p.finish();
        }
        _ => {}
    }
}

fn function_type_params(p: &mut Parser) {
    p.start(SyntaxKind::FunctionTypeParams);
    p.eat(SyntaxKind::OpenParen);
    while p.peek() != SyntaxKind::CloseParen {
        ty(p);
        if !p.eat(SyntaxKind::Comma) {
            break;
        }
    }
    p.eat(SyntaxKind::CloseParen);
    p.finish();
}
