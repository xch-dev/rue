use crate::{parser::Parser, SyntaxKind};

pub fn root(p: &mut Parser) {
    p.start(SyntaxKind::Root);
    while !p.at_end() {
        item(p);
    }
    p.finish();
}

fn item(p: &mut Parser) {
    if p.at(SyntaxKind::Fun) {
        function_item(p);
    } else {
        p.error(&[]);
    }
}

fn function_item(p: &mut Parser) {
    p.start(SyntaxKind::FunctionItem);
    p.expect(SyntaxKind::Fun);
    p.expect(SyntaxKind::Ident);
    function_params(p);
    p.expect(SyntaxKind::Arrow);
    ty(p);
    block(p);
    p.finish();
}

fn function_params(p: &mut Parser) {
    p.start(SyntaxKind::FunctionParamList);
    p.expect(SyntaxKind::OpenParen);
    while !p.at(SyntaxKind::CloseParen) {
        function_param(p);
        if !p.try_eat(SyntaxKind::Comma) {
            break;
        }
    }
    p.expect(SyntaxKind::CloseParen);
    p.finish();
}

fn function_param(p: &mut Parser) {
    p.start(SyntaxKind::FunctionParam);
    p.expect(SyntaxKind::Ident);
    p.expect(SyntaxKind::Colon);
    ty(p);
    p.finish();
}

fn block(p: &mut Parser) {
    p.start(SyntaxKind::Block);
    p.expect(SyntaxKind::OpenBrace);
    expr(p);
    p.expect(SyntaxKind::CloseBrace);
    p.finish();
}

enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Lt,
    Gt,
    Eq,
}

impl Op {
    fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Lt | Self::Gt | Self::Eq => (1, 2),
            Self::Add | Self::Sub => (3, 4),
            Self::Mul | Self::Div | Self::Rem => (5, 6),
        }
    }
}

const EXPR_RECOVERY_SET: &[SyntaxKind] = &[SyntaxKind::OpenBrace, SyntaxKind::CloseBrace];

fn expr(p: &mut Parser) {
    expr_binding_power(p, 0);
}

fn expr_binding_power(p: &mut Parser, minimum_binding_power: u8) {
    let checkpoint = p.checkpoint();

    if p.at(SyntaxKind::Int)
        || p.at(SyntaxKind::Ident)
        || p.at(SyntaxKind::True)
        || p.at(SyntaxKind::False)
        || p.at(SyntaxKind::Nil)
    {
        p.start(SyntaxKind::LiteralExpr);
        p.bump();
        p.finish();
    } else if p.at(SyntaxKind::If) {
        if_expr(p);
    } else {
        return p.error(EXPR_RECOVERY_SET);
    }

    while p.at(SyntaxKind::OpenParen) {
        p.start_at(checkpoint, SyntaxKind::FunctionCall);
        p.start(SyntaxKind::FunctionCallArgs);
        p.bump();
        while !p.at(SyntaxKind::CloseParen) {
            expr(p);
            if !p.try_eat(SyntaxKind::Comma) {
                break;
            }
        }
        p.expect(SyntaxKind::CloseParen);
        p.finish();
        p.finish();
    }

    loop {
        let op = if p.at(SyntaxKind::Plus) {
            Op::Add
        } else if p.at(SyntaxKind::Minus) {
            Op::Sub
        } else if p.at(SyntaxKind::Star) {
            Op::Mul
        } else if p.at(SyntaxKind::Slash) {
            Op::Div
        } else if p.at(SyntaxKind::Percent) {
            Op::Rem
        } else if p.at(SyntaxKind::LessThan) {
            Op::Lt
        } else if p.at(SyntaxKind::GreaterThan) {
            Op::Gt
        } else if p.at(SyntaxKind::Equals) {
            Op::Eq
        } else {
            return;
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
    p.expect(SyntaxKind::If);
    expr(p);
    block(p);
    p.expect(SyntaxKind::Else);
    block(p);
    p.finish();
}

const TYPE_RECOVERY_SET: &[SyntaxKind] = &[SyntaxKind::OpenBrace, SyntaxKind::CloseBrace];

fn ty(p: &mut Parser) {
    if p.at(SyntaxKind::Ident) {
        p.start(SyntaxKind::LiteralType);
        p.bump();
        p.finish();
    } else if p.at(SyntaxKind::Fun) {
        p.start(SyntaxKind::FunctionType);
        p.bump();
        function_type_params(p);
        p.expect(SyntaxKind::Arrow);
        ty(p);
        p.finish();
    } else {
        p.error(TYPE_RECOVERY_SET);
    }
}

fn function_type_params(p: &mut Parser) {
    p.start(SyntaxKind::FunctionTypeParams);
    p.expect(SyntaxKind::OpenParen);
    while !p.at(SyntaxKind::CloseParen) {
        ty(p);
        if !p.try_eat(SyntaxKind::Comma) {
            break;
        }
    }
    p.expect(SyntaxKind::CloseParen);
    p.finish();
}
