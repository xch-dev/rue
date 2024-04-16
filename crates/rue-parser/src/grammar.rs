use crate::{parser::Parser, BinaryOp, SyntaxKind};

pub fn root(p: &mut Parser) {
    p.start(SyntaxKind::Root);
    while !p.at(SyntaxKind::Eof) {
        item(p);
    }
    p.finish();
}

fn item(p: &mut Parser) {
    if p.at(SyntaxKind::Fun) {
        function_item(p);
    } else if p.at(SyntaxKind::Type) {
        type_alias_item(p);
    } else if p.at(SyntaxKind::Struct) {
        struct_item(p);
    } else if p.at(SyntaxKind::Enum) {
        enum_item(p);
    } else if p.at(SyntaxKind::Const) {
        const_item(p);
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
    p.expect(SyntaxKind::OpenParen);
    while !p.at(SyntaxKind::CloseParen) {
        function_param(p);
        if !p.try_eat(SyntaxKind::Comma) {
            break;
        }
    }
    p.expect(SyntaxKind::CloseParen);
}

fn function_param(p: &mut Parser) {
    p.start(SyntaxKind::FunctionParam);
    p.try_eat(SyntaxKind::Spread);
    p.expect(SyntaxKind::Ident);
    p.expect(SyntaxKind::Colon);
    ty(p);
    p.finish();
}

fn type_alias_item(p: &mut Parser) {
    p.start(SyntaxKind::TypeAliasItem);
    p.expect(SyntaxKind::Type);
    p.expect(SyntaxKind::Ident);
    p.expect(SyntaxKind::Assign);
    ty(p);
    p.expect(SyntaxKind::Semicolon);
    p.finish();
}

fn struct_item(p: &mut Parser) {
    p.start(SyntaxKind::StructItem);
    p.expect(SyntaxKind::Struct);
    p.expect(SyntaxKind::Ident);
    p.expect(SyntaxKind::OpenBrace);
    while !p.at(SyntaxKind::CloseBrace) {
        struct_field(p);
        if !p.try_eat(SyntaxKind::Comma) {
            break;
        }
    }
    p.expect(SyntaxKind::CloseBrace);
    p.finish();
}

fn struct_field(p: &mut Parser) {
    p.start(SyntaxKind::StructField);
    p.expect(SyntaxKind::Ident);
    p.expect(SyntaxKind::Colon);
    ty(p);
    p.finish();
}

fn enum_item(p: &mut Parser) {
    p.start(SyntaxKind::EnumItem);
    p.expect(SyntaxKind::Enum);
    p.expect(SyntaxKind::Ident);
    p.expect(SyntaxKind::OpenBrace);
    while !p.at(SyntaxKind::CloseBrace) {
        enum_variant(p);
        if !p.try_eat(SyntaxKind::Comma) {
            break;
        }
    }
    p.expect(SyntaxKind::CloseBrace);
    p.finish();
}

fn enum_variant(p: &mut Parser) {
    p.start(SyntaxKind::EnumVariant);
    p.expect(SyntaxKind::Ident);
    p.expect(SyntaxKind::Assign);
    p.expect(SyntaxKind::Int);
    if p.try_eat(SyntaxKind::OpenBrace) {
        while !p.at(SyntaxKind::CloseBrace) {
            struct_field(p);
            if !p.try_eat(SyntaxKind::Comma) {
                break;
            }
        }
        p.expect(SyntaxKind::CloseBrace);
    }
    p.finish();
}

fn const_item(p: &mut Parser) {
    p.start(SyntaxKind::ConstItem);
    p.expect(SyntaxKind::Const);
    p.expect(SyntaxKind::Ident);
    p.expect(SyntaxKind::Colon);
    ty(p);
    p.expect(SyntaxKind::Assign);
    expr(p);
    p.expect(SyntaxKind::Semicolon);
    p.finish();
}

fn block(p: &mut Parser) {
    p.start(SyntaxKind::Block);
    p.expect(SyntaxKind::OpenBrace);
    while !p.at(SyntaxKind::CloseBrace) && !p.at(SyntaxKind::Eof) {
        if p.at(SyntaxKind::Let) {
            let_stmt(p);
        } else if p.at(SyntaxKind::Return) {
            return_stmt(p);
        } else if p.at(SyntaxKind::Raise) {
            raise_stmt(p);
        } else if p.at(SyntaxKind::If) {
            if if_stmt_maybe_else(p, false) {
                break;
            }
        } else if p.at(SyntaxKind::Assert) {
            assert_stmt(p);
        } else if p.at(SyntaxKind::Fun) || p.at(SyntaxKind::Type) || p.at(SyntaxKind::Const) {
            item(p);
        } else {
            expr(p);
            break;
        }
    }
    p.expect(SyntaxKind::CloseBrace);
    p.finish();
}

fn let_stmt(p: &mut Parser) {
    p.start(SyntaxKind::LetStmt);
    p.expect(SyntaxKind::Let);
    p.expect(SyntaxKind::Ident);
    if p.try_eat(SyntaxKind::Colon) {
        ty(p);
    }
    p.expect(SyntaxKind::Assign);
    expr(p);
    p.expect(SyntaxKind::Semicolon);
    p.finish();
}

fn if_stmt_maybe_else(p: &mut Parser, expr_only: bool) -> bool {
    let cp = p.checkpoint();
    p.expect(SyntaxKind::If);
    expr(p);
    block(p);
    let mut has_else = false;
    if expr_only || p.at(SyntaxKind::Else) {
        p.start_at(cp, SyntaxKind::IfExpr);
        p.expect(SyntaxKind::Else);
        block(p);
        has_else = true;
    } else {
        p.start_at(cp, SyntaxKind::IfStmt);
    }
    p.finish();
    has_else
}

fn return_stmt(p: &mut Parser) {
    p.start(SyntaxKind::ReturnStmt);
    p.expect(SyntaxKind::Return);
    expr(p);
    p.expect(SyntaxKind::Semicolon);
    p.finish();
}

fn raise_stmt(p: &mut Parser) {
    p.start(SyntaxKind::RaiseStmt);
    p.expect(SyntaxKind::Raise);
    if !p.try_eat(SyntaxKind::Semicolon) {
        expr(p);
        p.expect(SyntaxKind::Semicolon);
    }
    p.finish();
}

fn assert_stmt(p: &mut Parser) {
    p.start(SyntaxKind::AssertStmt);
    p.expect(SyntaxKind::Assert);
    expr(p);
    p.expect(SyntaxKind::Semicolon);
    p.finish();
}

fn path(p: &mut Parser) {
    p.start(SyntaxKind::Path);
    p.expect(SyntaxKind::Ident);
    while p.try_eat(SyntaxKind::PathSeparator) {
        p.expect(SyntaxKind::Ident);
    }
    p.finish();
}

fn binding_power(op: BinaryOp) -> (u8, u8) {
    match op {
        BinaryOp::LessThan
        | BinaryOp::GreaterThan
        | BinaryOp::LessThanEquals
        | BinaryOp::GreaterThanEquals
        | BinaryOp::Equals
        | BinaryOp::NotEquals => (1, 2),
        BinaryOp::Add | BinaryOp::Subtract => (3, 4),
        BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Remainder => (5, 6),
    }
}

const EXPR_RECOVERY_SET: &[SyntaxKind] = &[SyntaxKind::OpenBrace, SyntaxKind::CloseBrace];

fn expr(p: &mut Parser) {
    expr_binding_power(p, 0);
}

fn expr_binding_power(p: &mut Parser, minimum_binding_power: u8) {
    if p.at(SyntaxKind::Not) {
        p.start(SyntaxKind::PrefixExpr);
        p.bump();
        expr_binding_power(p, 255);
        p.finish();
        return;
    }

    let checkpoint = p.checkpoint();

    if p.at(SyntaxKind::Int)
        || p.at(SyntaxKind::String)
        || p.at(SyntaxKind::True)
        || p.at(SyntaxKind::False)
        || p.at(SyntaxKind::Nil)
    {
        p.start(SyntaxKind::LiteralExpr);
        p.bump();
        p.finish();
    } else if p.at(SyntaxKind::Ident) {
        path(p);

        if p.at(SyntaxKind::OpenBrace) {
            p.start_at(checkpoint, SyntaxKind::InitializerExpr);
            p.bump();
            while !p.at(SyntaxKind::CloseBrace) {
                p.start(SyntaxKind::InitializerField);
                p.expect(SyntaxKind::Ident);
                if p.try_eat(SyntaxKind::Colon) {
                    expr(p);
                }
                p.finish();
                if !p.try_eat(SyntaxKind::Comma) {
                    break;
                }
            }
            p.expect(SyntaxKind::CloseBrace);
            p.finish();
        }
    } else if p.at(SyntaxKind::OpenBracket) {
        list_expr(p);
    } else if p.at(SyntaxKind::OpenBrace) {
        block(p);
    } else if p.at(SyntaxKind::OpenParen) {
        p.bump();
        expr(p);
        if p.try_eat(SyntaxKind::Comma) {
            p.start_at(checkpoint, SyntaxKind::PairExpr);
            expr(p);
        } else {
            p.start_at(checkpoint, SyntaxKind::GroupExpr);
        }
        p.expect(SyntaxKind::CloseParen);
        p.finish();
    } else if p.at(SyntaxKind::If) {
        if_stmt_maybe_else(p, true);
    } else if p.at(SyntaxKind::Fun) {
        lambda_expr(p);
    } else {
        return p.error(EXPR_RECOVERY_SET);
    }

    loop {
        if p.at(SyntaxKind::OpenParen) {
            p.start_at(checkpoint, SyntaxKind::FunctionCall);
            p.bump();
            while !p.at(SyntaxKind::CloseParen) {
                function_call_arg(p);
                if !p.try_eat(SyntaxKind::Comma) {
                    break;
                }
            }
            p.expect(SyntaxKind::CloseParen);
            p.finish();
        } else if p.at(SyntaxKind::Dot) {
            p.start_at(checkpoint, SyntaxKind::FieldAccess);
            p.bump();
            p.expect(SyntaxKind::Ident);
            p.finish();
        } else if p.at(SyntaxKind::OpenBracket) {
            p.start_at(checkpoint, SyntaxKind::IndexAccess);
            p.bump();
            p.expect(SyntaxKind::Int);
            p.expect(SyntaxKind::CloseBracket);
            p.finish();
        } else if p.at(SyntaxKind::As) {
            p.start_at(checkpoint, SyntaxKind::CastExpr);
            p.bump();
            ty(p);
            p.finish();
        } else if p.at(SyntaxKind::Is) {
            p.start_at(checkpoint, SyntaxKind::GuardExpr);
            p.bump();
            ty(p);
            p.finish();
        } else {
            break;
        }
    }

    loop {
        let op = if p.at(SyntaxKind::Plus) {
            BinaryOp::Add
        } else if p.at(SyntaxKind::Minus) {
            BinaryOp::Subtract
        } else if p.at(SyntaxKind::Star) {
            BinaryOp::Multiply
        } else if p.at(SyntaxKind::Slash) {
            BinaryOp::Divide
        } else if p.at(SyntaxKind::Percent) {
            BinaryOp::Remainder
        } else if p.at(SyntaxKind::LessThan) {
            BinaryOp::LessThan
        } else if p.at(SyntaxKind::GreaterThan) {
            BinaryOp::GreaterThan
        } else if p.at(SyntaxKind::LessThanEquals) {
            BinaryOp::LessThanEquals
        } else if p.at(SyntaxKind::GreaterThanEquals) {
            BinaryOp::GreaterThanEquals
        } else if p.at(SyntaxKind::Equals) {
            BinaryOp::Equals
        } else if p.at(SyntaxKind::NotEquals) {
            BinaryOp::NotEquals
        } else {
            return;
        };

        let (left_binding_power, right_binding_power) = binding_power(op);

        if left_binding_power < minimum_binding_power {
            return;
        }

        p.bump();

        p.start_at(checkpoint, SyntaxKind::BinaryExpr);
        expr_binding_power(p, right_binding_power);
        p.finish();
    }
}

fn function_call_arg(p: &mut Parser) {
    p.start(SyntaxKind::FunctionCallArg);
    p.try_eat(SyntaxKind::Spread);
    expr(p);
    p.finish();
}

fn list_expr(p: &mut Parser) {
    p.start(SyntaxKind::ListExpr);
    p.expect(SyntaxKind::OpenBracket);
    while !p.at(SyntaxKind::CloseBracket) {
        p.start(SyntaxKind::ListItem);
        p.try_eat(SyntaxKind::Spread);
        expr(p);
        p.finish();
        if !p.try_eat(SyntaxKind::Comma) {
            break;
        }
    }
    p.expect(SyntaxKind::CloseBracket);
    p.finish();
}

fn lambda_expr(p: &mut Parser) {
    p.start(SyntaxKind::LambdaExpr);
    p.expect(SyntaxKind::Fun);
    p.expect(SyntaxKind::OpenParen);
    while !p.at(SyntaxKind::CloseParen) {
        lambda_param(p);
        if !p.try_eat(SyntaxKind::Comma) {
            break;
        }
    }
    p.expect(SyntaxKind::CloseParen);
    if p.try_eat(SyntaxKind::Colon) {
        ty(p);
    }
    p.expect(SyntaxKind::FatArrow);
    expr(p);
    p.finish();
}

fn lambda_param(p: &mut Parser) {
    p.start(SyntaxKind::LambdaParam);
    p.try_eat(SyntaxKind::Spread);
    p.expect(SyntaxKind::Ident);
    if p.try_eat(SyntaxKind::Colon) {
        ty(p);
    }
    p.finish();
}

const TYPE_RECOVERY_SET: &[SyntaxKind] = &[SyntaxKind::OpenBrace, SyntaxKind::CloseBrace];

fn ty(p: &mut Parser) {
    let checkpoint = p.checkpoint();

    if p.at(SyntaxKind::Ident) {
        path(p);
    } else if p.at(SyntaxKind::Fun) {
        p.start(SyntaxKind::FunctionType);
        p.bump();
        p.expect(SyntaxKind::OpenParen);
        while !p.at(SyntaxKind::CloseParen) {
            function_type_param(p);
            if !p.try_eat(SyntaxKind::Comma) {
                break;
            }
        }
        p.expect(SyntaxKind::CloseParen);
        p.expect(SyntaxKind::Arrow);
        ty(p);
        p.finish();
    } else if p.at(SyntaxKind::OpenParen) {
        p.start(SyntaxKind::PairType);
        p.bump();
        ty(p);
        p.expect(SyntaxKind::Comma);
        ty(p);
        p.try_eat(SyntaxKind::Comma);
        p.expect(SyntaxKind::CloseParen);
        p.finish();
    } else {
        return p.error(TYPE_RECOVERY_SET);
    }

    while p.at(SyntaxKind::OpenBracket) {
        p.start_at(checkpoint, SyntaxKind::ListType);
        p.bump();
        p.expect(SyntaxKind::CloseBracket);
        p.finish();
    }
}

fn function_type_param(p: &mut Parser) {
    p.start(SyntaxKind::FunctionTypeParam);
    p.try_eat(SyntaxKind::Spread);
    ty(p);
    p.finish();
}
