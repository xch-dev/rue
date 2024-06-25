use rowan::Checkpoint;

use crate::{parser::Parser, BinaryOp, SyntaxKind};

pub fn root(p: &mut Parser<'_>) {
    p.start(SyntaxKind::Root);
    while !p.at(SyntaxKind::Eof) {
        item(p);
    }
    p.finish();
}

fn at_item(p: &mut Parser<'_>) -> bool {
    p.at(SyntaxKind::Mod)
        || p.at(SyntaxKind::Fun)
        || p.at(SyntaxKind::Type)
        || p.at(SyntaxKind::Struct)
        || p.at(SyntaxKind::Enum)
        || p.at(SyntaxKind::Const)
        || p.at(SyntaxKind::Import)
        || p.at(SyntaxKind::Export)
        || p.at(SyntaxKind::Inline)
}

fn item(p: &mut Parser<'_>) {
    let cp = p.checkpoint();
    p.try_eat(SyntaxKind::Export);
    let inline = p.try_eat(SyntaxKind::Inline);

    if p.at(SyntaxKind::Fun) {
        function_item(p, cp);
    } else if p.at(SyntaxKind::Const) {
        const_item(p, cp);
    } else if !inline {
        if p.at(SyntaxKind::Mod) {
            module_item(p, cp);
        } else if p.at(SyntaxKind::Type) {
            type_alias_item(p, cp);
        } else if p.at(SyntaxKind::Struct) {
            struct_item(p, cp);
        } else if p.at(SyntaxKind::Enum) {
            enum_item(p, cp);
        } else if p.at(SyntaxKind::Import) {
            import_item(p, cp);
        } else {
            p.error(&[]);
        }
    } else {
        p.error(&[]);
    }
}

fn module_item(p: &mut Parser<'_>, cp: Checkpoint) {
    p.start_at(cp, SyntaxKind::ModuleItem);
    p.expect(SyntaxKind::Mod);
    p.expect(SyntaxKind::Ident);
    p.expect(SyntaxKind::OpenBrace);
    while !p.at(SyntaxKind::CloseBrace) {
        item(p);
    }
    p.expect(SyntaxKind::CloseBrace);
    p.finish();
}

fn function_item(p: &mut Parser<'_>, cp: Checkpoint) {
    p.start_at(cp, SyntaxKind::FunctionItem);
    p.expect(SyntaxKind::Fun);
    p.expect(SyntaxKind::Ident);
    if p.at(SyntaxKind::LessThan) {
        generic_types(p);
    }
    function_params(p);
    p.expect(SyntaxKind::Arrow);
    ty(p);
    block(p);
    p.finish();
}

fn function_params(p: &mut Parser<'_>) {
    p.expect(SyntaxKind::OpenParen);
    while !p.at(SyntaxKind::CloseParen) {
        function_param(p);
        if !p.try_eat(SyntaxKind::Comma) {
            break;
        }
    }
    p.expect(SyntaxKind::CloseParen);
}

fn function_param(p: &mut Parser<'_>) {
    p.start(SyntaxKind::FunctionParam);
    p.try_eat(SyntaxKind::Spread);
    p.expect(SyntaxKind::Ident);
    p.try_eat(SyntaxKind::Question);
    p.expect(SyntaxKind::Colon);
    ty(p);
    p.finish();
}

fn type_alias_item(p: &mut Parser<'_>, cp: Checkpoint) {
    p.start_at(cp, SyntaxKind::TypeAliasItem);
    p.expect(SyntaxKind::Type);
    p.expect(SyntaxKind::Ident);
    p.expect(SyntaxKind::Assign);
    ty(p);
    p.expect(SyntaxKind::Semicolon);
    p.finish();
}

fn struct_item(p: &mut Parser<'_>, cp: Checkpoint) {
    p.start_at(cp, SyntaxKind::StructItem);
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

fn struct_field(p: &mut Parser<'_>) {
    p.start(SyntaxKind::StructField);
    p.expect(SyntaxKind::Ident);
    p.expect(SyntaxKind::Colon);
    ty(p);
    p.finish();
}

fn enum_item(p: &mut Parser<'_>, cp: Checkpoint) {
    p.start_at(cp, SyntaxKind::EnumItem);
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

fn enum_variant(p: &mut Parser<'_>) {
    p.start(SyntaxKind::EnumVariant);
    p.expect(SyntaxKind::Ident);
    if p.try_eat(SyntaxKind::Assign) {
        p.expect(SyntaxKind::Int);
    }
    if p.at(SyntaxKind::OpenBrace) {
        p.start(SyntaxKind::EnumVariantFields);
        p.bump();
        while !p.at(SyntaxKind::CloseBrace) {
            struct_field(p);
            if !p.try_eat(SyntaxKind::Comma) {
                break;
            }
        }
        p.expect(SyntaxKind::CloseBrace);
        p.finish();
    }
    p.finish();
}

fn const_item(p: &mut Parser<'_>, cp: Checkpoint) {
    p.start_at(cp, SyntaxKind::ConstItem);
    p.expect(SyntaxKind::Const);
    p.expect(SyntaxKind::Ident);
    p.expect(SyntaxKind::Colon);
    ty(p);
    p.expect(SyntaxKind::Assign);
    expr(p);
    p.expect(SyntaxKind::Semicolon);
    p.finish();
}

fn import_item(p: &mut Parser<'_>, cp: Checkpoint) {
    p.start_at(cp, SyntaxKind::ImportItem);
    p.expect(SyntaxKind::Import);
    import_path(p);
    p.expect(SyntaxKind::Semicolon);
    p.finish();
}

fn import_path(p: &mut Parser<'_>) {
    p.start(SyntaxKind::ImportPath);
    p.expect(SyntaxKind::Ident);
    while p.try_eat(SyntaxKind::PathSeparator) {
        if p.at(SyntaxKind::Ident) {
            p.expect(SyntaxKind::Ident);
        } else if p.at(SyntaxKind::OpenBrace) {
            import_group(p);
            break;
        } else {
            p.error(&[]);
            break;
        }
    }
    p.finish();
}

fn import_group(p: &mut Parser<'_>) {
    p.start(SyntaxKind::ImportGroup);
    p.expect(SyntaxKind::OpenBrace);
    while !p.at(SyntaxKind::CloseBrace) {
        import_path(p);
        if !p.try_eat(SyntaxKind::Comma) {
            break;
        }
    }
    p.expect(SyntaxKind::CloseBrace);
    p.finish();
}

fn block(p: &mut Parser<'_>) {
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
        } else if p.at(SyntaxKind::Assume) {
            assume_stmt(p);
        } else if at_item(p) {
            item(p);
        } else {
            expr(p);
            break;
        }
    }
    p.expect(SyntaxKind::CloseBrace);
    p.finish();
}

fn let_stmt(p: &mut Parser<'_>) {
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

fn if_stmt_maybe_else(p: &mut Parser<'_>, expr_only: bool) -> bool {
    let cp = p.checkpoint();
    p.expect(SyntaxKind::If);
    expr_binding_power(p, 0, false);
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

fn return_stmt(p: &mut Parser<'_>) {
    p.start(SyntaxKind::ReturnStmt);
    p.expect(SyntaxKind::Return);
    expr(p);
    p.expect(SyntaxKind::Semicolon);
    p.finish();
}

fn raise_stmt(p: &mut Parser<'_>) {
    p.start(SyntaxKind::RaiseStmt);
    p.expect(SyntaxKind::Raise);
    if !p.try_eat(SyntaxKind::Semicolon) {
        expr(p);
        p.expect(SyntaxKind::Semicolon);
    }
    p.finish();
}

fn assert_stmt(p: &mut Parser<'_>) {
    p.start(SyntaxKind::AssertStmt);
    p.expect(SyntaxKind::Assert);
    expr(p);
    p.expect(SyntaxKind::Semicolon);
    p.finish();
}

fn assume_stmt(p: &mut Parser<'_>) {
    p.start(SyntaxKind::AssumeStmt);
    p.expect(SyntaxKind::Assume);
    expr(p);
    p.expect(SyntaxKind::Semicolon);
    p.finish();
}

fn binding_power(op: BinaryOp) -> (u8, u8) {
    match op {
        BinaryOp::Or => (1, 2),
        BinaryOp::And => (3, 4),
        BinaryOp::Equals | BinaryOp::NotEquals => (5, 6),
        BinaryOp::LessThan
        | BinaryOp::GreaterThan
        | BinaryOp::LessThanEquals
        | BinaryOp::GreaterThanEquals => (7, 8),
        BinaryOp::Add | BinaryOp::Subtract => (9, 10),
        BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Remainder => (11, 12),
    }
}

const EXPR_RECOVERY_SET: &[SyntaxKind] = &[SyntaxKind::OpenBrace, SyntaxKind::CloseBrace];

fn expr(p: &mut Parser<'_>) {
    expr_binding_power(p, 0, true);
}

fn expr_binding_power(p: &mut Parser<'_>, minimum_binding_power: u8, allow_initializer: bool) {
    let checkpoint = p.checkpoint();

    if p.at(SyntaxKind::Not) || p.at(SyntaxKind::Minus) {
        p.start_at(checkpoint, SyntaxKind::PrefixExpr);
        p.bump();
        expr_binding_power(p, 255, allow_initializer);
        p.finish();
    } else if p.at(SyntaxKind::Int)
        || p.at(SyntaxKind::Hex)
        || p.at(SyntaxKind::String)
        || p.at(SyntaxKind::True)
        || p.at(SyntaxKind::False)
        || p.at(SyntaxKind::Nil)
    {
        p.start(SyntaxKind::LiteralExpr);
        p.bump();
        p.finish();
    } else if p.at(SyntaxKind::Ident) {
        path_expr(p);

        if p.at(SyntaxKind::OpenBrace) && allow_initializer {
            p.start_at(checkpoint, SyntaxKind::InitializerExpr);
            p.bump();
            while !p.at(SyntaxKind::CloseBrace) {
                p.start(SyntaxKind::InitializerField);
                p.expect(SyntaxKind::Ident);
                p.expect(SyntaxKind::Colon);
                expr(p);
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
            p.try_eat(SyntaxKind::Comma);
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
            p.start_at(checkpoint, SyntaxKind::FunctionCallExpr);
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
            p.start_at(checkpoint, SyntaxKind::FieldAccessExpr);
            p.bump();
            p.expect(SyntaxKind::Ident);
            p.finish();
        } else if p.at(SyntaxKind::OpenBracket) {
            p.start_at(checkpoint, SyntaxKind::IndexAccessExpr);
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
        } else if p.at(SyntaxKind::And) {
            BinaryOp::And
        } else if p.at(SyntaxKind::Or) {
            BinaryOp::Or
        } else {
            return;
        };

        let (left_binding_power, right_binding_power) = binding_power(op);

        if left_binding_power < minimum_binding_power {
            return;
        }

        p.bump();

        p.start_at(checkpoint, SyntaxKind::BinaryExpr);
        expr_binding_power(p, right_binding_power, allow_initializer);
        p.finish();
    }
}

fn path_expr(p: &mut Parser<'_>) {
    p.start(SyntaxKind::PathExpr);
    p.expect(SyntaxKind::Ident);
    while p.try_eat(SyntaxKind::PathSeparator) {
        p.expect(SyntaxKind::Ident);
    }
    p.finish();
}

fn function_call_arg(p: &mut Parser<'_>) {
    p.start(SyntaxKind::FunctionCallArg);
    p.try_eat(SyntaxKind::Spread);
    expr(p);
    p.finish();
}

fn list_expr(p: &mut Parser<'_>) {
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

fn lambda_expr(p: &mut Parser<'_>) {
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

fn lambda_param(p: &mut Parser<'_>) {
    p.start(SyntaxKind::LambdaParam);
    p.try_eat(SyntaxKind::Spread);
    p.expect(SyntaxKind::Ident);
    p.try_eat(SyntaxKind::Question);
    if p.try_eat(SyntaxKind::Colon) {
        ty(p);
    }
    p.finish();
}

const TYPE_RECOVERY_SET: &[SyntaxKind] = &[SyntaxKind::OpenBrace, SyntaxKind::CloseBrace];

fn ty(p: &mut Parser<'_>) {
    let checkpoint = p.checkpoint();

    if p.at(SyntaxKind::Ident) {
        path_type(p);
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

    loop {
        if p.at(SyntaxKind::OpenBracket) {
            p.start_at(checkpoint, SyntaxKind::ListType);
            p.bump();
            p.expect(SyntaxKind::CloseBracket);
            p.finish();
        } else if p.at(SyntaxKind::Question) {
            p.start_at(checkpoint, SyntaxKind::OptionalType);
            p.bump();
            p.finish();
        } else {
            break;
        }
    }
}

fn path_type(p: &mut Parser<'_>) {
    p.start(SyntaxKind::PathType);
    p.expect(SyntaxKind::Ident);
    while p.try_eat(SyntaxKind::PathSeparator) {
        p.expect(SyntaxKind::Ident);
    }
    p.finish();
}

fn function_type_param(p: &mut Parser<'_>) {
    p.start(SyntaxKind::FunctionTypeParam);
    p.try_eat(SyntaxKind::Spread);
    p.expect(SyntaxKind::Ident);
    p.try_eat(SyntaxKind::Question);
    p.expect(SyntaxKind::Colon);
    ty(p);
    p.finish();
}

fn generic_types(p: &mut Parser<'_>) {
    p.start(SyntaxKind::GenericTypes);
    p.expect(SyntaxKind::LessThan);
    while !p.at(SyntaxKind::GreaterThan) {
        p.expect(SyntaxKind::Ident);
        if !p.try_eat(SyntaxKind::Comma) {
            break;
        }
    }
    p.expect(SyntaxKind::GreaterThan);
    p.finish();
}
