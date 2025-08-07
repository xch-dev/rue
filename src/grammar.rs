use crate::{BinaryOp, SyntaxKind, T, parser::Parser};

pub fn document(p: &mut Parser<'_>) {
    p.start(SyntaxKind::Document);
    while !p.at(SyntaxKind::Eof) {
        item(p);
    }
    p.finish();
}

fn item(p: &mut Parser<'_>) {
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

fn ty(p: &mut Parser<'_>) {
    p.start(SyntaxKind::LiteralType);
    p.expect(SyntaxKind::Ident);
    p.finish();
}

fn block(p: &mut Parser<'_>) {
    p.start(SyntaxKind::Block);
    p.expect(T!['{']);
    while !p.at(T!['}']) {
        if stmt(p) == StatementKind::End {
            break;
        }
    }
    p.expect(T!['}']);
    p.finish();
}

#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StatementKind {
    Normal,
    End,
}

fn stmt(p: &mut Parser<'_>) -> StatementKind {
    if p.at(T![let]) {
        let_stmt(p);
    } else {
        expr(p);
        if !p.try_eat(T![;]) {
            return StatementKind::End;
        }
    }
    StatementKind::Normal
}

fn let_stmt(p: &mut Parser<'_>) {
    p.start(SyntaxKind::LetStmt);
    p.expect(T![let]);
    p.expect(SyntaxKind::Ident);
    p.expect(T![:]);
    ty(p);
    p.expect(T![=]);
    expr(p);
    p.expect(T![;]);
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
        BinaryOp::BitwiseOr => (9, 10),
        BinaryOp::BitwiseXor => (11, 12),
        BinaryOp::BitwiseAnd => (13, 14),
        BinaryOp::LeftShift | BinaryOp::RightShift => (15, 16),
        BinaryOp::Add | BinaryOp::Subtract => (17, 18),
        BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Remainder => (19, 20),
    }
}

fn expr(p: &mut Parser<'_>) {
    expr_binding_power(p, 0);
}

fn expr_binding_power(p: &mut Parser<'_>, minimum_binding_power: u8) {
    let checkpoint = p.checkpoint();

    if let Some(kind) = p.at_any(&[T![!], T![-], T![+], T![~]]) {
        p.start_at(checkpoint, SyntaxKind::PrefixExpr);
        p.expect(kind);
        expr_binding_power(p, 255);
        p.finish();
    } else if let Some(kind) = p.at_any(&[
        SyntaxKind::String,
        SyntaxKind::Hex,
        SyntaxKind::Integer,
        SyntaxKind::Ident,
        T![nil],
        T![true],
        T![false],
    ]) {
        p.start(SyntaxKind::LiteralExpr);
        p.expect(kind);
        p.finish();
    } else if p.at(T!['{']) {
        block(p);
    } else {
        p.skip();
        return;
    }

    loop {
        let Some(op_kind) = p.at_any(&[
            T![+],
            T![-],
            T![*],
            T![/],
            T![%],
            T![<],
            T![>],
            T![<=],
            T![>=],
            T![==],
            T![!=],
            T![&&],
            T![||],
            T![&],
            T![|],
            T![^],
            T![<<],
            T![>>],
        ]) else {
            return;
        };

        let op = match op_kind {
            T![+] => BinaryOp::Add,
            T![-] => BinaryOp::Subtract,
            T![*] => BinaryOp::Multiply,
            T![/] => BinaryOp::Divide,
            T![%] => BinaryOp::Remainder,
            T![<] => BinaryOp::LessThan,
            T![>] => BinaryOp::GreaterThan,
            T![<=] => BinaryOp::LessThanEquals,
            T![>=] => BinaryOp::GreaterThanEquals,
            T![==] => BinaryOp::Equals,
            T![!=] => BinaryOp::NotEquals,
            T![&&] => BinaryOp::And,
            T![||] => BinaryOp::Or,
            T![&] => BinaryOp::BitwiseAnd,
            T![|] => BinaryOp::BitwiseOr,
            T![^] => BinaryOp::BitwiseXor,
            T![<<] => BinaryOp::LeftShift,
            T![>>] => BinaryOp::RightShift,
            _ => unreachable!(),
        };

        let (left_binding_power, right_binding_power) = binding_power(op);

        if left_binding_power < minimum_binding_power {
            return;
        }

        p.expect(op_kind);

        p.start_at(checkpoint, SyntaxKind::BinaryExpr);
        expr_binding_power(p, right_binding_power);
        p.finish();
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};

    use crate::Lexer;

    use super::*;

    fn check(f: impl FnOnce(&mut Parser<'_>), source: &str, expect: Expect, errors: Expect) {
        let tokens = Lexer::new(source).collect::<Vec<_>>();

        let mut parser = Parser::new(source, tokens);
        f(&mut parser);

        let result = parser.build();

        let output = format!("{:#?}", result.node);

        let mut error_output = String::new();

        for error in result.errors {
            error_output.push_str(&error.message(source));
        }

        expect.assert_eq(&output);
        errors.assert_eq(&error_output);
    }

    #[test]
    fn test_literal_type() {
        check(ty, "String", expect![[r#"
            LiteralType@0..6
              Ident@0..6 "String"
        "#]], expect![]);
    }
}
