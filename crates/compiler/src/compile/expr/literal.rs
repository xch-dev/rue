use std::str::FromStr;

use clvmr::Allocator;
use num_bigint::BigInt;
use rue_ast::AstLiteralExpr;
use rue_hir::{Atom, Hir, Type, Value};
use rue_parser::{SyntaxKind, T};

use crate::Compiler;

pub fn compile_literal_expr(ctx: &mut Compiler, expr: &AstLiteralExpr) -> Value {
    let Some(value) = expr.value() else {
        return ctx.builtins().unresolved.clone();
    };

    match value.kind() {
        SyntaxKind::String => {
            let mut text = value.text();

            if let Some(stripped) = text.strip_prefix('"') {
                text = stripped;
            }

            if let Some(stripped) = text.strip_suffix('"') {
                text = stripped;
            }

            let bytes = text.as_bytes().to_vec();
            let ty = ctx.alloc_type(Type::Atom(Atom::BytesValue(bytes.clone(), true)));

            Value::new(ctx.alloc_hir(Hir::Atom(bytes)), ty)
        }
        SyntaxKind::Hex => todo!(),
        SyntaxKind::Integer => {
            let text = value.text().replace("_", "");

            let num = BigInt::from_str(&text).unwrap();
            let ty = ctx.alloc_type(Type::Atom(Atom::IntValue(num.clone())));

            Value::new(ctx.alloc_hir(Hir::Atom(bigint_atom(num))), ty)
        }
        SyntaxKind::Ident => ctx.builtins().unresolved.clone(),
        T![nil] => {
            let ty = ctx.builtins().nil;
            Value::new(ctx.alloc_hir(Hir::Atom(vec![])), ty)
        }
        T![true] => {
            let ty = ctx.builtins().true_type;
            Value::new(ctx.alloc_hir(Hir::Atom(vec![1])), ty)
        }
        T![false] => {
            let ty = ctx.builtins().false_type;
            Value::new(ctx.alloc_hir(Hir::Atom(vec![])), ty)
        }
        _ => unreachable!(),
    }
}

fn bigint_atom(value: BigInt) -> Vec<u8> {
    let mut allocator = Allocator::new();
    let atom = allocator.new_number(value).unwrap();
    allocator.atom(atom).to_vec()
}
