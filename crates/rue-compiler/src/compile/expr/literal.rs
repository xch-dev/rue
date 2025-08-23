use std::{borrow::Cow, str::FromStr};

use num_bigint::BigInt;
use rue_ast::AstLiteralExpr;
use rue_hir::{Hir, Value};
use rue_lir::bigint_atom;
use rue_parser::{SyntaxKind, T};
use rue_types::{Atom, AtomKind, AtomRestriction, Type};

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

            let hir = ctx.alloc_hir(Hir::String(text.to_string()));
            let ty = ctx.alloc_type(Type::Atom(Atom::new(
                AtomKind::Bytes,
                Some(AtomRestriction::Value(Cow::Owned(text.as_bytes().to_vec()))),
            )));
            Value::new(hir, ty)
        }
        SyntaxKind::Hex => {
            let mut text = value.text();

            if let Some(stripped) = text.strip_prefix("0x") {
                text = stripped;
            }

            let text = text.replace("_", "");

            let bytes = hex::decode(text).unwrap();
            let ty = ctx.alloc_type(Type::Atom(if bytes.is_empty() {
                Atom::NIL
            } else {
                Atom::new(
                    AtomKind::Bytes,
                    Some(AtomRestriction::Value(Cow::Owned(bytes.clone()))),
                )
            }));

            Value::new(ctx.alloc_hir(Hir::Bytes(bytes)), ty)
        }
        SyntaxKind::Integer => {
            let text = value.text().replace("_", "");

            let num = BigInt::from_str(&text).unwrap();
            let ty = ctx.alloc_type(Type::Atom(Atom::new(
                AtomKind::Int,
                Some(AtomRestriction::Value(Cow::Owned(bigint_atom(num.clone())))),
            )));

            Value::new(ctx.alloc_hir(Hir::Int(num)), ty)
        }
        T![nil] => ctx.builtins().nil.clone(),
        T![true] => ctx.builtins().true_value.clone(),
        T![false] => ctx.builtins().false_value.clone(),
        _ => unreachable!(),
    }
}
