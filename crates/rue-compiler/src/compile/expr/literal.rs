use log::debug;
use rue_ast::AstLiteralExpr;
use rue_hir::{Hir, Value};
use rue_types::Type;

use crate::{Compiler, LiteralValue};

pub fn compile_literal_expr(ctx: &mut Compiler, expr: &AstLiteralExpr) -> Value {
    let Some(value) = expr.value() else {
        debug!("Unresolved literal expr");
        return ctx.builtins().unresolved.clone();
    };

    let value = LiteralValue::decode(&value);

    let hir = match &value {
        LiteralValue::String(value) => Hir::String(value.clone()),
        LiteralValue::Bytes(bytes) => Hir::Bytes(bytes.clone()),
        LiteralValue::Int(value) => Hir::Int(value.clone()),
        LiteralValue::Nil => return ctx.builtins().nil.clone(),
        LiteralValue::True => return ctx.builtins().true_value.clone(),
        LiteralValue::False => return ctx.builtins().false_value.clone(),
    };

    let ty = ctx.alloc_type(Type::Atom(value.atom()));

    Value::new(ctx.alloc_hir(hir), ty)
}
