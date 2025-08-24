use rue_ast::AstListExpr;
use rue_hir::{Hir, Value};
use rue_types::{Pair, Type};

use crate::{Compiler, compile_expr};

pub fn compile_list_expr(ctx: &mut Compiler, list: &AstListExpr) -> Value {
    let mut values = Vec::new();

    for expr in list.exprs() {
        values.push(compile_expr(ctx, &expr));
    }

    let mut hir = ctx.builtins().nil.hir;
    let mut ty = ctx.builtins().nil.ty;

    for value in values.into_iter().rev() {
        hir = ctx.alloc_hir(Hir::Pair(value.hir, hir));
        ty = ctx.alloc_type(Type::Pair(Pair::new(value.ty, ty)));
    }

    Value::new(hir, ty)
}
