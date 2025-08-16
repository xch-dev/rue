use rue_ast::AstPairExpr;
use rue_hir::{Hir, Type, Value};

use crate::{Compiler, compile_expr};

pub fn compile_pair_expr(ctx: &mut Compiler, pair: &AstPairExpr) -> Value {
    let first = if let Some(first) = pair.first() {
        compile_expr(ctx, &first)
    } else {
        ctx.builtins().unresolved.clone()
    };

    let rest = if let Some(rest) = pair.rest() {
        compile_expr(ctx, &rest)
    } else {
        ctx.builtins().unresolved.clone()
    };

    let hir = ctx.alloc_hir(Hir::Pair(first.hir, rest.hir));
    let ty = ctx.alloc_type(Type::Pair(first.ty, rest.ty));

    Value::new(hir, ty)
}
