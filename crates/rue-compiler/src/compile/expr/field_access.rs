use log::debug;
use rue_ast::AstFieldAccessExpr;
use rue_hir::Value;

use crate::{Compiler, compile_expr, compile_named_field};

pub fn compile_field_access_expr(ctx: &mut Compiler, access: &AstFieldAccessExpr) -> Value {
    let expr = if let Some(expr) = access.expr() {
        compile_expr(ctx, &expr, None)
    } else {
        debug!("Unresolved field access expr");
        ctx.builtins().unresolved.clone()
    };

    let Some(name) = access.field() else {
        debug!("Unresolved field access name");
        return ctx.builtins().unresolved.clone();
    };

    compile_named_field(ctx, &expr, &name)
}
