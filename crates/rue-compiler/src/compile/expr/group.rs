use rue_ast::AstGroupExpr;
use rue_hir::Value;

use crate::{Compiler, compile_expr};

pub fn compile_group_expr(ctx: &mut Compiler, group: &AstGroupExpr) -> Value {
    let Some(expr) = group.expr() else {
        return ctx.builtins().unresolved.clone();
    };
    compile_expr(ctx, &expr)
}
