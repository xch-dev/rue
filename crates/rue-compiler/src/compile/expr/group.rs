use log::debug;
use rue_ast::AstGroupExpr;
use rue_hir::Value;
use rue_types::TypeId;

use crate::{Compiler, compile_expr};

pub fn compile_group_expr(
    ctx: &mut Compiler,
    group: &AstGroupExpr,
    expected_type: Option<TypeId>,
) -> Value {
    let Some(expr) = group.expr() else {
        debug!("Unresolved group expr");
        return ctx.builtins().unresolved.clone();
    };
    compile_expr(ctx, &expr, expected_type)
}
