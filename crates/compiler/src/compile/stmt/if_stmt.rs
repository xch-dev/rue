use rue_ast::AstIfStmt;
use rue_hir::Statement;

use crate::{Compiler, compile_block, compile_expr};

pub fn compile_if_stmt(ctx: &mut Compiler, stmt: &AstIfStmt) -> Statement {
    let condition = if let Some(condition) = stmt.condition() {
        compile_expr(ctx, &condition)
    } else {
        ctx.builtins().unresolved.clone()
    };

    let then_block = if let Some(then_block) = stmt.then_block() {
        let index = ctx.push_mappings(condition.then_map.clone());
        let value = compile_block(ctx, &then_block, false);
        ctx.revert_mappings(index);
        value
    } else {
        ctx.builtins().unresolved.clone()
    };

    ctx.push_mappings(condition.else_map);

    Statement::If(condition.hir, then_block.hir)
}
