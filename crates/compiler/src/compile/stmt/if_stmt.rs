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
        compile_block(ctx, &then_block)
    } else {
        ctx.builtins().unresolved.clone()
    };

    Statement::If(condition.hir, then_block.hir)
}
