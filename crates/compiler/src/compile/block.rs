use rue_ast::{AstBlock, AstStmtOrExpr};
use rue_hir::{Hir, HirId};

use crate::{Context, compile_expr, compile_stmt};

pub fn compile_block(ctx: &mut Context, block: &AstBlock) -> HirId {
    let mut statements = Vec::new();
    let mut body = None;

    for stmt in block.items() {
        match stmt {
            AstStmtOrExpr::Stmt(stmt) => {
                statements.push(compile_stmt(ctx, &stmt));
            }
            AstStmtOrExpr::Expr(expr) => {
                body = Some(compile_expr(ctx, &expr));
            }
        }
    }

    ctx.alloc_hir(Hir::Block(Block { statements, body }))
}
