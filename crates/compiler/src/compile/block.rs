use rue_ast::{AstBlock, AstStmtOrExpr};
use rue_hir::{Block, Hir, HirId};

use crate::{Compiler, compile_expr, compile_stmt};

pub fn compile_block(ctx: &mut Compiler, block: &AstBlock) -> HirId {
    let mut statements = Vec::new();
    let mut body = None;

    for stmt in block.items() {
        match stmt {
            AstStmtOrExpr::Stmt(stmt) => {
                statements.push(compile_stmt(ctx, &stmt));
            }
            AstStmtOrExpr::Expr(expr) => {
                // TODO: Preserve value
                body = Some(compile_expr(ctx, &expr).hir);
            }
        }
    }

    ctx.alloc_hir(Hir::Block(Block { statements, body }))
}
