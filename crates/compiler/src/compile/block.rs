use rue_ast::{AstBlock, AstStmtOrExpr};
use rue_hir::{Block, Hir, Value};

use crate::{Compiler, compile_expr, compile_stmt};

pub fn compile_block(ctx: &mut Compiler, block: &AstBlock) -> Value {
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

    let body_ty = body.as_ref().map(|body| body.ty);
    let body = body.map(|body| body.hir);

    let hir = ctx.alloc_hir(Hir::Block(Block { statements, body }));

    Value::new(hir, body_ty.unwrap_or(ctx.builtins().unresolved.ty))
}
