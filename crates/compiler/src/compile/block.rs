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

    // TODO: Handle early return blocks
    let body = body.unwrap();

    let hir = ctx.alloc_hir(Hir::Block(Block {
        statements,
        body: Some(body.hir),
    }));

    Value::new(hir, body.ty)
}
