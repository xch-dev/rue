use rue_ast::{AstBlock, AstNode, AstStmtOrExpr};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Block, Hir, Statement, Value};

use crate::{Compiler, compile_expr, compile_stmt};

pub fn compile_block(ctx: &mut Compiler, block: &AstBlock, is_expr: bool) -> Value {
    let mut statements = Vec::new();
    let mut body = None;
    let mut has_return = false;

    for stmt in block.items() {
        match stmt {
            AstStmtOrExpr::Stmt(stmt) => {
                let compiled = compile_stmt(ctx, &stmt);
                match &compiled {
                    Statement::Return(_) => {
                        if is_expr {
                            ctx.diagnostic(
                                stmt.syntax(),
                                DiagnosticKind::UnnecessaryExplicitReturn,
                            );
                        }
                        has_return = true;
                    }
                    Statement::Raise(_) => {
                        has_return = true;
                    }
                    Statement::Expr(_) => {
                        ctx.diagnostic(stmt.syntax(), DiagnosticKind::InvalidExpressionStatement);
                    }
                    _ => {}
                }
                statements.push(compiled);
            }
            AstStmtOrExpr::Expr(expr) => {
                if !is_expr {
                    ctx.diagnostic(expr.syntax(), DiagnosticKind::UnexpectedImplicitReturn);
                }
                body = Some(compile_expr(ctx, &expr));
                has_return = true;
            }
        }
    }

    if is_expr && !has_return {
        ctx.diagnostic(block.syntax(), DiagnosticKind::MissingReturn);
    }

    let body_ty = body.as_ref().map(|body| body.ty);
    let body = body.map(|body| body.hir);

    let hir = ctx.alloc_hir(Hir::Block(Block { statements, body }));

    Value::new(hir, body_ty.unwrap_or(ctx.builtins().unresolved.ty))
}
