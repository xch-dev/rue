use log::debug;
use rue_ast::{AstBlock, AstNode, AstStmtOrExpr};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Block, Hir, Scope, Statement, Value};
use rue_types::TypeId;

use crate::{Compiler, compile_expr, compile_stmt};

pub fn compile_block(
    ctx: &mut Compiler,
    block: &AstBlock,
    is_expr: bool,
    expected_type: Option<TypeId>,
) -> Value {
    let scope = ctx.alloc_scope(Scope::new());
    ctx.push_scope(scope);

    let mut statements = Vec::new();
    let mut body = None;
    let mut has_return = false;

    for stmt in block.items() {
        match stmt {
            AstStmtOrExpr::Stmt(stmt) => {
                let compiled = compile_stmt(ctx, &stmt, expected_type);
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
                body = Some(compile_expr(ctx, &expr, expected_type));
                has_return = true;
            }
        }
    }

    if is_expr && !has_return {
        ctx.diagnostic(block.syntax(), DiagnosticKind::MissingReturn);
    }

    let hir = ctx.alloc_hir(Hir::Block(Block {
        statements,
        body: body.as_ref().map(|value| value.hir),
    }));

    ctx.pop_scope();

    if let Some(body) = body {
        body.with_hir(hir)
    } else {
        if is_expr {
            debug!("Unresolved block body");
        }
        Value::new(hir, ctx.builtins().unresolved.ty)
    }
}
