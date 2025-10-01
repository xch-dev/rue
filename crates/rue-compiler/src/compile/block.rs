use log::debug;
use rue_ast::{AstBlock, AstNode, AstStmt, AstStmtOrExpr};
use rue_diagnostic::{DiagnosticKind, SrcLoc};
use rue_hir::{
    BindingSymbol, Block, Declaration, ExprStatement, Hir, IfStatement, Scope, Statement, Symbol,
    Value,
};
use rue_types::TypeId;

use crate::{Compiler, compile_expr, compile_type};

pub fn compile_block(
    ctx: &mut Compiler,
    block: &AstBlock,
    is_expr: bool,
    expected_type: Option<TypeId>,
    require_return: bool,
) -> Value {
    let scope = ctx.alloc_scope(Scope::new());
    ctx.push_scope(scope);

    let mut statements = Vec::new();
    let mut return_value = None;
    let mut implicit_return = false;

    for stmt in block.items() {
        let stmt = match stmt {
            AstStmtOrExpr::Stmt(stmt) => stmt,
            AstStmtOrExpr::Expr(expr) => {
                if !is_expr {
                    ctx.diagnostic(expr.syntax(), DiagnosticKind::UnexpectedImplicitReturn);
                }

                if return_value.is_none() {
                    return_value = Some(compile_expr(ctx, &expr, expected_type));
                    implicit_return = true;
                }

                continue;
            }
        };

        let compiled = match stmt {
            AstStmt::ExprStmt(stmt) => {
                let value = if let Some(expr) = stmt.expr() {
                    compile_expr(ctx, &expr, None)
                } else {
                    debug!("Unresolved expr stmt expr");
                    ctx.builtins().unresolved.clone()
                };

                let always_nil = ctx.is_castable(value.ty, ctx.builtins().types.nil);

                if !always_nil {
                    ctx.diagnostic(stmt.syntax(), DiagnosticKind::UnusedStatementValue);
                }

                Statement::Expr(ExprStatement {
                    hir: value.hir,
                    always_nil,
                })
            }
            AstStmt::LetStmt(stmt) => {
                let symbol = ctx.alloc_symbol(Symbol::Unresolved);

                ctx.push_declaration(Declaration::Symbol(symbol));

                let expected_type = stmt.ty().map(|ty| compile_type(ctx, &ty));

                let value = if let Some(expr) = stmt.value() {
                    compile_expr(ctx, &expr, expected_type)
                } else {
                    debug!("Unresolved let binding value");
                    ctx.diagnostic(stmt.syntax(), DiagnosticKind::MissingLetValue);
                    ctx.builtins().unresolved.clone()
                };

                let ty = if let Some(expected_type) = expected_type {
                    if let Some(expr) = stmt.value() {
                        ctx.assign_type(expr.syntax(), value.ty, expected_type);
                    }
                    expected_type
                } else {
                    value.ty
                };

                *ctx.symbol_mut(symbol) = Symbol::Binding(BindingSymbol {
                    name: stmt.name(),
                    ty,
                    value: value.hir,
                    inline: stmt.inline().is_some(),
                });

                if let Some(name) = stmt.name() {
                    ctx.last_scope_mut()
                        .insert_symbol(name.text().to_string(), symbol, false);
                }

                ctx.pop_declaration();

                Statement::Let(symbol)
            }
            AstStmt::IfStmt(stmt) => {
                let condition = if let Some(condition) = stmt.condition() {
                    let value = compile_expr(ctx, &condition, None);
                    ctx.check_condition(condition.syntax(), value.ty);
                    value
                } else {
                    debug!("Unresolved if stmt condition");
                    ctx.builtins().unresolved.clone()
                };

                let then_block = if let Some(then_block) = stmt.then_block() {
                    if stmt.inline().is_some() {
                        compile_block(ctx, &then_block, false, expected_type, true)
                    } else {
                        let index = ctx.push_mappings(condition.then_map.clone());
                        let value = compile_block(ctx, &then_block, false, expected_type, true);
                        ctx.revert_mappings(index);
                        value
                    }
                } else {
                    debug!("Unresolved if stmt then block");
                    ctx.builtins().unresolved.clone()
                };

                if stmt.inline().is_none() {
                    ctx.push_mappings(condition.else_map);
                }

                Statement::If(IfStatement {
                    condition: condition.hir,
                    then: then_block.hir,
                    inline: stmt.inline().is_some(),
                })
            }
            AstStmt::ReturnStmt(stmt) => {
                let value = if let Some(expr) = stmt.expr() {
                    compile_expr(ctx, &expr, expected_type)
                } else {
                    debug!("Unresolved return stmt expr");
                    ctx.builtins().unresolved.clone()
                };

                if is_expr {
                    ctx.diagnostic(stmt.syntax(), DiagnosticKind::UnnecessaryExplicitReturn);
                }

                if return_value.is_none() {
                    return_value = Some(value.clone());
                }

                Statement::Return(value.hir)
            }
            AstStmt::AssertStmt(stmt) => {
                let value = if let Some(expr) = stmt.expr() {
                    let value = compile_expr(ctx, &expr, None);
                    ctx.check_condition(expr.syntax(), value.ty);
                    value
                } else {
                    debug!("Unresolved assert expr");
                    ctx.builtins().unresolved.clone()
                };

                ctx.push_mappings(value.then_map);

                Statement::Assert(
                    value.hir,
                    SrcLoc::new(ctx.source().clone(), stmt.syntax().text_range().into()),
                )
            }
            AstStmt::RaiseStmt(stmt) => {
                let value = if let Some(expr) = stmt.expr() {
                    compile_expr(ctx, &expr, None)
                } else {
                    debug!("Unresolved raise stmt expr");
                    ctx.builtins().unresolved.clone()
                };

                if return_value.is_none() {
                    return_value = Some(Value::new(
                        ctx.alloc_hir(Hir::Unresolved),
                        ctx.builtins().types.never,
                    ));
                }

                Statement::Raise(value.hir)
            }
        };

        statements.push(compiled);
    }

    if return_value.is_none() && require_return {
        ctx.diagnostic(block.syntax(), DiagnosticKind::MissingReturn);
        return_value = Some(Value::new(
            ctx.alloc_hir(Hir::Unresolved),
            ctx.builtins().types.unresolved,
        ));
    }

    let hir = ctx.alloc_hir(Hir::Block(Block {
        statements,
        body: return_value
            .as_ref()
            .and_then(|value| implicit_return.then_some(value.hir)),
    }));

    ctx.pop_scope();

    if let Some(return_value) = return_value {
        return_value.with_hir(hir)
    } else {
        Value::new(hir, ctx.builtins().nil.ty)
    }
}
