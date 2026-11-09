use log::debug;
use rue_ast::{AstBlock, AstNode, AstStmt, AstStmtOrExpr};
use rue_diagnostic::{DiagnosticKind, SrcLoc};
use rue_hir::{
    BindingSymbol, Block, Declaration, ExprStatement, Hir, IfStatement, Statement, Symbol, Value,
};
use rue_types::{Type, TypeId, Union};

use rue_lint::{ExprStatementSite, InlineIf};

use crate::{
    Compiler, CompletionContext, SyntaxItemKind, compile_expr, compile_type, create_binding,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlockKind {
    /// The top level of a function or lambda body. It must end in an expression, and the
    /// caller is responsible for pushing the [`ReturnTarget`](crate::ReturnTarget) that
    /// nested `return` statements exit to.
    Body,
    /// A block used as an expression. It must end in an expression, and nested `return`
    /// statements are not allowed, since they would only exit the block itself.
    Expr,
    /// The then block of an if statement. It must end in `return` or `raise`, and its
    /// `return` statements exit to the enclosing return target, if there is one.
    Statement,
}

pub fn compile_block(
    ctx: &mut Compiler,
    block: &AstBlock,
    kind: BlockKind,
    expected_type: Option<TypeId>,
    require_return: bool,
) -> Value {
    let is_expr = kind != BlockKind::Statement;

    if kind == BlockKind::Expr {
        ctx.push_return_target(None);
    }

    let return_count = ctx.return_count();

    ctx.add_syntax(
        SyntaxItemKind::CompletionContext(CompletionContext::Expression),
        block.syntax().text_range(),
    );

    let index = ctx.mapping_checkpoint();

    let scope = ctx.alloc_child_scope();
    let range = block.syntax().text_range();
    ctx.push_scope(scope, range.start());

    let mut statements = Vec::new();
    let mut return_value = None;
    let mut return_types = Vec::new();
    let mut implicit_return = false;
    let mut inline_ifs = Vec::new();

    for stmt in block.items() {
        let stmt = match stmt {
            AstStmtOrExpr::Stmt(stmt) => {
                ctx.add_syntax(
                    SyntaxItemKind::CompletionContext(CompletionContext::Statement),
                    stmt.syntax().text_range(),
                );

                stmt
            }
            AstStmtOrExpr::Expr(expr) => {
                if !is_expr {
                    ctx.diagnostic(expr.syntax(), DiagnosticKind::UnexpectedImplicitReturn);
                }

                let value = compile_expr(ctx, &expr, expected_type);

                return_types.push(value.ty);

                if return_value.is_none() {
                    return_value = Some(value);
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

                let site = ExprStatementSite {
                    srcloc: ctx.srcloc(stmt.syntax()),
                    always_nil,
                };
                ctx.lint_sites_mut().expr_statements.push(site);

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
                    name: None,
                    value: value.with_type(ty),
                    inline: stmt.inline().is_some(),
                });

                if let Some(binding) = stmt.binding() {
                    let scope = ctx.alloc_child_scope();

                    ctx.push_scope(scope, stmt.syntax().text_range().start());

                    create_binding(ctx, symbol, &binding);
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
                        compile_block(ctx, &then_block, BlockKind::Statement, None, true)
                    } else {
                        let range = then_block.syntax().text_range();
                        let index = ctx.push_mappings(condition.then_map.clone(), range.start());
                        let value =
                            compile_block(ctx, &then_block, BlockKind::Statement, None, true);
                        ctx.revert_mappings(index, range.end());
                        value
                    }
                } else {
                    debug!("Unresolved if stmt then block");
                    ctx.builtins().unresolved.clone()
                };

                if stmt.inline().is_none() {
                    ctx.push_mappings(condition.else_map, stmt.syntax().text_range().end());
                } else {
                    inline_ifs.push((ctx.srcloc(stmt.syntax()), statements.len()));
                }

                Statement::If(IfStatement {
                    condition: condition.hir,
                    then: then_block.hir,
                    inline: stmt.inline().is_some(),
                })
            }
            AstStmt::ReturnStmt(stmt) => {
                ctx.increment_return_count();

                let has_target = ctx.return_target_mut().is_some();
                let target_type = ctx
                    .return_target_mut()
                    .and_then(|target| target.expected_type);

                let value = if let Some(expr) = stmt.expr() {
                    compile_expr(ctx, &expr, target_type)
                } else {
                    ctx.builtins().nil.clone()
                };

                if is_expr {
                    ctx.diagnostic(stmt.syntax(), DiagnosticKind::UnexpectedExplicitReturn);
                } else if !has_target {
                    ctx.diagnostic(stmt.syntax(), DiagnosticKind::ReturnInExpressionBlock);
                } else {
                    // Every `return` exits the enclosing function or lambda, so it must be
                    // checked against its return type rather than the type of this block.
                    if let Some(target_type) = target_type {
                        if let Some(expr) = stmt.expr() {
                            ctx.assign_type(expr.syntax(), value.ty, target_type);
                        } else {
                            ctx.assign_type(stmt.syntax(), value.ty, target_type);
                        }
                    }

                    if let Some(target) = ctx.return_target_mut() {
                        target.types.push(value.ty);
                    }
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

                ctx.push_mappings(value.then_map, stmt.syntax().text_range().end());

                Statement::Assert(
                    value.hir,
                    SrcLoc::new(ctx.source().clone(), stmt.syntax().text_range().into()),
                )
            }
            AstStmt::RaiseStmt(stmt) => {
                let value = stmt.expr().map(|expr| compile_expr(ctx, &expr, None));

                if return_value.is_none() {
                    return_value = Some(Value::new(
                        ctx.alloc_hir(Hir::Unresolved),
                        ctx.builtins().types.never,
                    ));
                }

                Statement::Raise(
                    value.map(|value| value.hir),
                    SrcLoc::new(ctx.source().clone(), stmt.syntax().text_range().into()),
                )
            }
            AstStmt::DebugStmt(stmt) => {
                let value = if let Some(expr) = stmt.expr() {
                    compile_expr(ctx, &expr, None)
                } else {
                    debug!("Unresolved print expr");
                    ctx.builtins().unresolved.clone()
                };

                Statement::Debug(
                    value.hir,
                    SrcLoc::new(ctx.source().clone(), stmt.syntax().text_range().into()),
                )
            }
        };

        statements.push(compiled);
    }

    if return_value.is_none() {
        return_types.push(ctx.builtins().types.nil);
    }

    let return_type = if kind == BlockKind::Statement || return_types.is_empty() {
        // Statement blocks always exit by either returning from the function or raising,
        // so control never continues past them with a value.
        ctx.builtins().types.never
    } else if return_types.len() == 1 {
        return_types[0]
    } else {
        ctx.alloc_type(Type::Union(Union::new(return_types)))
    };

    let mut return_value = return_value.map(|value| value.with_type(return_type));

    if return_value.is_none() && require_return {
        ctx.diagnostic(block.syntax(), DiagnosticKind::MissingReturn);
        return_value = Some(Value::new(ctx.alloc_hir(Hir::Unresolved), return_type));
    }

    let hir = ctx.alloc_hir(Hir::Block(Block {
        statements,
        body: return_value
            .as_ref()
            .and_then(|value| implicit_return.then_some(value.hir)),
    }));

    for (srcloc, index) in inline_ifs {
        ctx.add_inline_if(srcloc, InlineIf::Statement(hir, index));
    }

    ctx.revert_mappings(index, range.end());

    // The trailing expression's reference and type guard mappings only describe the
    // block's value if nothing else (such as a `return` statement) could have produced it.
    let definitely_trailing = implicit_return && ctx.return_count() == return_count;

    if kind == BlockKind::Expr {
        ctx.pop_return_target();
    }

    match return_value {
        Some(return_value) if definitely_trailing => return_value.with_hir(hir),
        Some(return_value) => Value::new(hir, return_value.ty),
        None => Value::new(hir, return_type),
    }
}
