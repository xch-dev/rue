use log::debug;
use rue_ast::{AstBlock, AstExpr, AstNode, AstStmt, AstStmtOrExpr};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{
    BindingSymbol, Block, ControlStatement, Declaration, ExprStatement, Hir, HirId, IfStatement,
    Statement, Symbol, Value, control_fallthroughs, control_may_return, statements_terminate,
};
use rue_parser::SyntaxNode;
use rue_types::{Type, TypeId, Union};

use crate::{
    Compiler, CompletionContext, SyntaxItemKind, compile_branching_expr, compile_expr,
    compile_type, create_binding,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlockKind {
    /// The top level of a function or lambda body, or a branch of the `if`, `match` or block that
    /// it ends with, since that's what the function returns too. It must end in an expression,
    /// `return` or `raise`, and the caller is responsible for pushing the
    /// [`ReturnTarget`](crate::ReturnTarget) that nested `return` statements exit to.
    Body,
    /// A block used as an expression. It must end in an expression, and nested `return`
    /// statements are not allowed, since they would only exit the block itself.
    Expr,
    /// A branch of a statement, such as the block of an `if` statement or an arm of a `match`
    /// statement. It can't end in an expression, since its value wouldn't be used, but it can end
    /// in `return` or `raise`, or fall through to the rest of the block after the statement. Its
    /// `return` statements exit to the enclosing return target, if there is one.
    Statement,
}

/// What the branches of an `if`, `match` or block are when they're blocks rather than values (see
/// [`compile_branching_expr`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Branches {
    pub kind: BlockKind,
    /// Whether a branch needs a value, rather than being able to fall through to the end without
    /// one, like the body of a function without a return type.
    pub require_value: bool,
}

impl Branches {
    /// The branches of a statement, which don't have values.
    pub const STATEMENT: Self = Self {
        kind: BlockKind::Statement,
        require_value: false,
    };

    pub fn is_statement(self) -> bool {
        self.kind == BlockKind::Statement
    }
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
    let mut trailing_expr = None;

    let mut items = block.items().peekable();

    while let Some(stmt) = items.next() {
        let is_last = items.peek().is_none();

        let stmt = match stmt {
            AstStmtOrExpr::Stmt(stmt) => {
                ctx.add_syntax(
                    SyntaxItemKind::CompletionContext(CompletionContext::Statement),
                    stmt.syntax().text_range(),
                );

                stmt
            }
            // An `if` and `else` chain or a `match` before the end of the block is a statement, as
            // is one that ends a statement block, whose value isn't used
            AstStmtOrExpr::Expr(expr) if expr.is_if_or_match() && (!is_last || !is_expr) => {
                let (value, narrowing) =
                    compile_branching_expr(ctx, &expr, None, Branches::STATEMENT);

                let inline = match &expr {
                    AstExpr::IfExpr(expr) => expr.inline().is_some(),
                    AstExpr::MatchExpr(expr) => expr.inline().is_some(),
                    _ => false,
                };

                let statement = control_statement(ctx, expr.syntax(), value.hir, inline);

                if statement.fallthroughs == 0 && return_value.is_none() {
                    return_value = Some(Value::new(
                        ctx.alloc_hir(Hir::Unresolved),
                        ctx.builtins().types.never,
                    ));
                }

                statements.push(Statement::Control(statement));

                if let Some(narrowing) = narrowing {
                    ctx.push_mappings(narrowing, expr.syntax().text_range().end());
                }

                continue;
            }
            AstStmtOrExpr::Expr(expr) => {
                if !is_expr {
                    ctx.diagnostic(expr.syntax(), DiagnosticKind::UnexpectedImplicitReturn);
                }

                // The value of a function body is what it returns, so the branches of an `if`,
                // `match` or block that's its value can return from the function too, and only
                // need a value if the body does
                let value = if kind == BlockKind::Body && expr.ends_with_brace() {
                    let branches = Branches {
                        kind: BlockKind::Body,
                        require_value: require_return,
                    };
                    compile_branching_expr(ctx, &expr, expected_type, branches).0
                } else {
                    compile_expr(ctx, &expr, expected_type)
                };

                return_types.push(value.ty);

                // The trailing expression is kept even if a `return` or `raise` comes before it, so
                // that the HIR matches the code. It's never evaluated in that case, since lowering
                // stops at the `return` or `raise`.
                trailing_expr.get_or_insert(value.hir);

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
                    ctx.unresolved()
                };

                Statement::Expr(ExprStatement {
                    hir: value.hir,
                    always_nil: ctx.is_castable(value.ty, ctx.builtins().types.nil),
                    srcloc: ctx.trimmed_srcloc(stmt.syntax()),
                })
            }
            AstStmt::LetStmt(stmt) => {
                let symbol = ctx.alloc_symbol(Symbol::Unresolved);

                ctx.push_declaration(Declaration::Symbol(symbol));

                let expected_type = stmt.ty().map(|ty| compile_type(ctx, &ty));

                // An annotation is a type of its own that the value must be assignable to
                let value = if let Some(expr) = stmt.value() {
                    ctx.with_expected_type_provisional(
                        ctx.is_expected_type_provisional() && expected_type.is_none(),
                        |ctx| {
                            ctx.with_expected_type_hint(
                                ctx.is_expected_type_hint() && expected_type.is_none(),
                                |ctx| compile_expr(ctx, &expr, expected_type),
                            )
                        },
                    )
                } else {
                    debug!("Unresolved let binding value");
                    ctx.diagnostic(stmt.syntax(), DiagnosticKind::MissingLetValue);
                    ctx.unresolved()
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

                    // The binding is only in scope after the statement, so that it isn't
                    // offered as a completion in its own value
                    ctx.push_scope(scope, stmt.syntax().text_range().end());

                    create_binding(ctx, symbol, &binding);
                }

                ctx.pop_declaration();

                Statement::Let(symbol, ctx.trimmed_srcloc(stmt.syntax()))
            }
            AstStmt::IfStmt(stmt) => {
                let condition = if let Some(condition) = stmt.condition() {
                    let value = compile_expr(ctx, &condition, None);
                    ctx.check_condition(condition.syntax(), value.ty);
                    value
                } else {
                    debug!("Unresolved if stmt condition");
                    ctx.unresolved()
                };

                let then_block = if let Some(then_block) = stmt.then_block() {
                    if stmt.inline().is_some() {
                        compile_block(ctx, &then_block, BlockKind::Statement, None, false)
                    } else {
                        let range = then_block.syntax().text_range();
                        let index = ctx.push_mappings(condition.then_map.clone(), range.start());
                        let value =
                            compile_block(ctx, &then_block, BlockKind::Statement, None, false);
                        ctx.revert_mappings(index, range.end());
                        value
                    }
                } else {
                    debug!("Unresolved if stmt then block");
                    ctx.unresolved()
                };

                let srcloc = ctx.trimmed_srcloc(stmt.syntax());

                // A block that ends in `return` or `raise` is an early exit, so the rest of the
                // block is its `else` branch, and only sees the values that the condition is false
                // for. Otherwise, the rest of the block comes after either branch.
                if control_fallthroughs(ctx, then_block.hir) == 0 {
                    if stmt.inline().is_none() {
                        ctx.push_mappings(condition.else_map, stmt.syntax().text_range().end());
                    }

                    Statement::If(IfStatement {
                        condition: condition.hir,
                        then: then_block.hir,
                        inline: stmt.inline().is_some(),
                        srcloc,
                    })
                } else {
                    let else_block = ctx.alloc_hir(Hir::Block(Block {
                        statements: Vec::new(),
                        body: None,
                    }));

                    let hir = ctx.alloc_hir(Hir::If(
                        condition.hir,
                        then_block.hir,
                        else_block,
                        stmt.inline().is_some(),
                    ));

                    Statement::Control(control_statement(
                        ctx,
                        stmt.syntax(),
                        hir,
                        stmt.inline().is_some(),
                    ))
                }
            }
            AstStmt::ReturnStmt(stmt) => {
                ctx.increment_return_count();

                let has_target = ctx.return_target_mut().is_some();
                let target_type = ctx
                    .return_target_mut()
                    .and_then(|target| target.expected_type);

                // The return type is a type of its own that the value must be assignable to
                let value = if let Some(expr) = stmt.expr() {
                    ctx.with_expected_type_provisional(
                        ctx.is_expected_type_provisional() && target_type.is_none(),
                        |ctx| {
                            ctx.with_expected_type_hint(
                                ctx.is_expected_type_hint() && target_type.is_none(),
                                |ctx| compile_expr(ctx, &expr, target_type),
                            )
                        },
                    )
                } else {
                    ctx.builtins().nil.clone()
                };

                // A `return` at the top level of a function body is allowed, but it's reported by
                // the `unnecessary_return` lint, since the body can end in an expression instead
                if kind == BlockKind::Expr {
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

                Statement::Return(value.hir, ctx.trimmed_srcloc(stmt.syntax()))
            }
            AstStmt::AssertStmt(stmt) => {
                let value = if let Some(expr) = stmt.expr() {
                    let value = compile_expr(ctx, &expr, None);
                    ctx.check_condition(expr.syntax(), value.ty);
                    value
                } else {
                    debug!("Unresolved assert expr");
                    ctx.unresolved()
                };

                ctx.push_mappings(value.then_map, stmt.syntax().text_range().end());

                Statement::Assert(value.hir, ctx.trimmed_srcloc(stmt.syntax()))
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
                    ctx.trimmed_srcloc(stmt.syntax()),
                )
            }
            AstStmt::DebugStmt(stmt) => {
                let value = if let Some(expr) = stmt.expr() {
                    compile_expr(ctx, &expr, None)
                } else {
                    debug!("Unresolved print expr");
                    ctx.unresolved()
                };

                Statement::Debug(value.hir, ctx.trimmed_srcloc(stmt.syntax()))
            }
        };

        statements.push(compiled);
    }

    if return_value.is_none() {
        return_types.push(ctx.builtins().types.nil);
    }

    let return_type = if kind == BlockKind::Statement {
        // A statement block doesn't have a value, so it either exits by returning from the
        // function or raising, or falls through to the rest of the block it's in
        if statements_terminate(&statements) {
            ctx.builtins().types.never
        } else {
            ctx.builtins().types.nil
        }
    } else if return_types.is_empty() {
        ctx.builtins().types.never
    } else if return_types.len() == 1 {
        return_types[0]
    } else {
        ctx.alloc_type(Type::Union(Union::new(return_types)))
    };

    let mut return_value = return_value.map(|value| value.with_type(return_type));

    if return_value.is_none() && require_return {
        ctx.diagnostic(block.syntax(), DiagnosticKind::MissingReturn);
        ctx.note_placeholder();
        return_value = Some(Value::new(ctx.alloc_hir(Hir::Unresolved), return_type));
    }

    let hir = ctx.alloc_hir(Hir::Block(Block {
        statements,
        body: trailing_expr,
    }));

    ctx.revert_mappings(index, range.end());

    // The trailing expression's reference and type guard mappings only describe the
    // block's value if nothing else (such as a `return` statement) could have produced it.
    let definitely_trailing = implicit_return && ctx.return_count() == return_count;

    if kind == BlockKind::Expr {
        ctx.pop_return_target();
    }

    let value = match return_value {
        Some(return_value) if definitely_trailing => return_value.with_hir(hir),
        Some(return_value) => Value::new(hir, return_value.ty),
        None => Value::new(hir, return_type),
    };

    // Blocks aren't always compiled with `compile_expr` (such as function bodies), so their type is
    // recorded here
    ctx.record_value(block.syntax(), value)
}

/// A statement whose branches are in the node, which is an `if` chain or a `match`.
fn control_statement(
    ctx: &mut Compiler,
    syntax: &SyntaxNode,
    hir: HirId,
    inline: bool,
) -> ControlStatement {
    let fallthroughs = control_fallthroughs(ctx, hir);
    let may_return = control_may_return(ctx, hir);

    // Every branch of an `inline` statement is evaluated, so each one that falls through would
    // evaluate the rest of the block, and each statement after it would double that
    if inline && may_return && fallthroughs > 1 {
        ctx.diagnostic(syntax, DiagnosticKind::InlineStatementSharedRest);
    }

    ControlStatement {
        hir,
        fallthroughs,
        may_return,
        inline,
        srcloc: ctx.trimmed_srcloc(syntax),
    }
}
