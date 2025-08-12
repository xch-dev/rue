use rue_ast::{AstLetStmt, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{BindingSymbol, Statement, Symbol};

use crate::{Compiler, compile_expr, compile_type};

pub fn compile_let_binding(ctx: &mut Compiler, stmt: &AstLetStmt) -> Statement {
    let value = if let Some(expr) = stmt.value() {
        compile_expr(ctx, &expr)
    } else {
        ctx.diagnostic(stmt.syntax(), DiagnosticKind::MissingLetValue);
        ctx.builtins().unresolved.clone()
    };

    let ty = if let Some(ty) = stmt.ty() {
        let ty = compile_type(ctx, &ty);
        if let Some(expr) = stmt.value() {
            ctx.assign_type(expr.syntax(), value.hir, value.ty, ty);
        }
        ty
    } else {
        value.ty
    };

    let symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
        name: stmt.name(),
        ty,
        value: value.hir,
    }));

    if let Some(name) = stmt.name() {
        if ctx.last_scope().symbol(name.text()).is_some() {
            ctx.diagnostic(
                &name,
                DiagnosticKind::DuplicateSymbol(name.text().to_string()),
            );
        }

        ctx.last_scope_mut()
            .insert_symbol(name.text().to_string(), symbol);
    }

    Statement::Let(symbol)
}
