use log::debug;
use rue_ast::{AstLetStmt, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{BindingSymbol, Statement, Symbol};

use crate::{Compiler, compile_expr, compile_type};

pub fn compile_let_binding(ctx: &mut Compiler, stmt: &AstLetStmt) -> Statement {
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

    let symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
        name: stmt.name(),
        ty,
        value: value.hir,
        inline: stmt.inline().is_some(),
    }));

    if let Some(name) = stmt.name() {
        ctx.last_scope_mut()
            .insert_symbol(name.text().to_string(), symbol, false);
    }

    Statement::Let(symbol)
}
