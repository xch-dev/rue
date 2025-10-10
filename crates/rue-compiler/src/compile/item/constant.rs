use log::debug;
use rue_ast::{AstConstantItem, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{ConstantSymbol, Declaration, Symbol, SymbolId, Value};

use crate::{Compiler, compile_expr, compile_type};

pub fn declare_constant(ctx: &mut Compiler, constant: &AstConstantItem) -> SymbolId {
    let symbol = ctx.alloc_symbol(Symbol::Unresolved);

    ctx.push_declaration(Declaration::Symbol(symbol));

    let ty = if let Some(ty) = constant.ty() {
        compile_type(ctx, &ty)
    } else {
        debug!("Unresolved constant type");
        ctx.builtins().unresolved.ty
    };

    let value = ctx.builtins().unresolved.hir;

    *ctx.symbol_mut(symbol) = Symbol::Constant(ConstantSymbol {
        name: constant.name(),
        value: Value::new(value, ty),
        inline: constant.inline().is_some(),
    });

    if let Some(name) = constant.name() {
        if ctx.last_scope().symbol(name.text()).is_some() {
            ctx.diagnostic(
                &name,
                DiagnosticKind::DuplicateSymbol(name.text().to_string()),
            );
        }

        ctx.last_scope_mut().insert_symbol(
            name.text().to_string(),
            symbol,
            constant.export().is_some(),
        );

        ctx.declaration_span(Declaration::Symbol(symbol), name.text_range());
    }

    ctx.pop_declaration();

    symbol
}

pub fn compile_constant(ctx: &mut Compiler, constant: &AstConstantItem, symbol: SymbolId) {
    ctx.push_declaration(Declaration::Symbol(symbol));

    let ty = if let Symbol::Constant(ConstantSymbol { value, .. }) = ctx.symbol(symbol) {
        value.ty
    } else {
        unreachable!();
    };

    let resolved_value = if let Some(expr) = constant.value() {
        let value = compile_expr(ctx, &expr, Some(ty));
        ctx.assign_type(expr.syntax(), value.ty, ty);
        value
    } else {
        debug!("Unresolved constant value");
        ctx.builtins().unresolved.clone()
    };

    let Symbol::Constant(ConstantSymbol { value, .. }) = ctx.symbol_mut(symbol) else {
        unreachable!();
    };

    *value = resolved_value.with_type(ty);

    ctx.pop_declaration();
}
