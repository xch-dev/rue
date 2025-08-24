use rue_ast::{AstConstantItem, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{ConstantSymbol, Symbol, SymbolId};

use crate::{Compiler, compile_expr, compile_type};

pub fn declare_constant(ctx: &mut Compiler, constant: &AstConstantItem) -> SymbolId {
    let ty = if let Some(ty) = constant.ty() {
        compile_type(ctx, &ty)
    } else {
        ctx.builtins().unresolved.ty
    };

    let value = ctx.builtins().unresolved.hir;

    let symbol = ctx.alloc_symbol(Symbol::Constant(ConstantSymbol {
        name: constant.name(),
        ty,
        value,
        inline: constant.inline().is_some(),
    }));

    if let Some(name) = constant.name() {
        if ctx.last_scope().symbol(name.text()).is_some() {
            ctx.diagnostic(
                &name,
                DiagnosticKind::DuplicateSymbol(name.text().to_string()),
            );
        }

        ctx.last_scope_mut()
            .insert_symbol(name.text().to_string(), symbol);
    }

    symbol
}

pub fn compile_constant(ctx: &mut Compiler, constant: &AstConstantItem, symbol: SymbolId) {
    let ty = if let Symbol::Constant(ConstantSymbol { ty, .. }) = ctx.symbol(symbol) {
        *ty
    } else {
        unreachable!();
    };

    let resolved_value = if let Some(expr) = constant.value() {
        let value = compile_expr(ctx, &expr, Some(ty));
        ctx.assign_type(expr.syntax(), value.ty, ty);
        value
    } else {
        ctx.builtins().unresolved.clone()
    };

    let Symbol::Constant(ConstantSymbol { value, .. }) = ctx.symbol_mut(symbol) else {
        unreachable!();
    };

    *value = resolved_value.hir;
}
