use rue_ast::{AstFunctionItem, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{FunctionSymbol, ParameterSymbol, Scope, Symbol, SymbolId};

use crate::{Compiler, compile_block, compile_generic_parameters, compile_type};

pub fn declare_function(ctx: &mut Compiler, function: &AstFunctionItem) -> SymbolId {
    let scope = ctx.alloc_scope(Scope::new());

    let return_type = if let Some(return_type) = function.return_type() {
        compile_type(ctx, &return_type)
    } else {
        ctx.builtins().unresolved.ty
    };

    let vars = if let Some(generic_parameters) = function.generic_parameters() {
        compile_generic_parameters(ctx, scope, &generic_parameters)
    } else {
        vec![]
    };

    let mut parameters = Vec::new();
    let mut param_types = Vec::new();

    ctx.push_scope(scope);

    for parameter in function.parameters() {
        let ty = if let Some(ty) = parameter.ty() {
            compile_type(ctx, &ty)
        } else {
            ctx.builtins().unresolved.ty
        };

        let symbol = ctx.alloc_symbol(Symbol::Parameter(ParameterSymbol {
            name: parameter.name(),
            ty,
        }));

        if let Some(name) = parameter.name() {
            if ctx.scope(scope).symbol(name.text()).is_some() {
                ctx.diagnostic(
                    &name,
                    DiagnosticKind::DuplicateSymbol(name.text().to_string()),
                );
            }

            ctx.scope_mut(scope)
                .insert_symbol(name.text().to_string(), symbol);
        }

        param_types.push(ty);
        parameters.push(symbol);
    }

    ctx.pop_scope();

    let body = ctx.builtins().unresolved.hir;

    let ty = ctx.alloc_type(Type::Fn(FunctionType {
        params: param_types,
        ret: return_type,
    }));

    let symbol = ctx.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: function.name(),
        ty,
        scope,
        vars,
        parameters,
        return_type,
        body,
    }));

    if let Some(name) = function.name() {
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

pub fn compile_function(ctx: &mut Compiler, function: &AstFunctionItem, symbol: SymbolId) {
    let (scope, return_type) = if let Symbol::Function(FunctionSymbol {
        scope, return_type, ..
    }) = ctx.symbol(symbol)
    {
        (*scope, *return_type)
    } else {
        unreachable!();
    };

    ctx.push_scope(scope);

    let resolved_body = if let Some(body) = function.body() {
        let index = ctx.mapping_checkpoint();
        let value = compile_block(ctx, &body, true);
        ctx.assign_type(body.syntax(), value.ty, return_type);
        ctx.revert_mappings(index);
        value
    } else {
        ctx.builtins().unresolved.clone()
    };

    ctx.pop_scope();

    let Symbol::Function(FunctionSymbol { body, .. }) = ctx.symbol_mut(symbol) else {
        unreachable!()
    };

    *body = resolved_body.hir;
}
