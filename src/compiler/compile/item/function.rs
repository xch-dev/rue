use crate::{
    AstFunctionItem, BindingSymbol, Context, DiagnosticKind, FunctionSymbol, Scope, Symbol,
    SymbolId, compile_generic_parameters, compile_type,
};

pub fn declare_function(ctx: &mut Context, function: &AstFunctionItem) -> SymbolId {
    let scope = ctx.alloc_scope(Scope::new());

    let return_type = if let Some(return_type) = function.return_type() {
        compile_type(ctx, &return_type)
    } else {
        ctx.builtins().unresolved
    };

    let vars = if let Some(generic_parameters) = function.generic_parameters() {
        compile_generic_parameters(ctx, scope, &generic_parameters)
    } else {
        vec![]
    };

    let mut parameters = Vec::new();

    for parameter in function.parameters() {
        let ty = if let Some(ty) = parameter.ty() {
            compile_type(ctx, &ty)
        } else {
            ctx.builtins().unresolved
        };

        let symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
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

        parameters.push(symbol);
    }

    let symbol = ctx.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: function.name(),
        scope,
        vars,
        parameters,
        return_type,
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

pub fn compile_function(ctx: &mut Context, function: &AstFunctionItem, symbol: SymbolId) {}
