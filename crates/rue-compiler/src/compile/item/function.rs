use indexmap::IndexMap;
use log::debug;
use rue_ast::{AstFunctionItem, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Declaration, FunctionKind, FunctionSymbol, ParameterSymbol, Symbol, SymbolId, Test};
use rue_types::{FunctionType, Type};

use crate::{
    Compiler, CompletionContext, SyntaxItem, SyntaxItemKind, compile_block,
    compile_generic_parameters, compile_type, create_binding,
};

pub fn declare_function(ctx: &mut Compiler, function: &AstFunctionItem) -> SymbolId {
    ctx.syntax_map_mut().add_item(SyntaxItem::new(
        SyntaxItemKind::CompletionContext(CompletionContext::Item),
        function.syntax().text_range(),
    ));

    let symbol = ctx.alloc_symbol(Symbol::Unresolved);

    if function.test().is_some() {
        let path = ctx.source().kind.clone();

        ctx.add_test(Test {
            name: function.name().map(|name| name.text().to_string()),
            path,
            symbol,
        });
    }

    ctx.push_declaration(Declaration::Symbol(symbol));

    let scope = ctx.alloc_child_scope();

    let vars = if let Some(generic_parameters) = function.generic_parameters() {
        compile_generic_parameters(ctx, scope, &generic_parameters)
    } else {
        vec![]
    };

    let range = function.syntax().text_range();
    ctx.push_scope(scope, range.start());

    let return_type = if let Some(return_type) = function.return_type() {
        compile_type(ctx, &return_type)
    } else {
        ctx.builtins().nil.ty
    };

    let mut parameters = IndexMap::new();
    let mut param_types = IndexMap::new();
    let mut nil_terminated = true;

    let len = function.parameters().count();

    for (i, parameter) in function.parameters().enumerate() {
        let is_spread = if let Some(spread) = parameter.spread() {
            if i == len - 1 {
                true
            } else {
                ctx.diagnostic(&spread, DiagnosticKind::NonFinalSpread);
                false
            }
        } else {
            false
        };

        if is_spread {
            nil_terminated = false;
        }

        let symbol = ctx.alloc_symbol(Symbol::Unresolved);

        ctx.push_declaration(Declaration::Symbol(symbol));

        let ty = if let Some(ty) = parameter.ty() {
            compile_type(ctx, &ty)
        } else {
            debug!("Unresolved function parameter type");
            ctx.builtins().unresolved.ty
        };

        let source = ctx.source().clone();

        *ctx.symbol_mut(symbol) = Symbol::Parameter(ParameterSymbol {
            name: None,
            source,
            ty,
        });

        let name = parameter
            .binding()
            .map_or(String::new(), |binding| binding.syntax().text().to_string());

        param_types.insert(name.clone(), ty);
        parameters.insert(name, symbol);

        ctx.pop_declaration();
    }

    ctx.pop_scope(range.end());

    let body = ctx.builtins().unresolved.hir;

    let ty = ctx.alloc_type(Type::Function(FunctionType {
        params: param_types,
        nil_terminated,
        ret: return_type,
    }));

    let source = ctx.source().clone();

    *ctx.symbol_mut(symbol) = Symbol::Function(FunctionSymbol {
        name: function.name(),
        source,
        ty,
        scope,
        vars,
        parameters,
        nil_terminated,
        return_type,
        body,
        kind: if function.inline().is_some() {
            FunctionKind::Inline
        } else if function.extern_kw().is_some() {
            FunctionKind::Sequential
        } else {
            FunctionKind::BinaryTree
        },
    });

    if let Some(name) = function.name() {
        if ctx.last_scope().symbol(name.text()).is_some() {
            ctx.diagnostic(
                &name,
                DiagnosticKind::DuplicateSymbol(name.text().to_string()),
            );
        } else {
            ctx.last_scope_mut().insert_symbol(
                name.text().to_string(),
                symbol,
                function.export().is_some(),
            );
        }

        ctx.declaration_span(Declaration::Symbol(symbol), name.text_range());
    }

    ctx.pop_declaration();

    symbol
}

pub fn compile_function(ctx: &mut Compiler, function: &AstFunctionItem, symbol: SymbolId) {
    ctx.push_declaration(Declaration::Symbol(symbol));

    let Symbol::Function(FunctionSymbol {
        scope,
        parameters,
        return_type,
        ..
    }) = ctx.symbol(symbol).clone()
    else {
        unreachable!();
    };

    let range = function.syntax().text_range();
    ctx.push_scope(scope, range.start());

    for (i, parameter) in function.parameters().enumerate() {
        let symbol = parameters[i];

        ctx.push_declaration(Declaration::Symbol(symbol));

        if let Some(binding) = parameter.binding() {
            create_binding(ctx, symbol, &binding);
        }

        ctx.pop_declaration();
    }

    let resolved_body = if let Some(body) = function.body() {
        let value = compile_block(
            ctx,
            &body,
            true,
            Some(return_type),
            function.return_type().is_some(),
        );
        ctx.assign_type(body.syntax(), value.ty, return_type);
        value
    } else {
        debug!("Unresolved function body");
        ctx.builtins().unresolved.clone()
    };

    ctx.pop_scope(range.end());

    let Symbol::Function(FunctionSymbol { body, .. }) = ctx.symbol_mut(symbol) else {
        unreachable!();
    };

    *body = resolved_body.hir;

    ctx.pop_declaration();
}
