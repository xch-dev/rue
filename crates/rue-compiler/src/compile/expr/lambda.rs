use log::debug;
use rue_ast::{AstLambdaExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{FunctionKind, FunctionSymbol, Hir, ParameterSymbol, Scope, Symbol, Value};
use rue_types::{FunctionType, Type, TypeId, Union};

use crate::{Compiler, compile_expr, compile_generic_parameters, compile_type, create_binding};

pub fn compile_lambda_expr(
    ctx: &mut Compiler,
    expr: &AstLambdaExpr,
    expected_type: Option<TypeId>,
) -> Value {
    let expected_functions = if let Some(ty) = expected_type {
        rue_types::extract_functions(ctx.types_mut(), ty)
    } else {
        vec![]
    };

    let scope = ctx.alloc_scope(Scope::new());

    let vars = if let Some(generic_parameters) = expr.generic_parameters() {
        compile_generic_parameters(ctx, scope, &generic_parameters)
    } else {
        vec![]
    };

    ctx.push_scope(scope);

    let mut parameters = Vec::new();
    let mut params = Vec::new();
    let mut nil_terminated = true;

    let len = expr.parameters().count();

    for (i, param) in expr.parameters().enumerate() {
        let is_spread = if let Some(spread) = param.spread() {
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

        let ty = if let Some(ty) = param.ty() {
            compile_type(ctx, &ty)
        } else if !expected_functions.is_empty()
            && let params = expected_functions
                .iter()
                .filter_map(|function| function.params.get(i).copied())
                .collect::<Vec<_>>()
            && !params.is_empty()
        {
            if params.len() == 1 {
                params[0]
            } else {
                ctx.alloc_type(Type::Union(Union::new(params)))
            }
        } else {
            debug!("Unresolved lambda parameter type due to missing inference");
            ctx.diagnostic(param.syntax(), DiagnosticKind::CannotInferParameterType);
            ctx.builtins().unresolved.ty
        };

        let symbol = ctx.alloc_symbol(Symbol::Parameter(ParameterSymbol { name: None, ty }));

        if let Some(binding) = param.binding() {
            create_binding(ctx, symbol, &binding);
        }

        parameters.push(symbol);
        params.push(ty);
    }

    let return_type = expr.ty().map(|ty| compile_type(ctx, &ty));

    let body = if let Some(body) = expr.body() {
        let result = compile_expr(ctx, &body, return_type);
        if let Some(return_type) = return_type {
            ctx.assign_type(expr.syntax(), result.ty, return_type);
        }
        result
    } else {
        debug!("Unresolved lambda body");
        ctx.builtins().unresolved.clone()
    };

    let return_type = return_type.unwrap_or(body.ty);

    let ty = ctx.alloc_type(Type::Function(FunctionType {
        params,
        nil_terminated,
        ret: return_type,
    }));

    ctx.pop_scope();

    let symbol = ctx.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: None,
        ty,
        scope,
        vars,
        parameters,
        nil_terminated,
        return_type,
        body: body.hir,
        kind: FunctionKind::Sequential,
    }));

    let hir = ctx.alloc_hir(Hir::Lambda(symbol));

    Value::new(hir, ty)
}
