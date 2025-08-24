use rue_ast::{AstLambdaExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{FunctionSymbol, Hir, ParameterSymbol, Scope, Symbol, Value};
use rue_types::{FunctionType, Type, TypeId, Union};

use crate::{Compiler, compile_expr, compile_type};

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

    ctx.push_scope(scope);

    let mut parameters = Vec::new();
    let mut params = Vec::new();

    for (i, param) in expr.parameters().enumerate() {
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
            ctx.diagnostic(
                param.syntax(),
                DiagnosticKind::CannotInferParameterType(param.name().unwrap().text().to_string()),
            );
            ctx.builtins().unresolved.ty
        };

        let symbol = ctx.alloc_symbol(Symbol::Parameter(ParameterSymbol {
            name: param.name(),
            ty,
        }));

        if let Some(name) = param.name() {
            if ctx.last_scope().symbol(name.text()).is_some() {
                ctx.diagnostic(
                    &name,
                    DiagnosticKind::DuplicateSymbol(name.text().to_string()),
                );
            }

            ctx.last_scope_mut()
                .insert_symbol(name.text().to_string(), symbol);
        }

        parameters.push(symbol);
        params.push(ty);
    }

    let return_type = if let Some(ty) = expr.ty() {
        compile_type(ctx, &ty)
    } else {
        ctx.builtins().unresolved.ty
    };

    let body = if let Some(body) = expr.body() {
        let result = compile_expr(ctx, &body, Some(return_type));
        ctx.assign_type(expr.syntax(), result.ty, return_type);
        result
    } else {
        ctx.builtins().unresolved.clone()
    };

    let ty = ctx.alloc_type(Type::Function(FunctionType {
        params,
        nil_terminated: true,
        ret: return_type,
    }));

    ctx.pop_scope();

    let symbol = ctx.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: None,
        ty,
        scope,
        vars: vec![],
        parameters,
        nil_terminated: true,
        return_type,
        body: body.hir,
        inline: false,
    }));

    let hir = ctx.alloc_hir(Hir::Lambda(symbol));

    Value::new(hir, ty)
}
