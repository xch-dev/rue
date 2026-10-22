use std::collections::HashMap;

use indexmap::IndexMap;
use log::debug;
use rue_ast::{AstExpr, AstLambdaExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{FunctionKind, FunctionSymbol, Hir, ParameterSymbol, Symbol, Value};
use rue_types::{FunctionType, Type, TypeId, Union, substitute_with_mappings};

use crate::{
    BlockKind, Compiler, CompletionContext, ReturnTarget, SyntaxItemKind, compile_block,
    compile_expr, compile_generic_parameters, compile_type, create_binding,
};

pub fn compile_lambda_expr(
    ctx: &mut Compiler,
    expr: &AstLambdaExpr,
    expected_type: Option<TypeId>,
) -> Value {
    let mut expected_functions = if let Some(ty) = expected_type {
        rue_types::extract_functions(ctx.types_mut(), ty)
    } else {
        vec![]
    };

    let scope = ctx.alloc_child_scope();

    let vars = if let Some(generic_parameters) = expr.generic_parameters() {
        compile_generic_parameters(ctx, scope, &generic_parameters)
    } else {
        // If the parameter types come from a generic function type, the lambda is generic over
        // its own copies of that function's generics, so that they don't leak out of it
        let mut vars = Vec::new();

        for function in &mut expected_functions {
            let mut mappings = HashMap::new();

            for &generic in &function.generics {
                let copy = ctx.ty(generic).clone();
                let copy = ctx.alloc_type(copy);
                mappings.insert(generic, copy);
                vars.push(copy);
            }

            for param in function.params.values_mut() {
                *param = substitute_with_mappings(ctx.types_mut(), *param, &mappings);
            }
        }

        vars
    };

    let range = expr.syntax().text_range();
    ctx.push_scope(scope, range.start());

    let mut parameters = IndexMap::new();
    let mut params = IndexMap::new();
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
                .filter_map(|function| function.params.get_index(i).map(|(_, id)| *id))
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

        let name = param
            .binding()
            .map_or(String::new(), |binding| binding.syntax().text().to_string());

        parameters.insert(name.clone(), symbol);
        params.insert(name, ty);
    }

    let return_type = expr.ty().map(|ty| compile_type(ctx, &ty));

    ctx.push_return_target(Some(ReturnTarget {
        expected_type: return_type,
        types: Vec::new(),
    }));

    let body = if let Some(body) = expr.body() {
        let result = if let AstExpr::Block(block) = &body {
            ctx.add_syntax(
                SyntaxItemKind::CompletionContext(CompletionContext::Expression),
                block.syntax().text_range(),
            );
            compile_block(ctx, block, BlockKind::Body, return_type, true)
        } else {
            compile_expr(ctx, &body, return_type)
        };
        if let Some(return_type) = return_type {
            ctx.assign_type(expr.syntax(), result.ty, return_type);
        }
        result
    } else {
        debug!("Unresolved lambda body");
        ctx.builtins().unresolved.clone()
    };

    let early_return_types = ctx
        .pop_return_target()
        .map(|target| target.types)
        .unwrap_or_default();

    // If the return type isn't specified, it's inferred from both the body and any
    // `return` statements that exit the lambda early.
    let return_type = return_type.unwrap_or_else(|| {
        if early_return_types.is_empty() {
            body.ty
        } else {
            let mut types = early_return_types;
            types.push(body.ty);
            ctx.alloc_type(Type::Union(Union::new(types)))
        }
    });

    let ty = ctx.alloc_type(Type::Function(FunctionType {
        generics: vars.clone(),
        params,
        nil_terminated,
        ret: return_type,
    }));

    ctx.pop_scope(range.end());

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
