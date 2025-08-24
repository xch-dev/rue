use rue_ast::{AstFunctionCallExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Hir, Value};
use rue_types::{Type, Union};

use crate::{Compiler, compile_expr};

pub fn compile_function_call_expr(ctx: &mut Compiler, call: &AstFunctionCallExpr) -> Value {
    let Some(expr) = call.expr() else {
        return ctx.builtins().unresolved.clone();
    };

    let expr = compile_expr(ctx, &expr, None);

    let expected_functions = rue_types::extract_functions(ctx.types_mut(), expr.ty);

    if expected_functions.len() > 1 {
        let name = ctx.type_name(expr.ty);
        ctx.diagnostic(
            call.syntax(),
            DiagnosticKind::CannotDisambiguateFunctionTypes(name),
        );
    }

    if expected_functions.is_empty() {
        let name = ctx.type_name(expr.ty);
        ctx.diagnostic(call.syntax(), DiagnosticKind::InvalidFunctionCall(name));
    }

    let mut args = Vec::new();

    for (i, arg) in call.args().enumerate() {
        let expected_type = if !expected_functions.is_empty()
            && let params = expected_functions
                .iter()
                .filter_map(|function| function.params.get(i).copied())
                .collect::<Vec<_>>()
            && !params.is_empty()
        {
            Some(if params.len() == 1 {
                params[0]
            } else {
                ctx.alloc_type(Type::Union(Union::new(params)))
            })
        } else {
            None
        };

        let value = if let Some(expr) = arg.expr() {
            compile_expr(ctx, &expr, expected_type)
        } else {
            ctx.builtins().unresolved.clone()
        };

        args.push(value.hir);
    }

    let hir = ctx.alloc_hir(Hir::FunctionCall(expr.hir, args));

    let ty = if expected_functions.is_empty() {
        ctx.builtins().unresolved.ty
    } else if expected_functions.len() == 1 {
        expected_functions[0].ret
    } else {
        ctx.alloc_type(Type::Union(Union::new(
            expected_functions
                .iter()
                .map(|function| function.ret)
                .collect(),
        )))
    };

    Value::new(hir, ty)
}
