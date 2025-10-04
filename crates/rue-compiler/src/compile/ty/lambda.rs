use log::debug;
use rue_ast::AstLambdaType;
use rue_diagnostic::DiagnosticKind;
use rue_hir::Scope;
use rue_types::{FunctionType, Type, TypeId};

use crate::{Compiler, compile_generic_parameters, compile_type};

pub fn compile_lambda_type(ctx: &mut Compiler, lambda: &AstLambdaType) -> TypeId {
    let scope = ctx.alloc_scope(Scope::new());

    if let Some(generic_parameters) = lambda.generic_parameters() {
        compile_generic_parameters(ctx, scope, &generic_parameters);
    }

    ctx.push_scope(scope);

    let mut params = Vec::new();
    let mut nil_terminated = true;

    let len = lambda.parameters().count();

    for (i, param) in lambda.parameters().enumerate() {
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
        } else {
            debug!("Unresolved lambda parameter type");
            ctx.builtins().unresolved.ty
        };

        params.push(ty);
    }

    let ret = if let Some(ty) = lambda.return_type() {
        compile_type(ctx, &ty)
    } else {
        debug!("Unresolved lambda return type");
        ctx.builtins().unresolved.ty
    };

    ctx.pop_scope();

    ctx.alloc_type(Type::Function(FunctionType {
        params,
        nil_terminated,
        ret,
    }))
}
