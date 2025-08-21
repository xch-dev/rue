use rue_ast::{AstGenericArguments, AstGenericParameters, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Generic, ScopeId, Type, TypeId};

use crate::{Compiler, compile_type};

pub fn compile_generic_parameters(
    ctx: &mut Compiler,
    scope: ScopeId,
    generic_parameters: &AstGenericParameters,
) -> Vec<TypeId> {
    let mut vars = Vec::new();

    let mut is_empty = true;

    for generic_parameter in generic_parameters.names() {
        is_empty = false;

        let ty = ctx.alloc_type(Type::Generic(Generic {
            name: Some(generic_parameter.clone()),
        }));

        if ctx.scope(scope).ty(generic_parameter.text()).is_some() {
            ctx.diagnostic(
                &generic_parameter,
                DiagnosticKind::DuplicateType(generic_parameter.text().to_string()),
            );
        }

        ctx.scope_mut(scope)
            .insert_type(generic_parameter.text().to_string(), ty);

        vars.push(ty);
    }

    if is_empty {
        ctx.diagnostic(
            generic_parameters.syntax(),
            DiagnosticKind::EmptyGenericParameters,
        );
    }

    vars
}

pub fn compile_generic_arguments(
    ctx: &mut Compiler,
    generic_arguments: &AstGenericArguments,
) -> Vec<TypeId> {
    let mut args = Vec::new();

    let mut is_empty = true;

    for generic_argument in generic_arguments.types() {
        is_empty = false;
        args.push(compile_type(ctx, &generic_argument));
    }

    if is_empty {
        ctx.diagnostic(
            generic_arguments.syntax(),
            DiagnosticKind::EmptyGenericArguments,
        );
    }

    args
}
