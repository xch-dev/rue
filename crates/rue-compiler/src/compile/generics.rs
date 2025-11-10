use rue_ast::{AstGenericArguments, AstGenericParameters, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Declaration, ScopeId};
use rue_types::{Generic, Type, TypeId};

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

        let name = ctx.local_name(&generic_parameter);

        let ty = ctx.alloc_type(Type::Generic(Generic { name: Some(name) }));

        ctx.declaration_span(Declaration::Type(ty), generic_parameter.text_range());

        ctx.push_declaration(Declaration::Type(ty));

        if ctx.scope(scope).ty(generic_parameter.text()).is_some() {
            ctx.diagnostic(
                &generic_parameter,
                DiagnosticKind::DuplicateType(generic_parameter.text().to_string()),
            );
        }

        ctx.scope_mut(scope)
            .insert_type(generic_parameter.text().to_string(), ty, false);

        vars.push(ty);

        ctx.pop_declaration();
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
