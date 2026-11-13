use rue_ast::{AstGenericArguments, AstGenericParameters, AstType};
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

    for generic_parameter in generic_parameters.names() {
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

    vars
}

pub fn compile_generic_arguments(
    ctx: &mut Compiler,
    generic_arguments: &AstGenericArguments,
) -> Vec<TypeId> {
    let mut args = Vec::new();

    for generic_argument in generic_arguments.types() {
        args.push(compile_type(ctx, &generic_argument));
    }

    args
}

/// Compiles the generic arguments given to a function, where a `_` (returned as `None`) leaves
/// that generic to be inferred
pub fn compile_function_generic_arguments(
    ctx: &mut Compiler,
    generic_arguments: &AstGenericArguments,
) -> Vec<Option<TypeId>> {
    generic_arguments
        .types()
        .map(|ty| (!is_placeholder(&ty)).then(|| compile_type(ctx, &ty)))
        .collect()
}

fn is_placeholder(ty: &AstType) -> bool {
    let AstType::PathType(path) = ty else {
        return false;
    };

    let mut segments = path.segments();

    segments.next().is_some_and(|segment| {
        segment.separator().is_none()
            && segment.generic_arguments().is_none()
            && segment.name().is_some_and(|name| name.text() == "_")
    }) && segments.next().is_none()
}
