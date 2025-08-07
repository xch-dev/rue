use crate::{
    AstGenericParameters, AstNode, Context, DiagnosticKind, ScopeId, Type, TypeId, VarType,
};

pub fn compile_generic_parameters(
    ctx: &mut Context,
    scope: ScopeId,
    generic_parameters: &AstGenericParameters,
) -> Vec<TypeId> {
    let mut vars = Vec::new();

    let mut is_empty = true;

    for generic_parameter in generic_parameters.names() {
        is_empty = false;

        let ty = ctx.alloc_type(Type::Var(VarType {
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
