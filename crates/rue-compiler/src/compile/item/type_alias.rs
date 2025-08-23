use rue_ast::AstTypeAliasItem;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Scope, ScopeId};
use rue_types::{Alias, Type, TypeId};

use crate::{Compiler, compile_generic_parameters, compile_type};

pub fn declare_type_alias(ctx: &mut Compiler, type_alias: &AstTypeAliasItem) -> (TypeId, ScopeId) {
    let scope = ctx.alloc_scope(Scope::new());

    let generics = if let Some(generic_parameters) = type_alias.generic_parameters() {
        compile_generic_parameters(ctx, scope, &generic_parameters)
    } else {
        vec![]
    };

    let inner = ctx.alloc_type(Type::Unresolved);

    let ty = ctx.alloc_type(Type::Alias(Alias {
        name: type_alias.name(),
        generics,
        inner,
    }));

    if let Some(name) = type_alias.name() {
        if ctx.last_scope().ty(name.text()).is_some() {
            ctx.diagnostic(
                &name,
                DiagnosticKind::DuplicateType(name.text().to_string()),
            );
        }

        ctx.last_scope_mut()
            .insert_type(name.text().to_string(), ty);
    }

    (ty, scope)
}

pub fn compile_type_alias(
    ctx: &mut Compiler,
    type_alias: &AstTypeAliasItem,
    ty: TypeId,
    scope: ScopeId,
) {
    ctx.push_scope(scope);

    let resolved_inner = if let Some(inner) = type_alias.ty() {
        compile_type(ctx, &inner)
    } else {
        ctx.builtins().unresolved.ty
    };

    ctx.pop_scope();

    let Type::Alias(Alias { inner, .. }) = ctx.ty(ty) else {
        unreachable!()
    };

    let resolved_inner = ctx.ty(resolved_inner).clone();
    let inner = *inner;

    *ctx.ty_mut(inner) = resolved_inner;
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::compile::tests::check;

    #[test]
    fn test_type_alias() {
        check("type Alias = Int;", expect![""]);
    }
}
