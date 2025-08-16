use rue_ast::AstTypeAliasItem;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Alias, Scope, Type, TypeId};

use crate::{Compiler, compile_generic_parameters, compile_type};

pub fn declare_type_alias(ctx: &mut Compiler, type_alias: &AstTypeAliasItem) -> TypeId {
    let scope = ctx.alloc_scope(Scope::new());

    let vars = if let Some(generic_parameters) = type_alias.generic_parameters() {
        compile_generic_parameters(ctx, scope, &generic_parameters)
    } else {
        vec![]
    };

    let inner = ctx.builtins().unresolved.ty;

    let ty = ctx.alloc_type(Type::Alias(Alias {
        name: type_alias.name(),
        scope,
        vars,
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

    ty
}

pub fn compile_type_alias(ctx: &mut Compiler, type_alias: &AstTypeAliasItem, ty: TypeId) {
    let scope = if let Type::Alias(Alias { scope, .. }) = ctx.ty(ty) {
        *scope
    } else {
        unreachable!();
    };

    ctx.push_scope(scope);

    let resolved_inner = if let Some(inner) = type_alias.ty() {
        compile_type(ctx, &inner)
    } else {
        ctx.builtins().unresolved.ty
    };

    ctx.pop_scope();

    let Type::Alias(Alias { inner, .. }) = ctx.ty_mut(ty) else {
        unreachable!()
    };

    *inner = resolved_inner;
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::compile::tests::check;

    #[test]
    fn test_type_alias() {
        check(
            "type Alias = Int;",
            expect![""],
        );
    }
}
