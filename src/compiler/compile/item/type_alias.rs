use crate::{
    AstTypeAliasItem, BindingType, Context, DiagnosticKind, Scope, Type, TypeId,
    compile_generic_parameters, compile_type,
};

pub fn declare_type_alias(ctx: &mut Context, type_alias: &AstTypeAliasItem) -> TypeId {
    let scope = ctx.alloc_scope(Scope::new());

    let vars = if let Some(generic_parameters) = type_alias.generic_parameters() {
        compile_generic_parameters(ctx, scope, &generic_parameters)
    } else {
        vec![]
    };

    let parent = ctx.builtins().unresolved.ty;

    let ty = ctx.alloc_type(Type::Binding(BindingType {
        name: type_alias.name(),
        scope,
        vars,
        parent,
        is_transparent: true,
        has_constraint: false,
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

pub fn compile_type_alias(ctx: &mut Context, type_alias: &AstTypeAliasItem, ty: TypeId) {
    let scope = if let Type::Binding(BindingType { scope, .. }) = ctx.ty(ty) {
        *scope
    } else {
        unreachable!();
    };

    ctx.push_scope(scope);

    let resolved_parent = if let Some(parent) = type_alias.ty() {
        compile_type(ctx, &parent)
    } else {
        ctx.builtins().unresolved.ty
    };

    ctx.pop_scope();

    let Type::Binding(BindingType { parent, .. }) = ctx.ty_mut(ty) else {
        unreachable!()
    };

    *parent = resolved_parent;
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::compiler::compile::tests::check;

    #[test]
    fn test_type_alias() {
        check(
            "type Alias = Int;",
            expect!["Undeclared type `Int` at 1:14"],
        );
    }
}
