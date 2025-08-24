use indexmap::IndexSet;
use rue_ast::AstStructItem;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Scope, ScopeId};
use rue_types::{Pair, Struct, Type, TypeId};

use crate::{Compiler, compile_generic_parameters, compile_type};

pub fn declare_struct_item(ctx: &mut Compiler, struct_item: &AstStructItem) -> (TypeId, ScopeId) {
    let scope = ctx.alloc_scope(Scope::new());

    let generics = if let Some(generic_parameters) = struct_item.generic_parameters() {
        compile_generic_parameters(ctx, scope, &generic_parameters)
    } else {
        vec![]
    };

    let inner = ctx.alloc_type(Type::Unresolved);
    let ty = ctx.alloc_type(Type::Unresolved);

    let mut fields = IndexSet::new();

    for field in struct_item.fields() {
        if let Some(name) = field.name()
            && !fields.insert(name.text().to_string())
        {
            ctx.diagnostic(
                &name,
                DiagnosticKind::DuplicateField(name.text().to_string()),
            );
        }
    }

    *ctx.ty_mut(ty) = Type::Struct(Struct {
        semantic: ty,
        inner,
        name: struct_item.name(),
        generics,
        fields,
    });

    if let Some(name) = struct_item.name() {
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

pub fn compile_struct_item(
    ctx: &mut Compiler,
    struct_item: &AstStructItem,
    ty: TypeId,
    scope: ScopeId,
) {
    ctx.push_scope(scope);

    let mut types = Vec::new();
    let mut names = IndexSet::new();

    for field in struct_item.fields() {
        let ty = if let Some(ty) = field.ty() {
            compile_type(ctx, &ty)
        } else {
            ctx.builtins().unresolved.ty
        };

        let Some(name) = field.name() else {
            continue;
        };

        if names.insert(name.text().to_string()) {
            types.push(ty);
        }
    }

    ctx.pop_scope();

    let mut resolved_inner = ctx.builtins().nil.ty;

    for ty in types.into_iter().rev() {
        resolved_inner = ctx.alloc_type(Type::Pair(Pair::new(ty, resolved_inner)));
    }

    let Type::Struct(Struct { inner, .. }) = ctx.ty(ty) else {
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
