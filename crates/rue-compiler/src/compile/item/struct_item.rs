use indexmap::IndexSet;
use log::debug;
use rue_ast::AstStructItem;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Declaration, Scope, ScopeId};
use rue_types::{Pair, Struct, Type, TypeId};

use crate::{Compiler, compile_expr, compile_generic_parameters, compile_type};

pub fn declare_struct_item(ctx: &mut Compiler, struct_item: &AstStructItem) -> (TypeId, ScopeId) {
    let ty = ctx.alloc_type(Type::Unresolved);

    ctx.push_declaration(Declaration::Type(ty));

    let scope = ctx.alloc_scope(Scope::new());

    let generics = if let Some(generic_parameters) = struct_item.generic_parameters() {
        compile_generic_parameters(ctx, scope, &generic_parameters)
    } else {
        vec![]
    };

    let inner = ctx.alloc_type(Type::Unresolved);

    let mut fields = IndexSet::new();
    let mut nil_terminated = true;

    let len = struct_item.fields().count();

    for (i, field) in struct_item.fields().enumerate() {
        let is_spread = if let Some(spread) = field.spread() {
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
        nil_terminated,
    });

    if let Some(name) = struct_item.name() {
        if ctx.last_scope().ty(name.text()).is_some() {
            ctx.diagnostic(
                &name,
                DiagnosticKind::DuplicateType(name.text().to_string()),
            );
        }

        ctx.last_scope_mut().insert_type(
            name.text().to_string(),
            ty,
            struct_item.export().is_some(),
        );
    }

    ctx.pop_declaration();

    (ty, scope)
}

pub fn compile_struct_item(
    ctx: &mut Compiler,
    struct_item: &AstStructItem,
    struct_type: TypeId,
    scope: ScopeId,
) {
    ctx.push_declaration(Declaration::Type(struct_type));

    let nil_terminated = if let Type::Struct(Struct { nil_terminated, .. }) = ctx.ty(struct_type) {
        *nil_terminated
    } else {
        unreachable!()
    };

    ctx.push_scope(scope);

    let mut types = Vec::new();
    let mut names = IndexSet::new();

    for field in struct_item.fields() {
        let expected_type = field.ty().map(|ty| compile_type(ctx, &ty));

        let field_type = if let Some(expr) = field.expr() {
            let value = compile_expr(ctx, &expr, expected_type);

            if let Some(name) = field.name() {
                ctx.insert_default_field(struct_type, name.text().to_string(), value.clone());
            }

            expected_type.unwrap_or(value.ty)
        } else if let Some(ty) = expected_type {
            ty
        } else {
            debug!("Unresolved struct item field expr");
            ctx.builtins().unresolved.ty
        };

        let Some(name) = field.name() else {
            continue;
        };

        if names.insert(name.text().to_string()) {
            types.push(field_type);
        }
    }

    ctx.pop_scope();

    let mut resolved_inner = ctx.builtins().nil.ty;

    for (i, ty) in types.into_iter().rev().enumerate() {
        if !nil_terminated && i == 0 {
            resolved_inner = ty;
        } else {
            resolved_inner = ctx.alloc_type(Type::Pair(Pair::new(ty, resolved_inner)));
        }
    }

    let Type::Struct(Struct { inner, .. }) = ctx.ty(struct_type) else {
        unreachable!()
    };

    let inner = *inner;

    *ctx.ty_mut(inner) = Type::Ref(resolved_inner);

    ctx.pop_declaration();
}
