use indexmap::IndexSet;
use log::debug;
use rue_ast::{AstNode, AstStructItem, AstTypeItem};
use rue_diagnostic::{DiagnosticKind, Name};
use rue_hir::{Declaration, ScopeId};
use rue_types::{Generic, Pair, Struct, Type, TypeId};

use super::{check_regular, report_and_break_cycle};
use crate::{
    Compiler, CompletionContext, SyntaxField, SyntaxItemKind, compile_expr,
    compile_generic_parameters, compile_type,
};

pub fn declare_struct_item(ctx: &mut Compiler, struct_item: &AstStructItem) -> (TypeId, ScopeId) {
    ctx.add_syntax(
        SyntaxItemKind::CompletionContext(CompletionContext::Item),
        struct_item.syntax().text_range(),
    );

    let ty = ctx.alloc_type(Type::Unresolved);

    ctx.push_declaration(Declaration::Type(ty));

    let scope = ctx.alloc_child_scope();

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

    let name = struct_item.name().map(|name| ctx.local_name(&name));

    *ctx.ty_mut(ty) = Type::Struct(Struct {
        name,
        semantic: ty,
        inner,
        generics,
        fields,
        nil_terminated,
        narrowed: false,
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

        ctx.declaration_span(Declaration::Type(ty), name.text_range());
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

    let range = struct_item.syntax().text_range();
    ctx.push_scope(scope, range.start());

    let mut types = Vec::new();
    let mut names = IndexSet::new();
    let mut field_types = Vec::new();

    for field in struct_item.fields() {
        let field_type = if let Some(ty) = field.ty() {
            compile_type(ctx, &ty)
        } else if field.expr().is_some() {
            // The field's type is inferred from its default value, which isn't compiled until all
            // types have been resolved. Until then, it's an opaque type that's only assignable to
            // and from itself, so that nothing can depend on what it will be.
            ctx.alloc_type(Type::Generic(Generic {
                name: Some(Name::new("{inferred}", None)),
            }))
        } else {
            debug!("Unresolved struct item field type");
            ctx.builtins().unresolved.ty
        };

        field_types.push(field_type);

        let Some(name) = field.name() else {
            continue;
        };

        if field.expr().is_some() && field.ty().is_none() {
            ctx.insert_inferred_field(struct_type, name.text().to_string());
        }

        if names.insert(name.text().to_string()) {
            types.push(field_type);
        }

        ctx.add_syntax(
            SyntaxItemKind::FieldDeclaration(SyntaxField {
                name: name.text().to_string(),
                container: struct_type,
                ty: field_type,
            }),
            name.text_range(),
        );
    }

    ctx.pop_scope(range.end());

    ctx.insert_field_types(struct_type, field_types);

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

/// Compiles the default values of a struct's fields. This happens after all types have been
/// resolved, since the default values can refer to types that are declared later. Fields without
/// a type annotation are resolved to the type of their default value.
pub fn compile_struct_defaults(
    ctx: &mut Compiler,
    struct_item: &AstStructItem,
    struct_type: TypeId,
    scope: ScopeId,
) {
    ctx.push_declaration(Declaration::Type(struct_type));

    let range = struct_item.syntax().text_range();
    ctx.push_scope(scope, range.start());

    let field_types = ctx.field_types(struct_type);

    for (field, field_type) in struct_item.fields().zip(field_types) {
        let Some(expr) = field.expr() else {
            continue;
        };

        let expected_type = field.ty().is_some().then_some(field_type);

        let mut value = compile_expr(ctx, &expr, expected_type);

        // The default value must be assignable to the declared field type. If the field type
        // depends on generic parameters, this effectively rejects the default, since a concrete
        // value can't be assigned to an arbitrary generic type.
        if let Some(expected_type) = expected_type
            && !ctx.is_assignable(value.ty, expected_type)
        {
            ctx.assign_type(expr.syntax(), value.ty, expected_type);
            value = ctx.builtins().unresolved.clone();
        }

        // Without a type annotation, the field keeps the exact type of its default value. This is
        // intentional, since fields such as `opcode = 51` are used to distinguish between structs
        // at runtime.
        if expected_type.is_none() {
            *ctx.ty_mut(field_type) = Type::Ref(value.ty);

            // Like any other type item, the struct must be checked for cycles that would prevent
            // it from being substituted or compared, now that its field type is known.
            let item = AstTypeItem::StructItem(struct_item.clone());

            if check_regular(ctx, &item, struct_type)
                && rue_types::is_non_contractive(ctx.types_mut(), field_type)
            {
                report_and_break_cycle(ctx, &item, struct_type, DiagnosticKind::NonContractiveType);
                *ctx.ty_mut(field_type) = Type::Ref(ctx.builtins().unresolved.ty);
            }
        }

        if let Some(name) = field.name() {
            ctx.insert_default_field(struct_type, name.text().to_string(), value);
        }
    }

    ctx.pop_scope(range.end());

    ctx.pop_declaration();
}
