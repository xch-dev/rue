use std::collections::HashSet;

use indexmap::IndexSet;
use log::debug;
use rue_ast::{AstNode, AstStructField, AstStructItem, AstTypeItem};
use rue_diagnostic::{DiagnosticKind, Name, SpreadTarget};
use rue_hir::{Declaration, Hir, ScopeId};
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

    let (fields, nil_terminated) = declare_fields(
        ctx,
        IndexSet::new(),
        &struct_item.fields().collect::<Vec<_>>(),
    );

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

/// Checks the fields of a struct (or enum variant) for duplicate names and spreads that aren't
/// last, and returns their names after the given ones, and whether the struct is nil terminated
/// (which it isn't if the last field is a spread).
pub(crate) fn declare_fields(
    ctx: &mut Compiler,
    mut names: IndexSet<String>,
    fields: &[AstStructField],
) -> (IndexSet<String>, bool) {
    let mut nil_terminated = true;

    for (i, field) in fields.iter().enumerate() {
        if let Some(spread) = field.spread() {
            if i == fields.len() - 1 {
                nil_terminated = false;
            } else {
                ctx.diagnostic(&spread, DiagnosticKind::NonFinalSpread(SpreadTarget::Field));
            }
        }

        if let Some(name) = field.name()
            && !names.insert(name.text().to_string())
        {
            ctx.diagnostic(
                &name,
                DiagnosticKind::DuplicateField(name.text().to_string()),
            );
        }
    }

    (names, nil_terminated)
}

pub fn compile_struct_item(
    ctx: &mut Compiler,
    struct_item: &AstStructItem,
    struct_type: TypeId,
    scope: ScopeId,
) {
    ctx.push_declaration(Declaration::Type(struct_type));

    let range = struct_item.syntax().text_range();
    ctx.push_scope(scope, range.start());

    let types = compile_fields(ctx, struct_type, struct_item.fields());

    ctx.pop_scope(range.end());

    resolve_struct_inner(ctx, struct_type, types);

    ctx.pop_declaration();
}

/// Compiles the types of the fields of a struct (or enum variant), and returns the type of each
/// field that isn't a duplicate. The types of every field are stored for compiling their default
/// values later (see `compile_field_defaults`).
pub(crate) fn compile_fields(
    ctx: &mut Compiler,
    struct_type: TypeId,
    fields: impl Iterator<Item = AstStructField>,
) -> Vec<TypeId> {
    let mut types = Vec::new();
    let mut names = IndexSet::new();
    let mut field_types = Vec::new();

    for field in fields {
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
            ctx.unresolved().ty
        };

        field_types.push(field_type);

        let Some(name) = field.name() else {
            continue;
        };

        if field.expr().is_some() {
            ctx.insert_declared_default(struct_type, name.text().to_string());

            if field.ty().is_none() {
                ctx.insert_inferred_field(struct_type, name.text().to_string());
            }
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

    ctx.insert_field_types(struct_type, field_types);

    types
}

/// Resolves the type that a struct (or enum variant) with fields of the given types is made of:
/// a list of them, whose last field is the rest of the list if it isn't nil terminated.
pub(crate) fn resolve_struct_inner(ctx: &mut Compiler, struct_type: TypeId, types: Vec<TypeId>) {
    let Type::Struct(Struct {
        inner,
        nil_terminated,
        ..
    }) = *ctx.ty(struct_type)
    else {
        unreachable!()
    };

    let mut resolved_inner = ctx.builtins().nil.ty;

    for (i, ty) in types.into_iter().rev().enumerate() {
        if !nil_terminated && i == 0 {
            resolved_inner = ty;
        } else {
            resolved_inner = ctx.alloc_type(Type::Pair(Pair::new(ty, resolved_inner)));
        }
    }

    *ctx.ty_mut(inner) = Type::Ref(resolved_inner);
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

    compile_field_defaults(
        ctx,
        &AstTypeItem::StructItem(struct_item.clone()),
        struct_type,
        struct_item.fields(),
    );

    ctx.pop_scope(range.end());

    ctx.pop_declaration();
}

/// Compiles the default values of the fields of a struct (or enum variant) in the item, in the
/// item's scope.
pub(crate) fn compile_field_defaults(
    ctx: &mut Compiler,
    item: &AstTypeItem,
    struct_type: TypeId,
    fields: impl Iterator<Item = AstStructField>,
) {
    let field_types = ctx.field_types(struct_type);

    for (field, field_type) in fields.zip(field_types) {
        let Some(expr) = field.expr() else {
            continue;
        };

        let expected_type = field.ty().is_some().then_some(field_type);

        ctx.set_compiling_default(
            field
                .name()
                .map(|name| (struct_type, name.text().to_string())),
        );
        let mut value = compile_expr(ctx, &expr, expected_type);
        ctx.set_compiling_default(None);

        // The default value must be assignable to the declared field type. If the field type
        // depends on generic parameters, this effectively rejects the default, since a concrete
        // value can't be assigned to an arbitrary generic type.
        if let Some(expected_type) = expected_type
            && !ctx.is_assignable(value.ty, expected_type)
        {
            ctx.assign_type(expr.syntax(), value.ty, expected_type);
            value = ctx.unresolved();
        }

        // Without a type annotation, the field keeps the exact type of its default value. This is
        // intentional, since fields such as `opcode = 51` are used to distinguish between structs
        // at runtime.
        if expected_type.is_none() {
            *ctx.ty_mut(field_type) = Type::Ref(value.ty);

            // Like any other type item, the struct must be checked for cycles that would prevent
            // it from being substituted or compared, now that its field type is known.
            if check_regular(ctx, item, struct_type)
                && rue_types::is_non_contractive(ctx.types_mut(), field_type)
            {
                report_and_break_cycle(ctx, item, struct_type, DiagnosticKind::NonContractiveType);
                *ctx.ty_mut(field_type) = Type::Ref(ctx.unresolved().ty);
            }
        }

        if let Some(name) = field.name() {
            ctx.insert_default_field(struct_type, name.text().to_string(), value);
        }
    }
}

/// Fills in the fields of struct initializers in default values whose own defaults hadn't been
/// compiled yet, now that every default value has been. A default value that depends on itself
/// through them is reported, and the field is left unresolved, since it would be infinite.
pub fn resolve_pending_defaults(ctx: &mut Compiler) {
    for pending in ctx.take_pending_defaults() {
        let field = (pending.ty, pending.name.clone());

        if let Some(user) = &pending.user
            && depends_on(ctx, &field, user)
        {
            let type_name = ctx.type_name_in(pending.scope, pending.ty);
            ctx.diagnostic_at(
                pending.srcloc,
                DiagnosticKind::RecursiveFieldDefault(pending.name, type_name),
            );
            ctx.note_placeholder();
            continue;
        }

        let Some(value) = ctx.default_field(pending.ty, &pending.name) else {
            // The default value wasn't compiled, which only happens if the struct has errors, but
            // the field is still reported rather than left unresolved without an error
            let type_name = ctx.type_name_in(pending.scope, pending.ty);
            ctx.diagnostic_at(
                pending.srcloc,
                DiagnosticKind::MissingRequiredFields(type_name, pending.name),
            );
            ctx.note_placeholder();
            continue;
        };

        // Unlike a default that's already compiled, this isn't checked against the field type of
        // the initializer's instantiation, since a default is only kept if it's assignable to
        // the declared field type, which makes it assignable to every instantiation of it
        let Hir::Struct(initializer) = ctx.hir_mut(pending.hir) else {
            unreachable!();
        };

        initializer.fields[pending.index].value = value.hir;
    }
}

/// Whether the default value of the field uses the default value of `target`, directly or
/// through the defaults it uses.
fn depends_on(ctx: &Compiler, field: &(TypeId, String), target: &(TypeId, String)) -> bool {
    let mut visited = HashSet::new();
    let mut stack = vec![field.clone()];

    while let Some(current) = stack.pop() {
        if &current == target {
            return true;
        }

        if visited.insert(current.clone()) {
            stack.extend(ctx.default_uses(&current).iter().cloned());
        }
    }

    false
}
