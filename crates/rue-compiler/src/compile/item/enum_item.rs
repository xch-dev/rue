use std::collections::HashMap;

use indexmap::IndexSet;
use num_bigint::BigInt;
use rue_ast::{AstEnumItem, AstEnumVariant, AstNode, AstTypeItem};
use rue_diagnostic::{DiagnosticKind, Name};
use rue_hir::{
    ConstantSymbol, Declaration, Hir, ModuleDeclarations, ModuleSymbol, Scope, ScopeId,
    StructField, StructInitializer, Symbol, Value,
};
use rue_types::{Alias, Apply, DISCRIMINANT_FIELD, Struct, Type, TypeId, Union};

use super::{
    compile_field_defaults, compile_fields, declare_fields, resolve_alias_inner,
    resolve_struct_inner,
};
use crate::{
    Compiler, CompletionContext, EnumVariant, LiteralValue, SyntaxItemKind,
    compile_generic_parameters, insert_local_module,
};

/// Declares an enum, which is a type alias for the union of its variants. Each variant is a struct
/// whose first field is its discriminant, followed by its own fields. If none of the variants have
/// fields, each variant is only its discriminant instead. The variants are declared in a namespace
/// with the enum's name, along with a constant for each variant without fields.
pub fn declare_enum_item(ctx: &mut Compiler, enum_item: &AstEnumItem) -> (TypeId, ScopeId) {
    ctx.add_syntax(
        SyntaxItemKind::CompletionContext(CompletionContext::Item),
        enum_item.syntax().text_range(),
    );

    let ty = ctx.alloc_type(Type::Unresolved);

    ctx.push_declaration(Declaration::Type(ty));

    let scope = ctx.alloc_child_scope();

    let generics = if let Some(generic_parameters) = enum_item.generic_parameters() {
        compile_generic_parameters(ctx, scope, &generic_parameters)
    } else {
        vec![]
    };

    let namespace = ctx.alloc_scope(Scope::new(None));
    let enum_name = enum_item.name().map(|name| name.text().to_string());

    // Without any fields, each variant is only its discriminant, which is also what makes it a
    // struct that isn't nil terminated (since it's only made of the last field)
    let has_fields = enum_item
        .variants()
        .any(|variant| variant.fields().is_some());

    let mut variants = Vec::new();

    for variant in enum_item.variants() {
        variants.push(declare_variant(
            ctx,
            &variant,
            ty,
            enum_name.as_deref(),
            &generics,
            namespace,
            has_fields,
        ));
    }

    let variant_types = variants
        .iter()
        .flatten()
        .map(|variant| variant.ty)
        .collect();
    ctx.insert_enum(ty, variant_types);
    ctx.insert_declared_variants(ty, variants);

    let inner = ctx.alloc_type(Type::Unresolved);
    let name = enum_item.name().map(|name| ctx.local_name(&name));

    *ctx.ty_mut(ty) = Type::Alias(Alias {
        name: name.clone(),
        inner,
        generics,
    });

    let parent = ctx.current_module();

    let module = ctx.alloc_symbol(Symbol::Module(ModuleSymbol {
        name,
        scope: namespace,
        parent: Some(parent),
        declarations: ModuleDeclarations::default(),
    }));

    ctx.insert_enum_member(Declaration::Symbol(module), ty);
    ctx.insert_reference_target(Declaration::Symbol(module), Declaration::Type(ty));

    if let Some(name) = enum_item.name() {
        let duplicate = ctx.last_scope().ty(name.text()).is_some();

        if duplicate {
            ctx.diagnostic(
                &name,
                DiagnosticKind::DuplicateType(name.text().to_string()),
            );
        }

        let exported = enum_item.export().is_some();

        ctx.last_scope_mut()
            .insert_type(name.text().to_string(), ty, exported);

        // The namespace can only collide with a module (since modules have their own namespace),
        // which is only reported if the enum's name isn't already reported as a duplicate type
        if !duplicate || ctx.last_scope().module(name.text()).is_none() {
            insert_local_module(ctx, &name, module, exported);
        }

        ctx.declaration_span(Declaration::Type(ty), name.text_range());
    }

    ctx.pop_declaration();

    (ty, scope)
}

/// Declares the struct type of a variant, and the constant for it if it has no fields, in the
/// enum's namespace. Returns `None` if the variant has no name, or the name is already taken.
fn declare_variant(
    ctx: &mut Compiler,
    variant: &AstEnumVariant,
    enum_type: TypeId,
    enum_name: Option<&str>,
    generics: &[TypeId],
    namespace: ScopeId,
    has_fields: bool,
) -> Option<EnumVariant> {
    let name = variant.name()?;

    if ctx.scope(namespace).ty(name.text()).is_some() {
        ctx.diagnostic(
            &name,
            DiagnosticKind::DuplicateVariant(name.text().to_string()),
        );
        return None;
    }

    let names = IndexSet::from([DISCRIMINANT_FIELD.to_string()]);

    let (fields, nil_terminated) = if has_fields {
        declare_fields(ctx, names, &variant.struct_fields().collect::<Vec<_>>())
    } else {
        (names, false)
    };

    let ty = ctx.alloc_type(Type::Unresolved);
    let inner = ctx.alloc_type(Type::Unresolved);

    // The variant is named by its path, since it isn't in scope by its own name (the LSP's enum
    // hover takes the variant's own name back from the end of it)
    let text = enum_name.map_or_else(
        || name.text().to_string(),
        |enum_name| format!("{enum_name}::{}", name.text()),
    );
    let srcloc = ctx.srcloc(&name);

    *ctx.ty_mut(ty) = Type::Struct(Struct {
        name: Some(Name::new(text, Some(srcloc))),
        semantic: ty,
        inner,
        generics: generics.to_vec(),
        fields,
        nil_terminated,
        narrowed: false,
    });

    ctx.scope_mut(namespace)
        .insert_type(name.text().to_string(), ty, true);
    ctx.declaration_span(Declaration::Type(ty), name.text_range());

    // Using a variant uses the enum, so that it isn't reported as unused
    ctx.add_reference(Declaration::Type(ty), Declaration::Type(enum_type));
    ctx.insert_enum_member(Declaration::Type(ty), enum_type);

    let constant = variant.fields().is_none().then(|| {
        let constant = ctx.alloc_symbol(Symbol::Unresolved);

        ctx.scope_mut(namespace)
            .insert_symbol(name.text().to_string(), constant, true);
        ctx.add_reference(Declaration::Symbol(constant), Declaration::Type(ty));
        ctx.insert_enum_member(Declaration::Symbol(constant), enum_type);
        ctx.insert_reference_target(Declaration::Symbol(constant), Declaration::Type(ty));

        constant
    });

    Some(EnumVariant { ty, constant })
}

pub fn compile_enum_item(ctx: &mut Compiler, enum_item: &AstEnumItem, ty: TypeId, scope: ScopeId) {
    ctx.push_declaration(Declaration::Type(ty));

    let range = enum_item.syntax().text_range();
    ctx.push_scope(scope, range.start());

    let generics = match ctx.ty(ty) {
        Type::Alias(alias) => alias.generics.clone(),
        _ => unreachable!(),
    };

    let mut discriminants = HashMap::<BigInt, String>::new();
    let mut previous: Option<BigInt> = None;

    for (variant, info) in enum_item.variants().zip(ctx.declared_variants(ty).to_vec()) {
        let discriminant = discriminant(&variant, previous.as_ref());
        previous = Some(discriminant.clone());

        let Some(info) = info else {
            continue;
        };

        let name = variant.name().unwrap();

        if let Some(other) = discriminants.get(&discriminant) {
            let kind = DiagnosticKind::DuplicateDiscriminant(
                name.text().to_string(),
                other.clone(),
                discriminant.to_string(),
            );

            if let Some(value) = variant
                .discriminant()
                .and_then(|discriminant| discriminant.value())
            {
                ctx.diagnostic(&value, kind);
            } else {
                ctx.diagnostic(&name, kind);
            }
        } else {
            discriminants.insert(discriminant.clone(), name.text().to_string());
        }

        let value = LiteralValue::Int(discriminant.clone());
        let discriminant_type = ctx.alloc_type(Type::Atom(value.atom()));
        let discriminant_hir = ctx.alloc_hir(Hir::Int(discriminant));
        let discriminant = Value::new(discriminant_hir, discriminant_type);

        ctx.insert_declared_default(info.ty, DISCRIMINANT_FIELD.to_string());
        ctx.insert_default_field(info.ty, DISCRIMINANT_FIELD.to_string(), discriminant);

        ctx.push_declaration(Declaration::Type(info.ty));
        let mut field_types = vec![discriminant_type];
        field_types.extend(compile_fields(ctx, info.ty, variant.struct_fields()));
        ctx.pop_declaration();

        resolve_struct_inner(ctx, info.ty, field_types);

        if let Some(constant) = info.constant {
            let Type::Struct(Struct { nil_terminated, .. }) = *ctx.ty(info.ty) else {
                unreachable!()
            };

            // A variant without fields is the same value for every instantiation of a generic
            // enum, so its constant uses the variant with none of the generics
            let constant_type = if generics.is_empty() {
                info.ty
            } else {
                let never = ctx.builtins().types.never;
                ctx.alloc_type(Type::Apply(Apply::new(
                    info.ty,
                    generics.iter().map(|&generic| (generic, never)).collect(),
                )))
            };

            let hir = ctx.alloc_hir(Hir::Struct(StructInitializer {
                ty: constant_type,
                fields: vec![StructField {
                    name: DISCRIMINANT_FIELD.to_string(),
                    value: discriminant_hir,
                    is_default: true,
                }],
                nil_terminated,
            }));

            *ctx.symbol_mut(constant) = Symbol::Constant(ConstantSymbol {
                name: Some(ctx.local_name(&name)),
                value: Value::new(hir, constant_type),
                inline: true,
            });
        }
    }

    ctx.pop_scope(range.end());

    let resolved_inner = match ctx.enum_variants(ty).unwrap_or_default() {
        [] => ctx.builtins().types.never,
        &[ty] => ty,
        types => {
            let types = types.to_vec();
            ctx.alloc_type(Type::Union(Union::new(types)))
        }
    };

    resolve_alias_inner(ctx, ty, resolved_inner);

    ctx.pop_declaration();
}

/// The variant's discriminant, which is the one it's given, or one more than the previous
/// variant's (starting from zero).
fn discriminant(variant: &AstEnumVariant, previous: Option<&BigInt>) -> BigInt {
    if let Some(discriminant) = variant.discriminant()
        && let Some(token) = discriminant.value()
        && let LiteralValue::Int(value) = LiteralValue::decode(&token)
    {
        return if discriminant.minus().is_some() {
            -value
        } else {
            value
        };
    }

    previous.map_or(BigInt::ZERO, |previous| previous + 1)
}

/// Compiles the default values of the variants' fields.
pub fn compile_enum_defaults(
    ctx: &mut Compiler,
    enum_item: &AstEnumItem,
    ty: TypeId,
    scope: ScopeId,
) {
    ctx.push_declaration(Declaration::Type(ty));

    let range = enum_item.syntax().text_range();
    ctx.push_scope(scope, range.start());

    let item = AstTypeItem::EnumItem(enum_item.clone());

    for (variant, info) in enum_item.variants().zip(ctx.declared_variants(ty).to_vec()) {
        let Some(info) = info else {
            continue;
        };

        ctx.push_declaration(Declaration::Type(info.ty));
        compile_field_defaults(ctx, &item, info.ty, variant.struct_fields());
        ctx.pop_declaration();
    }

    ctx.pop_scope(range.end());

    ctx.pop_declaration();
}
