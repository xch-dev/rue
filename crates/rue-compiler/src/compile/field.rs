use std::collections::HashSet;

use rue_diagnostic::DiagnosticKind;
use rue_hir::{FieldAccess, FieldKind, Hir, HirId, TypePath, Value};
use rue_lint::FieldRead;
use rue_types::{Struct, Type, TypeId, Union};

use crate::{Compiler, GetTextRange};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Field<'a> {
    Named(&'a str),
    First,
    Rest,
}

#[derive(Debug, Clone)]
pub enum FieldResult {
    Value(Value),
    Unknown,
    Error,
}

pub fn compile_pair_fields(
    ctx: &mut Compiler,
    node: &impl GetTextRange,
    expr: &Value,
) -> (Value, Value) {
    let first = compile_field(ctx, expr.clone(), &Field::First);
    let rest = compile_field(ctx, expr.clone(), &Field::Rest);

    let (FieldResult::Value(first), FieldResult::Value(rest)) = (first, rest) else {
        let name = ctx.type_name(expr.ty);
        ctx.diagnostic(node, DiagnosticKind::CannotDestructurePair(name));
        return (ctx.unresolved(), ctx.unresolved());
    };

    (first, rest)
}

pub fn compile_field(ctx: &mut Compiler, expr: Value, name: &Field<'_>) -> FieldResult {
    let ty = rue_types::unwrap_semantic(ctx.types_mut(), expr.ty, true);

    // The value's type as the parts that keep the references of untyped lambda parameters' types,
    // if narrowing saw through them (see `narrowed_with_references`)
    let raw = ctx.with_narrowed_references(expr.ty);

    // Narrowing a field of a struct produces a copy of the struct with a narrower inner type, so
    // the field of a union of these copies is the union of the field in each of them
    if matches!(ctx.ty(ty), Type::Union(_))
        && let Some(structs) = rue_types::extract_structs(ctx.types_mut(), ty)
    {
        // The copies as they were before they were substituted, if they're what the value's type
        // is made of, so that their fields keep the references in them (see `wrapped_at`)
        let structs = raw_structs(ctx, raw).unwrap_or(structs);

        let mut result: Option<Value> = None;
        let mut types = Vec::new();

        for ty in structs {
            match compile_struct_field(ctx, expr.clone(), &ty, name) {
                FieldResult::Value(value) => {
                    if !types.contains(&value.ty) {
                        types.push(value.ty);
                    }
                    result.get_or_insert(value);
                }
                result => return result,
            }
        }

        let mut value = result.expect("there is at least one struct");

        if types.len() > 1 {
            value.ty = ctx.alloc_type(Type::Union(Union::new(types)));
        }

        return FieldResult::Value(value);
    }

    match ctx.ty(ty).clone() {
        Type::Apply(_) | Type::Alias(_) | Type::Ref(_) => unreachable!(),
        // An error has already been reported for the unresolved type
        Type::Unresolved => FieldResult::Value(ctx.unresolved()),
        Type::Generic(_) | Type::Function(_) | Type::Never | Type::Any => FieldResult::Unknown,
        Type::Atom(_) => {
            if let &Field::Named(name) = name
                && name == "length"
            {
                let hir = alloc_field_access(ctx, expr.hir, "length", FieldKind::Length);
                FieldResult::Value(Value::new(hir, ctx.builtins().types.int))
            } else {
                FieldResult::Unknown
            }
        }
        Type::Pair(_) | Type::Union(_) => {
            let pairs = rue_types::extract_pairs(ctx.types_mut(), ty, true);

            match name {
                Field::Named("first") | Field::First if !pairs.is_empty() => {
                    let hir = alloc_field_access(
                        ctx,
                        expr.hir,
                        "first",
                        FieldKind::Path(vec![TypePath::First]),
                    );

                    let first = if pairs.len() == 1 {
                        wrapped_at(ctx, raw, &[TypePath::First]).unwrap_or(pairs[0].first)
                    } else if let Some(first) = wrapped_union_at(ctx, raw, TypePath::First) {
                        first
                    } else {
                        ctx.alloc_type(Type::Union(Union::new(
                            pairs.iter().map(|pair| pair.first).collect(),
                        )))
                    };

                    let mut value = Value::new(hir, first);

                    if let Some(mut reference) = expr.reference {
                        reference.path.push(TypePath::First);
                        value = value.with_reference(reference);
                    }

                    FieldResult::Value(value)
                }
                Field::Named("rest") | Field::Rest if !pairs.is_empty() => {
                    let hir = alloc_field_access(
                        ctx,
                        expr.hir,
                        "rest",
                        FieldKind::Path(vec![TypePath::Rest]),
                    );

                    let rest = if pairs.len() == 1 {
                        wrapped_at(ctx, raw, &[TypePath::Rest]).unwrap_or(pairs[0].rest)
                    } else if let Some(rest) = wrapped_union_at(ctx, raw, TypePath::Rest) {
                        rest
                    } else {
                        ctx.alloc_type(Type::Union(Union::new(
                            pairs.iter().map(|pair| pair.rest).collect(),
                        )))
                    };

                    let mut value = Value::new(hir, rest);

                    if let Some(mut reference) = expr.reference {
                        reference.path.push(TypePath::Rest);
                        value = value.with_reference(reference);
                    }

                    FieldResult::Value(value)
                }
                _ => FieldResult::Unknown,
            }
        }
        Type::Struct(ty) => {
            // The struct as it was before it was substituted, so that its fields keep the
            // references in it (see `wrapped_at`)
            let raw = unwrap_raw(ctx, raw);
            let ty = match ctx.ty(raw) {
                Type::Struct(raw_struct) if keeps_reference(ctx, raw) => raw_struct.clone(),
                _ => ty,
            };

            compile_struct_field(ctx, expr, &ty, name)
        }
    }
}

fn compile_struct_field(
    ctx: &mut Compiler,
    expr: Value,
    ty: &Struct,
    name: &Field<'_>,
) -> FieldResult {
    let &Field::Named(name) = name else {
        return FieldResult::Unknown;
    };

    let Some(index) = ty.fields.get_index_of(name) else {
        return FieldResult::Unknown;
    };

    if let Some(struct_name) = &ty.name {
        ctx.lint_sites_mut().field_reads.push(FieldRead {
            struct_name: struct_name.clone(),
            field: name.to_string(),
        });
    }

    // The struct's underlying type couldn't be resolved (for example, because its declaration was
    // rejected), which has already been reported
    if ctx.is_unresolved(ty.inner) {
        return FieldResult::Value(ctx.unresolved());
    }

    let mut path = Vec::new();
    let mut field_type = ty.inner;
    let mut reference = expr.reference;

    let needs_first = index + 1 < ty.fields.len() || ty.nil_terminated;

    for i in 0..index {
        path.push(TypePath::Rest);

        let pairs = rue_types::extract_pairs(ctx.types_mut(), field_type, true);

        if pairs.is_empty() || (pairs.len() > 1 && (i + 1 < index || needs_first)) {
            return FieldResult::Error;
        }

        field_type = if pairs.len() == 1 {
            pairs[0].rest
        } else {
            ctx.alloc_type(Type::Union(Union::new(
                pairs.into_iter().map(|pair| pair.rest).collect(),
            )))
        };

        if let Some(reference) = reference.as_mut() {
            reference.path.push(TypePath::Rest);
        }
    }

    if needs_first {
        path.push(TypePath::First);

        let pairs = rue_types::extract_pairs(ctx.types_mut(), field_type, true);

        if pairs.is_empty() {
            return FieldResult::Error;
        }

        field_type = if pairs.len() == 1 {
            pairs[0].first
        } else {
            ctx.alloc_type(Type::Union(Union::new(
                pairs.into_iter().map(|pair| pair.first).collect(),
            )))
        };

        if let Some(reference) = reference.as_mut() {
            reference.path.push(TypePath::First);
        }
    }

    // The field's type in the struct's type as it was before it was substituted, if that's the
    // struct given, which the references are in
    let field_type = wrapped_at(ctx, ty.inner, &path).unwrap_or(field_type);
    let hir = alloc_field_access(ctx, expr.hir, name, FieldKind::Path(path));
    let mut value = Value::new(hir, field_type);

    if let Some(reference) = reference {
        value = value.with_reference(reference);
    }

    FieldResult::Value(value)
}

fn alloc_field_access(ctx: &mut Compiler, value: HirId, name: &str, kind: FieldKind) -> HirId {
    ctx.alloc_hir(Hir::FieldAccess(FieldAccess {
        value,
        name: name.to_string(),
        kind,
    }))
}

/// The part of the type at the path, through its pairs, if it keeps a reference that an untyped
/// lambda parameter's type is made of (see `ParameterWrap`), so that the lambda's return type still
/// shows where the parameter flows (#362, step 13e). Substituting the type would see through the
/// reference.
fn wrapped_at(ctx: &Compiler, ty: TypeId, path: &[TypePath]) -> Option<TypeId> {
    let mut ty = ty;

    for step in path {
        let Type::Pair(pair) = ctx.ty(unwrap_raw(ctx, ty)) else {
            return None;
        };

        ty = match step {
            TypePath::First => pair.first,
            TypePath::Rest => pair.rest,
        };
    }

    keeps_reference(ctx, ty).then_some(ty)
}

/// The union of the parts of the pairs that the type is a union of, as they were before they were
/// substituted, if it's a union of pairs with a reference that an untyped lambda parameter's type
/// is made of in them (see `wrapped_at`).
fn wrapped_union_at(ctx: &mut Compiler, ty: TypeId, step: TypePath) -> Option<TypeId> {
    let parts = raw_members(ctx, ty)?
        .into_iter()
        .map(|member| match ctx.ty(member) {
            Type::Pair(pair) => Some(match step {
                TypePath::First => pair.first,
                TypePath::Rest => pair.rest,
            }),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;

    Some(ctx.alloc_type(Type::Union(Union::new(parts))))
}

/// The structs that the type is a union of, as they were before they were substituted, if it's a
/// union of structs with a reference that an untyped lambda parameter's type is made of in them.
fn raw_structs(ctx: &Compiler, ty: TypeId) -> Option<Vec<Struct>> {
    raw_members(ctx, ty)?
        .into_iter()
        .map(|member| match ctx.ty(member) {
            Type::Struct(ty) => Some(ty.clone()),
            _ => None,
        })
        .collect()
}

/// The members of the union the type is, through the aliases and unions in it, as they were before
/// they were substituted, if it has a reference that an untyped lambda parameter's type is made of
/// in it (see `wrapped_at`).
fn raw_members(ctx: &Compiler, ty: TypeId) -> Option<Vec<TypeId>> {
    if !keeps_reference(ctx, ty) {
        return None;
    }

    let Type::Union(union) = ctx.ty(unwrap_raw(ctx, ty)) else {
        return None;
    };

    let mut members = Vec::new();
    let mut stack: Vec<TypeId> = union.types.iter().rev().copied().collect();
    let mut visited = HashSet::new();

    while let Some(member) = stack.pop() {
        let member = unwrap_raw(ctx, member);

        if !visited.insert(member) {
            continue;
        }

        match ctx.ty(member) {
            Type::Union(union) => stack.extend(union.types.iter().rev().copied()),
            _ => members.push(member),
        }
    }

    Some(members)
}

/// Records the type a value of `from` is narrowed to as the parts of it that keep the references
/// that an untyped lambda parameter's type is made of (see `ParameterWrap`), if `narrowed` sees
/// through them. Narrowing substitutes what it narrows, so `list` in the `else` of `if list is nil`
/// with `list: List<A>` would lose `A`'s. The value keeps the narrowed type, so that it's named and
/// inferred from as it would be otherwise, and only its field accesses read the parts (see
/// `Compiler::add_narrowed_reference`), which are the same type.
///
/// `replaced` is `from` narrowed the same way, which keeps the references outside of the parts it
/// narrows, such as the rest of a struct when one of its fields is narrowed. If it doesn't keep any,
/// the parts are the members of `from` that `narrowed` is made of, with a member that's kept whole
/// staying as it is, alias and all. Nothing is recorded unless the parts are the same type as
/// `narrowed` and keep a reference that it doesn't already have.
pub(crate) fn narrowed_with_references(
    ctx: &mut Compiler,
    from: TypeId,
    replaced: TypeId,
    narrowed: TypeId,
) {
    if !keeps_reference(ctx, from) || keeps_reference(ctx, narrowed) {
        return;
    }

    let candidate = if keeps_reference(ctx, replaced) {
        replaced
    } else {
        let mut kept = Vec::new();
        let mut stack = vec![from];
        let mut visited = HashSet::new();

        while let Some(part) = stack.pop() {
            if !visited.insert(part) {
                continue;
            }

            if ctx.is_assignable(part, narrowed) {
                kept.push(part);
            } else if let Type::Union(union) = ctx.ty(unwrap_raw(ctx, part)) {
                stack.extend(union.types.iter().rev().copied());
            }
        }

        match kept.as_slice() {
            [] => return,
            [part] => *part,
            _ => ctx.alloc_type(Type::Union(Union::new(kept))),
        }
    };

    if keeps_reference(ctx, candidate)
        && ctx.is_assignable(candidate, narrowed)
        && ctx.is_assignable(narrowed, candidate)
    {
        ctx.add_narrowed_reference(narrowed, candidate);
    }
}

/// The type that an alias, or a reference that isn't an untyped lambda parameter's (such as where a
/// recursive type refers to itself), stands for, without substituting it, so that the references in
/// it are kept (see `wrapped_at`).
fn unwrap_raw(ctx: &Compiler, ty: TypeId) -> TypeId {
    let mut ty = ty;
    let mut visited = HashSet::new();

    while visited.insert(ty) {
        ty = match ctx.ty(ty) {
            Type::Alias(alias) => alias.inner,
            Type::Ref(inner) if !ctx.is_reported_as_another_type(ty) => *inner,
            _ => break,
        };
    }

    ty
}

/// Whether the type has a reference that an untyped lambda parameter's type is made of in it, through
/// the pairs, unions, structs and aliases it's made of, which are what field accesses reach.
fn keeps_reference(ctx: &Compiler, ty: TypeId) -> bool {
    let mut visited = HashSet::new();
    let mut stack = vec![ty];

    while let Some(ty) = stack.pop() {
        if !visited.insert(ty) {
            continue;
        }

        if ctx.is_reported_as_another_type(ty) {
            return true;
        }

        match ctx.ty(ty) {
            Type::Pair(pair) => stack.extend([pair.first, pair.rest]),
            Type::Union(union) => stack.extend(union.types.iter().copied()),
            Type::Struct(ty) => stack.push(ty.inner),
            Type::Alias(alias) => stack.push(alias.inner),
            &Type::Ref(inner) => stack.push(inner),
            _ => {}
        }
    }

    false
}
