use id_arena::Arena;
use indexmap::IndexMap;
use log::trace;

use crate::{
    Alias, BuiltinTypes, Comparison, Struct, Type, TypeId, Union, compare, stringify_impl,
    substitute,
};

pub fn subtract(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    lhs_id: TypeId,
    rhs_id: TypeId,
) -> TypeId {
    let lhs_id = substitute(arena, lhs_id);
    let rhs_id = substitute(arena, rhs_id);
    let lhs_name = stringify_impl(arena, lhs_id, &mut IndexMap::new());
    let rhs_name = stringify_impl(arena, rhs_id, &mut IndexMap::new());
    trace!("Subtracting {lhs_name} from {rhs_name}");
    let result = subtract_impl(arena, builtins, lhs_id, rhs_id);
    let new_name = stringify_impl(arena, result, &mut IndexMap::new());
    trace!("Subtraction from {lhs_name} to {rhs_name} yielded {new_name}");
    result
}

fn subtract_impl(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    lhs_id: TypeId,
    rhs_id: TypeId,
) -> TypeId {
    let mut lhs_variants = variants_of(arena, builtins, lhs_id);

    lhs_variants
        .retain(|variant| compare(arena, builtins, variant.type_id, rhs_id) > Comparison::Cast);

    repackage(arena, lhs_variants)
}

fn repackage(arena: &mut Arena<Type>, variants: Vec<Variant>) -> TypeId {
    let mut collected = vec![];
    let mut semantic_type_id = None;
    let mut leftover = vec![];

    for mut variant in variants {
        if let Some(id) = variant.semantic_type_ids.last().copied() {
            if let Some(semantic_type_id) = semantic_type_id {
                if id == semantic_type_id {
                    variant.semantic_type_ids.pop().unwrap();
                    collected.push(variant);
                } else {
                    leftover.push(variant);
                }
            } else {
                semantic_type_id = Some(id);
                variant.semantic_type_ids.pop().unwrap();
                collected.push(variant);
            }
        } else {
            leftover.push(variant);
        }
    }

    if collected.is_empty() {
        let leftover: Vec<TypeId> = leftover
            .into_iter()
            .map(|variant| variant.type_id)
            .collect();

        if leftover.is_empty() {
            return arena.alloc(Type::Never);
        } else if leftover.len() == 1 {
            return leftover[0];
        }

        return arena.alloc(Type::Union(Union::new(leftover)));
    }

    let semantic_type_id = semantic_type_id.unwrap();

    let inner = repackage(arena, collected);

    let result = match arena[semantic_type_id].clone() {
        Type::Alias(alias) => arena.alloc(Type::Alias(Alias { inner, ..alias })),
        Type::Struct(ty) => arena.alloc(Type::Struct(Struct { inner, ..ty })),
        _ => unreachable!(),
    };

    if leftover.is_empty() {
        return result;
    }

    let leftover = repackage(arena, leftover);

    arena.alloc(Type::Union(Union::new(vec![result, leftover])))
}

struct Variant {
    semantic_type_ids: Vec<TypeId>,
    type_id: TypeId,
}

fn variants_of(arena: &Arena<Type>, builtins: &BuiltinTypes, id: TypeId) -> Vec<Variant> {
    match arena[id].clone() {
        Type::Apply(_) => unreachable!(),
        Type::Ref(id) => variants_of(arena, builtins, id),
        Type::Unresolved | Type::Atom(_) | Type::Pair(_) => vec![Variant {
            semantic_type_ids: vec![],
            type_id: id,
        }],
        Type::Never => vec![],
        Type::Alias(alias) => {
            let mut variants = variants_of(arena, builtins, alias.inner);

            for variant in &mut variants {
                variant.semantic_type_ids.push(id);
            }

            variants
        }
        Type::Struct(ty) => {
            let mut variants = variants_of(arena, builtins, ty.inner);

            for variant in &mut variants {
                variant.semantic_type_ids.push(id);
            }

            variants
        }
        Type::Function(_) | Type::Generic => vec![
            Variant {
                semantic_type_ids: vec![],
                type_id: builtins.atom,
            },
            Variant {
                semantic_type_ids: vec![],
                type_id: builtins.any_pair,
            },
        ],
        Type::Union(ty) => {
            let mut variants = Vec::new();

            for variant in ty.types {
                variants.extend(variants_of(arena, builtins, variant));
            }

            variants
        }
    }
}
