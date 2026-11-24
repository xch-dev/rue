use std::ops::ControlFlow;

use id_arena::Arena;
use indexmap::{IndexSet, indexset};

use crate::{
    AtomRestriction, FieldShape, FunctionType, Pair, Struct, Type, TypeId, Union, Walk, substitute,
    walk_leaves,
};

#[derive(Debug, Clone)]
pub enum Atoms {
    Unrestricted,
    Restricted(IndexSet<AtomRestriction>),
}

// Each of these visits every type in a union once (see `walk_leaves`), so a type that's reached
// again, or a non-contractive cycle, doesn't contribute anything more. If `strict` is true, a
// member that doesn't contribute anything makes the result empty, rather than being left out.

pub fn extract_atoms(arena: &mut Arena<Type>, id: TypeId, strict: bool) -> Option<Atoms> {
    let id = substitute(arena, id);

    let mut result = None;

    let flow = walk_leaves(arena, id, Walk::Leaves, |_, ty, _| {
        let inner = match ty {
            Type::Unresolved => Atoms::Unrestricted,
            Type::Generic(_) | Type::Never | Type::Function(_) | Type::Pair(_) | Type::Any => {
                return if strict {
                    ControlFlow::Break(())
                } else {
                    ControlFlow::Continue(())
                };
            }
            Type::Atom(atom) => atom
                .restriction
                .clone()
                .map_or(Atoms::Unrestricted, |restriction| {
                    Atoms::Restricted(indexset![restriction])
                }),
            Type::Ref(_) | Type::Alias(_) | Type::Struct(_) | Type::Union(_) | Type::Apply(_) => {
                unreachable!()
            }
        };

        match (&mut result, inner) {
            (None, inner) | (Some(_), inner @ Atoms::Unrestricted) => result = Some(inner),
            (Some(Atoms::Unrestricted), _) => {}
            (Some(Atoms::Restricted(restrictions)), Atoms::Restricted(inner)) => {
                restrictions.extend(inner);
            }
        }

        ControlFlow::Continue(())
    });

    match flow {
        ControlFlow::Break(()) => None,
        ControlFlow::Continue(()) => result,
    }
}

pub fn extract_pairs(arena: &mut Arena<Type>, id: TypeId, strict: bool) -> Vec<Pair> {
    let id = substitute(arena, id);
    extract_pairs_impl(arena, id, strict)
}

/// The parts as one type: the part itself if there's only one, and otherwise a union of them.
pub(crate) fn union_of_parts(arena: &mut Arena<Type>, parts: Vec<TypeId>) -> TypeId {
    if parts.len() == 1 {
        parts[0]
    } else {
        arena.alloc(Type::Union(Union::new(parts)))
    }
}

/// The type of each field of a struct, which is the part of its inner type where its shape puts
/// the field, or the union of those parts if the inner type has a union of pairs on the way there.
/// A field is `None` if the inner type isn't a pair on the way to it, which it always is unless
/// the struct was rejected.
pub fn extract_field_types(arena: &mut Arena<Type>, ty: &Struct) -> Vec<Option<TypeId>> {
    let mut types = vec![None; ty.fields.len()];

    let shape = ty.shape();
    let mut parts = vec![(&shape, ty.inner)];

    while let Some((part, id)) = parts.pop() {
        match part {
            FieldShape::Nil => {}
            FieldShape::Field(index) => types[*index] = Some(id),
            FieldShape::Pair(first_part, rest_part) => {
                let pairs = extract_pairs(arena, id, false);

                if pairs.is_empty() {
                    continue;
                }

                let first = union_of_parts(arena, pairs.iter().map(|pair| pair.first).collect());
                let rest = union_of_parts(arena, pairs.iter().map(|pair| pair.rest).collect());

                parts.push((rest_part, rest));
                parts.push((first_part, first));
            }
        }
    }

    types
}

/// The pairs that make up an already substituted type, like `extract_pairs` without `strict`.
pub(crate) fn extract_pairs_substituted(arena: &Arena<Type>, id: TypeId) -> Vec<Pair> {
    extract_pairs_impl(arena, id, false)
}

fn extract_pairs_impl(arena: &Arena<Type>, id: TypeId, strict: bool) -> Vec<Pair> {
    let mut pairs = Vec::new();

    let flow = walk_leaves(arena, id, Walk::Leaves, |_, ty, _| {
        match ty {
            Type::Unresolved | Type::Never => {}
            Type::Generic(_) | Type::Atom(_) | Type::Function(_) | Type::Any => {
                if strict {
                    return ControlFlow::Break(());
                }
            }
            &Type::Pair(pair) => pairs.push(pair),
            Type::Ref(_) | Type::Alias(_) | Type::Struct(_) | Type::Union(_) | Type::Apply(_) => {
                unreachable!()
            }
        }

        ControlFlow::Continue(())
    });

    match flow {
        ControlFlow::Break(()) => Vec::new(),
        ControlFlow::Continue(()) => pairs,
    }
}

pub fn extract_functions(arena: &mut Arena<Type>, id: TypeId, strict: bool) -> Vec<FunctionType> {
    let id = substitute(arena, id);

    let mut functions = Vec::new();

    let flow = walk_leaves(arena, id, Walk::Leaves, |_, ty, _| {
        match ty {
            Type::Unresolved | Type::Never => {}
            Type::Generic(_) | Type::Atom(_) | Type::Pair(_) | Type::Any => {
                if strict {
                    return ControlFlow::Break(());
                }
            }
            Type::Function(function) => functions.push(function.clone()),
            Type::Ref(_) | Type::Alias(_) | Type::Struct(_) | Type::Union(_) | Type::Apply(_) => {
                unreachable!()
            }
        }

        ControlFlow::Continue(())
    });

    match flow {
        ControlFlow::Break(()) => Vec::new(),
        ControlFlow::Continue(()) => functions,
    }
}

/// If every variant of the type is an instance of the same struct (such as a struct and a
/// narrowed copy of it), returns each of those instances.
pub fn extract_structs(arena: &mut Arena<Type>, id: TypeId) -> Option<Vec<Struct>> {
    let id = substitute(arena, id);

    let mut structs = Vec::new();

    let flow = walk_leaves(arena, id, Walk::ToStructs, |_, ty, _| match ty {
        Type::Never => ControlFlow::Continue(()),
        Type::Unresolved
        | Type::Generic(_)
        | Type::Atom(_)
        | Type::Pair(_)
        | Type::Function(_)
        | Type::Any => ControlFlow::Break(()),
        Type::Struct(ty) => {
            structs.push(ty.clone());
            ControlFlow::Continue(())
        }
        Type::Ref(_) | Type::Alias(_) | Type::Union(_) | Type::Apply(_) => unreachable!(),
    });

    if flow.is_break() {
        return None;
    }

    let first = structs.first()?;

    if structs
        .iter()
        .any(|ty| ty.semantic != first.semantic || ty.fields != first.fields)
    {
        return None;
    }

    Some(structs)
}
