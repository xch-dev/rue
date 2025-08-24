use id_arena::Arena;
use indexmap::{IndexSet, indexset};

use crate::{AtomRestriction, Type};

#[derive(Debug, Clone)]
pub enum AtomRestrictions {
    Unrestricted,
    Either(IndexSet<AtomRestriction>),
    NotAtom,
}

pub fn atom_restrictions_of(arena: &Arena<Type>, ty: Type) -> AtomRestrictions {
    match ty {
        Type::Unresolved | Type::Apply(_) | Type::Generic => unreachable!(),
        Type::Ref(id) => atom_restrictions_of(arena, arena[id].clone()),
        Type::Alias(alias) => atom_restrictions_of(arena, arena[alias.inner].clone()),
        Type::Struct(ty) => atom_restrictions_of(arena, arena[ty.inner].clone()),
        Type::Atom(atom) => atom
            .restriction
            .map_or(AtomRestrictions::Unrestricted, |restriction| {
                AtomRestrictions::Either(indexset![restriction])
            }),
        Type::Pair(_) => AtomRestrictions::NotAtom,
        Type::Function(_) => AtomRestrictions::Unrestricted,
        Type::Union(ty) => {
            let mut restrictions = IndexSet::new();
            let mut has_atom = false;

            for &id in &ty.types {
                match atom_restrictions_of(arena, arena[id].clone()) {
                    AtomRestrictions::Unrestricted => return AtomRestrictions::Unrestricted,
                    AtomRestrictions::Either(inner) => {
                        for restriction in inner {
                            match &restriction {
                                AtomRestriction::Value(value) => {
                                    if restrictions.contains(&AtomRestriction::Length(value.len()))
                                    {
                                        continue;
                                    }
                                }
                                AtomRestriction::Length(length) => {
                                    restrictions.retain(|restriction| match restriction {
                                        AtomRestriction::Value(value) => value.len() != *length,
                                        AtomRestriction::Length(_) => true,
                                    });
                                }
                            }
                            restrictions.insert(restriction);
                        }
                        has_atom = true;
                    }
                    AtomRestrictions::NotAtom => {}
                }
            }

            if has_atom {
                AtomRestrictions::Either(restrictions)
            } else {
                AtomRestrictions::NotAtom
            }
        }
    }
}
