use std::collections::HashMap;

use rue_mir::{BinaryOp, UnaryOp};

use crate::{Database, Hir, HirId, Type, TypeId, compare_atoms};

#[derive(Debug, Clone)]
pub enum Comparison {
    Assignable,
    Castable,
    Constrainable(HirId),
    Incompatible,
}

pub fn compare_types(
    db: &mut Database,
    hir: HirId,
    from_map: &HashMap<TypeId, TypeId>,
    from_id: TypeId,
    to_map: &HashMap<TypeId, TypeId>,
    to_id: TypeId,
    inferred: &mut HashMap<TypeId, TypeId>,
) -> Comparison {
    if let Some(from_id) = from_map.get(&from_id) {
        return compare_types(db, hir, from_map, *from_id, to_map, to_id, inferred);
    }

    if let Some(to_id) = to_map.get(&to_id) {
        return compare_types(db, hir, from_map, from_id, to_map, *to_id, inferred);
    }

    if let Some(from_id) = inferred.get(&from_id) {
        return compare_types(db, hir, from_map, *from_id, to_map, to_id, inferred);
    }

    if let Some(to_id) = inferred.get(&to_id) {
        return compare_types(db, hir, from_map, from_id, to_map, *to_id, inferred);
    }

    match (db.ty(from_id).clone(), db.ty(to_id).clone()) {
        (Type::Unresolved, _) | (_, Type::Unresolved) => Comparison::Assignable,
        (Type::Generic(_), _) => Comparison::Incompatible,
        (_, Type::Generic(_)) => {
            inferred.insert(to_id, from_id);
            Comparison::Assignable
        }
        (Type::Apply(from_id, from_map), _) => {
            compare_types(db, hir, &from_map, from_id, to_map, to_id, inferred)
        }
        (_, Type::Apply(to_id, to_map)) => {
            compare_types(db, hir, from_map, from_id, &to_map, to_id, inferred)
        }
        (Type::Alias(from), _) => {
            compare_types(db, hir, from_map, from.inner, to_map, to_id, inferred)
        }
        (_, Type::Alias(to)) => {
            compare_types(db, hir, from_map, from_id, to_map, to.inner, inferred)
        }
        (Type::Union(..), _) => todo!(),
        (_, Type::Union(to_ids)) => {
            let mut result = Comparison::Incompatible;

            for to_id in to_ids {
                let comparison = compare_types(db, hir, from_map, from_id, to_map, to_id, inferred);

                match (&mut result, comparison) {
                    (Comparison::Incompatible, comparison) => {
                        result = comparison;
                    }
                    (_, Comparison::Assignable) => {
                        result = Comparison::Assignable;
                        break;
                    }
                    (Comparison::Assignable, _) => {
                        result = Comparison::Assignable;
                        break;
                    }
                    (Comparison::Castable, Comparison::Castable) => {}
                    (Comparison::Constrainable(..), Comparison::Castable) => {
                        result = Comparison::Castable;
                    }
                    (
                        Comparison::Constrainable(existing_constraints),
                        Comparison::Constrainable(new_constraints),
                    ) => {
                        let or = db.alloc_hir(Hir::Binary(
                            BinaryOp::Or,
                            *existing_constraints,
                            new_constraints,
                        ));
                        result = Comparison::Constrainable(or);
                    }
                    (Comparison::Castable, comparison @ Comparison::Constrainable(..)) => {
                        result = comparison;
                    }
                    (_, Comparison::Incompatible) => {}
                };
            }

            result
        }
        (Type::Atom(..), Type::Pair(..)) | (Type::Pair(..), Type::Atom(..)) => {
            Comparison::Incompatible
        }
        (Type::Pair(from_first, from_rest), Type::Pair(to_first, to_rest)) => {
            let first_hir = db.alloc_hir(Hir::Unary(UnaryOp::First, hir));
            let rest_hir = db.alloc_hir(Hir::Unary(UnaryOp::Rest, hir));
            let first = compare_types(
                db, first_hir, from_map, from_first, to_map, to_first, inferred,
            );
            let rest = compare_types(db, rest_hir, from_map, from_rest, to_map, to_rest, inferred);
            match (first, rest) {
                (Comparison::Assignable, Comparison::Assignable) => Comparison::Assignable,
                (Comparison::Castable, Comparison::Castable) => Comparison::Castable,
                (
                    Comparison::Constrainable(first_constraints),
                    Comparison::Constrainable(rest_constraints),
                ) => {
                    let and = db.alloc_hir(Hir::Binary(
                        BinaryOp::And,
                        first_constraints,
                        rest_constraints,
                    ));
                    Comparison::Constrainable(and)
                }
                (Comparison::Assignable, Comparison::Castable)
                | (Comparison::Castable, Comparison::Assignable) => Comparison::Castable,
                (
                    Comparison::Assignable | Comparison::Castable,
                    Comparison::Constrainable(constraints),
                )
                | (
                    Comparison::Constrainable(constraints),
                    Comparison::Assignable | Comparison::Castable,
                ) => Comparison::Constrainable(constraints),
                (Comparison::Incompatible, _) | (_, Comparison::Incompatible) => {
                    Comparison::Incompatible
                }
            }
        }
        (Type::Atom(from), Type::Atom(to)) => compare_atoms(db, hir, from, to),
    }
}
