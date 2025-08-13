use rue_mir::{BinaryOp, UnaryOp};

use crate::{Builtins, ComparisonContext, Database, Hir, HirId, Type, TypeId, compare_atoms};

#[derive(Debug, Clone)]
pub enum Comparison {
    Assignable,
    Castable,
    Constrainable(HirId),
    Incompatible,
}

pub fn compare_types(
    db: &mut Database,
    ctx: &mut ComparisonContext,
    builtins: &Builtins,
    mut from_id: TypeId,
    mut to_id: TypeId,
) -> Comparison {
    while let Some(new_from_id) = ctx.from(from_id) {
        from_id = new_from_id;
    }

    while let Some(new_to_id) = ctx.to(to_id) {
        to_id = new_to_id;
    }

    if let Some(new_from_id) = ctx.inferred(from_id) {
        from_id = new_from_id;
    }

    if let Some(new_to_id) = ctx.inferred(to_id) {
        to_id = new_to_id;
    }

    match (db.ty(from_id).clone(), db.ty(to_id).clone()) {
        (Type::Unresolved, _) | (_, Type::Unresolved) => Comparison::Assignable,
        (Type::Generic(_), _) => Comparison::Incompatible,
        (_, Type::Generic(_)) => {
            ctx.infer(to_id, from_id);
            Comparison::Assignable
        }
        (Type::Apply(from_id, from_map), _) => {
            ctx.push_from_map(from_map);
            let result = compare_types(db, ctx, builtins, from_id, to_id);
            ctx.pop_from_map();
            result
        }
        (_, Type::Apply(to_id, to_map)) => {
            ctx.push_to_map(to_map);
            let result = compare_types(db, ctx, builtins, from_id, to_id);
            ctx.pop_to_map();
            result
        }
        (Type::Alias(from), _) => compare_types(db, ctx, builtins, from.inner, to_id),
        (_, Type::Alias(to)) => compare_types(db, ctx, builtins, from_id, to.inner),
        (Type::Union(..), _) => todo!(),
        (_, Type::Union(to_ids)) => {
            let mut result = Comparison::Incompatible;

            for to_id in to_ids {
                let comparison = compare_types(db, ctx, builtins, from_id, to_id);

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
            let first_hir = db.alloc_hir(Hir::Unary(UnaryOp::First, ctx.hir()));
            let rest_hir = db.alloc_hir(Hir::Unary(UnaryOp::Rest, ctx.hir()));

            ctx.push_hir(first_hir);
            let first = compare_types(db, ctx, builtins, from_first, to_first);
            ctx.pop_hir();

            ctx.push_hir(rest_hir);
            let rest = compare_types(db, ctx, builtins, from_rest, to_rest);
            ctx.pop_hir();

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
        (Type::Atom(from), Type::Atom(to)) => compare_atoms(db, builtins, ctx.hir(), from, to),
    }
}
