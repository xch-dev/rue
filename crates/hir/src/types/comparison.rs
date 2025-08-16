use std::collections::HashMap;

use crate::{
    BinaryOp, Builtins, ComparisonContext, Constraint, Database, Hir, Type, TypeId, UnaryOp,
    compare_atoms,
};

#[derive(Debug, Clone)]
pub enum Comparison {
    Assignable,
    Castable,
    Constrainable(Constraint),
    Incompatible,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ComparisonKind {
    Assignable,
    Castable,
    Constrainable,
    Incompatible,
}

impl Comparison {
    pub fn kind(&self) -> ComparisonKind {
        match self {
            Comparison::Assignable => ComparisonKind::Assignable,
            Comparison::Castable => ComparisonKind::Castable,
            Comparison::Constrainable(..) => ComparisonKind::Constrainable,
            Comparison::Incompatible => ComparisonKind::Incompatible,
        }
    }
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
                        Comparison::Constrainable(_existing_constraints),
                        Comparison::Constrainable(_new_constraints),
                    ) => {
                        // let or = db.alloc_hir(Hir::Binary(
                        //     BinaryOp::Or,
                        //     *existing_constraints,
                        //     new_constraints,
                        // ));
                        // result = Comparison::Constrainable(or);
                        todo!()
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

            ctx.push_first(first_hir);
            let first = compare_types(db, ctx, builtins, from_first, to_first);
            ctx.pop_hir();

            ctx.push_rest(rest_hir);
            let rest = compare_types(db, ctx, builtins, from_rest, to_rest);
            ctx.pop_hir();

            match (first, rest) {
                (Comparison::Assignable, Comparison::Assignable) => Comparison::Assignable,
                (Comparison::Castable, Comparison::Castable) => Comparison::Castable,
                (Comparison::Constrainable(f), Comparison::Constrainable(r)) => {
                    // TODO: Else map?
                    let and = db.alloc_hir(Hir::Binary(BinaryOp::And, f.hir, r.hir));
                    let mut then_map = HashMap::new();
                    then_map.extend(f.then_map);
                    then_map.extend(r.then_map);
                    Comparison::Constrainable(Constraint::new(and, then_map, HashMap::new()))
                }
                (Comparison::Assignable, Comparison::Castable)
                | (Comparison::Castable, Comparison::Assignable) => Comparison::Castable,
                (
                    Comparison::Assignable | Comparison::Castable,
                    Comparison::Constrainable(constraint),
                )
                | (
                    Comparison::Constrainable(constraint),
                    Comparison::Assignable | Comparison::Castable,
                ) => Comparison::Constrainable(constraint),
                (Comparison::Incompatible, _) | (_, Comparison::Incompatible) => {
                    Comparison::Incompatible
                }
            }
        }
        (Type::Atom(from), Type::Atom(to)) => {
            compare_atoms(db, builtins, ctx.hir(), ctx.path(), to_id, from, to)
        }
    }
}
