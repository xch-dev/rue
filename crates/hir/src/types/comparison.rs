use std::collections::HashMap;

use crate::{
    BinaryOp, Builtins, ComparisonContext, Constraint, Database, Hir, Type, TypeId, UnaryOp,
    compare_atoms, compare_to_union, constrain_union, unwrap_type,
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
    from_id: TypeId,
    to_id: TypeId,
) -> Comparison {
    let from_id = unwrap_type(db, ctx, from_id);
    let to_id = unwrap_type(db, ctx, to_id);

    match (db.ty(from_id).clone(), db.ty(to_id).clone()) {
        (Type::Alias(_), _) | (_, Type::Alias(_)) => unreachable!(),
        (Type::Unresolved, _) | (_, Type::Unresolved) => Comparison::Assignable,
        (Type::Generic(_), _) => Comparison::Incompatible,
        (_, Type::Generic(_)) => {
            ctx.infer(to_id, from_id);
            Comparison::Assignable
        }
        (_, Type::Union(ids)) => compare_to_union(db, ctx, builtins, from_id, to_id, ids),
        (Type::Union(ids), _) => constrain_union(db, ctx, builtins, to_id, ids),
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
