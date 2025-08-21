use crate::{
    Check, ComparisonContext, Constraint, Database, Type, TypeId, TypePath, compare_atoms,
    compare_to_union, constrain_union, unwrap_type,
};

#[derive(Debug, Clone)]
pub enum Comparison {
    Assignable,
    Castable,
    Constrainable(Constraint),
    Incompatible(Check),
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
            Comparison::Incompatible(..) => ComparisonKind::Incompatible,
        }
    }
}

pub fn compare_types(
    db: &mut Database,
    ctx: &mut ComparisonContext,
    from_id: TypeId,
    to_id: TypeId,
) -> Comparison {
    let from_id = unwrap_type(db, ctx, from_id);
    let to_id = unwrap_type(db, ctx, to_id);

    match (db.ty(from_id).clone(), db.ty(to_id).clone()) {
        (Type::Alias(_), _) | (_, Type::Alias(_)) => unreachable!(),
        (Type::Unresolved, _) | (_, Type::Unresolved) => Comparison::Assignable,
        (Type::Generic(_), _) => Comparison::Incompatible(Check::Impossible),
        (_, Type::Generic(_)) => {
            ctx.infer(to_id, from_id);
            Comparison::Assignable
        }
        (Type::Fn(from), Type::Fn(to)) => {
            if from.params.len() != to.params.len() {
                return Comparison::Incompatible(Check::Impossible);
            }

            let mut result = compare_types(db, ctx, from.ret, to.ret);

            for (from_param, to_param) in from.params.into_iter().zip(to.params.into_iter()) {
                let comparison = compare_types(db, ctx, from_param, to_param);

                result = match (result, comparison) {
                    (Comparison::Assignable, Comparison::Assignable) => Comparison::Assignable,
                    (Comparison::Castable, Comparison::Castable) => Comparison::Castable,
                    (Comparison::Constrainable(..), _) | (_, Comparison::Constrainable(..)) => {
                        Comparison::Incompatible(Check::Impossible)
                    }
                    (Comparison::Assignable, Comparison::Castable)
                    | (Comparison::Castable, Comparison::Assignable) => Comparison::Castable,
                    (Comparison::Incompatible(..), _) | (_, Comparison::Incompatible(..)) => {
                        Comparison::Incompatible(Check::Impossible)
                    }
                };
            }

            result
        }
        (Type::Fn(..), _) | (_, Type::Fn(..)) => Comparison::Incompatible(Check::Impossible),
        (Type::Union(ids), _) => constrain_union(db, ctx, ids, to_id),
        (_, Type::Union(ids)) => compare_to_union(db, ctx, from_id, ids),
        (Type::Atom(..), Type::Pair(..)) => Comparison::Incompatible(Check::IsPair),
        (Type::Pair(..), Type::Atom(..)) => Comparison::Incompatible(Check::IsAtom),
        (Type::Pair(from_first, from_rest), Type::Pair(to_first, to_rest)) => {
            ctx.push(TypePath::First);
            let first = compare_types(db, ctx, from_first, to_first);
            ctx.pop();

            ctx.push(TypePath::Rest);
            let rest = compare_types(db, ctx, from_rest, to_rest);
            ctx.pop();

            match (first, rest) {
                (Comparison::Assignable, Comparison::Assignable) => Comparison::Assignable,
                (Comparison::Castable, Comparison::Castable) => Comparison::Castable,
                (Comparison::Assignable, Comparison::Castable)
                | (Comparison::Castable, Comparison::Assignable) => Comparison::Castable,
                (Comparison::Constrainable(first), Comparison::Constrainable(rest)) => {
                    // TODO: Else?
                    Comparison::Constrainable(Constraint::new(Check::Pair(
                        Some(Box::new(first.check)),
                        Some(Box::new(rest.check)),
                    )))
                }
                (
                    Comparison::Constrainable(constraint),
                    Comparison::Assignable | Comparison::Castable,
                ) => {
                    let check = constraint.check.clone();
                    Comparison::Constrainable(Constraint::new(Check::Pair(
                        Some(Box::new(check)),
                        None,
                    )))
                }
                (
                    Comparison::Assignable | Comparison::Castable,
                    Comparison::Constrainable(constraint),
                ) => {
                    let check = constraint.check.clone();
                    Comparison::Constrainable(Constraint::new(Check::Pair(
                        None,
                        Some(Box::new(check)),
                    )))
                }
                (Comparison::Incompatible(first), Comparison::Incompatible(rest)) => {
                    Comparison::Incompatible(Check::Pair(
                        Some(Box::new(first)),
                        Some(Box::new(rest)),
                    ))
                }
                (Comparison::Incompatible(kind), _) => {
                    Comparison::Incompatible(Check::Pair(Some(Box::new(kind)), None))
                }
                (_, Comparison::Incompatible(kind)) => {
                    Comparison::Incompatible(Check::Pair(None, Some(Box::new(kind))))
                }
            }
        }
        (Type::Atom(from), Type::Atom(to)) => compare_atoms(from, to),
    }
}
