use crate::{Check, Comparison, ComparisonContext, Constraint, Database, TypeId, compare_types};

pub fn constrain_union(
    db: &mut Database,
    ctx: &mut ComparisonContext,
    from: Vec<TypeId>,
    to: TypeId,
) -> Comparison {
    let mut checks = Vec::new();
    let mut incompatible_types = Vec::new();
    let mut has_constrainable = false;
    let mut has_assignable = false;
    let mut has_castable = false;

    for &from_id in &from {
        let comparison = compare_types(db, ctx, from_id, to);

        match comparison {
            Comparison::Assignable => {
                has_assignable = true;
            }
            Comparison::Castable => {
                has_castable = true;
            }
            Comparison::Constrainable(constraint) => {
                has_constrainable = true;
                incompatible_types.push(from_id);
                checks.push(constraint.check);
            }
            Comparison::Incompatible(check) => {
                incompatible_types.push(from_id);
                checks.push(check);
            }
        }
    }

    if !has_constrainable && !has_castable && !has_assignable && !checks.is_empty() {
        return Comparison::Incompatible(Check::And(checks));
    }

    if !checks.is_empty() {
        let mut constraint = Constraint::new(Check::And(checks));
        if incompatible_types.len() < from.len() {
            constraint = constraint.with_else(if incompatible_types.len() == 1 {
                incompatible_types[0]
            } else {
                db.alloc_type(crate::Type::Union(incompatible_types))
            });
        }
        return Comparison::Constrainable(constraint);
    }

    if has_castable {
        return Comparison::Castable;
    }

    if has_assignable {
        return Comparison::Assignable;
    }

    Comparison::Incompatible(Check::Impossible)
}

pub fn compare_to_union(
    db: &mut Database,
    ctx: &mut ComparisonContext,
    from: TypeId,
    to: Vec<TypeId>,
) -> Comparison {
    let mut result = Comparison::Incompatible(Check::Impossible);

    for id in to {
        let comparison = compare_types(db, ctx, from, id);

        match (result.clone(), comparison) {
            (Comparison::Incompatible(old), Comparison::Incompatible(new)) => {
                result = Comparison::Incompatible(Check::Or(vec![old, new]));
            }
            (Comparison::Assignable, _) => {
                result = Comparison::Assignable;
                break;
            }
            (_, Comparison::Assignable) => {
                result = Comparison::Assignable;
                break;
            }
            (Comparison::Incompatible(_), comparison) => {
                result = comparison;
            }
            (_, Comparison::Castable) => {
                result = Comparison::Castable;
            }
            (Comparison::Castable, Comparison::Constrainable(_) | Comparison::Incompatible(_))
            | (Comparison::Constrainable(_), Comparison::Incompatible(_)) => {}
            (Comparison::Constrainable(old), Comparison::Constrainable(new)) => {
                result = Comparison::Constrainable(Constraint::new(Check::Or(vec![
                    old.check, new.check,
                ])));
            }
        };
    }

    result
}
