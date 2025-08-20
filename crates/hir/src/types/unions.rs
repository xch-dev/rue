use crate::{Check, Comparison, ComparisonContext, Constraint, Database, TypeId, compare_types};

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
            (Comparison::Incompatible(_), comparison) => {
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
