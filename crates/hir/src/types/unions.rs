use crate::{
    BinaryOp, Builtins, Comparison, ComparisonContext, Constraint, Database, Hir, TypeId,
    compare_types,
};

pub fn compare_to_union(
    db: &mut Database,
    ctx: &mut ComparisonContext,
    builtins: &Builtins,
    from_id: TypeId,
    to_id: TypeId,
    ids: Vec<TypeId>,
) -> Comparison {
    let mut result = Comparison::Incompatible;

    for id in ids {
        let comparison = compare_types(db, ctx, builtins, from_id, id);

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
            (Comparison::Constrainable(old), Comparison::Constrainable(new)) => {
                // TODO: Handle multiple constraints
                let or = db.alloc_hir(Hir::Binary(BinaryOp::Or, old.hir, new.hir));
                result = Comparison::Constrainable(Constraint::to(or, ctx.path(), to_id));
            }
            (Comparison::Castable, comparison @ Comparison::Constrainable(..)) => {
                result = comparison;
            }
            (_, Comparison::Incompatible) => {}
        };
    }

    result
}
