use std::collections::HashMap;

use rue_hir::{HirId, Type, TypeId};

use crate::Context;

#[derive(Debug, Clone)]
pub enum Comparison {
    Assignable,
    Castable,
    Constrainable(Vec<Constraint>),
    Impossible,
}

#[derive(Debug, Clone, Copy)]
pub struct Constraint {
    pub hir: HirId,
    pub from: TypeId,
    pub to: TypeId,
}

pub fn check_type(
    ctx: &mut Context,
    from_map: &HashMap<TypeId, TypeId>,
    from_id: TypeId,
    to_map: &HashMap<TypeId, TypeId>,
    to_id: TypeId,
    inferred: &mut HashMap<TypeId, TypeId>,
) -> Comparison {
    let from_id = from_map.get(&from_id).copied().unwrap_or(from_id);
    let to_id = to_map.get(&to_id).copied().unwrap_or(to_id);

    match (ctx.ty(from_id), ctx.ty(to_id)) {
        (Type::Unresolved, _) | (_, Type::Unresolved) => Comparison::Assignable,
        (Type::Var(_), _) => unimplemented!(),
        (_, Type::Var(_)) => {
            if let Some(&ty) = inferred.get(&to_id) {
                check_type(ctx, from_map, from_id, &HashMap::new(), ty, inferred)
            } else {
                inferred.insert(to_id, from_id);
                Comparison::Assignable
            }
        }
        (Type::Alias(alias), _) => check_type(ctx, from_map, alias.inner, to_map, to_id, inferred),
        (_, Type::Alias(alias)) => {
            check_type(ctx, from_map, from_id, to_map, alias.inner, inferred)
        }
        (Type::Subtype(from), Type::Subtype(to)) => {}
    }
}
