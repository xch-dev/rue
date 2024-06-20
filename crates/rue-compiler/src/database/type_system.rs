use std::collections::HashSet;

use crate::{ty::Type, Comparison, Database, TypeId};
use Comparison::*;

pub trait TypeSystem {
    fn compare_type(&self, lhs: TypeId, rhs: TypeId) -> Comparison;
    fn is_cyclic(&self, type_id: TypeId) -> bool;
}

impl TypeSystem for Database {
    fn compare_type(&self, lhs: TypeId, rhs: TypeId) -> Comparison {
        self.compare_type_visitor(lhs, rhs, &mut HashSet::new())
    }

    fn is_cyclic(&self, type_id: TypeId) -> bool {
        self.is_cyclic_visitor(type_id, &mut HashSet::new())
    }
}

impl Database {
    fn compare_type_visitor(
        &self,
        lhs: TypeId,
        rhs: TypeId,
        visited: &mut HashSet<(TypeId, TypeId)>,
    ) -> Comparison {
        let key = (lhs, rhs);
        if lhs == rhs || !visited.insert(key) {
            return Equal;
        }

        let comparison = match (self.ty(lhs), self.ty(rhs)) {
            // Aliases are already resolved at this point.
            (Type::Alias(..), _) | (_, Type::Alias(..)) => unreachable!(),

            // We should have already given a diagnostic for `Unknown`.
            // So we will go ahead and pretend they are equal to everything.
            (Type::Unknown, _) | (_, Type::Unknown) => Equal,

            // These are of course equal atomic types.
            (Type::Any, Type::Any) => Equal,
            (Type::Int, Type::Int) => Equal,
            (Type::Bool, Type::Bool) => Equal,
            (Type::Bytes, Type::Bytes) => Equal,
            (Type::Bytes32, Type::Bytes32) => Equal,
            (Type::PublicKey, Type::PublicKey) => Equal,
            (Type::Nil, Type::Nil) => Equal,

            // You can cast `Any` to anything, but it's not implicit.
            // So many languages make `Any` implicit and it makes it hard to debug.
            (Type::Any, _) => Castable,

            // You have to explicitly convert between other atom types.
            // This is because the compiled output can change depending on the type.
            (Type::Int, Type::Bytes) => Castable,
            (Type::Bool, Type::Bytes | Type::Int) => Castable,
            (Type::Nil, Type::Bytes | Type::Int) => Castable,
            (Type::PublicKey, Type::Bytes | Type::Int) => Castable,
            (Type::Bytes, Type::Int) => Castable,
            (Type::Bytes32, Type::Int) => Castable,

            // We'll special case `Bytes32` converting to the unsized `Bytes` type.
            (Type::Bytes32, Type::Bytes) => Assignable,

            // Size changing conversions are not possible without a type guard.
            (Type::Bytes | Type::Int, Type::Bytes32) => Superset,
            (Type::Bool | Type::Nil | Type::PublicKey, Type::Bytes32) => Unrelated,
            (Type::Bytes | Type::Int, Type::Bool) => Superset,
            (Type::Bytes32 | Type::Nil | Type::PublicKey, Type::Bool) => Unrelated,

            // These are the variants of the `Optional` type.
            (Type::Nil, Type::Optional(..)) => Assignable,
        };

        visited.remove(&key);
        comparison
    }

    fn is_cyclic_visitor(&self, ty: TypeId, visited_aliases: &mut HashSet<TypeId>) -> bool {
        match self.ty_raw(ty).clone() {
            Type::List(..) => false,
            Type::Pair(left, right) => {
                self.is_cyclic_visitor(left, visited_aliases)
                    || self.is_cyclic_visitor(right, visited_aliases)
            }
            Type::Struct { .. } => false,
            Type::Enum { .. } => false,
            Type::EnumVariant { .. } => false,
            Type::Function { .. } => false,
            Type::Alias(alias) => {
                if !visited_aliases.insert(alias) {
                    return true;
                }
                self.is_cyclic_visitor(alias, visited_aliases)
            }
            Type::Unknown
            | Type::Nil
            | Type::Any
            | Type::Int
            | Type::Bool
            | Type::Bytes
            | Type::Bytes32
            | Type::PublicKey => false,
            Type::Optional(ty) => self.is_cyclic_visitor(ty, visited_aliases),
        }
    }
}
