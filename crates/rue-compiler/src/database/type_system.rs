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

            // We should treat `Bytes32` as a subtype of `Bytes` and therefore equal.
            // `PublicKey` however, can have different meaning, so is not equal.
            // `Bytes` is also not equal to `Bytes32` since it's unsized.
            (Type::Bytes32, Type::Bytes) => Equal,

            // You can cast `Any` to anything, but it's not implicit.
            // So many languages make `Any` implicit and it makes it hard to debug.
            (Type::Any, _) => Castable,

            // However, anything can be assigned to `Any` implicitly.
            (_, Type::Any) => Assignable,

            // You have to explicitly convert between other atom types.
            // This is because the compiled output can change depending on the type.
            (Type::Int, Type::Bytes) => Castable,
            (Type::Bool, Type::Bytes) => Castable,
            (Type::Bool, Type::Int) => Castable,
            (Type::Nil, Type::Bytes) => Castable,
            (Type::Nil, Type::Int) => Castable,
            (Type::Nil, Type::Bool) => Castable,
            (Type::PublicKey, Type::Bytes) => Castable,
            (Type::PublicKey, Type::Int) => Castable,
            (Type::Bytes, Type::Int) => Castable,
            (Type::Bytes32, Type::Int) => Castable,

            // Size changing conversions are not possible without a type guard.
            (Type::Bytes, Type::Bytes32) => Superset,
            (Type::Bytes, Type::PublicKey) => Superset,
            (Type::Bytes, Type::Nil) => Superset,
            (Type::Bytes, Type::Bool) => Superset,
            (Type::Int, Type::Bytes32) => Superset,
            (Type::Int, Type::PublicKey) => Superset,
            (Type::Int, Type::Nil) => Superset,
            (Type::Int, Type::Bool) => Superset,
            (Type::Bool, Type::PublicKey) => Unrelated,
            (Type::Bool, Type::Bytes32) => Unrelated,
            (Type::Bool, Type::Nil) => Unrelated,
            (Type::Nil, Type::Bytes32) => Unrelated,
            (Type::Nil, Type::PublicKey) => Unrelated,
            (Type::PublicKey, Type::Bytes32) => Unrelated,
            (Type::PublicKey, Type::Bool) => Unrelated,
            (Type::PublicKey, Type::Nil) => Unrelated,
            (Type::Bytes32, Type::PublicKey) => Unrelated,
            (Type::Bytes32, Type::Bool) => Unrelated,
            (Type::Bytes32, Type::Nil) => Unrelated,

            // These are the variants of the `Optional` type.
            (Type::Nil, Type::Optional(..)) => Assignable,
            (Type::Optional(lhs), Type::Optional(rhs)) => {
                self.compare_type_visitor(*lhs, *rhs, visited)
            }
            (_, Type::Optional(inner)) => self.compare_type_visitor(lhs, *inner, visited),

            // TODO: Unions would make this more generalized and useful.
            // I should add unions back.
            (Type::Optional(_inner), _) => Unrelated,

            // Compare both sides of a `Pair`.
            (Type::Pair(lhs), Type::Pair(rhs)) => {
                let first = self.compare_type_visitor(lhs.first, rhs.first, visited);
                let rest = self.compare_type_visitor(lhs.rest, rhs.rest, visited);
                first & rest
            }

            // A `Pair` is a valid `List` if its first type is the same as the list's inner type
            // and the rest is also a valid `List` of the same type.
            (Type::Pair(pair), Type::List(inner)) => {
                let inner = self.compare_type_visitor(pair.first, *inner, visited);
                let rest = self.compare_type_visitor(pair.rest, rhs, visited);
                inner & rest
            }

            // Nothing else can be assigned to or from a `Pair`.
            (Type::Pair(..), _) | (_, Type::Pair(..)) => Unrelated,

            // A `List` just compares with the inner type of another `List`.
            (Type::List(lhs), Type::List(rhs)) => self.compare_type_visitor(*lhs, *rhs, visited),

            // `Nil` is a valid list.
            (Type::Nil, Type::List(..)) => Castable,
            (Type::List(..), Type::Nil) => Superset,

            // `List` is not compatible with atoms.
            (Type::Bytes, Type::List(..)) => Unrelated,
            (Type::Bytes32, Type::List(..)) => Unrelated,
            (Type::PublicKey, Type::List(..)) => Unrelated,
            (Type::Int, Type::List(..)) => Unrelated,
            (Type::Bool, Type::List(..)) => Unrelated,
            (Type::List(..), Type::Bytes) => Unrelated,
            (Type::List(..), Type::Bytes32) => Unrelated,
            (Type::List(..), Type::PublicKey) => Unrelated,
            (Type::List(..), Type::Int) => Unrelated,
            (Type::List(..), Type::Bool) => Unrelated,

            // A `Struct` is castable to others only if the fields are identical.
            (Type::Struct(lhs), Type::Struct(rhs)) => {
                if lhs.fields.len() == rhs.fields.len() {
                    let mut result = Castable;
                    for i in 0..lhs.fields.len() {
                        result &= self.compare_type_visitor(lhs.fields[i], rhs.fields[i], visited);
                    }
                    result
                } else {
                    Unrelated
                }
            }
            (Type::Struct(..), _) | (_, Type::Struct(..)) => Unrelated,

            // Enum variants are assignable only to their respective enum.
            (Type::EnumVariant(variant_type), Type::Enum(..)) => {
                if variant_type.enum_type == rhs {
                    Assignable
                } else {
                    Unrelated
                }
            }

            // Enums and their variants are not assignable to anything except themselves.
            (Type::Enum(..), _) | (_, Type::Enum(..)) => Unrelated,
            (Type::EnumVariant(..), _) | (_, Type::EnumVariant(..)) => Unrelated,

            // The parameters and return types of functions are checked.
            // This can be made more flexible later.
            (Type::Function(lhs), Type::Function(rhs)) => {
                if lhs.param_types.len() != rhs.param_types.len() || lhs.rest != rhs.rest {
                    Unrelated
                } else {
                    let mut result =
                        self.compare_type_visitor(lhs.return_type, rhs.return_type, visited);

                    for i in 0..lhs.param_types.len() {
                        result &= self.compare_type_visitor(
                            lhs.param_types[i],
                            rhs.param_types[i],
                            visited,
                        );
                    }

                    result
                }
            }

            // There is likely nothing that should relate to a function.
            (Type::Function(..), _) | (_, Type::Function(..)) => Unrelated,
        };

        visited.remove(&key);
        comparison
    }

    fn is_cyclic_visitor(&self, ty: TypeId, visited_aliases: &mut HashSet<TypeId>) -> bool {
        match self.ty_raw(ty).clone() {
            Type::List(..) => false,
            Type::Pair(pair) => {
                self.is_cyclic_visitor(pair.first, visited_aliases)
                    || self.is_cyclic_visitor(pair.rest, visited_aliases)
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
