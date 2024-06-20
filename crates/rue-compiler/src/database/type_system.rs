use std::collections::HashSet;

use crate::{ty::Type, Database, TypeId};

pub trait TypeSystem {
    fn compare_type(&self, lhs: TypeId, rhs: TypeId) -> bool;
    fn check_type(&self, value_type: TypeId, assign_to: TypeId) -> bool;
    fn can_cast(&self, value_type: TypeId, cast_to: TypeId) -> bool;
    fn is_cyclic(&self, type_id: TypeId) -> bool;
}

impl TypeSystem for Database {
    fn compare_type(&self, a: TypeId, b: TypeId) -> bool {
        self.types_equal_visitor(a, b, &mut HashSet::new())
    }

    fn check_type(&self, from: TypeId, to: TypeId) -> bool {
        self.is_assignable_to(from, to, false, &mut HashSet::new())
    }

    fn can_cast(&self, from: TypeId, to: TypeId) -> bool {
        self.is_assignable_to(from, to, true, &mut HashSet::new())
    }

    fn is_cyclic(&self, type_id: TypeId) -> bool {
        self.detect_cycle(type_id, &mut HashSet::new())
    }
}

impl Database {
    fn is_assignable_to(
        &self,
        a: TypeId,
        b: TypeId,
        cast: bool,
        visited: &mut HashSet<(TypeId, TypeId)>,
    ) -> bool {
        let key = (a, b);

        if a == b || visited.contains(&key) {
            return true;
        }
        visited.insert(key);

        match (self.ty(a).clone(), self.ty(b).clone()) {
            // Primitive types.
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            (Type::Nil, Type::Nil) => true,
            (Type::Any, Type::Any) => true,
            (Type::Int, Type::Int) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Bytes | Type::Nil, Type::Bytes) => true,
            (Type::Bytes32, Type::Bytes32 | Type::Bytes) => true,
            (Type::PublicKey, Type::PublicKey) => true,
            (_, Type::Any) => true,

            // Primitive casts.
            (Type::Nil, Type::Bool | Type::Int) if cast => true,
            (Type::Int, Type::Bytes) if cast => true,
            (Type::Bytes | Type::Bytes32 | Type::PublicKey, Type::Int) if cast => true,
            (Type::PublicKey, Type::Bytes) if cast => true,
            (Type::Bool, Type::Int | Type::Bytes) if cast => true,
            (Type::Any, _) if cast => true,

            // List types with compatible items are also assignable.
            (Type::List(a), Type::List(b)) => self.is_assignable_to(a, b, cast, visited),

            (Type::Pair(a_left, a_right), Type::Pair(b_left, b_right)) => {
                self.is_assignable_to(a_left, b_left, cast, visited)
                    && self.is_assignable_to(a_right, b_right, cast, visited)
            }

            // Enum variants are assignable to their enum type.
            (Type::EnumVariant(enum_variant), _) if b == enum_variant.enum_type() => true,

            // Functions with compatible parameters and return type.
            (Type::Function(fun_a), Type::Function(fun_b)) => {
                if fun_a.parameter_types().len() != fun_b.parameter_types().len() {
                    return false;
                }

                for (a, b) in fun_a
                    .parameter_types()
                    .iter()
                    .zip(fun_b.parameter_types().iter())
                {
                    if !self.is_assignable_to(*a, *b, cast, visited) {
                        return false;
                    }
                }

                self.is_assignable_to(fun_a.return_type(), fun_b.return_type(), cast, visited)
            }

            // Optional types are assignable to themselves.
            (Type::Optional(inner_a), Type::Optional(inner_b)) => {
                self.is_assignable_to(inner_a, inner_b, cast, visited)
            }

            // Either nil or inner type is assignable to optionals.
            (a_type, Type::Optional(inner_b)) => {
                matches!(a_type, Type::Nil) || self.is_assignable_to(a, inner_b, cast, visited)
            }

            _ => false,
        }
    }

    fn types_equal_visitor(
        &self,
        a_id: TypeId,
        b_id: TypeId,
        visited: &mut HashSet<(TypeId, TypeId)>,
    ) -> bool {
        let key = (a_id, b_id);

        if a_id == b_id || visited.contains(&key) {
            return true;
        }
        visited.insert(key);

        let b = self.ty(b_id).clone();

        let equal = match self.ty(a_id).clone() {
            Type::Unknown => matches!(b, Type::Unknown),
            Type::Any => matches!(b, Type::Any),
            Type::Nil => matches!(b, Type::Nil),
            Type::Int => matches!(b, Type::Int),
            Type::Bool => matches!(b, Type::Bool),
            Type::Bytes => matches!(b, Type::Bytes),
            Type::Bytes32 => matches!(b, Type::Bytes32),
            Type::PublicKey => matches!(b, Type::PublicKey),
            Type::Enum(..) | Type::EnumVariant(..) | Type::Struct(..) => a_id == b_id,
            Type::List(inner) => {
                if let Type::List(other_inner) = b {
                    self.types_equal_visitor(inner, other_inner, visited)
                } else {
                    false
                }
            }
            Type::Pair(left, right) => {
                if let Type::Pair(other_left, other_right) = b {
                    self.types_equal_visitor(left, other_left, visited)
                        && self.types_equal_visitor(right, other_right, visited)
                } else {
                    false
                }
            }
            Type::Function(fun) => {
                if let Type::Function(other_fun) = b {
                    if fun.parameter_types().len() != other_fun.parameter_types().len() {
                        return false;
                    }

                    if fun.varargs() != other_fun.varargs() {
                        return false;
                    }

                    for (a, b) in fun
                        .parameter_types()
                        .iter()
                        .zip(other_fun.parameter_types().iter())
                    {
                        if !self.types_equal_visitor(*a, *b, visited) {
                            return false;
                        }
                    }

                    self.types_equal_visitor(fun.return_type(), other_fun.return_type(), visited)
                } else {
                    false
                }
            }
            Type::Alias(..) => unreachable!(),
            Type::Optional(ty) => {
                if let Type::Optional(other_ty) = b {
                    self.types_equal_visitor(ty, other_ty, visited)
                } else {
                    false
                }
            }
        };

        visited.remove(&key);

        equal
    }

    fn detect_cycle(&self, ty: TypeId, visited_aliases: &mut HashSet<TypeId>) -> bool {
        match self.ty_raw(ty).clone() {
            Type::List(..) => false,
            Type::Pair(left, right) => {
                self.detect_cycle(left, visited_aliases)
                    || self.detect_cycle(right, visited_aliases)
            }
            Type::Struct { .. } => false,
            Type::Enum { .. } => false,
            Type::EnumVariant { .. } => false,
            Type::Function { .. } => false,
            Type::Alias(alias) => {
                if !visited_aliases.insert(alias) {
                    return true;
                }
                self.detect_cycle(alias, visited_aliases)
            }
            Type::Unknown
            | Type::Nil
            | Type::Any
            | Type::Int
            | Type::Bool
            | Type::Bytes
            | Type::Bytes32
            | Type::PublicKey => false,
            Type::Optional(ty) => self.detect_cycle(ty, visited_aliases),
        }
    }
}
