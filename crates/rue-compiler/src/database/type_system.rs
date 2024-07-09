use std::collections::{HashMap, HashSet};

use crate::{
    value::{EnumType, EnumVariantType, FunctionType, PairType, StructType, Type},
    Comparison, Database, TypeId,
};

#[derive(Debug)]
struct ComparisonContext<'a> {
    visited: HashSet<(TypeId, TypeId)>,
    substitution_stack: &'a mut Vec<HashMap<TypeId, TypeId>>,
    initial_substitution_length: usize,
    generic_stack_frame: Option<usize>,
}

impl<'a> ComparisonContext<'a> {
    fn new(substitution_stack: &'a mut Vec<HashMap<TypeId, TypeId>>, infer_generics: bool) -> Self {
        let generic_stack_frame = if infer_generics {
            Some(substitution_stack.len() - 1)
        } else {
            None
        };
        let initial_substitution_length = substitution_stack.len();

        Self {
            visited: HashSet::new(),
            substitution_stack,
            initial_substitution_length,
            generic_stack_frame,
        }
    }
}

impl Database {
    pub fn compare_type(&self, lhs: TypeId, rhs: TypeId) -> Comparison {
        self.compare_type_visitor(
            lhs,
            rhs,
            &mut ComparisonContext::new(&mut Vec::new(), false),
        )
    }

    pub fn compare_type_with_generics(
        &self,
        lhs: TypeId,
        rhs: TypeId,
        generic_type_stack: &mut Vec<HashMap<TypeId, TypeId>>,
    ) -> Comparison {
        self.compare_type_visitor(
            lhs,
            rhs,
            &mut ComparisonContext::new(generic_type_stack, true),
        )
    }

    pub fn substitute_type(
        &mut self,
        type_id: TypeId,
        substitutions: &HashMap<TypeId, TypeId>,
    ) -> TypeId {
        self.substitute_type_visitor(type_id, substitutions, &mut HashSet::new())
    }

    pub fn first_type(&self, type_id: TypeId) -> Option<TypeId> {
        let Type::Pair(pair) = self.ty(type_id) else {
            return None;
        };
        Some(pair.first)
    }

    pub fn rest_type(&self, type_id: TypeId) -> Option<TypeId> {
        let Type::Pair(pair) = self.ty(type_id) else {
            return None;
        };
        Some(pair.rest)
    }

    pub fn unwrap_optional(&mut self, ty: TypeId) -> TypeId {
        match self.ty(ty) {
            Type::Optional(inner) => self.unwrap_optional(*inner),
            _ => ty,
        }
    }

    pub fn flatten_union(&mut self, type_id: TypeId) -> Vec<TypeId> {
        let mut visited = HashSet::new();
        let mut stack = vec![type_id];
        let mut result = Vec::new();

        while let Some(type_id) = stack.pop() {
            if !visited.insert(type_id) {
                continue;
            }

            match self.ty(type_id) {
                Type::Union(items) => stack.extend(items),
                _ => result.push(type_id),
            }
        }

        result
    }

    #[allow(clippy::match_same_arms, clippy::too_many_lines)]
    fn compare_type_visitor(
        &self,
        lhs: TypeId,
        rhs: TypeId,
        ctx: &mut ComparisonContext<'_>,
    ) -> Comparison {
        let key = (lhs, rhs);
        if lhs == rhs || !ctx.visited.insert(key) {
            return Comparison::Equal;
        }

        let comparison = match (self.ty(lhs), self.ty(rhs)) {
            // References are already resolved at this point.
            (Type::Ref(..), _) | (_, Type::Ref(..)) => unreachable!(),

            // Aliases are already resolved at this point.
            (Type::Alias(..), _) | (_, Type::Alias(..)) => unreachable!(),

            // We need to apply substitutions.
            (Type::Lazy(ty), _) => {
                ctx.substitution_stack.push(ty.substitutions.clone());
                let result = self.compare_type_visitor(ty.type_id, rhs, ctx);
                ctx.substitution_stack.pop().unwrap();
                result
            }

            (_, Type::Lazy(ty)) => {
                ctx.substitution_stack.push(ty.substitutions.clone());
                let result = self.compare_type_visitor(lhs, ty.type_id, ctx);
                ctx.substitution_stack.pop().unwrap();
                result
            }

            // We need to infer `Generic` types, and return `Unrelated` if incompatible.
            (_, Type::Generic) => {
                let mut found = None;

                for substititons in ctx.substitution_stack.iter().rev() {
                    if let Some(&substititon) = substititons.get(&rhs) {
                        found = Some(substititon);
                    }
                }

                if let Some(found) = found {
                    self.compare_type_visitor(lhs, found, ctx)
                } else if let Some(generic_stack_frame) = ctx.generic_stack_frame {
                    ctx.substitution_stack[generic_stack_frame].insert(rhs, lhs);
                    Comparison::Assignable
                } else {
                    Comparison::Unrelated
                }
            }

            (Type::Generic, _) => {
                let mut found = None;

                for (i, substititons) in ctx.substitution_stack.iter().enumerate().rev() {
                    if i < ctx.initial_substitution_length {
                        break;
                    }

                    if let Some(&substititon) = substititons.get(&lhs) {
                        found = Some(substititon);
                    }
                }

                if let Some(found) = found {
                    self.compare_type_visitor(found, rhs, ctx)
                } else {
                    Comparison::Unrelated
                }
            }

            // We should have already given a diagnostic for `Unknown`.
            // So we will go ahead and pretend they are equal to everything.
            (Type::Unknown, _) | (_, Type::Unknown) => Comparison::Equal,

            // Possibly undefined is a special type.
            (Type::Optional(..), _) | (_, Type::Optional(..)) => Comparison::Unrelated,

            // These are of course equal atomic types.
            (Type::Any, Type::Any) => Comparison::Equal,
            (Type::Int, Type::Int) => Comparison::Equal,
            (Type::Bool, Type::Bool) => Comparison::Equal,
            (Type::Bytes, Type::Bytes) => Comparison::Equal,
            (Type::Bytes32, Type::Bytes32) => Comparison::Equal,
            (Type::PublicKey, Type::PublicKey) => Comparison::Equal,
            (Type::Nil, Type::Nil) => Comparison::Equal,

            // We should treat `Bytes32` as a subtype of `Bytes` and therefore equal.
            // `PublicKey` however, can have different meaning, so is not equal.
            // `Bytes` is also not equal to `Bytes32` since it's unsized.
            (Type::Bytes32, Type::Bytes) => Comparison::Equal,

            // You can cast `Any` to anything, but it's not implicit.
            // So many languages make `Any` implicit and it makes it hard to debug.
            (Type::Any, _) => Comparison::Castable,

            // However, anything can be assigned to `Any` implicitly.
            (_, Type::Any) => Comparison::Assignable,

            // You have to explicitly convert between other atom types.
            // This is because the compiled output can change depending on the type.
            (Type::Int, Type::Bytes) => Comparison::Castable,
            (Type::Bool, Type::Bytes) => Comparison::Castable,
            (Type::Bool, Type::Int) => Comparison::Castable,
            (Type::Nil, Type::Int) => Comparison::Castable,
            (Type::Nil, Type::Bool) => Comparison::Castable,
            (Type::PublicKey, Type::Bytes) => Comparison::Castable,
            (Type::PublicKey, Type::Int) => Comparison::Castable,
            (Type::Bytes, Type::Int) => Comparison::Castable,
            (Type::Bytes32, Type::Int) => Comparison::Castable,

            // Let's allow assigning `Nil` to `Bytes` for ease of use.
            // The only alternative without needing a cast is empty strings.
            (Type::Nil, Type::Bytes) => Comparison::Assignable,

            // Size changing conversions are not possible without a type guard.
            (Type::Bytes, Type::Bytes32) => Comparison::Superset,
            (Type::Bytes, Type::PublicKey) => Comparison::Superset,
            (Type::Bytes, Type::Nil) => Comparison::Superset,
            (Type::Bytes, Type::Bool) => Comparison::Superset,
            (Type::Int, Type::Bytes32) => Comparison::Superset,
            (Type::Int, Type::PublicKey) => Comparison::Superset,
            (Type::Int, Type::Nil) => Comparison::Superset,
            (Type::Int, Type::Bool) => Comparison::Superset,
            (Type::Bool, Type::PublicKey) => Comparison::Unrelated,
            (Type::Bool, Type::Bytes32) => Comparison::Unrelated,
            (Type::Bool, Type::Nil) => Comparison::Unrelated,
            (Type::Nil, Type::Bytes32) => Comparison::Unrelated,
            (Type::Nil, Type::PublicKey) => Comparison::Unrelated,
            (Type::PublicKey, Type::Bytes32) => Comparison::Unrelated,
            (Type::PublicKey, Type::Bool) => Comparison::Unrelated,
            (Type::PublicKey, Type::Nil) => Comparison::Unrelated,
            (Type::Bytes32, Type::PublicKey) => Comparison::Unrelated,
            (Type::Bytes32, Type::Bool) => Comparison::Unrelated,
            (Type::Bytes32, Type::Nil) => Comparison::Unrelated,

            // Union types are equal if all of the variants are compatible.
            (Type::Union(items), _) => {
                let mut result = Comparison::Equal;
                for &item in items {
                    result &= self.compare_type_visitor(item, rhs, ctx);
                }
                result
            }

            (_, Type::Union(items)) => {
                let mut result = Comparison::Unrelated;
                for &item in items {
                    result |= self.compare_type_visitor(lhs, item, ctx);
                }
                result
            }

            // Compare both sides of a `Pair`.
            (Type::Pair(lhs), Type::Pair(rhs)) => {
                let first = self.compare_type_visitor(lhs.first, rhs.first, ctx);
                let rest = self.compare_type_visitor(lhs.rest, rhs.rest, ctx);
                first & rest
            }

            // Nothing else can be assigned to or from a `Pair`.
            (Type::Pair(..), _) | (_, Type::Pair(..)) => Comparison::Unrelated,

            // A `Struct` is castable to others only if the fields are identical.
            (Type::Struct(lhs), Type::Struct(rhs)) => {
                if lhs.fields.len() == rhs.fields.len() {
                    let mut result = Comparison::Castable;
                    for i in 0..lhs.fields.len() {
                        result &= self.compare_type_visitor(lhs.fields[i], rhs.fields[i], ctx);
                    }
                    result
                } else {
                    Comparison::Unrelated
                }
            }
            (Type::Struct(..), _) | (_, Type::Struct(..)) => Comparison::Unrelated,

            // Enum variants are assignable only to their respective enum.
            (Type::EnumVariant(variant_type), Type::Enum(..)) => {
                if variant_type.enum_type == rhs {
                    Comparison::Assignable
                } else {
                    Comparison::Unrelated
                }
            }

            // But not the other way around.
            (Type::Enum(..), Type::EnumVariant(variant_type)) => {
                if variant_type.enum_type == lhs {
                    Comparison::Superset
                } else {
                    Comparison::Unrelated
                }
            }

            // You can cast numeric enums to `Int`.
            (Type::EnumVariant(variant_type), Type::Bytes | Type::Int) => {
                self.compare_type_visitor(variant_type.enum_type, rhs, ctx)
            }

            (Type::Enum(enum_type), Type::Bytes | Type::Int) => {
                if enum_type.has_fields {
                    Comparison::Unrelated
                } else {
                    Comparison::Castable
                }
            }

            // Enums and their variants are not assignable to anything else.
            (Type::Enum(..), _) | (_, Type::Enum(..)) => Comparison::Unrelated,
            (Type::EnumVariant(..), _) | (_, Type::EnumVariant(..)) => Comparison::Unrelated,

            // The parameters and return types of functions are checked.
            // This can be made more flexible later.
            (Type::Function(lhs), Type::Function(rhs)) => {
                if lhs.param_types.len() != rhs.param_types.len() || lhs.rest != rhs.rest {
                    Comparison::Unrelated
                } else {
                    let mut result =
                        self.compare_type_visitor(lhs.return_type, rhs.return_type, ctx);

                    for i in 0..lhs.param_types.len() {
                        result &=
                            self.compare_type_visitor(lhs.param_types[i], rhs.param_types[i], ctx);
                    }

                    result
                }
            }

            // There is likely nothing that should relate to a function.
            (Type::Function(..), _) | (_, Type::Function(..)) => Comparison::Unrelated,
        };

        ctx.visited.remove(&key);
        comparison
    }

    fn substitute_type_visitor(
        &mut self,
        type_id: TypeId,
        substitutions: &HashMap<TypeId, TypeId>,
        visited: &mut HashSet<TypeId>,
    ) -> TypeId {
        if let Some(&substitution) = substitutions.get(&type_id) {
            return substitution;
        }

        if !visited.insert(type_id) {
            return type_id;
        }

        match self.ty(type_id).clone() {
            Type::Alias(..) | Type::Ref(..) => unreachable!(),
            Type::Int
            | Type::Any
            | Type::Bool
            | Type::Bytes
            | Type::Bytes32
            | Type::PublicKey
            | Type::Nil
            | Type::Unknown
            | Type::Generic => type_id,
            Type::Lazy(lazy) => {
                let mut new_substitutions = lazy.substitutions;
                new_substitutions.extend(substitutions);
                self.substitute_type_visitor(lazy.type_id, &new_substitutions, visited)
            }
            Type::Union(items) => {
                let new_items = items
                    .iter()
                    .flat_map(|item| {
                        let type_id = self.substitute_type_visitor(*item, substitutions, visited);
                        match self.ty(type_id) {
                            Type::Union(items) => items.clone(),
                            _ => vec![type_id],
                        }
                    })
                    .collect();
                if new_items == items {
                    type_id
                } else {
                    self.alloc_type(Type::Union(new_items))
                }
            }
            Type::Optional(inner_type_id) => {
                let new_inner = self.substitute_type_visitor(inner_type_id, substitutions, visited);
                if new_inner == inner_type_id {
                    type_id
                } else {
                    self.alloc_type(Type::Optional(new_inner))
                }
            }
            Type::Pair(pair) => {
                let new_pair = PairType {
                    first: self.substitute_type_visitor(pair.first, substitutions, visited),
                    rest: self.substitute_type_visitor(pair.rest, substitutions, visited),
                };
                if new_pair == pair {
                    type_id
                } else {
                    self.alloc_type(Type::Pair(new_pair))
                }
            }
            Type::Function(function) => {
                let new_function = FunctionType {
                    generic_types: function.generic_types.clone(),
                    param_types: function
                        .param_types
                        .iter()
                        .map(|param| self.substitute_type_visitor(*param, substitutions, visited))
                        .collect(),
                    return_type: self.substitute_type_visitor(
                        function.return_type,
                        substitutions,
                        visited,
                    ),
                    rest: function.rest,
                };
                if new_function == function {
                    type_id
                } else {
                    self.alloc_type(Type::Function(new_function))
                }
            }
            Type::Struct(struct_type) => {
                let new_struct = StructType {
                    original_type_id: struct_type.original_type_id,
                    fields: struct_type
                        .fields
                        .iter()
                        .map(|(name, field)| {
                            (
                                name.clone(),
                                self.substitute_type_visitor(*field, substitutions, visited),
                            )
                        })
                        .collect(),
                    rest: struct_type.rest,
                };
                if new_struct == struct_type {
                    type_id
                } else {
                    self.alloc_type(Type::Struct(new_struct))
                }
            }
            Type::Enum(enum_type) => {
                let new_enum = EnumType {
                    variants: enum_type
                        .variants
                        .iter()
                        .map(|(name, variant)| {
                            (
                                name.clone(),
                                self.substitute_type_visitor(*variant, substitutions, visited),
                            )
                        })
                        .collect(),
                    has_fields: enum_type.has_fields,
                };
                if new_enum == enum_type {
                    type_id
                } else {
                    self.alloc_type(Type::Enum(new_enum))
                }
            }
            Type::EnumVariant(enum_variant) => {
                let new_variant = EnumVariantType {
                    enum_type: enum_variant.enum_type,
                    original_type_id: enum_variant.original_type_id,
                    fields: enum_variant.fields.as_ref().map(|fields| {
                        fields
                            .iter()
                            .map(|(name, field)| {
                                (
                                    name.clone(),
                                    self.substitute_type_visitor(*field, substitutions, visited),
                                )
                            })
                            .collect()
                    }),
                    discriminant: enum_variant.discriminant.clone(),
                    rest: enum_variant.rest,
                };
                if new_variant == enum_variant {
                    type_id
                } else {
                    self.alloc_type(Type::EnumVariant(new_variant))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use indexmap::IndexMap;
    use num_bigint::BigInt;
    use num_traits::Zero;

    use crate::{
        compiler::{builtins, Builtins},
        scope::Scope,
        value::{
            AliasType, EnumType, EnumVariantType, FunctionType, LazyType, PairType, Rest,
            StructType,
        },
    };

    use super::*;

    fn setup() -> (Database, Builtins) {
        let mut db = Database::new();
        let ty = builtins(&mut db);
        (db, ty)
    }

    fn fields(items: &[TypeId]) -> IndexMap<String, TypeId> {
        let mut fields = IndexMap::new();
        for (i, item) in items.iter().enumerate() {
            fields.insert(format!("field_{i}"), *item);
        }
        fields
    }

    #[test]
    fn test_substitution() {
        let (mut db, ty) = setup();

        let a = db.alloc_type(Type::Generic);
        let b = db.alloc_type(Type::Generic);

        let a_pair = db.alloc_type(Type::Pair(PairType { first: a, rest: a }));

        let function = db.alloc_type(Type::Function(FunctionType {
            param_types: vec![a_pair],
            rest: Rest::Nil,
            return_type: b,
            generic_types: vec![a, b],
        }));

        let int_pair = db.alloc_type(Type::Pair(PairType {
            first: ty.int,
            rest: ty.int,
        }));

        let substitutions = [(a, ty.int), (b, ty.bool)].into_iter().collect();
        let substituted = db.alloc_type(Type::Lazy(LazyType {
            type_id: function,
            substitutions,
        }));

        let expected = db.alloc_type(Type::Function(FunctionType {
            param_types: vec![int_pair],
            rest: Rest::Nil,
            return_type: ty.bool,
            generic_types: vec![a, b],
        }));

        assert_eq!(db.compare_type(substituted, expected), Comparison::Equal);
    }

    #[test]
    fn test_alias_resolution() {
        let (mut db, ty) = setup();

        let scope_id = db.alloc_scope(Scope::default());

        let int_alias = db.alloc_type(Type::Alias(AliasType {
            type_id: ty.int,
            generic_types: Vec::new(),
            scope_id,
        }));
        assert_eq!(db.compare_type(int_alias, ty.int), Comparison::Equal);
        assert_eq!(db.compare_type(ty.int, int_alias), Comparison::Equal);
        assert_eq!(db.compare_type(int_alias, int_alias), Comparison::Equal);

        let double_alias = db.alloc_type(Type::Alias(AliasType {
            type_id: int_alias,
            generic_types: Vec::new(),
            scope_id,
        }));
        assert_eq!(db.compare_type(double_alias, ty.int), Comparison::Equal);
        assert_eq!(db.compare_type(ty.int, double_alias), Comparison::Equal);
        assert_eq!(
            db.compare_type(double_alias, double_alias),
            Comparison::Equal
        );
        assert_eq!(db.compare_type(double_alias, int_alias), Comparison::Equal);
        assert_eq!(db.compare_type(int_alias, double_alias), Comparison::Equal);
    }

    #[test]
    fn test_generic_type() {
        let (mut db, ty) = setup();
        let a = db.alloc_type(Type::Generic);
        let b = db.alloc_type(Type::Generic);
        assert_eq!(db.compare_type(a, b), Comparison::Unrelated);
        assert_eq!(db.compare_type(a, ty.int), Comparison::Unrelated);
        assert_eq!(db.compare_type(ty.int, a), Comparison::Unrelated);
        assert_eq!(db.compare_type(a, a), Comparison::Equal);
    }

    #[test]
    fn test_any_type() {
        let (db, ty) = setup();
        assert_eq!(db.compare_type(ty.any, ty.int), Comparison::Castable);
        assert_eq!(db.compare_type(ty.any, ty.any), Comparison::Equal);
        assert_eq!(db.compare_type(ty.int, ty.any), Comparison::Assignable);
    }

    #[test]
    fn test_unknown_type() {
        let (db, ty) = setup();
        assert_eq!(db.compare_type(ty.any, ty.unknown), Comparison::Equal);
        assert_eq!(db.compare_type(ty.int, ty.unknown), Comparison::Equal);
        assert_eq!(db.compare_type(ty.unknown, ty.int), Comparison::Equal);
        assert_eq!(db.compare_type(ty.unknown, ty.any), Comparison::Equal);
    }

    #[test]
    fn test_atom_types() {
        let (db, ty) = setup();
        assert_eq!(db.compare_type(ty.bytes, ty.bytes32), Comparison::Superset);
        assert_eq!(db.compare_type(ty.bytes32, ty.bytes), Comparison::Equal);
        assert_eq!(
            db.compare_type(ty.bytes, ty.public_key),
            Comparison::Superset
        );
        assert_eq!(
            db.compare_type(ty.public_key, ty.bytes),
            Comparison::Castable
        );
        assert_eq!(db.compare_type(ty.bytes, ty.int), Comparison::Castable);
        assert_eq!(db.compare_type(ty.int, ty.bytes), Comparison::Castable);
        assert_eq!(db.compare_type(ty.int, ty.int), Comparison::Equal);
    }

    #[test]
    fn test_nil_type() {
        let (db, ty) = setup();
        assert_eq!(db.compare_type(ty.nil, ty.int), Comparison::Castable);
        assert_eq!(db.compare_type(ty.nil, ty.bool), Comparison::Castable);
        assert_eq!(db.compare_type(ty.nil, ty.bytes), Comparison::Assignable);
        assert_eq!(db.compare_type(ty.nil, ty.bytes32), Comparison::Unrelated);
        assert_eq!(
            db.compare_type(ty.nil, ty.public_key),
            Comparison::Unrelated
        );
    }

    #[test]
    fn test_pair_type() {
        let (mut db, ty) = setup();

        let int_pair = db.alloc_type(Type::Pair(PairType {
            first: ty.int,
            rest: ty.int,
        }));
        assert_eq!(db.compare_type(int_pair, int_pair), Comparison::Equal);

        let bytes_pair = db.alloc_type(Type::Pair(PairType {
            first: ty.bytes,
            rest: ty.bytes,
        }));
        assert_eq!(db.compare_type(int_pair, bytes_pair), Comparison::Castable);
        assert_eq!(db.compare_type(bytes_pair, int_pair), Comparison::Castable);

        let bytes32_pair = db.alloc_type(Type::Pair(PairType {
            first: ty.bytes32,
            rest: ty.bytes32,
        }));
        assert_eq!(
            db.compare_type(bytes_pair, bytes32_pair),
            Comparison::Superset
        );
        assert_eq!(db.compare_type(bytes32_pair, bytes_pair), Comparison::Equal);

        let bytes32_bytes = db.alloc_type(Type::Pair(PairType {
            first: ty.bytes32,
            rest: ty.bytes,
        }));
        assert_eq!(
            db.compare_type(bytes_pair, bytes32_bytes),
            Comparison::Superset
        );
        assert_eq!(
            db.compare_type(bytes32_bytes, bytes_pair),
            Comparison::Equal
        );
    }

    #[test]
    fn test_struct_types() {
        let (mut db, ty) = setup();

        let two_ints = db.alloc_type(Type::Unknown);
        *db.ty_mut(two_ints) = Type::Struct(StructType {
            original_type_id: two_ints,
            fields: fields(&[ty.int, ty.int]),
            rest: Rest::Nil,
        });
        assert_eq!(db.compare_type(two_ints, two_ints), Comparison::Equal);

        let one_int = db.alloc_type(Type::Unknown);
        *db.ty_mut(one_int) = Type::Struct(StructType {
            original_type_id: one_int,
            fields: fields(&[ty.int]),
            rest: Rest::Nil,
        });
        assert_eq!(db.compare_type(one_int, two_ints), Comparison::Unrelated);

        let empty_struct = db.alloc_type(Type::Unknown);
        *db.ty_mut(empty_struct) = Type::Struct(StructType {
            original_type_id: empty_struct,
            fields: fields(&[]),
            rest: Rest::Nil,
        });
        assert_eq!(
            db.compare_type(empty_struct, empty_struct),
            Comparison::Equal
        );
    }

    #[test]
    fn test_enum_types() {
        let (mut db, ty) = setup();

        let enum_type = db.alloc_type(Type::Unknown);

        let variant = db.alloc_type(Type::Unknown);
        *db.ty_mut(variant) = Type::EnumVariant(EnumVariantType {
            enum_type,
            original_type_id: variant,
            fields: Some(fields(&[ty.int])),
            discriminant: BigInt::zero(),
            rest: Rest::Nil,
        });

        *db.ty_mut(enum_type) = Type::Enum(EnumType {
            has_fields: true,
            variants: fields(&[variant]),
        });

        assert_eq!(db.compare_type(enum_type, variant), Comparison::Superset);
        assert_eq!(db.compare_type(variant, enum_type), Comparison::Assignable);
    }

    #[test]
    fn test_function_types() {
        let (mut db, ty) = setup();

        let int_to_bool = db.alloc_type(Type::Function(FunctionType {
            param_types: vec![ty.int],
            rest: Rest::Nil,
            return_type: ty.bool,
            generic_types: Vec::new(),
        }));
        assert_eq!(db.compare_type(int_to_bool, int_to_bool), Comparison::Equal);

        let int_to_int = db.alloc_type(Type::Function(FunctionType {
            param_types: vec![ty.int],
            rest: Rest::Nil,
            return_type: ty.int,
            generic_types: Vec::new(),
        }));
        assert_eq!(
            db.compare_type(int_to_bool, int_to_int),
            Comparison::Castable
        );
        assert_eq!(
            db.compare_type(int_to_int, int_to_bool),
            Comparison::Superset
        );

        let int_pair = db.alloc_type(Type::Pair(PairType {
            first: ty.int,
            rest: ty.int,
        }));
        let int_pair_to_bool = db.alloc_type(Type::Function(FunctionType {
            param_types: vec![int_pair],
            rest: Rest::Nil,
            return_type: ty.bool,
            generic_types: Vec::new(),
        }));
        assert_eq!(
            db.compare_type(int_to_bool, int_pair_to_bool),
            Comparison::Unrelated
        );
        assert_eq!(
            db.compare_type(int_pair_to_bool, int_to_bool),
            Comparison::Unrelated
        );
    }
}
