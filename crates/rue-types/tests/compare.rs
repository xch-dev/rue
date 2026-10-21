//! Tests for assigning rigid generic type parameters (`compare`).
//!
//! A generic `T` that isn't being inferred stands for an unknown type, so it can only be assigned
//! to a type that holds every possible `T`. That includes `T` itself and any union or alias that
//! contains it, but not a concrete type or a different generic.

use id_arena::Arena;
use rue_types::{
    Alias, BuiltinTypes, Comparison, Generic, Inference, Type, TypeId, Union, compare,
    compare_with_inference,
};

struct Types {
    arena: Arena<Type>,
    builtins: BuiltinTypes,
}

impl Types {
    fn new() -> Self {
        let mut arena = Arena::new();
        let builtins = BuiltinTypes::new(&mut arena);
        Self { arena, builtins }
    }

    fn alloc(&mut self, ty: Type) -> TypeId {
        self.arena.alloc(ty)
    }

    fn union(&mut self, types: Vec<TypeId>) -> TypeId {
        self.alloc(Type::Union(Union::new(types)))
    }

    fn alias(&mut self, inner: TypeId) -> TypeId {
        self.alloc(Type::Alias(Alias {
            name: None,
            inner,
            generics: vec![],
        }))
    }

    fn generic(&mut self) -> TypeId {
        self.alloc(Type::Generic(Generic { name: None }))
    }

    fn optional(&mut self, inner: TypeId) -> TypeId {
        let nil = self.builtins.nil;
        self.union(vec![inner, nil])
    }

    fn compare(&mut self, lhs: TypeId, rhs: TypeId) -> Comparison {
        compare(&mut self.arena, &self.builtins, lhs, rhs)
    }
}

#[test]
fn test_generic_to_union_containing_it() {
    let mut t = Types::new();
    let generic = t.generic();
    let optional = t.optional(generic);
    let alias = t.alias(optional);
    let nested = t.alias(alias);

    assert_eq!(t.compare(generic, optional), Comparison::Assign);
    assert_eq!(t.compare(generic, alias), Comparison::Assign);
    assert_eq!(t.compare(generic, nested), Comparison::Assign);
}

#[test]
fn test_generic_to_and_from_identity_alias() {
    let mut t = Types::new();
    let generic = t.generic();
    let alias = t.alias(generic);
    let nested = t.alias(alias);

    assert_eq!(t.compare(generic, alias), Comparison::Assign);
    assert_eq!(t.compare(alias, generic), Comparison::Assign);
    assert_eq!(t.compare(nested, generic), Comparison::Assign);
}

#[test]
fn test_generic_to_concrete_types() {
    let mut t = Types::new();
    let generic = t.generic();
    let int = t.builtins.int;
    let optional_int = t.optional(int);
    let alias = t.alias(optional_int);

    assert_ne!(t.compare(generic, int), Comparison::Assign);
    assert_ne!(t.compare(generic, optional_int), Comparison::Assign);
    assert_ne!(t.compare(generic, alias), Comparison::Assign);
}

#[test]
fn test_generic_to_other_generic() {
    let mut t = Types::new();
    let generic = t.generic();
    let other = t.generic();
    let optional_other = t.optional(other);
    let alias_other = t.alias(optional_other);
    let identity_alias = t.alias(generic);

    assert_eq!(t.compare(generic, other), Comparison::Invalid);
    assert_eq!(t.compare(generic, optional_other), Comparison::Invalid);
    assert_eq!(t.compare(generic, alias_other), Comparison::Invalid);
    assert_eq!(t.compare(identity_alias, other), Comparison::Invalid);
}

#[test]
fn test_union_containing_generic_to_generic() {
    let mut t = Types::new();
    let generic = t.generic();
    let optional = t.optional(generic);
    let alias = t.alias(optional);

    assert_eq!(t.compare(optional, generic), Comparison::Invalid);
    assert_eq!(t.compare(alias, generic), Comparison::Invalid);
}

#[test]
fn test_rigid_generic_into_inferred_union() {
    let mut t = Types::new();
    let rigid = t.generic();
    let target = t.generic();
    let optional_target = t.optional(target);

    // Passing a `U` to a parameter of type `T | nil` should infer `T` as `U`
    let mut infer = Inference::new([target]);

    assert_eq!(
        compare_with_inference(
            &mut t.arena,
            &t.builtins,
            rigid,
            optional_target,
            Some(&mut infer)
        ),
        Comparison::Assign
    );

    let candidates = infer.candidates(target).unwrap();
    assert_eq!(candidates.lower, vec![rigid]);
    assert!(candidates.upper.is_empty());
}
