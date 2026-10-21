//! Tests for comparing generic function types (`compare`).
//!
//! A generic function is assignable to a function type if its generics can be instantiated to
//! make it so. The target's generics are rigid, since the function must work for all of them, so
//! two generic function types are compared up to renaming of their generics.

use std::collections::HashMap;

use id_arena::Arena;
use indexmap::IndexMap;
use rue_types::{
    BuiltinTypes, Comparison, FunctionType, Generic, Inference, Pair, Type, TypeId, Union, compare,
    compare_with_inference, resolve_inference, substitute_with_mappings,
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

    fn generic(&mut self) -> TypeId {
        self.arena.alloc(Type::Generic(Generic { name: None }))
    }

    fn pair(&mut self, first: TypeId, rest: TypeId) -> TypeId {
        self.arena.alloc(Type::Pair(Pair::new(first, rest)))
    }

    fn union(&mut self, types: Vec<TypeId>) -> TypeId {
        self.arena.alloc(Type::Union(Union::new(types)))
    }

    fn function(&mut self, generics: &[TypeId], params: &[TypeId], ret: TypeId) -> TypeId {
        self.arena.alloc(Type::Function(FunctionType {
            generics: generics.to_vec(),
            params: params
                .iter()
                .enumerate()
                .map(|(i, &ty)| (format!("p{i}"), ty))
                .collect::<IndexMap<_, _>>(),
            nil_terminated: true,
            ret,
        }))
    }

    /// `fn<T>(value: T) -> T`
    fn identity(&mut self) -> TypeId {
        let t = self.generic();
        self.function(&[t], &[t], t)
    }

    fn cmp(&mut self, lhs: TypeId, rhs: TypeId) -> Comparison {
        compare(&mut self.arena, &self.builtins, lhs, rhs)
    }
}

#[test]
fn generic_function_is_instantiated() {
    let mut db = Types::new();
    let BuiltinTypes {
        int, bytes, nil, ..
    } = db.builtins;

    let identity = db.identity();
    let int_to_int = db.function(&[], &[int], int);
    let int_or_nil = db.union(vec![int, nil]);
    let int_to_int_or_nil = db.function(&[], &[int], int_or_nil);
    let int_to_bytes = db.function(&[], &[int], bytes);

    assert_eq!(db.cmp(identity, int_to_int), Comparison::Assign);
    assert_eq!(db.cmp(identity, int_to_int_or_nil), Comparison::Assign);

    // Instantiating it with `Int` and returning `Bytes` requires a cast, as it would for
    // `fn(value: Int) -> Int`
    assert_eq!(db.cmp(identity, int_to_bytes), Comparison::Cast);
    assert_eq!(db.cmp(int_to_int, int_to_bytes), Comparison::Cast);
}

#[test]
fn generic_function_is_not_instantiated_inconsistently() {
    let mut db = Types::new();
    let BuiltinTypes {
        int, bytes32, nil, ..
    } = db.builtins;

    let identity = db.identity();
    let int_to_bytes32 = db.function(&[], &[int], bytes32);
    let int_or_nil = db.union(vec![int, nil]);
    let int_or_nil_to_int = db.function(&[], &[int_or_nil], int);

    assert_eq!(db.cmp(identity, int_to_bytes32), Comparison::Invalid);

    // This would return `nil` for a `nil` argument, which needs a cast to `Int`, as it would
    // for `fn(value: Int | nil) -> Int | nil`
    let int_or_nil_to_int_or_nil = db.function(&[], &[int_or_nil], int_or_nil);
    assert_eq!(db.cmp(identity, int_or_nil_to_int), Comparison::Cast);
    assert_eq!(
        db.cmp(int_or_nil_to_int_or_nil, int_or_nil_to_int),
        Comparison::Cast
    );
}

#[test]
fn generic_functions_are_compared_up_to_renaming() {
    let mut db = Types::new();
    let int = db.builtins.int;

    let identity_t = db.identity();
    let identity_u = db.identity();
    assert_eq!(db.cmp(identity_t, identity_u), Comparison::Assign);
    assert_eq!(db.cmp(identity_u, identity_t), Comparison::Assign);

    // fn<A, B>(a: A, b: B) -> A
    let a = db.generic();
    let b = db.generic();
    let first = db.function(&[a, b], &[a, b], a);

    // fn<X, Y>(x: X, y: Y) -> X
    let x = db.generic();
    let y = db.generic();
    let first_xy = db.function(&[x, y], &[x, y], x);
    assert_eq!(db.cmp(first, first_xy), Comparison::Assign);

    // fn<X, Y>(x: X, y: Y) -> Y
    let second_xy = db.function(&[x, y], &[x, y], y);
    assert_eq!(db.cmp(first, second_xy), Comparison::Invalid);

    // fn(x: Int, y: Int) -> Int
    let concrete = db.function(&[], &[int, int], int);
    assert_eq!(db.cmp(first, concrete), Comparison::Assign);
}

#[test]
fn rigid_generics_are_not_instantiated() {
    let mut db = Types::new();
    let int = db.builtins.int;

    let identity = db.identity();
    let int_to_int = db.function(&[], &[int], int);

    // fn<U>(value: U) -> Int
    let u = db.generic();
    let u_to_int = db.function(&[u], &[u], int);

    // fn<U, V>(value: U) -> V
    let u = db.generic();
    let v = db.generic();
    let u_to_v = db.function(&[u, v], &[u], v);

    // A concrete function doesn't work for every type
    assert_eq!(db.cmp(int_to_int, identity), Comparison::Invalid);
    assert_eq!(db.cmp(u_to_int, identity), Comparison::Invalid);

    // And the target's generics can't be instantiated to make it work
    assert_eq!(db.cmp(identity, u_to_int), Comparison::Invalid);
    assert_eq!(db.cmp(identity, u_to_v), Comparison::Invalid);
}

#[test]
fn generic_function_compared_to_its_own_generics() {
    let mut db = Types::new();
    let int = db.builtins.int;

    // Inside the body of `fn identity<T>(value: T) -> T`, the function's own `T` is rigid
    let generic = db.generic();
    let identity = db.function(&[generic], &[generic], generic);
    let rigid = db.function(&[], &[generic], generic);
    let rigid_to_int = db.function(&[], &[generic], int);

    assert_eq!(db.cmp(identity, rigid), Comparison::Assign);
    assert_eq!(db.cmp(identity, rigid_to_int), Comparison::Invalid);
}

#[test]
fn memoized_rigid_comparisons_are_not_reused_for_instances() {
    let mut db = Types::new();
    let int = db.builtins.int;

    // Inside the body of `fn identity<T>(value: T) -> T`, compare `(T, identity)` to
    // `(T, fn(value: Int) -> T)`. The first `T <: T` is between rigid generics, but in the
    // function `T` is being instantiated, so it needs a bound and can't reuse that result.
    let generic = db.generic();
    let identity = db.function(&[generic], &[generic], generic);
    let int_to_rigid = db.function(&[], &[int], generic);
    let lhs = db.pair(generic, identity);
    let rhs = db.pair(generic, int_to_rigid);

    assert_eq!(db.cmp(lhs, rhs), Comparison::Invalid);
}

#[test]
fn nested_bounds_use_rigid_generics() {
    let mut db = Types::new();
    let int = db.builtins.int;

    // Inside the body of `fn outer<T>() -> fn<U>(value: U) -> U`, `outer`'s own `T` is rigid
    let generic = db.generic();
    let identity = db.identity();
    let outer = db.function(&[generic], &[], identity);

    // So the inner bound `T <: Int` must not be taken as a bound on the instantiated `T`
    let rigid_to_int = db.function(&[], &[generic], int);
    let returns_rigid_to_int = db.function(&[], &[], rigid_to_int);
    assert_eq!(db.cmp(outer, returns_rigid_to_int), Comparison::Invalid);

    let rigid = db.function(&[], &[generic], generic);
    let returns_rigid = db.function(&[], &[], rigid);
    assert_eq!(db.cmp(outer, returns_rigid), Comparison::Assign);
}

#[test]
fn generic_function_in_contravariant_position() {
    let mut db = Types::new();
    let int = db.builtins.int;

    let identity = db.identity();
    let int_to_int = db.function(&[], &[int], int);

    // A function that is given a generic function can be used where a concrete function would
    // be given, but not the other way around
    let takes_identity = db.function(&[], &[identity], int);
    let takes_int_to_int = db.function(&[], &[int_to_int], int);

    assert_eq!(db.cmp(takes_int_to_int, takes_identity), Comparison::Assign);
    assert_eq!(
        db.cmp(takes_identity, takes_int_to_int),
        Comparison::Invalid
    );
}

#[test]
fn generic_function_argument_is_inferred() {
    let mut db = Types::new();
    let int = db.builtins.int;

    // fn apply<T>(function: fn(value: T) -> T, value: T) -> T
    let generic = db.generic();
    let param = db.function(&[], &[generic], generic);

    // apply(identity, 42)
    let identity = db.identity();
    let mut infer = Inference::new([generic]);

    for (arg, param) in [(identity, param), (int, generic)] {
        assert_eq!(
            compare_with_inference(&mut db.arena, &db.builtins, arg, param, Some(&mut infer)),
            Comparison::Assign
        );
    }

    // The generics of `identity` don't leak into the inferred type
    let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
    assert_eq!(mappings, HashMap::from_iter([(generic, int)]));

    let param = substitute_with_mappings(&mut db.arena, param, &mappings);
    assert_eq!(db.cmp(identity, param), Comparison::Assign);
}
