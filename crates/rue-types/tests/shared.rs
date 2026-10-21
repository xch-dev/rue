//! Tests for types that reference the same type more than once, such as `type T1 = (T0, T0);`.
//!
//! Expanding the references separately would make the work (and the number of types allocated)
//! exponential in the depth of the type, so these would never finish if that were the case.

use std::collections::HashMap;

use id_arena::Arena;
use indexmap::IndexMap;
use rue_types::{
    Alias, Apply, BuiltinTypes, Check, Comparison, FunctionType, Generic, Inference, Pair, Type,
    TypeId, Union, check, compare, compare_with_inference, substitute,
};

const DEPTH: usize = 32;

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

    fn alias(&mut self, inner: TypeId) -> TypeId {
        self.alloc(Type::Alias(Alias {
            name: None,
            inner,
            generics: vec![],
        }))
    }

    fn pair(&mut self, first: TypeId, rest: TypeId) -> TypeId {
        self.alloc(Type::Pair(Pair::new(first, rest)))
    }

    fn union(&mut self, types: Vec<TypeId>) -> TypeId {
        self.alloc(Type::Union(Union::new(types)))
    }

    /// `type T0 = leaf; type T1 = (T0, T0); ...`
    fn pairs(&mut self, leaf: TypeId, depth: usize) -> TypeId {
        let mut ty = self.alias(leaf);

        for _ in 0..depth {
            let pair = self.pair(ty, ty);
            ty = self.alias(pair);
        }

        ty
    }

    /// `type F0 = leaf; type F1 = fn(a: F0, b: F0) -> F0 | (F0, F0) | nil; ...`
    fn functions(&mut self, leaf: TypeId, depth: usize) -> TypeId {
        let mut ty = self.alias(leaf);

        for _ in 0..depth {
            let function = self.alloc(Type::Function(FunctionType {
                generics: vec![],
                params: IndexMap::from_iter([("a".to_string(), ty), ("b".to_string(), ty)]),
                nil_terminated: true,
                ret: ty,
            }));
            let pair = self.pair(ty, ty);
            let nil = self.builtins.nil;
            let union = self.union(vec![function, pair, nil]);
            ty = self.alias(union);
        }

        ty
    }

    /// `type Two<X> = (X, X); type G0 = leaf; type G1 = Two<G0>; ...`
    fn generics(&mut self, leaf: TypeId, depth: usize) -> TypeId {
        let generic = self.alloc(Type::Generic(Generic { name: None }));
        let pair = self.pair(generic, generic);
        let two = self.alloc(Type::Alias(Alias {
            name: None,
            inner: pair,
            generics: vec![generic],
        }));

        let mut ty = self.alias(leaf);

        for _ in 0..depth {
            let apply = self.alloc(Type::Apply(Apply::new(
                two,
                HashMap::from_iter([(generic, ty)]),
            )));
            ty = self.alias(apply);
        }

        ty
    }

    /// `type L0 = leaf; type R1 = nil | (L0, R1); type L1 = (L0, R1); ...`
    fn lists(&mut self, leaf: TypeId, depth: usize) -> TypeId {
        let mut ty = self.alias(leaf);

        for _ in 0..depth {
            let list = self.alloc(Type::Unresolved);
            let pair = self.pair(ty, list);
            let nil = self.builtins.nil;
            let inner = self.union(vec![nil, pair]);
            self.arena[list] = Type::Alias(Alias {
                name: None,
                inner,
                generics: vec![],
            });

            let pair = self.pair(ty, list);
            ty = self.alias(pair);
        }

        ty
    }

    /// Substitutes the type, and returns the number of types that were allocated.
    fn substituted_size(&mut self, ty: TypeId) -> usize {
        let len = self.arena.len();
        substitute(&mut self.arena, ty);
        self.arena.len() - len
    }

    fn compare(&mut self, lhs: TypeId, rhs: TypeId) -> Comparison {
        compare(&mut self.arena, &self.builtins, lhs, rhs)
    }
}

#[test]
fn test_shared_pairs() {
    let mut t = Types::new();
    let int = t.builtins.int;
    let bytes = t.builtins.bytes;
    let bytes32 = t.builtins.bytes32;
    let ints = t.pairs(int, DEPTH);
    let ints_copy = t.pairs(int, DEPTH);
    let bytes = t.pairs(bytes, DEPTH);
    let bytes32 = t.pairs(bytes32, DEPTH);

    assert!(t.substituted_size(ints) < 8 * DEPTH);

    assert_eq!(t.compare(ints, ints), Comparison::Assign);
    assert_eq!(t.compare(ints, ints_copy), Comparison::Assign);
    assert_eq!(t.compare(ints, bytes), Comparison::Cast);
    assert_eq!(t.compare(ints, bytes32), Comparison::Invalid);
    assert_eq!(t.compare(bytes32, bytes), Comparison::Assign);
}

#[test]
fn test_shared_functions() {
    let mut t = Types::new();
    let int = t.builtins.int;
    let bytes32 = t.builtins.bytes32;
    let ints = t.functions(int, DEPTH);
    let ints_copy = t.functions(int, DEPTH);
    let bytes32 = t.functions(bytes32, DEPTH);

    assert!(t.substituted_size(ints) < 16 * DEPTH);

    assert_eq!(t.compare(ints, ints), Comparison::Assign);
    assert_eq!(t.compare(ints, ints_copy), Comparison::Assign);
    assert_eq!(t.compare(ints, bytes32), Comparison::Invalid);
    assert_eq!(t.compare(bytes32, ints), Comparison::Invalid);
}

#[test]
fn test_shared_generic_arguments() {
    let mut t = Types::new();
    let int = t.builtins.int;
    let bytes32 = t.builtins.bytes32;
    let ints = t.generics(int, DEPTH);
    let ints_pairs = t.pairs(int, DEPTH);
    let bytes32 = t.generics(bytes32, DEPTH);

    // Each generic argument is substituted again with the mappings of the type it's applied to,
    // which copies it, so this is quadratic rather than linear in the depth
    assert!(t.substituted_size(ints) < 64 * DEPTH * DEPTH);

    assert_eq!(t.compare(ints, ints), Comparison::Assign);
    assert_eq!(t.compare(ints, ints_pairs), Comparison::Assign);
    assert_eq!(t.compare(ints_pairs, ints), Comparison::Assign);
    assert_eq!(t.compare(ints, bytes32), Comparison::Invalid);
}

#[test]
fn test_shared_recursive_types() {
    let mut t = Types::new();
    let int = t.builtins.int;
    let bytes32 = t.builtins.bytes32;
    let ints = t.lists(int, DEPTH);
    let ints_copy = t.lists(int, DEPTH);
    let bytes32 = t.lists(bytes32, DEPTH);

    assert!(t.substituted_size(ints) < 16 * DEPTH);

    assert_eq!(t.compare(ints, ints), Comparison::Assign);
    assert_eq!(t.compare(ints, ints_copy), Comparison::Assign);
    assert_eq!(t.compare(ints, bytes32), Comparison::Invalid);
}

#[test]
fn test_shared_checks() {
    let mut t = Types::new();
    let int = t.builtins.int;
    let nil = t.builtins.nil;
    let any = t.builtins.permissive_any;
    let ints = t.pairs(int, DEPTH);
    let ints_or_nil = t.union(vec![ints, nil]);

    assert_eq!(
        check(&mut t.arena, &t.builtins, ints_or_nil, ints),
        Ok(Check::IsPair {
            can_be_truthy: false
        })
    );

    // The check itself is too deep to generate, but it shouldn't take long to find that out
    assert!(check(&mut t.arena, &t.builtins, any, ints).is_err());
}

#[test]
fn test_shared_inference() {
    let mut t = Types::new();
    let int = t.builtins.int;
    let ints = t.pairs(int, DEPTH);

    // Arguments are compared with inference even if there's nothing to infer
    let mut infer = Inference::new([]);
    assert_eq!(
        compare_with_inference(&mut t.arena, &t.builtins, ints, ints, Some(&mut infer)),
        Comparison::Assign
    );

    // Comparisons that collect candidates aren't cached, so every candidate is still collected
    let generic = t.alloc(Type::Generic(Generic { name: None }));
    let generics = t.pairs(generic, 2);
    let ints = t.pairs(int, 2);
    let mut infer = Inference::new([generic]);
    assert_eq!(
        compare_with_inference(&mut t.arena, &t.builtins, ints, generics, Some(&mut infer)),
        Comparison::Assign
    );
    assert_eq!(infer.candidates(generic).unwrap().lower.len(), 4);
}
