//! Tests for types that reference the same type more than once, such as `type T1 = (T0, T0);`.
//!
//! Expanding the references separately would make the work (and the number of types allocated)
//! exponential in the depth of the type, so these would never finish if that were the case.

mod common;

use common::Types;

use std::collections::HashMap;

use rue_types::{
    Alias, Apply, Atoms, Check, Comparison, FunctionParameter, FunctionType, Generic, Inference,
    Struct, Type, TypeId, check, compare_with_inference, count_steps, extract_atoms,
    extract_functions, extract_pairs, extract_structs, intersect, substitute, subtract,
    with_step_limit,
};

const DEPTH: usize = 32;

impl Types {
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
                params: vec![
                    FunctionParameter::new("a", ty),
                    FunctionParameter::new("b", ty),
                ],
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

type Build = fn(&mut Types, TypeId, usize) -> TypeId;

const SHAPES: [(&str, Build); 4] = [
    ("pairs", Types::pairs),
    ("functions", Types::functions),
    ("generics", Types::generics),
    ("lists", Types::lists),
];

/// Builds a type of the shape at the given depth from `Int`, and returns the number of steps that
/// the operation takes on it (along with a copy of it, and the same shape built from `Bytes32`).
fn steps(build: Build, depth: usize, op: fn(&mut Types, TypeId, TypeId, TypeId)) -> usize {
    let mut t = Types::new();
    let int = t.builtins.int;
    let bytes32 = t.builtins.bytes32;
    let ints = build(&mut t, int, depth);
    let ints_copy = build(&mut t, int, depth);
    let bytes32 = build(&mut t, bytes32, depth);
    count_steps(|| op(&mut t, ints, ints_copy, bytes32)).1
}

/// Asserts that the steps taken by the operation grow linearly with the depth, by comparing them
/// at two depths. The depths are small enough that exponential growth still finishes quickly, and
/// that intersections don't reach their maximum depth (which would hide how they grow).
fn assert_linear(op: fn(&mut Types, TypeId, TypeId, TypeId)) {
    const DEPTH: usize = 4;

    for (name, build) in SHAPES {
        let small = steps(build, DEPTH, op);
        let large = steps(build, DEPTH * 2, op);

        // Doubling the depth at most doubles the steps, and constant terms only make that smaller.
        // The leeway of 1.5x means quadratic growth (a factor of 4) fails, as does exponential
        // growth (a factor of about `2^DEPTH`).
        assert!(
            large <= small * 3,
            "{name}: {small} steps at depth {DEPTH}, but {large} at depth {}",
            DEPTH * 2
        );
    }
}

#[test]
fn test_growth_substitute() {
    assert_linear(|t, ints, _, _| {
        substitute(&mut t.arena, ints);
    });
}

#[test]
fn test_growth_compare() {
    assert_linear(|t, ints, ints_copy, bytes32| {
        t.compare(ints, ints);
        t.compare(ints, ints_copy);
        t.compare(ints, bytes32);
        t.compare(bytes32, ints);
    });
}

#[test]
fn test_growth_check() {
    // Checking `pairs` from `Any` isn't included, since the check itself doubles in size at each
    // level until it's too large, so the work is bounded by the size limit instead (see
    // `test_check_size_limit` in `check.rs`)
    assert_linear(|t, ints, ints_copy, _| {
        let nil = t.builtins.nil;
        let ints_or_nil = t.union(vec![ints, nil]);
        let _ = check(&mut t.arena, &t.builtins, ints_or_nil, ints_copy);
    });
}

#[test]
fn test_growth_subtract() {
    assert_linear(|t, ints, ints_copy, _| {
        let nil = t.builtins.nil;
        let ints_or_nil = t.union(vec![ints, nil]);
        subtract(&mut t.arena, &t.builtins, ints_or_nil, ints_copy);
    });
}

#[test]
fn test_growth_subtract_split() {
    // Each pair is split by the part that differs from the target (#502)
    assert_linear(|t, ints, ints_copy, _| {
        let nil = t.builtins.nil;
        let ints_or_nil = t.union(vec![ints, nil]);
        let lhs = t.pair(ints_or_nil, ints);
        let rhs = t.pair(ints_copy, ints_copy);
        subtract(&mut t.arena, &t.builtins, lhs, rhs);
        let lhs = t.pair(ints, ints_or_nil);
        let rhs = t.pair(ints_copy, nil);
        subtract(&mut t.arena, &t.builtins, lhs, rhs);
    });
}

/// `type T0 = true | false; type T1 = (T0, T0); ...` and
/// `type R0 = true; type R1 = (T0, R0) | (R0, T0); ...`
///
/// Subtracting `R_n` from `T_n` splits both parts of the pair by `T_{n-1} - R_{n-1}`, so it's
/// exponential unless that's only worked out once, and quadratic unless the comparisons of each
/// level's parts share a memo.
fn split_both_ways(depth: usize) -> usize {
    let mut t = Types::new();
    let (yes, no) = (t.builtins.bool_true, t.builtins.bool_false);
    let mut lhs = t.union(vec![yes, no]);
    let mut rhs = yes;

    for _ in 0..depth {
        let pair = t.pair(lhs, lhs);
        let first = t.pair(lhs, rhs);
        let rest = t.pair(rhs, lhs);
        let union = t.union(vec![first, rest]);
        lhs = t.alias(pair);
        rhs = t.alias(union);
    }

    count_steps(|| subtract(&mut t.arena, &t.builtins, lhs, rhs)).1
}

#[test]
fn test_growth_subtract_split_both_ways() {
    // Doubling the depth at most doubles the steps if they're linear, as in `assert_linear`
    let small = split_both_ways(8);
    let large = split_both_ways(16);
    assert!(
        large <= small * 3,
        "{small} steps at depth 8, but {large} at depth 16"
    );
}

/// `type U0 = Int | (Int | nil, Int); type U1 = U0 | U0; ...`, from #496
#[test]
fn test_subtract_split_repeated_variant() {
    let mut t = Types::new();
    let (int, nil) = (t.builtins.int, t.builtins.nil);
    let optional = t.union(vec![int, nil]);
    let pair = t.pair(optional, int);
    let mut ty = t.union(vec![int, pair]);

    for _ in 0..8 {
        ty = t.union(vec![ty, ty]);
    }

    let nil_pair = t.pair(nil, int);
    let narrowed = subtract(&mut t.arena, &t.builtins, ty, nil_pair);

    // Each copy of the pair is narrowed to the same type, so the comparisons on the result are
    // shared rather than repeated for each copy
    let Type::Union(union) = t.arena[narrowed].clone() else {
        panic!("expected a union");
    };
    let mut pairs = union
        .types
        .iter()
        .copied()
        .filter(|&id| matches!(t.arena[id], Type::Pair(_)))
        .collect::<Vec<_>>();
    pairs.dedup();
    assert_eq!(pairs.len(), 1);
}

#[test]
fn test_growth_intersect() {
    assert_linear(|t, ints, _, bytes32| {
        let nil = t.builtins.nil;
        let ints_or_nil = t.union(vec![ints, nil]);
        intersect(&mut t.arena, &t.builtins, ints_or_nil, bytes32);
    });
}

/// `type U0 = members; type U1 = U0 | U0; ...`, with each level an alias of the union.
fn doubling_union(t: &mut Types, members: Vec<TypeId>, depth: usize) -> TypeId {
    let union = t.union(members);
    let mut ty = t.alias(union);

    for _ in 0..depth {
        let union = t.union(vec![ty, ty]);
        ty = t.alias(union);
    }

    ty
}

/// The walks over the members of a union visit each member once, rather than once for each way
/// it's reached, which would double the work at each level of `U1 = U0 | U0` (part of #496). The
/// shallow union comes first, so that walking a member each time it's reached fails on the number
/// of results quickly, rather than taking `2^30` steps on the deep one.
#[test]
fn test_doubling_union_extractors() {
    for levels in [8, 30] {
        let mut t = Types::new();
        let (int, nil) = (t.builtins.int, t.builtins.nil);
        let pair = t.pair(int, int);
        let function = t.alloc(Type::Function(FunctionType {
            generics: vec![],
            params: vec![],
            nil_terminated: true,
            ret: nil,
        }));
        let ty = doubling_union(&mut t, vec![int, pair, function], levels);

        let struct_id = t.alloc(Type::Unresolved);
        t.arena[struct_id] = Type::Struct(Struct {
            name: None,
            semantic: struct_id,
            inner: pair,
            generics: vec![],
            fields: ["a".to_string()].into_iter().collect(),
            nil_terminated: false,
            narrowed: false,
        });
        let structs = doubling_union(&mut t, vec![struct_id], levels);

        with_step_limit(10_000, || {
            assert!(matches!(
                extract_atoms(&mut t.arena, ty, false),
                Some(Atoms::Unrestricted)
            ));
            assert!(extract_atoms(&mut t.arena, ty, true).is_none());
            assert_eq!(extract_pairs(&mut t.arena, ty, false).len(), 1);
            assert!(extract_pairs(&mut t.arena, ty, true).is_empty());
            assert_eq!(extract_functions(&mut t.arena, ty, false).len(), 1);
            assert!(extract_functions(&mut t.arena, ty, true).is_empty());
            assert!(extract_structs(&mut t.arena, ty).is_none());
            assert_eq!(extract_structs(&mut t.arena, structs).unwrap().len(), 1);
        });
    }
}

/// `type U0 = Int | (Int | nil, Int); type U1 = U0 | U0; ...` without aliases, which `substitute`
/// leaves shared, so subtracting from it takes as many steps as there are levels (part of #496)
#[test]
fn test_subtract_doubling_union() {
    let mut t = Types::new();
    let (int, nil) = (t.builtins.int, t.builtins.nil);
    let optional = t.union(vec![int, nil]);
    let pair = t.pair(optional, int);
    let mut ty = t.union(vec![int, pair]);

    for _ in 0..30 {
        ty = t.union(vec![ty, ty]);
    }

    let nil_pair = t.pair(nil, int);
    with_step_limit(10_000, || {
        subtract(&mut t.arena, &t.builtins, ty, nil_pair);
    });
}
