//! Tests that reusing the results of earlier comparisons doesn't change what they return, and that
//! it keeps comparing recursive types fast.
//!
//! The differential tests compare random recursive types, generic aliases and generic function
//! types with the comparison memo on and off (`without_compare_memo`). The work without the memo
//! can be exponential, so the types are kept small, and each case without the memo gives up after a
//! number of steps (`with_step_limit`).

use std::{
    any::Any,
    cell::Cell,
    collections::HashMap,
    fmt::Write,
    panic::{AssertUnwindSafe, catch_unwind, resume_unwind},
    sync::Once,
};

use id_arena::Arena;
use indexmap::IndexSet;
use rue_diagnostic::Name;
use rue_types::{
    Alias, Apply, Atom, AtomRestriction, AtomSemantic, BuiltinTypes, CheckError, Comparison,
    FunctionParameter, FunctionType, Generic, Inference, Pair, Struct, TooComplex, Type, TypeId,
    Union, check, compare, compare_limited, compare_with_inference, count_abandoned_passes,
    count_steps, intersect, is_non_contractive, is_non_regular, reaches_non_contractive_cycle,
    resolve_inference, stringify, substitute, with_comparison_step_limit, with_fixpoint_budget,
    with_step_limit, without_compare_memo,
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

    fn generic(&mut self) -> TypeId {
        self.alloc(Type::Generic(Generic { name: None }))
    }

    fn pair(&mut self, first: TypeId, rest: TypeId) -> TypeId {
        self.alloc(Type::Pair(Pair::new(first, rest)))
    }

    fn union(&mut self, types: Vec<TypeId>) -> TypeId {
        self.alloc(Type::Union(Union::new(types)))
    }

    fn function(&mut self, generics: Vec<TypeId>, params: Vec<TypeId>, ret: TypeId) -> TypeId {
        self.alloc(Type::Function(FunctionType {
            generics,
            params: params
                .into_iter()
                .enumerate()
                .map(|(i, ty)| FunctionParameter::new(format!("p{i}"), ty))
                .collect(),
            nil_terminated: true,
            ret,
        }))
    }

    fn structure(&mut self, inner: TypeId) -> TypeId {
        let id = self.alloc(Type::Unresolved);
        self.arena[id] = Type::Struct(Struct {
            name: None,
            semantic: id,
            inner,
            generics: vec![],
            fields: IndexSet::new(),
            nil_terminated: true,
            narrowed: false,
        });
        id
    }

    /// Declares an alias whose inner type is filled in later by `define`, to allow recursion.
    fn declare_alias(&mut self, name: &str, generics: Vec<TypeId>) -> TypeId {
        let inner = self.alloc(Type::Unresolved);
        self.alloc(Type::Alias(Alias {
            name: Some(Name::new(name, None)),
            inner,
            generics,
        }))
    }

    fn define(&mut self, alias: TypeId, resolved: TypeId) {
        let Type::Alias(Alias { inner, .. }) = self.arena[alias] else {
            panic!("expected an alias");
        };
        self.arena[inner] = Type::Ref(resolved);
    }

    fn int_literal(&mut self, value: u8) -> TypeId {
        self.alloc(Type::Atom(Atom::new(
            AtomSemantic::Int,
            Some(AtomRestriction::Value(vec![value].into())),
        )))
    }

    fn apply(&mut self, inner: TypeId, generics: HashMap<TypeId, TypeId>) -> TypeId {
        self.alloc(Type::Apply(Apply::new(inner, generics)))
    }
}

/// `type G<A, B, C, D> = fn<U>(p0: G<B, C, D, A>, p1: G<A, A, B, C>, p2: G<D, B, B, A>) -> U;`
fn rotating_function(db: &mut Types) -> TypeId {
    let [a, b, c, d] = [db.generic(), db.generic(), db.generic(), db.generic()];
    let alias = db.declare_alias("G", vec![a, b, c, d]);
    let ret = db.generic();

    let apply = |db: &mut Types, args: [TypeId; 4]| {
        db.apply(alias, [a, b, c, d].into_iter().zip(args).collect())
    };

    let p0 = apply(db, [b, c, d, a]);
    let p1 = apply(db, [a, a, b, c]);
    let p2 = apply(db, [d, b, b, a]);
    let function = db.function(vec![ret], vec![p0, p1, p2], ret);
    db.define(alias, function);

    let BuiltinTypes {
        int,
        bytes,
        nil,
        bool,
        ..
    } = db.builtins;
    apply(db, [int, bytes, nil, bool])
}

/// A regression test gives up after this many steps, so that it fails quickly rather than hanging
/// if the work becomes exponential again.
const STEP_BOUND: usize = 1_000_000;

/// Runs the function on a thread with a larger stack, since comparing recursive types recurses
/// deeply (especially in debug builds).
fn with_large_stack<T: Send + 'static>(f: impl FnOnce() -> T + Send + 'static) -> T {
    std::thread::Builder::new()
        .stack_size(1 << 30)
        .spawn(f)
        .unwrap()
        .join()
        .unwrap_or_else(|payload| resume_unwind(payload))
}

#[test]
fn test_rotating_generic_function() {
    with_large_stack(|| {
        let mut db = Types::new();
        let lhs = rotating_function(&mut db);
        let rhs = rotating_function(&mut db);

        // This never finished, since nearly every comparison relied on one in progress, and
        // memoizing was turned off while the function's generic was being instantiated. A single
        // pass takes about 97,000 steps, so it's abandoned for iterating, and it takes about
        // 44,000 steps in all.
        let ((result, steps), abandoned) = count_abandoned_passes(|| {
            count_steps(|| {
                with_step_limit(STEP_BOUND, || {
                    compare(&mut db.arena, &db.builtins, lhs, rhs)
                })
            })
        });
        assert_eq!(result, Comparison::Assign);
        assert!(steps < 150_000, "took {steps} steps");
        assert_eq!(abandoned, 1);
    });
}

/// `fn<U>(x: T20) -> U` and `fn(x: S20) -> ret`, where `T0 = U`, `S0 = leaf`, and each
/// `T(k + 1) = (Tk, Tk)` and `S(k + 1) = (Sk, Sk)` share their halves.
fn shared_instance_pairs(db: &mut Types, leaf: TypeId, ret: TypeId) -> (TypeId, TypeId) {
    let u = db.generic();
    let (mut lhs, mut rhs) = (u, leaf);

    for _ in 0..20 {
        lhs = db.pair(lhs, lhs);
        rhs = db.pair(rhs, rhs);
    }

    let lhs = db.function(vec![u], vec![lhs], u);
    let rhs = db.function(vec![], vec![rhs], ret);
    (lhs, rhs)
}

#[test]
fn test_instance_comparisons_are_memoized() {
    with_large_stack(|| {
        // Comparing the parameters refers to `U`, which is being instantiated, so until #362's
        // step 13b.2 none of the million comparisons of their halves were memoized. They're cached
        // with the bound they put on `U` now, which is put on it again each time one is reused.
        for (returns_int, expected) in [(true, Comparison::Assign), (false, Comparison::Invalid)] {
            let mut db = Types::new();
            let BuiltinTypes { int, nil, .. } = db.builtins;
            let ret = if returns_int { int } else { nil };
            let (lhs, rhs) = shared_instance_pairs(&mut db, int, ret);

            let (result, steps) = count_steps(|| {
                with_step_limit(STEP_BOUND, || {
                    compare(&mut db.arena, &db.builtins, lhs, rhs)
                })
            });
            assert_eq!(result, expected);
            assert!(steps < 1_000, "took {steps} steps");
        }
    });
}

/// A cached summary is in a canonical order, without duplicates, so a comparison that refers to
/// an instance isn't cached while inferring: the candidates that checking the instance's bounds
/// collects would come in another order, and the inferred type would be `Bytes | Int` rather than
/// `Int | Bytes`. Here, comparing the second `(Int, Bytes)` with `(U, U)` would be reused from the
/// first.
#[test]
fn test_instance_comparisons_arent_memoized_while_inferring() {
    let inferred = |memo: bool| {
        let mut db = Types::new();
        let BuiltinTypes {
            int,
            bytes,
            permissive_any,
            ..
        } = db.builtins;
        let (u, t) = (db.generic(), db.generic());
        let both = db.pair(u, u);
        let function = db.function(vec![u], vec![both], u);
        let lhs = db.pair(function, function);
        let param = db.pair(int, bytes);
        let first = db.function(vec![], vec![param], permissive_any);
        let second = db.function(vec![], vec![param], t);
        let rhs = db.pair(first, second);

        let mut infer = Inference::new([t]);
        let mut compare =
            || compare_with_inference(&mut db.arena, &db.builtins, lhs, rhs, Some(&mut infer));
        let result = if memo {
            compare()
        } else {
            without_compare_memo(compare)
        };
        assert_eq!(result, Comparison::Assign);

        let resolved = resolve_inference(&mut db.arena, &db.builtins, &infer);
        stringify(&mut db.arena, resolved[&t])
    };

    assert_eq!(inferred(true), "Int | Bytes");
    assert_eq!(inferred(false), "Int | Bytes");
}

/// `type N0 = nil | (nil | (Int | (N1, N0)) | ((nil, N1), N1));`
/// `type N1 = (N0 | (Bytes | N0), N0);`
fn mutually_recursive(db: &mut Types) -> (TypeId, TypeId) {
    let BuiltinTypes {
        int, bytes, nil, ..
    } = db.builtins;
    let n0 = db.declare_alias("N0", vec![]);
    let n1 = db.declare_alias("N1", vec![]);

    let n1_n0 = db.pair(n1, n0);
    let int_or_pair = db.union(vec![int, n1_n0]);
    let nil_n1 = db.pair(nil, n1);
    let nested = db.pair(nil_n1, n1);
    let inner = db.union(vec![nil, int_or_pair, nested]);
    let n0_inner = db.union(vec![nil, inner]);
    db.define(n0, n0_inner);

    let bytes_or_n0 = db.union(vec![bytes, n0]);
    let first = db.union(vec![n0, bytes_or_n0]);
    let n1_inner = db.pair(first, n0);
    db.define(n1, n1_inner);

    (n0, n1)
}

#[test]
fn test_mutually_recursive_intersection() {
    let mut db = Types::new();
    let (n0, n1) = mutually_recursive(&mut db);
    let int = db.builtins.int;

    // `x is (((N1, Int), N0), N1)` with `x: N0` intersects the types, which compares each part of
    // them again. These took about 1,080,000 and 89,000 steps, and take about 3,600 and 1,700 now.
    let n1_int = db.pair(n1, int);
    let inner = db.pair(n1_int, n0);
    let target = db.pair(inner, n1);

    let (_, steps) = count_steps(|| {
        with_step_limit(STEP_BOUND, || {
            intersect(&mut db.arena, &db.builtins, n0, target)
        })
    });
    assert!(steps < 20_000, "took {steps} steps");

    let ((_, steps), abandoned) =
        count_abandoned_passes(|| count_steps(|| compare(&mut db.arena, &db.builtins, n0, target)));
    assert!(steps < 20_000, "took {steps} steps");

    // Recursive comparisons like these finish in a single pass, without iterating
    assert_eq!(abandoned, 0);
}

/// `type N0 = N1<(Bool, N1<Bytes, 2, 1>), N0, Bytes>;`
/// `type N1<A, B, C> = ((C, N1<A, 2, B> | N1<C, Bool, A>), N0);`
fn permuted_arguments(db: &mut Types) -> (TypeId, TypeId) {
    let BuiltinTypes { bool, bytes, .. } = db.builtins;
    let [a, b, c] = [db.generic(), db.generic(), db.generic()];
    let n0 = db.declare_alias("N0", vec![]);
    let n1 = db.declare_alias("N1", vec![a, b, c]);
    let one = db.int_literal(1);
    let two = db.int_literal(2);

    let apply =
        |db: &mut Types, args: [TypeId; 3]| db.apply(n1, [a, b, c].into_iter().zip(args).collect());

    let inner_n1 = apply(db, [bytes, two, one]);
    let first = db.pair(bool, inner_n1);
    let n0_inner = apply(db, [first, n0, bytes]);
    db.define(n0, n0_inner);

    let left = apply(db, [a, two, b]);
    let right = apply(db, [c, bool, a]);
    let either = db.union(vec![left, right]);
    let pair = db.pair(c, either);
    let n1_inner = db.pair(pair, n0);
    db.define(n1, n1_inner);

    (apply(db, [one, one, two]), n0)
}

#[test]
fn test_permuted_generic_alias_arguments() {
    with_large_stack(|| {
        let mut db = Types::new();
        let (n1, n0) = permuted_arguments(&mut db);

        // Each comparison relied on others in progress, so the fixpoint memo could only reuse it
        // while they were still on the stack, and these never finished (#286). The single pass
        // now gives up after about 30,000 steps, and iterating takes about 120,000 more.
        for (lhs, rhs) in [(n0, n1), (n1, n0)] {
            let ((result, steps), abandoned) = count_abandoned_passes(|| {
                count_steps(|| {
                    with_step_limit(STEP_BOUND, || {
                        compare(&mut db.arena, &db.builtins, lhs, rhs)
                    })
                })
            });
            assert_eq!(result, Comparison::Invalid);
            assert!(steps < 300_000, "took {steps} steps");
            assert_eq!(abandoned, 1);
        }
    });
}

// The tests below are cases found while working on #344, where an experimental change that let
// generic functions whose generics are nested inside of their parameters use fixpoint mode gave a
// different result than without the memo, by reusing a comparison that would have merged the
// generics instantiated by another one. Since #362's step 13b.1, a revisit doesn't merge instances
// anymore: it puts the bounds that the comparison it revisits puts on its instances (its summary)
// on the instances it's revisited with, so each occurrence of a generic function is instantiated
// on its own. Four of them hold now, since what merging made one instance is the instances of
// different function values. They guard the modes agreeing. Most compare the types in a new arena
// in every mode, since the order of the types can change which comparisons are reached again with
// which binders.

/// `type R = fn<T>(x: Px) -> nil; type Px = (Pf, T); type Pf = fn(g: R, h: fn(k: Pf) -> nil) -> Int;`
/// and `type Rq = fn(x: Qx) -> nil; type Qx = (Qf, Int);`
/// `type Qf = fn(g: Rg, h: fn(k: Qf) -> nil) -> (Int, Int); type Rg = fn(x: Qx) -> Int;`
fn merged_into_outer_function(db: &mut Types) -> (TypeId, TypeId) {
    let BuiltinTypes { nil, int, .. } = db.builtins;
    let [r, px, pf, rq, qx, qf, rg] =
        ["R", "Px", "Pf", "Rq", "Qx", "Qf", "Rg"].map(|name| db.declare_alias(name, vec![]));

    let t = db.generic();
    let function = db.function(vec![t], vec![px], nil);
    db.define(r, function);
    let pair = db.pair(pf, t);
    db.define(px, pair);
    let callback = db.function(vec![], vec![pf], nil);
    let function = db.function(vec![], vec![r, callback], int);
    db.define(pf, function);

    let function = db.function(vec![], vec![qx], nil);
    db.define(rq, function);
    let pair = db.pair(qf, int);
    db.define(qx, pair);
    let callback = db.function(vec![], vec![qf], nil);
    let ints = db.pair(int, int);
    let function = db.function(vec![], vec![rg, callback], ints);
    db.define(qf, function);
    let function = db.function(vec![], vec![qx], int);
    db.define(rg, function);

    (r, rq)
}

#[test]
fn test_merged_into_outer_function() {
    with_large_stack(|| {
        let mut db = Types::new();
        let (r, rq) = merged_into_outer_function(&mut db);

        // Comparing `(Qf, Pf)` used to merge `T` into the outer function's `T` when `(Qx, Px)` is
        // reached again, and `(Qf, Pf)` is then reached again itself. Iterating that change gave it
        // the least value every round, since the comparison whose `T` it changed was a different
        // one in each round, so it never finished. It's `Invalid` either way, since that comes
        // from comparing `(Int, Int)` with `Int`.
        let mut compare_with = |budget: Option<(usize, usize)>| {
            count_steps(|| {
                with_step_limit(STEP_BOUND, || match budget {
                    Some((per_item, base)) => with_fixpoint_budget(per_item, base, || {
                        compare(&mut db.arena, &db.builtins, r, rq)
                    }),
                    None => compare(&mut db.arena, &db.builtins, r, rq),
                })
            })
        };

        let (single, _) = compare_with(None);
        let (iterating, steps) = compare_with(Some((0, 0)));
        let without_memo = without_compare_memo(|| compare(&mut db.arena, &db.builtins, r, rq));

        assert_eq!(single, Comparison::Invalid);
        assert_eq!(iterating, single);
        assert_eq!(without_memo, single);
        assert!(steps < 10_000, "took {steps} steps");
    });
}

/// `type S1 = fn<U>(p: (U, fn(k: S1) -> nil), q: U) -> U;`
/// `type S2 = fn(p: (Never, fn(k: S2) -> nil), q: Bytes32) -> Bytes32;`
/// Returns `(S1, S1's body)` and `(S2, H)`, where `H` is
/// `fn(p: (Never, fn(k: S2) -> nil), q: Int) -> Int`.
fn reused_across_functions(db: &mut Types) -> (TypeId, TypeId) {
    let BuiltinTypes {
        nil,
        int,
        bytes32,
        never,
        ..
    } = db.builtins;
    let s1 = db.declare_alias("S1", vec![]);
    let s2 = db.declare_alias("S2", vec![]);
    let u = db.generic();
    let k1 = db.function(vec![], vec![s1], nil);
    let k2 = db.function(vec![], vec![s2], nil);
    let left = db.pair(u, k1);
    let right = db.pair(never, k2);
    let g = db.function(vec![u], vec![left, u], u);
    let f = db.function(vec![], vec![right, bytes32], bytes32);
    db.define(s1, g);
    db.define(s2, f);
    let h = db.function(vec![], vec![right, int], int);
    (db.pair(s1, g), db.pair(s2, h))
}

#[test]
fn test_reused_across_functions() {
    with_large_stack(|| {
        // Comparing `S1` to `S2` caches the comparison of `fn(k: S1) -> nil` to `fn(k: S2) -> nil`,
        // whose computation reaches `(U, ...)` against `(Never, ...)` again. That used to merge
        // the `U` of the function it's in into `H`'s, which reusing it skipped. Each is its own
        // instance now: `S1` is assignable to `S2` with `U = Bytes32`, and `S1`'s body to `H` with
        // `U = Int`, where comparing their `fn(k: S1) -> nil` parameters compares `S1` to `S2`
        // again
        let results = in_every_mode(reused_across_functions);
        assert_eq!(results, [Comparison::Assign; 3]);
    });
}

/// `fn<U>(p0: (U, fn(k: S1) -> nil), p1: fn(cb: fn<U>(p: (U, fn(k: S1) -> nil), q: U) -> U) -> nil) -> U`
/// against `fn(p0: (Never, fn(k: S2) -> nil), p1: fn(cb: S3) -> nil) -> Bytes32`, where `S1` and
/// `S2` are as in `reused_across_functions`, and `S3` is `S2` with `last` in place of `Bytes32`.
fn same_key_other_function(db: &mut Types, last: TypeId) -> (TypeId, TypeId) {
    let BuiltinTypes {
        nil,
        bytes32,
        never,
        ..
    } = db.builtins;
    let s1 = db.declare_alias("S1", vec![]);
    let s2 = db.declare_alias("S2", vec![]);
    let u = db.generic();
    let k1 = db.function(vec![], vec![s1], nil);
    let k2 = db.function(vec![], vec![s2], nil);
    let left = db.pair(u, k1);
    let right = db.pair(never, k2);
    let g = db.function(vec![u], vec![left, u], u);
    let f2 = db.function(vec![], vec![right, bytes32], bytes32);
    let f3 = db.function(vec![], vec![right, last], last);
    db.define(s1, g);
    db.define(s2, f2);
    let inner = db.function(vec![u], vec![left, u], u);
    let lhs_callback = db.function(vec![], vec![inner], nil);
    let rhs_callback = db.function(vec![], vec![f3], nil);
    let lhs = db.function(vec![u], vec![left, lhs_callback], u);
    let rhs = db.function(vec![], vec![right, rhs_callback], bytes32);
    (lhs, rhs)
}

#[test]
fn test_same_key_other_function() {
    with_large_stack(|| {
        // Comparing `(k2, k1)` revisits the comparison of the first parameters, which used to
        // merge into the outer function's `U`, and was then reused under the comparison of the
        // callbacks' functions, whose first parameters have the same key but their own `U`. Each
        // `U` is its own instance now, so the outer one is `Bytes32` and the callback's is `last`,
        // which holds whether that's `Bytes32` or `Int`
        for bytes32 in [false, true] {
            let run = |mode: u8| {
                let mut db = Types::new();
                let last = if bytes32 {
                    db.builtins.bytes32
                } else {
                    db.builtins.int
                };
                let (lhs, rhs) = same_key_other_function(&mut db, last);

                match mode {
                    0 => compare(&mut db.arena, &db.builtins, lhs, rhs),
                    1 => with_fixpoint_budget(0, 0, || {
                        compare(&mut db.arena, &db.builtins, lhs, rhs)
                    }),
                    _ => without_compare_memo(|| compare(&mut db.arena, &db.builtins, lhs, rhs)),
                }
            };

            // Each is compared in a new arena, since the order of the types can change which
            // comparisons are revisited with which binders
            assert_eq!(run(2), Comparison::Assign);
            assert_eq!(run(0), Comparison::Assign);
            assert_eq!(run(1), Comparison::Assign);
        }
    });
}

/// `type K1 = fn(k: S1) -> nil; type S1 = fn<U>(p: (U, (K1, K1)), q: U) -> U;`
/// `type K2 = fn(k: S2) -> nil; type S2 = fn<W>(p: (Never, (K2, K2)), q: W) -> W;`
/// Returns `fn<U>(p: (U, (K1, K1))) -> nil` and `fn(p: (Never, (K2, K2))) -> nil`.
fn merged_twice(db: &mut Types) -> (TypeId, TypeId) {
    let never = db.builtins.never;
    merged_twice_with(db, never)
}

/// Like `merged_twice`, with `first` in place of `Never`.
fn merged_twice_with(db: &mut Types, first: TypeId) -> (TypeId, TypeId) {
    let BuiltinTypes { nil, .. } = db.builtins;
    let s1 = db.declare_alias("S1", vec![]);
    let s2 = db.declare_alias("S2", vec![]);
    let u = db.generic();
    let w = db.generic();
    let k1 = db.function(vec![], vec![s1], nil);
    let k2 = db.function(vec![], vec![s2], nil);
    let both_k1 = db.pair(k1, k1);
    let both_k2 = db.pair(k2, k2);
    let left = db.pair(u, both_k1);
    let right = db.pair(first, both_k2);
    let f1 = db.function(vec![u], vec![left, u], u);
    let f2 = db.function(vec![w], vec![right, w], w);
    db.define(s1, f1);
    db.define(s2, f2);
    let lhs = db.function(vec![u], vec![left], nil);
    let rhs = db.function(vec![], vec![right], nil);
    (lhs, rhs)
}

#[test]
fn test_merged_twice() {
    with_large_stack(|| {
        let run = |mode: u8| {
            let mut db = Types::new();
            let (lhs, rhs) = merged_twice(&mut db);

            match mode {
                0 => compare(&mut db.arena, &db.builtins, lhs, rhs),
                1 => with_fixpoint_budget(0, 0, || compare(&mut db.arena, &db.builtins, lhs, rhs)),
                _ => without_compare_memo(|| compare(&mut db.arena, &db.builtins, lhs, rhs)),
            }
        };

        // `(K2, K1)` is compared twice, and each time, comparing `S1` to `S2` binds a new `U` and
        // `W`. That `U` used to be merged into the outer one when the parameters are reached again,
        // so the two `W`s were checked against each other. Each `U` is its own instance now: `S1`
        // is assignable to `S2` with `U = W`, so `K2` is assignable to `K1`, and the outer `U` only
        // needs to hold `Never`
        assert_eq!(run(2), Comparison::Assign);
        assert_eq!(run(0), Comparison::Assign);
        assert_eq!(run(1), Comparison::Assign);
    });
}

/// With `Int` in place of `Never`, `S1` isn't assignable to `S2`, since `W` would have to hold
/// `Int`. That's only found through the bound `U ≥ Int` that comparing the parameters puts on `U`,
/// which a revisit of it with the `U` of `S1` doesn't compute again. The revisit is given what
/// comparing the parameters put on the outer `U` (its summary), and the comparison is computed
/// again until that stops changing, so the revisit's `U` gets that bound as well.
#[test]
fn test_revisit_gets_summary() {
    with_large_stack(|| {
        let results = in_every_mode(|db| {
            let int = db.builtins.int;
            merged_twice_with(db, int)
        });
        assert_eq!(results, [Comparison::Invalid; 3]);
    });
}

/// `type N0 = fn<U>(p0: N1 | U, p1: fn(p0: U) -> nil) -> N0; type N1 = (N1, nil) | (N0, nil);`
/// Returns `N0` twice.
fn reflexive_through_member(db: &mut Types) -> (TypeId, TypeId) {
    let BuiltinTypes { nil, .. } = db.builtins;
    let n0 = db.declare_alias("N0", vec![]);
    let n1 = db.declare_alias("N1", vec![]);
    let u = db.generic();
    let first = db.union(vec![n1, u]);
    let second = db.function(vec![], vec![u], nil);
    let function = db.function(vec![u], vec![first, second], n0);
    db.define(n0, function);
    let recursive = db.pair(n1, nil);
    let outer = db.pair(n0, nil);
    let body = db.union(vec![recursive, outer]);
    db.define(n1, body);
    (n0, n0)
}

/// Comparing `N1 | U'` with `N1 | U` (from the rigid `U'` of the other `N0`) holds the `N1` with
/// the `N1` member, by revisiting `N0` against itself, which puts no bound on `U`. That member used
/// to count as bounding `U` anyway, since it assumed a comparison in progress, which could have
/// merged bounds into `U` when it finished. So `U ≥ N1` from the `U` member was checked against
/// `U ≤ U'`, and `N0` wasn't assignable to itself in exact mode. A revisit puts its summary on the
/// instances it's revisited with now, so whether a member bounds them is only whether it did.
#[test]
fn test_reflexive_through_member() {
    with_large_stack(|| {
        let results = in_every_mode(reflexive_through_member);
        assert_eq!(results, [Comparison::Assign; 3]);
    });
}

/// `type AL = (KL, fn<U>() -> (U, (U, KL))); type KL = (AL, nil);`
/// `type AR = (KR, fn() -> (Bytes32, (Int, KR))); type KR = (AR, nil);`
/// Returns `(AL, fn<U>(p: U) -> (U, KL))` and `(AR, fn(p: Int) -> (Int, KR))`.
fn reached_through_dependency(db: &mut Types) -> (TypeId, TypeId) {
    let BuiltinTypes {
        nil, int, bytes32, ..
    } = db.builtins;
    let [al, ar, kl, kr] = ["AL", "AR", "KL", "KR"].map(|name| db.declare_alias(name, vec![]));
    let u = db.generic();

    let inner_lhs = db.pair(u, kl);
    let inner_rhs = db.pair(int, kr);
    let ret_lhs = db.pair(u, inner_lhs);
    let ret_rhs = db.pair(bytes32, inner_rhs);
    let thunk_lhs = db.function(vec![u], vec![], ret_lhs);
    let thunk_rhs = db.function(vec![], vec![], ret_rhs);

    let pair = db.pair(kl, thunk_lhs);
    db.define(al, pair);
    let pair = db.pair(kr, thunk_rhs);
    db.define(ar, pair);
    let pair = db.pair(al, nil);
    db.define(kl, pair);
    let pair = db.pair(ar, nil);
    db.define(kr, pair);

    let lhs = db.function(vec![u], vec![u], inner_lhs);
    let rhs = db.function(vec![], vec![int], inner_rhs);
    (db.pair(al, lhs), db.pair(ar, rhs))
}

#[test]
fn test_reached_through_dependency() {
    with_large_stack(|| {
        // Comparing `(AL, AR)` caches `(KL, KR)`, which relies on `(AL, AR)`. Iterating reused it
        // while comparing the second functions' return types, where computing it again would reach
        // `(AL, AR)`, then the first functions, whose return types revisit those of the second ones,
        // which used to merge `U` (with its `Bytes32` bound) into theirs. Each `U` is its own
        // instance now: the thunk's `U` can be `Never`, since `(Never, (Never, KL))` is assignable
        // to `(Bytes32, (Int, KR))`, and the second function's `U` can be `Int`
        let results = in_every_mode(reached_through_dependency);
        assert_eq!(results, [Comparison::Assign; 3]);
    });
}

// The programs below are #344's, and two of the corpus programs that `nested-corpus` rebuilt for
// it (with its seed), which never finished before #362's step 13b.3: their generic functions nest
// their generics inside recursive aliases, so they were compared in exact mode, whose memo can't
// reuse anything inside a cycle. They're compared in fixpoint mode now, iterating over the bounds
// that the comparisons put on the instances (their summaries).

/// Applies the alias to each list of arguments, in the order of its generics.
fn apply_each<const N: usize>(db: &mut Types, alias: TypeId, args: [&[TypeId]; N]) -> [TypeId; N] {
    let Type::Alias(Alias { generics, .. }) = db.arena[alias].clone() else {
        panic!("expected an alias");
    };

    args.map(|args| {
        db.apply(
            alias,
            generics.iter().copied().zip(args.iter().copied()).collect(),
        )
    })
}

/// `type N0 = (fn<U00>(p0: N0, p1: N1<nil>) -> U00, N1<Bool> | N1<Bytes32>);`
/// `type N1<A> = Bytes32 | (fn(p0: (N1<A>, A), p1: fn<U20, U21>(p0: N0, p1: N1<U21>) -> N1<A>) -> (Bool, A));`
/// Returns `N0` twice, through two other aliases, as in #344's `type Q1 = N0; type Q2 = N0;`.
fn issue_344(db: &mut Types) -> (TypeId, TypeId) {
    let BuiltinTypes {
        nil, bool, bytes32, ..
    } = db.builtins;
    let a = db.generic();
    let n0 = db.declare_alias("N0", vec![]);
    let n1 = db.declare_alias("N1", vec![a]);
    let [u00, u20, u21] = [db.generic(), db.generic(), db.generic()];

    let [n1_nil, n1_bool, n1_bytes32, n1_a, n1_u21] =
        apply_each(db, n1, [&[nil], &[bool], &[bytes32], &[a], &[u21]]);
    let function = db.function(vec![u00], vec![n0, n1_nil], u00);
    let members = db.union(vec![n1_bool, n1_bytes32]);
    let body = db.pair(function, members);
    db.define(n0, body);

    let first = db.pair(n1_a, a);
    let callback = db.function(vec![u20, u21], vec![n0, n1_u21], n1_a);
    let ret = db.pair(bool, a);
    let function = db.function(vec![], vec![first, callback], ret);
    let body = db.union(vec![bytes32, function]);
    db.define(n1, body);

    let q1 = db.declare_alias("Q1", vec![]);
    db.define(q1, n0);
    let q2 = db.declare_alias("Q2", vec![]);
    db.define(q2, n0);
    (q2, q1)
}

/// Seed 666:
/// `type N0 = fn<U000, U001>(p0: fn<U010>(p0: fn(p0: N1<Bytes32>, p1: N1<U010>) -> N1<U010>, p1: Bool) -> N0, p1: ((N1<Bool>, N1<U001>), N0)) -> Bool;`
/// `type N1<A> = (fn<U100>(p0: fn(p0: Bytes, p1: N0) -> A, p1: A) -> N0, Bool);`
/// Returns `N0` twice, through two other aliases.
fn corpus_666(db: &mut Types) -> (TypeId, TypeId) {
    let BuiltinTypes {
        bool,
        bytes,
        bytes32,
        ..
    } = db.builtins;
    let a = db.generic();
    let n0 = db.declare_alias("N0", vec![]);
    let n1 = db.declare_alias("N1", vec![a]);
    let [u000, u001, u010, u100] = [db.generic(), db.generic(), db.generic(), db.generic()];

    let [n1_bytes32, n1_u010, n1_bool, n1_u001] =
        apply_each(db, n1, [&[bytes32], &[u010], &[bool], &[u001]]);
    let inner = db.function(vec![], vec![n1_bytes32, n1_u010], n1_u010);
    let first = db.function(vec![u010], vec![inner, bool], n0);
    let both = db.pair(n1_bool, n1_u001);
    let second = db.pair(both, n0);
    let body = db.function(vec![u000, u001], vec![first, second], bool);
    db.define(n0, body);

    let inner = db.function(vec![], vec![bytes, n0], a);
    let function = db.function(vec![u100], vec![inner, a], n0);
    let body = db.pair(function, bool);
    db.define(n1, body);

    let q1 = db.declare_alias("Q1", vec![]);
    db.define(q1, n0);
    let q2 = db.declare_alias("Q2", vec![]);
    db.define(q2, n0);
    (q2, q1)
}

/// Seed 1280:
/// `type N0<A, B> = (N1<(N0<nil, Bytes32>, Bytes)>, nil) | (N1<A>, nil);`
/// `type N1<A> = (Bytes32, (fn(p0: N0<A, A>, p1: A) -> Bytes32, N0<nil, A> | N1<nil>));`
/// `type Q1 = N0<(N0<Int, Bytes32>, nil) | (N0<nil, Bytes32>, nil), fn<U210, U211>(p0: U211, p1: N1<Bytes>) -> U211>;`
/// `type Q2 = N0<fn<U220, U221>(p0: N1<U220>) -> U220, fn<U230>(p0: U230, p1: U230) -> N1<nil>>;`
/// Returns `Q2` and `Q1`.
fn corpus_1280(db: &mut Types) -> (TypeId, TypeId) {
    let BuiltinTypes {
        nil,
        int,
        bytes,
        bytes32,
        ..
    } = db.builtins;
    let [a, b, c] = [db.generic(), db.generic(), db.generic()];
    let n0 = db.declare_alias("N0", vec![a, b]);
    let n1 = db.declare_alias("N1", vec![c]);
    let [u210, u211, u220, u221, u230] = [
        db.generic(),
        db.generic(),
        db.generic(),
        db.generic(),
        db.generic(),
    ];

    let [n0_nil_bytes32, n0_c_c, n0_nil_c, n0_int_bytes32] = apply_each(
        db,
        n0,
        [&[nil, bytes32], &[c, c], &[nil, c], &[int, bytes32]],
    );
    let pair = db.pair(n0_nil_bytes32, bytes);
    let [n1_pair, n1_a, n1_nil, n1_bytes, n1_u220] =
        apply_each(db, n1, [&[pair], &[a], &[nil], &[bytes], &[u220]]);
    let first = db.pair(n1_pair, nil);
    let second = db.pair(n1_a, nil);
    let body = db.union(vec![first, second]);
    db.define(n0, body);

    let function = db.function(vec![], vec![n0_c_c, c], bytes32);
    let rest = db.union(vec![n0_nil_c, n1_nil]);
    let inner = db.pair(function, rest);
    let body = db.pair(bytes32, inner);
    db.define(n1, body);

    let first = db.pair(n0_int_bytes32, nil);
    let second = db.pair(n0_nil_bytes32, nil);
    let union = db.union(vec![first, second]);
    let function = db.function(vec![u210, u211], vec![u211, n1_bytes], u211);
    let [q1] = apply_each(db, n0, [&[union, function]]);

    let first = db.function(vec![u220, u221], vec![n1_u220], u220);
    let second = db.function(vec![u230], vec![u230, u230], n1_nil);
    let [q2] = apply_each(db, n0, [&[first, second]]);
    (q2, q1)
}

#[rstest::rstest]
#[case::issue_344(issue_344, Comparison::Assign)]
#[case::corpus_666(corpus_666, Comparison::Assign)]
#[case::corpus_1280(corpus_1280, Comparison::Invalid)]
fn test_nested_generic_functions_in_cycles(
    #[case] types: fn(&mut Types) -> (TypeId, TypeId),
    #[case] expected: Comparison,
) {
    with_large_stack(move || {
        // Without the memo, they're still compared in exact mode, so they'd take too many steps
        let mut db = Types::new();
        let (lhs, rhs) = types(&mut db);
        let (result, steps) = count_steps(|| {
            with_step_limit(STEP_BOUND, || {
                compare(&mut db.arena, &db.builtins, lhs, rhs)
            })
        });
        assert_eq!(result, expected);
        // They each take a few thousand steps. Starting in a single pass, or not memoizing the
        // comparisons that refer to instances, takes about ten times as many
        assert!(steps < 10_000, "took {steps} steps");
    });
}

/// `type FL = fn<T>(p0: (T, WR) | (Int, WL), p1: Bytes) -> T; type WL = fn(p0: FL) -> nil;`
/// `type FR = fn(p0: (Int, WR), p1: Int) -> (nil, nil); type WR = fn(p0: FR) -> nil;`
/// Returns `FL` and `FR`.
fn held_by_revisit(db: &mut Types) -> (TypeId, TypeId) {
    let BuiltinTypes {
        int, bytes, nil, ..
    } = db.builtins;
    let [fl, wl, fr, wr] = ["FL", "WL", "FR", "WR"].map(|name| db.declare_alias(name, vec![]));
    let t = db.generic();
    let instantiated = db.pair(t, wr);
    let other = db.pair(int, wl);
    let param = db.union(vec![instantiated, other]);
    let function = db.function(vec![t], vec![param, bytes], t);
    db.define(fl, function);
    let function = db.function(vec![], vec![fl], nil);
    db.define(wl, function);
    let param = db.pair(int, wr);
    let ret = db.pair(nil, nil);
    let function = db.function(vec![], vec![param, int], ret);
    db.define(fr, function);
    let function = db.function(vec![], vec![fr], nil);
    db.define(wr, function);
    (fl, fr)
}

/// Both members of `(T, WR) | (Int, WL)` hold `(Int, WR)` if comparing `FL` with `FR` again is
/// `Assign`, and only the first bounds `T`, so its `Int ≤ T` is left out (see `Choices::is_held`),
/// and the result is `Cast` (from the second parameters). But that makes the second member only
/// `Cast`, so `Int ≤ T ≤ (nil, nil)` is checked after all, and the result is `Invalid`, which is
/// its least fixed point. Exact mode used to stop after one round, so it gave `Cast`, while
/// iterating in fixpoint mode gave `Invalid`. A revisit is given the previous round's result too
/// in exact mode for types like these, so it gives `Invalid` as well.
#[test]
fn test_held_by_revisit() {
    with_large_stack(|| {
        let results = in_every_mode(held_by_revisit);
        assert_eq!(results, [Comparison::Invalid; 3]);

        // Inferring always compares in exact mode, which gives it the same result
        let mut db = Types::new();
        let (lhs, rhs) = held_by_revisit(&mut db);
        let mut infer = Inference::new([]);
        let result =
            compare_with_inference(&mut db.arena, &db.builtins, lhs, rhs, Some(&mut infer));
        assert_eq!(result, Comparison::Invalid);
    });
}

/// `type FL = fn<T>(p0: T) -> FR;`
/// `type FR = fn<U>(p0: FL | fn(p0: U) -> FR | U, p1: ((WR, U), fn(p0: WR) -> WL)) -> nil;`
/// `type WL = fn(p0: FR) -> Bytes; type WR = fn(p0: Bytes) -> Bytes;`
/// Returns `FL` twice.
fn unmatched_member_bound(db: &mut Types) -> (TypeId, TypeId) {
    let BuiltinTypes { bytes, nil, .. } = db.builtins;
    let [fl, wl, fr, wr] = ["FL", "WL", "FR", "WR"].map(|name| db.declare_alias(name, vec![]));
    let t = db.generic();
    let function = db.function(vec![t], vec![t], fr);
    db.define(fl, function);
    let function = db.function(vec![], vec![fr], bytes);
    db.define(wl, function);
    let u = db.generic();
    let inner = db.function(vec![], vec![u], fr);
    let first = db.union(vec![fl, inner, u]);
    let pair = db.pair(wr, u);
    let callback = db.function(vec![], vec![wr], wl);
    let second = db.pair(pair, callback);
    let function = db.function(vec![u], vec![first, second], nil);
    db.define(fr, function);
    let function = db.function(vec![], vec![bytes], bytes);
    db.define(wr, function);
    (fl, fl)
}

/// This records a known disagreement rather than what should happen: `FL` should be assignable to
/// itself in every mode. Which union members match isn't monotone in their results (#360): when a
/// member that bounds `U` gets worse, it stops matching and its bounds stop counting, which makes
/// the union's result better. So comparing has no least fixed point here. Iterating in fixpoint
/// mode only ever joins the results upward, so it ends at a fixed point that can reject more than
/// exact mode, never accept more: inside `FL`, `FR` against itself is `Invalid` on its first
/// round, which iterating keeps for it everywhere, while exact mode computes it again where it's
/// reached and ends at `Assign`. Making the matched members monotone is a follow-up on #362 (next
/// to §5, question 2), which this test shows as a diff when it lands.
#[test]
fn test_unmatched_member_bound_modes() {
    with_large_stack(|| {
        let results = in_every_mode(unmatched_member_bound);
        assert_eq!(
            results,
            [Comparison::Assign, Comparison::Invalid, Comparison::Invalid]
        );
    });
}

/// `fn<B>(f: fn() -> B, g: fn() -> B) -> B` and `fn(f: P, g: P) -> (Int, Int)`, where
/// `P = fn<T>() -> (T, T)`, with each parameter list using one type twice.
fn inner_instance_reused(db: &mut Types) -> (TypeId, TypeId) {
    let int = db.builtins.int;
    let [b, t] = [db.generic(), db.generic()];
    let thunk = db.function(vec![], vec![], b);
    let call = db.function(vec![b], vec![thunk, thunk], b);
    let both = db.pair(t, t);
    let pairs = db.function(vec![t], vec![], both);
    let ints = db.pair(int, int);
    let target = db.function(vec![], vec![pairs, pairs], ints);
    (call, target)
}

/// The second comparison of `P` with `fn() -> B` is reused from the first, with its summary. The
/// instance of `P`'s `T` is a local binder in that summary, which is applied as an instance with
/// its own bounds, so that `B`'s bounds can still instantiate it (#363) as when it's computed
/// again without the memo.
#[test]
fn test_inner_instance_reused() {
    with_large_stack(|| {
        let results = in_every_mode(inner_instance_reused);
        assert_eq!(results, [Comparison::Assign; 3]);
    });
}

/// Compares the types without the memo, in a single pass, and iterating from the start, each in a
/// new arena.
fn in_every_mode(types: impl Fn(&mut Types) -> (TypeId, TypeId)) -> [Comparison; 3] {
    let compare_in = |mode: usize| {
        let mut db = Types::new();
        let (lhs, rhs) = types(&mut db);

        match mode {
            0 => without_compare_memo(|| compare(&mut db.arena, &db.builtins, lhs, rhs)),
            1 => compare(&mut db.arena, &db.builtins, lhs, rhs),
            _ => with_fixpoint_budget(0, 0, || compare(&mut db.arena, &db.builtins, lhs, rhs)),
        }
    };

    [compare_in(0), compare_in(1), compare_in(2)]
}

/// A small deterministic PRNG (`SplitMix64`), to avoid a dependency.
struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9e37_79b9_7f4a_7c15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        z ^ (z >> 31)
    }

    fn below(&mut self, n: usize) -> usize {
        (self.next() % n as u64) as usize
    }

    fn chance(&mut self, percent: usize) -> bool {
        self.below(100) < percent
    }

    fn pick<T: Copy>(&mut self, items: &[T]) -> T {
        items[self.below(items.len())]
    }
}

/// An alias and its generic parameters.
#[derive(Clone)]
struct Declared {
    alias: TypeId,
    generics: Vec<TypeId>,
}

struct Generator<'a> {
    db: &'a mut Types,
    rng: Rng,
    aliases: Vec<Declared>,
    structs: Vec<TypeId>,
    /// Whether modules with a non-contractive alias are skipped, as the compiler rejects them.
    contractive_only: bool,
    /// The percentage of a generic function type's parameters and return types that may nest its
    /// own generics inside of them, rather than being one of them or not referencing them.
    nesting: usize,
}

impl Generator<'_> {
    fn atom(&mut self) -> TypeId {
        let BuiltinTypes {
            int,
            bytes,
            bytes32,
            nil,
            bool,
            bool_true,
            ..
        } = self.db.builtins;
        self.rng.pick(&[int, bytes, bytes32, nil, bool, bool_true])
    }

    /// Generates a type, where `generics` are the generics in scope (an alias's parameters and the
    /// generics of enclosing function types).
    fn random_type(&mut self, depth: usize, generics: &[TypeId]) -> TypeId {
        if depth >= 3 {
            return if !generics.is_empty() && self.rng.chance(40) {
                self.rng.pick(generics)
            } else if self.rng.chance(50) {
                self.reference(depth, generics)
            } else {
                self.atom()
            };
        }

        match self.rng.below(16) {
            0..=1 => self.atom(),
            2..=3 if !generics.is_empty() => self.rng.pick(generics),
            2..=5 => self.reference(depth, generics),
            6..=8 => {
                let first = self.random_type(depth + 1, generics);
                let rest = self.random_type(depth + 1, generics);
                self.db.pair(first, rest)
            }
            9..=10 => {
                let count = 2 + self.rng.below(2);
                let types = (0..count)
                    .map(|_| self.random_type(depth + 1, generics))
                    .collect();
                self.db.union(types)
            }
            11..=13 => self.function(depth, generics),
            14 => {
                // Reuse structs sometimes, so that the same semantic appears in both types
                if !self.structs.is_empty() && self.rng.chance(50) {
                    self.structs[self.rng.below(self.structs.len())]
                } else {
                    let inner = self.random_type(depth + 1, generics);
                    let id = self.db.structure(inner);
                    self.structs.push(id);
                    id
                }
            }
            _ => self.db.builtins.permissive_any,
        }
    }

    /// A reference to one of the aliases, with arguments for its generics.
    fn reference(&mut self, depth: usize, generics: &[TypeId]) -> TypeId {
        let Declared {
            alias,
            generics: params,
        } = self.aliases[self.rng.below(self.aliases.len())].clone();

        if params.is_empty() {
            return alias;
        }

        let args = params
            .iter()
            .map(|&param| {
                let arg = if !generics.is_empty() && self.rng.chance(60) {
                    self.rng.pick(generics)
                } else if depth < 2 && self.rng.chance(30) {
                    self.random_type(depth + 2, generics)
                } else {
                    self.atom()
                };
                (param, arg)
            })
            .collect();

        self.db.apply(alias, args)
    }

    fn function(&mut self, depth: usize, generics: &[TypeId]) -> TypeId {
        let own: Vec<TypeId> = (0..self.rng.below(3)).map(|_| self.db.generic()).collect();
        let scope: Vec<TypeId> = generics.iter().chain(&own).copied().collect();

        // Usually the function's own generics are its parameters or return type, which is what
        // the compiler's generic functions look like. Sometimes they're nested inside of them.
        let component = |this: &mut Self| {
            if !own.is_empty() && this.rng.chance(45) {
                this.rng.pick(&own)
            } else if this.rng.chance(this.nesting) {
                this.random_type(depth + 1, &scope)
            } else {
                this.random_type(depth + 1, generics)
            }
        };

        let params = (0..=self.rng.below(3)).map(|_| component(self)).collect();
        let ret = component(self);
        self.db.function(own, params, ret)
    }

    /// Declares a few mutually recursive aliases, some of which have generics. Returns `None` if
    /// any of them isn't regular, or (if `contractive_only` is set) isn't contractive, since the
    /// compiler rejects those.
    fn module(&mut self) -> Option<()> {
        let count = 1 + self.rng.below(3);

        for i in 0..count {
            let generics: Vec<TypeId> = (0..self.rng.below(4)).map(|_| self.db.generic()).collect();
            let alias = self.db.declare_alias(&format!("T{i}"), generics.clone());
            self.aliases.push(Declared { alias, generics });
        }

        for declared in self.aliases.clone() {
            let inner = self.random_type(0, &declared.generics);
            self.db.define(declared.alias, inner);
        }

        // Substituting a non-regular type (such as `type L<T> = (T, L<(T, T)>);`) never finishes,
        // so that's checked first
        for declared in self.aliases.clone() {
            if is_non_regular(&self.db.arena, declared.alias)
                || (self.contractive_only && is_non_contractive(&mut self.db.arena, declared.alias))
            {
                return None;
            }
        }

        Some(())
    }

    /// A type without free generics to compare, which is usually one of the aliases.
    fn query(&mut self) -> TypeId {
        if self.rng.chance(60) {
            self.reference(2, &[])
        } else {
            self.random_type(1, &[])
        }
    }
}

/// The observable results of comparing two types in every way that compares them, so that they
/// can be compared with the memo on and off.
fn results(db: &mut Types, lhs: TypeId, rhs: TypeId, target: TypeId, generic: TypeId) -> String {
    let forward = compare(&mut db.arena, &db.builtins, lhs, rhs);
    let backward = compare(&mut db.arena, &db.builtins, rhs, lhs);

    let mut infer = Inference::new([generic]);
    let inferred =
        compare_with_inference(&mut db.arena, &db.builtins, lhs, target, Some(&mut infer));

    // The candidates are substituted again by each comparison, so they're compared by name
    let candidates = infer.candidates(generic).unwrap().clone();
    let mut names = |types: &[TypeId]| {
        types
            .iter()
            .map(|&ty| stringify(&mut db.arena, ty))
            .collect::<Vec<_>>()
    };
    let candidates = [
        names(&candidates.lower),
        names(&candidates.upper),
        names(&candidates.held),
    ];

    let checked = check(&mut db.arena, &db.builtins, lhs, rhs);
    let intersection = intersect(&mut db.arena, &db.builtins, lhs, rhs);
    let intersection = stringify(&mut db.arena, intersection);

    renumber_generics(&format!(
        "{forward:?} {backward:?} {inferred:?} {candidates:?} {checked:?} {intersection}"
    ))
}

/// Numbers the unnamed generics in the text in the order they appear, since substituting a generic
/// function allocates new ones (whose ids differ between the runs).
fn renumber_generics(text: &str) -> String {
    const PREFIX: &str = "{generic ";

    let mut numbers = HashMap::new();
    let mut result = String::new();
    let mut rest = text;

    while let Some(start) = rest.find(PREFIX) {
        let after = &rest[start + PREFIX.len()..];
        let end = after.find('}').unwrap();
        let next = numbers.len();
        let number = *numbers.entry(&after[..end]).or_insert(next);
        result.push_str(&rest[..start]);
        write!(result, "{PREFIX}{number}}}").unwrap();
        rest = &after[end + 1..];
    }

    result.push_str(rest);
    result
}

#[derive(Debug, Default)]
struct Summary {
    cases: usize,
    skipped_modules: usize,
    /// Cases that only finished without the memo, which should be rare.
    gave_up_with_memo: usize,
    gave_up_without_memo: usize,
    gave_up_both: usize,
    /// Cases that only finished with the memo in a single pass, but not with a smaller budget.
    gave_up_iterating: usize,
    steps_with_memo: usize,
    steps_iterating: usize,
    steps_without_memo: usize,
}

std::thread_local! {
    static GIVING_UP: Cell<bool> = const { Cell::new(false) };
}

/// Runs the function, or returns `None` if it takes more than `STEP_LIMIT` steps. Giving up panics,
/// which isn't printed.
fn give_up_after<T>(f: impl FnOnce() -> T) -> Option<T> {
    static HOOK: Once = Once::new();
    HOOK.call_once(|| {
        let default = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| {
            if !GIVING_UP.with(Cell::get) || !is_step_limit(info.payload()) {
                default(info);
            }
        }));
    });

    GIVING_UP.with(|giving_up| giving_up.set(true));
    let result = catch_unwind(AssertUnwindSafe(|| with_step_limit(STEP_LIMIT, f)));
    GIVING_UP.with(|giving_up| giving_up.set(false));

    match result {
        Ok(value) => Some(value),
        Err(payload) if is_step_limit(payload.as_ref()) => None,
        Err(payload) => resume_unwind(payload),
    }
}

fn is_step_limit(payload: &(dyn Any + Send)) -> bool {
    let message = payload
        .downcast_ref::<&str>()
        .copied()
        .or_else(|| payload.downcast_ref::<String>().map(String::as_str));
    message == Some("step limit exceeded")
}

/// Each case gives up after this many steps, since it can be exponential without the memo (and
/// with it, for generic functions that reference their generics inside recursive types).
const STEP_LIMIT: usize = 20_000;

/// A random module, and the cases to compare in it.
struct Module {
    db: Types,
    aliases: Vec<Declared>,
    /// Two types, a target, and the generic being inferred in it.
    cases: Vec<[TypeId; 4]>,
}

/// Generates a random module from the seed, and the cases to compare in it: two types, and a target
/// with a generic being inferred, such as a parameter of type `(T, rhs)`. Returns `None` if the
/// module is skipped (see `Generator::module`).
fn random_module(
    seed: u64,
    queries: usize,
    contractive_only: bool,
    nesting: usize,
) -> Option<Module> {
    let mut db = Types::new();
    let mut generator = Generator {
        db: &mut db,
        rng: Rng(seed),
        aliases: Vec::new(),
        structs: Vec::new(),
        contractive_only,
        nesting,
    };

    // Substituting some generic aliases takes exponential work (since each application copies its
    // arguments), so the checks give up as well
    give_up_after(|| generator.module()).flatten()?;

    let cases = (0..queries)
        .map(|_| {
            let lhs = generator.query();
            let rhs = generator.query();
            let generic = generator.db.generic();
            let target = if generator.rng.chance(50) {
                generator.db.pair(generic, rhs)
            } else {
                generator.db.union(vec![generic, rhs])
            };

            [lhs, rhs, target, generic]
        })
        .collect();

    let aliases = generator.aliases;
    Some(Module { db, aliases, cases })
}

/// Compares random types with the memo on and off, and panics if any result differs.
fn differential(
    seeds: impl IntoIterator<Item = u64>,
    queries: usize,
    contractive_only: bool,
    nesting: usize,
) -> Summary {
    let mut summary = Summary::default();

    for seed in seeds {
        let Some(Module { mut db, cases, .. }) =
            random_module(seed, queries, contractive_only, nesting)
        else {
            summary.skipped_modules += 1;
            continue;
        };

        for [lhs, rhs, target, generic] in cases {
            let with_memo =
                give_up_after(|| count_steps(|| results(&mut db, lhs, rhs, target, generic)));

            let without_memo = give_up_after(|| {
                let ((results, steps), abandoned) = count_abandoned_passes(|| {
                    count_steps(|| {
                        without_compare_memo(|| results(&mut db, lhs, rhs, target, generic))
                    })
                });

                // Without the memo, iterating couldn't reuse anything, so it never does
                assert_eq!(abandoned, 0, "seed {seed}");
                (results, steps)
            });

            let name = |db: &mut Types, id| stringify(&mut db.arena, id);

            // Iterating from the start (with no budget), or after abandoning a single pass partway
            // (with a small one), must give the same results as a single pass
            if let Some((with_memo, _)) = &with_memo {
                for (per_item, base) in [(0, 0), (1, 64)] {
                    let iterating = give_up_after(|| {
                        count_steps(|| {
                            with_fixpoint_budget(per_item, base, || {
                                results(&mut db, lhs, rhs, target, generic)
                            })
                        })
                    });

                    if let Some((iterating, steps)) = iterating {
                        assert_eq!(
                            with_memo,
                            &iterating,
                            "seed {seed} (budget {per_item}, {base}): {} and {} (target {})",
                            name(&mut db, lhs),
                            name(&mut db, rhs),
                            name(&mut db, target),
                        );
                        summary.steps_iterating += steps;
                    } else {
                        summary.gave_up_iterating += 1;
                    }
                }
            }

            let (with_memo, steps, without_memo, steps_without) = match (with_memo, without_memo) {
                (Some((with_memo, steps)), Some((without_memo, steps_without))) => {
                    (with_memo, steps, without_memo, steps_without)
                }
                (None, Some(_)) => {
                    summary.gave_up_with_memo += 1;
                    continue;
                }
                (Some(_), None) => {
                    summary.gave_up_without_memo += 1;
                    continue;
                }
                (None, None) => {
                    summary.gave_up_both += 1;
                    continue;
                }
            };

            assert_eq!(
                with_memo,
                without_memo,
                "seed {seed}: {} and {} (target {})",
                name(&mut db, lhs),
                name(&mut db, rhs),
                name(&mut db, target),
            );

            summary.cases += 1;
            summary.steps_with_memo += steps;
            summary.steps_without_memo += steps_without;
        }
    }

    summary
}

#[test]
fn test_memo_differential() {
    let summary = with_large_stack(|| differential(0..400, 4, true, NESTING));
    println!("{summary:?}");

    // Most modules should be usable, and most cases should finish
    assert!(summary.cases > 800, "{summary:?}");
    assert!(summary.gave_up_with_memo <= 2, "{summary:?}");
    assert!(summary.gave_up_iterating <= 2, "{summary:?}");

    // This gave different results with the memo when it cached comparisons that refer to a generic
    // being instantiated
    let summary = with_large_stack(|| differential([1978], 8, true, NESTING));
    assert_eq!(summary.cases, 8, "{summary:?}");

    // This gave different results with the memo when it wasn't limited to contractive types. Its
    // module has a cycle through an application of another alias (#282), which the compiler now
    // rejects, so the comparisons must keep working when they're given such a type directly.
    let summary = with_large_stack(|| differential([4706], 8, false, NESTING));
    assert_eq!(summary.cases, 5, "{summary:?}");
}

/// The usual percentage of generic function types' parameters and return types that nest their
/// own generics, and a higher one for the tests of generic functions that do (#344).
const NESTING: usize = 15;
const HEAVY_NESTING: usize = 70;

#[test]
fn test_memo_differential_nested() {
    let summary = with_large_stack(|| differential(0..150, 4, true, HEAVY_NESTING));
    println!("{summary:?}");
    assert!(summary.cases > 250, "{summary:?}");
    // Comparisons of generic functions that nest their generics iterate in fixpoint mode, so none
    // of these give up with the memo (#344)
    assert_eq!(summary.gave_up_with_memo, 0, "{summary:?}");
    assert!(summary.gave_up_iterating <= 2, "{summary:?}");

    // These gave different results when generic functions that nest their generics were compared
    // in fixpoint mode while working on #344: iterating reused a comparison that merged a generic
    // into one instantiated outside of it (31451), and such a comparison's approximation decreased
    // from one round to the next, since its value depends on where it's compared (35565)
    let summary = with_large_stack(|| differential([31_451], 8, true, HEAVY_NESTING));
    assert_eq!(summary.cases, 5, "{summary:?}");
    let summary = with_large_stack(|| differential([35_565], 8, true, HEAVY_NESTING));
    assert!(summary.cases > 0, "{summary:?}");
}

/// The results of the cases that `differential` generates from the seeds with `HEAVY_NESTING`, with
/// the memo. Each seed lists its aliases (or `skipped`, if its module is), and then each case as
/// two lines: the types, and the results (or `gave up`, for a case that takes more than
/// `STEP_LIMIT` steps).
fn nested_baseline(seeds: impl IntoIterator<Item = u64>, queries: usize) -> String {
    let mut text = String::new();

    for seed in seeds {
        let Some(Module {
            mut db,
            aliases,
            cases,
        }) = random_module(seed, queries, true, HEAVY_NESTING)
        else {
            writeln!(text, "{seed}: skipped").unwrap();
            continue;
        };

        // The aliases that the cases refer to by name
        for Declared { alias, .. } in aliases {
            let Type::Alias(Alias { inner, .. }) = db.arena[alias] else {
                unreachable!();
            };
            let name = stringify(&mut db.arena, alias);
            let definition = stringify(&mut db.arena, inner);
            let line = format!("{seed}: type {name} = {definition}");
            writeln!(text, "{}", renumber_generics(&line)).unwrap();
        }

        for (index, [lhs, rhs, target, generic]) in cases.into_iter().enumerate() {
            let names = [lhs, rhs, target].map(|id| stringify(&mut db.arena, id));
            let results = give_up_after(|| results(&mut db, lhs, rhs, target, generic))
                .unwrap_or_else(|| "gave up".to_string());
            let line = format!(
                "{seed}.{index}: {} to {}, target {}\n    {results}",
                names[0], names[1], names[2]
            );
            writeln!(text, "{}", renumber_generics(&line)).unwrap();
        }
    }

    text
}

/// A baseline of the results of comparing generic functions that nest their generics, so that
/// every result that a change to how they're compared changes shows in the diff of
/// `nested_baseline.txt`, which `UPDATE_EXPECT=1` rewrites. It was added for #362's step 13b, whose
/// first step (instantiating each occurrence of a generic function on its own, rather than merging
/// the instances of one that's reached again) changed none of these: no case here revisits a
/// comparison whose bounds on an instance differ from the original's. The steps after it change
/// how the results are reused, and this checks they don't change any.
#[test]
fn test_nested_baseline() {
    // The cases of `test_memo_differential_nested`, including the seeds that merging instances
    // made fixpoint mode get wrong
    let text =
        with_large_stack(|| nested_baseline(0..150, 4) + &nested_baseline([31_451, 35_565], 8));
    expect_test::expect_file!["nested_baseline.txt"].assert_eq(&text);
}

/// A larger run of `test_memo_differential_nested`, which takes a few minutes in release builds.
#[test]
#[ignore = "slow"]
fn test_memo_differential_nested_large() {
    let summary = with_large_stack(|| differential(30_000..45_000, 8, true, HEAVY_NESTING));
    println!("{summary:?}");
}

/// A larger run of `test_memo_differential`, which takes a few minutes in release builds:
/// `cargo test --release -p rue-types --test memo -- --ignored --nocapture`
#[test]
#[ignore = "slow"]
fn test_memo_differential_large() {
    let summary = with_large_stack(|| differential(1_000..21_000, 8, true, NESTING));
    println!("{summary:?}");
}

/// Checks that if no alias in a random module is non-contractive, no type built from them can
/// reach a non-contractive cycle either, which is what comparisons rely on. This missed cycles
/// that only pass through an application of an alias other than the root, such as
/// `type T0 = T1<Int>; type T1<A> = A | (nil, A) | T0;` (#282).
fn contractiveness(seeds: impl IntoIterator<Item = u64>) {
    for seed in seeds {
        let mut db = Types::new();
        let mut generator = Generator {
            db: &mut db,
            rng: Rng(seed),
            aliases: Vec::new(),
            structs: Vec::new(),
            contractive_only: true,
            nesting: NESTING,
        };

        let Some(Some(())) = give_up_after(|| generator.module()) else {
            continue;
        };

        let mut types: Vec<TypeId> = generator
            .aliases
            .iter()
            .map(|declared| declared.alias)
            .collect();
        types.extend((0..4).map(|_| generator.query()));

        for ty in types {
            let Some(substituted) = give_up_after(|| substitute(&mut db.arena, ty)) else {
                continue;
            };

            assert!(
                !reaches_non_contractive_cycle(&db.arena, substituted),
                "seed {seed}: {}",
                stringify(&mut db.arena, ty)
            );
        }
    }
}

#[test]
fn test_contractiveness() {
    with_large_stack(|| contractiveness(0..500));
}

/// A larger run of `test_contractiveness`, which takes about two minutes in release builds:
/// `cargo test --release -p rue-types --test memo -- --ignored`
#[test]
#[ignore = "slow"]
fn test_contractiveness_large() {
    with_large_stack(|| contractiveness(0..200_000));
}

#[test]
fn test_comparison_step_limit() {
    with_large_stack(|| {
        let mut db = Types::new();
        let (r, _) = merged_into_outer_function(&mut db);

        let (result, steps) = count_steps(|| compare_limited(&mut db.arena, &db.builtins, r, r));
        assert_eq!(result, Ok(Comparison::Assign));

        // With a limit below what it takes, the comparison gives up, which `compare` treats as
        // `Invalid`. Since #362's step 13b.3 it takes few enough steps (about 20) that a fraction
        // of what it takes in all, including substituting and checking the types, isn't below it
        assert!(steps > 2);
        let limit = 2;
        let limited = with_comparison_step_limit(limit, || {
            compare_limited(&mut db.arena, &db.builtins, r, r)
        });
        assert_eq!(limited, Err(TooComplex));

        let limited =
            with_comparison_step_limit(limit, || compare(&mut db.arena, &db.builtins, r, r));
        assert_eq!(limited, Comparison::Invalid);

        // Comparing again afterwards isn't affected
        let result = compare_limited(&mut db.arena, &db.builtins, r, r);
        assert_eq!(result, Ok(Comparison::Assign));
    });
}

#[test]
fn test_check_step_limit() {
    let mut db = Types::new();
    let (n0, n1) = mutually_recursive(&mut db);

    // This makes many comparisons, but once one gives up, the others give up right away rather than
    // each taking as many steps
    let (result, steps) = count_steps(|| {
        with_comparison_step_limit(100, || check(&mut db.arena, &db.builtins, n0, n1))
    });
    assert_eq!(result, Err(CheckError::TooComplex));
    assert!(steps < 250, "took {steps} steps");

    let result = check(&mut db.arena, &db.builtins, n0, n1);
    assert_eq!(result, Err(CheckError::RecursiveType));
}
