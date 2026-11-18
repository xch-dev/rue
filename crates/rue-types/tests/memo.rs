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
    stringify, substitute, with_comparison_step_limit, with_fixpoint_budget, with_step_limit,
    without_compare_memo,
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
// generics instantiated by another one. They're compared in exact mode as things are, so every
// mode agrees, and they guard any future change that compares them in fixpoint mode. All but
// `test_merged_into_outer_function` also fail if a comparison with an `Instance` binding is
// memoized. Most compare the types in a new arena in every mode, since the order of the types can
// change which comparisons are reached again with which binders.

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

        // Comparing `(Qf, Pf)` merges `T` into the outer function's `T` when `(Qx, Px)` is reached
        // again, and `(Qf, Pf)` is then reached again itself. Iterating that change gave it the
        // least value every round, since the comparison whose `T` it changed was a different one in
        // each round, so it never finished.
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
        // whose computation reaches `(U, ...)` against `(Never, ...)` again, which merges the
        // generic of the function it's in. Reusing it while comparing `S1`'s body to `H` skipped
        // the merge into `H`'s `U`, so its bounds weren't checked against each other, and this gave
        // `Assign` rather than `Invalid`.
        let results = in_every_mode(reused_across_functions);
        assert_eq!(results, [Comparison::Invalid; 3]);
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
        // Comparing `(k2, k1)` revisits the comparison of the first parameters, which merges into
        // the outer function's `U`. It was then reused under the comparison of the callbacks'
        // functions, whose first parameters have the same key but their own `U`, so their bounds
        // (`Bytes32` and `Int`) weren't checked against each other, and this was `Assign`
        for (bytes32, expected) in [(false, Comparison::Invalid), (true, Comparison::Assign)] {
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
            assert_eq!(run(2), expected);
            assert_eq!(run(0), expected);
            assert_eq!(run(1), expected);
        }
    });
}

/// `type K1 = fn(k: S1) -> nil; type S1 = fn<U>(p: (U, (K1, K1)), q: U) -> U;`
/// `type K2 = fn(k: S2) -> nil; type S2 = fn<W>(p: (Never, (K2, K2)), q: W) -> W;`
/// Returns `fn<U>(p: (U, (K1, K1))) -> nil` and `fn(p: (Never, (K2, K2))) -> nil`.
fn merged_twice(db: &mut Types) -> (TypeId, TypeId) {
    let BuiltinTypes { nil, never, .. } = db.builtins;
    let s1 = db.declare_alias("S1", vec![]);
    let s2 = db.declare_alias("S2", vec![]);
    let u = db.generic();
    let w = db.generic();
    let k1 = db.function(vec![], vec![s1], nil);
    let k2 = db.function(vec![], vec![s2], nil);
    let both_k1 = db.pair(k1, k1);
    let both_k2 = db.pair(k2, k2);
    let left = db.pair(u, both_k1);
    let right = db.pair(never, both_k2);
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
        // `W`, and merges that `U` into the outer one when the parameters are reached again. Reusing
        // the second one skipped its copy of the bounds, so the two `W`s weren't checked against
        // each other, and this gave `Assign`. Computing a result again doesn't have the same side
        // effects as the first time, even within the same comparisons.
        assert_eq!(run(2), Comparison::Invalid);
        assert_eq!(run(0), Comparison::Invalid);
        assert_eq!(run(1), Comparison::Invalid);
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
        // `(AL, AR)`, then the first functions, whose return types revisit those of the second ones
        // and merge `U` (with its `Bytes32` bound) into theirs. Skipping that gave `Assign`.
        let results = in_every_mode(reached_through_dependency);
        assert_eq!(results, [Comparison::Invalid; 3]);
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

/// Compares random types with the memo on and off, and panics if any result differs.
fn differential(
    seeds: impl IntoIterator<Item = u64>,
    queries: usize,
    contractive_only: bool,
    nesting: usize,
) -> Summary {
    let mut summary = Summary::default();

    for seed in seeds {
        let mut db = Types::new();
        let mut generator = Generator {
            db: &mut db,
            rng: Rng(seed),
            aliases: Vec::new(),
            structs: Vec::new(),
            contractive_only,
            nesting,
        };

        // Substituting some generic aliases takes exponential work (since each application copies
        // its arguments), so the checks give up as well
        if give_up_after(|| generator.module()).flatten().is_none() {
            summary.skipped_modules += 1;
            continue;
        }

        let cases: Vec<[TypeId; 4]> = (0..queries)
            .map(|_| {
                let lhs = generator.query();
                let rhs = generator.query();

                // A target with a generic being inferred, such as a parameter of type `(T, rhs)`
                let generic = generator.db.generic();
                let target = if generator.rng.chance(50) {
                    generator.db.pair(generic, rhs)
                } else {
                    generator.db.union(vec![generic, rhs])
                };

                [lhs, rhs, target, generic]
            })
            .collect();

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
    assert!(summary.gave_up_with_memo <= 2, "{summary:?}");
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
        let (r, rq) = merged_into_outer_function(&mut db);

        let (result, steps) = count_steps(|| compare_limited(&mut db.arena, &db.builtins, r, rq));
        assert_eq!(result, Ok(Comparison::Invalid));

        // With a limit below what it takes, the comparison gives up, which `compare` treats as
        // `Invalid`
        let limit = steps / 2;
        let limited = with_comparison_step_limit(limit, || {
            compare_limited(&mut db.arena, &db.builtins, r, rq)
        });
        assert_eq!(limited, Err(TooComplex));

        let limited =
            with_comparison_step_limit(limit, || compare(&mut db.arena, &db.builtins, rq, r));
        assert_eq!(limited, Comparison::Invalid);

        // Comparing again afterwards isn't affected
        let result = compare_limited(&mut db.arena, &db.builtins, r, rq);
        assert_eq!(result, Ok(Comparison::Invalid));
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
