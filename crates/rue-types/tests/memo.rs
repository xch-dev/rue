//! Tests that reusing the results of earlier comparisons doesn't change what they return, and that
//! it keeps comparing recursive types fast.
//!
//! The differential tests compare random recursive types, generic aliases and generic function
//! types with the comparison memo on and off (`without_compare_memo`). The work without the memo
//! can be exponential, so the types are kept small, and each case without the memo gives up after a
//! number of steps (`with_step_limit`).

use std::{
    collections::HashMap,
    panic::{AssertUnwindSafe, catch_unwind},
    sync::Once,
};

use id_arena::Arena;
use indexmap::IndexSet;
use rue_diagnostic::Name;
use rue_types::{
    Alias, Apply, BuiltinTypes, Comparison, FunctionParameter, FunctionType, Generic, Inference,
    Pair, Struct, Type, TypeId, Union, check, compare, compare_with_inference, count_steps,
    intersect, is_non_contractive, is_non_regular, stringify, with_step_limit,
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
        .unwrap()
}

#[test]
fn test_rotating_generic_function() {
    with_large_stack(|| {
        let mut db = Types::new();
        let lhs = rotating_function(&mut db);
        let rhs = rotating_function(&mut db);

        // This never finished, since nearly every comparison relied on one in progress, and
        // memoizing was turned off while the function's generic was being instantiated. It takes
        // about 97,000 steps now.
        let (result, steps) = count_steps(|| {
            with_step_limit(STEP_BOUND, || {
                compare(&mut db.arena, &db.builtins, lhs, rhs)
            })
        });
        assert_eq!(result, Comparison::Assign);
        assert!(steps < 300_000, "took {steps} steps");
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

    let (_, steps) = count_steps(|| compare(&mut db.arena, &db.builtins, n0, target));
    assert!(steps < 20_000, "took {steps} steps");
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
            } else if this.rng.chance(15) {
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
    /// any of them isn't regular or contractive, since the compiler rejects those.
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
                || is_non_contractive(&mut self.db.arena, declared.alias)
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

    format!("{forward:?} {backward:?} {inferred:?} {candidates:?} {checked:?} {intersection}")
}

#[derive(Debug, Default)]
struct Summary {
    cases: usize,
    skipped_modules: usize,
    /// Cases that only finished without the memo, which should be rare.
    gave_up_with_memo: usize,
    gave_up_without_memo: usize,
    gave_up_both: usize,
    steps_with_memo: usize,
    steps_without_memo: usize,
}

/// Each case gives up after this many steps, since it can be exponential without the memo (and
/// with it, for generic functions that reference their generics inside recursive types).
const STEP_LIMIT: usize = 20_000;

/// Compares random types with the memo on and off, and panics if any result differs.
fn differential(seeds: impl IntoIterator<Item = u64>, queries: usize) -> Summary {
    // Giving up panics, which shouldn't be printed
    static QUIET: Once = Once::new();
    QUIET.call_once(|| {
        let default = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| {
            if !info.to_string().contains("step limit") {
                default(info);
            }
        }));
    });

    let mut summary = Summary::default();

    for seed in seeds {
        let mut db = Types::new();
        let mut generator = Generator {
            db: &mut db,
            rng: Rng(seed),
            aliases: Vec::new(),
            structs: Vec::new(),
        };

        // Substituting some generic aliases takes exponential work (since each application copies
        // its arguments), so the checks give up as well
        let module = catch_unwind(AssertUnwindSafe(|| {
            with_step_limit(STEP_LIMIT, || generator.module())
        }));

        if !matches!(module, Ok(Some(()))) {
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
            let with_memo = catch_unwind(AssertUnwindSafe(|| {
                count_steps(|| {
                    with_step_limit(STEP_LIMIT, || results(&mut db, lhs, rhs, target, generic))
                })
            }));

            let without_memo = catch_unwind(AssertUnwindSafe(|| {
                count_steps(|| {
                    without_compare_memo(|| {
                        with_step_limit(STEP_LIMIT, || results(&mut db, lhs, rhs, target, generic))
                    })
                })
            }));

            let (with_memo, steps, without_memo, steps_without) = match (with_memo, without_memo) {
                (Ok((with_memo, steps)), Ok((without_memo, steps_without))) => {
                    (with_memo, steps, without_memo, steps_without)
                }
                (Err(_), Ok(_)) => {
                    summary.gave_up_with_memo += 1;
                    continue;
                }
                (Ok(_), Err(_)) => {
                    summary.gave_up_without_memo += 1;
                    continue;
                }
                (Err(_), Err(_)) => {
                    summary.gave_up_both += 1;
                    continue;
                }
            };

            let name = |db: &mut Types, id| stringify(&mut db.arena, id);
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
    let summary = with_large_stack(|| differential(0..400, 4));
    println!("{summary:?}");

    // Most modules should be usable, and most cases should finish
    assert!(summary.cases > 800, "{summary:?}");
    assert!(summary.gave_up_with_memo <= 2, "{summary:?}");

    // These gave different results with the memo when it wasn't limited to contractive types
    // (4706), or when it cached comparisons that refer to a generic being instantiated (1978)
    let summary = with_large_stack(|| differential([1978, 4706], 8));
    assert_eq!(summary.cases, 13, "{summary:?}");
}

/// A larger run of `test_memo_differential`, which takes a few minutes in release builds:
/// `cargo test --release -p rue-types --test memo -- --ignored --nocapture`
#[test]
#[ignore = "slow"]
fn test_memo_differential_large() {
    let summary = with_large_stack(|| differential(1_000..21_000, 8));
    println!("{summary:?}");
}
