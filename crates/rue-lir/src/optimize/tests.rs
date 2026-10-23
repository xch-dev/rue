use clvmr::{Allocator, ChiaDialect, NodePtr, run_program, serde::node_to_bytes};
use expect_test::{Expect, expect};
use id_arena::Arena;
use num_bigint::BigUint;
use rue_clvm::{disassemble_program, disassemble_value};

use crate::{CLVM_FLAGS, CodegenOptions, Lir, LirId, codegen, optimize};

fn atom(arena: &mut Arena<Lir>, bytes: &[u8]) -> LirId {
    arena.alloc(Lir::Atom(bytes.to_vec()))
}

fn path(arena: &mut Arena<Lir>, path: u32) -> LirId {
    arena.alloc(Lir::Path(BigUint::from(path)))
}

fn raise(arena: &mut Arena<Lir>) -> LirId {
    arena.alloc(Lir::Raise(vec![]))
}

fn not(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    arena.alloc(Lir::Not(value))
}

fn compile(arena: &Arena<Lir>, allocator: &mut Allocator, lir: LirId, optimized: bool) -> NodePtr {
    codegen(
        arena,
        allocator,
        lir,
        CodegenOptions {
            optimize_static_pairs: optimized,
        },
    )
    .unwrap()
}

fn run(allocator: &mut Allocator, program: NodePtr, env: NodePtr) -> Option<Vec<u8>> {
    let dialect = ChiaDialect::new(CLVM_FLAGS);
    let output = run_program(allocator, &dialect, program, env, 11_000_000_000).ok()?;
    Some(node_to_bytes(allocator, output.1).unwrap())
}

// Optimizes the LIR, checks the generated code, and checks that the optimized
// and unoptimized programs behave the same for each of the given environments
#[allow(clippy::needless_pass_by_value)]
fn check(arena: &mut Arena<Lir>, lir: LirId, envs: &[&str], expect: Expect) {
    let optimized = optimize(arena, lir);

    let mut allocator = Allocator::new();
    let unoptimized_ptr = compile(arena, &mut allocator, lir, false);
    let optimized_ptr = compile(arena, &mut allocator, optimized, true);

    expect.assert_eq(&disassemble_program(&allocator, optimized_ptr));

    for env in envs {
        let env_ptr = rue_clvm::assemble(&mut allocator, env).unwrap();
        let expected = run(&mut allocator, unoptimized_ptr, env_ptr);
        let actual = run(&mut allocator, optimized_ptr, env_ptr);
        assert_eq!(
            expected,
            actual,
            "{} and {} differ for env {env}",
            disassemble_program(&allocator, unoptimized_ptr),
            disassemble_program(&allocator, optimized_ptr),
        );
    }
}

const ENVS: &[&str] = &["()", "(())", "(5)", "(1)", "(0x00)", "((1 . 2))", "(5 7)"];

#[test]
fn test_not_ne_nil_is_not_value() {
    // (not (= x ())) must not be optimized to x
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let nil = atom(&mut arena, &[]);
    let eq = arena.alloc(Lir::Eq(x, nil));
    let lir = not(&mut arena, eq);
    check(&mut arena, lir, ENVS, expect!["(not (= 2 ()))"]);
}

#[test]
fn test_double_not() {
    // (not (not x)) is only x if x is already a boolean
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let inner = not(&mut arena, x);
    let lir = not(&mut arena, inner);
    check(&mut arena, lir, ENVS, expect!["(not (not 2))"]);

    let x = path(&mut arena, 2);
    let listp = arena.alloc(Lir::Listp(x, true));
    let inner = not(&mut arena, listp);
    let lir = not(&mut arena, inner);
    check(&mut arena, lir, ENVS, expect!["(l 2)"]);

    // Triple not only needs a single not
    let x = path(&mut arena, 2);
    let a = not(&mut arena, x);
    let b = not(&mut arena, a);
    let lir = not(&mut arena, b);
    check(&mut arena, lir, ENVS, expect!["(not 2)"]);
}

#[test]
fn test_not_eq_boolean_nil() {
    // (= (l x) ()) is (not (l x)), since (l x) is always an atom
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let listp = arena.alloc(Lir::Listp(x, true));
    let nil = atom(&mut arena, &[]);
    let eq = arena.alloc(Lir::Eq(listp, nil));
    let lir = not(&mut arena, eq);
    check(&mut arena, lir, ENVS, expect!["(l 2)"]);
}

#[test]
fn test_eq_nil_keeps_eq() {
    // (= x ()) fails if x is a pair, so it can't become (not x) in a condition
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let nil = atom(&mut arena, &[]);
    let eq = arena.alloc(Lir::Eq(nil, x));
    let one = atom(&mut arena, &[1]);
    let two = atom(&mut arena, &[2]);
    let lir = arena.alloc(Lir::If(eq, one, two, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(a (i (= () 2) (q 1 . 1) (q 1 . 2)) 1)"],
    );
}

#[test]
fn test_listp_no_type_assumption() {
    // (l x) can't be replaced by x, even if x is supposed to be a list
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let listp = arena.alloc(Lir::Listp(x, false));
    let one = atom(&mut arena, &[1]);
    let two = atom(&mut arena, &[2]);
    let lir = arena.alloc(Lir::If(listp, one, two, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(a (i (l 2) (q 1 . 1) (q 1 . 2)) 1)"],
    );
}

#[test]
fn test_not_pure_pair() {
    let mut arena = Arena::new();
    let a = atom(&mut arena, &[1]);
    let b = atom(&mut arena, &[]);
    let pair = arena.alloc(Lir::Cons(a, b));
    let lir = not(&mut arena, pair);
    check(&mut arena, lir, ENVS, expect!["()"]);
}

#[test]
fn test_not_impure_pair() {
    // The pair is truthy, but evaluating it fails
    let mut arena = Arena::new();
    let a = raise(&mut arena);
    let b = atom(&mut arena, &[]);
    let pair = arena.alloc(Lir::Cons(a, b));
    let lir = not(&mut arena, pair);
    check(&mut arena, lir, ENVS, expect!["(not (c (x) ()))"]);

    // Paths can fail if they traverse into an atom
    let a = path(&mut arena, 5);
    let b = atom(&mut arena, &[]);
    let pair = arena.alloc(Lir::Cons(a, b));
    let lir = not(&mut arena, pair);
    check(&mut arena, lir, ENVS, expect!["(not (c 5 ()))"]);
}

#[test]
fn test_if_impure_pair() {
    let mut arena = Arena::new();
    let a = raise(&mut arena);
    let b = atom(&mut arena, &[]);
    let pair = arena.alloc(Lir::Cons(a, b));
    let one = atom(&mut arena, &[1]);
    let two = atom(&mut arena, &[2]);
    let lir = arena.alloc(Lir::If(pair, one, two, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(a (i (c (x) ()) (q 1 . 1) (q 1 . 2)) 1)"],
    );

    let a = atom(&mut arena, &[1]);
    let b = atom(&mut arena, &[]);
    let pair = arena.alloc(Lir::Cons(a, b));
    let one = atom(&mut arena, &[1]);
    let two = atom(&mut arena, &[2]);
    let lir = arena.alloc(Lir::If(pair, one, two, false));
    check(&mut arena, lir, ENVS, expect!["(q . 1)"]);
}

#[test]
fn test_if_not_flips() {
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let cond = not(&mut arena, x);
    let one = atom(&mut arena, &[1]);
    let two = atom(&mut arena, &[2]);
    let lir = arena.alloc(Lir::If(cond, one, two, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(a (i 2 (q 1 . 2) (q 1 . 1)) 1)"],
    );

    // A double not in a condition only matters for its truthiness
    let x = path(&mut arena, 2);
    let a = not(&mut arena, x);
    let cond = not(&mut arena, a);
    let one = atom(&mut arena, &[1]);
    let two = atom(&mut arena, &[2]);
    let lir = arena.alloc(Lir::If(cond, one, two, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(a (i 2 (q 1 . 1) (q 1 . 2)) 1)"],
    );
}

#[test]
fn test_all_any_impure_pair() {
    let mut arena = Arena::new();
    let a = raise(&mut arena);
    let b = atom(&mut arena, &[]);
    let pair = arena.alloc(Lir::Cons(a, b));
    let x = path(&mut arena, 2);
    let lir = arena.alloc(Lir::All(vec![pair, x]));
    check(&mut arena, lir, ENVS, expect!["(all (c (x) ()) 2)"]);

    let a = raise(&mut arena);
    let b = atom(&mut arena, &[]);
    let pair = arena.alloc(Lir::Cons(a, b));
    let x = path(&mut arena, 2);
    let lir = arena.alloc(Lir::Any(vec![pair, x]));
    check(&mut arena, lir, ENVS, expect!["(any (c (x) ()) 2)"]);
}

#[test]
fn test_all_any_known() {
    let mut arena = Arena::new();
    let one = atom(&mut arena, &[1]);
    let a = atom(&mut arena, &[1]);
    let b = atom(&mut arena, &[]);
    let pair = arena.alloc(Lir::Cons(a, b));
    let x = path(&mut arena, 2);
    let lir = arena.alloc(Lir::All(vec![one, pair, x]));
    check(&mut arena, lir, ENVS, expect!["(all 2)"]);

    let nil = atom(&mut arena, &[]);
    let x = path(&mut arena, 2);
    let lir = arena.alloc(Lir::Any(vec![nil, x]));
    check(&mut arena, lir, ENVS, expect!["(any 2)"]);

    // The other arguments must still be evaluated, even though the result is known
    let nil = atom(&mut arena, &[]);
    let x = path(&mut arena, 5);
    let lir = arena.alloc(Lir::All(vec![nil, x]));
    check(&mut arena, lir, ENVS, expect!["(all () 5)"]);

    let one = atom(&mut arena, &[1]);
    let x = path(&mut arena, 5);
    let lir = arena.alloc(Lir::Any(vec![one, x]));
    check(&mut arena, lir, ENVS, expect!["(any (q . 1) 5)"]);

    // Double nots in arguments only matter for their truthiness
    let x = path(&mut arena, 2);
    let a = not(&mut arena, x);
    let b = not(&mut arena, a);
    let lir = arena.alloc(Lir::All(vec![b]));
    check(&mut arena, lir, ENVS, expect!["(all 2)"]);
}

#[test]
fn test_non_canonical_atoms_are_truthy() {
    let mut arena = Arena::new();
    for bytes in [&[0x00][..], &[0x00, 0x00], &[0x80]] {
        let value = atom(&mut arena, bytes);
        let lir = not(&mut arena, value);
        check(&mut arena, lir, ENVS, expect!["()"]);
    }
}

struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        self.0
    }

    fn below(&mut self, n: u64) -> u64 {
        self.next() % n
    }

    fn pick<T: Copy>(&mut self, items: &[T]) -> T {
        items[self.below(items.len() as u64) as usize]
    }
}

const FUZZ_ATOMS: &[&[u8]] = &[
    &[],
    &[1],
    &[2],
    &[0x00],
    &[0x00, 0x01],
    &[0xff],
    &[0x80],
    &[0x01, 0x00],
];

// These paths never share a parent, since opt_cons intentionally collapses a pair of sibling
// paths into their parent path, assuming that the parent is a pair (see its documentation)
// For the same reason, first and rest are never applied directly to a path
const FUZZ_PATHS: &[u32] = &[1, 2, 5, 11];

fn generate(arena: &mut Arena<Lir>, rng: &mut Rng, depth: u32) -> LirId {
    if depth == 0 || rng.below(5) == 0 {
        return if rng.below(2) == 0 {
            let bytes = rng.pick(FUZZ_ATOMS);
            atom(arena, bytes)
        } else {
            let p = rng.pick(FUZZ_PATHS);
            path(arena, p)
        };
    }

    let gen_args = |arena: &mut Arena<Lir>, rng: &mut Rng, min: u64| {
        let count = min + rng.below(3);
        (0..count)
            .map(|_| generate(arena, rng, depth - 1))
            .collect::<Vec<_>>()
    };

    let mut args = gen_args(arena, rng, 3);
    let mut next = || args.pop().unwrap();

    let lir = match rng.below(18) {
        0 => Lir::Not(next()),
        1 => Lir::Eq(next(), next()),
        2 => {
            let nil = atom(arena, &[]);
            if rng.below(2) == 0 {
                Lir::Eq(next(), nil)
            } else {
                Lir::Eq(nil, next())
            }
        }
        3 => Lir::Cons(next(), next()),
        4 => Lir::Listp(next(), rng.below(2) == 0),
        5 => Lir::If(next(), next(), next(), rng.below(2) == 0),
        6 => Lir::All(gen_args(arena, rng, 0)),
        7 => Lir::Any(gen_args(arena, rng, 0)),
        8 => Lir::Raise(vec![]),
        9 | 10 => {
            let mut value = next();
            while matches!(arena[value], Lir::Path(_)) {
                value = generate(arena, rng, depth - 1);
            }
            if rng.below(2) == 0 {
                Lir::First(value)
            } else {
                Lir::Rest(value)
            }
        }
        11 => Lir::Gt(next(), next()),
        12 => Lir::Add(gen_args(arena, rng, 1)),
        // The quoted program is itself optimized, so only quote atoms to keep the value the same
        13 => {
            let bytes = rng.pick(FUZZ_ATOMS);
            Lir::Quote(atom(arena, bytes))
        }
        14 => Lir::Sub(gen_args(arena, rng, 1)),
        15 => Lir::Mod(next(), next()),
        // Checks whether a value is odd
        16 => {
            let two = atom(arena, &[2]);
            let one = atom(arena, &[1]);
            let value = arena.alloc(Lir::Mod(next(), two));
            Lir::Eq(value, one)
        }
        // Runs a program in either a nil or the current environment
        17 => {
            let program = arena.alloc(Lir::Quote(next()));
            let env = if rng.below(2) == 0 {
                atom(arena, &[])
            } else {
                path(arena, 1)
            };
            Lir::Run(program, env)
        }
        _ => unreachable!(),
    };

    arena.alloc(lir)
}

fn generate_env(allocator: &mut Allocator, rng: &mut Rng) -> NodePtr {
    let mut env = NodePtr::NIL;

    for _ in 0..rng.below(4) {
        let item = if rng.below(4) == 0 {
            let first = allocator.new_atom(rng.pick(FUZZ_ATOMS)).unwrap();
            let rest = allocator.new_atom(rng.pick(FUZZ_ATOMS)).unwrap();
            allocator.new_pair(first, rest).unwrap()
        } else {
            allocator.new_atom(rng.pick(FUZZ_ATOMS)).unwrap()
        };
        env = allocator.new_pair(item, env).unwrap();
    }

    env
}

// Generates random LIR trees and checks that the optimized program behaves exactly
// like the unoptimized one, with every error treated as equal
#[test]
fn fuzz_optimizer_differential() {
    let mut rng = Rng(0x9e37_79b9_7f4a_7c15);

    for _ in 0..20_000 {
        let mut arena = Arena::new();
        let lir = generate(&mut arena, &mut rng, 4);
        let optimized = optimize(&mut arena, lir);

        let mut allocator = Allocator::new();
        let unoptimized_ptr = compile(&arena, &mut allocator, lir, false);
        let optimized_ptr = compile(&arena, &mut allocator, optimized, true);

        for _ in 0..6 {
            let env = generate_env(&mut allocator, &mut rng);
            let expected = run(&mut allocator, unoptimized_ptr, env);
            let actual = run(&mut allocator, optimized_ptr, env);

            if expected != actual {
                let show = |allocator: &mut Allocator, value: Option<Vec<u8>>| {
                    value.map_or("error".to_string(), |bytes| {
                        let ptr = clvmr::serde::node_from_bytes(allocator, &bytes).unwrap();
                        disassemble_value(allocator, ptr)
                    })
                };
                let expected = show(&mut allocator, expected);
                let actual = show(&mut allocator, actual);
                panic!(
                    "optimizer changed behavior for env {}\nunoptimized: {} => {expected}\noptimized: {} => {actual}",
                    disassemble_value(&allocator, env),
                    disassemble_program(&allocator, unoptimized_ptr),
                    disassemble_program(&allocator, optimized_ptr),
                );
            }
        }
    }
}
