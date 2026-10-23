//! Differential tests for optimizations that involve the environment (paths, `Run`, `Cons`
//! and closures). Random LIR trees are compiled with and without optimizations, then run
//! against random environments (including atoms where pairs would normally be), and the
//! results must be identical (or both must fail).

use clvmr::{Allocator, ChiaDialect, NodePtr, run_program, serde::node_to_bytes};
use id_arena::Arena;
use num_bigint::BigUint;
use rue_clvm::disassemble_program;
use rue_lir::{CLVM_FLAGS, CodegenOptions, Lir, LirId, codegen, optimize};

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
}

const ATOMS: &[&[u8]] = &[&[], &[1], &[2], &[3], &[0x00], &[0xff], &[0x01, 0x00]];
const PATHS: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 11];

fn gen_atom(arena: &mut Arena<Lir>, rng: &mut Rng) -> LirId {
    let atom = ATOMS[rng.below(ATOMS.len() as u64) as usize];
    arena.alloc(Lir::Atom(atom.to_vec()))
}

fn gen_path(arena: &mut Arena<Lir>, rng: &mut Rng) -> LirId {
    let path = PATHS[rng.below(PATHS.len() as u64) as usize];
    arena.alloc(Lir::Path(BigUint::from(path)))
}

fn gen_env(arena: &mut Arena<Lir>, rng: &mut Rng, depth: u32) -> LirId {
    match rng.below(5) {
        0 => arena.alloc(Lir::Atom(vec![])),
        1 => arena.alloc(Lir::Path(BigUint::from(1u8))),
        _ => gen_lir(arena, rng, depth),
    }
}

fn gen_lir(arena: &mut Arena<Lir>, rng: &mut Rng, depth: u32) -> LirId {
    if depth == 0 || rng.below(5) == 0 {
        return if rng.below(2) == 0 {
            gen_atom(arena, rng)
        } else {
            gen_path(arena, rng)
        };
    }

    let d = depth - 1;

    match rng.below(14) {
        0 | 1 => {
            let value = gen_lir(arena, rng, d);
            let callee = arena.alloc(Lir::Quote(value));
            let env = gen_env(arena, rng, d);
            arena.alloc(Lir::Run(callee, env))
        }
        2 => {
            let callee = gen_lir(arena, rng, d);
            let env = gen_env(arena, rng, d);
            arena.alloc(Lir::Run(callee, env))
        }
        3 => {
            let body = gen_lir(arena, rng, d);
            let function = arena.alloc(Lir::Quote(body));
            let captures = (0..rng.below(3)).map(|_| gen_lir(arena, rng, d)).collect();
            let has_parameters = rng.below(2) == 0;
            let closure = arena.alloc(Lir::Closure(function, captures, has_parameters));
            let env = gen_env(arena, rng, d);
            arena.alloc(Lir::Run(closure, env))
        }
        4 | 5 => {
            let value = gen_lir(arena, rng, d);
            arena.alloc(Lir::First(value))
        }
        6 | 7 => {
            let value = gen_lir(arena, rng, d);
            arena.alloc(Lir::Rest(value))
        }
        8 | 9 => {
            // Bias towards `(c (f x) (r x))` shaped pairs
            if rng.below(2) == 0 {
                let path = gen_path(arena, rng);
                let first = arena.alloc(Lir::First(path));
                let rest = arena.alloc(Lir::Rest(path));
                arena.alloc(Lir::Cons(first, rest))
            } else {
                let first = gen_lir(arena, rng, d);
                let rest = gen_lir(arena, rng, d);
                arena.alloc(Lir::Cons(first, rest))
            }
        }
        10 => {
            let value = gen_lir(arena, rng, d);
            arena.alloc(Lir::Listp(value, true))
        }
        11 => {
            let args = (0..=rng.below(3)).map(|_| gen_lir(arena, rng, d)).collect();
            arena.alloc(Lir::Add(args))
        }
        12 => {
            let args = (0..=rng.below(3)).map(|_| gen_lir(arena, rng, d)).collect();
            arena.alloc(Lir::Concat(args))
        }
        _ => {
            let value = gen_lir(arena, rng, d);
            arena.alloc(Lir::Strlen(value))
        }
    }
}

fn gen_value(allocator: &mut Allocator, rng: &mut Rng, depth: u32) -> NodePtr {
    if depth == 0 || rng.below(3) == 0 {
        let atom = ATOMS[rng.below(ATOMS.len() as u64) as usize];
        return allocator.new_atom(atom).unwrap();
    }

    let first = gen_value(allocator, rng, depth - 1);
    let rest = gen_value(allocator, rng, depth - 1);
    allocator.new_pair(first, rest).unwrap()
}

fn run(allocator: &mut Allocator, program: NodePtr, env: NodePtr) -> Option<Vec<u8>> {
    let dialect = ChiaDialect::new(CLVM_FLAGS);
    let output = run_program(allocator, &dialect, program, env, 11_000_000_000).ok()?;
    Some(node_to_bytes(allocator, output.1).unwrap())
}

#[test]
fn test_optimizer_env_differential() {
    let mut rng = Rng(0x9e37_79b9_7f4a_7c15);

    for _ in 0..20_000 {
        let mut arena = Arena::new();
        let lir = gen_lir(&mut arena, &mut rng, 5);
        let optimized = optimize(&mut arena, lir);

        let mut allocator = Allocator::new();

        let unoptimized_program = codegen(
            &arena,
            &mut allocator,
            lir,
            CodegenOptions {
                optimize_static_pairs: false,
            },
        )
        .unwrap();

        let optimized_program = codegen(
            &arena,
            &mut allocator,
            optimized,
            CodegenOptions {
                optimize_static_pairs: true,
            },
        )
        .unwrap();

        for _ in 0..8 {
            let env = gen_value(&mut allocator, &mut rng, 4);
            let expected = run(&mut allocator, unoptimized_program, env);
            let actual = run(&mut allocator, optimized_program, env);

            assert_eq!(
                expected,
                actual,
                "optimizer changed behavior\nenv: {}\nunoptimized: {}\noptimized: {}",
                disassemble_program(&allocator, env),
                disassemble_program(&allocator, unoptimized_program),
                disassemble_program(&allocator, optimized_program),
            );
        }
    }
}
