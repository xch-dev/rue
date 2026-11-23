//! What the optimizer's tests share to build LIR, generate code from it and run it.

use clvmr::{Allocator, ChiaDialect, NodePtr, run_program, serde::node_to_bytes};
use id_arena::Arena;
use num_bigint::{BigInt, BigUint};
use rue_clvm::disassemble_program;

use crate::{ClvmTarget, CodegenOptions, Lir, LirId, bigint_atom, codegen};

pub fn atom(arena: &mut Arena<Lir>, bytes: &[u8]) -> LirId {
    arena.alloc(Lir::Atom(bytes.to_vec()))
}

pub fn int(arena: &mut Arena<Lir>, value: i64) -> LirId {
    arena.alloc(Lir::Atom(bigint_atom(BigInt::from(value))))
}

pub fn path(arena: &mut Arena<Lir>, path: u32) -> LirId {
    arena.alloc(Lir::Path(BigUint::from(path)))
}

/// Generates the code for the target, folding static pairs if `optimized` is set.
pub fn compile(
    arena: &Arena<Lir>,
    allocator: &mut Allocator,
    lir: LirId,
    target: ClvmTarget,
    optimized: bool,
) -> NodePtr {
    codegen(
        arena,
        allocator,
        lir,
        CodegenOptions {
            optimize_static_pairs: optimized,
            target,
        },
    )
    .unwrap()
}

/// Runs the program with the flags of the target, returning its serialized output, or `None` if
/// it fails.
pub fn run(
    allocator: &mut Allocator,
    target: ClvmTarget,
    program: NodePtr,
    env: NodePtr,
) -> Option<Vec<u8>> {
    let dialect = ChiaDialect::new(target.flags());
    let output = run_program(allocator, &dialect, program, env, crate::MAX_BLOCK_COST).ok()?;
    Some(node_to_bytes(allocator, output.1).unwrap())
}

/// The code generated for the target as it is today, with static pairs folded.
pub fn disassemble(arena: &Arena<Lir>, lir: LirId) -> String {
    let mut allocator = Allocator::new();
    let program = compile(arena, &mut allocator, lir, ClvmTarget::TODAY, true);
    disassemble_program(&allocator, program)
}
