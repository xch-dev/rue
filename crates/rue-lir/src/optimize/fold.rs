use clvmr::{Allocator, ChiaDialect, NodePtr, SExp, run_program, serde::node_to_bytes};
use id_arena::Arena;

use crate::{CLVM_FLAGS, ClvmOp, Lir, LirId};

// The maximum cost spent evaluating a single operator at compile time
const MAX_FOLD_COST: u64 = 100_000_000;

// The cost of each byte in the serialized program
const COST_PER_BYTE: u64 = 12_000;

// The cost of quoting the folded value
const QUOTE_COST: u64 = 20;

// If every argument is an atom, we can run the operator at compile time to get the result
// This uses the same dialect as the runtime, so the result is always exactly the same
// If the operator would fail, we leave it alone so that it still fails at runtime
// We also only fold if it doesn't increase the total cost (for example, by producing a large atom)
pub fn fold_atoms(arena: &mut Arena<Lir>, op: ClvmOp, args: &[LirId]) -> Option<LirId> {
    let mut atoms = Vec::with_capacity(args.len());

    for &arg in args {
        let Lir::Atom(atom) = &arena[arg] else {
            return None;
        };

        atoms.push(atom.clone());
    }

    let result = run_op(op, &atoms)?;

    Some(arena.alloc(Lir::Atom(result)))
}

fn run_op(op: ClvmOp, atoms: &[Vec<u8>]) -> Option<Vec<u8>> {
    let mut allocator = Allocator::new();

    let quote = allocator.one();
    let mut args = NodePtr::NIL;

    for atom in atoms.iter().rev() {
        let atom = allocator.new_atom(atom).ok()?;
        let quoted = allocator.new_pair(quote, atom).ok()?;
        args = allocator.new_pair(quoted, args).ok()?;
    }

    let op = allocator.new_atom(&op.to_atom()).ok()?;
    let program = allocator.new_pair(op, args).ok()?;

    let dialect = ChiaDialect::new(CLVM_FLAGS);
    let reduction = run_program(
        &mut allocator,
        &dialect,
        program,
        NodePtr::NIL,
        MAX_FOLD_COST,
    )
    .ok()?;

    // Every operator we fold returns an atom, but we check just in case
    let SExp::Atom = allocator.sexp(reduction.1) else {
        return None;
    };

    let output = allocator.atom(reduction.1).to_vec();

    let folded = allocator.new_atom(&output).ok()?;
    let folded = allocator.new_pair(quote, folded).ok()?;

    let original_cost = reduction.0 + serialized_cost(&allocator, program)?;
    let folded_cost = QUOTE_COST + serialized_cost(&allocator, folded)?;

    (folded_cost <= original_cost).then_some(output)
}

fn serialized_cost(allocator: &Allocator, node: NodePtr) -> Option<u64> {
    let length = node_to_bytes(allocator, node).ok()?.len() as u64;
    Some(length * COST_PER_BYTE)
}
