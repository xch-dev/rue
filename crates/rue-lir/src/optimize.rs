mod arg_list;
mod env;
mod fold;
mod ops;
mod truthy;

#[cfg(test)]
mod tests;

use arg_list::*;
use env::*;
use fold::*;
use ops::*;
use truthy::*;

pub use truthy::is_pure;

use id_arena::Arena;

use crate::{ClvmTarget, DebugTags, Lir, LirId, SpanScopes, stack::grow_stack};

pub fn optimize(arena: &mut Arena<Lir>, lir: LirId, target: ClvmTarget) -> LirId {
    optimize_node(arena, lir, target, &mut None)
}

/// Optimizes the LIR like [`optimize`], and carries its tags over to the optimized LIR. A node
/// that the optimizer generates is tagged with the span of the innermost tagged node that it was
/// generated from, and the node that a function's body is optimized to is tagged with the function.
pub fn optimize_with_tags(
    arena: &mut Arena<Lir>,
    lir: LirId,
    target: ClvmTarget,
    tags: &mut DebugTags,
) -> LirId {
    let start = arena.len();
    let mut trace = Trace {
        tags,
        scopes: SpanScopes::default(),
    };
    let result = optimize_node(arena, lir, target, &mut Some(&mut trace));
    trace.scopes.apply(arena, start, &mut trace.tags.spans);
    result
}

/// What the optimizer keeps track of to carry tags over to the nodes it generates.
struct Trace<'a> {
    tags: &'a mut DebugTags,
    scopes: SpanScopes,
}

fn optimize_node(
    arena: &mut Arena<Lir>,
    lir: LirId,
    target: ClvmTarget,
    trace: &mut Option<&mut Trace<'_>>,
) -> LirId {
    let Some(trace_ref) = trace else {
        return grow_stack(|| optimize_step(arena, lir, target, trace));
    };

    let span = trace_ref.tags.spans.get(&lir).copied();

    if let Some(span) = span {
        trace_ref.scopes.enter(arena.len(), span);
    }

    let result = grow_stack(|| optimize_step(arena, lir, target, trace));

    let trace = trace.as_mut().expect("the trace is kept while optimizing");

    if span.is_some() {
        trace.scopes.exit(arena.len());
    }

    if let Some(&function) = trace.tags.functions.get(&lir) {
        trace.tags.functions.entry(result).or_insert(function);
    }

    if let Some(&label) = trace.tags.labels.get(&lir) {
        trace.tags.labels.entry(result).or_insert(label);
    }

    result
}

fn optimize_step(
    arena: &mut Arena<Lir>,
    lir: LirId,
    target: ClvmTarget,
    trace: &mut Option<&mut Trace<'_>>,
) -> LirId {
    let node = arena[lir]
        .clone()
        .map_children(|child| optimize_node(arena, child, target, trace));

    match node {
        Lir::Atom(atom) => opt_atom(arena, atom),
        Lir::Path(path) => opt_path(arena, path),
        Lir::Quote(value) => opt_quote(arena, value),
        Lir::Run(callee, env) => opt_run(arena, callee, env),
        Lir::Closure(callee, args, has_parameters) => {
            opt_closure(arena, callee, args, has_parameters)
        }
        Lir::First(value) => opt_first(arena, target, value),
        Lir::Rest(value) => opt_rest(arena, target, value),
        Lir::Cons(first, rest) => opt_cons(arena, first, rest),
        Lir::Listp(value) => opt_listp(arena, value),
        Lir::Add(args) => opt_add(arena, args),
        Lir::Sub(args) => opt_sub(arena, args),
        Lir::Mul(args) => opt_mul(arena, target, args),
        Lir::Div(left, right) => opt_div(arena, target, left, right),
        Lir::Divmod(left, right) => opt_divmod(arena, target, left, right),
        Lir::Mod(left, right) => opt_mod(arena, target, left, right),
        Lir::Eq(left, right) => opt_eq(arena, target, left, right),
        Lir::Not(value) => opt_not(arena, value),
        Lir::All(args) => opt_all(arena, args),
        Lir::Any(args) => opt_any(arena, args),
        Lir::If(condition, then, otherwise, inline) => {
            opt_if(arena, target, condition, then, otherwise, inline)
        }
        Lir::Raise(args) => opt_raise(arena, args),
        Lir::Concat(args) => opt_concat(arena, args),
        Lir::Strlen(value) => opt_strlen(arena, value),
        Lir::Logand(args) => opt_bitwise(arena, Bitwise::And, args),
        Lir::Logior(args) => opt_bitwise(arena, Bitwise::Or, args),
        Lir::Logxor(args) => opt_bitwise(arena, Bitwise::Xor, args),
        Lir::Sha256(args) => opt_sha256(arena, args, false),
        Lir::Sha256Inline(args) => opt_sha256(arena, args, true),
        Lir::Keccak256(args) => opt_keccak256(arena, args, false),
        Lir::Keccak256Inline(args) => opt_keccak256(arena, args, true),
        Lir::Keccak256Verify(value, hash) => opt_keccak256_verify(arena, value, hash),
        Lir::Op(op, arg) => opt_op(arena, op, arg),
        Lir::DebugPrint(srcloc, value) => opt_debug_print(arena, srcloc, value),
        // These are only run at compile time if their operands are constants
        node @ (Lir::Modpow(..)
        | Lir::Gt(..)
        | Lir::GtBytes(..)
        | Lir::Substr(..)
        | Lir::Lognot(_)
        | Lir::Ash(..)
        | Lir::Lsh(..)
        | Lir::PubkeyForExp(_)
        | Lir::G1Add(_)
        | Lir::G1Subtract(_)
        | Lir::G1Multiply(..)
        | Lir::G1Negate(_)
        | Lir::G1Map(..)
        | Lir::G2Add(_)
        | Lir::G2Subtract(_)
        | Lir::G2Multiply(..)
        | Lir::G2Negate(_)
        | Lir::G2Map(..)
        | Lir::BlsPairingIdentity(_)
        | Lir::BlsVerify(..)
        | Lir::CoinId(..)
        | Lir::K1Verify(..)
        | Lir::R1Verify(..)) => opt_fold(arena, target, node),
    }
}
