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

use id_arena::Arena;

use crate::{DebugTags, Lir, LirId, SpanScopes, stack::grow_stack};

/// How the optimizer may change the program.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct OptimizeOptions {
    /// Whether the program is run with `LIMITS` (see [`clvm_flags`](crate::clvm_flags)), so that
    /// the optimizer doesn't remove or add failures caused by the length of an operand. Without
    /// it, the optimizer assumes operands of any length are allowed, and may fold, flatten or
    /// rewrite operators in ways that remove those failures, or add them (such as by folding
    /// short constants into one that's too long).
    pub operand_limits: bool,
}

pub fn optimize(arena: &mut Arena<Lir>, lir: LirId, options: OptimizeOptions) -> LirId {
    optimize_node(arena, lir, options, &mut None)
}

/// Optimizes the LIR like [`optimize`], and carries its tags over to the optimized LIR. A node
/// that the optimizer generates is tagged with the span of the innermost tagged node that it was
/// generated from, and the node that a function's body is optimized to is tagged with the function.
pub fn optimize_with_tags(
    arena: &mut Arena<Lir>,
    lir: LirId,
    options: OptimizeOptions,
    tags: &mut DebugTags,
) -> LirId {
    let start = arena.len();
    let mut trace = Trace {
        tags,
        scopes: SpanScopes::default(),
    };
    let result = optimize_node(arena, lir, options, &mut Some(&mut trace));
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
    options: OptimizeOptions,
    trace: &mut Option<&mut Trace<'_>>,
) -> LirId {
    let Some(trace_ref) = trace else {
        return grow_stack(|| optimize_step(arena, lir, options, trace));
    };

    let span = trace_ref.tags.spans.get(&lir).copied();

    if let Some(span) = span {
        trace_ref.scopes.enter(arena.len(), span);
    }

    let result = grow_stack(|| optimize_step(arena, lir, options, trace));

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
    options: OptimizeOptions,
    trace: &mut Option<&mut Trace<'_>>,
) -> LirId {
    match arena[lir].clone() {
        Lir::Atom(atom) => opt_atom(arena, atom),
        Lir::Path(path) => opt_path(arena, path),
        Lir::Quote(value) => {
            let value = optimize_node(arena, value, options, trace);
            opt_quote(arena, value)
        }
        Lir::Run(callee, env) => {
            let callee = optimize_node(arena, callee, options, trace);
            let env = optimize_node(arena, env, options, trace);
            opt_run(arena, callee, env)
        }
        Lir::Closure(callee, args, has_parameters) => {
            let callee = optimize_node(arena, callee, options, trace);
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_closure(arena, callee, args, has_parameters)
        }
        Lir::First(value) => {
            let value = optimize_node(arena, value, options, trace);
            opt_first(arena, options, value)
        }
        Lir::Rest(value) => {
            let value = optimize_node(arena, value, options, trace);
            opt_rest(arena, options, value)
        }
        Lir::Cons(first, rest) => {
            let first = optimize_node(arena, first, options, trace);
            let rest = optimize_node(arena, rest, options, trace);
            opt_cons(arena, first, rest)
        }
        Lir::Listp(value) => {
            let value = optimize_node(arena, value, options, trace);
            opt_listp(arena, value)
        }
        Lir::Add(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_add(arena, args)
        }
        Lir::Sub(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_sub(arena, args)
        }
        Lir::Mul(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_mul(arena, options, args)
        }
        Lir::Div(left, right) => {
            let left = optimize_node(arena, left, options, trace);
            let right = optimize_node(arena, right, options, trace);
            opt_div(arena, options, left, right)
        }
        Lir::Divmod(left, right) => {
            let left = optimize_node(arena, left, options, trace);
            let right = optimize_node(arena, right, options, trace);
            opt_divmod(arena, options, left, right)
        }
        Lir::Mod(left, right) => {
            let left = optimize_node(arena, left, options, trace);
            let right = optimize_node(arena, right, options, trace);
            opt_mod(arena, options, left, right)
        }
        Lir::Modpow(base, exponent, modulus) => {
            let base = optimize_node(arena, base, options, trace);
            let exponent = optimize_node(arena, exponent, options, trace);
            let modulus = optimize_node(arena, modulus, options, trace);
            opt_modpow(arena, options, base, exponent, modulus)
        }
        Lir::Eq(left, right) => {
            let left = optimize_node(arena, left, options, trace);
            let right = optimize_node(arena, right, options, trace);
            opt_eq(arena, options, left, right)
        }
        Lir::Gt(left, right) => {
            let left = optimize_node(arena, left, options, trace);
            let right = optimize_node(arena, right, options, trace);
            opt_gt(arena, left, right)
        }
        Lir::GtBytes(left, right) => {
            let left = optimize_node(arena, left, options, trace);
            let right = optimize_node(arena, right, options, trace);
            opt_gtbytes(arena, left, right)
        }
        Lir::Not(value) => {
            let value = optimize_node(arena, value, options, trace);
            opt_not(arena, value)
        }
        Lir::All(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_all(arena, args)
        }
        Lir::Any(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_any(arena, args)
        }
        Lir::If(condition, then, otherwise, inline) => {
            let condition = optimize_node(arena, condition, options, trace);
            let then = optimize_node(arena, then, options, trace);
            let otherwise = optimize_node(arena, otherwise, options, trace);
            opt_if(arena, condition, then, otherwise, inline)
        }
        Lir::Raise(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_raise(arena, args)
        }
        Lir::Concat(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_concat(arena, args)
        }
        Lir::Strlen(value) => {
            let value = optimize_node(arena, value, options, trace);
            opt_strlen(arena, value)
        }
        Lir::Substr(string, start, end) => {
            let string = optimize_node(arena, string, options, trace);
            let start = optimize_node(arena, start, options, trace);
            let end = end.map(|end| optimize_node(arena, end, options, trace));
            opt_substr(arena, string, start, end)
        }
        Lir::Logand(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_logand(arena, args)
        }
        Lir::Logior(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_logior(arena, args)
        }
        Lir::Logxor(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_logxor(arena, args)
        }
        Lir::Lognot(value) => {
            let value = optimize_node(arena, value, options, trace);
            opt_lognot(arena, value)
        }
        Lir::Ash(value, shift) => {
            let value = optimize_node(arena, value, options, trace);
            let shift = optimize_node(arena, shift, options, trace);
            opt_ash(arena, value, shift)
        }
        Lir::Lsh(value, shift) => {
            let value = optimize_node(arena, value, options, trace);
            let shift = optimize_node(arena, shift, options, trace);
            opt_lsh(arena, value, shift)
        }
        Lir::PubkeyForExp(exp) => {
            let exp = optimize_node(arena, exp, options, trace);
            opt_pubkey_for_exp(arena, exp)
        }
        Lir::G1Add(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_g1_add(arena, args)
        }
        Lir::G1Subtract(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_g1_subtract(arena, args)
        }
        Lir::G1Multiply(left, right) => {
            let left = optimize_node(arena, left, options, trace);
            let right = optimize_node(arena, right, options, trace);
            opt_g1_multiply(arena, options, left, right)
        }
        Lir::G1Negate(value) => {
            let value = optimize_node(arena, value, options, trace);
            opt_g1_negate(arena, value)
        }
        Lir::G1Map(value, map) => {
            let value = optimize_node(arena, value, options, trace);
            let map = map.map(|map| optimize_node(arena, map, options, trace));
            opt_g1_map(arena, value, map)
        }
        Lir::G2Add(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_g2_add(arena, args)
        }
        Lir::G2Subtract(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_g2_subtract(arena, args)
        }
        Lir::G2Multiply(left, right) => {
            let left = optimize_node(arena, left, options, trace);
            let right = optimize_node(arena, right, options, trace);
            opt_g2_multiply(arena, options, left, right)
        }
        Lir::G2Negate(value) => {
            let value = optimize_node(arena, value, options, trace);
            opt_g2_negate(arena, value)
        }
        Lir::G2Map(value, map) => {
            let value = optimize_node(arena, value, options, trace);
            let map = map.map(|map| optimize_node(arena, map, options, trace));
            opt_g2_map(arena, value, map)
        }
        Lir::BlsPairingIdentity(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_bls_pairing_identity(arena, args)
        }
        Lir::BlsVerify(sig, args) => {
            let sig = optimize_node(arena, sig, options, trace);
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_bls_verify(arena, sig, args)
        }
        Lir::Sha256(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_sha256(arena, args, false)
        }
        Lir::Sha256Inline(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_sha256(arena, args, true)
        }
        Lir::Keccak256(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_keccak256(arena, args, false)
        }
        Lir::Keccak256Inline(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, options, trace))
                .collect();
            opt_keccak256(arena, args, true)
        }
        Lir::CoinId(parent_coin_info, puzzle_hash, amount) => {
            let parent_coin_info = optimize_node(arena, parent_coin_info, options, trace);
            let puzzle_hash = optimize_node(arena, puzzle_hash, options, trace);
            let amount = optimize_node(arena, amount, options, trace);
            opt_coin_id(arena, parent_coin_info, puzzle_hash, amount)
        }
        Lir::K1Verify(public_key, message, signature) => {
            let public_key = optimize_node(arena, public_key, options, trace);
            let message = optimize_node(arena, message, options, trace);
            let signature = optimize_node(arena, signature, options, trace);
            opt_k1_verify(arena, public_key, message, signature)
        }
        Lir::R1Verify(public_key, message, signature) => {
            let public_key = optimize_node(arena, public_key, options, trace);
            let message = optimize_node(arena, message, options, trace);
            let signature = optimize_node(arena, signature, options, trace);
            opt_r1_verify(arena, public_key, message, signature)
        }
        Lir::Op(op, arg) => {
            let arg = optimize_node(arena, arg, options, trace);
            opt_op(arena, op, arg)
        }
        Lir::DebugPrint(srcloc, value) => {
            let value = optimize_node(arena, value, options, trace);
            opt_debug_print(arena, srcloc, value)
        }
    }
}
