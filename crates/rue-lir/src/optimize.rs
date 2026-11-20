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
    match arena[lir].clone() {
        Lir::Atom(atom) => opt_atom(arena, atom),
        Lir::Path(path) => opt_path(arena, path),
        Lir::Quote(value) => {
            let value = optimize_node(arena, value, target, trace);
            opt_quote(arena, value)
        }
        Lir::Run(callee, env) => {
            let callee = optimize_node(arena, callee, target, trace);
            let env = optimize_node(arena, env, target, trace);
            opt_run(arena, callee, env)
        }
        Lir::Closure(callee, args, has_parameters) => {
            let callee = optimize_node(arena, callee, target, trace);
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_closure(arena, callee, args, has_parameters)
        }
        Lir::First(value) => {
            let value = optimize_node(arena, value, target, trace);
            opt_first(arena, target, value)
        }
        Lir::Rest(value) => {
            let value = optimize_node(arena, value, target, trace);
            opt_rest(arena, target, value)
        }
        Lir::Cons(first, rest) => {
            let first = optimize_node(arena, first, target, trace);
            let rest = optimize_node(arena, rest, target, trace);
            opt_cons(arena, first, rest)
        }
        Lir::Listp(value) => {
            let value = optimize_node(arena, value, target, trace);
            opt_listp(arena, value)
        }
        Lir::Add(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_add(arena, args)
        }
        Lir::Sub(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_sub(arena, args)
        }
        Lir::Mul(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_mul(arena, target, args)
        }
        Lir::Div(left, right) => {
            let left = optimize_node(arena, left, target, trace);
            let right = optimize_node(arena, right, target, trace);
            opt_div(arena, target, left, right)
        }
        Lir::Divmod(left, right) => {
            let left = optimize_node(arena, left, target, trace);
            let right = optimize_node(arena, right, target, trace);
            opt_divmod(arena, target, left, right)
        }
        Lir::Mod(left, right) => {
            let left = optimize_node(arena, left, target, trace);
            let right = optimize_node(arena, right, target, trace);
            opt_mod(arena, target, left, right)
        }
        Lir::Modpow(base, exponent, modulus) => {
            let base = optimize_node(arena, base, target, trace);
            let exponent = optimize_node(arena, exponent, target, trace);
            let modulus = optimize_node(arena, modulus, target, trace);
            opt_modpow(arena, target, base, exponent, modulus)
        }
        Lir::Eq(left, right) => {
            let left = optimize_node(arena, left, target, trace);
            let right = optimize_node(arena, right, target, trace);
            opt_eq(arena, target, left, right)
        }
        Lir::Gt(left, right) => {
            let left = optimize_node(arena, left, target, trace);
            let right = optimize_node(arena, right, target, trace);
            opt_gt(arena, target, left, right)
        }
        Lir::GtBytes(left, right) => {
            let left = optimize_node(arena, left, target, trace);
            let right = optimize_node(arena, right, target, trace);
            opt_gtbytes(arena, target, left, right)
        }
        Lir::Not(value) => {
            let value = optimize_node(arena, value, target, trace);
            opt_not(arena, value)
        }
        Lir::All(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_all(arena, args)
        }
        Lir::Any(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_any(arena, args)
        }
        Lir::If(condition, then, otherwise, inline) => {
            let condition = optimize_node(arena, condition, target, trace);
            let then = optimize_node(arena, then, target, trace);
            let otherwise = optimize_node(arena, otherwise, target, trace);
            opt_if(arena, condition, then, otherwise, inline)
        }
        Lir::Raise(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_raise(arena, args)
        }
        Lir::Concat(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_concat(arena, args)
        }
        Lir::Strlen(value) => {
            let value = optimize_node(arena, value, target, trace);
            opt_strlen(arena, value)
        }
        Lir::Substr(string, start, end) => {
            let string = optimize_node(arena, string, target, trace);
            let start = optimize_node(arena, start, target, trace);
            let end = end.map(|end| optimize_node(arena, end, target, trace));
            opt_substr(arena, target, string, start, end)
        }
        Lir::Logand(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_logand(arena, args)
        }
        Lir::Logior(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_logior(arena, args)
        }
        Lir::Logxor(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_logxor(arena, args)
        }
        Lir::Lognot(value) => {
            let value = optimize_node(arena, value, target, trace);
            opt_lognot(arena, target, value)
        }
        Lir::Ash(value, shift) => {
            let value = optimize_node(arena, value, target, trace);
            let shift = optimize_node(arena, shift, target, trace);
            opt_ash(arena, target, value, shift)
        }
        Lir::Lsh(value, shift) => {
            let value = optimize_node(arena, value, target, trace);
            let shift = optimize_node(arena, shift, target, trace);
            opt_lsh(arena, target, value, shift)
        }
        Lir::PubkeyForExp(exp) => {
            let exp = optimize_node(arena, exp, target, trace);
            opt_pubkey_for_exp(arena, target, exp)
        }
        Lir::G1Add(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_g1_add(arena, target, args)
        }
        Lir::G1Subtract(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_g1_subtract(arena, target, args)
        }
        Lir::G1Multiply(left, right) => {
            let left = optimize_node(arena, left, target, trace);
            let right = optimize_node(arena, right, target, trace);
            opt_g1_multiply(arena, target, left, right)
        }
        Lir::G1Negate(value) => {
            let value = optimize_node(arena, value, target, trace);
            opt_g1_negate(arena, target, value)
        }
        Lir::G1Map(value, map) => {
            let value = optimize_node(arena, value, target, trace);
            let map = map.map(|map| optimize_node(arena, map, target, trace));
            opt_g1_map(arena, target, value, map)
        }
        Lir::G2Add(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_g2_add(arena, target, args)
        }
        Lir::G2Subtract(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_g2_subtract(arena, target, args)
        }
        Lir::G2Multiply(left, right) => {
            let left = optimize_node(arena, left, target, trace);
            let right = optimize_node(arena, right, target, trace);
            opt_g2_multiply(arena, target, left, right)
        }
        Lir::G2Negate(value) => {
            let value = optimize_node(arena, value, target, trace);
            opt_g2_negate(arena, target, value)
        }
        Lir::G2Map(value, map) => {
            let value = optimize_node(arena, value, target, trace);
            let map = map.map(|map| optimize_node(arena, map, target, trace));
            opt_g2_map(arena, target, value, map)
        }
        Lir::BlsPairingIdentity(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_bls_pairing_identity(arena, target, args)
        }
        Lir::BlsVerify(sig, args) => {
            let sig = optimize_node(arena, sig, target, trace);
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_bls_verify(arena, target, sig, args)
        }
        Lir::Sha256(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_sha256(arena, args, false)
        }
        Lir::Sha256Inline(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_sha256(arena, args, true)
        }
        Lir::Keccak256(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_keccak256(arena, args, false)
        }
        Lir::Keccak256Inline(args) => {
            let args = args
                .iter()
                .map(|arg| optimize_node(arena, *arg, target, trace))
                .collect();
            opt_keccak256(arena, args, true)
        }
        Lir::CoinId(parent_coin_info, puzzle_hash, amount) => {
            let parent_coin_info = optimize_node(arena, parent_coin_info, target, trace);
            let puzzle_hash = optimize_node(arena, puzzle_hash, target, trace);
            let amount = optimize_node(arena, amount, target, trace);
            opt_coin_id(arena, target, parent_coin_info, puzzle_hash, amount)
        }
        Lir::K1Verify(public_key, message, signature) => {
            let public_key = optimize_node(arena, public_key, target, trace);
            let message = optimize_node(arena, message, target, trace);
            let signature = optimize_node(arena, signature, target, trace);
            opt_k1_verify(arena, target, public_key, message, signature)
        }
        Lir::R1Verify(public_key, message, signature) => {
            let public_key = optimize_node(arena, public_key, target, trace);
            let message = optimize_node(arena, message, target, trace);
            let signature = optimize_node(arena, signature, target, trace);
            opt_r1_verify(arena, target, public_key, message, signature)
        }
        Lir::Op(op, arg) => {
            let arg = optimize_node(arena, arg, target, trace);
            opt_op(arena, op, arg)
        }
        Lir::DebugPrint(srcloc, value) => {
            let value = optimize_node(arena, value, target, trace);
            opt_debug_print(arena, srcloc, value)
        }
    }
}
