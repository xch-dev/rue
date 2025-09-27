use id_arena::Arena;

use crate::{Lir, LirId};

pub fn has_path_quotable(arena: &Arena<Lir>, value: LirId) -> bool {
    if let Lir::Quote(value) = arena[value].clone() {
        has_path(arena, value)
    } else {
        has_path(arena, value)
    }
}

// TODO: Is this safe in all cases?
pub fn has_path(arena: &Arena<Lir>, value: LirId) -> bool {
    match arena[value].clone() {
        Lir::Atom(_) => false,
        Lir::Path(_) => true,
        Lir::Quote(_) => false,
        Lir::Run(callee, env) => has_path_quotable(arena, callee) || has_path(arena, env),
        Lir::Curry(callee, args) => {
            has_path_quotable(arena, callee) || args.iter().any(|arg| has_path(arena, *arg))
        }
        Lir::Closure(callee, args) => {
            has_path_quotable(arena, callee) || args.iter().any(|arg| has_path(arena, *arg))
        }
        Lir::First(value) => has_path(arena, value),
        Lir::Rest(value) => has_path(arena, value),
        Lir::Cons(first, rest) => has_path(arena, first) || has_path(arena, rest),
        Lir::Listp(value, _) => has_path(arena, value),
        Lir::Add(args) => args.iter().any(|arg| has_path(arena, *arg)),
        Lir::Sub(args) => args.iter().any(|arg| has_path(arena, *arg)),
        Lir::Mul(args) => args.iter().any(|arg| has_path(arena, *arg)),
        Lir::Div(left, right) => has_path(arena, left) || has_path(arena, right),
        Lir::Divmod(left, right) => has_path(arena, left) || has_path(arena, right),
        Lir::Mod(left, right) => has_path(arena, left) || has_path(arena, right),
        Lir::Modpow(base, exponent, modulus) => {
            has_path(arena, base) || has_path(arena, exponent) || has_path(arena, modulus)
        }
        Lir::Eq(left, right) => has_path(arena, left) || has_path(arena, right),
        Lir::Gt(left, right) => has_path(arena, left) || has_path(arena, right),
        Lir::GtBytes(left, right) => has_path(arena, left) || has_path(arena, right),
        Lir::Not(value) => has_path(arena, value),
        Lir::All(args) => args.iter().any(|arg| has_path(arena, *arg)),
        Lir::Any(args) => args.iter().any(|arg| has_path(arena, *arg)),
        Lir::If(condition, then, otherwise) => {
            has_path(arena, condition) || has_path(arena, then) || has_path(arena, otherwise)
        }
        Lir::Raise(args) => args.iter().any(|arg| has_path(arena, *arg)),
        Lir::Concat(args) => args.iter().any(|arg| has_path(arena, *arg)),
        Lir::Strlen(value) => has_path(arena, value),
        Lir::Substr(string, start, end) => {
            has_path(arena, string)
                || has_path(arena, start)
                || end.is_some_and(|end| has_path(arena, end))
        }
        Lir::Logand(args) => args.iter().any(|arg| has_path(arena, *arg)),
        Lir::Logior(args) => args.iter().any(|arg| has_path(arena, *arg)),
        Lir::Logxor(args) => args.iter().any(|arg| has_path(arena, *arg)),
        Lir::Lognot(value) => has_path(arena, value),
        Lir::Ash(value, shift) => has_path(arena, value) || has_path(arena, shift),
        Lir::Lsh(value, shift) => has_path(arena, value) || has_path(arena, shift),
        Lir::PubkeyForExp(value) => has_path(arena, value),
        Lir::G1Add(args) => args.iter().any(|arg| has_path(arena, *arg)),
        Lir::G1Subtract(args) => args.iter().any(|arg| has_path(arena, *arg)),
        Lir::G1Multiply(left, right) => has_path(arena, left) || has_path(arena, right),
        Lir::G1Negate(value) => has_path(arena, value),
        Lir::G1Map(value, map) => {
            has_path(arena, value) || map.is_some_and(|map| has_path(arena, map))
        }
        Lir::G2Add(args) => args.iter().any(|arg| has_path(arena, *arg)),
        Lir::G2Subtract(args) => args.iter().any(|arg| has_path(arena, *arg)),
        Lir::G2Multiply(left, right) => has_path(arena, left) || has_path(arena, right),
        Lir::G2Negate(value) => has_path(arena, value),
        Lir::G2Map(value, map) => {
            has_path(arena, value) || map.is_some_and(|map| has_path(arena, map))
        }
        Lir::BlsPairingIdentity(args) => args.iter().any(|arg| has_path(arena, *arg)),
        Lir::BlsVerify(sig, args) => {
            has_path(arena, sig) || args.iter().any(|arg| has_path(arena, *arg))
        }
        Lir::Sha256(args) => args.iter().any(|arg| has_path(arena, *arg)),
        Lir::Sha256Inline(args) => args.iter().any(|arg| has_path(arena, *arg)),
        Lir::Keccak256(args) => args.iter().any(|arg| has_path(arena, *arg)),
        Lir::CoinId(parent_coin_info, puzzle_hash, amount) => {
            has_path(arena, parent_coin_info)
                || has_path(arena, puzzle_hash)
                || has_path(arena, amount)
        }
        Lir::K1Verify(public_key, message, signature) => {
            has_path(arena, public_key) || has_path(arena, message) || has_path(arena, signature)
        }
        Lir::R1Verify(public_key, message, signature) => {
            has_path(arena, public_key) || has_path(arena, message) || has_path(arena, signature)
        }
    }
}
