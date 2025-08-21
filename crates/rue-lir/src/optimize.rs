use id_arena::Arena;

use crate::{Lir, LirId, first_path, rest_path};

pub fn optimize(arena: &mut Arena<Lir>, lir: LirId) -> LirId {
    match arena[lir].clone() {
        Lir::Atom(atom) => arena.alloc(Lir::Atom(atom)),
        Lir::Path(path) => arena.alloc(Lir::Path(path)),
        Lir::Quote(value) => {
            let value = optimize(arena, value);
            arena.alloc(Lir::Quote(value))
        }
        Lir::Run(callee, args) => {
            let callee = optimize(arena, callee);
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::Run(callee, args))
        }
        Lir::Curry(callee, args) => {
            let callee = optimize(arena, callee);
            if let Lir::Quote(value) = arena[callee].clone()
                && args.is_empty()
            {
                return value;
            }
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::Curry(callee, args))
        }
        Lir::Closure(callee, args) => {
            let callee = optimize(arena, callee);
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::Closure(callee, args))
        }
        Lir::First(value) => {
            let value = optimize(arena, value);
            if let Lir::Path(path) = arena[value].clone() {
                return arena.alloc(Lir::Path(first_path(path)));
            }
            arena.alloc(Lir::First(value))
        }
        Lir::Rest(value) => {
            let value = optimize(arena, value);
            if let Lir::Path(path) = arena[value].clone() {
                return arena.alloc(Lir::Path(rest_path(path)));
            }
            arena.alloc(Lir::Rest(value))
        }
        Lir::Cons(first, rest) => {
            let first = optimize(arena, first);
            let rest = optimize(arena, rest);
            arena.alloc(Lir::Cons(first, rest))
        }
        Lir::Listp(value) => {
            let value = optimize(arena, value);
            arena.alloc(Lir::Listp(value))
        }
        Lir::Add(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::Add(args))
        }
        Lir::Sub(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::Sub(args))
        }
        Lir::Mul(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::Mul(args))
        }
        Lir::Div(left, right) => {
            let left = optimize(arena, left);
            let right = optimize(arena, right);
            arena.alloc(Lir::Div(left, right))
        }
        Lir::Divmod(left, right) => {
            let left = optimize(arena, left);
            let right = optimize(arena, right);
            arena.alloc(Lir::Divmod(left, right))
        }
        Lir::Mod(left, right) => {
            let left = optimize(arena, left);
            let right = optimize(arena, right);
            arena.alloc(Lir::Mod(left, right))
        }
        Lir::Modpow(base, exponent, modulus) => {
            let base = optimize(arena, base);
            let exponent = optimize(arena, exponent);
            let modulus = optimize(arena, modulus);
            arena.alloc(Lir::Modpow(base, exponent, modulus))
        }
        Lir::Eq(left, right) => {
            let left = optimize(arena, left);
            let right = optimize(arena, right);
            arena.alloc(Lir::Eq(left, right))
        }
        Lir::Gt(left, right) => {
            let left = optimize(arena, left);
            let right = optimize(arena, right);
            arena.alloc(Lir::Gt(left, right))
        }
        Lir::GtBytes(left, right) => {
            let left = optimize(arena, left);
            let right = optimize(arena, right);
            arena.alloc(Lir::GtBytes(left, right))
        }
        Lir::Not(value) => {
            let value = optimize(arena, value);
            arena.alloc(Lir::Not(value))
        }
        Lir::All(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::All(args))
        }
        Lir::Any(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::Any(args))
        }
        Lir::If(condition, then, otherwise) => {
            let condition = optimize(arena, condition);
            let then = optimize(arena, then);
            let otherwise = optimize(arena, otherwise);
            arena.alloc(Lir::If(condition, then, otherwise))
        }
        Lir::Raise(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::Raise(args))
        }
        Lir::Concat(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::Concat(args))
        }
        Lir::Strlen(string) => {
            let string = optimize(arena, string);
            arena.alloc(Lir::Strlen(string))
        }
        Lir::Substr(string, start, end) => {
            let string = optimize(arena, string);
            let start = optimize(arena, start);
            let end = end.map(|end| optimize(arena, end));
            arena.alloc(Lir::Substr(string, start, end))
        }
        Lir::Logand(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::Logand(args))
        }
        Lir::Logior(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::Logior(args))
        }
        Lir::Logxor(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::Logxor(args))
        }
        Lir::Lognot(value) => {
            let value = optimize(arena, value);
            arena.alloc(Lir::Lognot(value))
        }
        Lir::Ash(value, shift) => {
            let value = optimize(arena, value);
            let shift = optimize(arena, shift);
            arena.alloc(Lir::Ash(value, shift))
        }
        Lir::Lsh(value, shift) => {
            let value = optimize(arena, value);
            let shift = optimize(arena, shift);
            arena.alloc(Lir::Lsh(value, shift))
        }
        Lir::PubkeyForExp(exp) => {
            let exp = optimize(arena, exp);
            arena.alloc(Lir::PubkeyForExp(exp))
        }
        Lir::G1Add(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::G1Add(args))
        }
        Lir::G1Subtract(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::G1Subtract(args))
        }
        Lir::G1Multiply(left, right) => {
            let left = optimize(arena, left);
            let right = optimize(arena, right);
            arena.alloc(Lir::G1Multiply(left, right))
        }
        Lir::G1Negate(value) => {
            let value = optimize(arena, value);
            arena.alloc(Lir::G1Negate(value))
        }
        Lir::G1Map(value, map) => {
            let value = optimize(arena, value);
            let map = map.map(|map| optimize(arena, map));
            arena.alloc(Lir::G1Map(value, map))
        }
        Lir::G2Add(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::G2Add(args))
        }
        Lir::G2Subtract(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::G2Subtract(args))
        }
        Lir::G2Multiply(left, right) => {
            let left = optimize(arena, left);
            let right = optimize(arena, right);
            arena.alloc(Lir::G2Multiply(left, right))
        }
        Lir::G2Negate(value) => {
            let value = optimize(arena, value);
            arena.alloc(Lir::G2Negate(value))
        }
        Lir::G2Map(value, map) => {
            let value = optimize(arena, value);
            let map = map.map(|map| optimize(arena, map));
            arena.alloc(Lir::G2Map(value, map))
        }
        Lir::BlsPairingIdentity(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::BlsPairingIdentity(args))
        }
        Lir::BlsVerify(sig, args) => {
            let sig = optimize(arena, sig);
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::BlsVerify(sig, args))
        }
        Lir::Sha256(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::Sha256(args))
        }
        Lir::Keccak256(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            arena.alloc(Lir::Keccak256(args))
        }
        Lir::CoinId(parent_coin_info, puzzle_hash, amount) => {
            let parent_coin_info = optimize(arena, parent_coin_info);
            let puzzle_hash = optimize(arena, puzzle_hash);
            let amount = optimize(arena, amount);
            arena.alloc(Lir::CoinId(parent_coin_info, puzzle_hash, amount))
        }
        Lir::K1Verify(public_key, message, signature) => {
            let public_key = optimize(arena, public_key);
            let message = optimize(arena, message);
            let signature = optimize(arena, signature);
            arena.alloc(Lir::K1Verify(public_key, message, signature))
        }
        Lir::R1Verify(public_key, message, signature) => {
            let public_key = optimize(arena, public_key);
            let message = optimize(arena, message);
            let signature = optimize(arena, signature);
            arena.alloc(Lir::R1Verify(public_key, message, signature))
        }
    }
}
