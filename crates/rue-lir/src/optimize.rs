#![allow(clippy::wildcard_imports)]

mod arg_list;
mod ops;
mod truthy;

use arg_list::*;
use ops::*;
use truthy::*;

use id_arena::Arena;

use crate::{Lir, LirId};

pub fn optimize(arena: &mut Arena<Lir>, lir: LirId) -> LirId {
    match arena[lir].clone() {
        Lir::Atom(atom) => opt_atom(arena, atom),
        Lir::Path(path) => opt_path(arena, path),
        Lir::Quote(value) => {
            let value = optimize(arena, value);
            opt_quote(arena, value)
        }
        Lir::Run(callee, env) => {
            let callee = optimize(arena, callee);
            let env = optimize(arena, env);
            opt_run(arena, callee, env)
        }
        Lir::Closure(callee, args, has_parameters) => {
            let callee = optimize(arena, callee);
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_closure(arena, callee, args, has_parameters)
        }
        Lir::First(value) => {
            let value = optimize(arena, value);
            opt_first(arena, value)
        }
        Lir::Rest(value) => {
            let value = optimize(arena, value);
            opt_rest(arena, value)
        }
        Lir::Cons(first, rest) => {
            let first = optimize(arena, first);
            let rest = optimize(arena, rest);
            opt_cons(arena, first, rest)
        }
        Lir::Listp(value, can_be_truthy) => {
            let value = optimize(arena, value);
            opt_listp(arena, value, can_be_truthy)
        }
        Lir::Add(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_add(arena, args)
        }
        Lir::Sub(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_sub(arena, args)
        }
        Lir::Mul(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_mul(arena, args)
        }
        Lir::Div(left, right) => {
            let left = optimize(arena, left);
            let right = optimize(arena, right);
            opt_div(arena, left, right)
        }
        Lir::Divmod(left, right) => {
            let left = optimize(arena, left);
            let right = optimize(arena, right);
            opt_divmod(arena, left, right)
        }
        Lir::Mod(left, right) => {
            let left = optimize(arena, left);
            let right = optimize(arena, right);
            opt_mod(arena, left, right)
        }
        Lir::Modpow(base, exponent, modulus) => {
            let base = optimize(arena, base);
            let exponent = optimize(arena, exponent);
            let modulus = optimize(arena, modulus);
            opt_modpow(arena, base, exponent, modulus)
        }
        Lir::Eq(left, right) => {
            let left = optimize(arena, left);
            let right = optimize(arena, right);
            opt_eq(arena, left, right)
        }
        Lir::Gt(left, right) => {
            let left = optimize(arena, left);
            let right = optimize(arena, right);
            opt_gt(arena, left, right)
        }
        Lir::GtBytes(left, right) => {
            let left = optimize(arena, left);
            let right = optimize(arena, right);
            opt_gtbytes(arena, left, right)
        }
        Lir::Not(value) => {
            let value = optimize(arena, value);
            opt_not(arena, value)
        }
        Lir::All(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_all(arena, args)
        }
        Lir::Any(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_any(arena, args)
        }
        Lir::If(condition, then, otherwise, inline) => {
            let condition = optimize(arena, condition);
            let then = optimize(arena, then);
            let otherwise = optimize(arena, otherwise);
            opt_if(arena, condition, then, otherwise, inline)
        }
        Lir::Raise(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_raise(arena, args)
        }
        Lir::Concat(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_concat(arena, args)
        }
        Lir::Strlen(value) => {
            let value = optimize(arena, value);
            opt_strlen(arena, value)
        }
        Lir::Substr(string, start, end) => {
            let string = optimize(arena, string);
            let start = optimize(arena, start);
            let end = end.map(|end| optimize(arena, end));
            opt_substr(arena, string, start, end)
        }
        Lir::Logand(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_logand(arena, args)
        }
        Lir::Logior(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_logior(arena, args)
        }
        Lir::Logxor(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_logxor(arena, args)
        }
        Lir::Lognot(value) => {
            let value = optimize(arena, value);
            opt_lognot(arena, value)
        }
        Lir::Ash(value, shift) => {
            let value = optimize(arena, value);
            let shift = optimize(arena, shift);
            opt_ash(arena, value, shift)
        }
        Lir::Lsh(value, shift) => {
            let value = optimize(arena, value);
            let shift = optimize(arena, shift);
            opt_lsh(arena, value, shift)
        }
        Lir::PubkeyForExp(exp) => {
            let exp = optimize(arena, exp);
            opt_pubkey_for_exp(arena, exp)
        }
        Lir::G1Add(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_g1_add(arena, args)
        }
        Lir::G1Subtract(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_g1_subtract(arena, args)
        }
        Lir::G1Multiply(left, right) => {
            let left = optimize(arena, left);
            let right = optimize(arena, right);
            opt_g1_multiply(arena, left, right)
        }
        Lir::G1Negate(value) => {
            let value = optimize(arena, value);
            opt_g1_negate(arena, value)
        }
        Lir::G1Map(value, map) => {
            let value = optimize(arena, value);
            let map = map.map(|map| optimize(arena, map));
            opt_g1_map(arena, value, map)
        }
        Lir::G2Add(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_g2_add(arena, args)
        }
        Lir::G2Subtract(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_g2_subtract(arena, args)
        }
        Lir::G2Multiply(left, right) => {
            let left = optimize(arena, left);
            let right = optimize(arena, right);
            opt_g2_multiply(arena, left, right)
        }
        Lir::G2Negate(value) => {
            let value = optimize(arena, value);
            opt_g2_negate(arena, value)
        }
        Lir::G2Map(value, map) => {
            let value = optimize(arena, value);
            let map = map.map(|map| optimize(arena, map));
            opt_g2_map(arena, value, map)
        }
        Lir::BlsPairingIdentity(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_bls_pairing_identity(arena, args)
        }
        Lir::BlsVerify(sig, args) => {
            let sig = optimize(arena, sig);
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_bls_verify(arena, sig, args)
        }
        Lir::Sha256(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_sha256(arena, args, false)
        }
        Lir::Sha256Inline(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_sha256(arena, args, true)
        }
        Lir::Keccak256(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_keccak256(arena, args, false)
        }
        Lir::Keccak256Inline(args) => {
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_keccak256(arena, args, true)
        }
        Lir::CoinId(parent_coin_info, puzzle_hash, amount) => {
            let parent_coin_info = optimize(arena, parent_coin_info);
            let puzzle_hash = optimize(arena, puzzle_hash);
            let amount = optimize(arena, amount);
            opt_coin_id(arena, parent_coin_info, puzzle_hash, amount)
        }
        Lir::K1Verify(public_key, message, signature) => {
            let public_key = optimize(arena, public_key);
            let message = optimize(arena, message);
            let signature = optimize(arena, signature);
            opt_k1_verify(arena, public_key, message, signature)
        }
        Lir::R1Verify(public_key, message, signature) => {
            let public_key = optimize(arena, public_key);
            let message = optimize(arena, message);
            let signature = optimize(arena, signature);
            opt_r1_verify(arena, public_key, message, signature)
        }
        Lir::Op(op, arg) => {
            let arg = optimize(arena, arg);
            opt_op(arena, op, arg)
        }
    }
}
