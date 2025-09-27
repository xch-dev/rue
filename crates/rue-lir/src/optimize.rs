use std::collections::VecDeque;

use clvmr::Allocator;
use id_arena::Arena;
use sha2::{Digest, Sha256};

use crate::{Lir, LirId, bigint_atom, first_path, rest_path};

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
        Lir::Curry(callee, args) => {
            let callee = optimize(arena, callee);
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_curry(arena, callee, args)
        }
        Lir::Closure(callee, args) => {
            let callee = optimize(arena, callee);
            let args = args.iter().map(|arg| optimize(arena, *arg)).collect();
            opt_closure(arena, callee, args)
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
        Lir::If(condition, then, otherwise) => {
            let condition = optimize(arena, condition);
            let then = optimize(arena, then);
            let otherwise = optimize(arena, otherwise);
            opt_if(arena, condition, then, otherwise)
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
            opt_keccak256(arena, args)
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
    }
}

// There's no way to optimize an atom
fn opt_atom(arena: &mut Arena<Lir>, atom: Vec<u8>) -> LirId {
    arena.alloc(Lir::Atom(atom))
}

// There's no way to optimize a path
fn opt_path(arena: &mut Arena<Lir>, path: u32) -> LirId {
    arena.alloc(Lir::Path(path))
}

// There's no way to optimize a quoted value, since nil is optimized by codegen already
fn opt_quote(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    arena.alloc(Lir::Quote(value))
}

// If the program is quoted, and the environment is the same as the parent,
// we can skip both quoting and running the program, and just use it directly
// We can also skip quoting if the program has no path, since it's not going to rely on the environment
fn opt_run(arena: &mut Arena<Lir>, callee: LirId, env: LirId) -> LirId {
    if let Lir::Quote(value) = arena[callee].clone()
        && let Lir::Path(path) = arena[env].clone()
        && (path == 1 || !has_path(arena, value))
    {
        return value;
    }

    arena.alloc(Lir::Run(callee, env))
}

// If the callee is quoted, and the arguments are empty,
// we can skip both quoting and currying, and just use it directly
// We can also skip quoting if the program has no path, since it's not going to rely on the environment
fn opt_curry(arena: &mut Arena<Lir>, callee: LirId, args: Vec<LirId>) -> LirId {
    if let Lir::Quote(value) = arena[callee].clone()
        && (args.is_empty() || !has_path(arena, value))
    {
        return value;
    }

    arena.alloc(Lir::Curry(callee, args))
}

// If there are no captures, we don't need to create a closure
// We can also skip the closure if the program has no path, since it's not going to rely on the captures
fn opt_closure(arena: &mut Arena<Lir>, callee: LirId, args: Vec<LirId>) -> LirId {
    if args.is_empty() || !has_path_quotable(arena, callee) {
        return callee;
    }

    arena.alloc(Lir::Closure(callee, args))
}

// If the value is a path, we can optimize it to a first path
// If the value is a cons, we can extract the first element out of it
fn opt_first(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    match arena[value].clone() {
        Lir::Path(path) => arena.alloc(Lir::Path(first_path(path))),
        Lir::Cons(first, _) => first,
        _ => arena.alloc(Lir::First(value)),
    }
}

// If the value is a path, we can optimize it to a rest path
// If the value is a cons, we can extract the rest element out of it
fn opt_rest(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    match arena[value].clone() {
        Lir::Path(path) => arena.alloc(Lir::Path(rest_path(path))),
        Lir::Cons(_, rest) => rest,
        _ => arena.alloc(Lir::Rest(value)),
    }
}

// If the first or rest is a raise, we can return it directly
fn opt_cons(arena: &mut Arena<Lir>, first: LirId, rest: LirId) -> LirId {
    if matches!(arena[first], Lir::Raise(_)) {
        return first;
    }

    if matches!(arena[rest], Lir::Raise(_)) {
        return rest;
    }

    arena.alloc(Lir::Cons(first, rest))
}

// If the value is an atom or pair, we know the result
// If the value is a raise, we can return it directly
fn opt_listp(arena: &mut Arena<Lir>, value: LirId, can_be_truthy: bool) -> LirId {
    match arena[value].clone() {
        Lir::Atom(_) => arena.alloc(Lir::Atom(vec![])),
        Lir::Cons(..) => arena.alloc(Lir::Atom(vec![1])),
        Lir::Raise(_) => value,
        _ => arena.alloc(Lir::Listp(value, can_be_truthy)),
    }
}

fn opt_add(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    let mut args = VecDeque::from(args);
    let mut result = Vec::new();

    while let Some(arg) = args.pop_front() {
        match arena[arg].clone() {
            Lir::Atom(atom) => {
                if let Some(last_id) = result
                    .iter_mut()
                    .find(|id| matches!(arena[**id], Lir::Atom(..)))
                    && let Lir::Atom(last) = arena[*last_id].clone()
                {
                    let mut allocator = Allocator::new();
                    let lhs = allocator.new_atom(&last).unwrap();
                    let rhs = allocator.new_atom(&atom).unwrap();
                    let sum = bigint_atom(allocator.number(lhs) + allocator.number(rhs));
                    *last_id = arena.alloc(Lir::Atom(sum));
                } else {
                    result.push(arg);
                }
            }
            Lir::Add(items) => {
                for item in items.into_iter().rev() {
                    args.push_front(item);
                }
            }
            _ => {
                result.push(arg);
            }
        }
    }

    if result.is_empty() {
        return arena.alloc(Lir::Atom(vec![]));
    }

    if result.len() == 1 {
        return result[0];
    }

    arena.alloc(Lir::Add(result))
}

fn opt_sub(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::Sub(args))
}

fn opt_mul(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::Mul(args))
}

fn opt_div(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    arena.alloc(Lir::Div(left, right))
}

fn opt_divmod(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    arena.alloc(Lir::Divmod(left, right))
}

fn opt_mod(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    arena.alloc(Lir::Mod(left, right))
}

fn opt_modpow(arena: &mut Arena<Lir>, base: LirId, exponent: LirId, modulus: LirId) -> LirId {
    arena.alloc(Lir::Modpow(base, exponent, modulus))
}

fn opt_eq(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    arena.alloc(Lir::Eq(left, right))
}

fn opt_gt(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    arena.alloc(Lir::Gt(left, right))
}

fn opt_gtbytes(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    arena.alloc(Lir::GtBytes(left, right))
}

// If the value is another not, we can unwrap both
// If it resolves to true or false, we know the result
// If the value is a raise, we can return it directly
fn opt_not(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    if let Lir::Not(value) = arena[value].clone() {
        return value;
    }

    match opt_truthy(arena, value) {
        Ok(true) => arena.alloc(Lir::Atom(vec![])),
        Ok(false) => arena.alloc(Lir::Atom(vec![1])),
        Err(value) => {
            if matches!(arena[value], Lir::Raise(_)) {
                return value;
            }

            arena.alloc(Lir::Not(value))
        }
    }
}

// If one of the arguments is true, we can ignore it
// If one of the arguments is false, we know the result is false
// If one of the arguments is a raise, we can return it directly
// TODO: Collapse nested alls
fn opt_all(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    let mut result = Vec::new();

    for arg in args {
        match opt_truthy(arena, arg) {
            Ok(true) => {}
            Ok(false) => return arena.alloc(Lir::Atom(vec![])),
            Err(arg) => {
                if matches!(arena[arg], Lir::Raise(_)) {
                    return arg;
                }

                result.push(arg);
            }
        }
    }

    arena.alloc(Lir::All(result))
}

// If one of the arguments is false, we can ignore it
// If one of the arguments is true, we know the result is true
// If one of the arguments is a raise, we can return it directly
// TODO: Collapse nested anys
fn opt_any(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    let mut result = Vec::new();

    for arg in args {
        match opt_truthy(arena, arg) {
            Ok(false) => {}
            Ok(true) => return arena.alloc(Lir::Atom(vec![1])),
            Err(arg) => {
                if matches!(arena[arg], Lir::Raise(_)) {
                    return arg;
                }

                result.push(arg);
            }
        }
    }

    arena.alloc(Lir::Any(result))
}

// If the condition is true, we can return the then branch
// If the condition is false, we can return the else branch
// If the condition is a raise, we can return it directly
// If the condition is a not, we can flip the then and else branches
fn opt_if(arena: &mut Arena<Lir>, condition: LirId, then: LirId, otherwise: LirId) -> LirId {
    match opt_truthy(arena, condition) {
        Ok(true) => then,
        Ok(false) => otherwise,
        Err(condition) => {
            if matches!(arena[condition], Lir::Raise(_)) {
                return condition;
            }

            let (condition, then, otherwise) = if let Lir::Not(opposite) = arena[condition].clone()
            {
                (opposite, otherwise, then)
            } else {
                (condition, then, otherwise)
            };

            arena.alloc(Lir::If(condition, then, otherwise))
        }
    }
}

// We can remove all arguments from raise, since the program fails either way
fn opt_raise(arena: &mut Arena<Lir>, _args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::Raise(vec![]))
}

fn opt_concat(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    let mut args = VecDeque::from(args);
    let mut result = Vec::new();

    while let Some(arg) = args.pop_front() {
        match arena[arg].clone() {
            Lir::Atom(atom) => {
                if let Some(last_id) = result.last_mut()
                    && let Lir::Atom(last) = arena[*last_id].clone()
                {
                    *last_id = arena.alloc(Lir::Atom([last, atom].concat()));
                } else {
                    result.push(arg);
                }
            }
            Lir::Concat(items) => {
                for item in items.into_iter().rev() {
                    args.push_front(item);
                }
            }
            _ => {
                result.push(arg);
            }
        }
    }

    if result.is_empty() {
        return arena.alloc(Lir::Atom(Vec::new()));
    }

    if result.len() == 1 {
        return result[0];
    }

    arena.alloc(Lir::Concat(result.into_iter().collect()))
}

// If the value is an atom, we know the result
// If the value is a raise, we can return it directly
fn opt_strlen(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    match arena[value].clone() {
        Lir::Atom(atom) => arena.alloc(Lir::Atom(bigint_atom(atom.len().into()))),
        Lir::Raise(_) => value,
        _ => arena.alloc(Lir::Strlen(value)),
    }
}

// If the string, start, or end is a raise, we can return it directly
fn opt_substr(arena: &mut Arena<Lir>, string: LirId, start: LirId, end: Option<LirId>) -> LirId {
    if matches!(arena[string], Lir::Raise(_)) {
        return string;
    }

    if matches!(arena[start], Lir::Raise(_)) {
        return start;
    }

    if let Some(end) = end
        && matches!(arena[end], Lir::Raise(_))
    {
        return end;
    }

    arena.alloc(Lir::Substr(string, start, end))
}

fn opt_logand(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::Logand(args))
}

fn opt_logior(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::Logior(args))
}

fn opt_logxor(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::Logxor(args))
}

fn opt_lognot(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    arena.alloc(Lir::Lognot(value))
}

fn opt_ash(arena: &mut Arena<Lir>, value: LirId, shift: LirId) -> LirId {
    arena.alloc(Lir::Ash(value, shift))
}

fn opt_lsh(arena: &mut Arena<Lir>, value: LirId, shift: LirId) -> LirId {
    arena.alloc(Lir::Lsh(value, shift))
}

fn opt_pubkey_for_exp(arena: &mut Arena<Lir>, exp: LirId) -> LirId {
    arena.alloc(Lir::PubkeyForExp(exp))
}

fn opt_g1_add(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::G1Add(args))
}

fn opt_g1_subtract(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::G1Subtract(args))
}

fn opt_g1_multiply(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    arena.alloc(Lir::G1Multiply(left, right))
}

fn opt_g1_negate(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    arena.alloc(Lir::G1Negate(value))
}

fn opt_g1_map(arena: &mut Arena<Lir>, value: LirId, map: Option<LirId>) -> LirId {
    arena.alloc(Lir::G1Map(value, map))
}

fn opt_g2_add(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::G2Add(args))
}

fn opt_g2_subtract(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::G2Subtract(args))
}

fn opt_g2_multiply(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    arena.alloc(Lir::G2Multiply(left, right))
}

fn opt_g2_negate(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    arena.alloc(Lir::G2Negate(value))
}

fn opt_g2_map(arena: &mut Arena<Lir>, value: LirId, map: Option<LirId>) -> LirId {
    arena.alloc(Lir::G2Map(value, map))
}

fn opt_bls_pairing_identity(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::BlsPairingIdentity(args))
}

fn opt_bls_verify(arena: &mut Arena<Lir>, sig: LirId, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::BlsVerify(sig, args))
}

fn opt_sha256(arena: &mut Arena<Lir>, args: Vec<LirId>, inline: bool) -> LirId {
    let mut args = VecDeque::from(args);
    let mut result = Vec::new();

    while let Some(arg) = args.pop_front() {
        match arena[arg].clone() {
            Lir::Atom(atom) => {
                if let Some(last_id) = result.last_mut()
                    && let Lir::Atom(last) = arena[*last_id].clone()
                {
                    *last_id = arena.alloc(Lir::Atom([last, atom].concat()));
                } else {
                    result.push(arg);
                }
            }
            Lir::Concat(items) => {
                for item in items.into_iter().rev() {
                    args.push_front(item);
                }
            }
            _ => {
                result.push(arg);
            }
        }
    }

    if inline
        && result.len() <= 1
        && let Lir::Atom(atom) = result
            .first()
            .copied()
            .map_or(Lir::Atom(vec![]), |id| arena[id].clone())
    {
        let value: [u8; 32] = Sha256::digest(&atom).into();
        return arena.alloc(Lir::Atom(value.to_vec()));
    }

    arena.alloc(Lir::Sha256(result.into_iter().collect()))
}

fn opt_keccak256(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::Keccak256(args))
}

fn opt_coin_id(
    arena: &mut Arena<Lir>,
    parent_coin_info: LirId,
    puzzle_hash: LirId,
    amount: LirId,
) -> LirId {
    arena.alloc(Lir::CoinId(parent_coin_info, puzzle_hash, amount))
}

fn opt_k1_verify(
    arena: &mut Arena<Lir>,
    public_key: LirId,
    message: LirId,
    signature: LirId,
) -> LirId {
    arena.alloc(Lir::K1Verify(public_key, message, signature))
}

fn opt_r1_verify(
    arena: &mut Arena<Lir>,
    public_key: LirId,
    message: LirId,
    signature: LirId,
) -> LirId {
    arena.alloc(Lir::R1Verify(public_key, message, signature))
}

fn opt_truthy(arena: &mut Arena<Lir>, value: LirId) -> Result<bool, LirId> {
    match arena[value].clone() {
        Lir::Atom(atom) => Ok(!atom.is_empty()),
        Lir::Cons(..) => Ok(true),
        Lir::Listp(inner, atom_can_be_truthy) => {
            if atom_can_be_truthy {
                Err(value)
            } else {
                opt_truthy(arena, inner)
            }
        }
        _ => Err(value),
    }
}

fn has_path_quotable(arena: &Arena<Lir>, value: LirId) -> bool {
    if let Lir::Quote(value) = arena[value].clone() {
        has_path(arena, value)
    } else {
        has_path(arena, value)
    }
}

// TODO: Is this safe in all cases?
fn has_path(arena: &Arena<Lir>, value: LirId) -> bool {
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
