use id_arena::Arena;
use num_bigint::BigInt;
use sha2::{Digest, Sha256};

use crate::{
    Lir, LirId, atom_bigint, bigint_atom, first_path,
    optimize::{ArgList, has_path, has_path_quotable, opt_truthy},
    rest_path,
};

// There's no way to optimize an atom
pub fn opt_atom(arena: &mut Arena<Lir>, atom: Vec<u8>) -> LirId {
    arena.alloc(Lir::Atom(atom))
}

// There's no way to optimize a path
pub fn opt_path(arena: &mut Arena<Lir>, path: u32) -> LirId {
    arena.alloc(Lir::Path(path))
}

// There's no way to optimize a quoted value, since nil is optimized by codegen already
pub fn opt_quote(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    arena.alloc(Lir::Quote(value))
}

// If the program is quoted, and the environment is the same as the parent,
// we can skip both quoting and running the program, and just use it directly
// We can also skip quoting if the program has no path, since it's not going to rely on the environment
pub fn opt_run(arena: &mut Arena<Lir>, callee: LirId, env: LirId) -> LirId {
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
pub fn opt_curry(arena: &mut Arena<Lir>, callee: LirId, args: Vec<LirId>) -> LirId {
    if let Lir::Quote(value) = arena[callee].clone()
        && (args.is_empty() || !has_path(arena, value))
    {
        return value;
    }

    arena.alloc(Lir::Curry(callee, args))
}

// If there are no captures, we don't need to create a closure
// We can also skip the closure if the program has no path, since it's not going to rely on the captures
pub fn opt_closure(arena: &mut Arena<Lir>, callee: LirId, args: Vec<LirId>) -> LirId {
    if args.is_empty() || !has_path_quotable(arena, callee) {
        return callee;
    }

    arena.alloc(Lir::Closure(callee, args))
}

// If the value is a path, we can optimize it to a first path
// If the value is a cons, we can extract the first element out of it
// If the value is a divmod, we can optimize it to a div since that's the first output
// If the value is a raise, we can return it directly
pub fn opt_first(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    match arena[value].clone() {
        Lir::Path(path) => arena.alloc(Lir::Path(first_path(path))),
        Lir::Cons(first, _) => first,
        Lir::Divmod(left, right) => opt_div(arena, left, right),
        Lir::Raise(_) => value,
        _ => arena.alloc(Lir::First(value)),
    }
}

// If the value is a path, we can optimize it to a rest path
// If the value is a cons, we can extract the rest element out of it
// If the value is a divmod, we can optimize it to a remainder since that's the rest output
// If the value is a raise, we can return it directly
pub fn opt_rest(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    match arena[value].clone() {
        Lir::Path(path) => arena.alloc(Lir::Path(rest_path(path))),
        Lir::Cons(_, rest) => rest,
        Lir::Divmod(left, right) => opt_mod(arena, left, right),
        Lir::Raise(_) => value,
        _ => arena.alloc(Lir::Rest(value)),
    }
}

// If the first or rest is a raise, we can return it directly
// TODO: Optimize pair of div and mod into divmod
pub fn opt_cons(arena: &mut Arena<Lir>, first: LirId, rest: LirId) -> LirId {
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
pub fn opt_listp(arena: &mut Arena<Lir>, value: LirId, can_be_truthy: bool) -> LirId {
    match arena[value].clone() {
        Lir::Atom(_) => arena.alloc(Lir::Atom(vec![])),
        Lir::Cons(..) => arena.alloc(Lir::Atom(vec![1])),
        Lir::Raise(_) => value,
        _ => arena.alloc(Lir::Listp(value, can_be_truthy)),
    }
}

// If the value is an atom, we can add it to a sum
// We can collapse nested adds
// If the value is a raise, we can return it directly
pub fn opt_add(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    let mut args = ArgList::new(args);
    let mut result = Vec::new();
    let mut sum = BigInt::from(0);

    while let Some(arg) = args.next() {
        match arena[arg].clone() {
            Lir::Atom(atom) => sum += atom_bigint(atom),
            Lir::Add(items) => args.prepend(items),
            Lir::Raise(_) => return arg,
            _ => result.push(arg),
        }
    }

    if sum != BigInt::from(0) {
        result.push(arena.alloc(Lir::Atom(bigint_atom(sum))));
    }

    if result.is_empty() {
        return arena.alloc(Lir::Atom(vec![]));
    }

    if result.len() == 1 {
        return result[0];
    }

    arena.alloc(Lir::Add(result))
}

pub fn opt_sub(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    let mut args = ArgList::new(args);
    let mut result = Vec::new();
    let mut first = None;
    let mut sum = BigInt::from(0);

    while let Some(arg) = args.next() {
        match arena[arg].clone() {
            Lir::Atom(atom) => {
                if first.is_none() {
                    first = Some(arg);
                    continue;
                }
                sum += atom_bigint(atom);
            }
            Lir::Sub(items) => {
                if first.is_none() {
                    first = Some(arena.alloc(Lir::Atom(vec![])));
                }
                args.prepend(items);
            }
            Lir::Raise(_) => return arg,
            _ => {
                if first.is_none() {
                    first = Some(arg);
                    continue;
                }
                result.push(arg);
            }
        }
    }

    if let Some(first) = first {
        result.insert(0, first);
    }

    if sum != BigInt::from(0) {
        result.push(arena.alloc(Lir::Atom(bigint_atom(sum))));
    }

    if result.is_empty() {
        return arena.alloc(Lir::Atom(vec![]));
    }

    if result.len() == 1 {
        return result[0];
    }

    arena.alloc(Lir::Sub(result))
}

pub fn opt_mul(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::Mul(args))
}

pub fn opt_div(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    arena.alloc(Lir::Div(left, right))
}

pub fn opt_divmod(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    arena.alloc(Lir::Divmod(left, right))
}

pub fn opt_mod(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    arena.alloc(Lir::Mod(left, right))
}

pub fn opt_modpow(arena: &mut Arena<Lir>, base: LirId, exponent: LirId, modulus: LirId) -> LirId {
    arena.alloc(Lir::Modpow(base, exponent, modulus))
}

pub fn opt_eq(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    arena.alloc(Lir::Eq(left, right))
}

pub fn opt_gt(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    arena.alloc(Lir::Gt(left, right))
}

pub fn opt_gtbytes(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    arena.alloc(Lir::GtBytes(left, right))
}

// If the value is another not, we can unwrap both
// If it resolves to true or false, we know the result
// If the value is a raise, we can return it directly
pub fn opt_not(arena: &mut Arena<Lir>, value: LirId) -> LirId {
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
pub fn opt_all(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
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
pub fn opt_any(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
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
pub fn opt_if(arena: &mut Arena<Lir>, condition: LirId, then: LirId, otherwise: LirId) -> LirId {
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
pub fn opt_raise(arena: &mut Arena<Lir>, _args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::Raise(vec![]))
}

pub fn opt_concat(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    let mut args = ArgList::new(args);
    let mut result = Vec::new();

    while let Some(arg) = args.next() {
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
                args.prepend(items);
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
pub fn opt_strlen(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    match arena[value].clone() {
        Lir::Atom(atom) => arena.alloc(Lir::Atom(bigint_atom(atom.len().into()))),
        Lir::Raise(_) => value,
        _ => arena.alloc(Lir::Strlen(value)),
    }
}

// If the string, start, or end is a raise, we can return it directly
pub fn opt_substr(
    arena: &mut Arena<Lir>,
    string: LirId,
    start: LirId,
    end: Option<LirId>,
) -> LirId {
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

pub fn opt_logand(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::Logand(args))
}

pub fn opt_logior(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::Logior(args))
}

pub fn opt_logxor(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::Logxor(args))
}

pub fn opt_lognot(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    arena.alloc(Lir::Lognot(value))
}

pub fn opt_ash(arena: &mut Arena<Lir>, value: LirId, shift: LirId) -> LirId {
    arena.alloc(Lir::Ash(value, shift))
}

pub fn opt_lsh(arena: &mut Arena<Lir>, value: LirId, shift: LirId) -> LirId {
    arena.alloc(Lir::Lsh(value, shift))
}

pub fn opt_pubkey_for_exp(arena: &mut Arena<Lir>, exp: LirId) -> LirId {
    arena.alloc(Lir::PubkeyForExp(exp))
}

pub fn opt_g1_add(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::G1Add(args))
}

pub fn opt_g1_subtract(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::G1Subtract(args))
}

pub fn opt_g1_multiply(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    arena.alloc(Lir::G1Multiply(left, right))
}

pub fn opt_g1_negate(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    arena.alloc(Lir::G1Negate(value))
}

pub fn opt_g1_map(arena: &mut Arena<Lir>, value: LirId, map: Option<LirId>) -> LirId {
    arena.alloc(Lir::G1Map(value, map))
}

pub fn opt_g2_add(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::G2Add(args))
}

pub fn opt_g2_subtract(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::G2Subtract(args))
}

pub fn opt_g2_multiply(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    arena.alloc(Lir::G2Multiply(left, right))
}

pub fn opt_g2_negate(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    arena.alloc(Lir::G2Negate(value))
}

pub fn opt_g2_map(arena: &mut Arena<Lir>, value: LirId, map: Option<LirId>) -> LirId {
    arena.alloc(Lir::G2Map(value, map))
}

pub fn opt_bls_pairing_identity(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::BlsPairingIdentity(args))
}

pub fn opt_bls_verify(arena: &mut Arena<Lir>, sig: LirId, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::BlsVerify(sig, args))
}

pub fn opt_sha256(arena: &mut Arena<Lir>, args: Vec<LirId>, inline: bool) -> LirId {
    let mut args = ArgList::new(args);
    let mut result = Vec::new();

    while let Some(arg) = args.next() {
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
                args.prepend(items);
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

pub fn opt_keccak256(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::Keccak256(args))
}

pub fn opt_coin_id(
    arena: &mut Arena<Lir>,
    parent_coin_info: LirId,
    puzzle_hash: LirId,
    amount: LirId,
) -> LirId {
    arena.alloc(Lir::CoinId(parent_coin_info, puzzle_hash, amount))
}

pub fn opt_k1_verify(
    arena: &mut Arena<Lir>,
    public_key: LirId,
    message: LirId,
    signature: LirId,
) -> LirId {
    arena.alloc(Lir::K1Verify(public_key, message, signature))
}

pub fn opt_r1_verify(
    arena: &mut Arena<Lir>,
    public_key: LirId,
    message: LirId,
    signature: LirId,
) -> LirId {
    arena.alloc(Lir::R1Verify(public_key, message, signature))
}
