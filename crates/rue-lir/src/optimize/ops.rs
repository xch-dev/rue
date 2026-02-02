use std::num::Saturating;

use id_arena::Arena;
use num_bigint::{BigInt, Sign};
use num_integer::Integer;
use sha2::{Digest, Sha256};
use sha3::Keccak256;

use crate::{
    ClvmOp, Lir, LirId, atom_bigint, bigint_atom, first_path,
    optimize::{ArgList, opt_truthy},
    parent_path, rest_path,
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
    if let Lir::Quote(value) = arena[callee].clone() {
        if let Lir::Path(1) = arena[env].clone() {
            return value;
        }

        if let Lir::Atom(atom) = arena[env].clone()
            && atom.is_empty()
        {
            return value;
        }
    }

    arena.alloc(Lir::Run(callee, env))
}

// If there are no captures, we don't need to create a closure
// We can also skip the closure if the program has no path, since it's not going to rely on the captures
pub fn opt_closure(
    arena: &mut Arena<Lir>,
    callee: LirId,
    args: Vec<LirId>,
    has_parameters: bool,
) -> LirId {
    if args.is_empty() {
        return callee;
    }

    arena.alloc(Lir::Closure(callee, args, has_parameters))
}

// If the value is a path, we can optimize it to a first path
// If the value is a cons, we can extract the first element out of it
// If the value is a divmod, we can optimize it to a div since that's the first output
// If the value is a raise, we can return it directly
pub fn opt_first(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    match arena[value].clone() {
        Lir::Path(path) => arena.alloc(Lir::Path(first_path(path))),
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

    if let Lir::Path(f_path) = arena[first].clone()
        && let Lir::Path(r_path) = arena[rest].clone()
        && let Some(parent) = parent_path(f_path)
        && Some(parent) == parent_path(r_path)
        && first_path(parent) == f_path
        && rest_path(parent) == r_path
    {
        return arena.alloc(Lir::Path(parent));
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

    if result.len() == 1 && matches!(arena[result[0]], Lir::Atom(_)) {
        return result[0];
    }

    arena.alloc(Lir::Add(result))
}

// Split the subtraction into minuends and subtrahends and sum them together,
// then subtract the subtrahend from the minuend
pub fn opt_sub(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    let mut minuend_value = BigInt::from(0);
    let mut other_minuends = Vec::new();
    let mut other_subtrahends = Vec::new();

    let mut args = ArgList::new(args);
    let mut minuend_count = Saturating(1usize);

    while let Some(arg) = args.next() {
        match arena[arg].clone() {
            Lir::Atom(atom) => {
                let value = atom_bigint(atom);

                if minuend_count > Saturating(0) {
                    minuend_value += value;
                } else {
                    // Fold subtrahend values into the minuend as an optimization
                    minuend_value -= value;
                }
            }
            Lir::Add(items) => {
                if minuend_count > Saturating(0) {
                    minuend_count += items.len();
                }

                args.prepend(items);
            }
            Lir::Sub(items) => {
                if !items.is_empty() {
                    if minuend_count > Saturating(0) {
                        // Just unroll the inner subtraction into the outer subtraction
                        // The first item is the new minuend, the rest are the subtrahends
                        args.prepend(items);
                        minuend_count += 1;
                    } else {
                        // The minuend of the inner subtraction is a subtrahend of the outer subtraction
                        args.prepend(vec![items[0]]);

                        // The subtrahends of the inner subtraction are minuends of the outer subtraction
                        // One extra because minuend_count will be subtracted at the end of this iteration
                        minuend_count += items.len();
                        args.prepend(items.iter().skip(1).copied().collect());
                    }
                }
            }
            Lir::Raise(_) => return arg,
            _ => {
                if minuend_count > Saturating(0) {
                    other_minuends.push(arg);
                } else {
                    other_subtrahends.push(arg);
                }
            }
        }

        minuend_count -= 1;
    }

    if other_minuends.is_empty() && other_subtrahends.is_empty() {
        return arena.alloc(Lir::Atom(bigint_atom(minuend_value)));
    }

    if minuend_value > BigInt::from(0)
        || (minuend_value != BigInt::from(0) && !other_subtrahends.is_empty())
    {
        other_minuends.insert(0, arena.alloc(Lir::Atom(bigint_atom(minuend_value))));
    } else if minuend_value < BigInt::from(0) {
        other_subtrahends.insert(0, arena.alloc(Lir::Atom(bigint_atom(-minuend_value))));
    }

    let minuend = if other_minuends.len() == 1
        && (matches!(arena[other_minuends[0]], Lir::Atom(_)) || !other_subtrahends.is_empty())
    {
        other_minuends[0]
    } else if other_minuends.is_empty() {
        arena.alloc(Lir::Atom(vec![]))
    } else {
        arena.alloc(Lir::Add(other_minuends))
    };

    if other_subtrahends.is_empty() {
        return minuend;
    }

    let mut result = vec![minuend];

    for subtrahend in other_subtrahends {
        result.push(subtrahend);
    }

    arena.alloc(Lir::Sub(result))
}

// If the value is an atom, we can add it to a product
// We can collapse nested muls
// If the value is a raise, we can return it directly
pub fn opt_mul(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    let mut args = ArgList::new(args);
    let mut result = Vec::new();
    let mut product = BigInt::from(1);

    while let Some(arg) = args.next() {
        match arena[arg].clone() {
            Lir::Atom(atom) => product *= atom_bigint(atom),
            Lir::Mul(items) => args.prepend(items),
            Lir::Raise(_) => return arg,
            _ => result.push(arg),
        }
    }

    if product != BigInt::from(1) {
        result.push(arena.alloc(Lir::Atom(bigint_atom(product))));
    }

    if result.is_empty() && matches!(arena[result[0]], Lir::Atom(_)) {
        return arena.alloc(Lir::Atom(vec![1]));
    }

    if result.len() == 1 {
        return result[0];
    }

    arena.alloc(Lir::Mul(result))
}

// If both values are atoms, we can perform the division (as long as the right value is not zero)
// If either value is a raise, we can return it directly
pub fn opt_div(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    if matches!(arena[left], Lir::Raise(_)) {
        return left;
    }

    if matches!(arena[right], Lir::Raise(_)) {
        return right;
    }

    if let Lir::Atom(left) = arena[left].clone()
        && let Lir::Atom(right) = arena[right].clone()
        && let left = atom_bigint(left)
        && let right = atom_bigint(right)
        && right.sign() != Sign::NoSign
    {
        return arena.alloc(Lir::Atom(bigint_atom(left.div_floor(&right))));
    }

    arena.alloc(Lir::Div(left, right))
}

// If both values are atoms, we can perform the division and remainder (as long as the right value is not zero)
// If either value is a raise, we can return it directly
pub fn opt_divmod(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    if matches!(arena[left], Lir::Raise(_)) {
        return left;
    }

    if matches!(arena[right], Lir::Raise(_)) {
        return right;
    }

    if let Lir::Atom(left) = arena[left].clone()
        && let Lir::Atom(right) = arena[right].clone()
        && let left = atom_bigint(left)
        && let right = atom_bigint(right)
        && right.sign() != Sign::NoSign
    {
        let (quotient, remainder) = left.div_mod_floor(&right);

        let quotient = arena.alloc(Lir::Atom(bigint_atom(quotient)));
        let remainder = arena.alloc(Lir::Atom(bigint_atom(remainder)));

        return arena.alloc(Lir::Cons(quotient, remainder));
    }

    arena.alloc(Lir::Divmod(left, right))
}

// If both values are atoms, we can perform the remainder (as long as the right value is not zero)
// If either value is a raise, we can return it directly
pub fn opt_mod(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    if matches!(arena[left], Lir::Raise(_)) {
        return left;
    }

    if matches!(arena[right], Lir::Raise(_)) {
        return right;
    }

    if let Lir::Atom(left) = arena[left].clone()
        && let Lir::Atom(right) = arena[right].clone()
        && let left = atom_bigint(left)
        && let right = atom_bigint(right)
        && right.sign() != Sign::NoSign
    {
        return arena.alloc(Lir::Atom(bigint_atom(left.mod_floor(&right))));
    }

    arena.alloc(Lir::Mod(left, right))
}

pub fn opt_modpow(arena: &mut Arena<Lir>, base: LirId, exponent: LirId, modulus: LirId) -> LirId {
    arena.alloc(Lir::Modpow(base, exponent, modulus))
}

pub fn opt_eq(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    if let Lir::Mod(lhs, rhs) = arena[left].clone()
        && let Lir::Atom(rhs) = arena[rhs].clone()
        && atom_bigint(rhs) == BigInt::from(2)
        && let Lir::Atom(eq) = arena[right].clone()
        && atom_bigint(eq) == BigInt::from(1)
    {
        return arena.alloc(Lir::Logand(vec![lhs, right]));
    }

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
    match opt_truthy(arena, value) {
        Ok(true) => arena.alloc(Lir::Atom(vec![])),
        Ok(false) => arena.alloc(Lir::Atom(vec![1])),
        Err(value) => {
            if let Lir::Not(value) = arena[value].clone() {
                return value;
            }

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
    let mut has_false = false;

    for arg in args {
        match opt_truthy(arena, arg) {
            Ok(true) => {}
            Ok(false) => {
                if !has_false {
                    has_false = true;
                    result.push(arg);
                }
            }
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
    let mut has_true = false;

    for arg in args {
        match opt_truthy(arena, arg) {
            Ok(false) => {}
            Ok(true) => {
                if !has_true {
                    has_true = true;
                    result.push(arg);
                }
            }
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
pub fn opt_if(
    arena: &mut Arena<Lir>,
    condition: LirId,
    then: LirId,
    otherwise: LirId,
    inline: bool,
) -> LirId {
    match opt_truthy(arena, condition) {
        Ok(true) if !inline => then,
        Ok(false) if !inline => otherwise,
        Ok(condition) => {
            let nil = arena.alloc(Lir::Atom(vec![]));

            if condition {
                arena.alloc(Lir::If(nil, otherwise, then, inline))
            } else {
                arena.alloc(Lir::If(nil, then, otherwise, inline))
            }
        }
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

            arena.alloc(Lir::If(condition, then, otherwise, inline))
        }
    }
}

// We can remove all arguments from raise, since the program fails either way
pub fn opt_raise(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    arena.alloc(Lir::Raise(args))
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

    if result.len() == 1 && matches!(arena[result[0]], Lir::Atom(_)) {
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
    let mut args = ArgList::new(args);
    let mut result = Vec::new();
    let mut value = BigInt::from(-1);

    while let Some(arg) = args.next() {
        match arena[arg].clone() {
            Lir::Atom(atom) => value &= atom_bigint(atom),
            Lir::Logand(items) => args.prepend(items),
            Lir::Raise(_) => return arg,
            _ => result.push(arg),
        }
    }

    if value != BigInt::from(-1) {
        result.push(arena.alloc(Lir::Atom(bigint_atom(value))));
    }

    if result.is_empty() {
        return arena.alloc(Lir::Atom(bigint_atom(BigInt::from(-1))));
    }

    if result.len() == 1 && matches!(arena[result[0]], Lir::Atom(_)) {
        return result[0];
    }

    arena.alloc(Lir::Logand(result))
}

pub fn opt_logior(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    let mut args = ArgList::new(args);
    let mut result = Vec::new();
    let mut value = BigInt::from(0);

    while let Some(arg) = args.next() {
        match arena[arg].clone() {
            Lir::Atom(atom) => value |= atom_bigint(atom),
            Lir::Logior(items) => args.prepend(items),
            Lir::Raise(_) => return arg,
            _ => result.push(arg),
        }
    }

    if value != BigInt::from(0) {
        result.push(arena.alloc(Lir::Atom(bigint_atom(value))));
    }

    if result.is_empty() {
        return arena.alloc(Lir::Atom(vec![]));
    }

    if result.len() == 1 && matches!(arena[result[0]], Lir::Atom(_)) {
        return result[0];
    }

    arena.alloc(Lir::Logior(result))
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

pub fn opt_keccak256(arena: &mut Arena<Lir>, args: Vec<LirId>, inline: bool) -> LirId {
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
        let value: [u8; 32] = Keccak256::digest(&atom).into();
        return arena.alloc(Lir::Atom(value.to_vec()));
    }

    arena.alloc(Lir::Keccak256(result.into_iter().collect()))
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

pub fn opt_op(arena: &mut Arena<Lir>, op: ClvmOp, arg: LirId) -> LirId {
    arena.alloc(Lir::Op(op, arg))
}

pub fn opt_debug_print(arena: &mut Arena<Lir>, srcloc: String, value: LirId) -> LirId {
    arena.alloc(Lir::DebugPrint(srcloc, value))
}
