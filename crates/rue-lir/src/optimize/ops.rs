use std::num::Saturating;

use id_arena::Arena;
use num_bigint::{BigInt, BigUint, Sign};
use num_integer::Integer;
use sha2::{Digest, Sha256};
use sha3::Keccak256;

use crate::{
    ClvmOp, Lir, LirId, atom_bigint, bigint_atom, first_path,
    optimize::{ArgList, fold_atoms, opt_truthy, reads_env},
    rest_path,
};

// There's no way to optimize an atom
pub fn opt_atom(arena: &mut Arena<Lir>, atom: Vec<u8>) -> LirId {
    arena.alloc(Lir::Atom(atom))
}

// There's no way to optimize a path
pub fn opt_path(arena: &mut Arena<Lir>, path: BigUint) -> LirId {
    arena.alloc(Lir::Path(path))
}

// There's no way to optimize a quoted value, since nil is optimized by codegen already
pub fn opt_quote(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    arena.alloc(Lir::Quote(value))
}

// If the program is quoted, and the environment is the same as the parent,
// we can skip both quoting and running the program, and just use it directly
// We can also skip quoting if the environment is nil and the program never reads its environment,
// since the result doesn't depend on which environment it's run in
pub fn opt_run(arena: &mut Arena<Lir>, callee: LirId, env: LirId) -> LirId {
    if let Lir::Quote(value) = arena[callee].clone() {
        if let Lir::Path(path) = arena[env].clone()
            && path == BigUint::from(1u8)
        {
            return value;
        }

        if let Lir::Atom(atom) = arena[env].clone()
            && atom.is_empty()
            && !reads_env(arena, value)
        {
            return value;
        }
    }

    arena.alloc(Lir::Run(callee, env))
}

// If there are no captures, we don't need to create a closure
// The closure would be `(a (q . F) 1)` if it has parameters, which is equivalent to `F`
// Otherwise it would be `(a (q . F) ())`, which is only equivalent to `F` if it never reads its environment
pub fn opt_closure(
    arena: &mut Arena<Lir>,
    callee: LirId,
    args: Vec<LirId>,
    has_parameters: bool,
) -> LirId {
    if args.is_empty()
        && (has_parameters
            || matches!(arena[callee].clone(), Lir::Quote(value) if !reads_env(arena, value)))
    {
        return callee;
    }

    arena.alloc(Lir::Closure(callee, args, has_parameters))
}

// If the value is a path, we can optimize it to a first path, since both fail if it's an atom
// Path 0 is excluded, since it evaluates to nil rather than reading the environment
// If the value is a cons, we can extract the first element out of it
// If the value is a divmod, we can optimize it to a div since that's the first output
pub fn opt_first(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    match arena[value].clone() {
        Lir::Path(path) if path != BigUint::ZERO => arena.alloc(Lir::Path(first_path(&path))),
        Lir::Divmod(left, right) => opt_div(arena, left, right),
        _ => arena.alloc(Lir::First(value)),
    }
}

// If the value is a path, we can optimize it to a rest path, since both fail if it's an atom
// Path 0 is excluded, since it evaluates to nil rather than reading the environment
// If the value is a cons, we can extract the rest element out of it
// If the value is a divmod, we can optimize it to a remainder since that's the rest output
pub fn opt_rest(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    match arena[value].clone() {
        Lir::Path(path) if path != BigUint::ZERO => arena.alloc(Lir::Path(rest_path(&path))),
        Lir::Divmod(left, right) => opt_mod(arena, left, right),
        _ => arena.alloc(Lir::Rest(value)),
    }
}

// TODO: Optimize pair of div and mod into divmod
// Note that `(c (f p) (r p))` can't be simplified to `p`, since `p` may be an atom at runtime
// (for example, if it comes from the solution), in which case the original fails but `p` wouldn't
pub fn opt_cons(arena: &mut Arena<Lir>, first: LirId, rest: LirId) -> LirId {
    arena.alloc(Lir::Cons(first, rest))
}

// If the value is an atom, we know the result
pub fn opt_listp(arena: &mut Arena<Lir>, value: LirId, can_be_truthy: bool) -> LirId {
    match arena[value].clone() {
        Lir::Atom(_) => arena.alloc(Lir::Atom(vec![])),
        _ => arena.alloc(Lir::Listp(value, can_be_truthy)),
    }
}

// If the value is an atom, we can add it to a sum
// We can collapse nested adds
pub fn opt_add(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    let mut args = ArgList::new(args);
    let mut result = Vec::new();
    let mut sum = BigInt::from(0);

    while let Some(arg) = args.next() {
        match arena[arg].clone() {
            Lir::Atom(atom) => sum += atom_bigint(atom),
            Lir::Add(items) => args.prepend(items),
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
pub fn opt_mul(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    let mut args = ArgList::new(args);
    let mut result = Vec::new();
    let mut product = BigInt::from(1);

    while let Some(arg) = args.next() {
        match arena[arg].clone() {
            Lir::Atom(atom) => product *= atom_bigint(atom),
            Lir::Mul(items) => args.prepend(items),
            _ => result.push(arg),
        }
    }

    if product != BigInt::from(1) {
        result.push(arena.alloc(Lir::Atom(bigint_atom(product))));
    }

    if result.is_empty() {
        return arena.alloc(Lir::Atom(vec![1]));
    }

    // A lone non-constant operand is kept as `(* x)`, since multiplication
    // normalizes non-canonical integer atoms (e.g. `0x0001` to `1`)
    if result.len() == 1 && matches!(arena[result[0]], Lir::Atom(_)) {
        return result[0];
    }

    arena.alloc(Lir::Mul(result))
}

// If both values are atoms, we can perform the division (as long as the right value is not zero)
pub fn opt_div(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
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
pub fn opt_divmod(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
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
pub fn opt_mod(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
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
    if let Some(folded) = fold_atoms(arena, ClvmOp::Modpow, &[base, exponent, modulus]) {
        return folded;
    }

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

    if let Some(folded) = fold_atoms(arena, ClvmOp::Eq, &[left, right]) {
        return folded;
    }

    arena.alloc(Lir::Eq(left, right))
}

pub fn opt_gt(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    if let Some(folded) = fold_atoms(arena, ClvmOp::Gt, &[left, right]) {
        return folded;
    }

    arena.alloc(Lir::Gt(left, right))
}

pub fn opt_gtbytes(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    if let Some(folded) = fold_atoms(arena, ClvmOp::GtBytes, &[left, right]) {
        return folded;
    }

    arena.alloc(Lir::GtBytes(left, right))
}

// If the value is another not, we can unwrap both
// If it resolves to true or false, we know the result
pub fn opt_not(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    match opt_truthy(arena, value) {
        Ok(true) => arena.alloc(Lir::Atom(vec![])),
        Ok(false) => arena.alloc(Lir::Atom(vec![1])),
        Err(value) => {
            if let Lir::Not(value) = arena[value].clone() {
                return value;
            }

            arena.alloc(Lir::Not(value))
        }
    }
}

// If one of the arguments is true, we can ignore it
// If one of the arguments is false, we know the result is false
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
                result.push(arg);
            }
        }
    }

    arena.alloc(Lir::All(result))
}

// If one of the arguments is false, we can ignore it
// If one of the arguments is true, we know the result is true
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
                result.push(arg);
            }
        }
    }

    arena.alloc(Lir::Any(result))
}

// If the condition is true, we can return the then branch
// If the condition is false, we can return the else branch
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
pub fn opt_strlen(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    match arena[value].clone() {
        Lir::Atom(atom) => arena.alloc(Lir::Atom(bigint_atom(atom.len().into()))),
        _ => arena.alloc(Lir::Strlen(value)),
    }
}

pub fn opt_substr(
    arena: &mut Arena<Lir>,
    string: LirId,
    start: LirId,
    end: Option<LirId>,
) -> LirId {
    let args: Vec<LirId> = [string, start].into_iter().chain(end).collect();

    if let Some(folded) = fold_atoms(arena, ClvmOp::Substr, &args) {
        return folded;
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
    let mut args = ArgList::new(args);
    let mut result = Vec::new();
    let mut value = BigInt::from(0);

    while let Some(arg) = args.next() {
        match arena[arg].clone() {
            Lir::Atom(atom) => value ^= atom_bigint(atom),
            Lir::Logxor(items) => args.prepend(items),
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

    arena.alloc(Lir::Logxor(result))
}

pub fn opt_lognot(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    if let Some(folded) = fold_atoms(arena, ClvmOp::Lognot, &[value]) {
        return folded;
    }

    arena.alloc(Lir::Lognot(value))
}

pub fn opt_ash(arena: &mut Arena<Lir>, value: LirId, shift: LirId) -> LirId {
    if let Some(folded) = fold_atoms(arena, ClvmOp::Ash, &[value, shift]) {
        return folded;
    }

    arena.alloc(Lir::Ash(value, shift))
}

pub fn opt_lsh(arena: &mut Arena<Lir>, value: LirId, shift: LirId) -> LirId {
    if let Some(folded) = fold_atoms(arena, ClvmOp::Lsh, &[value, shift]) {
        return folded;
    }

    arena.alloc(Lir::Lsh(value, shift))
}

pub fn opt_pubkey_for_exp(arena: &mut Arena<Lir>, exp: LirId) -> LirId {
    if let Some(folded) = fold_atoms(arena, ClvmOp::PubkeyForExp, &[exp]) {
        return folded;
    }

    arena.alloc(Lir::PubkeyForExp(exp))
}

pub fn opt_g1_add(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    if let Some(folded) = fold_atoms(arena, ClvmOp::G1Add, &args) {
        return folded;
    }

    arena.alloc(Lir::G1Add(args))
}

pub fn opt_g1_subtract(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    if let Some(folded) = fold_atoms(arena, ClvmOp::G1Subtract, &args) {
        return folded;
    }

    arena.alloc(Lir::G1Subtract(args))
}

pub fn opt_g1_multiply(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    if let Some(folded) = fold_atoms(arena, ClvmOp::G1Multiply, &[left, right]) {
        return folded;
    }

    arena.alloc(Lir::G1Multiply(left, right))
}

pub fn opt_g1_negate(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    if let Some(folded) = fold_atoms(arena, ClvmOp::G1Negate, &[value]) {
        return folded;
    }

    arena.alloc(Lir::G1Negate(value))
}

pub fn opt_g1_map(arena: &mut Arena<Lir>, value: LirId, map: Option<LirId>) -> LirId {
    let args: Vec<LirId> = [value].into_iter().chain(map).collect();

    if let Some(folded) = fold_atoms(arena, ClvmOp::G1Map, &args) {
        return folded;
    }

    arena.alloc(Lir::G1Map(value, map))
}

pub fn opt_g2_add(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    if let Some(folded) = fold_atoms(arena, ClvmOp::G2Add, &args) {
        return folded;
    }

    arena.alloc(Lir::G2Add(args))
}

pub fn opt_g2_subtract(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    if let Some(folded) = fold_atoms(arena, ClvmOp::G2Subtract, &args) {
        return folded;
    }

    arena.alloc(Lir::G2Subtract(args))
}

pub fn opt_g2_multiply(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    if let Some(folded) = fold_atoms(arena, ClvmOp::G2Multiply, &[left, right]) {
        return folded;
    }

    arena.alloc(Lir::G2Multiply(left, right))
}

pub fn opt_g2_negate(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    if let Some(folded) = fold_atoms(arena, ClvmOp::G2Negate, &[value]) {
        return folded;
    }

    arena.alloc(Lir::G2Negate(value))
}

pub fn opt_g2_map(arena: &mut Arena<Lir>, value: LirId, map: Option<LirId>) -> LirId {
    let args: Vec<LirId> = [value].into_iter().chain(map).collect();

    if let Some(folded) = fold_atoms(arena, ClvmOp::G2Map, &args) {
        return folded;
    }

    arena.alloc(Lir::G2Map(value, map))
}

pub fn opt_bls_pairing_identity(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    if let Some(folded) = fold_atoms(arena, ClvmOp::BlsPairingIdentity, &args) {
        return folded;
    }

    arena.alloc(Lir::BlsPairingIdentity(args))
}

pub fn opt_bls_verify(arena: &mut Arena<Lir>, sig: LirId, args: Vec<LirId>) -> LirId {
    let fold_args: Vec<LirId> = [sig].into_iter().chain(args.iter().copied()).collect();

    if let Some(folded) = fold_atoms(arena, ClvmOp::BlsVerify, &fold_args) {
        return folded;
    }

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
    if let Some(folded) = fold_atoms(
        arena,
        ClvmOp::CoinId,
        &[parent_coin_info, puzzle_hash, amount],
    ) {
        return folded;
    }

    arena.alloc(Lir::CoinId(parent_coin_info, puzzle_hash, amount))
}

pub fn opt_k1_verify(
    arena: &mut Arena<Lir>,
    public_key: LirId,
    message: LirId,
    signature: LirId,
) -> LirId {
    if let Some(folded) = fold_atoms(
        arena,
        ClvmOp::Secp256K1Verify,
        &[public_key, message, signature],
    ) {
        return folded;
    }

    arena.alloc(Lir::K1Verify(public_key, message, signature))
}

pub fn opt_r1_verify(
    arena: &mut Arena<Lir>,
    public_key: LirId,
    message: LirId,
    signature: LirId,
) -> LirId {
    if let Some(folded) = fold_atoms(
        arena,
        ClvmOp::Secp256R1Verify,
        &[public_key, message, signature],
    ) {
        return folded;
    }

    arena.alloc(Lir::R1Verify(public_key, message, signature))
}

pub fn opt_op(arena: &mut Arena<Lir>, op: ClvmOp, arg: LirId) -> LirId {
    arena.alloc(Lir::Op(op, arg))
}

pub fn opt_debug_print(arena: &mut Arena<Lir>, srcloc: String, value: LirId) -> LirId {
    arena.alloc(Lir::DebugPrint(srcloc, value))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mul_by_one_keeps_mul() {
        let mut arena = Arena::new();
        let value = arena.alloc(Lir::Path(BigUint::from(2u8)));
        let one = arena.alloc(Lir::Atom(vec![1]));
        let result = opt_mul(&mut arena, vec![value, one]);
        assert!(matches!(&arena[result], Lir::Mul(args) if *args == [value]));
    }

    #[test]
    fn test_mul_constants_folds() {
        let mut arena = Arena::new();
        let two = arena.alloc(Lir::Atom(vec![2]));
        let three = arena.alloc(Lir::Atom(vec![3]));
        let result = opt_mul(&mut arena, vec![two, three]);
        assert!(matches!(&arena[result], Lir::Atom(atom) if *atom == [6]));
    }

    fn path(arena: &mut Arena<Lir>, path: u32) -> LirId {
        arena.alloc(Lir::Path(BigUint::from(path)))
    }

    #[test]
    fn test_run_nil_env_keeps_env_reads() {
        let mut arena = Arena::new();
        let value = path(&mut arena, 1);
        let callee = arena.alloc(Lir::Quote(value));
        let nil = arena.alloc(Lir::Atom(vec![]));
        let result = opt_run(&mut arena, callee, nil);
        assert!(matches!(arena[result], Lir::Run(c, e) if c == callee && e == nil));
    }

    #[test]
    fn test_run_nil_env_keeps_implicit_env_reads() {
        // `(a (q . (a (q . 2) 1)) ())` reads path 2 of the nil environment, which fails
        let mut arena = Arena::new();
        let inner = path(&mut arena, 2);
        let inner = arena.alloc(Lir::Quote(inner));
        let one = path(&mut arena, 1);
        let value = arena.alloc(Lir::Run(inner, one));
        let callee = arena.alloc(Lir::Quote(value));
        let nil = arena.alloc(Lir::Atom(vec![]));
        let result = opt_run(&mut arena, callee, nil);
        assert!(matches!(arena[result], Lir::Run(..)));
    }

    #[test]
    fn test_run_nil_env_without_env_reads() {
        let mut arena = Arena::new();
        let quoted = path(&mut arena, 2);
        let quoted = arena.alloc(Lir::Quote(quoted));
        let one = arena.alloc(Lir::Atom(vec![1]));
        let value = arena.alloc(Lir::Cons(quoted, one));
        let callee = arena.alloc(Lir::Quote(value));
        let nil = arena.alloc(Lir::Atom(vec![]));
        let result = opt_run(&mut arena, callee, nil);
        assert_eq!(result, value);
    }

    #[test]
    fn test_run_same_env() {
        let mut arena = Arena::new();
        let value = path(&mut arena, 2);
        let callee = arena.alloc(Lir::Quote(value));
        let env = path(&mut arena, 1);
        let result = opt_run(&mut arena, callee, env);
        assert_eq!(result, value);
    }

    #[test]
    fn test_cons_of_first_and_rest_is_kept() {
        // `(c 2 3)` fails if the environment is an atom, but `1` wouldn't
        let mut arena = Arena::new();
        let first = path(&mut arena, 2);
        let rest = path(&mut arena, 3);
        let result = opt_cons(&mut arena, first, rest);
        assert!(matches!(arena[result], Lir::Cons(f, r) if f == first && r == rest));
    }

    #[test]
    fn test_closure_without_parameters_keeps_env_reads() {
        // `(a (q . F) ())` runs `F` in a nil environment, so it can't be replaced with `F`
        let mut arena = Arena::new();
        let body = path(&mut arena, 2);
        let callee = arena.alloc(Lir::Quote(body));
        let result = opt_closure(&mut arena, callee, vec![], false);
        assert!(matches!(arena[result], Lir::Closure(..)));

        let result = opt_closure(&mut arena, callee, vec![], true);
        assert_eq!(result, callee);

        let body = arena.alloc(Lir::Atom(vec![42]));
        let callee = arena.alloc(Lir::Quote(body));
        let result = opt_closure(&mut arena, callee, vec![], false);
        assert_eq!(result, callee);
    }

    #[test]
    fn test_first_and_rest_of_path_zero() {
        // Path 0 evaluates to nil, so `(f ())` fails rather than reading path 2
        let mut arena = Arena::new();
        let zero = path(&mut arena, 0);
        let result = opt_first(&mut arena, zero);
        assert!(matches!(arena[result], Lir::First(_)));
        let result = opt_rest(&mut arena, zero);
        assert!(matches!(arena[result], Lir::Rest(_)));

        let five = path(&mut arena, 5);
        let result = opt_first(&mut arena, five);
        assert!(matches!(&arena[result], Lir::Path(p) if *p == BigUint::from(9u8)));
    }
}
