use std::num::Saturating;

use id_arena::Arena;
use num_bigint::{BigInt, BigUint, Sign};
use num_integer::Integer;
use sha2::{Digest, Sha256};
use sha3::Keccak256;

use crate::{
    ClvmOp, Lir, LirId, atom_bigint, bigint_atom, first_path,
    optimize::{ArgList, Truthiness, fold_atoms, is_boolean, opt_truthy},
    parent_path, rest_path,
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
// We can also skip quoting if the program has no path, since it's not going to rely on the environment
pub fn opt_run(arena: &mut Arena<Lir>, callee: LirId, env: LirId) -> LirId {
    if let Lir::Quote(value) = arena[callee].clone() {
        if let Lir::Path(path) = arena[env].clone()
            && path == BigUint::from(1u8)
        {
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
pub fn opt_first(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    match arena[value].clone() {
        Lir::Path(path) => arena.alloc(Lir::Path(first_path(&path))),
        Lir::Divmod(left, right) => opt_div(arena, left, right),
        _ => arena.alloc(Lir::First(value)),
    }
}

// If the value is a path, we can optimize it to a rest path
// If the value is a cons, we can extract the rest element out of it
// If the value is a divmod, we can optimize it to a remainder since that's the rest output
pub fn opt_rest(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    match arena[value].clone() {
        Lir::Path(path) => arena.alloc(Lir::Path(rest_path(&path))),
        Lir::Divmod(left, right) => opt_mod(arena, left, right),
        _ => arena.alloc(Lir::Rest(value)),
    }
}

// TODO: Optimize pair of div and mod into divmod
pub fn opt_cons(arena: &mut Arena<Lir>, first: LirId, rest: LirId) -> LirId {
    if let Lir::Path(f_path) = arena[first].clone()
        && let Lir::Path(r_path) = arena[rest].clone()
        && let Some(parent) = parent_path(&f_path)
        && parent_path(&r_path).as_ref() == Some(&parent)
        && first_path(&parent) == f_path
        && rest_path(&parent) == r_path
    {
        return arena.alloc(Lir::Path(parent));
    }

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

// If the value's truthiness is known, we know the result
// Since not only looks at the truthiness of its operand, we can simplify the operand
// We can only unwrap a double not if the inner value is already () or 1, since otherwise
// the value would change (for example, (not (not 5)) is 1 rather than 5)
pub fn opt_not(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    match opt_truthy(arena, value) {
        Truthiness::Known(true) => arena.alloc(Lir::Atom(vec![])),
        Truthiness::Known(false) => arena.alloc(Lir::Atom(vec![1])),
        Truthiness::Unknown(value) => {
            if let Lir::Not(inner) = arena[value].clone()
                && is_boolean(arena, inner)
            {
                return inner;
            }

            arena.alloc(Lir::Not(value))
        }
    }
}

// If one of the arguments is known to be true and can't fail, we can ignore it
// If one of the arguments is known to be false, we know the result is false,
// but the other arguments must still be evaluated in case they fail
// Since all only looks at the truthiness of its arguments, we can simplify them
// TODO: Collapse nested alls
pub fn opt_all(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    let mut result = Vec::new();
    let mut has_false = false;

    for arg in args {
        match opt_truthy(arena, arg) {
            Truthiness::Known(true) => {}
            Truthiness::Known(false) => {
                if !has_false {
                    has_false = true;
                    result.push(arg);
                }
            }
            Truthiness::Unknown(arg) => {
                result.push(arg);
            }
        }
    }

    arena.alloc(Lir::All(result))
}

// If one of the arguments is known to be false and can't fail, we can ignore it
// If one of the arguments is known to be true, we know the result is true,
// but the other arguments must still be evaluated in case they fail
// Since any only looks at the truthiness of its arguments, we can simplify them
// TODO: Collapse nested anys
pub fn opt_any(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    let mut result = Vec::new();
    let mut has_true = false;

    for arg in args {
        match opt_truthy(arena, arg) {
            Truthiness::Known(false) => {}
            Truthiness::Known(true) => {
                if !has_true {
                    has_true = true;
                    result.push(arg);
                }
            }
            Truthiness::Unknown(arg) => {
                result.push(arg);
            }
        }
    }

    arena.alloc(Lir::Any(result))
}

// If the condition is known to be true or false and can't fail, we can return that branch
// Inline ifs evaluate both branches eagerly, so we keep them but with a nil condition
// Since the condition only matters for its truthiness, we can simplify it
// If the condition is a not, we can flip the then and else branches
pub fn opt_if(
    arena: &mut Arena<Lir>,
    condition: LirId,
    then: LirId,
    otherwise: LirId,
    inline: bool,
) -> LirId {
    match opt_truthy(arena, condition) {
        Truthiness::Known(true) if !inline => then,
        Truthiness::Known(false) if !inline => otherwise,
        Truthiness::Known(condition) => {
            let nil = arena.alloc(Lir::Atom(vec![]));

            if condition {
                arena.alloc(Lir::If(nil, otherwise, then, inline))
            } else {
                arena.alloc(Lir::If(nil, then, otherwise, inline))
            }
        }
        Truthiness::Unknown(condition) => {
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
}
