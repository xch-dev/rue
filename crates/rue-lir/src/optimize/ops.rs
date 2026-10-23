use id_arena::Arena;
use num_bigint::{BigInt, BigUint, Sign};
use num_integer::Integer;
use sha2::{Digest, Sha256};
use sha3::Keccak256;

use crate::{
    ClvmOp, Lir, LirId, atom_bigint, bigint_atom, first_path,
    optimize::{ArgList, fold_atoms, opt_truthy},
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
// If there's a nested subtraction, it's flattened along with the sum by `opt_sub`
pub fn opt_add(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    if args.iter().any(|&arg| matches!(arena[arg], Lir::Sub(_))) {
        let sum = arena.alloc(Lir::Add(args));
        return opt_sub(arena, vec![sum]);
    }

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

// Flatten nested additions and subtractions into a list of positive and negative
// terms, folding constants together, then subtract the negative terms from the sum
// of the positive terms
//
// Every term carries its own sign, since a nested subtraction in a negative position
// (e.g. `a - (b - c)`) flips the signs of all of its operands
pub fn opt_sub(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    let mut constant = BigInt::from(0);
    let mut positive = Vec::new();
    let mut negative = Vec::new();

    // This is a stack, so the arguments are pushed in reverse to preserve their order
    let mut stack: Vec<(LirId, bool)> = Vec::new();

    for (i, arg) in args.into_iter().enumerate().rev() {
        stack.push((arg, i == 0));
    }

    while let Some((arg, is_positive)) = stack.pop() {
        match arena[arg].clone() {
            Lir::Atom(atom) => {
                if is_positive {
                    constant += atom_bigint(atom);
                } else {
                    constant -= atom_bigint(atom);
                }
            }
            Lir::Add(items) => {
                for item in items.into_iter().rev() {
                    stack.push((item, is_positive));
                }
            }
            Lir::Sub(items) => {
                // The first item keeps the sign of the subtraction, the rest are flipped
                for (i, item) in items.into_iter().enumerate().rev() {
                    stack.push((item, if i == 0 { is_positive } else { !is_positive }));
                }
            }
            _ => {
                if is_positive {
                    positive.push(arg);
                } else {
                    negative.push(arg);
                }
            }
        }
    }

    if positive.is_empty() && negative.is_empty() {
        return arena.alloc(Lir::Atom(bigint_atom(constant)));
    }

    // If there's nothing else to subtract from, the constant (even if it's negative) is the minuend
    // Otherwise, the constant goes last, so that optimizing the output again is stable
    if constant > BigInt::from(0)
        || (positive.is_empty() && !negative.is_empty() && constant != BigInt::from(0))
    {
        positive.push(arena.alloc(Lir::Atom(bigint_atom(constant))));
    } else if constant < BigInt::from(0) {
        negative.push(arena.alloc(Lir::Atom(bigint_atom(-constant))));
    }

    // A lone non-constant operand is kept as `(+ x)`, since addition
    // normalizes non-canonical integer atoms (e.g. `0x0001` to `1`)
    if negative.is_empty() {
        return arena.alloc(Lir::Add(positive));
    }

    let minuend = match positive.len() {
        0 => arena.alloc(Lir::Atom(vec![])),
        1 => positive[0],
        _ => arena.alloc(Lir::Add(positive)),
    };

    arena.alloc(Lir::Sub([vec![minuend], negative].concat()))
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

// If the value is `(% x 2)`, the result is canonical `0` or `1`
// Since `=` compares bytes, this only matches the canonical encoding of `1`
fn is_odd_check(arena: &Arena<Lir>, value: LirId, one: LirId) -> Option<LirId> {
    if let Lir::Mod(lhs, rhs) = arena[value].clone()
        && let Lir::Atom(rhs) = arena[rhs].clone()
        && atom_bigint(rhs) == BigInt::from(2)
        && let Lir::Atom(one) = &arena[one]
        && one.as_slice() == [1]
    {
        return Some(lhs);
    }

    None
}

pub fn opt_eq(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> LirId {
    if let Some(value) = is_odd_check(arena, left, right) {
        return opt_logand(arena, vec![value, right]);
    }

    if let Some(value) = is_odd_check(arena, right, left) {
        return opt_logand(arena, vec![value, left]);
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
    use clvmr::{Allocator, ChiaDialect, NodePtr, run_program, serde::node_to_bytes};

    use rue_clvm::disassemble_program;

    use crate::{CLVM_FLAGS, CodegenOptions, codegen, optimize};

    use super::*;

    fn atom(arena: &mut Arena<Lir>, value: i64) -> LirId {
        arena.alloc(Lir::Atom(bigint_atom(BigInt::from(value))))
    }

    fn path(arena: &mut Arena<Lir>, value: u32) -> LirId {
        arena.alloc(Lir::Path(BigUint::from(value)))
    }

    fn run(allocator: &mut Allocator, program: NodePtr, env: NodePtr) -> Option<Vec<u8>> {
        let dialect = ChiaDialect::new(CLVM_FLAGS);
        let result = run_program(allocator, &dialect, program, env, 11_000_000_000).ok()?;
        Some(node_to_bytes(allocator, result.1).unwrap())
    }

    fn compile(
        arena: &Arena<Lir>,
        allocator: &mut Allocator,
        lir: LirId,
        optimize: bool,
    ) -> NodePtr {
        codegen(
            arena,
            allocator,
            lir,
            CodegenOptions {
                optimize_static_pairs: optimize,
            },
        )
        .unwrap()
    }

    fn disassemble(arena: &Arena<Lir>, lir: LirId) -> String {
        let mut allocator = Allocator::new();
        let program = compile(arena, &mut allocator, lir, true);
        disassemble_program(&allocator, program)
    }

    fn assert_equivalent(arena: &mut Arena<Lir>, lir: LirId, envs: &[Vec<Vec<u8>>]) -> LirId {
        let optimized = optimize(arena, lir);

        let mut allocator = Allocator::new();
        let unoptimized_program = compile(arena, &mut allocator, lir, false);
        let optimized_program = compile(arena, &mut allocator, optimized, true);

        for env in envs {
            let mut env_ptr = NodePtr::NIL;

            for value in env.iter().rev() {
                let value = allocator.new_atom(value).unwrap();
                env_ptr = allocator.new_pair(value, env_ptr).unwrap();
            }

            assert_eq!(
                run(&mut allocator, unoptimized_program, env_ptr),
                run(&mut allocator, optimized_program, env_ptr),
                "optimized program differs for environment {env:?}"
            );
        }

        optimized
    }

    fn int_env(values: &[i64]) -> Vec<Vec<u8>> {
        values
            .iter()
            .map(|value| bigint_atom(BigInt::from(*value)))
            .collect()
    }

    // a - b + c - 1
    #[test]
    fn test_sub_inside_add_inside_sub() {
        let mut arena = Arena::new();
        let a = path(&mut arena, 2);
        let b = path(&mut arena, 5);
        let c = path(&mut arena, 11);
        let one = atom(&mut arena, 1);
        let a_minus_b = arena.alloc(Lir::Sub(vec![a, b]));
        let plus_c = arena.alloc(Lir::Add(vec![a_minus_b, c]));
        let lir = arena.alloc(Lir::Sub(vec![plus_c, one]));

        let optimized = assert_equivalent(&mut arena, lir, &[int_env(&[100, 10, 1])]);
        assert_eq!(disassemble(&arena, optimized), "(- (+ 2 11) 5 (q . 1))");
    }

    // -fee + amount - spent
    #[test]
    fn test_negation_inside_add_inside_sub() {
        let mut arena = Arena::new();
        let fee = path(&mut arena, 2);
        let amount = path(&mut arena, 5);
        let spent = path(&mut arena, 11);
        let nil = arena.alloc(Lir::Atom(vec![]));
        let neg_fee = arena.alloc(Lir::Sub(vec![nil, fee]));
        let plus_amount = arena.alloc(Lir::Add(vec![neg_fee, amount]));
        let lir = arena.alloc(Lir::Sub(vec![plus_amount, spent]));

        assert_equivalent(&mut arena, lir, &[int_env(&[10, 1000, 100])]);
    }

    // a - (b - (c - d))
    #[test]
    fn test_sub_nested_in_negative_position() {
        let mut arena = Arena::new();
        let a = path(&mut arena, 2);
        let b = path(&mut arena, 5);
        let c = path(&mut arena, 11);
        let d = path(&mut arena, 23);
        let c_minus_d = arena.alloc(Lir::Sub(vec![c, d]));
        let b_minus = arena.alloc(Lir::Sub(vec![b, c_minus_d]));
        let lir = arena.alloc(Lir::Sub(vec![a, b_minus]));

        let optimized = assert_equivalent(&mut arena, lir, &[int_env(&[1000, 100, 10, 1])]);
        assert_eq!(disassemble(&arena, optimized), "(- (+ 2 11) 5 23)");
    }

    #[test]
    fn test_sub_constants_fold() {
        let mut arena = Arena::new();
        let ten = atom(&mut arena, 10);
        let twenty = atom(&mut arena, 20);
        let thirty = atom(&mut arena, 30);
        let one = atom(&mut arena, 1);
        let inner = arena.alloc(Lir::Sub(vec![ten, twenty]));
        let add = arena.alloc(Lir::Add(vec![inner, thirty]));
        let lir = arena.alloc(Lir::Sub(vec![add, one]));
        let result = opt_sub(&mut arena, vec![lir]);
        assert!(matches!(&arena[result], Lir::Atom(atom) if *atom == [19]));
    }

    // 100 - x - 300
    #[test]
    fn test_sub_negative_constant_is_minuend() {
        let mut arena = Arena::new();
        let hundred = atom(&mut arena, 100);
        let value = path(&mut arena, 2);
        let three_hundred = atom(&mut arena, 300);
        let lir = arena.alloc(Lir::Sub(vec![hundred, value, three_hundred]));

        let optimized = assert_equivalent(&mut arena, lir, &[int_env(&[200])]);
        assert_eq!(disassemble(&arena, optimized), "(- (q . -200) 2)");
    }

    // (a - b) + (c - 1)
    #[test]
    fn test_add_flattens_sub() {
        let mut arena = Arena::new();
        let a = path(&mut arena, 2);
        let b = path(&mut arena, 5);
        let c = path(&mut arena, 11);
        let one = atom(&mut arena, 1);
        let a_minus_b = arena.alloc(Lir::Sub(vec![a, b]));
        let c_minus_one = arena.alloc(Lir::Sub(vec![c, one]));
        let lir = arena.alloc(Lir::Add(vec![a_minus_b, c_minus_one]));

        let optimized = assert_equivalent(&mut arena, lir, &[int_env(&[100, 10, 1])]);
        assert_eq!(disassemble(&arena, optimized), "(- (+ 2 11) 5 (q . 1))");
    }

    #[test]
    fn test_sub_lone_operand_keeps_add() {
        let mut arena = Arena::new();
        let value = path(&mut arena, 2);
        let zero = arena.alloc(Lir::Atom(vec![]));
        let result = opt_sub(&mut arena, vec![value, zero]);
        assert!(matches!(&arena[result], Lir::Add(args) if *args == [value]));
    }

    #[test]
    fn test_sub_is_stable() {
        let mut arena = Arena::new();
        let value = path(&mut arena, 2);
        let other = path(&mut arena, 5);

        for constant in [-1, 1] {
            let constant = atom(&mut arena, constant);
            for args in [vec![value, constant], vec![value, other, constant]] {
                let lir = arena.alloc(Lir::Sub(args));
                let once = optimize(&mut arena, lir);
                let twice = optimize(&mut arena, once);

                let mut allocator = Allocator::new();
                let once = compile(&arena, &mut allocator, once, true);
                let twice = compile(&arena, &mut allocator, twice, true);
                assert_eq!(
                    node_to_bytes(&allocator, once).unwrap(),
                    node_to_bytes(&allocator, twice).unwrap()
                );
            }
        }
    }

    #[test]
    fn test_odd_check_requires_canonical_one() {
        let mut arena = Arena::new();
        let value = path(&mut arena, 2);
        let two = atom(&mut arena, 2);
        let one = atom(&mut arena, 1);
        let non_canonical_one = arena.alloc(Lir::Atom(vec![0, 1]));

        let modulo = arena.alloc(Lir::Mod(value, two));
        let result = opt_eq(&mut arena, modulo, one);
        assert_eq!(disassemble(&arena, result), "(logand 2 (q . 1))");

        let result = opt_eq(&mut arena, one, modulo);
        assert_eq!(disassemble(&arena, result), "(logand 2 (q . 1))");

        let result = opt_eq(&mut arena, modulo, non_canonical_one);
        assert_eq!(
            disassemble(&arena, result),
            "(= (% 2 (q . 2)) (q . 0x0001))"
        );

        let lir = arena.alloc(Lir::Eq(modulo, non_canonical_one));
        assert_equivalent(&mut arena, lir, &[int_env(&[3]), int_env(&[4])]);
    }

    struct Rng(u64);

    impl Rng {
        fn next(&mut self) -> u64 {
            self.0 ^= self.0 << 13;
            self.0 ^= self.0 >> 7;
            self.0 ^= self.0 << 17;
            self.0
        }

        fn below(&mut self, n: usize) -> usize {
            (self.next() % n as u64) as usize
        }
    }

    const ATOMS: &[&[u8]] = &[
        &[],
        &[1],
        &[2],
        &[0x00],
        &[0x00, 0x01],
        &[0xff],
        &[0x80],
        &[7],
        &[0x01, 0x00],
    ];

    fn random_arith(arena: &mut Arena<Lir>, rng: &mut Rng, depth: u32) -> LirId {
        if depth == 0 || rng.below(4) == 0 {
            return match rng.below(3) {
                0 => arena.alloc(Lir::Atom(ATOMS[rng.below(ATOMS.len())].to_vec())),
                // Path 7 is a pair in the test environments, so operations on it fail
                _ => path(arena, [2, 5, 11, 7][rng.below(4)]),
            };
        }

        let len = rng.below(4);
        let args = (0..len)
            .map(|_| random_arith(arena, rng, depth - 1))
            .collect();

        match rng.below(5) {
            0 | 1 => arena.alloc(Lir::Add(args)),
            2 | 3 => arena.alloc(Lir::Sub(args)),
            _ => {
                let value = random_arith(arena, rng, depth - 1);
                let two = atom(arena, 2);
                let one = arena.alloc(Lir::Atom(ATOMS[rng.below(ATOMS.len())].to_vec()));
                let modulo = arena.alloc(Lir::Mod(value, two));
                if rng.below(2) == 0 {
                    arena.alloc(Lir::Eq(modulo, one))
                } else {
                    arena.alloc(Lir::Eq(one, modulo))
                }
            }
        }
    }

    #[test]
    fn test_random_arithmetic_equivalence() {
        let mut rng = Rng(0x9e37_79b9_7f4a_7c15);

        for _ in 0..5000 {
            let mut arena = Arena::new();
            let lir = random_arith(&mut arena, &mut rng, 5);

            let envs: Vec<Vec<Vec<u8>>> = (0..6)
                .map(|_| {
                    (0..3)
                        .map(|_| ATOMS[rng.below(ATOMS.len())].to_vec())
                        .collect()
                })
                .collect();

            let optimized = assert_equivalent(&mut arena, lir, &envs);

            // Optimizing a second time shouldn't change the output
            let twice = optimize(&mut arena, optimized);
            let mut allocator = Allocator::new();
            let once = compile(&arena, &mut allocator, optimized, true);
            let twice = compile(&arena, &mut allocator, twice, true);
            assert_eq!(
                node_to_bytes(&allocator, once).unwrap(),
                node_to_bytes(&allocator, twice).unwrap()
            );
        }
    }

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
