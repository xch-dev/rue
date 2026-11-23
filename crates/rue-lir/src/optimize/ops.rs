use std::mem;

use id_arena::Arena;
use num_bigint::{BigInt, BigUint, Sign};
use num_integer::Integer;
use sha2::{Digest, Sha256};
use sha3::Keccak256;

use crate::{
    ClvmOp, ClvmTarget, Lir, LirId, MAX_DIVIDEND_BYTES, MAX_DIVISOR_BYTES, MAX_MUL_OPERAND_BYTES,
    atom_bigint, bigint_atom, first_path,
    optimize::{
        ArgList, Truthiness, fold_atoms, is_atom, is_boolean, is_nil, is_pure, opt_truthy,
        reads_env,
    },
    parent_path, path_to_atom, rest_path,
    stack::grow_stack,
};

// There's no way to optimize an atom
pub fn opt_atom(arena: &mut Arena<Lir>, atom: Vec<u8>) -> LirId {
    arena.alloc(Lir::Atom(atom))
}

// There's no way to optimize a path
pub fn opt_path(arena: &mut Arena<Lir>, path: BigUint) -> LirId {
    arena.alloc(Lir::Path(path))
}

/// Runs a node that's generated as a call to its operator (see [`Lir::operator`]) at compile time
/// if its operands are all constants (see [`fold_atoms`]), or keeps it as it is.
pub fn opt_fold(arena: &mut Arena<Lir>, target: ClvmTarget, node: Lir) -> LirId {
    let op = node
        .operator()
        .expect("only nodes that are calls to their operator are folded");
    let args: Vec<LirId> = node.children().collect();

    if let Some(folded) = fold_atoms(arena, target, op, &args) {
        return folded;
    }

    arena.alloc(node)
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
// If the value is a cons, we can extract the first element out of it, as long as the rest can't fail
// If the value is a divmod, we can optimize it to a div since that's the first output
pub fn opt_first(arena: &mut Arena<Lir>, target: ClvmTarget, value: LirId) -> LirId {
    match arena[value].clone() {
        Lir::Path(path) if path != BigUint::ZERO => arena.alloc(Lir::Path(first_path(&path))),
        Lir::Cons(first, rest) if is_pure(arena, rest) => first,
        Lir::Divmod(left, right) => opt_div(arena, target, left, right),
        _ => arena.alloc(Lir::First(value)),
    }
}

// If the value is a path, we can optimize it to a rest path, since both fail if it's an atom
// Path 0 is excluded, since it evaluates to nil rather than reading the environment
// If the value is a cons, we can extract the rest element out of it, as long as the first can't fail
// If the value is a divmod, we can optimize it to a remainder since that's the rest output
pub fn opt_rest(arena: &mut Arena<Lir>, target: ClvmTarget, value: LirId) -> LirId {
    match arena[value].clone() {
        Lir::Path(path) if path != BigUint::ZERO => arena.alloc(Lir::Path(rest_path(&path))),
        Lir::Cons(first, rest) if is_pure(arena, first) => rest,
        Lir::Divmod(left, right) => opt_mod(arena, target, left, right),
        _ => arena.alloc(Lir::Rest(value)),
    }
}

// A pair of the quotient and remainder of the same operands is a divmod, which fails in the same
// cases (division by zero, or an operand that's a pair)
// If the first and rest are paths to the first and rest of the same parent, we can use the parent path
// This assumes that the value at the parent path is a pair, as its type says it is. If it's actually
// an atom (e.g. from a solution which doesn't match the declared types), the original would fail,
// but the parent path wouldn't. That failure was never a check: types are assumptions about the
// solution, as in Chialisp, and aren't checked at runtime unless the program checks them with a
// type guard. So this doesn't give the spender anything that a wrongly shaped pair couldn't.
pub fn opt_cons(arena: &mut Arena<Lir>, first: LirId, rest: LirId) -> LirId {
    if let Lir::Div(div_left, div_right) = arena[first].clone()
        && let Lir::Mod(mod_left, mod_right) = arena[rest].clone()
        && same_value(arena, div_left, mod_left)
        && same_value(arena, div_right, mod_right)
    {
        return arena.alloc(Lir::Divmod(div_left, div_right));
    }

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
// If the value is a pair that can't fail, we know the result too
pub fn opt_listp(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    match arena[value].clone() {
        Lir::Atom(_) => arena.alloc(Lir::Atom(vec![])),
        Lir::Cons(..) if is_pure(arena, value) => arena.alloc(Lir::Atom(vec![1])),
        _ => arena.alloc(Lir::Listp(value)),
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

// Without `operand_limits`, every constant operand is folded into one product, and nested products
// are flattened, since multiplication only fails for an operand that's a pair
//
// With it, each run of adjacent constant operands is folded into its product. `LIMITS` checks the
// length of each operand and of the running product after each one (see `MAX_MUL_OPERAND_BYTES`),
// so this doesn't flatten nested products, whose inner product is checked as an operand, or move
// constants, which would change the running products when an operand is zero at runtime.
//
// A run is only folded if each of its operands and its product fit in an operand, and none of them
// are zero. Then each running product inside the run is at most the one after it, so folding it
// doesn't change whether the product fails. A product of one is left out, since it doesn't change
// the running products either.
pub fn opt_mul(arena: &mut Arena<Lir>, target: ClvmTarget, args: Vec<LirId>) -> LirId {
    if !target.limits_operands() {
        return opt_unlimited_mul(arena, args);
    }

    let mut result = Vec::new();
    let mut run = Vec::new();
    let mut folded = false;

    for arg in args {
        if matches!(arena[arg], Lir::Atom(_)) {
            run.push(arg);
        } else {
            folded |= fold_mul_run(arena, &mut run, &mut result);
            result.push(arg);
        }
    }

    folded |= fold_mul_run(arena, &mut run, &mut result);

    if result.is_empty() {
        return arena.alloc(Lir::Atom(vec![1]));
    }

    // A lone product of constants is used directly, but anything else alone is kept as `(* x)`,
    // since multiplication normalizes non-canonical integer atoms (e.g. `0x0001` to `1`), and
    // fails for an operand that's too long
    if folded && result.len() == 1 && matches!(arena[result[0]], Lir::Atom(_)) {
        return result[0];
    }

    arena.alloc(Lir::Mul(result))
}

/// Folds every constant operand into one product, and flattens nested products.
fn opt_unlimited_mul(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
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

    // A lone non-constant operand is kept as `(* x)`, since multiplication normalizes
    // non-canonical integer atoms (e.g. `0x0001` to `1`)
    if result.len() == 1 && matches!(arena[result[0]], Lir::Atom(_)) {
        return result[0];
    }

    arena.alloc(Lir::Mul(result))
}

/// Adds the product of a run of constant operands to the result if it can be folded, or else the
/// operands themselves, and returns whether it was folded.
fn fold_mul_run(arena: &mut Arena<Lir>, run: &mut Vec<LirId>, result: &mut Vec<LirId>) -> bool {
    if run.is_empty() {
        return false;
    }

    let run = mem::take(run);
    let mut product = BigInt::from(1);

    for &arg in &run {
        let Lir::Atom(atom) = &arena[arg] else {
            unreachable!("a run only contains atoms");
        };

        let value = atom_bigint(atom.clone());

        if atom.len() > MAX_MUL_OPERAND_BYTES || value.sign() == Sign::NoSign {
            result.extend(run);
            return false;
        }

        product *= value;
    }

    let product = bigint_atom(product);

    if product.len() > MAX_MUL_OPERAND_BYTES {
        result.extend(run);
        return false;
    }

    if product != [1] {
        result.push(arena.alloc(Lir::Atom(product)));
    }

    true
}

/// Whether the operands of `/`, `%` or `divmod` are allowed, which they always are unless the
/// program is compiled with `operand_limits`.
fn division_within_limits(target: ClvmTarget, left: &[u8], right: &[u8]) -> bool {
    !target.limits_operands()
        || (left.len() <= MAX_DIVIDEND_BYTES && right.len() <= MAX_DIVISOR_BYTES)
}

// If both values are atoms, we can perform the division (as long as the right value is not zero,
// and `operand_limits` allows the operands)
pub fn opt_div(arena: &mut Arena<Lir>, target: ClvmTarget, left: LirId, right: LirId) -> LirId {
    if let Lir::Atom(left) = arena[left].clone()
        && let Lir::Atom(right) = arena[right].clone()
        && division_within_limits(target, &left, &right)
        && let left = atom_bigint(left)
        && let right = atom_bigint(right)
        && right.sign() != Sign::NoSign
    {
        return arena.alloc(Lir::Atom(bigint_atom(left.div_floor(&right))));
    }

    arena.alloc(Lir::Div(left, right))
}

// If both values are atoms, we can perform the division and remainder (as long as the right value is
// not zero, and `operand_limits` allows the operands)
pub fn opt_divmod(arena: &mut Arena<Lir>, target: ClvmTarget, left: LirId, right: LirId) -> LirId {
    if let Lir::Atom(left) = arena[left].clone()
        && let Lir::Atom(right) = arena[right].clone()
        && division_within_limits(target, &left, &right)
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

// If both values are atoms, we can perform the remainder (as long as the right value is not zero, and
// `operand_limits` allows the operands)
pub fn opt_mod(arena: &mut Arena<Lir>, target: ClvmTarget, left: LirId, right: LirId) -> LirId {
    if let Lir::Atom(left) = arena[left].clone()
        && let Lir::Atom(right) = arena[right].clone()
        && division_within_limits(target, &left, &right)
        && let left = atom_bigint(left)
        && let right = atom_bigint(right)
        && right.sign() != Sign::NoSign
    {
        return arena.alloc(Lir::Atom(bigint_atom(left.mod_floor(&right))));
    }

    arena.alloc(Lir::Mod(left, right))
}

// If the value is `(% x 2)`, the result is canonical `0` or `1`
// Since `=` compares bytes, this only matches the canonical encoding of `1`
// `logand` doesn't limit the length of its operands, so with `operand_limits`, where `%` fails for
// a dividend longer than `MAX_DIVIDEND_BYTES`, this only matches if `x` is known to be short enough
fn is_odd_check(arena: &Arena<Lir>, target: ClvmTarget, value: LirId, one: LirId) -> Option<LirId> {
    if let Lir::Mod(lhs, rhs) = arena[value].clone()
        && let Lir::Atom(rhs) = arena[rhs].clone()
        && atom_bigint(rhs) == BigInt::from(2)
        && let Lir::Atom(one) = &arena[one]
        && one.as_slice() == [1]
        && (!target.limits_operands() || fits_dividend(arena, lhs))
    {
        return Some(lhs);
    }

    None
}

// Whether the value is known to be no longer than `MAX_DIVIDEND_BYTES`, so that `LIMITS` allows it
// as a dividend. A short enough constant dividend is already folded by `opt_mod`, so the constant
// case is only defensive
fn fits_dividend(arena: &Arena<Lir>, value: LirId) -> bool {
    match &arena[value] {
        Lir::Atom(atom) => atom.len() <= MAX_DIVIDEND_BYTES,
        _ => is_boolean(arena, value),
    }
}

pub fn opt_eq(arena: &mut Arena<Lir>, target: ClvmTarget, left: LirId, right: LirId) -> LirId {
    if let Some(value) = is_odd_check(arena, target, left, right) {
        return opt_bitwise(arena, Bitwise::And, vec![value, right]);
    }

    if let Some(value) = is_odd_check(arena, target, right, left) {
        return opt_bitwise(arena, Bitwise::And, vec![value, left]);
    }

    opt_fold(arena, target, Lir::Eq(left, right))
}

// If the value's truthiness is known, we know the result
// Since not only looks at the truthiness of its operand, we can simplify the operand
// We can only unwrap a double not if the inner value is already () or 1, since otherwise
// the value would change (for example, (not (not 5)) is 1 rather than 5)
pub fn opt_not(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    match opt_truthy(arena, value) {
        Truthiness::Known(true) => arena.alloc(Lir::Atom(vec![])),
        Truthiness::Known(false) => arena.alloc(Lir::Atom(vec![1])),
        Truthiness::Unknown(replacement) => {
            if let Lir::Not(inner) = arena[replacement].clone() {
                if is_boolean(arena, inner) {
                    return inner;
                }

                // Avoid adding a second not if it can't be removed
                return arena.alloc(Lir::Not(value));
            }

            arena.alloc(Lir::Not(replacement))
        }
    }
}

// If one of the arguments is known to be true and can't fail, we can ignore it
// If one of the arguments is known to be false, we know the result is false,
// but the other arguments must still be evaluated in case they fail
// Since all only looks at the truthiness of its arguments, we can simplify them
// A nested all evaluates all of its arguments too, so they can be added to this one
pub fn opt_all(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    let mut args = ArgList::new(args);
    let mut result = Vec::new();
    let mut has_false = false;

    while let Some(arg) = args.next() {
        match opt_truthy(arena, arg) {
            Truthiness::Known(true) => {}
            Truthiness::Known(false) => {
                if !has_false {
                    has_false = true;
                    result.push(arg);
                }
            }
            Truthiness::Unknown(arg) => {
                if let Lir::All(items) = arena[arg].clone() {
                    args.prepend(items);
                } else {
                    result.push(arg);
                }
            }
        }
    }

    arena.alloc(Lir::All(result))
}

// If one of the arguments is known to be false and can't fail, we can ignore it
// If one of the arguments is known to be true, we know the result is true,
// but the other arguments must still be evaluated in case they fail
// Since any only looks at the truthiness of its arguments, we can simplify them
// A nested any evaluates all of its arguments too, so they can be added to this one
pub fn opt_any(arena: &mut Arena<Lir>, args: Vec<LirId>) -> LirId {
    let mut args = ArgList::new(args);
    let mut result = Vec::new();
    let mut has_true = false;

    while let Some(arg) = args.next() {
        match opt_truthy(arena, arg) {
            Truthiness::Known(false) => {}
            Truthiness::Known(true) => {
                if !has_true {
                    has_true = true;
                    result.push(arg);
                }
            }
            Truthiness::Unknown(arg) => {
                if let Lir::Any(items) = arena[arg].clone() {
                    args.prepend(items);
                } else {
                    result.push(arg);
                }
            }
        }
    }

    arena.alloc(Lir::Any(result))
}

// If the condition is known to be true or false and can't fail, we can return that branch
// Inline ifs evaluate both branches eagerly, so the other branch is only dropped if it can't fail,
// and otherwise both are kept with a nil condition
// Since the condition only matters for its truthiness, we can simplify it
// If the condition is a not, we can flip the then and else branches
// If both branches are atoms, they can't fail, so they can be evaluated eagerly instead
// `(i c 1 ())` is `c` if `c` is always `()` or `1`, and `(i c () 1)` is always `(not c)`
pub fn opt_if(
    arena: &mut Arena<Lir>,
    target: ClvmTarget,
    condition: LirId,
    then: LirId,
    otherwise: LirId,
    inline: bool,
) -> LirId {
    match opt_truthy(arena, condition) {
        Truthiness::Known(true) if !inline || is_pure(arena, otherwise) => then,
        Truthiness::Known(false) if !inline || is_pure(arena, then) => otherwise,
        Truthiness::Known(condition) => {
            let nil = arena.alloc(Lir::Atom(vec![]));

            if condition {
                arena.alloc(Lir::If(nil, otherwise, then, inline))
            } else {
                arena.alloc(Lir::If(nil, then, otherwise, inline))
            }
        }
        Truthiness::Unknown(replacement) => {
            // The replacement only has the same truthiness, so the original is used as a value
            if is_atom(arena, then, &[1])
                && is_nil(arena, otherwise)
                && is_boolean(arena, condition)
            {
                return condition;
            }

            if is_nil(arena, then) && is_atom(arena, otherwise, &[1]) {
                return opt_not(arena, condition);
            }

            // Turning an `if` into a bool is pushed into its branches when one of them already
            // is one, such as `(i (i a 1 b) 1 ())` from `a || b` to `(i a 1 (i b 1 ()))`, which
            // only turns the other branch into a bool
            if is_atom(arena, then, &[1])
                && is_nil(arena, otherwise)
                && let Lir::If(inner, inner_then, inner_otherwise, inner_inline) =
                    arena[condition].clone()
                && (is_boolean(arena, inner_then) || is_boolean(arena, inner_otherwise))
            {
                let inner_then = opt_if(arena, target, inner_then, then, otherwise, false);
                let inner_otherwise =
                    opt_if(arena, target, inner_otherwise, then, otherwise, false);
                return opt_if(
                    arena,
                    target,
                    inner,
                    inner_then,
                    inner_otherwise,
                    inner_inline,
                );
            }

            // An inline `if` evaluates both branches, so one that picks between a bool and a
            // constant is `any` or `all` of the condition and the bool, which is what `a || b`
            // and `a && b` are when `b` is evaluated either way
            if inline {
                if let Some(check) = nil_check(arena, replacement, then, otherwise, inline) {
                    return check;
                }

                if let Some(logic) = eager_logic(arena, replacement, then, otherwise) {
                    return logic;
                }
            }

            let (condition, then, otherwise) =
                if let Lir::Not(opposite) = arena[replacement].clone() {
                    (opposite, otherwise, then)
                } else {
                    (replacement, then, otherwise)
                };

            if !inline {
                if let Some(merged) = merge_checks(arena, target, condition, then, otherwise) {
                    return merged;
                }

                if let Some(direct) = branch_directly(arena, target, condition, then, otherwise) {
                    return direct;
                }
            }

            alloc_if(arena, condition, then, otherwise, inline)
        }
    }
}

/// `(not x)` for `(i (l x) () (not x))`, or for the same with `(= x ())` if the `if` isn't inline
/// (see [`alloc_if`]).
fn nil_check(
    arena: &mut Arena<Lir>,
    condition: LirId,
    then: LirId,
    otherwise: LirId,
    inline: bool,
) -> Option<LirId> {
    let Lir::Listp(value) = arena[condition] else {
        return None;
    };

    if !is_nil(arena, then) {
        return None;
    }

    let checked = match arena[otherwise] {
        Lir::Not(checked) => checked,
        Lir::Eq(left, right) if !inline && is_nil(arena, right) => left,
        Lir::Eq(left, right) if !inline && is_nil(arena, left) => right,
        _ => return None,
    };

    same_value(arena, value, checked).then(|| arena.alloc(Lir::Not(value)))
}

/// `(i c 1 b)` is `(any c b)` and `(i c b ())` is `(all c b)` if `b` is always `()` or `1`, since
/// every operand is evaluated in order either way, and `(i c () b)` and `(i c b 1)` are the same
/// with `(not c)`. The condition can be a replacement that only has the same truthiness.
fn eager_logic(
    arena: &mut Arena<Lir>,
    condition: LirId,
    then: LirId,
    otherwise: LirId,
) -> Option<LirId> {
    // An `if` between two constants is already as small as it gets
    let is_bool = |arena: &Arena<Lir>, value: LirId| {
        !matches!(arena[value], Lir::Atom(_)) && is_boolean(arena, value)
    };

    if is_atom(arena, then, &[1]) && is_bool(arena, otherwise) {
        return Some(opt_any(arena, vec![condition, otherwise]));
    }

    if is_nil(arena, otherwise) && is_bool(arena, then) {
        return Some(opt_all(arena, vec![condition, then]));
    }

    if is_nil(arena, then) && is_bool(arena, otherwise) {
        let negated = opt_not(arena, condition);
        return Some(opt_all(arena, vec![negated, otherwise]));
    }

    if is_atom(arena, otherwise, &[1]) && is_bool(arena, then) {
        let negated = opt_not(arena, condition);
        return Some(opt_any(arena, vec![negated, then]));
    }

    None
}

/// How many nodes a condition can have and still be checked along with the one before it (see
/// [`merge_checks`]). None of them is a call, so each is at most one operator, which costs a few
/// hundred to run on atoms the size of a hash, far less than the bytes that a check of its own
/// takes.
const MAX_MERGED_CHECK_SIZE: usize = 24;

/// A check that's only reached once another check has passed, and that fails the same way, is
/// checked along with it: `(a (i a (q 2 (i b (q . T) (q 8)) 1) (q 8)) 1)`, which is what two
/// asserts in a row are, becomes `(a (i (all a b) (q . T) (q 8)) 1)`, which is about 17 bytes
/// smaller. A check that passes when its condition is false is merged the same way, with the
/// condition negated, and so is one that branches to the same result in more than one place,
/// such as `(a (i b (q . T) (q 2 (i c (q . T) (q 8)) 1)) 1)` from an assert of `b || c` (see
/// [`pass_condition`]).
///
/// If the first condition fails the check, the program fails whether or not the rest is
/// evaluated first, and if it passes, the rest is evaluated either way. So this never changes
/// whether the program fails, even if the second condition relies on the first, as `(> x 5)`
/// does after a check that `x` is an atom. Two things could still be told apart, so they're
/// ruled out:
///
/// - A condition that isn't cheap, such as a call, would run before the program fails, when it
///   would have failed right away (see [`is_cheap_check`]).
/// - A condition with an operator that can fail for operands of the right types, such as a
///   division, would fail with its own error where the first check would have raised. A
///   condition that relies on the first one still can, for a value that fails that check.
///
/// The checks after such a condition are left as they are, and are the result of the ones that
/// are merged. The checks of a debug build raise with their own locations, so they're never the
/// same.
fn merge_checks(
    arena: &mut Arena<Lir>,
    target: ClvmTarget,
    condition: LirId,
    then: LirId,
    otherwise: LirId,
) -> Option<LirId> {
    let (passes, rest, failure) = match (&arena[then], &arena[otherwise]) {
        (_, Lir::Raise(_)) => (true, then, otherwise),
        (Lir::Raise(_), _) => (false, otherwise, then),
        _ => return None,
    };

    if !matches!(arena[rest], Lir::If(_, _, _, false)) {
        return None;
    }

    let mut result = None;
    let mut budget = MAX_MERGED_CHECK_SIZE;
    let inner = pass_condition(arena, target, rest, failure, &mut result, &mut budget)?;
    let result = result?;

    // Nothing in the rest fails the same way, so there's nothing to merge
    if matches!(arena[inner], Lir::Atom(_)) {
        return None;
    }

    // Checks that all pass when their conditions are false fail when any of them is true
    if !passes && let Lir::Not(inner) = arena[inner] {
        let any = opt_any(arena, vec![condition, inner]);
        return Some(opt_if(arena, target, any, failure, result, false));
    }

    let first = if passes {
        condition
    } else {
        opt_not(arena, condition)
    };
    let all = opt_all(arena, vec![first, inner]);

    Some(opt_if(arena, target, all, result, failure, false))
}

/// The condition under which a tree of checks reaches its result rather than the failure, as a
/// bool: an `if` with a cheap condition (see [`is_cheap_check`]) whose branches are the failure,
/// the result, or more checks of the same kind. The result is whatever the first branch that
/// isn't one of those is, and every other such branch must be the same value. An `if` that isn't
/// made of those is a result itself, so the checks after one that isn't cheap are left as they
/// are.
///
/// The budget is the number of nodes in the conditions and of branches that are followed, which
/// is reduced by them.
fn pass_condition(
    arena: &mut Arena<Lir>,
    target: ClvmTarget,
    tree: LirId,
    failure: LirId,
    result: &mut Option<LirId>,
    budget: &mut usize,
) -> Option<LirId> {
    *budget = budget.checked_sub(1)?;

    if same_value(arena, tree, failure) {
        return Some(arena.alloc(Lir::Atom(vec![])));
    }

    if result.is_some_and(|result| same_value(arena, tree, result)) {
        return Some(arena.alloc(Lir::Atom(vec![1])));
    }

    if let Lir::If(condition, then, otherwise, false) = arena[tree] {
        let result_before = *result;
        let budget_before = *budget;

        // The budget bounds how deep this goes
        let branches = if is_cheap_check(arena, target, condition, budget) {
            pass_condition(arena, target, then, failure, result, budget).and_then(|then| {
                let otherwise = pass_condition(arena, target, otherwise, failure, result, budget)?;
                Some((then, otherwise))
            })
        } else {
            None
        };

        // If both branches reach the result, or both fail, the condition would only be kept to
        // be evaluated, between two constants that are larger than what they stand for
        let same = |arena: &Arena<Lir>, then: LirId, otherwise: LirId| {
            matches!(
                (&arena[then], &arena[otherwise]),
                (Lir::Atom(then), Lir::Atom(otherwise)) if then.is_empty() == otherwise.is_empty()
            )
        };

        if let Some((then, otherwise)) = branches
            && !same(arena, then, otherwise)
        {
            return Some(opt_if(arena, target, condition, then, otherwise, false));
        }

        *result = result_before;
        *budget = budget_before;
    }

    if result.is_some() {
        return None;
    }

    *result = Some(tree);
    Some(arena.alloc(Lir::Atom(vec![1])))
}

/// Whether the condition has no more nodes than the budget, which is reduced by them, and is
/// only made of values and of operators that can't fail for operands of the right types. These
/// are among the ones the failure analysis of the HIR treats as unable to fail, which leaves
/// out anything that runs other code, which could run for as long as it likes. It also leaves
/// out the operators that only can't fail for some constants (division and shifts), and the
/// ones that cost far more to run than a check does (`pubkey_for_exp`, `keccak256`).
fn is_cheap_check(
    arena: &Arena<Lir>,
    target: ClvmTarget,
    condition: LirId,
    budget: &mut usize,
) -> bool {
    let Some(rest) = budget.checked_sub(1) else {
        return false;
    };

    *budget = rest;

    // The budget bounds how deep this goes
    let mut all = |values: &[LirId]| {
        values
            .iter()
            .all(|&value| is_cheap_check(arena, target, value, budget))
    };

    match &arena[condition] {
        Lir::Atom(_) | Lir::Path(_) | Lir::Quote(_) => true,
        Lir::First(value)
        | Lir::Rest(value)
        | Lir::Listp(value)
        | Lir::Not(value)
        | Lir::Strlen(value)
        | Lir::Lognot(value) => all(&[*value]),
        Lir::Cons(left, right)
        | Lir::Eq(left, right)
        | Lir::Gt(left, right)
        | Lir::GtBytes(left, right) => all(&[*left, *right]),
        Lir::If(condition, then, otherwise, _) => all(&[*condition, *then, *otherwise]),
        Lir::Add(args)
        | Lir::Sub(args)
        | Lir::Concat(args)
        | Lir::Logand(args)
        | Lir::Logior(args)
        | Lir::Logxor(args)
        | Lir::All(args)
        | Lir::Any(args)
        | Lir::Sha256(args)
        | Lir::Sha256Inline(args) => all(args),
        // `LIMITS` rejects an operand that's too long
        Lir::Mul(args) => !target.limits_operands() && all(args),
        Lir::Run(..)
        | Lir::Closure(..)
        | Lir::Div(..)
        | Lir::Divmod(..)
        | Lir::Mod(..)
        | Lir::Modpow(..)
        | Lir::Raise(_)
        | Lir::Substr(..)
        | Lir::Ash(..)
        | Lir::Lsh(..)
        | Lir::PubkeyForExp(_)
        | Lir::G1Add(_)
        | Lir::G1Subtract(_)
        | Lir::G1Multiply(..)
        | Lir::G1Negate(_)
        | Lir::G1Map(..)
        | Lir::G2Add(_)
        | Lir::G2Subtract(_)
        | Lir::G2Multiply(..)
        | Lir::G2Negate(_)
        | Lir::G2Map(..)
        | Lir::BlsPairingIdentity(_)
        | Lir::BlsVerify(..)
        | Lir::Keccak256(_)
        | Lir::Keccak256Inline(_)
        | Lir::CoinId(..)
        | Lir::K1Verify(..)
        | Lir::R1Verify(..)
        | Lir::Keccak256Verify(..)
        | Lir::Op(..)
        | Lir::DebugPrint(..) => false,
    }
}

/// An `if` whose condition is another `if` with a branch that's a constant, such as
/// `(a (i (a (i a (q 1 . 1) (q . B)) 1) (q . T) (q . F)) 1)` from `if a || b`, already knows which
/// way it goes when the inner condition picks the constant. Branching there directly gives
/// `(a (i a (q . T) (q 2 (i B (q . T) (q . F)) 1)) 1)`, which runs one less `a`, and has the branch
/// that's taken twice in place of the constant. So it's only done if that branch is no larger
/// than the constant was, which is five bytes for `(q 1 . 1)` and one for `()`.
fn branch_directly(
    arena: &mut Arena<Lir>,
    target: ClvmTarget,
    condition: LirId,
    then: LirId,
    otherwise: LirId,
) -> Option<LirId> {
    let Lir::If(inner, inner_then, inner_otherwise, false) = arena[condition] else {
        return None;
    };

    let repeated = |arena: &Arena<Lir>, constant: LirId| {
        let Lir::Atom(atom) = &arena[constant] else {
            return None;
        };

        let (taken, constant_size) = if atom.is_empty() {
            (otherwise, 1)
        } else {
            (then, 5)
        };

        (quoted_size(arena, taken)? <= constant_size).then_some(taken)
    };

    if let Some(taken) = repeated(arena, inner_then) {
        let rest = opt_if(arena, target, inner_otherwise, then, otherwise, false);
        return Some(opt_if(arena, target, inner, taken, rest, false));
    }

    if let Some(taken) = repeated(arena, inner_otherwise) {
        let rest = opt_if(arena, target, inner_then, then, otherwise, false);
        return Some(opt_if(arena, target, inner, rest, taken, false));
    }

    None
}

/// How many bytes the value takes up as the quoted branch of an `if`, if it's one of the values
/// that are at most five: `()`, a path or `(x)` in one byte, which is quoted as it is, or an atom
/// of one byte, which is `(q 1 . ATOM)`.
fn quoted_size(arena: &Arena<Lir>, value: LirId) -> Option<usize> {
    let single_byte = |bytes: &[u8]| matches!(bytes, [byte] if *byte < 0x80);

    match &arena[value] {
        Lir::Atom(atom) if atom.is_empty() => Some(1),
        Lir::Atom(atom) if single_byte(atom) => Some(5),
        Lir::Path(path) if single_byte(&path_to_atom(path)) => Some(3),
        Lir::Raise(args) if args.is_empty() => Some(5),
        _ => None,
    }
}

/// Allocates an `if` whose condition is already optimized, making it inline if both branches are
/// atoms, since evaluating both is then the same as choosing one and is cheaper.
///
/// `(i (l x) () (not x))` is `(not x)`, since `not` is already false for a pair. This is what a
/// check for nil is lowered to when the value could also be a pair or another atom, such as
/// `x is nil` for an `Any`. The check can also end in `(= x ())`, which is `(not x)` for an atom,
/// and is only evaluated for one unless the `if` is inline.
pub fn alloc_if(
    arena: &mut Arena<Lir>,
    condition: LirId,
    then: LirId,
    otherwise: LirId,
    inline: bool,
) -> LirId {
    if let Some(check) = nil_check(arena, condition, then, otherwise, inline) {
        return check;
    }

    let inline =
        inline || (matches!(arena[then], Lir::Atom(_)) && matches!(arena[otherwise], Lir::Atom(_)));

    arena.alloc(Lir::If(condition, then, otherwise, inline))
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

/// A bitwise operator that takes any number of operands.
#[derive(Debug, Clone, Copy)]
pub enum Bitwise {
    And,
    Or,
    Xor,
}

impl Bitwise {
    fn operator(self) -> ClvmOp {
        match self {
            Self::And => ClvmOp::Logand,
            Self::Or => ClvmOp::Logior,
            Self::Xor => ClvmOp::Logxor,
        }
    }

    /// The result of the operator without any operands, which doesn't change the result when
    /// it's an operand.
    fn identity(self) -> BigInt {
        match self {
            Self::And => BigInt::from(-1),
            Self::Or | Self::Xor => BigInt::from(0),
        }
    }

    fn apply(self, value: &mut BigInt, operand: BigInt) {
        match self {
            Self::And => *value &= operand,
            Self::Or => *value |= operand,
            Self::Xor => *value ^= operand,
        }
    }

    fn node(self, args: Vec<LirId>) -> Lir {
        match self {
            Self::And => Lir::Logand(args),
            Self::Or => Lir::Logior(args),
            Self::Xor => Lir::Logxor(args),
        }
    }
}

// Nested uses of the same operator are flattened, and the constant operands are combined into one
// at the end, which is left out if it doesn't change the result
pub fn opt_bitwise(arena: &mut Arena<Lir>, op: Bitwise, args: Vec<LirId>) -> LirId {
    let mut args = ArgList::new(args);
    let mut result = Vec::new();
    let identity = op.identity();
    let mut value = identity.clone();

    while let Some(arg) = args.next() {
        match &arena[arg] {
            Lir::Atom(atom) => op.apply(&mut value, atom_bigint(atom)),
            node if node.operator() == Some(op.operator()) => {
                args.prepend(node.children().collect());
            }
            _ => result.push(arg),
        }
    }

    if value != identity {
        result.push(arena.alloc(Lir::Atom(bigint_atom(value))));
    }

    if result.is_empty() {
        return arena.alloc(Lir::Atom(bigint_atom(identity)));
    }

    if result.len() == 1 && matches!(arena[result[0]], Lir::Atom(_)) {
        return result[0];
    }

    arena.alloc(op.node(result))
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

// If both values are atoms and the hash matches, the check passes, so it's nil
// Otherwise it's kept, so that it still fails at runtime
pub fn opt_keccak256_verify(arena: &mut Arena<Lir>, value: LirId, hash: LirId) -> LirId {
    let matches = matches!(
        (&arena[value], &arena[hash]),
        (Lir::Atom(value), Lir::Atom(hash)) if Keccak256::digest(value).as_slice() == hash.as_slice()
    );

    if matches {
        return arena.alloc(Lir::Atom(vec![]));
    }

    arena.alloc(Lir::Keccak256Verify(value, hash))
}

/// Whether both values are the same expression, so they evaluate to the same result (or both
/// fail) in the same environment. This is conservative, so it can return `false` for values
/// that are the same.
fn same_value(arena: &Arena<Lir>, left: LirId, right: LirId) -> bool {
    if left == right {
        return true;
    }

    grow_stack(|| same_value_step(arena, left, right))
}

fn same_value_step(arena: &Arena<Lir>, left: LirId, right: LirId) -> bool {
    match (&arena[left], &arena[right]) {
        (Lir::Atom(left), Lir::Atom(right)) => left == right,
        (Lir::Path(left), Lir::Path(right)) => left == right,
        (Lir::Quote(left), Lir::Quote(right))
        | (Lir::First(left), Lir::First(right))
        | (Lir::Rest(left), Lir::Rest(right))
        | (Lir::Strlen(left), Lir::Strlen(right))
        | (Lir::Lognot(left), Lir::Lognot(right))
        | (Lir::Not(left), Lir::Not(right)) => same_value(arena, *left, *right),
        (Lir::Cons(a, b), Lir::Cons(c, d))
        | (Lir::Div(a, b), Lir::Div(c, d))
        | (Lir::Mod(a, b), Lir::Mod(c, d))
        | (Lir::Divmod(a, b), Lir::Divmod(c, d))
        | (Lir::Run(a, b), Lir::Run(c, d)) => {
            same_value(arena, *a, *c) && same_value(arena, *b, *d)
        }
        (Lir::Add(left), Lir::Add(right))
        | (Lir::Sub(left), Lir::Sub(right))
        | (Lir::Mul(left), Lir::Mul(right))
        | (Lir::Concat(left), Lir::Concat(right))
        | (Lir::Sha256(left), Lir::Sha256(right))
        // Two raises with the same arguments fail the same way, which `merge_checks` relies on
        | (Lir::Raise(left), Lir::Raise(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right)
                    .all(|(left, right)| same_value(arena, *left, *right))
        }
        _ => false,
    }
}

pub fn opt_op(arena: &mut Arena<Lir>, op: ClvmOp, arg: LirId) -> LirId {
    arena.alloc(Lir::Op(op, arg))
}

pub fn opt_debug_print(arena: &mut Arena<Lir>, srcloc: String, value: LirId) -> LirId {
    arena.alloc(Lir::DebugPrint(srcloc, value))
}

#[cfg(test)]
mod tests {
    use clvmr::{Allocator, NodePtr, serde::node_to_bytes};

    use super::*;
    use crate::optimize::{
        optimize,
        test_helpers::{compile, disassemble, int, path, run},
    };

    const UNLIMITED: ClvmTarget = ClvmTarget::TODAY;
    const LIMITED: ClvmTarget = ClvmTarget::TODAY_WITH_LIMITS;
    const HARD_FORK_2: ClvmTarget = ClvmTarget::HARD_FORK_2;

    /// Checks that the LIR behaves the same after it's optimized, for each target, and returns
    /// what it's optimized to for the default one.
    fn assert_equivalent(arena: &mut Arena<Lir>, lir: LirId, envs: &[Vec<Vec<u8>>]) -> LirId {
        assert_equivalent_with(arena, LIMITED, lir, envs);
        assert_equivalent_with(arena, HARD_FORK_2, lir, envs);
        assert_equivalent_with(arena, UNLIMITED, lir, envs)
    }

    fn assert_equivalent_with(
        arena: &mut Arena<Lir>,
        target: ClvmTarget,
        lir: LirId,
        envs: &[Vec<Vec<u8>>],
    ) -> LirId {
        let optimized = optimize(arena, lir, target);

        let mut allocator = Allocator::new();
        let unoptimized_program = compile(arena, &mut allocator, lir, target, false);
        let optimized_program = compile(arena, &mut allocator, optimized, target, true);

        for env in envs {
            let mut env_ptr = NodePtr::NIL;

            for value in env.iter().rev() {
                let value = allocator.new_atom(value).unwrap();
                env_ptr = allocator.new_pair(value, env_ptr).unwrap();
            }

            assert_eq!(
                run(&mut allocator, target, unoptimized_program, env_ptr),
                run(&mut allocator, target, optimized_program, env_ptr),
                "optimized program differs for environment {env:?} with {target:?}"
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
        let one = int(&mut arena, 1);
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
        let ten = int(&mut arena, 10);
        let twenty = int(&mut arena, 20);
        let thirty = int(&mut arena, 30);
        let one = int(&mut arena, 1);
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
        let hundred = int(&mut arena, 100);
        let value = path(&mut arena, 2);
        let three_hundred = int(&mut arena, 300);
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
        let one = int(&mut arena, 1);
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
            let constant = int(&mut arena, constant);
            for args in [vec![value, constant], vec![value, other, constant]] {
                let lir = arena.alloc(Lir::Sub(args));
                let once = optimize(&mut arena, lir, UNLIMITED);
                let twice = optimize(&mut arena, once, UNLIMITED);

                let mut allocator = Allocator::new();
                let once = compile(&arena, &mut allocator, once, UNLIMITED, true);
                let twice = compile(&arena, &mut allocator, twice, UNLIMITED, true);
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
        let two = int(&mut arena, 2);
        let one = int(&mut arena, 1);
        let non_canonical_one = arena.alloc(Lir::Atom(vec![0, 1]));

        let modulo = arena.alloc(Lir::Mod(value, two));
        let result = opt_eq(&mut arena, UNLIMITED, modulo, one);
        assert_eq!(disassemble(&arena, result), "(logand 2 (q . 1))");

        let result = opt_eq(&mut arena, UNLIMITED, one, modulo);
        assert_eq!(disassemble(&arena, result), "(logand 2 (q . 1))");

        let result = opt_eq(&mut arena, UNLIMITED, modulo, non_canonical_one);
        assert_eq!(
            disassemble(&arena, result),
            "(= (% 2 (q . 2)) (q . 0x0001))"
        );

        let lir = arena.alloc(Lir::Eq(modulo, non_canonical_one));
        assert_equivalent(&mut arena, lir, &[int_env(&[3]), int_env(&[4])]);
    }

    // With `operand_limits`, `%` fails for a dividend longer than 256 bytes, but `logand` doesn't,
    // so the odd check is only rewritten if the dividend is known to be short enough
    #[test]
    fn test_odd_check_with_operand_limits_requires_short_dividend() {
        let mut arena = Arena::new();
        let value = path(&mut arena, 2);
        let two = int(&mut arena, 2);
        let one = int(&mut arena, 1);

        let modulo = arena.alloc(Lir::Mod(value, two));
        let result = opt_eq(&mut arena, LIMITED, modulo, one);
        assert_eq!(disassemble(&arena, result), "(= (% 2 (q . 2)) (q . 1))");

        let result = opt_eq(&mut arena, LIMITED, one, modulo);
        assert_eq!(disassemble(&arena, result), "(= (q . 1) (% 2 (q . 2)))");

        let lir = arena.alloc(Lir::Eq(modulo, one));
        let long = [vec![1], vec![0; 299]].concat();
        let envs = [vec![long], int_env(&[3])];

        // Without the limits, the long dividend is odd-checked with `logand` like any other
        let optimized = assert_equivalent(&mut arena, lir, &envs);
        assert_eq!(disassemble(&arena, optimized), "(logand 2 (q . 1))");

        // A boolean is short enough, as is a constant
        let five = path(&mut arena, 5);
        let boolean = arena.alloc(Lir::Eq(value, five));
        let modulo = arena.alloc(Lir::Mod(boolean, two));
        let result = opt_eq(&mut arena, LIMITED, modulo, one);
        assert_eq!(disassemble(&arena, result), "(logand (= 2 5) (q . 1))");

        for (len, rewritten) in [(256, true), (257, false)] {
            let constant = arena.alloc(Lir::Atom(vec![1; len]));
            let modulo = arena.alloc(Lir::Mod(constant, two));
            assert_eq!(
                is_odd_check(&arena, LIMITED, modulo, one).is_some(),
                rewritten
            );
        }
    }

    // `LIMITS` limits the operands of `modpow` to 256 bytes and the scalar of `g1_multiply` and
    // `g2_multiply` to 1024 bytes, so they're only folded past those lengths without
    // `operand_limits`, or after hard fork 2, which removes `LIMITS`
    #[test]
    fn test_limited_operators_only_fold_without_operand_limits() {
        let mut arena = Arena::new();
        let long_base = arena.alloc(Lir::Atom(vec![1; 257]));
        let two = int(&mut arena, 2);
        let seven = int(&mut arena, 7);
        // The generator of G1
        let generator = "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb";
        let point = arena.alloc(Lir::Atom(
            (0..generator.len())
                .step_by(2)
                .map(|i| u8::from_str_radix(&generator[i..i + 2], 16).unwrap())
                .collect(),
        ));
        let long_scalar = arena.alloc(Lir::Atom(vec![1; 1025]));

        // The generator of G2
        let generator = "93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8";
        let g2_point = arena.alloc(Lir::Atom(
            (0..generator.len())
                .step_by(2)
                .map(|i| u8::from_str_radix(&generator[i..i + 2], 16).unwrap())
                .collect(),
        ));

        for (target, folded) in [(UNLIMITED, true), (LIMITED, false), (HARD_FORK_2, true)] {
            let result = opt_fold(&mut arena, target, Lir::Modpow(long_base, two, seven));
            assert_eq!(matches!(arena[result], Lir::Atom(_)), folded);

            let result = opt_fold(&mut arena, target, Lir::G1Multiply(point, long_scalar));
            assert_eq!(matches!(arena[result], Lir::Atom(_)), folded);

            let result = opt_fold(&mut arena, target, Lir::G2Multiply(g2_point, long_scalar));
            assert_eq!(matches!(arena[result], Lir::Atom(_)), folded);
        }

        // Short operands fold, unless `modpow` is disabled, as `DISABLE_OP` does with
        // `operand_limits` before hard fork 2
        for (target, folded) in [(UNLIMITED, true), (LIMITED, false), (HARD_FORK_2, true)] {
            let result = opt_fold(&mut arena, target, Lir::Modpow(two, two, seven));
            assert_eq!(
                matches!(&arena[result], Lir::Atom(atom) if *atom == [4]),
                folded
            );
        }
    }

    // Operands that leave nothing to compute are the operator's identity: -1 for `logand`, and
    // nil for `logior` and `logxor`
    #[test]
    fn test_bitwise_without_operands_is_identity() {
        let mut arena = Arena::new();

        for (op, operands, expected) in [
            (Bitwise::And, vec![], -1),
            (Bitwise::And, vec![-1], -1),
            (Bitwise::And, vec![-1, -1], -1),
            (Bitwise::Or, vec![], 0),
            (Bitwise::Or, vec![0], 0),
            (Bitwise::Xor, vec![], 0),
            (Bitwise::Xor, vec![0], 0),
            (Bitwise::Xor, vec![5, 5], 0),
        ] {
            let args = operands
                .iter()
                .map(|&value| int(&mut arena, value))
                .collect();
            let result = opt_bitwise(&mut arena, op, args);
            assert!(
                matches!(&arena[result], Lir::Atom(atom) if *atom == bigint_atom(BigInt::from(expected))),
                "{op:?} of {operands:?}: {:?}",
                arena[result]
            );
        }
    }

    // A check of constants passes or fails at compile time, so it's only folded if it passes
    #[test]
    fn test_keccak256_verify_folds_matching_constants() {
        let mut arena = Arena::new();
        let value = arena.alloc(Lir::Atom(b"hello".to_vec()));
        let hash = arena.alloc(Lir::Atom(Keccak256::digest(b"hello").to_vec()));
        let wrong = arena.alloc(Lir::Atom(vec![0; 32]));
        let path = path(&mut arena, 2);

        let result = opt_keccak256_verify(&mut arena, value, hash);
        assert!(matches!(&arena[result], Lir::Atom(atom) if atom.is_empty()));

        for (value, hash) in [(value, wrong), (path, hash), (value, path)] {
            let result = opt_keccak256_verify(&mut arena, value, hash);
            assert!(matches!(arena[result], Lir::Keccak256Verify(v, h) if v == value && h == hash));
        }
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
        // Longer than `LIMITS` allows for an operand of `*` or the dividend of `%`
        &[0x01; 257],
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

        match rng.below(7) {
            0 | 1 => arena.alloc(Lir::Add(args)),
            2 | 3 => arena.alloc(Lir::Sub(args)),
            4 => arena.alloc(Lir::Mul(args)),
            5 => {
                let value = random_arith(arena, rng, depth - 1);
                let divisor = random_arith(arena, rng, depth - 1);
                arena.alloc(Lir::Div(value, divisor))
            }
            _ => {
                let value = random_arith(arena, rng, depth - 1);
                let two = int(arena, 2);
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

            for target in [UNLIMITED, LIMITED, HARD_FORK_2] {
                let optimized = assert_equivalent_with(&mut arena, target, lir, &envs);

                // Optimizing a second time shouldn't change the output
                let twice = optimize(&mut arena, optimized, target);
                let mut allocator = Allocator::new();
                let once = compile(&arena, &mut allocator, optimized, target, true);
                let twice = compile(&arena, &mut allocator, twice, target, true);
                assert_eq!(
                    node_to_bytes(&allocator, once).unwrap(),
                    node_to_bytes(&allocator, twice).unwrap()
                );
            }
        }
    }

    #[test]
    fn test_mul_by_one_keeps_mul() {
        let mut arena = Arena::new();
        let value = arena.alloc(Lir::Path(BigUint::from(2u8)));
        let one = arena.alloc(Lir::Atom(vec![1]));
        let result = opt_mul(&mut arena, UNLIMITED, vec![value, one]);
        assert!(matches!(&arena[result], Lir::Mul(args) if *args == [value]));
    }

    #[test]
    fn test_mul_constants_folds() {
        let mut arena = Arena::new();
        let two = arena.alloc(Lir::Atom(vec![2]));
        let three = arena.alloc(Lir::Atom(vec![3]));
        let result = opt_mul(&mut arena, UNLIMITED, vec![two, three]);
        assert!(matches!(&arena[result], Lir::Atom(atom) if *atom == [6]));
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
    fn test_cons_of_first_and_rest_uses_parent() {
        let mut arena = Arena::new();
        let first = path(&mut arena, 4);
        let rest = path(&mut arena, 6);
        let result = opt_cons(&mut arena, first, rest);
        assert!(matches!(&arena[result], Lir::Path(p) if *p == BigUint::from(2u8)));
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
        let result = opt_first(&mut arena, UNLIMITED, zero);
        assert!(matches!(arena[result], Lir::First(_)));
        let result = opt_rest(&mut arena, UNLIMITED, zero);
        assert!(matches!(arena[result], Lir::Rest(_)));

        let five = path(&mut arena, 5);
        let result = opt_first(&mut arena, UNLIMITED, five);
        assert!(matches!(&arena[result], Lir::Path(p) if *p == BigUint::from(9u8)));
    }
}
