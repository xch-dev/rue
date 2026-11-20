use id_arena::Arena;

use crate::{
    Lir, LirId,
    optimize::{alloc_if, opt_all, opt_any, opt_not},
    stack::grow_stack,
};

/// What is statically known about the truthiness of an already optimized value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Truthiness {
    /// The value always has this truthiness, and evaluating it can never fail,
    /// so it can be discarded entirely.
    Known(bool),

    /// The truthiness isn't known statically, or the value must still be evaluated
    /// since it may fail. The id is a replacement which is only equivalent in
    /// truthiness (and in whether it fails), NOT in value. It must only be used
    /// where nothing but the truthiness is observed, such as the condition of an `i`,
    /// an argument to `all` or `any`, or the operand of `not`.
    Unknown(LirId),
}

// Note that `i`, `not`, `all`, and `any` only check whether a value is nil.
// They accept pairs (which are truthy) and any atom (only the empty atom is falsy,
// so non-canonical atoms like `0x00` are truthy) without failing.
//
// One of these rewrites assumes that a value matches its static type, the same way opt_cons
// does: `(= x ())` is `(not x)` when `x` is an atom type. If the solution has a pair where an atom
// is expected, the optimized program skips the `=` failure. Rue doesn't check values against their
// types at runtime anyway, so this doesn't give the spender anything that a wrongly shaped value
// couldn't, and it keeps conditions cheaper. It's only applied where just the truthiness is
// observed.
//
// A type guard between nil and a pair (such as `is nil` on a `List`) doesn't need a rewrite like
// this, since it's already generated as a truthiness check (see `generate_check_hir`).
pub fn opt_truthy(arena: &mut Arena<Lir>, value: LirId) -> Truthiness {
    grow_stack(|| opt_truthy_step(arena, value))
}

fn opt_truthy_step(arena: &mut Arena<Lir>, value: LirId) -> Truthiness {
    match arena[value].clone() {
        Lir::Atom(atom) => Truthiness::Known(!atom.is_empty()),
        // A quote can't fail, and evaluates to the compiled code of its value: nil for the empty
        // atom, and a pair for any other atom (which compiles to `(q . atom)`) or for a pair
        Lir::Quote(inner) => match &arena[inner] {
            Lir::Atom(atom) => Truthiness::Known(!atom.is_empty()),
            Lir::Cons(..) => Truthiness::Known(true),
            _ => Truthiness::Unknown(value),
        },
        // A pair is always truthy, but it can only be discarded if it can't fail
        Lir::Cons(..) if is_pure(arena, value) => Truthiness::Known(true),
        // `(l x)` of a pair is truthy, and fails exactly when the pair does
        Lir::Listp(inner) if matches!(arena[inner], Lir::Cons(..)) => opt_truthy(arena, inner),
        // `all` and `any` of a single value have the same truthiness as it
        Lir::All(args) | Lir::Any(args) if args.len() == 1 => opt_truthy(arena, args[0]),
        Lir::All(args) if args.is_empty() => Truthiness::Known(true),
        Lir::Any(args) if args.is_empty() => Truthiness::Known(false),
        // Since `not` accepts any value, `(not x)` has the opposite truthiness of `x`
        Lir::Not(inner) => opt_negated_truthy(arena, inner, Some(value)),
        // `(= x ())` is `(not x)` if `x` is an atom
        // This assumes that the value matches its type (see above)
        Lir::Eq(left, right) => {
            if is_nil(arena, left) {
                return opt_negated_truthy(arena, right, None);
            }

            if is_nil(arena, right) {
                return opt_negated_truthy(arena, left, None);
            }

            Truthiness::Unknown(value)
        }
        // Only the truthiness of the branch that's taken is observed, so each branch can be
        // replaced by one with the same truthiness. A branch whose truthiness is known can't fail,
        // so it can be replaced by an atom with that truthiness, even if the `if` is inline.
        //
        // An inline `if` evaluates both branches, so if one of them is known, its truthiness is
        // that of `any` or `all` of the condition and the other branch, such as `(i c 1 b)` from
        // `c || b`, which is `(any c b)`.
        Lir::If(condition, then, otherwise, inline) => {
            let then_truthiness = opt_truthy(arena, then);
            let otherwise_truthiness = opt_truthy(arena, otherwise);

            if inline {
                match (then_truthiness, otherwise_truthiness) {
                    (Truthiness::Known(true), Truthiness::Unknown(otherwise)) => {
                        return Truthiness::Unknown(opt_any(arena, vec![condition, otherwise]));
                    }
                    (Truthiness::Unknown(then), Truthiness::Known(false)) => {
                        return Truthiness::Unknown(opt_all(arena, vec![condition, then]));
                    }
                    (Truthiness::Known(false), Truthiness::Unknown(otherwise)) => {
                        let negated = opt_not(arena, condition);
                        return Truthiness::Unknown(opt_all(arena, vec![negated, otherwise]));
                    }
                    (Truthiness::Unknown(then), Truthiness::Known(true)) => {
                        let negated = opt_not(arena, condition);
                        return Truthiness::Unknown(opt_any(arena, vec![negated, then]));
                    }
                    (Truthiness::Known(_), Truthiness::Known(_))
                    | (Truthiness::Unknown(_), Truthiness::Unknown(_)) => {}
                }
            }

            let new_then = replacement(arena, then_truthiness);
            let new_otherwise = replacement(arena, otherwise_truthiness);

            if new_then == then && new_otherwise == otherwise {
                return Truthiness::Unknown(value);
            }

            Truthiness::Unknown(alloc_if(arena, condition, new_then, new_otherwise, inline))
        }
        _ => Truthiness::Unknown(value),
    }
}

// A value with the given truthiness, which fails exactly when the value it was found for does
fn replacement(arena: &mut Arena<Lir>, truthiness: Truthiness) -> LirId {
    match truthiness {
        Truthiness::Known(true) => arena.alloc(Lir::Atom(vec![1])),
        Truthiness::Known(false) => arena.alloc(Lir::Atom(vec![])),
        Truthiness::Unknown(replacement) => replacement,
    }
}

// Computes the truthiness of `(not value)`, reusing `negated` if it's already that expression
fn opt_negated_truthy(arena: &mut Arena<Lir>, value: LirId, negated: Option<LirId>) -> Truthiness {
    match opt_truthy(arena, value) {
        Truthiness::Known(known) => Truthiness::Known(!known),
        Truthiness::Unknown(replacement) => {
            if let Lir::Not(inner) = arena[replacement].clone() {
                return opt_truthy(arena, inner);
            }

            if replacement == value
                && let Some(negated) = negated
            {
                return Truthiness::Unknown(negated);
            }

            Truthiness::Unknown(arena.alloc(Lir::Not(replacement)))
        }
    }
}

/// Whether evaluating the value can never fail, so it's safe to discard.
/// Paths aren't pure, since traversing into an atom fails.
pub fn is_pure(arena: &Arena<Lir>, value: LirId) -> bool {
    match &arena[value] {
        Lir::Atom(_) | Lir::Quote(_) => true,
        Lir::Cons(first, rest) => grow_stack(|| is_pure(arena, *first) && is_pure(arena, *rest)),
        _ => false,
    }
}

/// Whether the value is always either `()` or `1` (if it doesn't fail).
pub fn is_boolean(arena: &Arena<Lir>, value: LirId) -> bool {
    match &arena[value] {
        Lir::Atom(atom) => atom.is_empty() || atom.as_slice() == [1],
        Lir::Not(_)
        | Lir::Eq(..)
        | Lir::Gt(..)
        | Lir::GtBytes(..)
        | Lir::Listp(..)
        | Lir::All(_)
        | Lir::Any(_) => true,
        Lir::If(_, then, otherwise, _) => {
            grow_stack(|| is_boolean(arena, *then) && is_boolean(arena, *otherwise))
        }
        _ => false,
    }
}

pub fn is_nil(arena: &Arena<Lir>, value: LirId) -> bool {
    is_atom(arena, value, &[])
}

/// Whether the value is the given atom.
pub fn is_atom(arena: &Arena<Lir>, value: LirId, bytes: &[u8]) -> bool {
    matches!(&arena[value], Lir::Atom(atom) if atom.as_slice() == bytes)
}
