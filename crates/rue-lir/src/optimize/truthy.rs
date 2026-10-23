use id_arena::Arena;

use crate::{Lir, LirId};

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
// We intentionally don't make use of the static type of a value here (for example,
// assuming that a `List` is never a non-nil atom, or that a `Bytes` is never a pair),
// since a spender controls the solution and it isn't checked against the declared types
// at runtime. Doing so would make the optimized program behave differently from the
// unoptimized one.
pub fn opt_truthy(arena: &mut Arena<Lir>, value: LirId) -> Truthiness {
    match arena[value].clone() {
        Lir::Atom(atom) => Truthiness::Known(!atom.is_empty()),
        // A pair is always truthy, but it can only be discarded if it can't fail
        Lir::Cons(..) if is_pure(arena, value) => Truthiness::Known(true),
        // Since `not` accepts any value, `(not (not x))` has the same truthiness as `x`
        Lir::Not(inner) => {
            if let Lir::Not(inner) = arena[inner].clone() {
                return opt_truthy(arena, inner);
            }

            Truthiness::Unknown(value)
        }
        // If the other operand is always an atom, `=` can't fail and `(= x ())` is `(not x)`
        Lir::Eq(left, right) => {
            if is_nil(arena, left) && is_boolean(arena, right) {
                return opt_negated_truthy(arena, right);
            }

            if is_nil(arena, right) && is_boolean(arena, left) {
                return opt_negated_truthy(arena, left);
            }

            Truthiness::Unknown(value)
        }
        _ => Truthiness::Unknown(value),
    }
}

// Computes the truthiness of `(not value)`
fn opt_negated_truthy(arena: &mut Arena<Lir>, value: LirId) -> Truthiness {
    match opt_truthy(arena, value) {
        Truthiness::Known(value) => Truthiness::Known(!value),
        Truthiness::Unknown(value) => {
            if let Lir::Not(inner) = arena[value].clone() {
                return opt_truthy(arena, inner);
            }

            Truthiness::Unknown(arena.alloc(Lir::Not(value)))
        }
    }
}

/// Whether evaluating the value can never fail, so it's safe to discard.
/// Paths aren't pure, since traversing into an atom fails.
pub fn is_pure(arena: &Arena<Lir>, value: LirId) -> bool {
    match &arena[value] {
        Lir::Atom(_) | Lir::Quote(_) => true,
        Lir::Cons(first, rest) => is_pure(arena, *first) && is_pure(arena, *rest),
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
        Lir::If(_, then, otherwise, _) => is_boolean(arena, *then) && is_boolean(arena, *otherwise),
        _ => false,
    }
}

fn is_nil(arena: &Arena<Lir>, value: LirId) -> bool {
    matches!(&arena[value], Lir::Atom(atom) if atom.is_empty())
}
