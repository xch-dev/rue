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
// Two of these rewrites assume that a value matches its static type, the same way opt_cons
// does: `(l x)` on a `List` is truthy exactly when `x` is, and `(= x ())` is `(not x)` when `x`
// is an atom type. If the solution doesn't match the declared types (an atom where a list is
// expected, or a pair where an atom is expected), the optimized program can take a different
// branch or skip the `=` failure. Rue doesn't check values against their types at runtime anyway,
// so this doesn't give the spender anything that a wrongly shaped value couldn't, and it keeps
// conditions cheaper. These rewrites are only applied where just the truthiness is observed.
pub fn opt_truthy(arena: &mut Arena<Lir>, value: LirId) -> Truthiness {
    match arena[value].clone() {
        Lir::Atom(atom) => Truthiness::Known(!atom.is_empty()),
        // A pair is always truthy, but it can only be discarded if it can't fail
        Lir::Cons(..) if is_pure(arena, value) => Truthiness::Known(true),
        // Since `not` accepts any value, `(not x)` has the opposite truthiness of `x`
        Lir::Not(inner) => opt_negated_truthy(arena, inner, Some(value)),
        // A list is either nil or a pair, so it's truthy exactly when it's a pair
        // This assumes that the value matches its type (see above)
        Lir::Listp(inner, false) => opt_truthy(arena, inner),
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
        _ => Truthiness::Unknown(value),
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
