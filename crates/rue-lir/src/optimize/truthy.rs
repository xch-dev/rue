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
// None of these rewrites assume that a value matches its static type, since the solution isn't
// checked against the declared types at runtime. For example, `(l x)` isn't replaced with `x`
// even if `x` is a list, since a truthy atom would take the other branch.
pub fn opt_truthy(arena: &mut Arena<Lir>, value: LirId) -> Truthiness {
    match arena[value].clone() {
        Lir::Atom(atom) => Truthiness::Known(!atom.is_empty()),
        // A pair is always truthy, but it can only be discarded if it can't fail
        Lir::Cons(..) if is_pure(arena, value) => Truthiness::Known(true),
        // Since `not` accepts any value, `(not x)` has the opposite truthiness of `x`
        Lir::Not(inner) => opt_negated_truthy(arena, inner, Some(value)),
        // `(= x ())` is `(not x)` if `x` is always an atom, since `=` fails on a pair
        Lir::Eq(left, right) => {
            if is_nil(arena, left) && returns_atom(arena, right) {
                return opt_negated_truthy(arena, right, None);
            }

            if is_nil(arena, right) && returns_atom(arena, left) {
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
        | Lir::Listp(_)
        | Lir::All(_)
        | Lir::Any(_) => true,
        Lir::If(_, then, otherwise, _) => is_boolean(arena, *then) && is_boolean(arena, *otherwise),
        _ => false,
    }
}

/// Whether the value is always an atom (if it doesn't fail), regardless of the environment.
pub fn returns_atom(arena: &Arena<Lir>, value: LirId) -> bool {
    match &arena[value] {
        Lir::Atom(_)
        | Lir::Add(_)
        | Lir::Sub(_)
        | Lir::Mul(_)
        | Lir::Div(..)
        | Lir::Mod(..)
        | Lir::Modpow(..)
        | Lir::Eq(..)
        | Lir::Gt(..)
        | Lir::GtBytes(..)
        | Lir::Not(_)
        | Lir::Listp(_)
        | Lir::All(_)
        | Lir::Any(_)
        | Lir::Concat(_)
        | Lir::Strlen(_)
        | Lir::Substr(..)
        | Lir::Logand(_)
        | Lir::Logior(_)
        | Lir::Logxor(_)
        | Lir::Lognot(_)
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
        | Lir::Sha256(_)
        | Lir::Sha256Inline(_)
        | Lir::Keccak256(_)
        | Lir::Keccak256Inline(_)
        | Lir::CoinId(..)
        | Lir::K1Verify(..)
        | Lir::R1Verify(..) => true,
        Lir::If(_, then, otherwise, _) => {
            returns_atom(arena, *then) && returns_atom(arena, *otherwise)
        }
        Lir::Path(_)
        | Lir::Quote(_)
        | Lir::Run(..)
        | Lir::Closure(..)
        | Lir::First(_)
        | Lir::Rest(_)
        | Lir::Cons(..)
        | Lir::Divmod(..)
        | Lir::Raise(_)
        | Lir::Op(..)
        | Lir::DebugPrint(..) => false,
    }
}

fn is_nil(arena: &Arena<Lir>, value: LirId) -> bool {
    matches!(&arena[value], Lir::Atom(atom) if atom.is_empty())
}
