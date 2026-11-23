use crate::{Block, Database, FieldAccess, FieldKind, Hir, HirId, UnaryOp};

/// Decides whether the values that [`is_small_static`] can't judge from their structure alone are
/// small and static, since that depends on where they're used: references, calls and guards.
pub(crate) trait StaticContext {
    /// Whether the value is static and its atoms take up no more than the budget, which is
    /// reduced by their size. This is asked for each value that isn't a literal or made of other
    /// values (see [`is_small_static`]), so anything that the context doesn't know is `false`.
    fn is_small_static_here(&mut self, db: &Database, hir: HirId, budget: &mut usize) -> bool;
}

/// Whether the value is static and its atoms take up no more than the budget, which is reduced by
/// their size. Each atom counts its bytes and one more for its size prefix (see [`take`]). Pairs,
/// lists, structs, casts, unary plus, fields of static values, evaluated constant expressions and
/// blocks without statements are static if what they're made of is, and the context decides the
/// rest (see [`StaticContext`]).
pub(crate) fn is_small_static(
    cx: &mut impl StaticContext,
    db: &Database,
    hir: HirId,
    budget: &mut usize,
) -> bool {
    match db.hir(hir) {
        Hir::Nil | Hir::Bool(_) => take(budget, 1),
        Hir::Int(value) => take(budget, value.bits().div_ceil(8) as usize + 1),
        Hir::Bytes(bytes) => take(budget, bytes.len() + 1),
        Hir::String(string) => take(budget, string.len() + 1),
        Hir::Pair(first, rest) => {
            is_small_static(cx, db, *first, budget) && is_small_static(cx, db, *rest, budget)
        }
        Hir::List(list) => list
            .items
            .iter()
            .all(|&item| is_small_static(cx, db, item, budget)),
        Hir::Struct(initializer) => initializer
            .values()
            .all(|value| is_small_static(cx, db, value, budget)),
        Hir::Unary(UnaryOp::Plus, hir) | Hir::Cast(hir) => is_small_static(cx, db, *hir, budget),
        // A field of a static value is folded, and is no larger than the value
        Hir::FieldAccess(FieldAccess {
            value,
            kind: FieldKind::Path(_),
            ..
        }) => is_small_static(cx, db, *value, budget),
        Hir::Const(expr) => expr
            .result
            .is_some_and(|result| is_small_static(cx, db, result, budget)),
        Hir::Block(Block {
            statements,
            body: Some(body),
        }) if statements.is_empty() => is_small_static(cx, db, *body, budget),
        _ => cx.is_small_static_here(db, hir, budget),
    }
}

/// Takes the size out of the budget, if it fits.
pub(crate) fn take(budget: &mut usize, size: usize) -> bool {
    match budget.checked_sub(size) {
        Some(rest) => {
            *budget = rest;
            true
        }
        None => false,
    }
}
