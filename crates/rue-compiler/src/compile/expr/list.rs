use log::debug;
use rue_ast::{AstExpr, AstListExpr, AstNode};
use rue_hir::{FunctionCall, Hir, HirId, List, Value};
use rue_types::{Pair, Type, TypeId, Union};

use super::call_inference::{
    compile_refined_element, holds_other_pairs, list_of, pair_parts, record_element,
};
use crate::{Compiler, compile_expr};

/// Compiles a list. Its type has a pair for each item, with the type of the last item as the rest
/// if it's spread, such as `(A, (B, Rest))` for `[a, b, ...rest]`.
///
/// A list can also spread an item before its last one, such as `[a, ...items, last]`. How many
/// items it has from there on isn't known, so from the first such spread, the rest of the list is
/// a `List` of every item's type and every spread list's item types, such as
/// `(A, List<Item | Last>)`, and each spread from there on, including a spread last item, must be
/// a list (see [`Tail`]).
pub fn compile_list_expr(
    ctx: &mut Compiler,
    list: &AstListExpr,
    mut expected_type: Option<TypeId>,
) -> Value {
    let mut values = Vec::new();
    let mut nil_terminated = true;

    // Once the list could be held as other pairs than its elements are expected to make, the
    // element types are only a hint, and so are those of the rest of the list
    let mut hint = ctx.is_expected_type_hint();

    let len = list.items().count();

    // The first item that's spread before the last one, which the rest of the list is built in
    // front of at runtime from
    let tail_start = list
        .items()
        .take(len.saturating_sub(1))
        .position(|item| item.spread().is_some());
    let mut tail: Option<Tail> = None;

    // The type the last item was expected to be, which an item past the end of the pairs the
    // expected type has gets (see below)
    let mut last_item_type = None;

    for (i, item) in list.items().enumerate() {
        let is_spread = item.spread().is_some();

        if tail_start == Some(i) {
            hint |= expected_type.is_some_and(|ty| holds_other_pairs(ctx, ty));
            tail = Some(Tail::new(ctx, expected_type));
        }

        if let Some(tail) = &mut tail {
            tail.compile_item(ctx, item.expr().as_ref(), is_spread, hint);
            continue;
        }

        let item_type = if is_spread {
            nil_terminated = false;
            expected_type
        } else if let Some(ty) = expected_type
            && let Some((first, rest)) = pair_parts(ctx, ty)
        {
            hint |= holds_other_pairs(ctx, ty);
            expected_type = Some(rest);
            last_item_type = Some(first);

            Some(first)
        } else {
            // An item past the end of the expected type's pairs, such as `make()` in `[3, make()]`
            // with `(2, nil)` expected, gets the type the item before it was expected to be: the
            // list's expected type is often a list literal's inferred type, which only has as many
            // items as that literal, such as `(2, nil)` for `[2]`, and that's what the type would
            // be as a `List`. A list expected to be a type of its own that ends there can't be
            // longer anyway, so this only matters for one whose expected type is a hint (see
            // `Compiler::with_expected_type_hint`). Where the expected type goes on as something
            // other than `nil`, such as `Any`, it isn't the end of a list
            last_item_type.filter(|_| {
                expected_type.is_some_and(|rest| {
                    let nil = ctx.builtins().types.nil;
                    ctx.is_assignable(rest, nil)
                })
            })
        };

        let value = if let Some(expr) = item.expr() {
            let (value, item_type) = compile_refined_element(ctx, &expr, item_type, hint);
            record_element(ctx, value.ty, item_type);
            value
        } else {
            debug!("Unresolved list item expr");
            ctx.unresolved()
        };

        values.push(value);
    }

    if let Some(tail) = tail {
        let tail = tail.finish(ctx);

        // A list that starts with the spread is only what's built from there
        if values.is_empty() {
            return tail;
        }

        values.push(tail);
        nil_terminated = false;
    }

    let mut ty = ctx.builtins().nil.ty;

    for (i, value) in values.iter().rev().enumerate() {
        if !nil_terminated && i == 0 {
            ty = value.ty;
            continue;
        }
        ty = ctx.alloc_type(Type::Pair(Pair::new(value.ty, ty)));
    }

    let hir = ctx.alloc_hir(Hir::List(List {
        items: values.into_iter().map(|value| value.hir).collect(),
        nil_terminated,
    }));

    Value::new(hir, ty)
}

/// The part of a list from the first item that's spread before the last one. Each such spread is
/// a call to the builtin function that puts a list's items in front of the rest of the list (see
/// `Builtins::spread_list`), unless what's spread is a list literal, whose items are put in the
/// list directly.
struct Tail {
    /// The type this part of the list is expected to be, which each spread is compiled against.
    expected: Option<TypeId>,
    /// The type each item that isn't spread is expected to be.
    expected_item: Option<TypeId>,
    /// Each item, and whether it's spread.
    items: Vec<(HirId, bool)>,
    /// The types of the items, and of the items of each spread.
    item_types: Vec<TypeId>,
}

impl Tail {
    fn new(ctx: &mut Compiler, expected: Option<TypeId>) -> Self {
        let expected_item = expected
            .and_then(|ty| pair_parts(ctx, ty))
            .map(|(first, _)| first);

        Self {
            expected,
            expected_item,
            items: Vec::new(),
            item_types: Vec::new(),
        }
    }

    /// Compiles an item against what the earlier items refine it to, and records its type, as an
    /// item before the tail is, so that a lambda after a spread gets its parameter type in a
    /// generic call's argument.
    fn compile_item(
        &mut self,
        ctx: &mut Compiler,
        expr: Option<&AstExpr>,
        is_spread: bool,
        hint: bool,
    ) {
        let Some(expr) = expr else {
            debug!("Unresolved list item expr");
            self.items.push((ctx.unresolved().hir, is_spread));
            return;
        };

        if is_spread {
            let expected = self.expected;
            let value = ctx.with_expected_type_hint(hint, |ctx| compile_expr(ctx, expr, expected));
            record_element(ctx, value.ty, expected);

            let any = ctx.builtins().types.permissive_any;
            let any_list = list_of(ctx, any);
            ctx.assign_type(expr.syntax(), value.ty, any_list);

            self.item_types.extend(list_item_types(ctx, value.ty));
            self.items.push((value.hir, true));
        } else {
            let (value, item_type) = compile_refined_element(ctx, expr, self.expected_item, hint);
            record_element(ctx, value.ty, item_type);
            self.item_types.push(value.ty);
            self.items.push((value.hir, false));
        }
    }

    fn finish(mut self, ctx: &mut Compiler) -> Value {
        // The list is built from the end: the items after each spread are the rest it's put in
        // front of
        let mut rest = match self.items.last() {
            Some(&(hir, true)) => {
                self.items.pop();
                hir
            }
            _ => ctx.builtins().nil.hir,
        };
        let mut pending: Vec<HirId> = Vec::new();

        for (hir, spread) in self.items.into_iter().rev() {
            if !spread {
                pending.push(hir);
                continue;
            }

            if let Hir::List(literal) = ctx.hir(hir).clone()
                && literal.nil_terminated
            {
                pending.extend(literal.items.into_iter().rev());
                continue;
            }

            rest = prepend(ctx, &mut pending, rest);

            let spread_list = ctx.builtins().spread_list;
            let function = ctx.alloc_hir(Hir::Reference(spread_list));
            rest = ctx.alloc_hir(Hir::FunctionCall(FunctionCall {
                function,
                args: vec![hir, rest],
                nil_terminated: true,
            }));
        }

        let hir = prepend(ctx, &mut pending, rest);

        // The same type is only in the union once, so that a union of the same function type,
        // which can't be called, isn't made from a list of functions
        let mut members: Vec<TypeId> = Vec::new();

        for ty in self.item_types {
            if !members
                .iter()
                .any(|&member| ctx.is_assignable(ty, member) && ctx.is_assignable(member, ty))
            {
                members.push(ty);
            }
        }

        let item_type = ctx.alloc_type(Type::Union(Union::new(members)));
        let ty = list_of(ctx, item_type);

        Value::new(hir, ty)
    }
}

/// A list of the pending items, which were collected in reverse, in front of the rest.
fn prepend(ctx: &mut Compiler, pending: &mut Vec<HirId>, rest: HirId) -> HirId {
    if pending.is_empty() {
        return rest;
    }

    let mut items: Vec<HirId> = pending.drain(..).rev().collect();
    items.push(rest);

    ctx.alloc_hir(Hir::List(List {
        items,
        nil_terminated: false,
    }))
}

/// The types of the items of a list type, following its pairs until they end or come back to a
/// type they've already been, such as `T` for `List<T>`, or `1` and `2` for `[1, 2]`. The pairs of
/// each member of a union are followed separately. A type whose pairs go on for longer than that,
/// such as a very long tuple, has `Any` for the items past the limit, so the list's type is never
/// narrower than what it holds.
fn list_item_types(ctx: &mut Compiler, ty: TypeId) -> Vec<TypeId> {
    let mut item_types = Vec::new();
    // The types the pairs have gone through, other than plain pairs: a type the pairs come back to
    // is an alias or other type with pairs inside of it, such as `List<T>`, whose rest is itself,
    // or an alternating list, which comes back after two pairs. A plain pair, such as the rest of
    // a tuple, can't be one, so leaving them out keeps a long tuple from being compared with every
    // type before it
    let mut seen = vec![ty];
    let mut pending = vec![ty];
    let mut steps = 0;

    while let Some(current) = pending.pop() {
        for pair in rue_types::extract_pairs(ctx.types_mut(), current, false) {
            steps += 1;

            if steps > MAX_LIST_ITEM_TYPES {
                item_types.push(ctx.builtins().types.recursive_any);
                return item_types;
            }

            item_types.push(pair.first);

            if !matches!(ctx.ty(pair.rest), Type::Pair(_)) {
                if seen.iter().any(|&seen| {
                    ctx.is_assignable(pair.rest, seen) && ctx.is_assignable(seen, pair.rest)
                }) {
                    continue;
                }

                seen.push(pair.rest);
            }

            pending.push(pair.rest);
        }
    }

    item_types
}

const MAX_LIST_ITEM_TYPES: usize = 1024;
