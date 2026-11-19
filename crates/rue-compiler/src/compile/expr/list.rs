use log::debug;
use rue_ast::AstListExpr;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Hir, List, Value};
use rue_types::{Pair, Type, TypeId, Union};

use super::call_inference::{compile_refined_element, holds_other_pairs, record_element};
use crate::Compiler;

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

    // The type the last item was expected to be, which an item past the end of the pairs the
    // expected type has gets (see below)
    let mut last_item_type = None;

    for (i, item) in list.items().enumerate() {
        let is_spread = if let Some(spread) = item.spread() {
            if i == len - 1 {
                true
            } else {
                ctx.diagnostic(&spread, DiagnosticKind::NonFinalSpread);
                false
            }
        } else {
            false
        };

        let item_type = if is_spread {
            nil_terminated = false;
            expected_type
        } else if let Some(ty) = expected_type
            && let pairs = rue_types::extract_pairs(ctx.types_mut(), ty, false)
            && !pairs.is_empty()
        {
            hint |= holds_other_pairs(ctx, ty);

            let first = if pairs.len() == 1 {
                pairs[0].first
            } else {
                ctx.alloc_type(Type::Union(Union::new(
                    pairs.iter().map(|pair| pair.first).collect(),
                )))
            };

            let rest = if pairs.len() == 1 {
                pairs[0].rest
            } else {
                ctx.alloc_type(Type::Union(Union::new(
                    pairs.iter().map(|pair| pair.rest).collect(),
                )))
            };

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
