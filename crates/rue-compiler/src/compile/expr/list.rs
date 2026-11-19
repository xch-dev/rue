use log::debug;
use rue_ast::AstListExpr;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Hir, List, Value};
use rue_types::{Pair, Type, TypeId, Union};

use super::call_inference::{
    compile_element, holds_other_pairs, record_element, refine_element_expected,
};
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

            Some(first)
        } else {
            None
        };

        let value = if let Some(expr) = item.expr() {
            let (item_type, fallback) = match item_type {
                Some(expected) => {
                    let (expected, fallback) = refine_element_expected(ctx, expected, &expr);
                    (Some(expected), fallback)
                }
                None => (None, false),
            };
            let fallback = fallback || ctx.is_expected_type_fallback();

            let value = ctx.with_expected_type_fallback(fallback, |ctx| {
                ctx.with_expected_type_hint(hint, |ctx| compile_element(ctx, &expr, item_type))
            });
            record_element(ctx, value.ty, item_type);
            value
        } else {
            debug!("Unresolved list item expr");
            ctx.builtins().unresolved.clone()
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
