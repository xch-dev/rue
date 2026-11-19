use log::debug;
use rue_ast::AstPairExpr;
use rue_hir::{Hir, Value};
use rue_types::{Pair, Type, TypeId, Union};

use super::call_inference::{
    compile_element, compile_refined_element, holds_other_pairs, record_element,
};
use crate::Compiler;

pub fn compile_pair_expr(
    ctx: &mut Compiler,
    pair: &AstPairExpr,
    expected_type: Option<TypeId>,
) -> Value {
    let (expected_first, expected_rest) = if let Some(ty) = expected_type
        && let pairs = rue_types::extract_pairs(ctx.types_mut(), ty, false)
        && !pairs.is_empty()
    {
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

        (Some(first), Some(rest))
    } else {
        (None, None)
    };

    let hint =
        ctx.is_expected_type_hint() || expected_type.is_some_and(|ty| holds_other_pairs(ctx, ty));

    let first = if let Some(first) = pair.first() {
        ctx.with_expected_type_hint(hint, |ctx| compile_element(ctx, &first, expected_first))
    } else {
        debug!("Unresolved pair first");
        ctx.builtins().unresolved.clone()
    };

    record_element(ctx, first.ty, expected_first);

    let rest = if let Some(rest) = pair.rest() {
        compile_refined_element(ctx, &rest, expected_rest, hint).0
    } else {
        debug!("Unresolved pair rest");
        ctx.builtins().unresolved.clone()
    };

    let hir = ctx.alloc_hir(Hir::Pair(first.hir, rest.hir));
    let ty = ctx.alloc_type(Type::Pair(Pair::new(first.ty, rest.ty)));

    Value::new(hir, ty)
}
