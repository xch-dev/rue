use log::debug;
use rue_ast::AstPairExpr;
use rue_hir::{Hir, Value};
use rue_types::{Pair, Type, TypeId};

use super::call_inference::{
    compile_element, compile_refined_element, holds_other_pairs, pair_parts, record_element,
    record_rest,
};
use crate::Compiler;

pub fn compile_pair_expr(
    ctx: &mut Compiler,
    pair: &AstPairExpr,
    expected_type: Option<TypeId>,
) -> Value {
    let (expected_first, expected_rest) = expected_type.and_then(|ty| pair_parts(ctx, ty)).unzip();

    let hint =
        ctx.is_expected_type_hint() || expected_type.is_some_and(|ty| holds_other_pairs(ctx, ty));

    let first = if let Some(first) = pair.first() {
        ctx.with_expected_type_hint(hint, |ctx| compile_element(ctx, &first, expected_first))
    } else {
        debug!("Unresolved pair first");
        ctx.unresolved()
    };

    record_element(ctx, first.ty, expected_first);

    let rest = if let Some(rest) = pair.rest() {
        let (value, expected_rest) = compile_refined_element(ctx, &rest, expected_rest, hint);

        record_rest(ctx, value.ty, expected_rest);

        value
    } else {
        debug!("Unresolved pair rest");
        ctx.unresolved()
    };

    let hir = ctx.alloc_hir(Hir::Pair(first.hir, rest.hir));
    let ty = ctx.alloc_type(Type::Pair(Pair::new(first.ty, rest.ty)));

    Value::new(hir, ty)
}
