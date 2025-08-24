use rue_ast::AstListExpr;
use rue_hir::{Hir, Value};
use rue_types::{Pair, Type, TypeId, Union};

use crate::{Compiler, compile_expr};

pub fn compile_list_expr(
    ctx: &mut Compiler,
    list: &AstListExpr,
    mut expected_type: Option<TypeId>,
) -> Value {
    let mut values = Vec::new();

    for item in list.items() {
        let item_type = if let Some(ty) = expected_type
            && let pairs = rue_types::extract_pairs(ctx.types_mut(), ty)
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

            expected_type = Some(rest);

            Some(first)
        } else {
            None
        };

        let value = if let Some(expr) = item.expr() {
            compile_expr(ctx, &expr, item_type)
        } else {
            ctx.builtins().unresolved.clone()
        };

        values.push(value);
    }

    let mut hir = ctx.builtins().nil.hir;
    let mut ty = ctx.builtins().nil.ty;

    for value in values.into_iter().rev() {
        hir = ctx.alloc_hir(Hir::Pair(value.hir, hir));
        ty = ctx.alloc_type(Type::Pair(Pair::new(value.ty, ty)));
    }

    Value::new(hir, ty)
}
