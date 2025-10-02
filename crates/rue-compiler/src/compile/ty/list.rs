use log::debug;
use rue_ast::AstListType;
use rue_diagnostic::DiagnosticKind;
use rue_types::{Pair, Type, TypeId};

use crate::{Compiler, compile_type};

pub fn compile_list_type(ctx: &mut Compiler, list: &AstListType) -> TypeId {
    let mut types = Vec::new();
    let mut nil_terminated = true;

    let len = list.items().count();

    for (i, item) in list.items().enumerate() {
        if let Some(spread) = item.spread() {
            if i == len - 1 {
                nil_terminated = false;
            } else {
                ctx.diagnostic(&spread, DiagnosticKind::NonFinalSpread);
            }
        }

        let ty = if let Some(ty) = item.ty() {
            compile_type(ctx, &ty)
        } else {
            debug!("Unresolved list item type");
            ctx.builtins().unresolved.ty
        };

        types.push(ty);
    }

    let mut result = ctx.builtins().nil.ty;

    for (i, ty) in types.into_iter().rev().enumerate() {
        if !nil_terminated && i == 0 {
            result = ty;
        } else {
            result = ctx.alloc_type(Type::Pair(Pair::new(ty, result)));
        }
    }

    result
}
