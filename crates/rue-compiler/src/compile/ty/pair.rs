use rue_ast::AstPairType;
use rue_types::{Pair, Type, TypeId};

use crate::{Compiler, compile_type};

pub fn compile_pair_type(ctx: &mut Compiler, pair: &AstPairType) -> TypeId {
    let first = if let Some(first) = pair.first() {
        compile_type(ctx, &first)
    } else {
        ctx.builtins().unresolved.ty
    };

    let rest = if let Some(rest) = pair.rest() {
        compile_type(ctx, &rest)
    } else {
        ctx.builtins().unresolved.ty
    };

    ctx.alloc_type(Type::Pair(Pair::new(first, rest)))
}
