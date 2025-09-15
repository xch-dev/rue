use log::debug;
use rue_ast::AstGroupType;
use rue_types::TypeId;

use crate::{Compiler, compile_type};

pub fn compile_group_type(ctx: &mut Compiler, group: &AstGroupType) -> TypeId {
    let Some(ty) = group.ty() else {
        debug!("Unresolved group type");
        return ctx.builtins().unresolved.ty;
    };
    compile_type(ctx, &ty)
}
