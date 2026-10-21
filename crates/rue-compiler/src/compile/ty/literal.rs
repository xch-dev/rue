use log::debug;
use rue_ast::AstLiteralType;
use rue_types::{Type, TypeId};

use crate::{Compiler, LiteralValue};

pub fn compile_literal_type(ctx: &mut Compiler, literal: &AstLiteralType) -> TypeId {
    let Some(value) = literal.value() else {
        debug!("Unresolved literal type");
        return ctx.builtins().unresolved.ty;
    };

    match LiteralValue::decode(&value) {
        LiteralValue::Nil => ctx.builtins().nil.ty,
        LiteralValue::True => ctx.builtins().true_value.ty,
        LiteralValue::False => ctx.builtins().false_value.ty,
        value => ctx.alloc_type(Type::Atom(value.atom())),
    }
}
