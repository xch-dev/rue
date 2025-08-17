use std::str::FromStr;

use num_bigint::BigInt;
use rue_ast::AstLiteralType;
use rue_hir::{Atom, Type, TypeId};
use rue_parser::{SyntaxKind, T};

use crate::Compiler;

pub fn compile_literal_type(ctx: &mut Compiler, literal: &AstLiteralType) -> TypeId {
    let Some(value) = literal.value() else {
        return ctx.builtins().unresolved.ty;
    };

    match value.kind() {
        SyntaxKind::String => {
            let mut text = value.text();

            if let Some(stripped) = text.strip_prefix('"') {
                text = stripped;
            }

            if let Some(stripped) = text.strip_suffix('"') {
                text = stripped;
            }

            ctx.alloc_type(Type::Atom(Atom::StringValue(text.to_string())))
        }
        SyntaxKind::Hex => {
            let mut text = value.text();

            if let Some(stripped) = text.strip_prefix("0x") {
                text = stripped;
            }

            let text = text.replace("_", "");
            let bytes = hex::decode(text).unwrap();

            ctx.alloc_type(Type::Atom(if bytes.is_empty() {
                Atom::Nil
            } else {
                Atom::BytesValue(bytes.clone())
            }))
        }
        SyntaxKind::Integer => {
            let text = value.text().replace("_", "");
            let num = BigInt::from_str(&text).unwrap();
            ctx.alloc_type(Type::Atom(Atom::IntValue(num.clone())))
        }
        T![nil] => ctx.builtins().nil.ty,
        T![true] => ctx.builtins().true_value.ty,
        T![false] => ctx.builtins().false_value.ty,
        _ => unreachable!(),
    }
}
