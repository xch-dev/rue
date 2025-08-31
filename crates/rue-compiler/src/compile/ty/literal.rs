use std::{borrow::Cow, str::FromStr};

use num_bigint::BigInt;
use rue_ast::AstLiteralType;
use rue_lir::bigint_atom;
use rue_parser::{SyntaxKind, T};
use rue_types::{Atom, AtomRestriction, AtomSemantic, Type, TypeId};

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

            ctx.alloc_type(Type::Atom(Atom::new(
                AtomSemantic::Bytes,
                Some(AtomRestriction::Value(Cow::Owned(text.as_bytes().to_vec()))),
            )))
        }
        SyntaxKind::Hex => {
            let mut text = value.text();

            if let Some(stripped) = text.strip_prefix("0x") {
                text = stripped;
            }

            let text = text.replace("_", "");
            let bytes = hex::decode(text).unwrap();

            ctx.alloc_type(Type::Atom(if bytes.is_empty() {
                Atom::NIL
            } else {
                Atom::new(
                    if bytes.len() == 48 {
                        AtomSemantic::PublicKey
                    } else {
                        AtomSemantic::Bytes
                    },
                    Some(AtomRestriction::Value(Cow::Owned(bytes))),
                )
            }))
        }
        SyntaxKind::Integer => {
            let text = value.text().replace("_", "");
            let num = BigInt::from_str(&text).unwrap();
            ctx.alloc_type(Type::Atom(Atom::new(
                AtomSemantic::Int,
                Some(AtomRestriction::Value(Cow::Owned(bigint_atom(num)))),
            )))
        }
        T![nil] => ctx.builtins().nil.ty,
        T![true] => ctx.builtins().true_value.ty,
        T![false] => ctx.builtins().false_value.ty,
        _ => unreachable!(),
    }
}
