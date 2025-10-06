use std::{borrow::Cow, str::FromStr};

use log::debug;
use num_bigint::BigInt;
use num_traits::Num;
use rue_ast::AstLiteralType;
use rue_lir::bigint_atom;
use rue_parser::{SyntaxKind, T};
use rue_types::{Atom, AtomRestriction, AtomSemantic, Type, TypeId};

use crate::Compiler;

pub fn compile_literal_type(ctx: &mut Compiler, literal: &AstLiteralType) -> TypeId {
    let Some(value) = literal.value() else {
        debug!("Unresolved literal type");
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
                AtomSemantic::String,
                Some(AtomRestriction::Value(Cow::Owned(text.as_bytes().to_vec()))),
            )))
        }
        SyntaxKind::Hex => {
            let mut text = value.text();

            if let Some(stripped) = text.strip_prefix("0x") {
                text = stripped;
            }

            let text = text.replace('_', "");
            let bytes = hex::decode(text).unwrap();

            ctx.alloc_type(Type::Atom(if bytes.is_empty() {
                Atom::NIL
            } else {
                Atom::new(
                    if bytes.len() == 48 {
                        AtomSemantic::PublicKey
                    } else if bytes.len() == 96 {
                        AtomSemantic::Signature
                    } else {
                        AtomSemantic::Bytes
                    },
                    Some(AtomRestriction::Value(Cow::Owned(bytes))),
                )
            }))
        }
        SyntaxKind::Binary => {
            let mut text = value.text();

            if let Some(stripped) = text.strip_prefix("0b") {
                text = stripped;
            }

            let text = text.replace('_', "");
            let bigint = if text.is_empty() {
                BigInt::ZERO
            } else {
                BigInt::from_str_radix(&text, 2).expect("invalid binary literal")
            };

            ctx.alloc_type(Type::Atom(Atom::new(
                AtomSemantic::Int,
                Some(AtomRestriction::Value(Cow::Owned(bigint_atom(bigint)))),
            )))
        }
        SyntaxKind::Octal => {
            let mut text = value.text();

            if let Some(stripped) = text.strip_prefix("0o") {
                text = stripped;
            }

            let text = text.replace('_', "");
            let bigint = if text.is_empty() {
                BigInt::ZERO
            } else {
                BigInt::from_str_radix(&text, 8).expect("invalid octal literal")
            };

            ctx.alloc_type(Type::Atom(Atom::new(
                AtomSemantic::Int,
                Some(AtomRestriction::Value(Cow::Owned(bigint_atom(bigint)))),
            )))
        }
        SyntaxKind::Integer => {
            let text = value.text().replace('_', "");
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
