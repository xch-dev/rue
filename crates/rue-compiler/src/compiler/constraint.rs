use num_bigint::BigInt;

use crate::{value::Type, TypeId};

use super::Compiler;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constraint {
    Atom,
    Pair,
    Length(usize),
    Discriminant(BigInt),
    Or(Vec<Constraint>),
}

/**
 * Bytes => Bytes32
 * Atom => Atom, Length(32)
 * (= (strlen hir) 32)
 *
 * Bytes => PublicKey | Bytes
 * Atom => Atom, Or(Length(48), Length(32))
 * (or (= (strlen hir) 48) (= (strlen hir) 32)
 *
 * Enum => Variant
 */

impl Compiler<'_> {
    fn type_immediate_constraints(&self, type_id: TypeId) -> Vec<Constraint> {
        match self.db.ty(type_id) {
            Type::Unknown | Type::Any | Type::Function(..) => Vec::new(),
            Type::Bytes | Type::Int => vec![Constraint::Atom],
            Type::Pair(..) => vec![Constraint::Pair],
            Type::Bytes32 => vec![Constraint::Atom, Constraint::Length(32)],
            Type::PublicKey => vec![Constraint::Atom, Constraint::Length(48)],
            Type::Bool => vec![
                Constraint::Atom,
                Constraint::Or(vec![Constraint::Length(0), Constraint::Length(1)]),
            ],
            Type::Nil => vec![Constraint::Atom, Constraint::Length(0)],
            Type::Enum(enum_type) => {
                let mut variants = Vec::new();

                for variant_type in enum_type.variants.values() {
                    let Type::EnumVariant(variant_type) = self.db.ty(*variant_type) else {
                        unreachable!();
                    };
                    variants.push(Constraint::Discriminant(variant_type.discriminant.clone()));
                }

                vec![
                    if enum_type.has_fields {
                        Constraint::Pair
                    } else {
                        Constraint::Atom
                    },
                    Constraint::Or(variants),
                ]
            }
            Type::EnumVariant(variant_type) => {
                let Type::Enum(enum_type) = self.db.ty(variant_type.enum_type) else {
                    unreachable!();
                };
                vec![if enum_type.has_fields {
                    Constraint::Pair
                } else {
                    Constraint::Atom
                }]
            }
        }
    }
}
