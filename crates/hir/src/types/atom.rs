use std::fmt;

use clvmr::Allocator;
use num_bigint::BigInt;
use rue_mir::{BinaryOp, UnaryOp};

use crate::{Builtins, Comparison, Database, Hir, HirId};

#[derive(Debug, Clone)]
pub enum Atom {
    Bytes,
    BytesValue(Vec<u8>, bool),
    Bytes32,
    PublicKey,
    Int,
    IntValue(BigInt),
    Nil,
    Bool,
    BoolValue(bool),
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bytes => write!(f, "Bytes"),
            Self::BytesValue(value, is_string) => {
                if *is_string {
                    write!(f, "\"{}\"", String::from_utf8_lossy(value))
                } else {
                    write!(f, "0x{}", hex::encode(value))
                }
            }
            Self::Bytes32 => write!(f, "Bytes32"),
            Self::PublicKey => write!(f, "PublicKey"),
            Self::Int => write!(f, "Int"),
            Self::IntValue(value) => write!(f, "{value}"),
            Self::Nil => write!(f, "nil"),
            Self::Bool => write!(f, "Bool"),
            Self::BoolValue(value) => write!(f, "{value}"),
        }
    }
}

pub fn compare_atoms(
    db: &mut Database,
    builtins: &Builtins,
    hir: HirId,
    from: Atom,
    to: Atom,
) -> Comparison {
    match (from, to) {
        (Atom::Bytes, Atom::Bytes) => Comparison::Assignable,
        (Atom::Bytes, Atom::Int) => Comparison::Castable,
        (Atom::BytesValue(..), Atom::Bytes) => Comparison::Assignable,
        (Atom::BytesValue(..), Atom::Int) => Comparison::Castable,
        (Atom::Int, Atom::Int) => Comparison::Assignable,
        (Atom::Int, Atom::Bytes) => Comparison::Castable,
        (Atom::IntValue(..), Atom::Int) => Comparison::Assignable,
        (Atom::IntValue(..), Atom::Bytes) => Comparison::Castable,
        (Atom::Bool, Atom::Bool) => Comparison::Assignable,
        (Atom::Bool, Atom::Bytes) => Comparison::Castable,
        (Atom::Bool, Atom::Int) => Comparison::Castable,
        (Atom::Bool, Atom::Bytes32) => Comparison::Incompatible,
        (Atom::Bool, Atom::PublicKey) => Comparison::Incompatible,
        (Atom::BoolValue(..), Atom::Bool) => Comparison::Assignable,
        (Atom::BoolValue(..), Atom::Int) => Comparison::Castable,
        (Atom::BoolValue(..), Atom::Bytes) => Comparison::Castable,
        (Atom::BoolValue(..), Atom::Bytes32) => Comparison::Incompatible,
        (Atom::BoolValue(..), Atom::PublicKey) => Comparison::Incompatible,
        (Atom::Bytes32, Atom::Bytes32) => Comparison::Assignable,
        (Atom::Bytes32, Atom::Bytes) => Comparison::Assignable,
        (Atom::Bytes32, Atom::Int) => Comparison::Castable,
        (Atom::Bytes32, Atom::Bool) => Comparison::Incompatible,
        (Atom::Bytes32, Atom::BoolValue(..)) => Comparison::Incompatible,
        (Atom::Bytes32, Atom::PublicKey) => Comparison::Incompatible,
        (Atom::Bytes32, Atom::Nil) => Comparison::Incompatible,
        (Atom::PublicKey, Atom::PublicKey) => Comparison::Assignable,
        (Atom::PublicKey, Atom::Bytes) => Comparison::Castable,
        (Atom::PublicKey, Atom::Int) => Comparison::Castable,
        (Atom::PublicKey, Atom::Bool) => Comparison::Incompatible,
        (Atom::PublicKey, Atom::BoolValue(..)) => Comparison::Incompatible,
        (Atom::PublicKey, Atom::Bytes32) => Comparison::Incompatible,
        (Atom::PublicKey, Atom::Nil) => Comparison::Incompatible,
        (Atom::Nil, Atom::Nil) => Comparison::Assignable,
        (Atom::Nil, Atom::Bytes) => Comparison::Assignable,
        (Atom::Nil, Atom::Int) => Comparison::Castable,
        (Atom::Nil, Atom::Bytes32) => Comparison::Incompatible,
        (Atom::Nil, Atom::PublicKey) => Comparison::Incompatible,
        (Atom::Nil, Atom::Bool) => Comparison::Castable,
        (Atom::Bytes | Atom::Int, Atom::Bytes32) => {
            let strlen = db.alloc_hir(Hir::Unary(UnaryOp::Strlen, hir));
            let len = db.alloc_hir(Hir::Int(BigInt::from(32)));
            let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, strlen, len));
            Comparison::Constrainable(eq)
        }
        (Atom::Bytes | Atom::Int, Atom::PublicKey) => {
            let strlen = db.alloc_hir(Hir::Unary(UnaryOp::Strlen, hir));
            let len = db.alloc_hir(Hir::Int(BigInt::from(48)));
            let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, strlen, len));
            Comparison::Constrainable(eq)
        }
        (Atom::Bytes | Atom::Int, Atom::Bool) => {
            let eq_true = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, builtins.true_value.hir));
            let eq_false = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, builtins.false_value.hir));
            let or = db.alloc_hir(Hir::Binary(BinaryOp::Or, eq_true, eq_false));
            Comparison::Constrainable(or)
        }
        (Atom::Bytes | Atom::Int, Atom::BytesValue(value, _)) => {
            let atom = db.alloc_hir(Hir::Bytes(value.clone()));
            let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
            Comparison::Constrainable(eq)
        }
        (Atom::Bool, Atom::BytesValue(value, _)) => {
            if value.is_empty() || value == [1] {
                let atom = db.alloc_hir(Hir::Bytes(value.clone()));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(eq)
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::Bool, Atom::IntValue(value)) => {
            let atom = bigint_atom(value.clone());
            if atom.is_empty() || atom == [1] {
                let atom = db.alloc_hir(Hir::Bytes(atom));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(eq)
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::Bytes | Atom::Int, Atom::IntValue(value)) => {
            let atom = db.alloc_hir(Hir::Int(value.clone()));
            let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
            Comparison::Constrainable(eq)
        }
        (Atom::Bytes | Atom::Int | Atom::Bool, Atom::BoolValue(value)) => {
            let atom = db.alloc_hir(Hir::Bool(value));
            let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
            Comparison::Constrainable(eq)
        }
        (Atom::Bytes | Atom::Int | Atom::Bool, Atom::Nil) => {
            let atom = db.alloc_hir(Hir::Nil);
            let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
            Comparison::Constrainable(eq)
        }
        (Atom::BytesValue(from, _), Atom::BytesValue(to, _)) => {
            if from == to {
                Comparison::Assignable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::IntValue(from), Atom::IntValue(to)) => {
            if bigint_atom(from.clone()) == bigint_atom(to.clone()) {
                Comparison::Assignable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::BoolValue(from), Atom::BoolValue(to)) => {
            if from == to {
                Comparison::Assignable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::BytesValue(from, _), Atom::Bytes32) => {
            if from.len() == 32 {
                Comparison::Assignable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::IntValue(from), Atom::Bytes32) => {
            if bigint_atom(from.clone()).len() == 32 {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::BytesValue(from, _), Atom::PublicKey) => {
            if from.len() == 48 {
                Comparison::Assignable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::IntValue(from), Atom::PublicKey) => {
            if bigint_atom(from.clone()).len() == 48 {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::BytesValue(bytes, _), Atom::Nil) | (Atom::Nil, Atom::BytesValue(bytes, _)) => {
            if bytes.is_empty() {
                Comparison::Assignable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::IntValue(int), Atom::Nil) | (Atom::Nil, Atom::IntValue(int)) => {
            if bigint_atom(int.clone()).is_empty() {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::BytesValue(from, _), Atom::Bool) => {
            if from.is_empty() || from == [1] {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::IntValue(from), Atom::Bool) => {
            let atom = bigint_atom(from.clone());
            if atom.is_empty() || atom == [1] {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::BytesValue(from, _), Atom::BoolValue(value))
        | (Atom::BoolValue(value), Atom::BytesValue(from, _)) => {
            if (from.is_empty() && !value) || (from == [1] && value) {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::IntValue(from), Atom::BoolValue(value))
        | (Atom::BoolValue(value), Atom::IntValue(from)) => {
            let atom = bigint_atom(from.clone());
            if (atom.is_empty() && !value) || (atom == [1] && value) {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::BytesValue(bytes, _), Atom::IntValue(int))
        | (Atom::IntValue(int), Atom::BytesValue(bytes, _)) => {
            if bigint_atom(int.clone()) == bytes {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::Bytes32, Atom::BytesValue(bytes, _)) => {
            if bytes.len() == 32 {
                let atom = db.alloc_hir(Hir::Bytes(bytes.clone()));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(eq)
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::PublicKey, Atom::BytesValue(bytes, _)) => {
            if bytes.len() == 48 {
                let atom = db.alloc_hir(Hir::Bytes(bytes.clone()));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(eq)
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::Bytes32, Atom::IntValue(int)) => {
            let atom = bigint_atom(int.clone());
            if atom.len() == 32 {
                let atom = db.alloc_hir(Hir::Bytes(atom));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(eq)
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::PublicKey, Atom::IntValue(int)) => {
            let atom = bigint_atom(int.clone());
            if atom.len() == 48 {
                let atom = db.alloc_hir(Hir::Bytes(atom));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(eq)
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::Nil, Atom::BoolValue(value)) | (Atom::BoolValue(value), Atom::Nil) => {
            if value {
                Comparison::Incompatible
            } else {
                Comparison::Castable
            }
        }
    }
}

pub fn bigint_atom(value: BigInt) -> Vec<u8> {
    let mut allocator = Allocator::new();
    let atom = allocator.new_number(value).unwrap();
    allocator.atom(atom).to_vec()
}
