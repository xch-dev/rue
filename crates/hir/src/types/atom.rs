use std::fmt;

use clvmr::Allocator;
use num_bigint::BigInt;

use crate::{BinaryOp, Builtins, Comparison, Constraint, Database, Hir, HirId, TypeId, UnaryOp};

#[derive(Debug, Clone)]
pub enum Atom {
    Bytes,
    BytesValue(Vec<u8>),
    StringValue(String),
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
            Self::BytesValue(value) => {
                if value.is_empty() {
                    write!(f, "nil")
                } else {
                    write!(f, "0x{}", hex::encode(value))
                }
            }
            Self::StringValue(value) => write!(f, "\"{value}\""),
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
    from_id: TypeId,
    to_id: TypeId,
    from: Atom,
    to: Atom,
) -> Comparison {
    match (from, to) {
        (Atom::Bytes, Atom::Bytes) => Comparison::Assignable,
        (Atom::Bytes, Atom::Int) => Comparison::Castable,
        (Atom::BytesValue(..) | Atom::StringValue(..), Atom::Bytes) => Comparison::Assignable,
        (Atom::BytesValue(..) | Atom::StringValue(..), Atom::Int) => Comparison::Castable,
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
            Comparison::Constrainable(Constraint::to(eq, from_id, to_id))
        }
        (Atom::Bytes | Atom::Int, Atom::PublicKey) => {
            let strlen = db.alloc_hir(Hir::Unary(UnaryOp::Strlen, hir));
            let len = db.alloc_hir(Hir::Int(BigInt::from(48)));
            let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, strlen, len));
            Comparison::Constrainable(Constraint::to(eq, from_id, to_id))
        }
        (Atom::Bytes | Atom::Int, Atom::Bool) => {
            let eq_true = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, builtins.true_value.hir));
            let eq_false = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, builtins.false_value.hir));
            let or = db.alloc_hir(Hir::Binary(BinaryOp::Or, eq_true, eq_false));
            Comparison::Constrainable(Constraint::to(or, from_id, to_id))
        }
        (Atom::Bytes | Atom::Int, Atom::BytesValue(value)) => {
            let atom = db.alloc_hir(Hir::Bytes(value.clone()));
            let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
            Comparison::Constrainable(Constraint::to(eq, from_id, to_id))
        }
        (Atom::Bool, Atom::BytesValue(value)) => {
            if value.is_empty() || value == [1] {
                let atom = db.alloc_hir(Hir::Bytes(value.clone()));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(Constraint::if_else(
                    eq,
                    from_id,
                    to_id,
                    if value.is_empty() {
                        builtins.true_value.ty
                    } else {
                        builtins.false_value.ty
                    },
                ))
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::Bytes | Atom::Int, Atom::StringValue(value)) => {
            let atom = db.alloc_hir(Hir::String(value.clone()));
            let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
            Comparison::Constrainable(Constraint::to(eq, from_id, to_id))
        }
        (Atom::Bool, Atom::StringValue(value)) => {
            let bytes = value.as_bytes().to_vec();
            if bytes.is_empty() || bytes == [1] {
                let atom = db.alloc_hir(Hir::String(value));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(Constraint::if_else(
                    eq,
                    from_id,
                    to_id,
                    if bytes.is_empty() {
                        builtins.true_value.ty
                    } else {
                        builtins.false_value.ty
                    },
                ))
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::Bool, Atom::IntValue(value)) => {
            let bytes = bigint_atom(value.clone());
            if bytes.is_empty() || bytes == [1] {
                let atom = db.alloc_hir(Hir::Int(value.clone()));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(Constraint::if_else(
                    eq,
                    from_id,
                    to_id,
                    if bytes.is_empty() {
                        builtins.true_value.ty
                    } else {
                        builtins.false_value.ty
                    },
                ))
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::Bytes | Atom::Int, Atom::IntValue(value)) => {
            let atom = db.alloc_hir(Hir::Int(value.clone()));
            let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
            Comparison::Constrainable(Constraint::to(eq, from_id, to_id))
        }
        (Atom::Bytes | Atom::Int, Atom::BoolValue(value)) => {
            let atom = db.alloc_hir(Hir::Bool(value));
            let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
            Comparison::Constrainable(Constraint::to(eq, from_id, to_id))
        }
        (Atom::Bool, Atom::BoolValue(value)) => {
            let atom = db.alloc_hir(Hir::Bool(value));
            let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
            Comparison::Constrainable(Constraint::if_else(
                eq,
                from_id,
                to_id,
                if value {
                    builtins.false_value.ty
                } else {
                    builtins.true_value.ty
                },
            ))
        }
        (Atom::Bytes | Atom::Int, Atom::Nil) => {
            let atom = db.alloc_hir(Hir::Nil);
            let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
            Comparison::Constrainable(Constraint::to(eq, from_id, to_id))
        }
        (Atom::Bool, Atom::Nil) => {
            let atom = db.alloc_hir(Hir::Nil);
            let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
            Comparison::Constrainable(Constraint::if_else(
                eq,
                from_id,
                to_id,
                builtins.true_value.ty,
            ))
        }
        (Atom::BytesValue(from), Atom::BytesValue(to)) => {
            if from == to {
                Comparison::Assignable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::StringValue(from), Atom::StringValue(to)) => {
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
        (Atom::BytesValue(from), Atom::Bytes32) => {
            if from.len() == 32 {
                Comparison::Assignable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::StringValue(from), Atom::Bytes32) => {
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
        (Atom::BytesValue(from), Atom::PublicKey) => {
            if from.len() == 48 {
                Comparison::Assignable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::StringValue(from), Atom::PublicKey) => {
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
        (Atom::BytesValue(bytes), Atom::Nil) | (Atom::Nil, Atom::BytesValue(bytes)) => {
            if bytes.is_empty() {
                Comparison::Assignable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::StringValue(value), Atom::Nil) | (Atom::Nil, Atom::StringValue(value)) => {
            if value.is_empty() {
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
        (Atom::BytesValue(from), Atom::Bool) => {
            if from.is_empty() || from == [1] {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::StringValue(from), Atom::Bool) => {
            if from.is_empty() || from.as_bytes() == [1] {
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
        (Atom::BytesValue(from), Atom::BoolValue(value))
        | (Atom::BoolValue(value), Atom::BytesValue(from)) => {
            if (from.is_empty() && !value) || (from == [1] && value) {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::StringValue(from), Atom::BoolValue(value))
        | (Atom::BoolValue(value), Atom::StringValue(from)) => {
            if (from.is_empty() && !value) || (from.as_bytes() == [1] && value) {
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
        (Atom::BytesValue(bytes), Atom::IntValue(int))
        | (Atom::IntValue(int), Atom::BytesValue(bytes)) => {
            if bigint_atom(int.clone()) == bytes {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::StringValue(string), Atom::IntValue(int))
        | (Atom::IntValue(int), Atom::StringValue(string)) => {
            if bigint_atom(int.clone()) == string.as_bytes() {
                Comparison::Castable
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::Bytes32, Atom::BytesValue(bytes)) => {
            if bytes.len() == 32 {
                let atom = db.alloc_hir(Hir::Bytes(bytes.clone()));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(Constraint::to(eq, from_id, to_id))
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::Bytes32, Atom::StringValue(string)) => {
            if string.len() == 32 {
                let atom = db.alloc_hir(Hir::String(string));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(Constraint::to(eq, from_id, to_id))
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::PublicKey, Atom::BytesValue(bytes)) => {
            if bytes.len() == 48 {
                let atom = db.alloc_hir(Hir::Bytes(bytes.clone()));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(Constraint::to(eq, from_id, to_id))
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::PublicKey, Atom::StringValue(value)) => {
            if value.len() == 48 {
                let atom = db.alloc_hir(Hir::String(value));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(Constraint::to(eq, from_id, to_id))
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::Bytes32, Atom::IntValue(int)) => {
            let atom = bigint_atom(int.clone());
            if atom.len() == 32 {
                let atom = db.alloc_hir(Hir::Int(int.clone()));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(Constraint::to(eq, from_id, to_id))
            } else {
                Comparison::Incompatible
            }
        }
        (Atom::PublicKey, Atom::IntValue(int)) => {
            let atom = bigint_atom(int.clone());
            if atom.len() == 48 {
                let atom = db.alloc_hir(Hir::Int(int.clone()));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(Constraint::to(eq, from_id, to_id))
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
        (Atom::BytesValue(bytes), Atom::StringValue(string))
        | (Atom::StringValue(string), Atom::BytesValue(bytes)) => {
            if bytes == string.as_bytes().to_vec() {
                Comparison::Assignable
            } else {
                Comparison::Incompatible
            }
        }
    }
}

pub fn bigint_atom(value: BigInt) -> Vec<u8> {
    let mut allocator = Allocator::new();
    let atom = allocator.new_number(value).unwrap();
    allocator.atom(atom).to_vec()
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};

    use crate::{BindingSymbol, Symbol, Type, tests::debug_hir};

    use super::*;

    use Atom::*;

    fn check(from: Atom, to: Atom, expect: Expect) {
        let mut db = Database::new();
        let builtins = Builtins::new(&mut db);

        let symbol = db.alloc_symbol(Symbol::Binding(BindingSymbol {
            name: None,
            ty: builtins.nil.ty,
            value: builtins.nil.hir,
        }));

        let hir = db.alloc_hir(Hir::Reference(symbol));

        let from_id = db.alloc_type(Type::Atom(from.clone()));
        let to_id = db.alloc_type(Type::Atom(to.clone()));

        let comparison = compare_atoms(&mut db, &builtins, hir, from_id, to_id, from, to);

        let value = match comparison {
            Comparison::Assignable => "assign".to_string(),
            Comparison::Castable => "cast".to_string(),
            Comparison::Constrainable(constraint) => debug_hir(&db, constraint.hir),
            Comparison::Incompatible => "fail".to_string(),
        };

        expect.assert_eq(&value);
    }

    #[test]
    fn test_nil() {
        check(Nil, Nil, expect!["assign"]);
        check(Nil, Bytes, expect!["assign"]);
        check(Nil, Int, expect!["cast"]);
        check(Int, Nil, expect!["(Id { idx: 0 } == nil)"]);
        check(Bytes, Nil, expect!["(Id { idx: 0 } == nil)"]);
        check(Bytes32, Nil, expect!["fail"]);
    }

    #[test]
    fn test_bool() {
        check(Bool, Bool, expect!["assign"]);
        check(Bool, Bytes, expect!["cast"]);
        check(Bool, Int, expect!["cast"]);
        check(Bool, Bytes32, expect!["fail"]);
        check(Bool, PublicKey, expect!["fail"]);
        check(Bool, Nil, expect!["(Id { idx: 0 } == nil)"]);
        check(Nil, Bool, expect!["cast"]);
        check(
            Int,
            Bool,
            expect!["((Id { idx: 0 } == true) || (Id { idx: 0 } == false))"],
        );
        check(BoolValue(true), Bool, expect!["assign"]);
        check(BoolValue(false), Bool, expect!["assign"]);
        check(Bool, BoolValue(true), expect!["(Id { idx: 0 } == true)"]);
        check(Bool, BoolValue(false), expect!["(Id { idx: 0 } == false)"]);
        check(BoolValue(true), Int, expect!["cast"]);
    }

    #[test]
    fn test_bytes() {
        check(Bytes, Bytes, expect!["assign"]);
        check(Bytes32, Bytes, expect!["assign"]);
        check(Bytes, Bytes32, expect!["((strlen Id { idx: 0 }) == 32)"]);
        check(Bytes32, Bytes32, expect!["assign"]);
        check(StringValue(String::new()), Bytes, expect!["assign"]);
        check(Bytes, BytesValue(vec![]), expect!["(Id { idx: 0 } == nil)"]);
        check(
            Bytes,
            StringValue("hello".to_string()),
            expect![[r#"(Id { idx: 0 } == "hello")"#]],
        );
        check(Bytes32, StringValue("hello".to_string()), expect!["fail"]);
    }

    #[test]
    fn test_int() {
        check(Bytes, Int, expect!["cast"]);
        check(Int, Bytes, expect!["cast"]);
        check(Int, Bytes32, expect!["((strlen Id { idx: 0 }) == 32)"]);
        check(Bytes32, Int, expect!["cast"]);
        check(IntValue(BigInt::from(1)), Int, expect!["assign"]);
        check(
            Int,
            IntValue(BigInt::from(1)),
            expect!["(Id { idx: 0 } == 1)"],
        );
    }
}
