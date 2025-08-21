use std::fmt;

use clvmr::Allocator;
use num_bigint::BigInt;

use crate::{Check, Comparison, Constraint};

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
            Self::BoolValue(value) => write!(f, "{value}"),
        }
    }
}

pub fn compare_atoms(from: Atom, to: Atom) -> Comparison {
    match (from, to) {
        (Atom::Bytes, Atom::Bytes) => Comparison::Assignable,
        (Atom::Bytes, Atom::Int) => Comparison::Castable,
        (Atom::BytesValue(..) | Atom::StringValue(..), Atom::Bytes) => Comparison::Assignable,
        (Atom::BytesValue(..) | Atom::StringValue(..), Atom::Int) => Comparison::Castable,
        (Atom::Int, Atom::Int) => Comparison::Assignable,
        (Atom::Int, Atom::Bytes) => Comparison::Castable,
        (Atom::IntValue(..), Atom::Int) => Comparison::Assignable,
        (Atom::IntValue(..), Atom::Bytes) => Comparison::Castable,
        (Atom::BoolValue(..), Atom::Int) => Comparison::Castable,
        (Atom::BoolValue(..), Atom::Bytes) => Comparison::Castable,
        (Atom::BoolValue(..), Atom::Bytes32) => Comparison::Incompatible(Check::Length(32)),
        (Atom::BoolValue(..), Atom::PublicKey) => Comparison::Incompatible(Check::Length(48)),
        (Atom::Bytes32, Atom::Bytes32) => Comparison::Assignable,
        (Atom::Bytes32, Atom::Bytes) => Comparison::Assignable,
        (Atom::Bytes32, Atom::Int) => Comparison::Castable,
        (Atom::Bytes32, Atom::BoolValue(value)) => {
            Comparison::Incompatible(Check::Value(bool_atom(value)))
        }
        (Atom::Bytes32, Atom::PublicKey) => Comparison::Incompatible(Check::Length(48)),
        (Atom::Bytes32, Atom::Nil) => Comparison::Incompatible(Check::Value(vec![])),
        (Atom::PublicKey, Atom::PublicKey) => Comparison::Assignable,
        (Atom::PublicKey, Atom::Bytes) => Comparison::Castable,
        (Atom::PublicKey, Atom::Int) => Comparison::Castable,
        (Atom::PublicKey, Atom::BoolValue(value)) => {
            Comparison::Incompatible(Check::Value(bool_atom(value)))
        }
        (Atom::PublicKey, Atom::Bytes32) => Comparison::Incompatible(Check::Length(32)),
        (Atom::PublicKey, Atom::Nil) => Comparison::Incompatible(Check::Value(vec![])),
        (Atom::Nil, Atom::Nil) => Comparison::Assignable,
        (Atom::Nil, Atom::Bytes) => Comparison::Assignable,
        (Atom::Nil, Atom::Int) => Comparison::Castable,
        (Atom::Nil, Atom::Bytes32) => Comparison::Incompatible(Check::Length(32)),
        (Atom::Nil, Atom::PublicKey) => Comparison::Incompatible(Check::Length(48)),
        (Atom::Bytes | Atom::Int, Atom::Bytes32) => {
            Comparison::Constrainable(Constraint::new(Check::Length(32)))
        }
        (Atom::Bytes | Atom::Int, Atom::PublicKey) => {
            Comparison::Constrainable(Constraint::new(Check::Length(48)))
        }
        (Atom::Bytes | Atom::Int, Atom::BytesValue(value)) => {
            Comparison::Constrainable(Constraint::new(Check::Value(value)))
        }
        (Atom::Bytes | Atom::Int, Atom::StringValue(value)) => {
            Comparison::Constrainable(Constraint::new(Check::Value(value.as_bytes().to_vec())))
        }
        (Atom::Bytes | Atom::Int, Atom::IntValue(value)) => {
            Comparison::Constrainable(Constraint::new(Check::Value(bigint_atom(value.clone()))))
        }
        (Atom::Bytes | Atom::Int, Atom::BoolValue(value)) => {
            Comparison::Constrainable(Constraint::new(Check::Value(bool_atom(value))))
        }
        (Atom::Bytes | Atom::Int, Atom::Nil) => {
            Comparison::Constrainable(Constraint::new(Check::Value(vec![])))
        }
        (Atom::BytesValue(from), Atom::BytesValue(to)) => {
            if from == to {
                Comparison::Assignable
            } else {
                Comparison::Incompatible(Check::Value(to))
            }
        }
        (Atom::StringValue(from), Atom::StringValue(to)) => {
            if from == to {
                Comparison::Assignable
            } else {
                Comparison::Incompatible(Check::Value(to.as_bytes().to_vec()))
            }
        }
        (Atom::IntValue(from), Atom::IntValue(to)) => {
            if bigint_atom(from.clone()) == bigint_atom(to.clone()) {
                Comparison::Assignable
            } else {
                Comparison::Incompatible(Check::Value(bigint_atom(to)))
            }
        }
        (Atom::BoolValue(from), Atom::BoolValue(to)) => {
            if from == to {
                Comparison::Assignable
            } else {
                Comparison::Incompatible(Check::Value(bool_atom(to)))
            }
        }
        (Atom::BytesValue(from), Atom::Bytes32) => {
            if from.len() == 32 {
                Comparison::Assignable
            } else {
                Comparison::Incompatible(Check::Length(32))
            }
        }
        (Atom::StringValue(from), Atom::Bytes32) => {
            if from.len() == 32 {
                Comparison::Assignable
            } else {
                Comparison::Incompatible(Check::Length(32))
            }
        }
        (Atom::IntValue(from), Atom::Bytes32) => {
            if bigint_atom(from.clone()).len() == 32 {
                Comparison::Castable
            } else {
                Comparison::Incompatible(Check::Length(32))
            }
        }
        (Atom::BytesValue(from), Atom::PublicKey) => {
            if from.len() == 48 {
                Comparison::Assignable
            } else {
                Comparison::Incompatible(Check::Length(48))
            }
        }
        (Atom::StringValue(from), Atom::PublicKey) => {
            if from.len() == 48 {
                Comparison::Assignable
            } else {
                Comparison::Incompatible(Check::Length(48))
            }
        }
        (Atom::IntValue(from), Atom::PublicKey) => {
            if bigint_atom(from.clone()).len() == 48 {
                Comparison::Castable
            } else {
                Comparison::Incompatible(Check::Length(48))
            }
        }
        (Atom::BytesValue(from), Atom::Nil) => {
            if from.is_empty() {
                Comparison::Assignable
            } else {
                Comparison::Incompatible(Check::Value(vec![]))
            }
        }
        (Atom::Nil, Atom::BytesValue(to)) => {
            if to.is_empty() {
                Comparison::Assignable
            } else {
                Comparison::Incompatible(Check::Value(to))
            }
        }
        (Atom::StringValue(from), Atom::Nil) => {
            if from.is_empty() {
                Comparison::Assignable
            } else {
                Comparison::Incompatible(Check::Value(vec![]))
            }
        }
        (Atom::Nil, Atom::StringValue(to)) => {
            if to.is_empty() {
                Comparison::Assignable
            } else {
                Comparison::Incompatible(Check::Value(to.as_bytes().to_vec()))
            }
        }
        (Atom::IntValue(from), Atom::Nil) => {
            if bigint_atom(from.clone()).is_empty() {
                Comparison::Castable
            } else {
                Comparison::Incompatible(Check::Value(vec![]))
            }
        }
        (Atom::Nil, Atom::IntValue(to)) => {
            if bigint_atom(to.clone()).is_empty() {
                Comparison::Castable
            } else {
                Comparison::Incompatible(Check::Value(bigint_atom(to)))
            }
        }
        (Atom::BytesValue(from), Atom::BoolValue(to)) => {
            if (from.is_empty() && !to) || (from == [1] && to) {
                Comparison::Castable
            } else {
                Comparison::Incompatible(Check::Value(bool_atom(to)))
            }
        }
        (Atom::BoolValue(from), Atom::BytesValue(to)) => {
            if (to.is_empty() && !from) || (to == [1] && from) {
                Comparison::Castable
            } else {
                Comparison::Incompatible(Check::Value(to))
            }
        }
        (Atom::StringValue(from), Atom::BoolValue(to)) => {
            if (from.is_empty() && !to) || (from.as_bytes() == [1] && to) {
                Comparison::Castable
            } else {
                Comparison::Incompatible(Check::Value(bool_atom(to)))
            }
        }
        (Atom::BoolValue(from), Atom::StringValue(to)) => {
            if (to.is_empty() && !from) || (to.as_bytes() == [1] && from) {
                Comparison::Castable
            } else {
                Comparison::Incompatible(Check::Value(to.as_bytes().to_vec()))
            }
        }
        (Atom::IntValue(from), Atom::BoolValue(to)) => {
            let atom = bigint_atom(from.clone());
            if (atom.is_empty() && !to) || (atom == [1] && to) {
                Comparison::Castable
            } else {
                Comparison::Incompatible(Check::Value(bool_atom(to)))
            }
        }
        (Atom::BoolValue(from), Atom::IntValue(to)) => {
            let atom = bigint_atom(to.clone());
            if (atom.is_empty() && !from) || (atom == [1] && from) {
                Comparison::Castable
            } else {
                Comparison::Incompatible(Check::Value(bigint_atom(to)))
            }
        }
        (Atom::BytesValue(from), Atom::IntValue(to)) => {
            if bigint_atom(to.clone()) == from {
                Comparison::Castable
            } else {
                Comparison::Incompatible(Check::Value(bigint_atom(to)))
            }
        }
        (Atom::IntValue(from), Atom::BytesValue(to)) => {
            if bigint_atom(from.clone()) == to {
                Comparison::Castable
            } else {
                Comparison::Incompatible(Check::Value(to))
            }
        }
        (Atom::StringValue(from), Atom::IntValue(to)) => {
            if bigint_atom(to.clone()) == from.as_bytes().to_vec() {
                Comparison::Castable
            } else {
                Comparison::Incompatible(Check::Value(bigint_atom(to)))
            }
        }
        (Atom::IntValue(from), Atom::StringValue(to)) => {
            if bigint_atom(from.clone()) == to.as_bytes().to_vec() {
                Comparison::Castable
            } else {
                Comparison::Incompatible(Check::Value(to.as_bytes().to_vec()))
            }
        }
        (Atom::Bytes32, Atom::BytesValue(to)) => {
            let check = Check::Value(to.clone());
            if to.len() == 32 {
                Comparison::Constrainable(Constraint::new(check))
            } else {
                Comparison::Incompatible(check)
            }
        }
        (Atom::Bytes32, Atom::StringValue(to)) => {
            let check = Check::Value(to.as_bytes().to_vec());
            if to.len() == 32 {
                Comparison::Constrainable(Constraint::new(check))
            } else {
                Comparison::Incompatible(check)
            }
        }
        (Atom::Bytes32, Atom::IntValue(to)) => {
            let atom = bigint_atom(to);
            let check = Check::Value(atom.clone());
            if atom.len() == 32 {
                Comparison::Constrainable(Constraint::new(check))
            } else {
                Comparison::Incompatible(check)
            }
        }
        (Atom::PublicKey, Atom::BytesValue(to)) => {
            let check = Check::Value(to.clone());
            if to.len() == 48 {
                Comparison::Constrainable(Constraint::new(check))
            } else {
                Comparison::Incompatible(check)
            }
        }
        (Atom::PublicKey, Atom::StringValue(to)) => {
            let check = Check::Value(to.as_bytes().to_vec());
            if to.len() == 48 {
                Comparison::Constrainable(Constraint::new(check))
            } else {
                Comparison::Incompatible(check)
            }
        }
        (Atom::PublicKey, Atom::IntValue(to)) => {
            let atom = bigint_atom(to.clone());
            let check = Check::Value(atom.clone());
            if atom.len() == 48 {
                Comparison::Constrainable(Constraint::new(check))
            } else {
                Comparison::Incompatible(check)
            }
        }
        (Atom::Nil, Atom::BoolValue(value)) => {
            if value {
                Comparison::Incompatible(Check::Value(vec![1]))
            } else {
                Comparison::Castable
            }
        }
        (Atom::BoolValue(value), Atom::Nil) => {
            if value {
                Comparison::Incompatible(Check::Value(vec![]))
            } else {
                Comparison::Castable
            }
        }
        (Atom::BytesValue(from), Atom::StringValue(to)) => {
            if from == to.as_bytes().to_vec() {
                Comparison::Assignable
            } else {
                Comparison::Incompatible(Check::Value(to.as_bytes().to_vec()))
            }
        }
        (Atom::StringValue(from), Atom::BytesValue(to)) => {
            if from.as_bytes().to_vec() == to {
                Comparison::Assignable
            } else {
                Comparison::Incompatible(Check::Value(to))
            }
        }
    }
}

pub fn bigint_atom(value: BigInt) -> Vec<u8> {
    let mut allocator = Allocator::new();
    let atom = allocator.new_number(value).unwrap();
    allocator.atom(atom).to_vec()
}

pub fn bool_atom(value: bool) -> Vec<u8> {
    if value { vec![1] } else { vec![] }
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};

    use super::*;

    use Atom::*;

    fn check(from: Atom, to: Atom, expect: Expect) {
        let comparison = compare_atoms(from, to);

        let value = match comparison {
            Comparison::Assignable => "assign".to_string(),
            Comparison::Castable => "cast".to_string(),
            Comparison::Constrainable(check) => format!("({check:?})"),
            Comparison::Incompatible(..) => "fail".to_string(),
        };

        expect.assert_eq(&value);
    }

    #[test]
    fn test_nil() {
        check(Nil, Nil, expect!["assign"]);
        check(Nil, Bytes, expect!["assign"]);
        check(Nil, Int, expect!["cast"]);
        check(Int, Nil, expect!["(<0> == nil)"]);
        check(Bytes, Nil, expect!["(<0> == nil)"]);
        check(Bytes32, Nil, expect!["fail"]);
    }

    #[test]
    fn test_bool() {
        // check(Bool, Bool, expect!["assign"]);
        // check(Bool, Bytes, expect!["cast"]);
        // check(Bool, Int, expect!["cast"]);
        // check(Bool, Bytes32, expect!["fail"]);
        // check(Bool, PublicKey, expect!["fail"]);
        // check(Bool, Nil, expect!["(<0> == nil)"]);
        // check(Nil, Bool, expect!["cast"]);
        // check(Int, Bool, expect!["((<0> == true) || (<0> == false))"]);
        // check(BoolValue(true), Bool, expect!["assign"]);
        // check(BoolValue(false), Bool, expect!["assign"]);
        // check(Bool, BoolValue(true), expect!["(<0> == true)"]);
        // check(Bool, BoolValue(false), expect!["(<0> == false)"]);
        // check(BoolValue(true), Int, expect!["cast"]);
    }

    #[test]
    fn test_bytes() {
        check(Bytes, Bytes, expect!["assign"]);
        check(Bytes32, Bytes, expect!["assign"]);
        check(Bytes, Bytes32, expect!["((strlen <0>) == 32)"]);
        check(Bytes32, Bytes32, expect!["assign"]);
        check(StringValue(String::new()), Bytes, expect!["assign"]);
        check(Bytes, BytesValue(vec![]), expect!["(<0> == nil)"]);
        check(
            Bytes,
            StringValue("hello".to_string()),
            expect![[r#"(<0> == "hello")"#]],
        );
        check(Bytes32, StringValue("hello".to_string()), expect!["fail"]);
    }

    #[test]
    fn test_int() {
        check(Bytes, Int, expect!["cast"]);
        check(Int, Bytes, expect!["cast"]);
        check(Int, Bytes32, expect!["((strlen <0>) == 32)"]);
        check(Bytes32, Int, expect!["cast"]);
        check(IntValue(BigInt::from(1)), Int, expect!["assign"]);
        check(Int, IntValue(BigInt::from(1)), expect!["(<0> == 1)"]);
    }
}
