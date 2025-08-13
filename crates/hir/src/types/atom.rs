use std::fmt;

use num_bigint::BigInt;

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
