use std::{borrow::Cow, str::FromStr};

use num_bigint::BigInt;
use num_traits::Num;
use rue_lir::bigint_atom;
use rue_parser::{SyntaxKind, SyntaxToken, T};
use rue_types::{Atom, AtomRestriction, AtomSemantic};

/// The decoded value of a literal token, shared between literal expressions and literal types
/// so that both always agree on the value (and therefore the type) of a given literal.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralValue {
    String(String),
    Bytes(Vec<u8>),
    Int(BigInt),
    Nil,
    True,
    False,
}

impl LiteralValue {
    pub fn decode(token: &SyntaxToken) -> Self {
        Self::decode_text(token.kind(), token.text())
    }

    fn decode_text(kind: SyntaxKind, text: &str) -> Self {
        match kind {
            SyntaxKind::String => {
                let mut text = text;

                if let Some(stripped) = text.strip_prefix('"') {
                    text = stripped;
                }

                if let Some(stripped) = text.strip_suffix('"') {
                    text = stripped;
                }

                Self::String(text.to_string())
            }
            SyntaxKind::Hex => {
                let text = text.strip_prefix("0x").unwrap_or(text);

                let mut text = text.replace('_', "");

                if text.len() % 2 == 1 {
                    text.insert(0, '0');
                }

                Self::Bytes(hex::decode(text).expect("invalid hex literal"))
            }
            SyntaxKind::Binary => {
                let text = text.strip_prefix("0b").unwrap_or(text);
                Self::Int(decode_radix(text, 2))
            }
            SyntaxKind::Octal => {
                let text = text.strip_prefix("0o").unwrap_or(text);
                Self::Int(decode_radix(text, 8))
            }
            SyntaxKind::Integer => {
                let text = text.replace('_', "");
                Self::Int(BigInt::from_str(&text).expect("invalid integer literal"))
            }
            T![nil] => Self::Nil,
            T![true] => Self::True,
            T![false] => Self::False,
            _ => unreachable!(),
        }
    }

    pub fn atom(&self) -> Atom {
        match self {
            Self::String(value) => Atom::new(
                AtomSemantic::String,
                Some(AtomRestriction::Value(Cow::Owned(
                    value.as_bytes().to_vec(),
                ))),
            ),
            Self::Bytes(bytes) => {
                if bytes.is_empty() {
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
                        Some(AtomRestriction::Value(Cow::Owned(bytes.clone()))),
                    )
                }
            }
            Self::Int(value) => Atom::new(
                AtomSemantic::Int,
                Some(AtomRestriction::Value(Cow::Owned(bigint_atom(
                    value.clone(),
                )))),
            ),
            Self::Nil => Atom::NIL,
            Self::True => Atom::TRUE,
            Self::False => Atom::FALSE,
        }
    }
}

fn decode_radix(text: &str, radix: u32) -> BigInt {
    let text = text.replace('_', "");

    if text.is_empty() {
        BigInt::ZERO
    } else {
        BigInt::from_str_radix(&text, radix).expect("invalid integer literal")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hex_odd_length() {
        assert_eq!(
            LiteralValue::decode_text(SyntaxKind::Hex, "0x1"),
            LiteralValue::Bytes(vec![0x01])
        );
        assert_eq!(
            LiteralValue::decode_text(SyntaxKind::Hex, "0x01"),
            LiteralValue::Bytes(vec![0x01])
        );
        assert_eq!(
            LiteralValue::decode_text(SyntaxKind::Hex, "0x1_23"),
            LiteralValue::Bytes(vec![0x01, 0x23])
        );
    }

    #[test]
    fn test_hex_empty() {
        assert_eq!(
            LiteralValue::decode_text(SyntaxKind::Hex, "0x"),
            LiteralValue::Bytes(vec![])
        );
        assert_eq!(
            LiteralValue::decode_text(SyntaxKind::Hex, "0x")
                .atom()
                .restriction,
            Atom::NIL.restriction
        );
    }

    #[test]
    fn test_ints() {
        assert_eq!(
            LiteralValue::decode_text(SyntaxKind::Integer, "42_000"),
            LiteralValue::Int(BigInt::from(42_000))
        );
        assert_eq!(
            LiteralValue::decode_text(SyntaxKind::Binary, "0b10_1010"),
            LiteralValue::Int(BigInt::from(42))
        );
        assert_eq!(
            LiteralValue::decode_text(SyntaxKind::Octal, "0o52"),
            LiteralValue::Int(BigInt::from(42))
        );
        assert_eq!(
            LiteralValue::decode_text(SyntaxKind::Binary, "0b"),
            LiteralValue::Int(BigInt::ZERO)
        );
    }

    #[test]
    fn test_string() {
        assert_eq!(
            LiteralValue::decode_text(SyntaxKind::String, "\"hello\""),
            LiteralValue::String("hello".to_string())
        );
    }
}
