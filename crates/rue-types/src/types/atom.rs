use std::{borrow::Cow, fmt};

use clvmr::Allocator;
use derive_more::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display)]
pub enum AtomSemantic {
    #[display("Atom")]
    Any,

    #[display("Bytes")]
    Bytes,

    #[display("Int")]
    Int,

    #[display("PublicKey")]
    PublicKey,

    #[display("Bool")]
    Bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AtomRestriction {
    Value(Cow<'static, [u8]>),
    Length(usize),
}

#[derive(Debug, Clone)]
pub struct Atom {
    pub semantic: AtomSemantic,
    pub restriction: Option<AtomRestriction>,
}

impl Atom {
    pub const ANY: Self = Self::new(AtomSemantic::Any, None);
    pub const NIL: Self = Self::new(
        AtomSemantic::Bytes,
        Some(AtomRestriction::Value(Cow::Borrowed(&[]))),
    );
    pub const FALSE: Self = Self::new(
        AtomSemantic::Bool,
        Some(AtomRestriction::Value(Cow::Borrowed(&[]))),
    );
    pub const TRUE: Self = Self::new(
        AtomSemantic::Bool,
        Some(AtomRestriction::Value(Cow::Borrowed(&[1]))),
    );
    pub const BYTES: Self = Self::new(AtomSemantic::Bytes, None);
    pub const BYTES_32: Self = Self::new(AtomSemantic::Bytes, Some(AtomRestriction::Length(32)));
    pub const PUBLIC_KEY: Self =
        Self::new(AtomSemantic::PublicKey, Some(AtomRestriction::Length(48)));
    pub const INT: Self = Self::new(AtomSemantic::Int, None);

    pub const fn new(semantic: AtomSemantic, restriction: Option<AtomRestriction>) -> Self {
        Self {
            semantic,
            restriction,
        }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = format!("{}", self.semantic);

        let qualifier = match &self.restriction {
            None => None,
            Some(AtomRestriction::Length(length)) => {
                if self.semantic == AtomSemantic::PublicKey && *length == 48 {
                    None
                } else {
                    Some(format!("{length}"))
                }
            }
            Some(AtomRestriction::Value(value)) => match self.semantic {
                AtomSemantic::Any | AtomSemantic::Bytes | AtomSemantic::PublicKey => {
                    if value.is_empty() {
                        return write!(f, "nil");
                    }

                    return write!(f, "0x{}", hex::encode(value));
                }
                AtomSemantic::Int => {
                    let mut allocator = Allocator::new();
                    let ptr = allocator.new_atom(value).unwrap();
                    return write!(f, "{}", allocator.number(ptr));
                }
                AtomSemantic::Bool => {
                    if value.is_empty() {
                        return write!(f, "false");
                    } else if value.as_ref() == [1] {
                        return write!(f, "true");
                    }

                    return write!(f, "0x{}", hex::encode(value));
                }
            },
        };

        if let Some(qualifier) = qualifier {
            write!(f, "{name}{qualifier}")
        } else {
            write!(f, "{name}")
        }
    }
}
