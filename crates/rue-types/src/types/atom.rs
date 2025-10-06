use std::{borrow::Cow, fmt};

use clvmr::Allocator;
use derive_more::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display)]
pub enum AtomSemantic {
    #[display("Atom")]
    Any,

    #[display("Bytes")]
    Bytes,

    #[display("String")]
    String,

    #[display("Int")]
    Int,

    #[display("PublicKey")]
    PublicKey,

    #[display("Signature")]
    Signature,

    #[display("K1PublicKey")]
    K1PublicKey,

    #[display("K1Signature")]
    K1Signature,

    #[display("R1PublicKey")]
    R1PublicKey,

    #[display("R1Signature")]
    R1Signature,

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
    pub const STRING: Self = Self::new(AtomSemantic::String, None);
    pub const PUBLIC_KEY: Self =
        Self::new(AtomSemantic::PublicKey, Some(AtomRestriction::Length(48)));
    pub const SIGNATURE: Self =
        Self::new(AtomSemantic::Signature, Some(AtomRestriction::Length(96)));
    pub const K1_PUBLIC_KEY: Self =
        Self::new(AtomSemantic::K1PublicKey, Some(AtomRestriction::Length(33)));
    pub const K1_SIGNATURE: Self =
        Self::new(AtomSemantic::K1Signature, Some(AtomRestriction::Length(64)));
    pub const R1_PUBLIC_KEY: Self =
        Self::new(AtomSemantic::R1PublicKey, Some(AtomRestriction::Length(33)));
    pub const R1_SIGNATURE: Self =
        Self::new(AtomSemantic::Signature, Some(AtomRestriction::Length(64)));
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
        match self.semantic {
            AtomSemantic::Any => write!(f, "Atom"),
            AtomSemantic::Bytes => match &self.restriction {
                None => write!(f, "Bytes"),
                Some(AtomRestriction::Length(length)) => {
                    if *length == 32 {
                        write!(f, "Bytes32")
                    } else {
                        write!(f, "Bytes")
                    }
                }
                Some(AtomRestriction::Value(value)) => {
                    if value.is_empty() {
                        write!(f, "nil")
                    } else {
                        write!(f, "0x{}", hex::encode(value))
                    }
                }
            },
            AtomSemantic::String => match &self.restriction {
                None | Some(AtomRestriction::Length(_)) => {
                    write!(f, "String")
                }
                Some(AtomRestriction::Value(value)) => {
                    write!(f, "\"{}\"", String::from_utf8_lossy(value))
                }
            },
            AtomSemantic::Int => match &self.restriction {
                None | Some(AtomRestriction::Length(_)) => write!(f, "Int"),
                Some(AtomRestriction::Value(value)) => {
                    let mut allocator = Allocator::new();
                    let ptr = allocator.new_atom(value).unwrap();
                    let bigint = allocator.number(ptr);
                    write!(f, "{bigint}")
                }
            },
            AtomSemantic::PublicKey => write!(f, "PublicKey"),
            AtomSemantic::Signature => write!(f, "Signature"),
            AtomSemantic::K1PublicKey => write!(f, "K1PublicKey"),
            AtomSemantic::K1Signature => write!(f, "K1Signature"),
            AtomSemantic::R1PublicKey => write!(f, "R1PublicKey"),
            AtomSemantic::R1Signature => write!(f, "R1Signature"),
            AtomSemantic::Bool => match &self.restriction {
                None | Some(AtomRestriction::Length(_)) => write!(f, "Bool"),
                Some(AtomRestriction::Value(value)) => {
                    if value.is_empty() {
                        write!(f, "false")
                    } else if value.as_ref() == [1] {
                        write!(f, "true")
                    } else {
                        write!(f, "Bool")
                    }
                }
            },
        }
    }
}
