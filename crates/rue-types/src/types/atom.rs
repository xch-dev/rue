use std::{borrow::Cow, fmt};

use clvmr::Allocator;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AtomKind {
    Bytes,
    Int,
    PublicKey,
    Bool,
}

#[derive(Debug, Clone)]
pub enum AtomRestriction {
    Value(Cow<'static, [u8]>),
    Length(usize),
}

#[derive(Debug, Clone)]
pub struct Atom {
    pub kind: AtomKind,
    pub restriction: Option<AtomRestriction>,
}

impl Atom {
    pub const NIL: Self = Self::new(
        AtomKind::Bytes,
        Some(AtomRestriction::Value(Cow::Borrowed(&[]))),
    );
    pub const FALSE: Self = Self::new(
        AtomKind::Bool,
        Some(AtomRestriction::Value(Cow::Borrowed(&[]))),
    );
    pub const TRUE: Self = Self::new(
        AtomKind::Bool,
        Some(AtomRestriction::Value(Cow::Borrowed(&[1]))),
    );
    pub const BYTES: Self = Self::new(AtomKind::Bytes, None);
    pub const BYTES_32: Self = Self::new(AtomKind::Bytes, Some(AtomRestriction::Length(32)));
    pub const PUBLIC_KEY: Self = Self::new(AtomKind::PublicKey, Some(AtomRestriction::Length(48)));
    pub const INT: Self = Self::new(AtomKind::Int, None);

    pub const fn new(kind: AtomKind, restriction: Option<AtomRestriction>) -> Self {
        Self { kind, restriction }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (&self.kind, &self.restriction) {
            (AtomKind::Bytes, None) => write!(f, "Bytes"),
            (AtomKind::Bytes, Some(AtomRestriction::Length(length))) => {
                write!(f, "Bytes{}", length)
            }
            (AtomKind::Bytes, Some(AtomRestriction::Value(value))) => {
                if value.is_empty() {
                    write!(f, "nil")
                } else {
                    write!(f, "0x{}", hex::encode(value))
                }
            }
            (AtomKind::Int, None) => write!(f, "Int"),
            (AtomKind::Int, Some(AtomRestriction::Value(value))) => {
                let mut allocator = Allocator::new();
                let ptr = allocator.new_atom(value).unwrap();
                write!(f, "{}", allocator.number(ptr))
            }
            (AtomKind::Int, Some(AtomRestriction::Length(length))) => {
                write!(f, "Int{}", length)
            }
            (AtomKind::PublicKey, None) => write!(f, "PublicKey"),
            (AtomKind::PublicKey, Some(AtomRestriction::Length(48))) => write!(f, "PublicKey"),
            (AtomKind::PublicKey, Some(AtomRestriction::Length(length))) => {
                write!(f, "PublicKey{}", length)
            }
            (AtomKind::PublicKey, Some(AtomRestriction::Value(value))) => {
                if value.is_empty() {
                    write!(f, "nil")
                } else {
                    write!(f, "0x{}", hex::encode(value))
                }
            }
            (AtomKind::Bool, None) => write!(f, "Bool"),
            (AtomKind::Bool, Some(AtomRestriction::Value(value))) => {
                if value.is_empty() {
                    write!(f, "false")
                } else if value.as_ref() == [1] {
                    write!(f, "true")
                } else {
                    write!(f, "0x{}", hex::encode(value))
                }
            }
            (AtomKind::Bool, Some(AtomRestriction::Length(length))) => {
                write!(f, "Bool{}", length)
            }
        }
    }
}
