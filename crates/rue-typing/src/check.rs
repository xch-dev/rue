use std::fmt;

mod check_error;
mod check_type;
mod simplify_and;
mod simplify_check;
mod simplify_or;
mod stringify_check;

pub use check_error::*;

pub(crate) use check_type::*;
use num_bigint::BigInt;
pub(crate) use simplify_and::*;
pub(crate) use simplify_check::*;
pub(crate) use simplify_or::*;
pub(crate) use stringify_check::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Check {
    None,
    IsPair,
    IsAtom,
    IsBool,
    Value(BigInt),
    Length(usize),
    And(Vec<Check>),
    Or(Vec<Check>),
    If(Box<Check>, Box<Check>, Box<Check>),
    First(Box<Check>),
    Rest(Box<Check>),
}

impl fmt::Display for Check {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        stringify_check(self, f, &mut Vec::new())
    }
}
