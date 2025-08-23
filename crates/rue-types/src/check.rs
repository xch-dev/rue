use crate::AtomRestriction;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Check {
    None,
    IsAtom,
    IsPair,
    Pair(Box<Check>, Box<Check>),
    Atom(AtomRestriction),
    And(Vec<Check>),
    Or(Vec<Check>),
}
