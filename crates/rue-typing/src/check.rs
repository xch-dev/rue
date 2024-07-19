#[derive(Debug, Clone)]
pub enum Check {
    IsPair,
    IsAtom,
    IsBool,
    IsNil,
    Length(usize),
    Not(Box<Check>),
    And(Vec<Check>),
    Or(Vec<Check>),
}

pub fn check_type() -> Check {
    todo!()
}
