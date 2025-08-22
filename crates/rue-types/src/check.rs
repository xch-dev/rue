use std::borrow::Cow;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Check {
    None,
    Pair(Box<Check>, Box<Check>),
    Value(Cow<'static, [u8]>),
    Length(usize),
}
