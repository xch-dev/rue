use std::borrow::Cow;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Check {
    Value(Cow<'static, [u8]>),
    Length(usize),
}
