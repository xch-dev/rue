use crate::LirId;

#[derive(Debug, Clone)]
pub enum Lir {
    Atom(Vec<u8>),
    Path(u32),
    Listp(LirId),
    Not(LirId),
    Add(Vec<LirId>),
    Sub(Vec<LirId>),
    Mul(Vec<LirId>),
    Div(LirId, LirId),
}
