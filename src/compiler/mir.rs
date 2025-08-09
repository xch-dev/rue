use crate::{BinaryOp, MirId, UnaryOp};

#[derive(Debug, Clone)]
pub enum Mir {
    Unresolved,
    Atom(Vec<u8>),
    Unary(UnaryOp, MirId),
    Binary(BinaryOp, MirId, MirId),
}
