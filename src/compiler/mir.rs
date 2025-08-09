use crate::{BinaryOp, MirId, SymbolId, UnaryOp};

#[derive(Debug, Clone)]
pub enum Mir {
    Unresolved,
    Atom(Vec<u8>),
    Reference(SymbolId),
    Unary(UnaryOp, MirId),
    Binary(BinaryOp, MirId, MirId),
}
