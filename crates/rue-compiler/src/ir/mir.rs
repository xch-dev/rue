use crate::{BinOp, EnvironmentId, MirId, Op, SymbolId};

#[derive(Debug, Clone)]
pub enum Mir {
    Atom(Vec<u8>),
    Environment(EnvironmentId, MirId),
    Reference(SymbolId),
    Pair(MirId, MirId),
    Op(Op, MirId),
    BinaryOp(BinOp, MirId, MirId),
    Substr(MirId, MirId, MirId),
    Raise(Option<MirId>),
    If(MirId, MirId, MirId),
    Run(MirId, MirId),
    Curry(MirId, Vec<MirId>),
    Closure(MirId, Vec<MirId>),
    Quote(MirId),
}
