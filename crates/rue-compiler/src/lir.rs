use crate::database::LirId;

#[derive(Debug, Clone)]
pub enum Lir {
    Atom(Vec<u8>),
    Pair(LirId, LirId),
    Path(u32),
    Run(LirId, Option<LirId>),
    Quote(LirId),
    Curry(LirId, Vec<LirId>),
    Closure(LirId, Vec<LirId>),
    First(LirId),
    Rest(LirId),
    Raise(Option<LirId>),
    Sha256(Vec<LirId>),
    Listp(LirId),
    Strlen(LirId),
    PubkeyForExp(LirId),
    If(LirId, LirId, LirId),
    Not(LirId),
    All(Vec<LirId>),
    Any(Vec<LirId>),
    Concat(Vec<LirId>),
    PointAdd(Vec<LirId>),
    LogNot(LirId),
    LogAnd(Vec<LirId>),
    LogIor(Vec<LirId>),
    LogXor(Vec<LirId>),
    Add(Vec<LirId>),
    Sub(Vec<LirId>),
    Mul(Vec<LirId>),
    Div(LirId, LirId),
    Divmod(LirId, LirId),
    Eq(LirId, LirId),
    Gt(LirId, LirId),
    GtBytes(LirId, LirId),
}
