use id_arena::Id;

pub type LirId = Id<Lir>;

#[derive(Debug, Clone)]
pub enum Lir {
    Atom(Vec<u8>),
    First(LirId),
    Rest(LirId),
    Cons(LirId, LirId),
    Listp(LirId),
    Add(Vec<LirId>),
    Sub(Vec<LirId>),
    Mul(Vec<LirId>),
    Div(LirId, LirId),
    Divmod(LirId, LirId),
    Mod(LirId, LirId),
    Modpow(LirId, LirId, LirId),
    Eq(LirId, LirId),
    Gt(LirId, LirId),
    Not(LirId),
    All(Vec<LirId>),
    Any(Vec<LirId>),
    If(LirId, LirId, LirId),
    Raise(Vec<LirId>),
}
