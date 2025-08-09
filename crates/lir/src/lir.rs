use id_arena::Id;

pub type LirId = Id<Lir>;

#[derive(Debug, Clone)]
pub enum Lir {
    Atom(Vec<u8>),
    Cons(LirId, LirId),
}
