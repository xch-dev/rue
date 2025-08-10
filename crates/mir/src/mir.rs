use id_arena::Id;

pub type MirId = Id<Mir>;

#[derive(Debug, Clone)]
pub enum Mir {
    Atom(Vec<u8>),
}
