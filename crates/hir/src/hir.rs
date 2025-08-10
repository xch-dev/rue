use id_arena::Id;

pub type HirId = Id<Hir>;

#[derive(Debug, Clone)]
pub enum Hir {
    Unresolved,
    Atom(Vec<u8>),
}
