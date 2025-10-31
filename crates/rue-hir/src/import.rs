use id_arena::Id;
use rue_parser::SyntaxToken;

pub type ImportId = Id<Import>;

#[derive(Debug, Clone)]
pub struct Import {
    pub name: SyntaxToken,
    pub exported: bool,
    pub include_self: bool,
    pub include_all: bool,
    pub children: Vec<ImportId>,
}
