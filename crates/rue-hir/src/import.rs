use id_arena::Id;
use rue_parser::SyntaxToken;

pub type ImportId = Id<Import>;

#[derive(Debug, Clone)]
pub struct Import {
    pub path: Vec<SyntaxToken>,
    pub items: Items,
    pub exported: bool,
}

#[derive(Debug, Clone)]
pub enum Items {
    All(SyntaxToken),
    Named(Vec<SyntaxToken>),
}
