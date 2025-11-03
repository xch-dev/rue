use id_arena::Id;
use rue_parser::SyntaxToken;

use crate::Declaration;

pub type ImportId = Id<Import>;

#[derive(Debug, Clone)]
pub struct Import {
    pub path: Vec<SyntaxToken>,
    pub items: Items,
    pub exported: bool,
    pub declarations: Vec<(String, Declaration)>,
}

#[derive(Debug, Clone)]
pub enum Items {
    All(SyntaxToken),
    Named(Vec<SyntaxToken>),
}
