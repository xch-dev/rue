use id_arena::Id;
use rue_diagnostic::{Name, Source};

use crate::Declaration;

pub type ImportId = Id<Import>;

#[derive(Debug, Clone)]
pub struct Import {
    pub source: Source,
    pub path: Vec<Name>,
    pub items: Items,
    pub exported: bool,
    pub declarations: Vec<(String, Declaration)>,
}

#[derive(Debug, Clone)]
pub enum Items {
    All(Name),
    Named(Name),
}
