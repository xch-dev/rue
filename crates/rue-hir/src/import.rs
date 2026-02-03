use id_arena::Id;
use rue_diagnostic::{Name, Source};

use crate::{Declaration, ScopeId};

pub type ImportId = Id<Import>;

#[derive(Debug, Clone)]
pub struct Import {
    pub base_scope: ScopeId,
    pub source: Source,
    pub path: Vec<Name>,
    pub items: Items,
    pub exported: bool,
    pub declarations: Vec<(String, Declaration)>,
    pub has_super: bool,
}

#[derive(Debug, Clone)]
pub enum Items {
    All(Name),
    Named(Name),
}
