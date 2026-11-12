use id_arena::Id;
use rue_diagnostic::{Name, Source};

use crate::{Declaration, ScopeId};

pub type ImportId = Id<Import>;

/// Where a declaration was referenced through an import.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ImportUser {
    /// A reference in the declaration, such as a function's body.
    Declaration(Declaration),
    /// A reference in the path of another import.
    Import(ImportId),
    /// A reference outside of any declaration or import.
    Other,
}

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
