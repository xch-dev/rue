use id_arena::Id;

use crate::{environment::Environment, hir::Hir, mir::Mir, scope::Scope, symbol::Symbol};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub(super) Id<Symbol>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub(super) Id<Scope>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HirId(pub(super) Id<Hir>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MirId(pub(super) Id<Mir>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnvironmentId(pub(super) Id<Environment>);
