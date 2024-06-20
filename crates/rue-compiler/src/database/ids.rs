use id_arena::Id;

use crate::{hir::Hir, lir::Lir, optimizer::Environment, scope::Scope, symbol::Symbol, ty::Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub(super) Id<Symbol>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub(super) Id<Scope>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub(super) Id<Type>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HirId(pub(super) Id<Hir>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LirId(pub(super) Id<Lir>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnvironmentId(pub(super) Id<Environment>);
