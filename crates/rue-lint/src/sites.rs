use rue_diagnostic::{Name, SrcLoc};
use rue_hir::{HirId, ScopeId};
use rue_types::{Comparison, TypeId};

/// Facts recorded by the compiler while type checking, for lints that depend on types that
/// aren't kept in the HIR. The compiler records every site; the lints decide what to report.
#[derive(Debug, Default, Clone)]
pub struct LintSites {
    pub casts: Vec<CastSite>,
    pub guards: Vec<GuardSite>,
    pub conditions: Vec<ConditionSite>,
    pub expr_statements: Vec<ExprStatementSite>,
    pub inline_ifs: Vec<InlineIfSite>,
    /// The items of imports that resolved, but didn't bring anything into scope (for example,
    /// because everything they name is already declared there). For a glob import, this is the
    /// `*`.
    pub empty_imports: Vec<Name>,
}

/// A cast expression (`value as T`).
#[derive(Debug, Clone)]
pub struct CastSite {
    pub srcloc: SrcLoc,
    /// The scope the cast is in, which is used to name the types.
    pub scope: ScopeId,
    pub from: TypeId,
    pub to: TypeId,
    /// How `from` compares to `to`.
    pub comparison: Comparison,
}

/// A type guard expression (`value is T`) that can be checked at runtime.
#[derive(Debug, Clone)]
pub struct GuardSite {
    pub srcloc: SrcLoc,
    /// The scope the guard is in, which is used to name the types.
    pub scope: ScopeId,
    pub from: TypeId,
    pub to: TypeId,
    /// Whether the guard emits a check at runtime, rather than always being true.
    pub has_runtime_check: bool,
}

/// The condition of an `if` expression, `if` statement or `assert`.
#[derive(Debug, Clone)]
pub struct ConditionSite {
    pub srcloc: SrcLoc,
    pub ty: TypeId,
}

/// An expression used as a statement.
#[derive(Debug, Clone)]
pub struct ExprStatementSite {
    pub srcloc: SrcLoc,
    /// Whether the expression always evaluates to nil.
    pub always_nil: bool,
}

/// An `inline if`, whose branches are both evaluated regardless of the condition.
#[derive(Debug, Clone)]
pub struct InlineIfSite {
    pub srcloc: SrcLoc,
    pub inline_if: InlineIf,
}

#[derive(Debug, Clone, Copy)]
pub enum InlineIf {
    /// An `inline if` expression, which is the given [`Hir::If`](rue_hir::Hir::If).
    Expr(HirId),
    /// An `inline if` statement, at the given index in the statements of a
    /// [`Hir::Block`](rue_hir::Hir::Block). The rest of the block is its else branch.
    Statement(HirId, usize),
}
