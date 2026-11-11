use rue_diagnostic::{Name, SrcLoc};
use rue_hir::ScopeId;
use rue_types::{Comparison, TypeId};

/// Facts recorded by the compiler while compiling, for lints that need something that isn't kept
/// in the HIR. The compiler records every site; the lints decide what to report.
///
/// Most lints don't need sites, since they run as [`HirLintPass`](crate::HirLintPass)es over the
/// HIR, which records the type and location of every expression. Casts and guards are sites
/// because they don't produce a HIR node of their own: a cast compiles to the same node as its
/// value, and a guard compiles to the check it performs, which doesn't say what it's checking.
#[derive(Debug, Default, Clone)]
pub struct LintSites {
    pub casts: Vec<CastSite>,
    pub guards: Vec<GuardSite>,
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
