use rue_diagnostic::Name;

/// Facts recorded by the compiler while compiling, for lints that need something that isn't kept
/// in the HIR. The compiler records every site; the lints decide what to report.
///
/// Most lints don't need sites, since they run as [`HirLintPass`](crate::HirLintPass)es over the
/// HIR, which records the type and location of every expression.
#[derive(Debug, Default, Clone)]
pub struct LintSites {
    /// The items of imports that resolved, but didn't bring anything into scope (for example,
    /// because everything they name is already declared there). For a glob import, this is the
    /// `*`.
    pub empty_imports: Vec<Name>,
}
