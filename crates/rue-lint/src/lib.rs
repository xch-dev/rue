//! Lints run as passes after compilation. Each pass works from the HIR and the declaration graph
//! in the [`Database`](rue_hir::Database), the parsed documents, and the [`LintSites`] that the
//! compiler records while type checking. Most passes are [`HirLintPass`]es, which are called for
//! each node of the HIR, and can look up the type and location of the expression it was compiled
//! from. The compiler records these facts without judging them, so whether something is
//! reported, and at what level, is decided entirely here.

mod context;
mod levels;
mod lints;
mod sites;
mod visit;

pub use context::*;
pub use levels::*;
pub use sites::*;
pub use visit::*;

use std::collections::HashSet;

use rue_ast::AstDocument;
use rue_diagnostic::{Diagnostic, DiagnosticSeverity, LintLevels, Source};
use rue_hir::{Database, Declaration};
use rue_types::BuiltinTypes;

/// Everything the lint passes need beyond the database.
#[derive(Debug)]
pub struct LintInput<'a> {
    /// The sites recorded by the compiler while compiling the documents.
    pub sites: LintSites,
    /// The parsed documents that were compiled, along with their sources.
    pub documents: Vec<(&'a Source, &'a AstDocument)>,
    /// The declarations that are used externally, such as `main`, tests and exports.
    pub entrypoints: &'a HashSet<Declaration>,
    /// Whether compilation reported any errors. Code that failed to compile may not have recorded
    /// all of its sites, so `expect` attributes aren't checked, as in Rust.
    pub has_errors: bool,
}

/// Runs every lint pass, and returns the diagnostics that should be reported at the given levels.
pub fn run_lints(
    db: &mut Database,
    types: &BuiltinTypes,
    levels: LintLevels,
    input: &LintInput<'_>,
) -> Vec<Diagnostic> {
    let (levels, mut diagnostics) = ScopedLintLevels::new(levels, &input.documents);

    let mut cx = LintContext::new(db, types, levels);

    for pass in lints::PASSES {
        pass(&mut cx, input);
    }

    // Invalid lint attributes also stop expectations from being checked, as in Rust, but lints
    // reported as errors (such as denied lints) don't
    let has_errors = input.has_errors
        || diagnostics
            .iter()
            .any(|diagnostic| diagnostic.severity == DiagnosticSeverity::Error);

    if !has_errors {
        cx.report_unfulfilled_expectations();
    }

    diagnostics.extend(cx.into_diagnostics());
    diagnostics
}
