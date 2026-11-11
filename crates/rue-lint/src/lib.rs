//! Lints run as passes after compilation. Each pass works from the HIR and the declaration graph
//! in the [`Database`](rue_hir::Database), the parsed documents, and the [`LintSites`] that the
//! compiler records while type checking. The compiler records those sites without judging them,
//! so whether something is reported, and at what level, is decided entirely here.

mod context;
mod levels;
mod lints;
mod sites;

pub use context::*;
pub use levels::*;
pub use sites::*;

use std::collections::HashSet;

use rue_ast::AstDocument;
use rue_diagnostic::{Diagnostic, LintLevels, Source};
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
    /// all of its sites, so `expect` attributes aren't checked, since they could be reported as
    /// unfulfilled by mistake.
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

    if !input.has_errors {
        cx.report_unfulfilled_expectations();
    }

    diagnostics.extend(cx.into_diagnostics());
    diagnostics
}
