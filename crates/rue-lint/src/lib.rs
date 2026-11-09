//! Lints run as passes after compilation. Each pass works from the HIR and the declaration graph
//! in the [`Database`](rue_hir::Database), the parsed documents, and the [`LintSites`] that the
//! compiler records while type checking. The compiler records those sites without judging them,
//! so whether something is reported, and at what level, is decided entirely here.

mod context;
mod lints;
mod sites;

pub use context::*;
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
}

/// Runs every lint pass, and returns the diagnostics that should be reported at the given levels.
pub fn run_lints(
    db: &mut Database,
    types: &BuiltinTypes,
    levels: LintLevels,
    input: &LintInput<'_>,
) -> Vec<Diagnostic> {
    let mut cx = LintContext::new(db, types, levels);

    for pass in lints::PASSES {
        pass(&mut cx, input);
    }

    cx.into_diagnostics()
}
