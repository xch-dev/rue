//! Lints run as passes after compilation. Each pass works from the HIR and the declaration graph
//! in the [`Database`](rue_hir::Database), the parsed documents, and the [`LintSites`] that the
//! compiler records while type checking. Most passes are [`HirLintPass`]es, which are called for
//! each node of the HIR, and can look up the type and location of the expression it was compiled
//! from. The compiler records these facts without judging them, so whether something is
//! reported, and at what level, is decided entirely here.

mod context;
mod equality;
mod levels;
mod lints;
mod sites;
mod text;
mod visit;

pub use context::*;
pub use levels::*;
pub use lints::{BytesAsInt, bytes_as_int};
pub use sites::*;
pub use visit::*;

use std::{cell::OnceCell, collections::HashSet};

use rue_ast::AstDocument;
use rue_diagnostic::{Diagnostic, DiagnosticSeverity, LintLevels, Source};
use rue_hir::{Database, Declaration, FailureAnalysis, SymbolId};
use rue_options::CompilerOptions;
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
    /// The functions that are compiled as programs of their own, whose parameters are the
    /// solution: each file's `main`, and the main file's exports (see `FileTree::programs`).
    pub programs: &'a HashSet<SymbolId>,
    /// Whether compilation reported any errors. Code that failed to compile may not have recorded
    /// all of its sites, so `expect` attributes aren't checked, as in Rust.
    pub has_errors: bool,
    /// Whether the program is compiled for after hard fork 2.
    pub hard_fork_2: bool,
    /// The declarations that are live, which are computed the first time a pass needs them.
    live: OnceCell<HashSet<Declaration>>,
    /// Whether values may fail as written, which is shared between the passes that check it.
    failures: FailureAnalysis,
}

impl<'a> LintInput<'a> {
    pub fn new(
        sites: LintSites,
        documents: Vec<(&'a Source, &'a AstDocument)>,
        entrypoints: &'a HashSet<Declaration>,
        programs: &'a HashSet<SymbolId>,
        has_errors: bool,
        options: &CompilerOptions,
    ) -> Self {
        Self {
            sites,
            documents,
            entrypoints,
            programs,
            has_errors,
            hard_fork_2: options.hard_fork_2,
            live: OnceCell::new(),
            failures: FailureAnalysis::new(options.enforces_operand_limits()),
        }
    }

    /// Whether values may fail as written (see [`FailureAnalysis`]).
    pub(crate) fn failures(&self) -> &FailureAnalysis {
        &self.failures
    }

    /// The declarations whose code is evaluated, or is needed by code that's kept, starting from
    /// the entrypoints.
    pub(crate) fn live_declarations(&self, db: &Database) -> &HashSet<Declaration> {
        self.live
            .get_or_init(|| lints::live_declarations(db, self.entrypoints, &self.failures))
    }
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

    cx.report_unknown_lints();

    for pass in lints::INPUT_PASSES {
        if pass.lints.iter().any(|&lint| cx.is_enabled(lint)) {
            (pass.run)(&mut cx, input);
        }
    }

    run_hir_passes(&mut cx, lints::hir_passes(input));

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
