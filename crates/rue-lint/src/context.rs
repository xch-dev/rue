use std::ops::Range;

use rowan::TextRange;
use rue_diagnostic::{Diagnostic, DiagnosticKind, Lint, LintLevel, LintLevels, Source, SrcLoc};
use rue_hir::{Database, ScopeId};
use rue_types::{BuiltinTypes, Comparison, TypeId};

/// The state shared by lint passes, which collects the diagnostics they report.
#[derive(Debug)]
pub struct LintContext<'a> {
    db: &'a mut Database,
    types: &'a BuiltinTypes,
    levels: LintLevels,
    diagnostics: Vec<Diagnostic>,
}

impl<'a> LintContext<'a> {
    pub fn new(db: &'a mut Database, types: &'a BuiltinTypes, levels: LintLevels) -> Self {
        Self {
            db,
            types,
            levels,
            diagnostics: Vec::new(),
        }
    }

    pub fn db(&self) -> &Database {
        self.db
    }

    pub fn builtin_types(&self) -> &BuiltinTypes {
        self.types
    }

    pub fn level(&self, lint: Lint) -> LintLevel {
        self.levels.get(lint)
    }

    /// Whether the lint will be reported, so that passes can skip work for allowed lints.
    pub fn is_enabled(&self, lint: Lint) -> bool {
        self.level(lint) != LintLevel::Allow
    }

    /// Reports a lint at its configured level. Nothing is reported if the lint is allowed.
    pub fn report(&mut self, lint: Lint, srcloc: SrcLoc, message: impl Into<String>) {
        let Some(severity) = self.level(lint).severity() else {
            return;
        };

        self.diagnostics.push(Diagnostic::new(
            srcloc,
            DiagnosticKind::Lint {
                lint,
                message: message.into(),
            },
            severity,
        ));
    }

    pub fn into_diagnostics(self) -> Vec<Diagnostic> {
        self.diagnostics
    }

    pub fn compare(&mut self, from: TypeId, to: TypeId) -> Comparison {
        rue_types::compare(self.db.types_mut(), self.types, from, to)
    }

    pub fn is_assignable(&mut self, from: TypeId, to: TypeId) -> bool {
        self.compare(from, to) == Comparison::Assign
    }

    pub fn is_castable(&mut self, from: TypeId, to: TypeId) -> bool {
        matches!(
            self.compare(from, to),
            Comparison::Assign | Comparison::Cast
        )
    }

    /// The name of a type as it would be written in the given scope.
    pub fn type_name(&mut self, scope: ScopeId, ty: TypeId) -> String {
        self.db.type_name_in(scope, ty)
    }
}

pub(crate) fn srcloc(source: &Source, range: TextRange) -> SrcLoc {
    let span: Range<usize> = range.start().into()..range.end().into();
    SrcLoc::new(source.clone(), span)
}
