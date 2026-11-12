use std::ops::Range;

use rowan::TextRange;
use rue_diagnostic::{
    Applicability, Diagnostic, DiagnosticKind, Lint, LintLevel, LintName, Name, Source, SourceKind,
    SrcLoc,
};
use rue_hir::{Database, Declaration, HirId, ScopeId, SymbolId};
use rue_types::{BuiltinTypes, Comparison, Type, TypeId};

use crate::{
    ScopedLintLevels, WalkState,
    text::{comment_count, separate_words},
};

/// The state shared by lint passes, which collects the diagnostics they report.
#[derive(Debug)]
pub struct LintContext<'a> {
    db: &'a mut Database,
    types: &'a BuiltinTypes,
    levels: ScopedLintLevels,
    diagnostics: Vec<Diagnostic>,
    pub(crate) walk: WalkState,
}

impl<'a> LintContext<'a> {
    pub fn new(db: &'a mut Database, types: &'a BuiltinTypes, levels: ScopedLintLevels) -> Self {
        Self {
            db,
            types,
            levels,
            diagnostics: Vec::new(),
            walk: WalkState::default(),
        }
    }

    /// The function, constant or struct being walked by the [`HirLintPass`](crate::HirLintPass)es.
    /// For a struct, this is one of its field defaults.
    pub fn enclosing_declaration(&self) -> Option<Declaration> {
        self.walk.declaration
    }

    /// The innermost function or lambda being walked, if any.
    pub fn enclosing_function(&self) -> Option<SymbolId> {
        self.walk.functions.last().copied()
    }

    /// The HIR nodes that enclose the current one, from the outermost to the innermost. During
    /// [`check_statement`](crate::HirLintPass::check_statement), the innermost is the block.
    pub fn parents(&self) -> &[HirId] {
        &self.walk.parents
    }

    /// The HIR node that directly encloses the current one, if any.
    pub fn parent(&self) -> Option<HirId> {
        self.walk.parents.last().copied()
    }

    pub(crate) fn db_mut(&mut self) -> &mut Database {
        self.db
    }

    pub fn db(&self) -> &Database {
        self.db
    }

    pub fn builtin_types(&self) -> &BuiltinTypes {
        self.types
    }

    /// Whether the lint could be reported anywhere, so that passes can skip work for allowed lints.
    pub fn is_enabled(&self, lint: Lint) -> bool {
        self.levels.is_enabled_anywhere(lint)
    }

    /// Whether reporting the lint at the location would have any effect, so that passes can skip
    /// expensive checks where it's allowed.
    pub fn is_enabled_at(&self, lint: Lint, srcloc: &SrcLoc) -> bool {
        self.levels.is_enabled_at(lint, srcloc)
    }

    /// Reports a lint at its configured level, and returns the diagnostic so that notes and
    /// suggestions can be added to it. Nothing is reported if the lint is allowed.
    pub fn report(
        &mut self,
        lint: Lint,
        srcloc: SrcLoc,
        message: impl Into<String>,
    ) -> Option<&mut Diagnostic> {
        let level = self.levels.report(lint, &srcloc);
        self.push(lint, srcloc, message, level)
    }

    /// Reports the names in attributes that don't refer to a known lint or group. An attribute
    /// can allow `unknown_lint` for the names in itself, as in Rust.
    pub(crate) fn report_unknown_lints(&mut self) {
        for (name, srcloc) in self.levels.take_unknown_lints() {
            self.report(Lint::UnknownLint, srcloc, format!("Unknown lint `{name}`"));
        }
    }

    /// Reports the `expect` attributes whose lint was never reported inside of them. This must be
    /// called after every pass has run.
    pub(crate) fn report_unfulfilled_expectations(&mut self) {
        for (name, srcloc, level) in self.levels.report_unfulfilled_expectations() {
            let message = match name {
                LintName::Lint(Lint::UnfulfilledLintExpectation) => {
                    "Lint `unfulfilled_lint_expectation` can't be expected, so this is always reported"
                        .to_string()
                }
                LintName::Lint(lint) => {
                    format!("Expected lint `{lint}` to be reported here, but it never was")
                }
                LintName::Group(group) => format!(
                    "Expected a lint in group `{group}` to be reported here, but none of them were"
                ),
            };

            self.push(Lint::UnfulfilledLintExpectation, srcloc, message, level);
        }
    }

    fn push(
        &mut self,
        lint: Lint,
        srcloc: SrcLoc,
        message: impl Into<String>,
        level: LintLevel,
    ) -> Option<&mut Diagnostic> {
        let severity = level.severity()?;

        self.diagnostics.push(Diagnostic::new(
            srcloc,
            DiagnosticKind::Lint {
                lint,
                message: message.into(),
            },
            severity,
        ));

        self.diagnostics.last_mut()
    }

    /// The diagnostics that were reported. A suggestion that would remove a comment (because the
    /// code it replaces has more comments than its replacement) isn't applied automatically, since
    /// the comment might matter. Replacements that would join a word to the code around them (such
    /// as `return+a` becoming `returna`) get a space.
    pub fn into_diagnostics(mut self) -> Vec<Diagnostic> {
        for suggestion in self
            .diagnostics
            .iter_mut()
            .flat_map(|diagnostic| &mut diagnostic.suggestions)
            .filter(|suggestion| suggestion.applicability == Applicability::MachineApplicable)
        {
            for edit in &mut suggestion.edits {
                separate_words(
                    &edit.srcloc.source.text,
                    edit.srcloc.span.clone(),
                    &mut edit.replacement,
                );
            }

            if suggestion
                .edits
                .iter()
                .any(|edit| comment_count(edit.srcloc.text()) > comment_count(&edit.replacement))
            {
                suggestion.applicability = Applicability::MaybeIncorrect;
            }
        }

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

    /// Whether the type is `Never`, so a value of it is never produced, such as because the
    /// expression always raises.
    pub fn is_never(&mut self, ty: TypeId) -> bool {
        let semantic = self.without_aliases(ty);
        matches!(self.db.ty(semantic), Type::Never)
    }

    /// The type that an alias refers to, or the type itself if it isn't an alias.
    pub fn without_aliases(&mut self, ty: TypeId) -> TypeId {
        rue_types::unwrap_semantic(self.db.types_mut(), ty, true)
    }

    /// Whether the type is unresolved, or contains an unresolved type anywhere inside of it,
    /// because an error has already been reported for it. Such types are compatible with
    /// everything, so lints that compare them would report errors twice.
    pub fn contains_unresolved(&self, ty: TypeId) -> bool {
        rue_types::contains_unresolved(self.db.types(), ty)
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

/// Identifies a declaration by the location of its name, which every copy of it (such as a struct
/// for generic arguments or narrowing) shares.
pub(crate) type NameKey = (SourceKind, usize);

pub(crate) fn name_key(name: &Name) -> Option<NameKey> {
    name.srcloc().map(srcloc_key)
}

pub(crate) fn srcloc_key(srcloc: &SrcLoc) -> NameKey {
    (srcloc.source.kind.clone(), srcloc.span.start)
}

/// The key of a name, from the token it's written as in the syntax tree.
pub(crate) fn token_key(source: &Source, token: &rue_parser::SyntaxToken) -> NameKey {
    (source.kind.clone(), token.text_range().start().into())
}
