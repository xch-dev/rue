use std::{collections::HashMap, ops::Range};

use rue_ast::{AstAttribute, AstDocument, AstNode};
use rue_diagnostic::{
    Diagnostic, DiagnosticKind, DiagnosticSeverity, Lint, LintLevel, LintLevels, Source,
    SourceKind, SrcLoc,
};
use rue_parser::SyntaxKind;

use crate::srcloc;

/// The level of every lint at each location, which combines the project's levels with the
/// attributes that override them in the source code (such as `#[allow(unused_binding)]`).
#[derive(Debug, Clone)]
pub struct ScopedLintLevels {
    project: LintLevels,
    scopes: HashMap<SourceKind, Vec<LintScope>>,
}

/// A lint level set by an attribute, which applies to the item or statement it's attached to.
#[derive(Debug, Clone)]
struct LintScope {
    range: Range<usize>,
    lint: Lint,
    level: LintLevel,
}

impl ScopedLintLevels {
    /// Collects the lint attributes in the documents, along with diagnostics for any that are
    /// invalid.
    pub fn new(
        project: LintLevels,
        documents: &[(&Source, &AstDocument)],
    ) -> (Self, Vec<Diagnostic>) {
        let mut scopes = HashMap::<SourceKind, Vec<LintScope>>::new();
        let mut diagnostics = Vec::new();

        for &(source, document) in documents {
            // Descendants are visited in preorder, so a scope always comes after the scopes that
            // enclose it, and after the scopes of earlier attributes on the same node.
            for attribute in document
                .syntax()
                .descendants()
                .filter_map(AstAttribute::cast)
            {
                let mut error = |kind, range| {
                    diagnostics.push(Diagnostic::new(
                        srcloc(source, range),
                        kind,
                        DiagnosticSeverity::Error,
                    ));
                };

                let Some(parent) = attribute
                    .syntax()
                    .parent()
                    .filter(|parent| accepts_attributes(parent.kind()))
                else {
                    error(
                        DiagnosticKind::MisplacedAttribute,
                        attribute.syntax().text_range(),
                    );
                    continue;
                };

                // Malformed attributes have already been reported by the parser
                let Some(name) = attribute.name() else {
                    continue;
                };

                let Ok(level) = name.text().parse::<LintLevel>() else {
                    error(
                        DiagnosticKind::UnknownAttribute(name.text().to_string()),
                        name.text_range(),
                    );
                    continue;
                };

                let mut arguments = attribute.arguments().peekable();

                if arguments.peek().is_none() {
                    error(
                        DiagnosticKind::MissingLintNames(name.text().to_string()),
                        attribute.syntax().text_range(),
                    );
                    continue;
                }

                let range = parent.text_range();
                let range = range.start().into()..range.end().into();

                for argument in arguments {
                    let Some(lint) = Lint::from_name(argument.text()) else {
                        diagnostics.push(Diagnostic::new(
                            srcloc(source, argument.text_range()),
                            DiagnosticKind::UnknownLint(argument.text().to_string()),
                            DiagnosticSeverity::Warning,
                        ));
                        continue;
                    };

                    scopes
                        .entry(source.kind.clone())
                        .or_default()
                        .push(LintScope {
                            range: range.clone(),
                            lint,
                            level,
                        });
                }
            }
        }

        (Self { project, scopes }, diagnostics)
    }

    /// The level of a lint reported at the given location, which is set by the innermost
    /// attribute that encloses it, or by the project if there isn't one.
    pub fn get(&self, lint: Lint, srcloc: &SrcLoc) -> LintLevel {
        self.scopes
            .get(&srcloc.source.kind)
            .and_then(|scopes| {
                scopes.iter().rev().find(|scope| {
                    scope.lint == lint
                        && scope.range.start <= srcloc.span.start
                        && srcloc.span.end <= scope.range.end
                })
            })
            .map_or_else(|| self.project.get(lint), |scope| scope.level)
    }

    /// Whether the lint is reported anywhere, so that passes can skip work for allowed lints.
    pub fn is_enabled_anywhere(&self, lint: Lint) -> bool {
        self.project.get(lint) != LintLevel::Allow
            || self
                .scopes
                .values()
                .flatten()
                .any(|scope| scope.lint == lint && scope.level != LintLevel::Allow)
    }
}

/// Whether attributes can be attached to nodes of the given kind.
fn accepts_attributes(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::ModuleItem
            | SyntaxKind::FunctionItem
            | SyntaxKind::ConstantItem
            | SyntaxKind::TypeAliasItem
            | SyntaxKind::StructItem
            | SyntaxKind::ImportItem
            | SyntaxKind::LetStmt
            | SyntaxKind::ExprStmt
            | SyntaxKind::IfStmt
            | SyntaxKind::ReturnStmt
            | SyntaxKind::AssertStmt
            | SyntaxKind::RaiseStmt
            | SyntaxKind::DebugStmt
    )
}
