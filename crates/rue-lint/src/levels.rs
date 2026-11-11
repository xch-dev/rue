use std::{collections::HashMap, ops::Range};

use rue_ast::{AstAttribute, AstDocument, AstNode};
use rue_diagnostic::{
    Diagnostic, DiagnosticKind, DiagnosticSeverity, Lint, LintLevel, LintLevels, Source,
    SourceKind, SrcLoc,
};
use rue_parser::{SyntaxKind, SyntaxNode};

use crate::srcloc;

/// The level of every lint at each location, which combines the project's levels with the
/// attributes that override them in the source code (such as `#[allow(unused_binding)]`).
#[derive(Debug, Clone)]
pub struct ScopedLintLevels {
    project: LintLevels,
    scopes: HashMap<(SourceKind, Lint), Vec<LintScope>>,
}

/// A lint level set by an attribute, which applies to the item or statement it's attached to.
#[derive(Debug, Clone)]
struct LintScope {
    range: Range<usize>,
    level: LintLevel,
}

impl ScopedLintLevels {
    /// Collects the lint attributes in the documents, along with diagnostics for any that are
    /// invalid.
    pub fn new(
        project: LintLevels,
        documents: &[(&Source, &AstDocument)],
    ) -> (Self, Vec<Diagnostic>) {
        let mut scopes = HashMap::<(SourceKind, Lint), Vec<LintScope>>::new();
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

                let Some(target) = target(&attribute) else {
                    // At the top level, this is after an item that failed to parse, which has
                    // already been reported by the parser
                    if attribute.syntax().parent().map(|parent| parent.kind())
                        != Some(SyntaxKind::Document)
                    {
                        error(
                            DiagnosticKind::MisplacedAttribute,
                            attribute.syntax().text_range(),
                        );
                    }
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

                let range = target.text_range();
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

                    let scopes = scopes.entry((source.kind.clone(), lint)).or_default();

                    // A forbidden lint can't be set to any other level, by the project or by an
                    // enclosing attribute (including an earlier one on the same node)
                    if level != LintLevel::Forbid
                        && level_at(&project, scopes, lint, &range) == LintLevel::Forbid
                    {
                        diagnostics.push(Diagnostic::new(
                            srcloc(source, argument.text_range()),
                            DiagnosticKind::ForbiddenLintOverride { lint, level },
                            DiagnosticSeverity::Error,
                        ));
                        continue;
                    }

                    scopes.push(LintScope {
                        range: range.clone(),
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
        let scopes = self
            .scopes
            .get(&(srcloc.source.kind.clone(), lint))
            .map_or(&[][..], Vec::as_slice);

        level_at(&self.project, scopes, lint, &srcloc.span)
    }

    /// Whether the lint is reported anywhere, so that passes can skip work for allowed lints.
    pub fn is_enabled_anywhere(&self, lint: Lint) -> bool {
        self.project.get(lint) != LintLevel::Allow
            || self.scopes.iter().any(|((_, scope_lint), scopes)| {
                *scope_lint == lint && scopes.iter().any(|scope| scope.level != LintLevel::Allow)
            })
    }
}

/// The level of a lint over the given range, which is set by the innermost of the lint's scopes
/// that encloses it, or by the project if there isn't one.
fn level_at(
    project: &LintLevels,
    scopes: &[LintScope],
    lint: Lint,
    range: &Range<usize>,
) -> LintLevel {
    scopes
        .iter()
        .rev()
        .find(|scope| scope.range.start <= range.start && range.end <= scope.range.end)
        .map_or_else(|| project.get(lint), |scope| scope.level)
}

/// The item or statement that an attribute applies to. Some statements (such as an `inline if`
/// followed by a semicolon) begin with an expression that the attributes are parsed into, so this
/// is the closest enclosing item or statement that starts with the attribute.
fn target(attribute: &AstAttribute) -> Option<SyntaxNode> {
    let start = attribute.syntax().text_range().start();

    attribute
        .syntax()
        .ancestors()
        .skip(1)
        .take_while(|node| node.text_range().start() == start)
        .find(|node| accepts_attributes(node.kind()))
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
