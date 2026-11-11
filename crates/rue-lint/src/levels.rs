use std::{collections::HashMap, ops::Range};

use rowan::Direction;
use rue_ast::{AstAttribute, AstDocument, AstNode};
use rue_diagnostic::{
    Diagnostic, DiagnosticKind, DiagnosticSeverity, ForbiddenBy, LineCol, Lint, LintLevel,
    LintLevels, LintName, Source, SourceKind, SrcLoc,
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
    /// Where the attribute starts, which may be after the start of the range if there are earlier
    /// attributes on the same node.
    attribute: usize,
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
                    let Some(name) = LintName::from_name(argument.text()) else {
                        diagnostics.push(Diagnostic::new(
                            srcloc(source, argument.text_range()),
                            DiagnosticKind::UnknownLint(argument.text().to_string()),
                            DiagnosticSeverity::Warning,
                        ));
                        continue;
                    };

                    let mut forbidden = None;

                    // A group sets the level of each lint in it, so a later attribute on the same
                    // node, or an attribute on a node inside it, overrides it for a single lint
                    for &lint in name.lints() {
                        let scopes = scopes.entry((source.kind.clone(), lint)).or_default();

                        // A forbidden lint can't be set to any other level, by the project or by
                        // an enclosing attribute (including an earlier one on the same node)
                        let (current, forbidden_by) = match enclosing_scope(scopes, &range) {
                            Some(scope) => (
                                scope.level,
                                ForbiddenBy::Attribute {
                                    line: LineCol::new(&source.text, scope.attribute).line + 1,
                                },
                            ),
                            None => (project.get(lint), ForbiddenBy::Project),
                        };

                        if !current.can_change_to(level) {
                            forbidden.get_or_insert((lint, forbidden_by));
                            continue;
                        }

                        scopes.push(LintScope {
                            range: range.clone(),
                            level,
                            attribute: attribute.syntax().text_range().start().into(),
                        });
                    }

                    // The other lints in a group are still set if some of them are forbidden
                    if let Some((lint, forbidden_by)) = forbidden {
                        diagnostics.push(Diagnostic::new(
                            srcloc(source, argument.text_range()),
                            name.forbidden_override(lint, level, forbidden_by),
                            DiagnosticSeverity::Error,
                        ));
                    }
                }
            }
        }

        (Self { project, scopes }, diagnostics)
    }

    /// The level of a lint reported at the given location, which is set by the innermost
    /// attribute that encloses it, or by the project if there isn't one.
    pub fn get(&self, lint: Lint, srcloc: &SrcLoc) -> LintLevel {
        self.scopes
            .get(&(srcloc.source.kind.clone(), lint))
            .and_then(|scopes| enclosing_scope(scopes, &srcloc.span))
            .map_or_else(|| self.project.get(lint), |scope| scope.level)
    }

    /// Whether the lint is reported anywhere, so that passes can skip work for allowed lints.
    pub fn is_enabled_anywhere(&self, lint: Lint) -> bool {
        self.project.get(lint) != LintLevel::Allow
            || self.scopes.iter().any(|((_, scope_lint), scopes)| {
                *scope_lint == lint && scopes.iter().any(|scope| scope.level != LintLevel::Allow)
            })
    }
}

/// The innermost of a lint's scopes that encloses the given range, if there is one.
fn enclosing_scope<'a>(scopes: &'a [LintScope], range: &Range<usize>) -> Option<&'a LintScope> {
    scopes
        .iter()
        .rev()
        .find(|scope| scope.range.start <= range.start && range.end <= scope.range.end)
}

/// The item or statement that an attribute applies to. Some statements (such as an `inline if`
/// followed by a semicolon) begin with an expression that the attributes are parsed into, so this
/// is the closest enclosing item or statement that starts with the attributes.
fn target(attribute: &AstAttribute) -> Option<SyntaxNode> {
    // An attribute can follow other attributes, so find where the first of them starts
    let start = attribute
        .syntax()
        .siblings_with_tokens(Direction::Prev)
        .filter(|element| !element.kind().is_trivia())
        .take_while(|element| element.kind() == SyntaxKind::Attribute)
        .last()?
        .text_range()
        .start();

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
