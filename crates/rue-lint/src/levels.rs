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
    /// The expectations of `expect` attributes, in the order they appear in each document.
    expectations: Vec<Expectation>,
}

/// A lint level set by an attribute, which applies to the item or statement it's attached to.
#[derive(Debug, Clone)]
struct LintScope {
    range: Range<usize>,
    level: LintLevel,
    /// The index of the expectation, if this scope was set by an `expect` attribute.
    expectation: Option<usize>,
}

/// An `expect` attribute allows a lint like `allow` does, but expects it to be reported inside of
/// its scope. It's fulfilled once that happens, and is reported as unfulfilled otherwise.
#[derive(Debug, Clone)]
struct Expectation {
    lint: Lint,
    /// The lint name in the attribute.
    srcloc: SrcLoc,
    fulfilled: bool,
}

impl ScopedLintLevels {
    /// Collects the lint attributes in the documents, along with diagnostics for any that are
    /// invalid.
    pub fn new(
        project: LintLevels,
        documents: &[(&Source, &AstDocument)],
    ) -> (Self, Vec<Diagnostic>) {
        let mut scopes = HashMap::<(SourceKind, Lint), Vec<LintScope>>::new();
        let mut expectations = Vec::new();
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

                // An `expect` allows the lint, and also records whether it was reported
                let (level, expect) = if name.text() == "expect" {
                    (Ok(LintLevel::Allow), true)
                } else {
                    (name.text().parse::<LintLevel>(), false)
                };

                let Ok(level) = level else {
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

                    let expectation = expect.then(|| {
                        expectations.push(Expectation {
                            lint,
                            srcloc: srcloc(source, argument.text_range()),
                            fulfilled: false,
                        });
                        expectations.len() - 1
                    });

                    scopes
                        .entry((source.kind.clone(), lint))
                        .or_default()
                        .push(LintScope {
                            range: range.clone(),
                            level,
                            expectation,
                        });
                }
            }
        }

        (
            Self {
                project,
                scopes,
                expectations,
            },
            diagnostics,
        )
    }

    /// The level of a lint reported at the given location, which is set by the innermost
    /// attribute that encloses it, or by the project if there isn't one.
    pub fn get(&self, lint: Lint, srcloc: &SrcLoc) -> LintLevel {
        self.scope(lint, srcloc, None)
            .map_or_else(|| self.project.get(lint), |scope| scope.level)
    }

    /// The level of a lint that is being reported at the given location, like [`Self::get`].
    /// If the level is set by an `expect` attribute, this fulfills its expectation.
    pub fn report(&mut self, lint: Lint, srcloc: &SrcLoc) -> LintLevel {
        self.report_except(lint, srcloc, None)
    }

    fn report_except(&mut self, lint: Lint, srcloc: &SrcLoc, except: Option<usize>) -> LintLevel {
        let Some(scope) = self.scope(lint, srcloc, except) else {
            return self.project.get(lint);
        };

        let level = scope.level;

        if let Some(index) = scope.expectation {
            self.expectations[index].fulfilled = true;
        }

        level
    }

    /// Whether the lint is reported anywhere, so that passes can skip work for allowed lints.
    /// An `expect` attribute counts, since the lint has to run to know whether it's fulfilled.
    pub fn is_enabled_anywhere(&self, lint: Lint) -> bool {
        self.project.get(lint) != LintLevel::Allow
            || self.scopes.iter().any(|((_, scope_lint), scopes)| {
                *scope_lint == lint
                    && scopes
                        .iter()
                        .any(|scope| scope.level != LintLevel::Allow || scope.expectation.is_some())
            })
    }

    /// Reports the expectations that were never fulfilled as `unfulfilled_lint_expectation`, once
    /// every other lint has been reported. Returns the expected lint, the location and the level
    /// of each one.
    pub(crate) fn report_unfulfilled_expectations(&mut self) -> Vec<(Lint, SrcLoc, LintLevel)> {
        // Reporting an unfulfilled expectation can fulfill an expectation of
        // `unfulfilled_lint_expectation` that encloses it, so those are checked last. They're in
        // preorder, so they're checked in reverse to go from the innermost outwards.
        let (mut last, mut order): (Vec<usize>, Vec<usize>) = (0..self.expectations.len())
            .partition(|&index| self.expectations[index].lint == Lint::UnfulfilledLintExpectation);

        last.reverse();
        order.append(&mut last);

        let mut unfulfilled = Vec::new();

        for index in order {
            let expectation = &self.expectations[index];

            if expectation.fulfilled {
                continue;
            }

            let lint = expectation.lint;
            let srcloc = expectation.srcloc.clone();

            // The attribute is inside of its own scope, but an expectation can't fulfill itself
            let level = self.report_except(Lint::UnfulfilledLintExpectation, &srcloc, Some(index));

            unfulfilled.push((lint, srcloc, level));
        }

        unfulfilled
    }

    /// The innermost attribute that sets the level of a lint at the given location, ignoring the
    /// one with the given expectation.
    fn scope(&self, lint: Lint, srcloc: &SrcLoc, except: Option<usize>) -> Option<&LintScope> {
        self.scopes
            .get(&(srcloc.source.kind.clone(), lint))
            .and_then(|scopes| {
                scopes.iter().rev().find(|scope| {
                    scope.range.start <= srcloc.span.start
                        && srcloc.span.end <= scope.range.end
                        && (except.is_none() || scope.expectation != except)
                })
            })
    }
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
