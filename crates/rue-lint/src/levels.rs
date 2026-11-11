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
    /// The expectations of `expect` attributes, in the order they appear in each document.
    expectations: Vec<Expectation>,
}

/// A lint level set by an attribute, which applies to the item or statement it's attached to.
#[derive(Debug, Clone)]
struct LintScope {
    range: Range<usize>,
    level: LintLevel,
    /// Where the attribute starts, which may be after the start of the range if there are earlier
    /// attributes on the same node.
    attribute: usize,
    /// The index of the expectation, if this scope was set by an `expect` attribute.
    expectation: Option<usize>,
}

/// An `expect` attribute allows a lint like `allow` does, but expects it to be reported inside of
/// its scope. It's fulfilled once that happens, and is reported as unfulfilled otherwise. For a
/// group, any lint in the group fulfills it.
#[derive(Debug, Clone)]
struct Expectation {
    name: LintName,
    /// The lint or group name in the attribute.
    srcloc: SrcLoc,
    /// The range of the item or statement that the attribute is attached to.
    range: Range<usize>,
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

                // The name of the level in the attribute, for reporting forbidden lints
                let level_name = if expect { "expect" } else { level.name() };

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
                    let mut expected = None;

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

                        // The lints in a group share an expectation, which is only created if one
                        // of them isn't forbidden
                        let expectation = expect.then(|| {
                            *expected.get_or_insert_with(|| {
                                expectations.push(Expectation {
                                    name,
                                    srcloc: srcloc(source, argument.text_range()),
                                    range: range.clone(),
                                    fulfilled: false,
                                });
                                expectations.len() - 1
                            })
                        });

                        scopes.push(LintScope {
                            range: range.clone(),
                            level,
                            attribute: attribute.syntax().text_range().start().into(),
                            expectation,
                        });
                    }

                    // The other lints in a group are still set if some of them are forbidden
                    if let Some((lint, forbidden_by)) = forbidden {
                        diagnostics.push(Diagnostic::new(
                            srcloc(source, argument.text_range()),
                            name.forbidden_override(lint, level_name, forbidden_by),
                            DiagnosticSeverity::Error,
                        ));
                    }
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

    /// The level of a lint that is being reported at the given location, which is set by the
    /// innermost attribute that encloses it, or by the project if there isn't one. If that's an
    /// `expect` attribute, this fulfills its expectation.
    pub fn report(&mut self, lint: Lint, srcloc: &SrcLoc) -> LintLevel {
        self.report_outside(lint, srcloc, None)
    }

    /// Like [`Self::report`], but ignores the attributes on the item or statement with the given
    /// range.
    fn report_outside(
        &mut self,
        lint: Lint,
        srcloc: &SrcLoc,
        excluded: Option<&Range<usize>>,
    ) -> LintLevel {
        let Some(scopes) = self.scopes.get(&(srcloc.source.kind.clone(), lint)) else {
            return self.project.get(lint);
        };

        let Some(innermost) = enclosing_scope_outside(scopes, &srcloc.span, excluded) else {
            return self.project.get(lint);
        };

        // The lint can be expected more than once on the same item or statement, and all of
        // those expectations are fulfilled together
        if innermost.expectation.is_some() {
            for scope in scopes {
                if scope.range == innermost.range
                    && let Some(index) = scope.expectation
                {
                    self.expectations[index].fulfilled = true;
                }
            }
        }

        innermost.level
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
    pub(crate) fn report_unfulfilled_expectations(&mut self) -> Vec<(LintName, SrcLoc, LintLevel)> {
        // Reporting an unfulfilled expectation can fulfill an expectation of
        // `unfulfilled_lint_expectation` that encloses it, so those are checked last. They're in
        // preorder, so they're checked in reverse to go from the innermost outwards.
        let (mut last, mut order): (Vec<usize>, Vec<usize>) = (0..self.expectations.len())
            .partition(|&index| {
                self.expectations[index]
                    .name
                    .lints()
                    .contains(&Lint::UnfulfilledLintExpectation)
            });

        last.reverse();
        order.append(&mut last);

        let mut unfulfilled = Vec::new();

        for index in order {
            let expectation = &self.expectations[index];

            if expectation.fulfilled {
                continue;
            }

            let name = expectation.name;
            let srcloc = expectation.srcloc.clone();

            // The attribute is inside of its own scope, but an expectation can't fulfill itself or
            // other expectations on the same item or statement
            let range = expectation.range.clone();
            let level =
                self.report_outside(Lint::UnfulfilledLintExpectation, &srcloc, Some(&range));

            unfulfilled.push((name, srcloc, level));
        }

        unfulfilled
    }
}

/// The innermost of a lint's scopes that encloses the given range, if there is one.
fn enclosing_scope<'a>(scopes: &'a [LintScope], range: &Range<usize>) -> Option<&'a LintScope> {
    enclosing_scope_outside(scopes, range, None)
}

/// Like [`enclosing_scope`], but ignores the scopes of attributes on the item or statement with
/// the excluded range.
fn enclosing_scope_outside<'a>(
    scopes: &'a [LintScope],
    range: &Range<usize>,
    excluded: Option<&Range<usize>>,
) -> Option<&'a LintScope> {
    scopes.iter().rev().find(|scope| {
        scope.range.start <= range.start
            && range.end <= scope.range.end
            && excluded.is_none_or(|excluded| scope.range != *excluded)
    })
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
