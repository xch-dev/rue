use std::{collections::HashMap, ops::Range};

use rowan::Direction;
use rue_ast::{AstAttribute, AstDocument, AstModuleItem, AstNode};
use rue_diagnostic::{
    AttributeLevel, Diagnostic, DiagnosticKind, DiagnosticSeverity, ForbiddenBy, LineCol, Lint,
    LintLevel, LintLevels, LintName, Source, SourceKind, SrcLoc,
};
use rue_parser::{SyntaxKind, SyntaxNode, T};

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

/// A lint level set by an attribute, which applies to the item or statement it's attached to, or
/// to the whole file or module for an inner attribute.
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

                let inner = attribute.is_inner();

                let target = if inner {
                    inner_target(&attribute)
                } else {
                    target(&attribute)
                };

                if target.is_none() {
                    // This is after an item that failed to parse, which has already been reported
                    // by the parser
                    if is_after_failed_item(&attribute) {
                        continue;
                    }

                    // The rest of the attribute is still checked, so that any other mistakes in
                    // it are reported at the same time
                    error(
                        if inner {
                            DiagnosticKind::MisplacedInnerAttribute
                        } else {
                            DiagnosticKind::MisplacedAttribute
                        },
                        attribute.syntax().text_range(),
                    );
                }

                // Malformed attributes have already been reported by the parser
                let Some(name) = attribute.name() else {
                    continue;
                };

                let Ok(attribute_level) = name.text().parse::<AttributeLevel>() else {
                    error(
                        DiagnosticKind::UnknownAttribute(name.text().to_string()),
                        name.text_range(),
                    );
                    continue;
                };

                // An `expect` allows the lint, and also records whether it was reported
                let level = attribute_level.lint_level();
                let expect = attribute_level == AttributeLevel::Expect;

                let mut arguments = attribute.arguments().peekable();

                if arguments.peek().is_none() {
                    error(
                        DiagnosticKind::MissingLintNames(name.text().to_string()),
                        attribute.syntax().text_range(),
                    );
                    continue;
                }

                let range = target.map(|target| {
                    let range = target.text_range();
                    range.start().into()..range.end().into()
                });

                for argument in arguments {
                    let Some(name) = LintName::from_name(argument.text()) else {
                        diagnostics.push(Diagnostic::new(
                            srcloc(source, argument.text_range()),
                            DiagnosticKind::UnknownLint(argument.text().to_string()),
                            DiagnosticSeverity::Warning,
                        ));
                        continue;
                    };

                    // Misplaced attributes don't apply to anything
                    let Some(range) = &range else {
                        continue;
                    };

                    // `unfulfilled_lint_expectation` can't be expected, since reporting it would
                    // fulfill its own expectation. As in Rust, the attribute doesn't allow it, and
                    // it's always reported.
                    if expect && name == LintName::Lint(Lint::UnfulfilledLintExpectation) {
                        expectations.push(Expectation {
                            name,
                            srcloc: srcloc(source, argument.text_range()),
                            fulfilled: false,
                        });
                        continue;
                    }

                    let mut forbidden = None;
                    let mut expected = None;

                    // A group sets the level of each lint in it, so a later attribute on the same
                    // node, or an attribute on a node inside it, overrides it for a single lint
                    for &lint in name.lints() {
                        // A group that contains `unfulfilled_lint_expectation` can't expect it
                        // either, so its level is left unchanged
                        if expect && lint == Lint::UnfulfilledLintExpectation {
                            continue;
                        }

                        let scopes = scopes.entry((source.kind.clone(), lint)).or_default();

                        // A forbidden lint can't be set to any other level, by the project or by
                        // an enclosing attribute (including an earlier one on the same node)
                        let (current, forbidden_by) = match enclosing_scope(scopes, range) {
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
                            name.forbidden_override(lint, attribute_level, forbidden_by),
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
        let Some(scopes) = self.scopes.get(&(srcloc.source.kind.clone(), lint)) else {
            return self.project.get(lint);
        };

        let Some(position) = scopes
            .iter()
            .rposition(|scope| encloses(&scope.range, &srcloc.span))
        else {
            return self.project.get(lint);
        };

        let innermost = &scopes[position];

        // The lint can be expected more than once on the same item or statement, and those
        // expectations are fulfilled together, unless another attribute between them overrides
        // the earlier ones. The scopes of a node's attributes are next to each other.
        for scope in scopes[..=position]
            .iter()
            .rev()
            .take_while(|scope| scope.range == innermost.range && scope.expectation.is_some())
        {
            if let Some(index) = scope.expectation {
                self.expectations[index].fulfilled = true;
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
    /// every other lint has been reported. Returns the expected lint or group, the location and
    /// the level of each one.
    pub(crate) fn report_unfulfilled_expectations(&mut self) -> Vec<(LintName, SrcLoc, LintLevel)> {
        let unfulfilled = self
            .expectations
            .iter()
            .filter(|expectation| !expectation.fulfilled)
            .map(|expectation| (expectation.name, expectation.srcloc.clone()))
            .collect::<Vec<_>>();

        // These reports can't fulfill any expectations, since `unfulfilled_lint_expectation`
        // can't be expected
        unfulfilled
            .into_iter()
            .map(|(name, srcloc)| {
                let level = self.report(Lint::UnfulfilledLintExpectation, &srcloc);
                (name, srcloc, level)
            })
            .collect()
    }
}

/// The innermost of a lint's scopes that encloses the given range, if there is one.
fn enclosing_scope<'a>(scopes: &'a [LintScope], range: &Range<usize>) -> Option<&'a LintScope> {
    scopes
        .iter()
        .rev()
        .find(|scope| encloses(&scope.range, range))
}

fn encloses(outer: &Range<usize>, inner: &Range<usize>) -> bool {
    outer.start <= inner.start && inner.end <= outer.end
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

/// The file or module that an inner attribute applies to, if it's at the start of one. Anywhere
/// else, it's misplaced.
fn inner_target(attribute: &AstAttribute) -> Option<SyntaxNode> {
    let parent = attribute.syntax().parent()?;

    let is_attribute = |inner: AstAttribute| inner.syntax() == attribute.syntax();

    let at_start = if let Some(document) = AstDocument::cast(parent.clone()) {
        document.inner_attributes().any(is_attribute)
    } else if let Some(module) = AstModuleItem::cast(parent.clone()) {
        module.inner_attributes().any(is_attribute)
    } else {
        false
    };

    at_start.then_some(parent)
}

/// Whether an attribute that doesn't apply to anything was left in the file or a module's body by
/// an item that failed to parse, rather than being attached to the wrong kind of node.
fn is_after_failed_item(attribute: &AstAttribute) -> bool {
    let Some(parent) = attribute.syntax().parent() else {
        return false;
    };

    match parent.kind() {
        SyntaxKind::Document => true,
        // The attributes before `mod` are attached to the module, and the ones in its body are
        // after the opening brace
        SyntaxKind::ModuleItem => attribute
            .syntax()
            .siblings_with_tokens(Direction::Prev)
            .any(|element| element.kind() == T!['{']),
        _ => false,
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
