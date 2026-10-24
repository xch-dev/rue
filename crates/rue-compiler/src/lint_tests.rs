use std::sync::Arc;

use rue_diagnostic::{Diagnostic, DiagnosticSeverity, Lint, LintLevel, Source, SourceKind};
use rue_options::CompilerOptions;

use crate::{Compiler, File, FileTree};

const SOURCE: &str = "fn main() -> Int {\n    let unused = 1;\n    --42\n}\n";

fn compile(configure: impl FnOnce(&mut CompilerOptions)) -> (Vec<Diagnostic>, bool) {
    compile_source(SOURCE, configure)
}

fn compile_source(
    source: &str,
    configure: impl FnOnce(&mut CompilerOptions),
) -> (Vec<Diagnostic>, bool) {
    let mut options = CompilerOptions {
        std: false,
        ..CompilerOptions::default()
    };
    configure(&mut options);

    let mut ctx = Compiler::new(options);

    let tree = FileTree::File(File::new(
        &mut ctx,
        "main".to_string(),
        Source::new(Arc::from(source), SourceKind::File("main.rue".to_string())),
    ));

    tree.compile(&mut ctx);

    let has_errors = ctx.has_errors();
    (ctx.take_diagnostics(), has_errors)
}

fn severities(diagnostics: &[Diagnostic]) -> Vec<(Option<Lint>, DiagnosticSeverity)> {
    diagnostics
        .iter()
        .map(|diagnostic| (diagnostic.lint(), diagnostic.severity))
        .collect()
}

#[test]
fn lints_use_default_levels() {
    let (diagnostics, has_errors) = compile(|_| {});

    assert_eq!(
        severities(&diagnostics),
        vec![
            (Some(Lint::DoubleNegation), DiagnosticSeverity::Warning),
            (Some(Lint::UnusedBinding), DiagnosticSeverity::Warning),
        ]
    );
    assert!(!has_errors);
}

#[test]
fn allowed_lints_are_not_reported() {
    let (diagnostics, has_errors) =
        compile(|options| options.lints.set(Lint::UnusedBinding, LintLevel::Allow));

    assert_eq!(
        severities(&diagnostics),
        vec![(Some(Lint::DoubleNegation), DiagnosticSeverity::Warning)]
    );
    assert!(!has_errors);
}

#[test]
fn denied_lints_are_errors() {
    let (diagnostics, has_errors) =
        compile(|options| options.lints.set(Lint::DoubleNegation, LintLevel::Deny));

    assert_eq!(
        severities(&diagnostics),
        vec![
            (Some(Lint::DoubleNegation), DiagnosticSeverity::Error),
            (Some(Lint::UnusedBinding), DiagnosticSeverity::Warning),
        ]
    );
    assert!(has_errors);
}

#[test]
fn warned_lints_are_warnings() {
    let (diagnostics, has_errors) = compile(|options| {
        options.lints.set(Lint::DoubleNegation, LintLevel::Allow);
        options.lints.set(Lint::UnusedBinding, LintLevel::Warn);
    });

    assert_eq!(
        severities(&diagnostics),
        vec![(Some(Lint::UnusedBinding), DiagnosticSeverity::Warning)]
    );
    assert!(!has_errors);
}

#[test]
fn unnecessary_plus_only_applies_to_ints() {
    let (diagnostics, has_errors) = compile_source("fn main() -> Int {\n    +42\n}\n", |_| {});

    assert_eq!(
        severities(&diagnostics),
        vec![(Some(Lint::UnnecessaryPlus), DiagnosticSeverity::Warning)]
    );
    assert!(!has_errors);

    // `+` on anything else is a type error, so it isn't reported as having no effect
    let (diagnostics, has_errors) =
        compile_source("fn main() -> Int {\n    +\"hello\"\n}\n", |_| {});

    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.lint().is_none())
    );
    assert!(has_errors);
}
