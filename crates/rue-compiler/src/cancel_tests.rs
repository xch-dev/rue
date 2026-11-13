use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use rue_diagnostic::{Diagnostic, DiagnosticSeverity, Source, SourceKind};
use rue_options::CompilerOptions;

use crate::{Cancel, Compiler, File, FileTree};

/// Five functions with a type error each, and nested modules with another two.
const SOURCE: &str = "
fn a() -> Int { \"a\" }
fn b() -> Int { \"b\" }
fn c() -> Int { \"c\" }
mod inner {
    fn d() -> Int { \"d\" }
    fn e() -> Int { \"e\" }
}
";

/// Compiles the source, with a cancel that says to stop once it has been asked `after` times
/// (or never), and returns the diagnostics, how many times it was asked, and whether the compile
/// stopped.
fn compile(after: Option<usize>) -> (Vec<Diagnostic>, usize, bool) {
    let mut ctx = Compiler::new(CompilerOptions {
        std: false,
        ..CompilerOptions::default()
    });

    let asked = Arc::new(AtomicUsize::new(0));

    ctx.set_cancel(Some(Cancel::new({
        let asked = asked.clone();
        move || {
            let count = asked.fetch_add(1, Ordering::Relaxed) + 1;
            after.is_some_and(|after| count > after)
        }
    })));

    let tree = FileTree::File(File::new(
        &mut ctx,
        "main".to_string(),
        Source::new(Arc::from(SOURCE), SourceKind::File("main.rue".to_string())),
    ));

    tree.compile(&mut ctx);

    let cancelled = ctx.is_cancelled();
    (
        ctx.take_diagnostics(),
        asked.load(Ordering::Relaxed),
        cancelled,
    )
}

fn errors(diagnostics: &[Diagnostic]) -> usize {
    diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.severity == DiagnosticSeverity::Error)
        .count()
}

#[test]
fn compile_that_is_not_cancelled_is_complete() {
    let (diagnostics, asked, cancelled) = compile(None);

    assert!(!cancelled);
    assert_eq!(errors(&diagnostics), 5);

    // The functions are never used, so lints ran
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.lint().is_some())
    );

    // It's asked before the bodies, before each item (including the module and the items in
    // it), and before and after evaluating constants
    assert_eq!(asked, 1 + 6 + 2 + 1);
}

#[test]
fn compile_stops_before_the_next_body() {
    // Asked before the bodies, and then before `a` and `b`, so only `a` and `b` are compiled
    let (diagnostics, asked, cancelled) = compile(Some(3));

    assert!(cancelled);
    assert_eq!(errors(&diagnostics), 2);
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.lint().is_none())
    );

    // Once it says to stop, it isn't asked again
    assert_eq!(asked, 4);
}

#[test]
fn compile_stops_inside_a_module() {
    // `a`, `b`, `c` and `d` are compiled
    let (diagnostics, _, cancelled) = compile(Some(1 + 5));

    assert!(cancelled);
    assert_eq!(errors(&diagnostics), 4);
}

#[test]
fn compile_stops_before_the_bodies() {
    let (diagnostics, asked, cancelled) = compile(Some(0));

    assert!(cancelled);
    assert_eq!(errors(&diagnostics), 0);
    assert_eq!(asked, 1);
}
