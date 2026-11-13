use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use rue_diagnostic::{Diagnostic, DiagnosticSeverity, Source, SourceKind};
use rue_options::CompilerOptions;

use crate::{Cancel, Compiler, File, FileTree};

/// A struct whose field has an undeclared type, which is reported while compiling types (before
/// any body), and five functions with a type error each, two of them in a module.
const SOURCE: &str = "
struct S { x: Missing }
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
    assert_eq!(errors(&diagnostics), 6);

    // The functions are never used, so lints ran
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.lint().is_some())
    );

    // The compiler asks before compiling types, before each symbol item (the three functions, the
    // module, and the two functions in it), and before and after evaluating constants. The last
    // one is this test's own `is_cancelled` after the compile. Each place it asks is covered by
    // this count, since a compile that stops early still stops at the next place
    assert_eq!(asked, 1 + 6 + 2 + 1);
}

#[test]
fn compile_stops_before_the_bodies() {
    // Nothing after the imports is compiled, including types
    let (diagnostics, asked, cancelled) = compile(Some(0));

    assert!(cancelled);
    assert_eq!(errors(&diagnostics), 0);
    assert_eq!(asked, 1);
}

#[test]
fn compile_stops_before_the_next_body() {
    // Types are compiled, and so are `a` and `b`, but `c` isn't
    let (diagnostics, asked, cancelled) = compile(Some(3));

    assert!(cancelled);
    assert_eq!(errors(&diagnostics), 1 + 2);
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
    // Types, `a`, `b`, `c` and `d` are compiled, but `e` isn't
    let (diagnostics, _, cancelled) = compile(Some(1 + 5));

    assert!(cancelled);
    assert_eq!(errors(&diagnostics), 1 + 4);
}
