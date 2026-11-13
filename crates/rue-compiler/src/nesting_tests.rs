//! Tests that deeply nested programs don't overflow the stack during compilation, since the parser
//! limits how deeply they can be nested.

use std::{path::PathBuf, sync::Arc, thread};

use clvmr::Allocator;
use rue_diagnostic::{Diagnostic, DiagnosticKind, Source, SourceKind};
use rue_options::CompilerOptions;
use rue_parser::MAX_NESTING_DEPTH;

use crate::{Compiler, File, FileTree};

/// Generates a program with the given number of levels of nesting.
type Shape = fn(usize) -> String;

/// Programs that nest.
const SHAPES: &[(&str, Shape)] = &[
    ("parentheses", |n| {
        format!("fn main() -> Int {{ {}1{} }}", "(".repeat(n), ")".repeat(n))
    }),
    ("pairs", |n| {
        format!(
            "fn main() -> Any {{ {}nil{} }}",
            "(1, ".repeat(n),
            ")".repeat(n)
        )
    }),
    ("lists", |n| {
        format!("fn main() -> Any {{ {}1{} }}", "[".repeat(n), "]".repeat(n))
    }),
    ("blocks", |n| {
        format!("fn main() -> Int {{ {}1{} }}", "{".repeat(n), "}".repeat(n))
    }),
    ("prefix operators", |n| {
        format!("fn main() -> Int {{ {}1 }}", "- ".repeat(n))
    }),
    ("binary operators", |n| {
        format!("fn main() -> Int {{ 1{} }}", " + 1".repeat(n))
    }),
    // Each call is two levels deep, since its arguments are in list items
    ("calls", |n| {
        format!(
            "fn main() -> Int {{ {}1{} }}\n\nfn f(value: Int) -> Int {{ value }}",
            "f(".repeat(n / 2),
            ")".repeat(n / 2)
        )
    }),
    ("casts", |n| {
        format!("fn main() -> Int {{ 1{} }}", " as Int".repeat(n))
    }),
    ("lambdas", |n| {
        format!("fn main() -> Any {{ {}1 }}", "fn() => ".repeat(n))
    }),
    ("if conditions", |n| {
        format!(
            "fn main() -> Bool {{ {}true{} }}",
            "if ".repeat(n),
            " { true } else { false }".repeat(n)
        )
    }),
    ("else if chains", |n| {
        format!(
            "fn main() -> Int {{ if true {{ 1 }}{} else {{ 2 }} }}",
            " else if true { 1 }".repeat(n)
        )
    }),
    ("types", |n| {
        format!("fn main() -> {}Int{} {{ 1 }}", "(".repeat(n), ")".repeat(n))
    }),
    ("generic arguments", |n| {
        format!(
            "fn main() -> {}Int{} {{ 1 }}\n\ntype Alias<T> = T;",
            "Alias<".repeat(n),
            ">".repeat(n)
        )
    }),
    ("modules", |n| {
        format!(
            "fn main() -> Int {{ 1 }}\n\n{}fn f() -> Int {{ 1 }}{}",
            "mod m { ".repeat(n),
            " }".repeat(n)
        )
    }),
];

/// Compiles the source, then generates code for `main` if there weren't any errors.
fn compile(source: &str) -> (Vec<Diagnostic>, bool) {
    let mut ctx = Compiler::new(CompilerOptions {
        std: false,
        ..CompilerOptions::default()
    });

    let kind = SourceKind::File("main.rue".to_string());

    let tree = FileTree::File(File::new(
        &mut ctx,
        "main".to_string(),
        Source::new(Arc::from(source), kind.clone()),
    ));

    tree.compile(&mut ctx);

    let has_errors = ctx.has_errors();

    if !has_errors {
        tree.main(&mut ctx, &mut Allocator::new(), &kind, PathBuf::from("."))
            .unwrap()
            .expect("main exists");
    }

    (ctx.take_diagnostics(), has_errors)
}

/// The limit leaves room for a 2 MiB thread, such as the language server's, in a release build.
/// Frames are much larger in debug builds, which need a bigger stack for the same depth.
fn on_small_stack(f: impl FnOnce() + Send + 'static) {
    let stack_size = if cfg!(debug_assertions) { 32 } else { 2 } * 1024 * 1024;

    thread::Builder::new()
        .stack_size(stack_size)
        .spawn(f)
        .unwrap()
        .join()
        .unwrap();
}

fn is_too_deep(diagnostic: &Diagnostic) -> bool {
    matches!(diagnostic.kind, DiagnosticKind::NestingTooDeep(_))
}

#[test]
fn nesting_within_the_limit_compiles() {
    on_small_stack(|| {
        for (name, shape) in SHAPES {
            let (diagnostics, has_errors) = compile(&shape(MAX_NESTING_DEPTH - 8));
            assert!(!has_errors, "{name}: {diagnostics:?}");
        }
    });
}

#[test]
fn nesting_beyond_the_limit_is_an_error() {
    on_small_stack(|| {
        for (name, shape) in SHAPES {
            let (diagnostics, has_errors) = compile(&shape(10_000));

            assert!(has_errors, "{name}");
            assert!(
                diagnostics.iter().any(is_too_deep),
                "{name}: {diagnostics:?}"
            );
        }
    });
}
