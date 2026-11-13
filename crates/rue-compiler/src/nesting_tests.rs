//! Tests that deeply nested programs don't overflow the stack during compilation, since the parser
//! limits how deeply they can be nested, and lowering limits how deeply the code it generates can
//! be nested.

use std::{path::PathBuf, sync::Arc, thread};

use clvmr::Allocator;
use rue_diagnostic::{Diagnostic, DiagnosticKind, DiagnosticSeverity, Source, SourceKind};
use rue_hir::MAX_LOWERING_DEPTH;
use rue_options::CompilerOptions;
use rue_parser::MAX_NESTING_DEPTH;

use crate::{Compiler, Error, File, FileTree};

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
    // Each call is two levels deep: one for the call wrapping the callee, and one for its argument
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

/// Flat programs that generate code which nests once for each link in a chain, since each link is
/// inlined into the previous one, or nests the rest of the block inside of it.
const CHAINS: &[(&str, Shape)] = &[
    ("functions", |n| {
        format!(
            "fn main() -> Int {{ f0() }}\n{}fn f{n}() -> Int {{ 1 }}",
            links(n, |i| format!("fn f{i}() -> Int {{ f{}() + 1 }}\n", i + 1))
        )
    }),
    ("inline functions", |n| {
        format!(
            "fn main() -> Int {{ f0() }}\n{}inline fn f{n}() -> Int {{ 1 }}",
            links(n, |i| {
                format!("inline fn f{i}() -> Int {{ f{}() + 1 }}\n", i + 1)
            })
        )
    }),
    ("constants", |n| {
        format!(
            "fn main(a: Int) -> Int {{ C0 + a }}\n{}const C{n}: Int = 1;",
            links(n, |i| format!("const C{i}: Int = C{} + 1;\n", i + 1))
        )
    }),
    ("inline constants", |n| {
        format!(
            "fn main(a: Int) -> Int {{ C0 + a }}\n{}inline const C{n}: Int = 1;",
            links(n, |i| format!("inline const C{i}: Int = C{} + 1;\n", i + 1))
        )
    }),
    ("statements", |n| {
        format!(
            "fn main(a: Int) -> Int {{\n{}a\n}}",
            links(n, |i| format!("if a == {i} {{ return {i}; }}\n"))
        )
    }),
    // Checks whether the functions can raise, since both branches of an `inline if` are evaluated
    ("calls in an inline if", |n| {
        format!(
            "fn main(a: Int) -> Int {{ inline if a == 0 {{ f0() }} else {{ 1 }} }}\n{}fn f{n}() -> Int {{ raise; }}",
            links(n, |i| format!("fn f{i}() -> Int {{ f{}() + 1 }}\n", i + 1))
        )
    }),
    // The inline function's body is expanded at each call, inside of its argument, so this nests
    // much more deeply than the source code does
    ("nested inline calls", |n| {
        format!(
            "fn main(a: Int) -> Int {{ {}a{} }}\n\ninline fn f(value: Int) -> Int {{ value{} }}",
            "f(".repeat(n / 10),
            ")".repeat(n / 10),
            " + 1".repeat(10)
        )
    }),
];

/// Generates a link of a chain for each index up to `n`.
fn links(n: usize, link: impl Fn(usize) -> String) -> String {
    (0..n).map(link).collect()
}

/// Compiles the source, then generates code for `main`, which fails if there were any errors.
fn compile(source: &str) -> (Vec<Diagnostic>, Result<(), Error>) {
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

    let result = tree
        .main(&mut ctx, &mut Allocator::new(), &kind, PathBuf::from("."))
        .map(|main| {
            main.expect("main exists");
        });

    (ctx.take_diagnostics(), result)
}

fn errors(diagnostics: &[Diagnostic]) -> Vec<&Diagnostic> {
    diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.severity == DiagnosticSeverity::Error)
        .collect()
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
            let (diagnostics, result) = compile(&shape(MAX_NESTING_DEPTH - 8));
            assert!(result.is_ok(), "{name}: {result:?} {diagnostics:?}");
        }
    });
}

#[test]
fn nesting_beyond_the_limit_is_an_error() {
    on_small_stack(|| {
        for (name, shape) in SHAPES {
            let (diagnostics, result) = compile(&shape(10_000));

            assert!(
                matches!(result, Err(Error::CompilationFailed)),
                "{name}: {result:?}"
            );
            assert!(
                diagnostics.iter().any(is_too_deep),
                "{name}: {diagnostics:?}"
            );
        }
    });
}

#[test]
fn chains_within_the_lowering_limit_compile() {
    on_small_stack(|| {
        for (name, shape) in CHAINS {
            let (diagnostics, result) = compile(&shape(100));
            assert!(result.is_ok(), "{name}: {result:?} {diagnostics:?}");
        }
    });
}

#[test]
fn chains_beyond_the_lowering_limit_are_an_error() {
    on_small_stack(|| {
        for (name, shape) in CHAINS {
            let (diagnostics, result) = compile(&shape(1_000));

            assert_eq!(errors(&diagnostics).len(), 0, "{name}: {diagnostics:?}");

            let Err(Error::LoweringTooDeep(message)) = result else {
                panic!("{name}: {result:?}");
            };

            assert!(
                message.starts_with(&format!(
                    "Generated code is nested too deeply, since the limit is {MAX_LOWERING_DEPTH} \
                     levels at main.rue:"
                )),
                "{name}: {message}"
            );
        }
    });
}

#[test]
fn chains_beyond_the_lowering_limit_in_const_expressions_are_an_error() {
    on_small_stack(|| {
        let (_, shape) = CHAINS[0];
        let source = shape(1_000).replacen("{ f0() }", "{ const { f0() } }", 1);
        let (diagnostics, result) = compile(&source);

        assert!(
            matches!(result, Err(Error::CompilationFailed)),
            "{result:?}"
        );

        let errors = errors(&diagnostics);
        assert_eq!(errors.len(), 1, "{diagnostics:?}");
        assert!(matches!(
            errors[0].kind,
            DiagnosticKind::LoweringTooDeep(MAX_LOWERING_DEPTH)
        ));
    });
}

#[test]
fn long_chains_of_const_expressions_are_evaluated() {
    on_small_stack(|| {
        let source = format!(
            "fn main() -> Int {{ C0 }}\n{}const C1000: Int = 1;",
            links(1_000, |i| {
                format!("const C{i}: Int = const {{ C{} + 1 }};\n", i + 1)
            })
        );

        let (diagnostics, result) = compile(&source);
        assert!(result.is_ok(), "{result:?} {diagnostics:?}");
    });
}
