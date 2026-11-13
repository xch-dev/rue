//! Tests that deeply nested programs don't overflow the stack during compilation, since the parser
//! limits how deeply they can be nested, and lowering limits how deeply the code it generates can
//! be nested.

use std::{path::PathBuf, sync::Arc, thread};

use clvmr::Allocator;
use rue_diagnostic::{Diagnostic, DiagnosticKind, DiagnosticSeverity, Lint, Source, SourceKind};
use rue_hir::{DependencyGraph, MAX_LOWERING_DEPTH, has_reachable_unresolved};
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

/// Compiles the source, without generating code.
fn compile_tree(source: &str) -> (Compiler, FileTree, SourceKind) {
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

    (ctx, tree, kind)
}

/// Compiles the source, then generates code for `main`, which fails if there were any errors.
fn compile(source: &str) -> (Vec<Diagnostic>, Result<(), Error>) {
    let (mut ctx, tree, kind) = compile_tree(source);

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

/// The limits leave room for a 2 MiB thread, such as the language server's, in a release build.
/// Frames are much larger in debug builds, which need a bigger stack for the same depth.
fn on_small_stack(f: impl FnOnce() + Send + 'static) {
    on_stack(if cfg!(debug_assertions) { 32 } else { 2 }, f);
}

fn on_stack(mebibytes: usize, f: impl FnOnce() + Send + 'static) {
    thread::Builder::new()
        .stack_size(mebibytes * 1024 * 1024)
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

/// How many links the chains of references have. The passes that follow them use the same amount
/// of stack however long they are, so they run on a 2 MiB stack even in a debug build, and
/// recursing once per link would overflow it.
const LONG_CHAIN: usize = 1_000;

#[test]
fn long_chains_of_const_expressions_are_evaluated() {
    on_stack(2, || {
        let source = format!(
            "fn main() -> Int {{ C0 }}\n{}const C{LONG_CHAIN}: Int = 1;",
            links(LONG_CHAIN, |i| {
                format!("const C{i}: Int = const {{ C{} + 1 }};\n", i + 1)
            })
        );

        let (diagnostics, result) = compile(&source);
        assert!(result.is_ok(), "{result:?} {diagnostics:?}");
    });
}

/// The dependency graph and the check for unresolved code follow the whole chain of constants.
/// Lowering it would go deeper than this stack has room for in a debug build, so they're run
/// without generating code.
#[test]
fn long_chains_of_constants_are_followed() {
    on_stack(2, || {
        let source = format!(
            "fn main(a: Int) -> Int {{ C0 + a }}\n{}const C{LONG_CHAIN}: Int = 1;",
            links(LONG_CHAIN, |i| format!(
                "const C{i}: Int = C{} + 1;\n",
                i + 1
            ))
        );

        let (mut ctx, tree, kind) = compile_tree(&source);
        let diagnostics = ctx.take_diagnostics();
        assert!(errors(&diagnostics).is_empty(), "{diagnostics:?}");

        let scope = ctx.module(tree.find(&kind).unwrap().module).scope;
        let main = ctx.scope(scope).symbol("main").unwrap();

        let graph = DependencyGraph::build(&ctx, main, *ctx.options());
        assert_eq!(graph.dependencies(main, false).len(), LONG_CHAIN + 2);

        assert!(!has_reachable_unresolved(&ctx, main));
    });
}

/// Whether a function can raise is checked through the whole chain of calls.
#[test]
fn long_chains_of_calls_are_followed() {
    on_stack(2, || {
        let source = format!(
            "fn main(a: Int) -> Int {{ inline if a == 0 {{ f0() }} else {{ 1 }} }}\n{}fn f{LONG_CHAIN}() -> Int {{ raise; }}",
            links(LONG_CHAIN, |i| format!(
                "fn f{i}() -> Int {{ f{}() + 1 }}\n",
                i + 1
            ))
        );

        let (mut ctx, ..) = compile_tree(&source);
        let diagnostics = ctx.take_diagnostics();

        assert!(
            diagnostics
                .iter()
                .any(|diagnostic| diagnostic.lint() == Some(Lint::InlineIfMayRaise)),
            "{diagnostics:?}"
        );
    });
}

/// The search for unconditional recursion follows the whole chain of bindings. It used little stack
/// for each binding when it recursed, so the chain is longer than the others.
#[test]
fn long_chains_of_bindings_are_followed() {
    const LINKS: usize = LONG_CHAIN * 10;

    on_stack(2, || {
        let source = format!(
            "fn main() -> Int {{ f(1) }}\n\nfn f(a: Int) -> Int {{\nlet x0 = f(a);\n{}x{LINKS}\n}}",
            links(LINKS, |i| format!("let x{} = x{i};\n", i + 1))
        );

        let (mut ctx, ..) = compile_tree(&source);
        let diagnostics = ctx.take_diagnostics();

        assert!(
            diagnostics
                .iter()
                .any(|diagnostic| diagnostic.lint() == Some(Lint::UnconditionalRecursion)),
            "{diagnostics:?}"
        );
    });
}

/// Programs whose types nest once per item or link, even though the source doesn't nest, along with
/// how many items or links to test them with. Each one type checks without errors. The sizes are
/// enough to overflow a 2 MiB stack in a debug build if the operations on types didn't grow the
/// stack, or if the lints that walk types recursed, but no bigger, since some of them are slow.
const DEEP_TYPES: &[(&str, usize, Shape)] = &[
    // A list literal's type is a chain of pairs, with one pair for each item. This one is long
    // enough for the lints that walk types to overflow too, if they recursed
    ("list literal", 5_000, |n| {
        format!(
            "fn main() -> Any {{\nlet x = [{}];\nx\n}}",
            links(n, |i| format!("{i}, "))
        )
    }),
    ("list literal assigned to a list type", 2_000, |n| {
        format!(
            "fn main() -> Any {{\nlet x: List<Int> = [{}];\nx\n}}",
            links(n, |i| format!("{i}, "))
        )
    }),
    ("list literal returned as a list type", 2_000, |n| {
        format!(
            "fn main() -> List<Int> {{ [{}] }}",
            links(n, |i| format!("{i}, "))
        )
    }),
    // Each call's inferred return type wraps the previous one
    ("inferred types", 400, |n| {
        format!(
            "fn main() -> Any {{\nlet x0 = 1;\n{}x{n}\n}}\n\nfn wrap<T>(value: T) -> (T, nil) {{ (value, nil) }}",
            links(n, |i| format!("let x{} = wrap(x{i});\n", i + 1))
        )
    }),
    ("type aliases", 1_000, |n| {
        format!(
            "fn main(value: T0) -> Any {{ value }}\n{}type T{n} = Int;",
            links(n, |i| format!("type T{i} = (T{}, nil);\n", i + 1))
        )
    }),
    // The lint for unused exports walks the types that are used outside of the module
    ("exported type aliases", 1_000, |n| {
        format!(
            "fn main(value: m::T0) -> Any {{ value }}\n\nmod m {{\n{}export type T{n} = Int;\n}}",
            links(n, |i| format!("export type T{i} = (T{}, nil);\n", i + 1))
        )
    }),
    // Narrowing the type in the else branch subtracts `nil` from each of the union's variants
    ("union aliases narrowed by a guard", 1_000, |n| {
        format!(
            "fn main(value: T0 | nil) -> Any {{ if value is nil {{ 1 }} else {{ value }} }}\n{}type T{n} = Bytes;",
            links(n, |i| format!("type T{i} = Int | T{};\n", i + 1))
        )
    }),
    // Narrowing a field walks through each alias in the union to get to the field
    ("union aliases narrowed by a guard on a field", 1_000, |n| {
        format!(
            "fn main(value: T0) -> Any {{ if value.first is 1 {{ value }} else {{ 2 }} }}\n{}type T{n} = (Int, nil);",
            links(n, |i| format!("type T{i} = (Int, nil) | T{};\n", i + 1))
        )
    }),
    // Each application of the alias nests its whole body, so this compares two types that are 100
    // levels deep for each level of nesting in the source
    ("nested generic aliases", 3_000, |n| {
        let depth = n / 100;
        format!(
            "fn main(value: {}Int{}) -> {}Any{} {{ value }}\n\ntype W<T> = {}T{};",
            "W<".repeat(depth),
            ">".repeat(depth),
            "W<".repeat(depth),
            ">".repeat(depth),
            "(".repeat(100),
            ", nil)".repeat(100)
        )
    }),
];

/// The operations on types recurse once per level of a type, and grow the stack as they go, so
/// these run on a 2 MiB stack in every build. Code isn't generated, since lowering a long list
/// uses more stack than this in a debug build (#114).
#[test]
fn deep_types_are_checked() {
    on_stack(2, || {
        for &(name, size, shape) in DEEP_TYPES {
            let (mut ctx, ..) = compile_tree(&shape(size));
            let diagnostics = ctx.take_diagnostics();
            assert!(errors(&diagnostics).is_empty(), "{name}: {diagnostics:?}");
        }
    });
}

/// Checking a value against a deep type at runtime is still limited, but it's reported rather than
/// overflowing the stack while the type is substituted.
#[test]
fn deep_type_checks_are_an_error() {
    on_stack(2, || {
        let source = format!(
            "fn main(value: Any) -> Bool {{ value is T0 }}\n{}type T{LONG_CHAIN} = nil;",
            links(LONG_CHAIN, |i| format!("type T{i} = (Int, T{});\n", i + 1))
        );

        let (mut ctx, ..) = compile_tree(&source);
        let diagnostics = ctx.take_diagnostics();
        let errors = errors(&diagnostics);

        assert_eq!(errors.len(), 1, "{diagnostics:?}");
        assert!(matches!(
            errors[0].kind,
            DiagnosticKind::TypeCheckDepthExceeded
        ));
    });
}
