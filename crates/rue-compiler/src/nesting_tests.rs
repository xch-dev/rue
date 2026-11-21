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
    // A binding that's used once is lowered where it's referenced, inside of the rest of the block
    ("single-use bindings", |n| {
        format!(
            "fn main(a: Int) -> Int {{\nlet x0 = a + 1;\n{}x{n}\n}}",
            links(n, |i| format!("let x{} = x{i};\n", i + 1))
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

/// The parser's limit leaves room for a 2 MiB thread, such as the language server's, in a release
/// build. Frames are much larger in debug builds, which need a bigger stack for the same depth.
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

/// Lowering grows the stack as it goes, so the chains run on a 2 MiB stack even in a debug build,
/// where each level of lowering uses enough stack that the limit alone wouldn't prevent an overflow.
#[test]
fn chains_within_the_lowering_limit_compile() {
    on_stack(2, || {
        for (name, shape) in CHAINS {
            let (diagnostics, result) = compile(&shape(100));
            assert!(result.is_ok(), "{name}: {result:?} {diagnostics:?}");
        }
    });
}

#[test]
fn chains_beyond_the_lowering_limit_are_an_error() {
    on_stack(2, || {
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
    on_stack(2, || {
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
/// The chain is longer than the lowering limit allows, so they're run without generating code.
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

/// A long chain of calls compiles all the way through code generation. Each function in the chain
/// calls the next one twice, so they aren't inlined into each other. Each one is lowered on its own,
/// so the generated code doesn't nest once per link. The dependency graph
/// and the check for unresolved code still follow the whole chain from `main`, as they do in
/// `long_chains_of_constants_are_followed`.
///
/// When those two passes recursed, this overflowed a 2 MiB stack with 400 links in a debug build
/// and 1,000 in a release build, though not with 200 and 500. Generating code for the chain takes
/// time that grows with the cube of its length (#267), so it's no longer than it needs to be to
/// catch that.
#[test]
fn long_chains_of_calls_compile() {
    const LINKS: usize = if cfg!(debug_assertions) {
        LONG_CHAIN / 2
    } else {
        LONG_CHAIN
    };

    on_stack(2, || {
        let source = format!(
            "fn main(a: Int) -> Int {{ f0(a) }}\n{}fn f{LINKS}(a: Int) -> Int {{ a }}",
            links(LINKS, |i| format!(
                "fn f{i}(a: Int) -> Int {{ f{}(a) + f{}(a + 1) }}\n",
                i + 1,
                i + 1
            ))
        );

        let (diagnostics, result) = compile(&source);
        assert!(result.is_ok(), "{result:?} {diagnostics:?}");
    });
}

/// Reading a binding that's only a path where it's referenced nests deeper than binding it does,
/// so measuring whether the program is smaller that way can go past the lowering limit when the
/// program itself doesn't. That way of lowering it isn't kept, and nothing is decided from it for
/// the bindings inside of it, rather than failing to compile.
#[test]
fn path_bindings_that_nest_too_deeply_are_bound() {
    on_stack(2, || {
        let pair = (0..8).fold("Int".to_string(), |ty, _| format!("({ty}, Int)"));
        let first = ".first".repeat(8);

        for (asserts, inner) in [(505, false), (499, true), (495, true)] {
            let (binding, body) = if inner {
                (
                    "assert n > 0;\nlet i = wide.i;\n",
                    "v * v + i * i + i * i + i * i + i * i + i * i + i * i",
                )
            } else {
                ("", "v * v")
            };

            let source = format!(
                "struct Wide {{ _a: Int, _b: Int, _c: Int, _d: Int, _e: Int, _f: Int, _g: Int, \
                 _h: Int, i: Int }}\n\nfn main(p: {pair}, wide: Wide, n: Int) -> Int {{\n\
                 let v = p{first};\n{binding}{}{body}\n}}",
                "assert n > 0;\n".repeat(asserts)
            );

            let (diagnostics, result) = compile(&source);
            assert!(
                result.is_ok(),
                "{asserts} {inner}: {result:?} {diagnostics:?}"
            );
        }
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

/// Whether an inline function's argument can be lowered inside of a closure, rather than captured,
/// is checked by following the whole chain of constants it references, which goes further than the
/// lowering limit allows the code itself to nest.
#[test]
fn long_chains_of_inline_arguments_are_checked() {
    const LINKS: usize = LONG_CHAIN * 10;

    on_stack(2, || {
        let source = format!(
            "fn main() -> Int {{ g(C0)() }}\n\ninline fn g(value: Int) -> fn() -> Int {{ fn() => value }}\n{}const C{LINKS}: Int = 1;",
            links(LINKS, |i| format!("const C{i}: Int = C{};\n", i + 1))
        );

        let (diagnostics, result) = compile(&source);
        assert!(errors(&diagnostics).is_empty(), "{diagnostics:?}");
        assert!(
            matches!(result, Err(Error::LoweringTooDeep(_))),
            "{result:?}"
        );
    });
}

/// The bindings of consecutive `let` statements are grouped so that each one is bound after the ones
/// it depends on. Each binding here is used twice, so it isn't inlined, and depends on the one
/// before it, so there's a group for each of them, which is slow unless grouping avoids redoing
/// work for each group. Each one also captures the parameter, as in the reproduction from #296.
#[test]
fn long_runs_of_bindings_are_grouped() {
    on_stack(2, || {
        let source = format!(
            "fn main(a: Int) -> Int {{\nlet x0 = a + 1;\n{}x{LONG_CHAIN}\n}}",
            links(LONG_CHAIN, |i| format!(
                "let x{} = x{i} * x{i} + a;\n",
                i + 1
            ))
        );

        let (diagnostics, result) = compile(&source);
        assert!(result.is_ok(), "{result:?} {diagnostics:?}");
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
/// these run on a 2 MiB stack in every build. Code isn't generated, since some of these are deeper
/// than the lowering limit allows.
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

/// The parameters of `main` are a list, with a level of its environment for each one. The
/// environment is cloned for the bindings in its body, and searched for the path to each symbol it
/// references. The search would overflow a 2 MiB stack in a debug build with this many parameters if
/// it recursed once per level. Cloning and dropping use less stack for each level, so they're tested
/// with deeper environments in `rue-hir`.
#[test]
fn many_parameters_are_lowered() {
    const PARAMETERS: usize = 10_000;

    on_stack(2, || {
        let source = format!(
            "fn main({}) -> Int {{\nlet x = a{} + 1;\nx * x\n}}",
            links(PARAMETERS, |i| format!("a{i}: Int, ")),
            PARAMETERS - 1
        );

        let (diagnostics, result) = compile(&source);
        assert!(result.is_ok(), "{result:?} {diagnostics:?}");
    });
}

/// The parameters of other functions are a tree, which is split so that the references on either
/// side are balanced. Parameters that aren't referenced don't change the balance, so they're split
/// off one at a time, and the tree is as deep as there are unreferenced parameters. It's built, and
/// the arguments are lowered into it at each call, without recursing once per level.
#[test]
fn many_unreferenced_parameters_are_lowered() {
    const PARAMETERS: usize = 10_000;

    on_stack(2, || {
        let arguments = (0..PARAMETERS)
            .map(|i| i.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        // The function is called twice, so that it isn't inlined
        let source = format!(
            "fn main() -> Int {{ f({arguments}) + f({arguments}) }}\n\nfn f({}) -> Int {{ a{} }}",
            links(PARAMETERS, |i| format!("a{i}: Int, ")),
            PARAMETERS - 1
        );

        let (diagnostics, result) = compile(&source);
        assert!(result.is_ok(), "{result:?} {diagnostics:?}");
    });
}

/// The error for code that's nested too deeply points to the first thing lowered past the limit,
/// which has to be the same on every run (#325). Each call's arguments used to be lowered in the
/// order of a `HashMap`, which is random for each map, so the error pointed to either argument of
/// the call that went past the limit. Each compilation here makes new maps, so it would very
/// likely differ between them if the order still depended on one.
#[test]
fn lowering_too_deep_reports_the_same_location_every_time() {
    let source = format!(
        "fn add(a: Int, b: Int) -> Int {{\n    a + b\n}}\n\nfn main(n: Int) -> Int {{\n{}    n\n}}\n",
        "    assert add(n, n) > 0;\n".repeat(MAX_LOWERING_DEPTH + 8)
    );

    on_stack(2, move || {
        for _ in 0..20 {
            let (diagnostics, result) = compile(&source);
            assert_eq!(errors(&diagnostics).len(), 0, "{diagnostics:?}");

            let Err(Error::LoweringTooDeep(message)) = result else {
                panic!("{result:?}");
            };

            // The first argument of the call on line 514 is lowered first
            assert_eq!(
                message,
                format!(
                    "Generated code is nested too deeply, since the limit is {MAX_LOWERING_DEPTH} \
                     levels at main.rue:514:16"
                )
            );
        }
    });
}
