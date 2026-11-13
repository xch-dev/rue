//! Tests for the nesting limit, which prevents deeply nested input from overflowing the stack,
//! both in the parser and in the later stages of compilation that process the syntax tree.

use std::{path::Path, sync::Arc, thread};

use rowan::WalkEvent;
use rue_diagnostic::{DiagnosticKind, Source, SourceKind};
use rue_lexer::Lexer;

use crate::{MAX_NESTING_DEPTH, ParseResult, Parser, SyntaxNode};

/// Far deeper than the limit, and enough to overflow the stack without it.
const DEEP: usize = 10_000;

/// Generates source code with the given number of levels of nesting.
type Shape = fn(usize) -> String;

/// Constructs that nest.
const SHAPES: &[(&str, Shape)] = &[
    ("parentheses", |n| {
        format!("fn main() -> Int {{ {}1{} }}", "(".repeat(n), ")".repeat(n))
    }),
    ("pairs", |n| {
        format!(
            "fn main() -> Int {{ {}nil{} }}",
            "(1, ".repeat(n),
            ")".repeat(n)
        )
    }),
    ("lists", |n| {
        format!("fn main() -> Int {{ {}1{} }}", "[".repeat(n), "]".repeat(n))
    }),
    ("blocks", |n| {
        format!("fn main() -> Int {{ {}1{} }}", "{".repeat(n), "}".repeat(n))
    }),
    ("prefix operators", |n| {
        format!("fn main() -> Int {{ {}1 }}", "-".repeat(n))
    }),
    ("binary operators", |n| {
        format!("fn main() -> Int {{ 1{} }}", " + 1".repeat(n))
    }),
    ("calls", |n| {
        format!("fn main() -> Int {{ f{} }}", "()".repeat(n))
    }),
    ("field accesses", |n| {
        format!("fn main() -> Int {{ x{} }}", ".a".repeat(n))
    }),
    ("casts", |n| {
        format!("fn main() -> Int {{ 1{} }}", " as Int".repeat(n))
    }),
    ("lambdas", |n| {
        format!("fn main() -> Int {{ {}1 }}", "fn() => ".repeat(n))
    }),
    ("if conditions", |n| {
        format!(
            "fn main() -> Int {{ {}true{} }}",
            "if ".repeat(n),
            " { 1 } else { 2 }".repeat(n)
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
            "fn main() -> {}Int{} {{ 1 }}",
            "A<".repeat(n),
            ">".repeat(n)
        )
    }),
    ("lambda types", |n| {
        format!("fn main() -> {}Int {{ 1 }}", "fn() -> ".repeat(n))
    }),
    ("bindings", |n| {
        format!(
            "fn main({}a{}: Int) -> Int {{ 1 }}",
            "[".repeat(n),
            "]".repeat(n)
        )
    }),
    ("modules", |n| {
        format!(
            "{}fn main() -> Int {{ 1 }}{}",
            "mod m { ".repeat(n),
            " }".repeat(n)
        )
    }),
    ("imports", |n| {
        format!("import a{}b{};", "::{a".repeat(n), "}".repeat(n))
    }),
];

fn parse(source: &str) -> ParseResult {
    let source = Source::new(Arc::from(source), SourceKind::File("main.rue".to_string()));
    let tokens = Lexer::new(&source.text).collect::<Vec<_>>();
    Parser::new(source, tokens).parse()
}

/// The number of nodes on the longest path from the root to a leaf, measured without recursion.
fn height(node: &SyntaxNode) -> usize {
    let mut depth = 0;
    let mut max = 0;

    for event in node.preorder() {
        match event {
            WalkEvent::Enter(_) => {
                depth += 1;
                max = max.max(depth);
            }
            WalkEvent::Leave(_) => depth -= 1,
        }
    }

    max
}

fn messages(result: &ParseResult) -> Vec<String> {
    result
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.message(Path::new(".")))
        .collect()
}

/// Runs the test on a thread with a smaller stack than the main thread, like the language
/// server's, so that a regression overflows rather than only using a lot of stack.
fn on_small_stack(f: impl FnOnce() + Send + 'static) {
    thread::Builder::new()
        .stack_size(2 * 1024 * 1024)
        .spawn(f)
        .unwrap()
        .join()
        .unwrap();
}

#[test]
fn nesting_within_the_limit_is_parsed() {
    on_small_stack(|| {
        for (name, shape) in SHAPES {
            let result = parse(&shape(MAX_NESTING_DEPTH - 8));

            assert!(
                !result
                    .diagnostics
                    .iter()
                    .any(|diagnostic| matches!(diagnostic.kind, DiagnosticKind::NestingTooDeep(_))),
                "{name}: {:?}",
                messages(&result)
            );
        }
    });
}

#[test]
fn nesting_beyond_the_limit_is_reported_once() {
    on_small_stack(|| {
        for (name, shape) in SHAPES {
            let source = shape(DEEP);
            let result = parse(&source);

            assert_eq!(
                result.node.text().to_string(),
                source,
                "{name}: tree isn't lossless"
            );

            let too_deep = result
                .diagnostics
                .iter()
                .filter(|diagnostic| matches!(diagnostic.kind, DiagnosticKind::NestingTooDeep(_)))
                .count();

            assert_eq!(too_deep, 1, "{name}: {:?}", messages(&result));

            // Each level of nesting can be a few nodes in the tree (such as a list item inside of
            // a list), but it's still bounded
            let height = height(&result.node);
            assert!(
                height <= MAX_NESTING_DEPTH * 3,
                "{name}: the tree is {height} nodes high"
            );
        }
    });
}

#[test]
fn nesting_limit_is_reported_while_recovering() {
    // The missing operand is reported first, and the rest of the file would be skipped silently
    // if the nesting error was suppressed by the recovery from it
    let source = format!(
        "fn main() -> Int {{ 1 + ; {}1{} }}",
        "(".repeat(DEEP),
        ")".repeat(DEEP)
    );

    on_small_stack(move || {
        let result = parse(&source);

        assert_eq!(
            messages(&result),
            [
                "Expected expression, found `;` at main.rue:1:24",
                "Nested too deeply, since the limit is 256 levels at main.rue:1:281",
            ]
        );
    });
}

#[test]
fn rest_of_the_file_is_skipped() {
    let source = format!(
        "fn main() -> Int {{ {}1{} }}\n\nfn other() -> Int {{ 1 + }}",
        "(".repeat(DEEP),
        ")".repeat(DEEP)
    );

    let result = parse(&source);

    assert_eq!(
        messages(&result),
        ["Nested too deeply, since the limit is 256 levels at main.rue:1:275"]
    );
}
