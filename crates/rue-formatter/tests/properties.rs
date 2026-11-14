//! Property tests for the lexer, parser and formatter.
//!
//! These check properties that must hold for every input, rather than the output for particular
//! inputs:
//!
//! - The lexer's tokens cover the input exactly, in order, and the lexer and parser finish without
//!   panicking on any input.
//! - The parser's syntax tree is lossless: its text is exactly the input, even for invalid input.
//! - Formatting source that parses without errors gives source that also parses without errors,
//!   has the same tokens apart from trivia, trailing commas and import order, keeps every comment,
//!   and is unchanged by formatting it again (which `format_source` checks itself, returning an
//!   error otherwise).
//!
//! The inputs come from several generators: arbitrary strings, sequences of Rue tokens, generated
//! expressions and imports, and the real Rue files in the repository with tokens deleted,
//! duplicated, swapped or inserted, or with whitespace and comments added between them.
//!
//! Each property runs proptest's default of 256 cases with a fixed seed, so that the suite stays
//! fast and gives the same result on every run. Set `PROPTEST_CASES` to run more cases, and
//! `PROPTEST_RNG_SEED` to try different ones. Most of the formatter bugs these properties found
//! took a few thousand cases to reach, so the 256 cases don't catch every regression of them; the
//! regression tests in the formatter's unit tests do. Failing cases aren't saved to files; when a
//! property finds a bug, the shrunk input belongs in an ordinary regression test.

use std::{
    fs,
    path::{Path, PathBuf},
    sync::{Arc, LazyLock},
};

use proptest::{
    prelude::*,
    sample::Index,
    test_runner::{Config, RngSeed},
};
use rue_diagnostic::{Source, SourceKind};
use rue_formatter::{FormatError, FormatOptions, format_source};
use rue_lexer::Lexer;
use rue_parser::{ParseResult, Parser, SyntaxKind, SyntaxNode, SyntaxToken, T};

/// The inputs are the same on every run unless `PROPTEST_RNG_SEED` is set, so that a property
/// can't fail on a change that has nothing to do with it.
fn config() -> Config {
    let default = Config::default();
    let rng_seed = if std::env::var_os("PROPTEST_RNG_SEED").is_some() {
        default.rng_seed
    } else {
        RngSeed::Fixed(0)
    };
    Config {
        rng_seed,
        failure_persistence: None,
        ..default
    }
}

/// Every Rue file in the repository: the tests, the examples and the standard library.
static CORPUS: LazyLock<Vec<String>> = LazyLock::new(|| {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let mut paths = Vec::new();
    for dir in ["tests", "examples", "crates/rue-compiler/src/std"] {
        collect_rue_files(&root.join(dir), &mut paths);
    }
    paths.sort();
    assert!(
        !paths.is_empty(),
        "no Rue files found under {}",
        root.display()
    );
    paths
        .iter()
        .map(|path| fs::read_to_string(path).expect("Rue file should be readable"))
        .collect()
});

fn collect_rue_files(dir: &Path, paths: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).expect("corpus directory should be readable") {
        let path = entry.expect("directory entry should be readable").path();
        if path.is_dir() {
            collect_rue_files(&path, paths);
        } else if path.extension().is_some_and(|extension| extension == "rue") {
            paths.push(path);
        }
    }
}

fn parse(text: &str) -> ParseResult {
    let source = Source::new(Arc::from(text), SourceKind::File("<property>".to_string()));
    Parser::new(source, Lexer::new(text).collect()).parse()
}

/// The lexer's tokens are non-empty, in order, cover the input with no gaps, and end on character
/// boundaries.
fn check_lexer(text: &str) {
    let mut end = 0;
    for token in Lexer::new(text) {
        assert_eq!(token.span.start, end, "gap or overlap before {token:?}");
        assert!(token.span.end > token.span.start, "empty token {token:?}");
        assert!(
            text.is_char_boundary(token.span.end),
            "{token:?} doesn't end on a character boundary"
        );
        end = token.span.end;
    }
    assert_eq!(
        end,
        text.len(),
        "the tokens don't reach the end of the input"
    );
}

/// The syntax tree's text is exactly the input, and every diagnostic points into the input.
fn check_parser(text: &str) {
    let result = parse(text);
    assert_eq!(
        result.node.text().to_string(),
        text,
        "the syntax tree isn't lossless"
    );
    for diagnostic in &result.diagnostics {
        let span = &diagnostic.srcloc.span;
        assert!(
            span.start <= span.end
                && span.end <= text.len()
                && text.is_char_boundary(span.start)
                && text.is_char_boundary(span.end),
            "diagnostic {:?} has an invalid span {span:?}",
            diagnostic.kind
        );
    }
}

/// Source that parses without errors formats to equivalent source, which formats to itself. The
/// formatter rejects anything else.
fn check_formatter(text: &str, options: &FormatOptions) {
    let input = parse(text);
    let parses_cleanly = input.diagnostics.is_empty();
    let formatted = match format_source(text, options) {
        Ok(formatted) => formatted,
        Err(FormatError::Parse { diagnostics }) => {
            // The formatter also reports syntax errors in its own output, so show which source
            // they're in
            assert!(
                !parses_cleanly,
                "the formatter reported syntax errors in source the parser accepts: {:?}\n{}",
                diagnostics
                    .iter()
                    .map(|diagnostic| &diagnostic.kind)
                    .collect::<Vec<_>>(),
                diagnostics[0].srcloc.source.text
            );
            return;
        }
        Err(error) => panic!("the formatter failed on source that parses: {error}"),
    };
    assert!(
        parses_cleanly,
        "the formatter accepted source with syntax errors"
    );

    let reparsed = parse(&formatted);
    assert!(
        reparsed.diagnostics.is_empty(),
        "the formatted source has syntax errors: {:?}\n{formatted}",
        reparsed
            .diagnostics
            .iter()
            .map(|diagnostic| &diagnostic.kind)
            .collect::<Vec<_>>()
    );
    assert_eq!(
        Signature::new(&input.node),
        Signature::new(&reparsed.node),
        "formatting changed the program\n{formatted}"
    );
}

/// What formatting must preserve, computed independently of the formatter's own check.
#[derive(Debug, PartialEq, Eq)]
struct Signature {
    /// The full path of every item imported at the top level, sorted, since the formatter sorts
    /// imports and the paths inside them.
    imports: Vec<String>,
    /// The tokens of everything else, in order.
    rest: Vec<String>,
    /// The text of every comment, sorted, since the formatter can move a comment past a token.
    comments: Vec<String>,
}

impl Signature {
    fn new(root: &SyntaxNode) -> Self {
        let mut imports = Vec::new();
        let mut rest = Vec::new();
        for child in root.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Node(node) if node.kind() == SyntaxKind::ImportItem => {
                    // An import's attributes apply to every path in it
                    let (attributes, path): (Vec<_>, Vec<_>) =
                        significant_tokens(&node).into_iter().partition(|token| {
                            token
                                .parent_ancestors()
                                .any(|node| node.kind() == SyntaxKind::Attribute)
                        });
                    let prefix = attributes.iter().map(token_key).collect::<String>();
                    expand_import(&path, &mut 0, &prefix, &mut imports);
                }
                rowan::NodeOrToken::Node(node) => {
                    rest.extend(significant_tokens(&node).iter().map(token_key));
                }
                rowan::NodeOrToken::Token(token) => {
                    if !token.kind().is_trivia() {
                        rest.push(token_key(&token));
                    }
                }
            }
        }
        imports.sort();

        let mut comments = root
            .descendants_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .filter(|token| {
                matches!(
                    token.kind(),
                    SyntaxKind::LineComment | SyntaxKind::BlockComment
                )
            })
            .map(|token| token.text().trim_end().to_string())
            .collect::<Vec<_>>();
        comments.sort();

        Self {
            imports,
            rest,
            comments,
        }
    }
}

/// The non-trivia tokens of a node, leaving out commas just before a closing delimiter, which the
/// formatter adds or removes depending on whether the list is broken across lines.
fn significant_tokens(node: &SyntaxNode) -> Vec<SyntaxToken> {
    let tokens = node
        .descendants_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .filter(|token| !token.kind().is_trivia())
        .collect::<Vec<_>>();
    tokens
        .iter()
        .enumerate()
        .filter(|(index, token)| {
            token.kind() != T![,]
                || !tokens
                    .get(index + 1)
                    .is_some_and(|next| matches!(next.kind(), T![')'] | T![']'] | T!['}'] | T![>]))
        })
        .map(|(_, token)| token.clone())
        .collect()
}

/// Adds the full path of each item that an import (or a path inside an import group) imports, so
/// that `import a::{b, c::{d, e}};` gives `import a::b`, `import a::c::d` and `import a::c::e`.
/// Returns the `,` or `}` that ends the path, if any.
fn expand_import(
    tokens: &[SyntaxToken],
    index: &mut usize,
    prefix: &str,
    paths: &mut Vec<String>,
) -> Option<SyntaxKind> {
    let mut path = prefix.to_string();
    let mut grouped = false;
    while let Some(token) = tokens.get(*index) {
        *index += 1;
        match token.kind() {
            T![,] | T!['}'] => {
                if !grouped {
                    paths.push(path);
                }
                return Some(token.kind());
            }
            T!['{'] => {
                grouped = true;
                while expand_import(tokens, index, &path, paths) == Some(T![,]) {}
            }
            _ => {
                path.push(' ');
                path.push_str(&token_key(token));
            }
        }
    }
    if !grouped {
        paths.push(path);
    }
    None
}

fn token_key(token: &SyntaxToken) -> String {
    format!("{:?} {}", token.kind(), token.text())
}

/// The text of each of the lexer's tokens, which concatenate back to the input.
fn token_texts(text: &str) -> Vec<String> {
    Lexer::new(text)
        .map(|token| text[token.span].to_string())
        .collect()
}

/// Short pieces of Rue source: keywords, punctuation, identifiers, literals (including malformed
/// ones), comments, whitespace and characters that aren't part of Rue.
fn fragment() -> impl Strategy<Value = String> {
    const KEYWORDS: &[&str] = &[
        "nil", "true", "false", "import", "export", "extern", "from", "inline", "test", "mod",
        "fn", "const", "type", "struct", "let", "if", "else", "return", "assert", "raise", "debug",
        "is", "as", "super", "Int", "Bytes", "Bool", "Any", "List",
    ];
    const PUNCTUATION: &[&str] = &[
        "(", ")", "{", "}", "[", "]", "+", "-", "*", "/", "%", "=", "<", ">", "!", "&", "|", "~",
        "^", ".", ",", ":", ";", "#", "==", "!=", "<=", ">=", "&&", "||", "<<", ">>", ">>>", "->",
        "=>", "::", "...", "#!",
    ];
    const TRIVIA: &[&str] = &[
        " ",
        "  ",
        "\n",
        "\n\n",
        "\t",
        "\r\n",
        "// comment\n",
        "//",
        "/* block */",
        "/* outer /* nested */ */",
        "/* unterminated",
        "/*/",
    ];
    const OTHER: &[&str] = &["@", "$", "?", "`", "'", "\\", "\0", "\u{e9}", "\u{1f980}"];

    prop_oneof![
        4 => proptest::sample::select(KEYWORDS).prop_map(str::to_string),
        6 => proptest::sample::select(PUNCTUATION).prop_map(str::to_string),
        4 => "[a-zA-Z_][a-zA-Z0-9_]{0,5}",
        1 => "[0-9][0-9_]{0,4}",
        1 => "0x[0-9a-fA-F_]{0,4}",
        1 => "0b[01_]{0,4}",
        1 => "0o[0-7_]{0,4}",
        1 => "\"[a-z \\\\]{0,5}\"?",
        3 => proptest::sample::select(TRIVIA).prop_map(str::to_string),
        1 => proptest::sample::select(OTHER).prop_map(str::to_string),
    ]
}

/// A sequence of fragments, joined with or without whitespace between them.
fn token_soup() -> impl Strategy<Value = String> {
    prop::collection::vec((fragment(), any::<bool>()), 0..48).prop_map(|fragments| {
        let mut text = String::new();
        for (fragment, space) in fragments {
            text.push_str(&fragment);
            if space {
                text.push(' ');
            }
        }
        text
    })
}

/// Arbitrary strings, and strings made mostly of the characters that make up Rue tokens.
fn arbitrary_text() -> impl Strategy<Value = String> {
    prop_oneof![
        any::<String>(),
        "[a-z0-9_ \n\t(){}\\[\\]+*/%=<>!&|~^.,:;#\"x-]{0,64}",
    ]
}

#[derive(Debug, Clone)]
enum Mutation {
    Delete(Index),
    Duplicate(Index),
    Swap(Index, Index),
    Insert(Index, String),
}

/// A file from the repository with a few tokens deleted, duplicated, swapped or inserted, which
/// usually gives source that is nearly valid.
fn mutated_corpus() -> impl Strategy<Value = String> {
    let mutation = prop_oneof![
        any::<Index>().prop_map(Mutation::Delete),
        any::<Index>().prop_map(Mutation::Duplicate),
        (any::<Index>(), any::<Index>()).prop_map(|(a, b)| Mutation::Swap(a, b)),
        (any::<Index>(), fragment()).prop_map(|(at, fragment)| Mutation::Insert(at, fragment)),
    ];
    (any::<Index>(), prop::collection::vec(mutation, 0..6)).prop_map(|(file, mutations)| {
        let mut tokens = token_texts(file.get(&CORPUS));
        for mutation in mutations {
            if tokens.is_empty() {
                break;
            }
            match mutation {
                Mutation::Delete(at) => {
                    tokens.remove(at.index(tokens.len()));
                }
                Mutation::Duplicate(at) => {
                    let at = at.index(tokens.len());
                    tokens.insert(at, tokens[at].clone());
                }
                Mutation::Swap(a, b) => {
                    let len = tokens.len();
                    tokens.swap(a.index(len), b.index(len));
                }
                Mutation::Insert(at, fragment) => {
                    tokens.insert(at.index(tokens.len() + 1), fragment);
                }
            }
        }
        tokens.concat()
    })
}

/// Inserts whitespace and comments between the tokens of a source file. This keeps the tokens
/// themselves intact, so it gives valid source more often than other mutations, but it can still
/// separate the parts of an operator such as `>=`.
fn with_extra_trivia(text: String) -> impl Strategy<Value = String> {
    const TRIVIA: &[&str] = &[
        " ",
        "\n",
        "\n\n\n",
        "\t",
        " // line comment\n",
        "\n// own-line comment\n",
        " /* block */ ",
        " /* block */ \n",
        "\n/* multi\n   line */\n",
        " //\n",
        "\n// own-line comment\n\n",
        " // trailing\n// own-line\n",
        " /* trailing */\n/* multi\n   line */\n",
    ];
    let insertion = (any::<Index>(), proptest::sample::select(TRIVIA));
    prop::collection::vec(insertion, 0..12).prop_map(move |insertions| {
        let mut tokens = token_texts(&text);
        for (at, trivia) in insertions {
            tokens.insert(at.index(tokens.len() + 1), trivia.to_string());
        }
        tokens.concat()
    })
}

/// A file from the repository, unchanged, which usually parses without errors.
fn corpus_file() -> impl Strategy<Value = String> {
    any::<Index>().prop_map(|file| file.get(&CORPUS).clone())
}

/// A random expression, built from Rue's operators, calls, lists, pairs, `if` and lambdas, which
/// gives the formatter's line breaking a lot to do.
fn expression() -> impl Strategy<Value = String> {
    const BINARY: &[&str] = &[
        "+", "-", "*", "/", "%", "<", ">", "<=", ">=", "==", "!=", "&&", "||", "&", "|", "^", "<<",
        ">>", ">>>",
    ];
    let leaf = prop_oneof![
        "[a-z][a-z_]{0,12}",
        "[0-9]{1,6}",
        Just("nil".to_string()),
        Just("true".to_string()),
        "0x[0-9a-f]{2,8}",
        "\"[a-z ]{0,16}\"",
    ];
    leaf.prop_recursive(5, 48, 4, |inner| {
        prop_oneof![
            (
                inner.clone(),
                proptest::sample::select(BINARY),
                inner.clone()
            )
                .prop_map(|(left, op, right)| format!("{left} {op} {right}")),
            (
                proptest::sample::select(&["!", "-", "~"][..]),
                inner.clone()
            )
                .prop_map(|(op, operand)| format!("{op}{operand}")),
            inner.clone().prop_map(|inner| format!("({inner})")),
            ("[a-z]{1,10}", prop::collection::vec(inner.clone(), 0..4))
                .prop_map(|(name, args)| format!("{name}({})", args.join(", "))),
            prop::collection::vec(inner.clone(), 0..4)
                .prop_map(|items| format!("[{}]", items.join(", "))),
            (inner.clone(), inner.clone()).prop_map(|(first, rest)| format!("({first}, {rest})")),
            (inner.clone(), inner.clone(), inner.clone()).prop_map(|(condition, then, other)| {
                format!("if {condition} {{ {then} }} else {{ {other} }}")
            }),
            (inner.clone(), "[a-z]{1,8}").prop_map(|(value, field)| format!("{value}.{field}")),
            inner.clone().prop_map(|value| format!("{value} as Int")),
            inner.clone().prop_map(|value| format!("{value} is Bytes")),
            inner.prop_map(|body| format!("fn(x: Int) => {body}")),
        ]
    })
}

/// A small program around generated expressions.
fn generated_program() -> impl Strategy<Value = String> {
    (
        prop::collection::vec(import_path(), 0..4),
        expression(),
        expression(),
    )
        .prop_map(|(imports, value, result)| {
            let imports = imports
                .iter()
                .flat_map(|path| ["import ", path, ";\n"])
                .collect::<String>();
            format!("{imports}fn main() -> Int {{\n    let value = {value};\n    {result}\n}}\n")
        })
}

/// A random import path, with nested import groups.
fn import_path() -> impl Strategy<Value = String> {
    "[a-z]{1,6}".prop_recursive(3, 16, 4, |inner| {
        prop_oneof![
            ("[a-z]{1,6}", inner.clone()).prop_map(|(name, rest)| format!("{name}::{rest}")),
            ("[a-z]{1,6}", prop::collection::vec(inner, 1..4))
                .prop_map(|(name, items)| format!("{name}::{{{}}}", items.join(", "))),
        ]
    })
}

/// A few imports with whitespace and comments between their tokens. These are small, so the
/// comments are dense, which reaches the sorting of imports and the comments around them.
fn imports_with_comments() -> impl Strategy<Value = String> {
    prop::collection::vec(import_path(), 1..5)
        .prop_map(|paths| {
            paths
                .iter()
                .flat_map(|path| ["import ", path, ";\n"])
                .chain(["fn main() {}\n"])
                .collect::<String>()
        })
        .prop_flat_map(with_extra_trivia)
}

/// Source for the formatter properties: mostly source that parses without errors, since the
/// formatter rejects anything else.
fn formatter_input() -> impl Strategy<Value = String> {
    prop_oneof![
        2 => corpus_file(),
        2 => generated_program(),
        1 => mutated_corpus(),
    ]
    .prop_flat_map(with_extra_trivia)
}

fn format_options() -> impl Strategy<Value = FormatOptions> {
    prop_oneof![
        3 => Just(FormatOptions::default()),
        1 => (10usize..120, 1usize..8).prop_map(|(max_width, indent_width)| FormatOptions {
            max_width,
            indent_width,
        }),
    ]
}

fn any_input() -> impl Strategy<Value = String> {
    prop_oneof![
        arbitrary_text(),
        token_soup(),
        mutated_corpus(),
        corpus_file().prop_flat_map(with_extra_trivia),
    ]
}

proptest! {
    #![proptest_config(config())]

    #[test]
    fn any_input_satisfies_every_property(text in any_input()) {
        check_lexer(&text);
        check_parser(&text);
        check_formatter(&text, &FormatOptions::default());
    }

    #[test]
    fn formatter_preserves_the_program(text in formatter_input(), options in format_options()) {
        check_formatter(&text, &options);
    }

    #[test]
    fn formatter_preserves_imports_and_their_comments(text in imports_with_comments()) {
        check_formatter(&text, &FormatOptions::default());
    }
}

/// The properties also hold for every file in the repository as it is.
#[test]
fn corpus_files_satisfy_every_property() {
    for text in CORPUS.iter() {
        check_lexer(text);
        check_parser(text);
        check_formatter(text, &FormatOptions::default());
    }
}

/// The signature ignores the order of imports and of the paths in an import group, but not which
/// path each imported item is under.
#[test]
fn signature_compares_full_import_paths() {
    let signature = |text: &str| Signature::new(&parse(text).node);
    assert_eq!(
        signature("import z;\nimport a::{d, b::{c, e}};"),
        signature("import a::{b::{e, c,}, d};\nimport z;"),
    );
    assert_ne!(
        signature("import a::{b::c, d::e};"),
        signature("import a::{b::e, d::c};"),
    );
}
