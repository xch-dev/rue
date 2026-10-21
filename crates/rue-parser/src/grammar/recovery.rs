//! Tests for how the parser recovers from syntax errors. The goal is for each mistake to result in
//! a single diagnostic at the right place, with the rest of the file still parsed normally, rather
//! than a cascade of errors for everything that comes after it.

use std::{fmt::Write, fs, path::Path, sync::Arc};

use expect_test::{Expect, expect};
use rowan::NodeOrToken;
use rue_diagnostic::{Source, SourceKind};
use rue_lexer::Lexer;

use crate::{ParseResult, Parser, SyntaxKind, SyntaxNode};

fn parse(source: &str) -> ParseResult {
    let source = Source::new(Arc::from(source), SourceKind::File("main.rue".to_string()));
    let tokens = Lexer::new(&source.text).collect::<Vec<_>>();
    Parser::new(source, tokens).parse()
}

/// Checks the structure of the syntax tree and the errors for a document. The tree only includes
/// nodes and error tokens, since that's what matters for recovery.
#[allow(clippy::needless_pass_by_value)]
fn check(source: &str, tree: Expect, errors: Expect) {
    let result = parse(source);

    assert_eq!(
        result.node.text().to_string(),
        source,
        "tree isn't lossless"
    );

    let mut output = String::new();
    write_tree(&mut output, &result.node, 0);

    let messages = result
        .diagnostics
        .iter()
        .map(|error| error.message(Path::new(".")))
        .collect::<Vec<_>>()
        .join("\n");

    tree.assert_eq(&output);
    errors.assert_eq(&messages);
}

fn write_tree(output: &mut String, node: &SyntaxNode, depth: usize) {
    writeln!(output, "{}{:?}", "  ".repeat(depth), node.kind()).unwrap();

    for child in node.children_with_tokens() {
        match child {
            NodeOrToken::Node(node) => write_tree(output, &node, depth + 1),
            NodeOrToken::Token(token) => {
                if token.kind() == SyntaxKind::Error {
                    writeln!(output, "{}Error {:?}", "  ".repeat(depth + 1), token.text()).unwrap();
                }
            }
        }
    }
}

/// Every Rue file in the repository, which are all expected to be free of syntax errors.
fn corpus() -> Vec<(String, String)> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let mut files = Vec::new();
    collect(&root.join("tests"), &mut files);
    collect(&root.join("examples"), &mut files);
    files.sort();
    assert!(!files.is_empty(), "no Rue files found");
    files
}

fn collect(dir: &Path, files: &mut Vec<(String, String)>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };

    for entry in entries {
        let path = entry.unwrap().path();

        if path.is_dir() {
            collect(&path, files);
        } else if path.extension().is_some_and(|ext| ext == "rue") {
            files.push((
                path.display().to_string(),
                fs::read_to_string(&path).unwrap(),
            ));
        }
    }
}

/// The spans of every non-trivia token in the source.
fn token_spans(source: &str) -> Vec<(usize, usize)> {
    Lexer::new(source)
        .filter(|token| {
            !matches!(
                token.kind,
                rue_lexer::TokenKind::Whitespace
                    | rue_lexer::TokenKind::LineComment
                    | rue_lexer::TokenKind::BlockComment { .. }
            )
        })
        .map(|token| (token.span.start, token.span.end))
        .collect()
}

#[test]
fn test_corpus_parses_without_errors() {
    for (name, source) in corpus() {
        let result = parse(&source);
        let errors = result
            .diagnostics
            .iter()
            .map(|error| error.message(Path::new(".")))
            .collect::<Vec<_>>();
        assert!(errors.is_empty(), "{name} has syntax errors: {errors:#?}");
        assert_eq!(
            result.node.text().to_string(),
            source,
            "{name} isn't lossless"
        );
    }
}

/// Removes each token from each file in turn (along with truncating the file after each token),
/// and checks that the parser terminates, keeps every character of the source in the syntax tree,
/// and reports a small number of errors rather than one for every token after the mistake.
#[test]
fn test_corpus_mutations_recover() {
    const MAX_ERRORS: usize = 5;

    let mut worst = (0, String::new());

    for (name, source) in corpus() {
        let spans = token_spans(&source);

        // Keep the test fast by only mutating a sample of the tokens in large files
        let step = (spans.len() / 25).max(1);

        for &(start, end) in spans.iter().step_by(step) {
            for mutated in [
                format!("{}{}", &source[..start], &source[end..]),
                source[..end].to_string(),
            ] {
                let result = parse(&mutated);

                assert_eq!(
                    result.node.text().to_string(),
                    mutated,
                    "{name} isn't lossless after mutation"
                );

                let count = result.diagnostics.len();
                if count > worst.0 {
                    worst = (count, format!("{name}:\n{mutated}"));
                }
            }
        }
    }

    assert!(
        worst.0 <= MAX_ERRORS,
        "{} errors for a single mutation of {}",
        worst.0,
        worst.1
    );
}

#[test]
fn test_skipped_tokens_are_error_tokens() {
    let result = parse("fn main() { let x = => 1; }");

    let errors = result
        .node
        .descendants_with_tokens()
        .filter(|element| element.kind() == SyntaxKind::Error)
        .map(|element| element.to_string())
        .collect::<Vec<_>>();

    assert_eq!(errors, ["=", ">", "1"]);
}

#[test]
fn test_empty_document() {
    check(
        "",
        expect![[r#"
        Document
    "#]],
        expect![""],
    );
    check(
        "// just a comment\n",
        expect![[r#"
        Document
    "#]],
        expect![""],
    );
}

#[test]
fn test_missing_semicolon_before_statement_on_next_line() {
    check(
        "fn main() {\n    let x = 1\n    let y = 2;\n    y\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    LiteralExpr
                  LetStmt
                    NamedBinding
                    LiteralExpr
                  PathExpr
                    PathSegment
        "#]],
        expect!["Expected `;`, found `let` at main.rue:3:5"],
    );

    check(
        "fn main() {\n    foo()\n    bar();\n    42\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  FunctionCallExpr
                    PathExpr
                      PathSegment
                  ExprStmt
                    FunctionCallExpr
                      PathExpr
                        PathSegment
                  LiteralExpr
        "#]],
        expect!["Expected one of `;`, `}`, found identifier at main.rue:3:5"],
    );

    check(
        "fn main() {\n    return 1\n}\n\nfn other() {}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  ReturnStmt
                    LiteralExpr
              FunctionItem
                Block
        "#]],
        expect!["Expected `;`, found `}` at main.rue:3:1"],
    );
}

#[test]
fn test_garbage_after_expression_on_same_line() {
    check(
        "fn main() {\n    foo() bar baz;\n    42\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  FunctionCallExpr
                    PathExpr
                      PathSegment
                  Error "bar"
                  Error "baz"
                  Error ";"
                  LiteralExpr
        "#]],
        expect!["Expected one of `;`, `}`, found identifier at main.rue:2:11"],
    );
}

#[test]
fn test_missing_expression() {
    check(
        "fn main() {\n    let x = ;\n    x\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                  PathExpr
                    PathSegment
        "#]],
        expect!["Expected expression, found `;` at main.rue:2:13"],
    );

    check(
        "fn main() {\n    let x = 1 + ;\n    x\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    BinaryExpr
                      LiteralExpr
                  PathExpr
                    PathSegment
        "#]],
        expect!["Expected expression, found `;` at main.rue:2:17"],
    );

    check(
        "fn main() {\n    let x = 1 +\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    BinaryExpr
                      LiteralExpr
        "#]],
        expect!["Expected expression, found `}` at main.rue:3:1"],
    );

    check(
        "const VALUE: Int = ;\n\nfn main() {}",
        expect![[r#"
            Document
              ConstantItem
                PathType
                  PathSegment
              FunctionItem
                Block
        "#]],
        expect!["Expected expression, found `;` at main.rue:1:20"],
    );
}

#[test]
fn test_invalid_expression_is_skipped() {
    check(
        "fn main() {\n    let x = => 1;\n    x\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    Error "="
                    Error ">"
                    Error "1"
                  PathExpr
                    PathSegment
        "#]],
        expect!["Expected expression, found `=` at main.rue:2:13"],
    );

    check(
        "fn main() {\n    let x = 1 @ 2;\n    x\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    LiteralExpr
                    Error "@"
                  ExprStmt
                    LiteralExpr
                  PathExpr
                    PathSegment
        "#]],
        expect![[r#"
            Unknown `@` at main.rue:2:15
            Expected `;`, found integer literal at main.rue:2:17"#]],
    );
}

#[test]
fn test_missing_closing_brace_before_next_item() {
    check(
        "fn main() {\n    let x = 1;\n\nfn other() -> Int {\n    42\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    LiteralExpr
              FunctionItem
                PathType
                  PathSegment
                Block
                  LiteralExpr
        "#]],
        expect!["Expected `}`, found `fn` at main.rue:4:1"],
    );

    check(
        "fn main() {\n    let x = 1;\n\nstruct Point { x: Int }",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    LiteralExpr
              StructItem
                StructField
                  PathType
                    PathSegment
        "#]],
        expect!["Expected `}`, found `struct` at main.rue:4:1"],
    );

    check(
        "fn main() {\n    let x = 1;\n\ninline const VALUE: Int = 1;",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    LiteralExpr
              ConstantItem
                PathType
                  PathSegment
                LiteralExpr
        "#]],
        expect!["Expected `}`, found `inline` at main.rue:4:1"],
    );
}

#[test]
fn test_missing_closing_brace_at_eof() {
    check(
        "fn main() {\n    let x = 1;\n",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    LiteralExpr
        "#]],
        expect!["Expected `}`, found eof at main.rue:3:1"],
    );
}

#[test]
fn test_lambda_and_const_block_are_not_items() {
    check(
        "fn main() {\n    let f = fn(x: Int) => x;\n    const { 1 }\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    LambdaExpr
                      FunctionParameter
                        NamedBinding
                        PathType
                          PathSegment
                      PathExpr
                        PathSegment
                  ConstExpr
                    Block
                      LiteralExpr
        "#]],
        expect![""],
    );
}

#[test]
fn test_stray_closing_delimiters_in_block() {
    check(
        "fn main() {\n    foo());\n    let x = 1;\n    x\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  FunctionCallExpr
                    PathExpr
                      PathSegment
                  Error ")"
                  Error ";"
                  LetStmt
                    NamedBinding
                    LiteralExpr
                  PathExpr
                    PathSegment
        "#]],
        expect!["Expected one of `;`, `}`, found `)` at main.rue:2:10"],
    );

    check(
        "fn main() {\n    ];\n    42\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  Error "]"
                  Error ";"
                  LiteralExpr
        "#]],
        expect!["Expected one of `}`, statement, found `]` at main.rue:2:5"],
    );
}

#[test]
fn test_stray_else() {
    check(
        "fn main() -> Int {\n    if true {\n        1\n     else {\n        2\n    }\n}\n\nfn other() {}",
        expect![[r#"
            Document
              FunctionItem
                PathType
                  PathSegment
                Block
                  IfStmt
                    LiteralExpr
                    Block
                      LiteralExpr
                      Error "else"
                      Error "{"
                      Error "2"
                      Error "}"
              FunctionItem
                Block
        "#]],
        expect![[r#"
            Expected one of `;`, `}`, found `else` at main.rue:4:6
            Expected one of `else`, `}`, found `fn` at main.rue:9:1"#]],
    );
}

#[test]
fn test_stray_closing_brace_at_top_level() {
    check(
        "fn main() {}\n}\n}\n\nfn other() {}",
        expect![[r#"
            Document
              FunctionItem
                Block
              Error "}"
              Error "}"
              FunctionItem
                Block
        "#]],
        expect!["Expected item, found `}` at main.rue:2:1"],
    );
}

#[test]
fn test_statements_at_top_level() {
    check(
        "let x = 1;\nlet y = { x + 1 };\n\nfn main() {}",
        expect![[r#"
            Document
              Error "let"
              Error "x"
              Error "="
              Error "1"
              Error ";"
              Error "let"
              Error "y"
              Error "="
              Error "{"
              Error "x"
              Error "+"
              Error "1"
              Error "}"
              Error ";"
              FunctionItem
                Block
        "#]],
        expect!["Expected item, found `let` at main.rue:1:1"],
    );

    check(
        "pub fn main() {}",
        expect![[r#"
            Document
              Error "pub"
              FunctionItem
                Block
        "#]],
        expect!["Expected item, found identifier at main.rue:1:1"],
    );
}

#[test]
fn test_missing_closing_paren_in_call() {
    check(
        "fn main() {\n    foo(1, 2\n    let x = 1;\n    x\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  FunctionCallExpr
                    PathExpr
                      PathSegment
                    ListItem
                      LiteralExpr
                    ListItem
                      LiteralExpr
                  LetStmt
                    NamedBinding
                    LiteralExpr
                  PathExpr
                    PathSegment
        "#]],
        expect!["Expected one of `,`, `)`, found `let` at main.rue:3:5"],
    );

    check(
        "fn main() {\n    foo(\n}\n\nfn other() {}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  FunctionCallExpr
                    PathExpr
                      PathSegment
                    ListItem
              FunctionItem
                Block
        "#]],
        expect!["Expected one of `)`, `...`, expression, found `}` at main.rue:3:1"],
    );
}

#[test]
fn test_missing_comma_in_list() {
    check(
        "fn main() {\n    foo(1 2, 3);\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  ExprStmt
                    FunctionCallExpr
                      PathExpr
                        PathSegment
                      ListItem
                        LiteralExpr
                      Error "2"
                      ListItem
                        LiteralExpr
        "#]],
        expect!["Expected one of `,`, `)`, found integer literal at main.rue:2:11"],
    );

    check(
        "fn main() {\n    let list = [1, 2 3, 4 5];\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    ListExpr
                      ListItem
                        LiteralExpr
                      ListItem
                        LiteralExpr
                      Error "3"
                      ListItem
                        LiteralExpr
                      Error "5"
        "#]],
        expect!["Expected one of `,`, `]`, found integer literal at main.rue:2:22"],
    );

    check(
        "fn main() {\n    let point = Point { x: 1 y: 2 };\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    StructInitializerExpr
                      PathExpr
                        PathSegment
                      StructInitializerField
                        LiteralExpr
                      Error "y"
                      Error ":"
                      Error "2"
        "#]],
        expect!["Expected one of `,`, `}`, found identifier at main.rue:2:30"],
    );
}

#[test]
fn test_missing_list_element() {
    check(
        "fn main() {\n    foo(, 1);\n    [1, , 2];\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  ExprStmt
                    FunctionCallExpr
                      PathExpr
                        PathSegment
                      ListItem
                      ListItem
                        LiteralExpr
                  ExprStmt
                    ListExpr
                      ListItem
                        LiteralExpr
                      ListItem
                      ListItem
                        LiteralExpr
        "#]],
        expect![[r#"
            Expected one of `)`, `...`, expression, found `,` at main.rue:2:9
            Expected one of `]`, `...`, expression, found `,` at main.rue:3:9"#]],
    );
}

#[test]
fn test_missing_closing_bracket_in_list() {
    check(
        "fn main() {\n    let list = [1, 2;\n    list\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    ListExpr
                      ListItem
                        LiteralExpr
                      ListItem
                        LiteralExpr
                  PathExpr
                    PathSegment
        "#]],
        expect!["Expected one of `,`, `]`, found `;` at main.rue:2:21"],
    );
}

#[test]
fn test_pair_and_group_recovery() {
    check(
        "fn main() {\n    let pair = (1 2);\n    let group = (1 2 3);\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    GroupExpr
                      LiteralExpr
                      Error "2"
                  LetStmt
                    NamedBinding
                    GroupExpr
                      LiteralExpr
                      Error "2"
                      Error "3"
        "#]],
        expect![[r#"
            Expected one of `,`, `)`, found integer literal at main.rue:2:19
            Expected one of `,`, `)`, found integer literal at main.rue:3:20"#]],
    );

    check(
        "fn main() {\n    let (a b) = pair;\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    PairBinding
                      NamedBinding
                      Error "b"
                    PathExpr
                      PathSegment
        "#]],
        expect!["Expected `,`, found identifier at main.rue:2:12"],
    );

    check(
        "type Pair = (Int Int);\nfn main() {}",
        expect![[r#"
            Document
              TypeAliasItem
                GroupType
                  PathType
                    PathSegment
                  Error "Int"
              FunctionItem
                Block
        "#]],
        expect!["Expected one of `,`, `)`, found identifier at main.rue:1:18"],
    );
}

#[test]
fn test_missing_field_name() {
    check(
        "fn main() {\n    let x = value.;\n    let y = value..other;\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    FieldAccessExpr
                      PathExpr
                        PathSegment
                  LetStmt
                    NamedBinding
                    FieldAccessExpr
                      PathExpr
                        PathSegment
                  ExprStmt
                    Error "."
                    Error "other"
        "#]],
        expect![[r#"
            Expected identifier, found `;` at main.rue:2:19
            Expected identifier, found `.` at main.rue:3:19"#]],
    );
}

#[test]
fn test_function_parameter_recovery() {
    check(
        "fn main( {}\n\nfn other() {}",
        expect![[r#"
            Document
              FunctionItem
                Block
              FunctionItem
                Block
        "#]],
        expect!["Expected one of `)`, `,`, found `{` at main.rue:1:10"],
    );

    check(
        "fn main(a: Int -> Int {\n    a\n}",
        expect![[r#"
            Document
              FunctionItem
                FunctionParameter
                  NamedBinding
                  PathType
                    PathSegment
                PathType
                  PathSegment
                Block
                  PathExpr
                    PathSegment
        "#]],
        expect!["Expected one of `,`, `)`, found `-` at main.rue:1:16"],
    );

    check(
        "fn main(a, b: Int) {}",
        expect![[r#"
            Document
              FunctionItem
                FunctionParameter
                  NamedBinding
                FunctionParameter
                  NamedBinding
                  PathType
                    PathSegment
                Block
        "#]],
        expect!["Expected `:`, found `,` at main.rue:1:10"],
    );

    check(
        "fn main(a Int, b: Int) {}",
        expect![[r#"
            Document
              FunctionItem
                FunctionParameter
                  NamedBinding
                  PathType
                    PathSegment
                FunctionParameter
                  NamedBinding
                  PathType
                    PathSegment
                Block
        "#]],
        expect!["Expected `:`, found identifier at main.rue:1:11"],
    );

    check(
        "fn main({ x, y }: Point) {}",
        expect![[r#"
            Document
              FunctionItem
                FunctionParameter
                  StructBinding
                    StructFieldBinding
                    StructFieldBinding
                  PathType
                    PathSegment
                Block
        "#]],
        expect![""],
    );
}

#[test]
fn test_function_signature_recovery() {
    check(
        "fn (a: Int) {}",
        expect![[r#"
        Document
          FunctionItem
            FunctionParameter
              NamedBinding
              PathType
                PathSegment
            Block
    "#]],
        expect!["Expected identifier, found `(` at main.rue:1:4"],
    );

    check(
        "fn main {}",
        expect![[r#"
        Document
          FunctionItem
            Block
    "#]],
        expect!["Expected one of `<`, `(`, found `{` at main.rue:1:9"],
    );

    check(
        "fn main() -> {}",
        expect![[r#"
        Document
          FunctionItem
            Block
    "#]],
        expect!["Expected type, found `{` at main.rue:1:14"],
    );

    check(
        "fn main()\n\nfn other() {}",
        expect![[r#"
            Document
              FunctionItem
              FunctionItem
                Block
        "#]],
        expect!["Missing function body at main.rue:3:1"],
    );
}

#[test]
fn test_generic_recovery() {
    check(
        "fn main<T(value: T) -> T {\n    value\n}",
        expect![[r#"
            Document
              FunctionItem
                GenericParameters
                FunctionParameter
                  NamedBinding
                  PathType
                    PathSegment
                PathType
                  PathSegment
                Block
                  PathExpr
                    PathSegment
        "#]],
        expect!["Expected one of `,`, `>`, found `(` at main.rue:1:10"],
    );

    check(
        "fn main() -> List<Int {\n    nil\n}",
        expect![[r#"
            Document
              FunctionItem
                PathType
                  PathSegment
                    GenericArguments
                      PathType
                        PathSegment
                Block
                  LiteralExpr
        "#]],
        expect!["Expected one of `,`, `>`, found `{` at main.rue:1:23"],
    );

    check(
        "type Alias<A B> = A;",
        expect![[r#"
            Document
              TypeAliasItem
                GenericParameters
                  Error "B"
                PathType
                  PathSegment
        "#]],
        expect!["Expected one of `,`, `>`, found identifier at main.rue:1:14"],
    );
}

#[test]
fn test_type_recovery() {
    check(
        "const VALUE: = 42;\nfn main() {}",
        expect![[r#"
            Document
              ConstantItem
                LiteralExpr
              FunctionItem
                Block
        "#]],
        expect!["Expected type, found `=` at main.rue:1:14"],
    );

    check(
        "fn main() {\n    let x: = 42;\n    let y: Int | = 42;\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    LiteralExpr
                  LetStmt
                    NamedBinding
                    UnionType
                      PathType
                        PathSegment
                    LiteralExpr
        "#]],
        expect![[r#"
            Expected type, found `=` at main.rue:2:12
            Expected type, found `=` at main.rue:3:18"#]],
    );

    check(
        "type Callback = fn Int -> Int;\nfn main() {}",
        expect![[r#"
            Document
              TypeAliasItem
                LambdaType
                  PathType
                    PathSegment
              Error "-"
              Error ">"
              Error "Int"
              Error ";"
              FunctionItem
                Block
        "#]],
        expect![[r#"
            Expected one of `<`, `(`, found identifier at main.rue:1:20
            Expected `;`, found `-` at main.rue:1:24"#]],
    );
}

#[test]
fn test_binding_recovery() {
    check(
        "fn main() {\n    let = 42;\n    let 5 = 42;\n    let [a b] = list;\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    LiteralExpr
                  LetStmt
                    Error "5"
                    LiteralExpr
                  LetStmt
                    ListBinding
                      ListBindingItem
                        NamedBinding
                      Error "b"
                    PathExpr
                      PathSegment
        "#]],
        expect![[r#"
            Expected binding, found `=` at main.rue:2:9
            Expected binding, found integer literal at main.rue:3:9
            Expected one of `,`, `]`, found identifier at main.rue:4:12"#]],
    );
}

#[test]
fn test_struct_recovery() {
    check(
        "struct Point {\n    x: Int\n    y: Int,\n}\n\nfn main() {}",
        expect![[r#"
            Document
              StructItem
                StructField
                  PathType
                    PathSegment
                Error "y"
                Error ":"
                Error "Int"
              FunctionItem
                Block
        "#]],
        expect!["Expected one of `=`, `,`, `}`, found identifier at main.rue:3:5"],
    );

    check(
        "struct Point\n\nfn main() {}",
        expect![[r#"
            Document
              StructItem
              FunctionItem
                Block
        "#]],
        expect!["Expected one of `<`, `{`, found `fn` at main.rue:3:1"],
    );

    check(
        "struct Point {\n    x: Int,\n\nfn main() {}",
        expect![[r#"
            Document
              StructItem
                StructField
                  PathType
                    PathSegment
                StructField
              FunctionItem
                Block
        "#]],
        expect!["Expected one of `}`, `...`, identifier, found `fn` at main.rue:4:1"],
    );
}

#[test]
fn test_struct_initializer_missing_open_brace() {
    check(
        "fn main() {\n    let point = Point x: 1, y: 2 };\n    point\n}\n\nfn other() {}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    PathExpr
                      PathSegment
                  PathExpr
                    PathSegment
                  Error ":"
                  Error "1"
                  Error ","
                  Error "y"
                  Error ":"
                  Error "2"
              Error ";"
              Error "point"
              Error "}"
              FunctionItem
                Block
        "#]],
        expect![[r#"
            Expected `;`, found identifier at main.rue:2:23
            Expected one of `;`, `}`, found `:` at main.rue:2:24
            Expected item, found `;` at main.rue:2:35"#]],
    );
}

#[test]
fn test_if_recovery() {
    check(
        "fn main() -> Int {\n    if true\n        1\n    } else {\n        2\n    }\n}\n\nfn other() {}",
        expect![[r#"
            Document
              FunctionItem
                PathType
                  PathSegment
                Block
                  IfStmt
                    LiteralExpr
                    Block
                  LiteralExpr
              Error "else"
              Error "{"
              Error "2"
              Error "}"
              Error "}"
              FunctionItem
                Block
        "#]],
        expect![[r#"
            Expected `{`, found integer literal at main.rue:3:9
            Expected item, found `else` at main.rue:4:7"#]],
    );

    check(
        "fn main() -> Int {\n    let x = if true { 1 };\n    x\n}",
        expect![[r#"
            Document
              FunctionItem
                PathType
                  PathSegment
                Block
                  LetStmt
                    NamedBinding
                    IfExpr
                      LiteralExpr
                      Block
                        LiteralExpr
                      Block
                  PathExpr
                    PathSegment
        "#]],
        expect!["Expected `else`, found `;` at main.rue:2:26"],
    );

    check(
        "fn main() -> Int {\n    if { 1 } else { 2 }\n}",
        expect![[r#"
            Document
              FunctionItem
                PathType
                  PathSegment
                Block
                  IfExpr
                    Block
                      LiteralExpr
                    Block
                    Block
                      LiteralExpr
        "#]],
        expect!["Expected `{`, found `else` at main.rue:2:14"],
    );
}

#[test]
fn test_import_recovery() {
    check(
        "import foo::bar\n\nfn main() {}",
        expect![[r#"
            Document
              ImportItem
                ImportPath
                  ImportPathSegment
                  ImportPathSegment
              FunctionItem
                Block
        "#]],
        expect!["Expected one of `::`, `;`, found `fn` at main.rue:3:1"],
    );

    check(
        "import foo::;\nimport foo::{a b};\nfn main() {}",
        expect![[r#"
            Document
              ImportItem
                ImportPath
                  ImportPathSegment
                  ImportPathSegment
              ImportItem
                ImportPath
                  ImportPathSegment
                  ImportPathSegment
                    ImportPath
                      ImportPathSegment
                    Error "b"
              FunctionItem
                Block
        "#]],
        expect![[r#"
            Expected one of `*`, `{`, `super`, identifier, found `;` at main.rue:1:13
            Expected one of `::`, `,`, `}`, found identifier at main.rue:2:16"#]],
    );
}

#[test]
fn test_module_recovery() {
    check(
        "mod inner {\n    fn main() {\n}\n\nfn other() {}",
        expect![[r#"
            Document
              ModuleItem
                FunctionItem
                  Block
                FunctionItem
                  Block
        "#]],
        expect!["Expected `}`, found eof at main.rue:5:14"],
    );

    check(
        "mod inner {\n    let x = 1;\n}\n\nfn other() {}",
        expect![[r#"
            Document
              ModuleItem
                Error "let"
                Error "x"
                Error "="
                Error "1"
                Error ";"
              FunctionItem
                Block
        "#]],
        expect!["Expected item, found `let` at main.rue:2:5"],
    );

    check(
        "mod inner\n\nfn other() {}",
        expect![[r#"
            Document
              ModuleItem
              FunctionItem
                Block
        "#]],
        expect!["Expected `{`, found `fn` at main.rue:3:1"],
    );
}

#[test]
fn test_lexer_errors() {
    check(
        "fn main() {\n    let x = \"hello;\n}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    LiteralExpr
        "#]],
        expect!["Unterminated string literal at main.rue:2:13"],
    );

    check(
        "fn main() {\n    let x = 1 /* unterminated",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
                    LiteralExpr
        "#]],
        expect!["Unterminated block comment at main.rue:2:15"],
    );
}

#[test]
fn test_multiple_independent_errors() {
    check(
        "fn a() {\n    let x = ;\n}\n\nfn b() {\n    foo(1 2);\n}\n\nfn c(x Int) {}\n\nfn d() {}",
        expect![[r#"
            Document
              FunctionItem
                Block
                  LetStmt
                    NamedBinding
              FunctionItem
                Block
                  ExprStmt
                    FunctionCallExpr
                      PathExpr
                        PathSegment
                      ListItem
                        LiteralExpr
                      Error "2"
              FunctionItem
                FunctionParameter
                  NamedBinding
                  PathType
                    PathSegment
                Block
              FunctionItem
                Block
        "#]],
        expect![[r#"
            Expected expression, found `;` at main.rue:2:13
            Expected one of `,`, `)`, found integer literal at main.rue:6:11
            Expected `:`, found identifier at main.rue:9:8"#]],
    );
}
