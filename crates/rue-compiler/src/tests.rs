use std::{collections::HashSet, fmt, fs};

use clvm_traits::{FromClvm, ToClvm};
use clvm_utils::tree_hash;
use clvmr::{run_program, serde::node_to_bytes, Allocator, ChiaDialect, NodePtr};
use rue_parser::ParserErrorKind;

use crate::{compile, DiagnosticInfo};

macro_rules! example {
    ( $name:ident, $example:expr ) => {
        #[test]
        fn $name() {
            example(
                include_str!(concat!("../../../examples/", stringify!($name), ".rue")),
                $example,
            );
        }
    };
}

example!(
    hello_world,
    Example {
        environment: (),
        parse: &[],
        compile: &[],
        hash: "7cacd67079cbf0a1cd915e9456ebb09fca6d375a63744e4e0e442b8f00759d24",
        size_bytes: 24,
        output: "Hello, world!".to_string(),
        runtime_cost: 175
    }
);

example!(
    factorial,
    Example {
        environment: (),
        parse: &[],
        compile: &[],
        hash: "894f61248f98988924f255b780e8b67a840fd41d5b911e4f292d1fa9f9c804e0",
        size_bytes: 113,
        output: 1307674368000u64,
        runtime_cost: 46068
    }
);

example!(
    let_bindings,
    Example {
        environment: [42, 34],
        parse: &[],
        compile: &[],
        hash: "aa247d144d1c5a3554c3d689c09fb2c0aced58a628e52030eac2bbd219018289",
        size_bytes: 81,
        output: 324,
        runtime_cost: 5087
    }
);

example!(
    nested_scopes,
    Example {
        environment: [42],
        parse: &[],
        compile: &[],
        hash: "f62477bea5e9e0499265972002bf8205e064467c75a652d95302d0aacbbc6f25",
        size_bytes: 421,
        output: 1258649915136u64,
        runtime_cost: 20043
    }
);

example!(
    fibonacci,
    Example {
        environment: (),
        parse: &[],
        compile: &[],
        hash: "0ee1d01c128a1d1ff6997404f800126172ec803c1b48fe79cbdcdf5eb3fa2c46",
        size_bytes: 137,
        output: 55,
        runtime_cost: 441764
    }
);

macro_rules! example_list {
    ( $( $name:ident ),+ $(,)? ) => {{
        let mut examples = HashSet::new();
        $( examples.insert(stringify!($name).to_string()); $name; )+
        examples
    }};
}

#[allow(path_statements)]
#[test]
fn example_list() {
    let examples = example_list!(
        hello_world,
        factorial,
        let_bindings,
        nested_scopes,
        fibonacci
    );
    let mut found_examples = HashSet::new();

    for example in fs::read_dir("../../examples")
        .unwrap()
        .map(|file| file.unwrap().file_name().to_str().unwrap().to_string())
        .filter(|path| path.ends_with(".rue"))
    {
        found_examples.insert(example[..example.len() - 4].to_string());
    }

    assert_eq!(examples, found_examples);
}

struct Example<'a, T, O> {
    environment: T,
    parse: &'a [ParserErrorKind],
    compile: &'a [DiagnosticInfo],
    hash: &'a str,
    size_bytes: usize,
    output: O,
    runtime_cost: u64,
}

#[allow(clippy::too_many_arguments)]
fn example<T, O>(
    source: &str,
    Example {
        environment,
        parse: expected_parser_errors,
        compile: expected_compiler_errors,
        hash: expected_hash,
        size_bytes,
        output: expected_output,
        runtime_cost,
    }: Example<T, O>,
) where
    T: ToClvm<NodePtr>,
    O: FromClvm<NodePtr> + PartialEq + fmt::Debug,
{
    let (root, parser_errors) = rue_parser::parse(source);
    let mut allocator = Allocator::new();
    let output = compile(&mut allocator, root);

    let parser_errors: Vec<ParserErrorKind> = parser_errors
        .into_iter()
        .map(|error| error.kind().clone())
        .collect();
    let compiler_errors: Vec<DiagnosticInfo> = output
        .diagnostics()
        .iter()
        .map(|diagnostic| diagnostic.info().clone())
        .collect();

    assert_eq!(
        parser_errors, expected_parser_errors,
        "parser errors mismatch"
    );
    assert_eq!(
        compiler_errors, expected_compiler_errors,
        "compiler errors mismatch"
    );

    let bytes = node_to_bytes(&allocator, output.node_ptr).unwrap();
    println!("compiled program: {}", hex::encode(&bytes));
    assert_eq!(bytes.len(), size_bytes, "program size mismatch");

    let hash = hex::encode(tree_hash(&allocator, output.node_ptr));
    assert_eq!(hash, expected_hash, "hash mismatch");

    let environment = environment.to_clvm(&mut allocator).unwrap();
    let output = run_program(
        &mut allocator,
        &ChiaDialect::new(0),
        output.node_ptr,
        environment,
        u64::MAX,
    )
    .unwrap();
    assert_eq!(output.0, runtime_cost, "runtime cost mismatch");

    let output = O::from_clvm(&allocator, output.1).unwrap();
    assert_eq!(output, expected_output, "output mismatch");
}
