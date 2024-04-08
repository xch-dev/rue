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
        hash: "291e4594b43d58e833cab95e4b165c5fac6b4d8391c81ebfd20efdd8d58b92d8",
        size_bytes: 97,
        output: 1307674368000u64,
        runtime_cost: 43743
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
        hash: "951ba85ff214a65c4d07814672544b6686e8f4a819550543473fb8eb26aae6a3",
        size_bytes: 121,
        output: 55,
        runtime_cost: 414329
    }
);

example!(
    lambda_functions,
    Example {
        environment: (),
        parse: &[],
        compile: &[],
        hash: "4487281127571db9621d134467f47ec74af9becea41a45fb942610b5df172328",
        size_bytes: 103,
        output: 86,
        runtime_cost: 3247
    }
);

example!(
    struct_type,
    Example {
        environment: (),
        parse: &[],
        compile: &[],
        hash: "0805d86735757b42ff0ad4194d8f356d6a600e8a2cd6494ecb50a8d586ffe1ef",
        size_bytes: 161,
        output: 341120935282u64,
        runtime_cost: 3019
    }
);

example!(
    lists,
    Example {
        environment: (),
        parse: &[],
        compile: &[],
        hash: "0f8516625bd122a47b5ad2bb871034ff13dd175cca88c4a0641f8a4d7ee95d7d",
        size_bytes: 219,
        output: 21,
        runtime_cost: 6266
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
        fibonacci,
        lambda_functions,
        struct_type,
        lists
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

    let hash = hex::encode(tree_hash(&allocator, output.node_ptr));

    let environment = environment.to_clvm(&mut allocator).unwrap();
    let output = run_program(
        &mut allocator,
        &ChiaDialect::new(0),
        output.node_ptr,
        environment,
        u64::MAX,
    )
    .unwrap();

    let deserialized = O::from_clvm(&allocator, output.1).unwrap();

    assert_eq!(deserialized, expected_output, "output mismatch");
    assert_eq!(bytes.len(), size_bytes, "program size mismatch");
    assert_eq!(output.0, runtime_cost, "runtime cost mismatch");
    assert_eq!(hash, expected_hash, "hash mismatch");
}
