use clvmr::{run_program, serde::node_to_bytes, Allocator, ChiaDialect, NodePtr};
use rue_compiler::compile;
use rue_parser::{parse, AstNode};

fn main() {
    let source = include_str!("../hello.rue");
    let (ast, errors) = parse(source);

    if !errors.is_empty() {
        eprintln!("{:#?}", ast.syntax());
        for error in errors {
            eprintln!("{}", error);
        }
        return;
    }

    let mut allocator = Allocator::new();
    let output = compile(&mut allocator, ast);

    if !output.errors().is_empty() {
        for error in output.errors() {
            eprintln!("{}", error);
        }
        return;
    }

    let bytes = node_to_bytes(&allocator, output.node_ptr()).unwrap();
    println!("Serialized program: {}", hex::encode(bytes));
    match run_program(
        &mut allocator,
        &ChiaDialect::new(0),
        output.node_ptr(),
        NodePtr::NIL,
        0,
    ) {
        Ok(output) => println!(
            "Serialized output with nil solution: {}",
            hex::encode(node_to_bytes(&allocator, output.1).unwrap())
        ),
        Err(error) => eprintln!("Error: {:?}", error),
    }
}
