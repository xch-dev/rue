use clvmr::{run_program, serde::node_to_bytes, Allocator, ChiaDialect, NodePtr};
use rue_compiler::compile;
use rue_parser::{parse, AstNode};

fn main() {
    let source = include_str!("../hello.rue");
    let (ast, errors) = parse(source);

    if !errors.is_empty() {
        for error in errors {
            eprintln!("{}", error);
        }
        return;
    }

    eprintln!("{:#?}", ast.syntax());

    let mut allocator = Allocator::new();
    let output = compile(&mut allocator, ast);

    eprintln!("{:#?}", &output.errors);
    if !output.errors.is_empty() {
        return;
    }

    if let Some(node_ptr) = output.node_ptr {
        let bytes = node_to_bytes(&allocator, node_ptr).unwrap();
        println!("Serialize: {}", hex::encode(bytes));
        match run_program(
            &mut allocator,
            &ChiaDialect::new(0),
            node_ptr,
            NodePtr::NIL,
            0,
        ) {
            Ok(output) => println!(
                "Eval: {}",
                hex::encode(node_to_bytes(&allocator, output.1).unwrap())
            ),
            Err(error) => eprintln!("Error: {:?}", error),
        }
    }
}
