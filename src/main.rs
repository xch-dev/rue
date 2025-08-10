use std::{env, fs};

use anyhow::Result;
use clvm_tools_rs::classic::clvm_tools::binutils::{assemble, disassemble};
use clvmr::{Allocator, ChiaDialect, run_program};
use id_arena::Arena;
use rue_ast::{AstDocument, AstNode};
use rue_compiler::{Context, compile_document, declare_document};
use rue_lexer::Lexer;
use rue_lir::codegen;
use rue_parser::Parser;

fn main() -> Result<()> {
    let source = fs::read_to_string("main.rue")?;
    let tokens = Lexer::new(&source).collect::<Vec<_>>();
    let parser = Parser::new(&source, tokens);
    let result = parser.parse();

    // println!("{:#?}", result.node);

    for error in result.errors {
        println!("{}", error.message(&source));
    }

    let ast = AstDocument::cast(result.node).unwrap();

    let mut ctx = Context::new();

    let scope = ctx.alloc_scope(Scope::new());
    let declarations = declare_document(&mut ctx, scope, &ast);
    compile_document(&mut ctx, scope, &ast, declarations);

    for error in ctx.errors() {
        println!("{}", error.message(&source));
    }

    let symbol = ctx.scope(scope).symbol("main").unwrap();
    let mut graph = Graph::new();
    graph_symbol(&ctx, &mut graph, symbol);
    let mir = lower_reference(&mut ctx, symbol);

    let mut allocator = Allocator::new();
    let mut arena = Arena::new();
    let lir = optimize(&mut ctx, &mut arena, mir);
    let ptr = codegen(&arena, &mut allocator, lir)?;

    println!("Program: {}", disassemble(&allocator, ptr, None));

    let env = env::args().nth(1).unwrap_or_default();
    let env = assemble(&mut allocator, &env).unwrap();

    println!("Solution: {}", disassemble(&allocator, env, None));

    let output = run_program(&mut allocator, &ChiaDialect::new(0), ptr, env, u64::MAX)
        .unwrap()
        .1;

    println!("Output: {}", disassemble(&allocator, output, None));

    Ok(())
}
