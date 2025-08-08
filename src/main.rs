use std::fs;

use anyhow::Result;
use clvm_tools_rs::classic::clvm_tools::binutils::disassemble;
use clvmr::{Allocator, ChiaDialect, NodePtr, run_program};
use rue::{
    AstDocument, AstNode, Context, Lexer, Parser, Scope, codegen, compile_document,
    declare_document, document, lower_reference,
};

fn main() -> Result<()> {
    let source = fs::read_to_string("main.rue")?;
    let tokens = Lexer::new(&source).collect::<Vec<_>>();
    let mut parser = Parser::new(&source, tokens);
    document(&mut parser);
    let result = parser.build();

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
    let mir = lower_reference(&mut ctx, symbol);

    let mut allocator = Allocator::new();
    let ptr = codegen(&mut ctx, &mut allocator, mir);

    println!("Program: {}", disassemble(&allocator, ptr, None));

    let output = run_program(
        &mut allocator,
        &ChiaDialect::new(0),
        ptr,
        NodePtr::NIL,
        u64::MAX,
    )
    .unwrap()
    .1;

    println!("Output: {}", disassemble(&allocator, output, None));

    Ok(())
}
