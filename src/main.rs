use std::fs;

use anyhow::Result;
use rue::{
    AstDocument, AstNode, Context, Lexer, Parser, Scope, compile_document, declare_document,
    document,
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

    println!("\n{:#?}", result.node);

    let ast = AstDocument::cast(result.node).unwrap();

    let mut ctx = Context::new();

    let scope = ctx.alloc_scope(Scope::new());
    let declarations = declare_document(&mut ctx, scope, &ast);
    compile_document(&mut ctx, scope, &ast, declarations);

    println!("{:#?}", ctx.scope(scope));

    Ok(())
}
