use clvmr::{Allocator, NodePtr};
use id_arena::Arena;
use rue_ast::{AstDocument, AstNode};
use rue_diagnostic::{Diagnostic, DiagnosticSeverity};
use rue_hir::{DependencyGraph, Environment, Scope, lower_symbol};
use rue_lexer::Lexer;
use rue_lir::{Error, codegen, optimize};
use rue_parser::Parser;

use crate::{Compiler, compile_document, declare_document};

#[derive(Debug, Clone)]
pub struct Compilation {
    pub diagnostics: Vec<Diagnostic>,
    pub program: Option<NodePtr>,
}

pub fn compile_file(allocator: &mut Allocator, file: &str) -> Result<Compilation, Error> {
    let tokens = Lexer::new(file).collect::<Vec<_>>();
    let parser = Parser::new(file, tokens);
    let parse_result = parser.parse();

    let mut compilation = Compilation {
        diagnostics: parse_result.diagnostics,
        program: None,
    };

    let Some(ast) = AstDocument::cast(parse_result.node) else {
        return Ok(compilation);
    };

    let mut ctx = Compiler::new();

    let scope = ctx.alloc_scope(Scope::new());
    let declarations = declare_document(&mut ctx, scope, &ast);
    compile_document(&mut ctx, scope, &ast, declarations);

    compilation.diagnostics.extend_from_slice(ctx.diagnostics());

    if compilation
        .diagnostics
        .iter()
        .any(|d| d.kind.severity() == DiagnosticSeverity::Error)
    {
        return Ok(compilation);
    }

    let symbol = ctx.scope(scope).symbol("main").unwrap();
    let graph = DependencyGraph::build(&ctx, symbol);

    let mut arena = Arena::new();
    let lir = lower_symbol(&ctx, &mut arena, &graph, &Environment::default(), symbol);
    let lir = optimize(&mut arena, lir);
    let ptr = codegen(&arena, allocator, lir)?;

    compilation.program = Some(ptr);

    Ok(compilation)
}
