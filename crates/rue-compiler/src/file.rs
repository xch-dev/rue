use clvmr::{Allocator, NodePtr};
use id_arena::Arena;
use rue_ast::{AstDocument, AstNode};
use rue_diagnostic::{Diagnostic, DiagnosticSeverity};
use rue_hir::{DependencyGraph, Environment, Scope, ScopeId, lower_symbol};
use rue_lexer::Lexer;
use rue_lir::{Error, codegen, optimize};
use rue_parser::Parser;

use crate::{Compiler, compile_document, declare_document};

#[derive(Debug, Clone)]
pub struct Compilation {
    pub diagnostics: Vec<Diagnostic>,
    pub program: Option<NodePtr>,
}

#[derive(Debug, Clone)]
struct PartialCompilation {
    diagnostics: Vec<Diagnostic>,
    scope: ScopeId,
}

pub fn compile_file(allocator: &mut Allocator, file: &str) -> Result<Compilation, Error> {
    let mut ctx = Compiler::new();
    let std = compile_file_partial(&mut ctx, include_str!("../../../std.rue"));
    ctx.push_scope(std.scope);
    let file = compile_file_partial(&mut ctx, file);

    let mut compilation = Compilation {
        diagnostics: file.diagnostics,
        program: None,
    };

    if compilation
        .diagnostics
        .iter()
        .any(|d| d.kind.severity() == DiagnosticSeverity::Error)
    {
        return Ok(compilation);
    }

    let symbol = ctx.scope(file.scope).symbol("main").unwrap();
    let graph = DependencyGraph::build(&ctx, symbol);

    let mut arena = Arena::new();
    let lir = lower_symbol(
        &ctx,
        &mut arena,
        &graph,
        &Environment::default(),
        symbol,
        true,
    );
    let lir = optimize(&mut arena, lir);
    let ptr = codegen(&arena, allocator, lir)?;

    compilation.program = Some(ptr);

    Ok(compilation)
}

fn compile_file_partial(ctx: &mut Compiler, file: &str) -> PartialCompilation {
    let tokens = Lexer::new(file).collect::<Vec<_>>();
    let parser = Parser::new(file, tokens);
    let parse_result = parser.parse();

    let scope = ctx.alloc_scope(Scope::new());

    let mut compilation = PartialCompilation {
        diagnostics: parse_result.diagnostics,
        scope,
    };

    let Some(ast) = AstDocument::cast(parse_result.node) else {
        return compilation;
    };

    let declarations = declare_document(ctx, scope, &ast);
    compile_document(ctx, scope, &ast, declarations);

    compilation.diagnostics.extend_from_slice(ctx.diagnostics());

    compilation
}
