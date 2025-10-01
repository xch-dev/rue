use std::{collections::HashSet, sync::Arc};

use clvmr::{Allocator, NodePtr};
use id_arena::Arena;
use rue_ast::{AstDocument, AstNode};
use rue_diagnostic::{Diagnostic, DiagnosticSeverity, Source, SourceKind};
use rue_hir::{DependencyGraph, Environment, Lowerer, Scope, ScopeId};
use rue_lexer::Lexer;
use rue_lir::{Error, codegen, optimize};
use rue_options::CompilerOptions;
use rue_parser::Parser;

use crate::{Compiler, check_unused, compile_document, declare_document};

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

pub fn compile_file(
    allocator: &mut Allocator,
    file: Source,
    options: CompilerOptions,
) -> Result<Compilation, Error> {
    compile_file_impl(allocator, file, options, true)
}

pub fn analyze_file(file: Source, options: CompilerOptions) -> Result<Compilation, Error> {
    compile_file_impl(&mut Allocator::new(), file, options, false)
}

fn compile_file_impl(
    allocator: &mut Allocator,
    file: Source,
    options: CompilerOptions,
    do_codegen: bool,
) -> Result<Compilation, Error> {
    let mut ctx = Compiler::new(options);
    let std = compile_file_partial(
        &mut ctx,
        Source::new(Arc::from(include_str!("./std.rue")), SourceKind::Std),
    );

    let mut scope = Scope::new();

    for (name, symbol) in ctx.scope(std.scope).exported_symbols() {
        scope.insert_symbol(name.to_string(), symbol, false);
    }

    for (name, ty) in ctx.scope(std.scope).exported_types() {
        scope.insert_type(name.to_string(), ty, false);
    }

    let scope = ctx.alloc_scope(scope);

    ctx.push_scope(scope);
    let file = compile_file_partial(&mut ctx, file);

    let mut compilation = Compilation {
        diagnostics: file.diagnostics,
        program: None,
    };

    let main_symbol = ctx.scope(file.scope).symbol("main");

    let mut entrypoints = HashSet::new();

    entrypoints.extend(main_symbol);

    for test in ctx.tests() {
        entrypoints.insert(test);
    }

    check_unused(&mut ctx, &entrypoints);

    compilation.diagnostics.extend(ctx.take_diagnostics());

    if !do_codegen
        || compilation
            .diagnostics
            .iter()
            .any(|d| d.kind.severity() == DiagnosticSeverity::Error)
    {
        return Ok(compilation);
    }

    let Some(symbol) = main_symbol else {
        return Ok(compilation);
    };

    let graph = DependencyGraph::build(&ctx, symbol);

    let mut arena = Arena::new();
    let mut lowerer = Lowerer::new(&mut ctx, &mut arena, &graph, options, symbol);
    let mut lir = lowerer.lower_symbol_value(&Environment::default(), symbol);

    if options.optimize_lir {
        lir = optimize(&mut arena, lir);
    }

    compilation.program = Some(codegen(&arena, allocator, lir)?);

    Ok(compilation)
}

fn compile_file_partial(ctx: &mut Compiler, source: Source) -> PartialCompilation {
    let tokens = Lexer::new(&source.text).collect::<Vec<_>>();
    let parser = Parser::new(source.clone(), tokens);
    let parse_result = parser.parse();

    ctx.set_source(source);

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

    compilation.diagnostics.extend(ctx.take_diagnostics());

    compilation
}
