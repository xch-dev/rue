use std::{collections::HashSet, sync::Arc};

use clvmr::{Allocator, NodePtr};
use id_arena::Arena;
use indexmap::IndexMap;
use rue_ast::{AstDocument, AstNode};
use rue_diagnostic::{Diagnostic, DiagnosticSeverity, Source, SourceKind};
use rue_hir::{
    Declaration, DependencyGraph, Environment, Lowerer, ModuleDeclarations, Scope, ScopeId,
    SymbolId,
};
use rue_lexer::Lexer;
use rue_lir::{Error, codegen, optimize};
use rue_options::CompilerOptions;
use rue_parser::Parser;

use crate::{
    Compiler, check_unused, compile_symbol_items, compile_type_items, declare_symbol_items,
    declare_type_items,
};

#[derive(Debug, Clone)]
pub struct Compilation {
    pub diagnostics: Vec<Diagnostic>,
    pub main: Option<NodePtr>,
    pub exports: IndexMap<String, NodePtr>,
    pub tests: Vec<Test>,
}

#[derive(Debug, Clone)]
pub struct CompilationWithContext {
    pub compilation: Compilation,
    pub compiler: Compiler,
    pub std_scope: ScopeId,
    pub file_scope: ScopeId,
    pub scope_map: crate::ScopeMap,
}

#[derive(Debug, Clone)]
pub struct Test {
    pub name: String,
    pub program: NodePtr,
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

pub fn analyze_file_with_context(
    file: Source,
    options: CompilerOptions,
) -> Result<CompilationWithContext, Error> {
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
    let file_partial = compile_file_partial(&mut ctx, file);

    let mut compilation = Compilation {
        diagnostics: [std.diagnostics, file_partial.diagnostics].concat(),
        main: None,
        exports: IndexMap::new(),
        tests: Vec::new(),
    };

    let main_symbol = ctx.scope(file_partial.scope).symbol("main");

    let mut entrypoints = HashSet::new();

    entrypoints.extend(main_symbol.map(Declaration::Symbol));

    for test in ctx.tests() {
        entrypoints.insert(Declaration::Symbol(test));
    }

    for (_, symbol) in ctx.scope(file_partial.scope).exported_symbols() {
        entrypoints.insert(Declaration::Symbol(symbol));
    }

    for (_, ty) in ctx.scope(file_partial.scope).exported_types() {
        entrypoints.insert(Declaration::Type(ty));
    }

    check_unused(&mut ctx, &entrypoints);

    compilation.diagnostics.extend(ctx.take_diagnostics());
    
    // Finalize the scope map
    ctx.scope_map_mut().finalize();
    let scope_map = ctx.scope_map().clone();

    Ok(CompilationWithContext {
        compilation,
        compiler: ctx,
        std_scope: std.scope,
        file_scope: file_partial.scope,
        scope_map,
    })
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
        diagnostics: [std.diagnostics, file.diagnostics].concat(),
        main: None,
        exports: IndexMap::new(),
        tests: Vec::new(),
    };

    let main_symbol = ctx.scope(file.scope).symbol("main");

    let mut entrypoints = HashSet::new();

    entrypoints.extend(main_symbol.map(Declaration::Symbol));

    for test in ctx.tests() {
        entrypoints.insert(Declaration::Symbol(test));
    }

    for (_, symbol) in ctx.scope(file.scope).exported_symbols() {
        entrypoints.insert(Declaration::Symbol(symbol));
    }

    for (_, ty) in ctx.scope(file.scope).exported_types() {
        entrypoints.insert(Declaration::Type(ty));
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

    if let Some(symbol) = main_symbol {
        compilation.main = Some(generate(&mut ctx, allocator, symbol, options)?);
    }

    for (name, symbol) in ctx
        .scope(file.scope)
        .exported_symbols()
        .map(|(name, symbol)| (name.to_string(), symbol))
        .collect::<Vec<_>>()
    {
        compilation.exports.insert(
            name.to_string(),
            generate(&mut ctx, allocator, symbol, options)?,
        );
    }

    for test in ctx.tests().collect::<Vec<_>>() {
        let name = ctx.symbol(test).name().unwrap().text().to_string();

        compilation.tests.push(Test {
            name,
            program: generate(&mut ctx, allocator, test, options)?,
        });
    }

    Ok(compilation)
}

fn generate(
    ctx: &mut Compiler,
    allocator: &mut Allocator,
    symbol: SymbolId,
    options: CompilerOptions,
) -> Result<NodePtr, Error> {
    let graph = DependencyGraph::build(ctx, symbol, options);

    let mut arena = Arena::new();
    let mut lowerer = Lowerer::new(ctx, &mut arena, &graph, options, symbol);
    let mut lir = lowerer.lower_symbol_value(&Environment::default(), symbol);

    if options.optimize_lir {
        lir = optimize(&mut arena, lir);
    }

    codegen(&arena, allocator, lir)
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

    let mut declarations = ModuleDeclarations::default();

    declare_type_items(ctx, scope, ast.items(), &mut declarations);
    declare_symbol_items(ctx, scope, ast.items(), &mut declarations);
    compile_type_items(ctx, scope, ast.items(), &declarations);
    compile_symbol_items(ctx, scope, ast.items(), &declarations);

    compilation.diagnostics.extend(ctx.take_diagnostics());

    compilation
}
