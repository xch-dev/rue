use std::{collections::HashSet, sync::Arc};

use clvmr::{Allocator, NodePtr};
use id_arena::Arena;
use indexmap::IndexMap;
use rowan::TextSize;
use rue_ast::{AstDocument, AstNode};
use rue_diagnostic::{Diagnostic, DiagnosticSeverity, Source, SourceKind};
use rue_hir::{
    Declaration, DependencyGraph, Environment, Lowerer, ModuleDeclarations, Scope, ScopeId,
    SymbolId,
};
use rue_lexer::Lexer;
use rue_lir::{Error, codegen, optimize, stringify_lir};
use rue_options::CompilerOptions;
use rue_parser::Parser;

use crate::{
    Compiler, SyntaxMap, check_unused, compile_symbol_items, compile_type_items,
    declare_symbol_items, declare_type_items,
};

#[derive(Debug, Clone)]
pub struct Compilation {
    pub compiler: Compiler,
    pub diagnostics: Vec<Diagnostic>,
    pub main: Option<CodegenOutput>,
    pub exports: IndexMap<String, CodegenOutput>,
    pub tests: Vec<Test>,
    pub syntax_map: SyntaxMap,
    pub source: Source,
}

#[derive(Debug, Clone)]
pub struct CodegenOutput {
    pub program: NodePtr,
    pub lir: String,
}

#[derive(Debug, Clone)]
pub struct Test {
    pub name: String,
    pub output: CodegenOutput,
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

#[allow(clippy::needless_pass_by_value)]
fn compile_file_impl(
    allocator: &mut Allocator,
    file: Source,
    options: CompilerOptions,
    do_codegen: bool,
) -> Result<Compilation, Error> {
    let mut ctx = Compiler::new(options);
    let std_source = include_str!("./std.rue");
    let std = compile_file_partial(
        &mut ctx,
        Source::new(Arc::from(std_source), SourceKind::Std),
    );

    let mut scope = Scope::new();

    for (name, symbol) in ctx.scope(std.scope).exported_symbols() {
        scope.insert_symbol(name.to_string(), symbol, false);
    }

    for (name, ty) in ctx.scope(std.scope).exported_types() {
        scope.insert_type(name.to_string(), ty, false);
    }

    let scope = ctx.alloc_scope(scope);

    ctx.push_scope(scope, TextSize::from(0));
    let compiled_file = compile_file_partial(&mut ctx, file.clone());
    #[allow(clippy::cast_possible_truncation)]
    ctx.pop_scope(TextSize::from(std_source.len() as u32));

    let mut diagnostics = [std.diagnostics, compiled_file.diagnostics].concat();
    let mut exports = IndexMap::new();
    let mut tests = Vec::new();

    let syntax_map = ctx.syntax_map(&file.kind).unwrap().clone();
    let main_symbol = ctx.scope(compiled_file.scope).symbol("main");

    let mut entrypoints = HashSet::new();

    entrypoints.extend(main_symbol.map(Declaration::Symbol));

    for test in ctx.tests() {
        entrypoints.insert(Declaration::Symbol(test));
    }

    for (_, symbol) in ctx.scope(compiled_file.scope).exported_symbols() {
        entrypoints.insert(Declaration::Symbol(symbol));
    }

    for (_, ty) in ctx.scope(compiled_file.scope).exported_types() {
        entrypoints.insert(Declaration::Type(ty));
    }

    check_unused(&mut ctx, &entrypoints);

    diagnostics.extend(ctx.take_diagnostics());

    let mut main = None;

    if !do_codegen
        || diagnostics
            .iter()
            .any(|d| d.kind.severity() == DiagnosticSeverity::Error)
    {
        return Ok(Compilation {
            compiler: ctx,
            diagnostics,
            main,
            exports,
            tests,
            syntax_map,
            source: file,
        });
    }

    if let Some(symbol) = main_symbol {
        main = Some(generate(&mut ctx, allocator, symbol, options)?);
    }

    for (name, symbol) in ctx
        .scope(compiled_file.scope)
        .exported_symbols()
        .map(|(name, symbol)| (name.to_string(), symbol))
        .collect::<Vec<_>>()
    {
        exports.insert(
            name.to_string(),
            generate(&mut ctx, allocator, symbol, options)?,
        );
    }

    for test in ctx.tests().collect::<Vec<_>>() {
        let name = ctx.symbol(test).name().unwrap().text().to_string();

        tests.push(Test {
            name,
            output: generate(&mut ctx, allocator, test, options)?,
        });
    }

    Ok(Compilation {
        compiler: ctx,
        diagnostics,
        main,
        exports,
        tests,
        syntax_map,
        source: file,
    })
}

fn generate(
    ctx: &mut Compiler,
    allocator: &mut Allocator,
    symbol: SymbolId,
    options: CompilerOptions,
) -> Result<CodegenOutput, Error> {
    let graph = DependencyGraph::build(ctx, symbol, options);

    let mut arena = Arena::new();
    let mut lowerer = Lowerer::new(ctx, &mut arena, &graph, options, symbol);
    let mut lir = lowerer.lower_symbol_value(&Environment::default(), symbol);

    if options.optimize_lir {
        lir = optimize(&mut arena, lir);
    }

    let program = codegen(&arena, allocator, lir)?;

    Ok(CodegenOutput {
        program,
        lir: stringify_lir(&arena, lir),
    })
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

    let range = ast.syntax().text_range();
    ctx.push_scope(scope, range.start());
    declare_type_items(ctx, ast.items(), &mut declarations);
    declare_symbol_items(ctx, ast.items(), &mut declarations);
    compile_type_items(ctx, ast.items(), &declarations);
    compile_symbol_items(ctx, ast.items(), &declarations);
    ctx.pop_scope(range.end());

    compilation.diagnostics.extend(ctx.take_diagnostics());

    compilation
}
