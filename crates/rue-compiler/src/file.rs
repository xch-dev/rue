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
pub struct Test {
    pub name: String,
    pub program: NodePtr,
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

    let std = File::std(&mut ctx).unwrap();
    let file = File::parse(&mut ctx, file).unwrap();

    compile_file_tree(&mut ctx, FileTree::File(std.clone()))?;

    for (name, symbol) in ctx
        .scope(std.scope)
        .exported_symbols()
        .map(|(name, symbol)| (name.to_string(), symbol))
        .collect::<Vec<_>>()
    {
        ctx.scope_mut(file.scope).insert_symbol(name, symbol, false);
    }

    for (name, ty) in ctx
        .scope(std.scope)
        .exported_types()
        .map(|(name, ty)| (name.to_string(), ty))
        .collect::<Vec<_>>()
    {
        ctx.scope_mut(file.scope).insert_type(name, ty, false);
    }

    compile_file_tree(&mut ctx, FileTree::File(file.clone()))?;

    let mut compilation = Compilation {
        diagnostics: ctx.take_diagnostics(),
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

#[derive(Debug, Clone)]
pub struct File {
    source: Source,
    document: AstDocument,
    scope: ScopeId,
    declarations: ModuleDeclarations,
}

impl File {
    pub fn parse(ctx: &mut Compiler, source: Source) -> Option<Self> {
        let tokens = Lexer::new(&source.text).collect::<Vec<_>>();
        let parser = Parser::new(source.clone(), tokens);
        let parse = parser.parse();

        for diagnostic in parse.diagnostics {
            ctx.insert_diagnostic(diagnostic);
        }

        Some(Self {
            source,
            document: AstDocument::cast(parse.node)?,
            scope: ctx.alloc_scope(Scope::new()),
            declarations: ModuleDeclarations::default(),
        })
    }

    pub fn std(ctx: &mut Compiler) -> Option<Self> {
        Self::parse(
            ctx,
            Source::new(Arc::from(include_str!("./std.rue")), SourceKind::Std),
        )
    }
}

#[derive(Debug, Clone)]
pub struct Directory {
    scope: ScopeId,
    children: Vec<FileTree>,
}

#[derive(Debug, Clone)]
pub enum FileTree {
    File(File),
    Directory(Directory),
}

pub fn compile_file_tree(ctx: &mut Compiler, mut tree: FileTree) -> Result<(), Error> {
    declare_types_tree(ctx, &mut tree)?;
    declare_symbols_tree(ctx, &mut tree)?;
    compile_types_tree(ctx, &tree)?;
    compile_symbols_tree(ctx, &tree)?;
    Ok(())
}

fn declare_types_tree(ctx: &mut Compiler, tree: &mut FileTree) -> Result<(), Error> {
    match tree {
        FileTree::File(file) => {
            ctx.set_source(file.source.clone());
            ctx.push_scope(file.scope);
            declare_type_items(ctx, file.document.items(), &mut file.declarations);
            ctx.pop_scope();
        }
        FileTree::Directory(directory) => {
            ctx.push_scope(directory.scope);
            for child in &mut directory.children {
                declare_types_tree(ctx, child)?;
            }
            ctx.pop_scope();
        }
    }

    Ok(())
}

fn declare_symbols_tree(ctx: &mut Compiler, tree: &mut FileTree) -> Result<(), Error> {
    match tree {
        FileTree::File(file) => {
            ctx.set_source(file.source.clone());
            ctx.push_scope(file.scope);
            declare_symbol_items(ctx, file.document.items(), &mut file.declarations);
            ctx.pop_scope();
        }
        FileTree::Directory(directory) => {
            ctx.push_scope(directory.scope);
            for child in &mut directory.children {
                declare_symbols_tree(ctx, child)?;
            }
            ctx.pop_scope();
        }
    }

    Ok(())
}

fn compile_types_tree(ctx: &mut Compiler, tree: &FileTree) -> Result<(), Error> {
    match tree {
        FileTree::File(file) => {
            ctx.set_source(file.source.clone());
            ctx.push_scope(file.scope);
            compile_type_items(ctx, file.document.items(), &file.declarations);
            ctx.pop_scope();
        }
        FileTree::Directory(directory) => {
            ctx.push_scope(directory.scope);
            for child in &directory.children {
                compile_types_tree(ctx, child)?;
            }
            ctx.pop_scope();
        }
    }

    Ok(())
}

fn compile_symbols_tree(ctx: &mut Compiler, tree: &FileTree) -> Result<(), Error> {
    match tree {
        FileTree::File(file) => {
            ctx.set_source(file.source.clone());
            ctx.push_scope(file.scope);
            compile_symbol_items(ctx, file.document.items(), &file.declarations);
            ctx.pop_scope();
        }
        FileTree::Directory(directory) => {
            ctx.push_scope(directory.scope);
            for child in &directory.children {
                compile_symbols_tree(ctx, child)?;
            }
            ctx.pop_scope();
        }
    }

    Ok(())
}
