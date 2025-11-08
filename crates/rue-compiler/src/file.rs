use std::{collections::HashSet, sync::Arc};

use clvmr::{Allocator, NodePtr};
use id_arena::Arena;
use indexmap::IndexMap;
use rowan::TextSize;
use rue_ast::{AstDocument, AstNode};
use rue_diagnostic::{Diagnostic, DiagnosticSeverity, Source, SourceKind};
use rue_hir::{
    Declaration, DependencyGraph, Environment, Lowerer, ModuleDeclarations, ModuleSymbol, Symbol,
    SymbolId,
};
use rue_lexer::Lexer;
use rue_lir::{Error, codegen, optimize};
use rue_options::CompilerOptions;
use rue_parser::Parser;

use crate::{
    Compiler, ImportCache, SyntaxMap, check_unused, compile_symbol_items, compile_type_items,
    declare_module_items, declare_symbol_items, declare_type_items, resolve_imports,
};

#[derive(Debug, Clone)]
pub struct Compilation {
    pub compiler: Compiler,
    pub diagnostics: Vec<Diagnostic>,
    pub main: Option<NodePtr>,
    pub exports: IndexMap<String, NodePtr>,
    pub tests: Vec<Test>,
    pub syntax_map: SyntaxMap,
    pub source: Source,
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
    let std_scope = std.module(&ctx).scope;

    let scope = ctx.alloc_child_scope();

    for (name, symbol) in ctx
        .scope(std_scope)
        .exported_symbols()
        .map(|(name, symbol)| (name.to_string(), symbol))
        .collect::<Vec<_>>()
    {
        ctx.scope_mut(scope)
            .insert_symbol(name.to_string(), symbol, false);
    }

    for (name, ty) in ctx
        .scope(std_scope)
        .exported_types()
        .map(|(name, ty)| (name.to_string(), ty))
        .collect::<Vec<_>>()
    {
        ctx.scope_mut(scope)
            .insert_type(name.to_string(), ty, false);
    }

    ctx.push_scope(scope, TextSize::from(0));
    let compiled_file = compile_file_partial(&mut ctx, file.clone());
    let compiled_file_scope = compiled_file.module(&ctx).scope;
    #[allow(clippy::cast_possible_truncation)]
    ctx.pop_scope(TextSize::from(std_source.len() as u32));

    let mut exports = IndexMap::new();
    let mut tests = Vec::new();

    let syntax_map = ctx.syntax_map(&file.kind).unwrap().clone();
    let main_symbol = ctx.scope(compiled_file_scope).symbol("main");

    let mut entrypoints = HashSet::new();

    entrypoints.extend(main_symbol.map(Declaration::Symbol));

    for test in ctx.tests() {
        entrypoints.insert(Declaration::Symbol(test));
    }

    for (_, symbol) in ctx.scope(compiled_file_scope).exported_symbols() {
        entrypoints.insert(Declaration::Symbol(symbol));
    }

    for (_, ty) in ctx.scope(compiled_file_scope).exported_types() {
        entrypoints.insert(Declaration::Type(ty));
    }

    check_unused(&mut ctx, &entrypoints);

    let diagnostics = ctx.take_diagnostics();

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
        .scope(compiled_file_scope)
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
            program: generate(&mut ctx, allocator, test, options)?,
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

fn compile_file_partial(ctx: &mut Compiler, source: Source) -> File {
    let file = File::new(ctx, source);

    let mut cache = ImportCache::default();

    file.declare_modules(ctx);
    file.declare_types(ctx);

    resolve_imports(ctx, vec![file.module], &mut cache, false);

    file.declare_symbols(ctx);

    resolve_imports(ctx, vec![file.module], &mut cache, false);
    resolve_imports(ctx, vec![file.module], &mut cache, true);

    file.compile_types(ctx);
    file.compile_symbols(ctx);

    file
}

struct File {
    source: Source,
    document: AstDocument,
    module: SymbolId,
}

impl File {
    fn new(ctx: &mut Compiler, source: Source) -> Self {
        let tokens = Lexer::new(&source.text).collect::<Vec<_>>();
        let parser = Parser::new(source.clone(), tokens);
        let parse_result = parser.parse();

        ctx.extend_diagnostics(parse_result.diagnostics);

        let scope = ctx.alloc_child_scope();

        let module = ctx.alloc_symbol(Symbol::Module(ModuleSymbol {
            name: None,
            scope,
            declarations: ModuleDeclarations::default(),
        }));

        let document = AstDocument::cast(parse_result.node).unwrap();

        Self {
            source,
            document,
            module,
        }
    }

    fn module<'a>(&'a self, ctx: &'a Compiler) -> &'a ModuleSymbol {
        match ctx.symbol(self.module) {
            Symbol::Module(module) => module,
            _ => unreachable!(),
        }
    }

    fn module_mut<'a>(&'a self, ctx: &'a mut Compiler) -> &'a mut ModuleSymbol {
        match ctx.symbol_mut(self.module) {
            Symbol::Module(module) => module,
            _ => unreachable!(),
        }
    }

    fn begin(&self, ctx: &mut Compiler) -> ModuleDeclarations {
        let scope = self.module(ctx).scope;
        ctx.set_source(self.source.clone());
        ctx.push_scope(scope, self.document.syntax().text_range().start());
        self.module(ctx).declarations.clone()
    }

    fn end(&self, ctx: &mut Compiler, declarations: ModuleDeclarations) {
        ctx.pop_scope(self.document.syntax().text_range().end());
        self.module_mut(ctx).declarations = declarations;
    }

    fn declare_modules(&self, ctx: &mut Compiler) {
        let mut declarations = self.begin(ctx);
        declare_module_items(ctx, self.document.items(), &mut declarations);
        self.end(ctx, declarations);
    }

    fn declare_types(&self, ctx: &mut Compiler) {
        let mut declarations = self.begin(ctx);
        declare_type_items(ctx, self.document.items(), &mut declarations);
        self.end(ctx, declarations);
    }

    fn declare_symbols(&self, ctx: &mut Compiler) {
        let mut declarations = self.begin(ctx);
        declare_symbol_items(ctx, self.document.items(), &mut declarations);
        self.end(ctx, declarations);
    }

    fn compile_types(&self, ctx: &mut Compiler) {
        let declarations = self.begin(ctx);
        compile_type_items(ctx, self.document.items(), &declarations);
        self.end(ctx, declarations);
    }

    fn compile_symbols(&self, ctx: &mut Compiler) {
        let declarations = self.begin(ctx);
        compile_symbol_items(ctx, self.document.items(), &declarations);
        self.end(ctx, declarations);
    }
}
