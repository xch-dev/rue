use std::{collections::HashSet, sync::Arc};

use clvmr::{Allocator, NodePtr};
use id_arena::Arena;
use indexmap::{IndexMap, IndexSet, indexset};
use rowan::TextSize;
use rue_ast::{AstDocument, AstNode};
use rue_diagnostic::{Diagnostic, DiagnosticSeverity, Source, SourceKind};
use rue_hir::{
    Declaration, DependencyGraph, Environment, Lowerer, ModuleDeclarations, ModuleSymbol, ScopeId,
    Symbol, SymbolId,
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
pub enum FileTree {
    File(File),
    Directory(Directory),
}

impl FileTree {
    pub fn name(&self) -> &str {
        match self {
            Self::File(file) => &file.name,
            Self::Directory(directory) => &directory.name,
        }
    }

    pub fn module(&self) -> SymbolId {
        match self {
            Self::File(file) => file.module,
            Self::Directory(directory) => directory.module,
        }
    }

    pub fn all_modules(&self) -> IndexSet<SymbolId> {
        match self {
            Self::File(file) => indexset![file.module],
            Self::Directory(directory) => directory
                .children
                .iter()
                .flat_map(FileTree::all_modules)
                .collect(),
        }
    }

    pub fn compile(&self, ctx: &mut Compiler) {
        let mut cache = ImportCache::default();

        let modules = self.all_modules().into_iter().collect::<Vec<_>>();

        self.declare_modules(ctx);
        self.declare_types(ctx);

        resolve_imports(ctx, modules.clone(), &mut cache, false);

        self.declare_symbols(ctx);

        resolve_imports(ctx, modules.clone(), &mut cache, false);
        resolve_imports(ctx, modules.clone(), &mut cache, true);

        self.compile_types(ctx);
        self.compile_symbols(ctx);
    }

    fn declare_modules(&self, ctx: &mut Compiler) {
        match self {
            Self::File(file) => file.declare_modules(ctx),
            Self::Directory(directory) => directory.declare_modules(ctx),
        }
    }

    fn declare_types(&self, ctx: &mut Compiler) {
        match self {
            Self::File(file) => file.declare_types(ctx),
            Self::Directory(directory) => directory.declare_types(ctx),
        }
    }

    fn declare_symbols(&self, ctx: &mut Compiler) {
        match self {
            Self::File(file) => file.declare_symbols(ctx),
            Self::Directory(directory) => directory.declare_symbols(ctx),
        }
    }

    fn compile_types(&self, ctx: &mut Compiler) {
        match self {
            Self::File(file) => file.compile_types(ctx),
            Self::Directory(directory) => directory.compile_types(ctx),
        }
    }

    fn compile_symbols(&self, ctx: &mut Compiler) {
        match self {
            Self::File(file) => file.compile_symbols(ctx),
            Self::Directory(directory) => directory.compile_symbols(ctx),
        }
    }
}

#[derive(Debug, Clone)]
pub struct File {
    pub name: String,
    pub source: Source,
    pub document: AstDocument,
    pub module: SymbolId,
    pub sibling_scope: ScopeId,
}

impl File {
    pub fn new(ctx: &mut Compiler, name: String, source: Source) -> Self {
        let tokens = Lexer::new(&source.text).collect::<Vec<_>>();
        let parser = Parser::new(source.clone(), tokens);
        let parse_result = parser.parse();

        ctx.extend_diagnostics(parse_result.diagnostics);

        let document = AstDocument::cast(parse_result.node).unwrap();

        let sibling_scope = ctx.alloc_child_scope();

        ctx.push_scope(sibling_scope, document.syntax().text_range().start());

        let scope = ctx.alloc_child_scope();

        ctx.pop_scope(document.syntax().text_range().end());

        let module = ctx.alloc_symbol(Symbol::Module(ModuleSymbol {
            name: None,
            scope,
            declarations: ModuleDeclarations::default(),
        }));

        Self {
            name,
            source,
            document,
            module,
            sibling_scope,
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
        ctx.push_scope(
            self.sibling_scope,
            self.document.syntax().text_range().start(),
        );
        ctx.push_scope(scope, self.document.syntax().text_range().start());
        self.module(ctx).declarations.clone()
    }

    fn end(&self, ctx: &mut Compiler, declarations: ModuleDeclarations) {
        ctx.pop_scope(self.document.syntax().text_range().end());
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

#[derive(Debug, Clone)]
pub struct Directory {
    pub name: String,
    pub children: Vec<FileTree>,
    pub module: SymbolId,
}

impl Directory {
    pub fn new(ctx: &mut Compiler, name: String, children: Vec<FileTree>) -> Self {
        let scope = ctx.alloc_child_scope();

        let module = ctx.alloc_symbol(Symbol::Module(ModuleSymbol {
            name: None,
            scope,
            declarations: ModuleDeclarations::default(),
        }));

        let mut child_modules = IndexMap::new();

        for child in &children {
            let name = child.name().to_string();
            let module = child.module();
            child_modules.insert(name.clone(), module);
            ctx.scope_mut(scope).insert_symbol(name, module, true);
        }

        for child in &children {
            let mut child_modules = child_modules.clone();

            child_modules.shift_remove(child.name());

            for (name, module) in child_modules {
                if let FileTree::File(file) = child {
                    ctx.scope_mut(file.sibling_scope)
                        .insert_symbol(name, module, false);
                }
            }
        }

        Self {
            name,
            children,
            module,
        }
    }

    fn declare_modules(&self, ctx: &mut Compiler) {
        for child in &self.children {
            child.declare_modules(ctx);
        }
    }

    fn declare_types(&self, ctx: &mut Compiler) {
        for child in &self.children {
            child.declare_types(ctx);
        }
    }

    fn declare_symbols(&self, ctx: &mut Compiler) {
        for child in &self.children {
            child.declare_symbols(ctx);
        }
    }

    fn compile_types(&self, ctx: &mut Compiler) {
        for child in &self.children {
            child.compile_types(ctx);
        }
    }

    fn compile_symbols(&self, ctx: &mut Compiler) {
        for child in &self.children {
            child.compile_symbols(ctx);
        }
    }
}

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
    let file = File::new(ctx, source.kind.to_string().replace(".rue", ""), source);

    FileTree::File(file.clone()).compile(ctx);

    file
}
