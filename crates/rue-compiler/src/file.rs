use std::{collections::HashSet, fs, io, path::Path, sync::Arc};

use clvmr::{Allocator, NodePtr};
use id_arena::Arena;
use indexmap::{IndexMap, IndexSet, indexset};
use rue_ast::{AstDocument, AstNode};
use rue_diagnostic::{Source, SourceKind};
use rue_hir::{
    Declaration, DependencyGraph, Environment, Lowerer, ModuleDeclarations, ModuleSymbol, ScopeId,
    Symbol, SymbolId,
};
use rue_lexer::Lexer;
use rue_parser::Parser;
use thiserror::Error;

use crate::{
    Compiler, ImportCache, check_unused, compile_symbol_items, compile_type_items,
    declare_module_items, declare_symbol_items, declare_type_items, resolve_imports,
};

#[derive(Debug, Error)]
pub enum Error {
    #[error("IO error: {0}")]
    Io(#[from] io::Error),

    #[error("Codegen error: {0}")]
    Lir(#[from] rue_lir::Error),

    #[error("Source not found in compilation unit: {0}")]
    SourceNotFound(SourceKind),
}

#[derive(Debug, Clone)]
pub struct CompiledTest {
    pub name: Option<String>,
    pub path: SourceKind,
    pub symbol: SymbolId,
    pub ptr: NodePtr,
}

#[derive(Debug, Clone)]
pub struct CompiledExport {
    pub name: String,
    pub symbol: SymbolId,
    pub ptr: NodePtr,
}

#[derive(Debug, Clone)]
pub struct CompilationUnit {
    tree: FileTree,
}

impl CompilationUnit {
    pub fn new(ctx: &mut Compiler, path: &Path) -> Result<Self, Error> {
        let prelude = create_prelude(ctx);

        ctx.push_scope_raw(prelude);

        let tree =
            FileTree::from_path(ctx, path)?.ok_or(io::Error::other("Invalid file extension"))?;

        tree.compile(ctx);

        ctx.pop_scope_raw();

        let entrypoints = tree.entrypoints(ctx);
        check_unused(ctx, &entrypoints);

        Ok(Self { tree })
    }

    pub fn single_file(ctx: &mut Compiler, path: &Path, file_name: String) -> Result<Self, Error> {
        let prelude = create_prelude(ctx);

        ctx.push_scope_raw(prelude);

        let text = fs::read_to_string(path)?;
        let tree = FileTree::File(File::new(
            ctx,
            "main".to_string(),
            Source::new(Arc::from(text), SourceKind::File(file_name)),
        ));

        tree.compile(ctx);

        ctx.pop_scope_raw();

        let entrypoints = tree.entrypoints(ctx);
        check_unused(ctx, &entrypoints);

        Ok(Self { tree })
    }

    pub fn main(
        &self,
        ctx: &mut Compiler,
        allocator: &mut Allocator,
        path: &SourceKind,
    ) -> Result<Option<NodePtr>, Error> {
        let tree = self
            .tree
            .find(path)
            .ok_or_else(|| Error::SourceNotFound(path.clone()))?;

        let scope = ctx.module(tree.module()).scope;
        let Some(main) = ctx.scope(scope).symbol("main") else {
            return Ok(None);
        };

        Ok(Some(codegen(ctx, allocator, main)?))
    }

    pub fn exports(
        &self,
        ctx: &mut Compiler,
        allocator: &mut Allocator,
        path: &SourceKind,
        export_name: Option<&str>,
    ) -> Result<Vec<CompiledExport>, Error> {
        let tree = self
            .tree
            .find(path)
            .ok_or_else(|| Error::SourceNotFound(path.clone()))?;

        let scope = ctx.module(tree.module()).scope;

        let mut exports = Vec::new();

        for (name, symbol) in ctx
            .scope(scope)
            .exported_symbols()
            .map(|(name, symbol)| (name.to_string(), symbol))
            .collect::<Vec<_>>()
        {
            if let Some(export_name) = export_name
                && name != export_name
            {
                continue;
            }

            let ptr = codegen(ctx, allocator, symbol)?;
            exports.push(CompiledExport { name, symbol, ptr });
        }

        Ok(exports)
    }

    pub fn tests(
        &self,
        ctx: &mut Compiler,
        allocator: &mut Allocator,
        path: Option<&SourceKind>,
        search_term: Option<&str>,
    ) -> Result<Vec<CompiledTest>, Error> {
        let tests = ctx
            .tests()
            .filter(|test| {
                if let Some(path) = path
                    && &test.path != path
                {
                    return false;
                }

                if let Some(search_term) = search_term {
                    let Some(name) = &test.name else {
                        return false;
                    };

                    return name.to_lowercase().contains(&search_term.to_lowercase());
                }

                true
            })
            .cloned()
            .collect::<Vec<_>>();

        let mut outputs = Vec::new();

        for test in tests {
            let ptr = codegen(ctx, allocator, test.symbol)?;
            outputs.push(CompiledTest {
                name: test.name,
                path: test.path,
                symbol: test.symbol,
                ptr,
            });
        }

        Ok(outputs)
    }
}

#[derive(Debug, Clone)]
pub enum FileTree {
    File(File),
    Directory(Directory),
}

impl FileTree {
    pub fn from_path(ctx: &mut Compiler, path: &Path) -> Result<Option<Self>, Error> {
        let file_name = path
            .file_name()
            .ok_or(io::Error::new(
                io::ErrorKind::InvalidFilename,
                "Missing file name",
            ))?
            .to_string_lossy()
            .to_string();

        if fs::metadata(path)?.is_file() {
            let text = fs::read_to_string(path)?;

            let source = Source::new(Arc::from(text), normalize_path(path)?);

            #[allow(clippy::case_sensitive_file_extension_comparisons)]
            if !file_name.ends_with(".rue") {
                return Ok(None);
            }

            Ok(Some(Self::File(File::new(
                ctx,
                file_name.replace(".rue", ""),
                source,
            ))))
        } else {
            let mut children = Vec::new();

            for entry in fs::read_dir(path)? {
                let entry = entry?;
                let path = entry.path();
                let tree = Self::from_path(ctx, &path)?;
                children.extend(tree);
            }

            Ok(Some(Self::Directory(Directory::new(
                ctx, file_name, children,
            ))))
        }
    }

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

    pub fn entrypoints(&self, ctx: &Compiler) -> HashSet<Declaration> {
        let mut entrypoints = ctx
            .tests()
            .map(|test| Declaration::Symbol(test.symbol))
            .collect::<HashSet<_>>();

        let mut stack = vec![self];

        while let Some(current) = stack.pop() {
            match current {
                Self::File(file) => {
                    let scope = file.module(ctx).scope;

                    if let Some(main) = ctx.scope(scope).symbol("main") {
                        entrypoints.insert(Declaration::Symbol(main));
                    }

                    ctx.scope(scope).exported_symbols().for_each(|(_, symbol)| {
                        entrypoints.insert(Declaration::Symbol(symbol));
                    });

                    ctx.scope(scope).exported_types().for_each(|(_, ty)| {
                        entrypoints.insert(Declaration::Type(ty));
                    });
                }
                Self::Directory(directory) => {
                    directory.children.iter().for_each(|child| {
                        stack.push(child);
                    });
                }
            }
        }

        entrypoints
    }

    pub fn find(&self, path: &SourceKind) -> Option<&FileTree> {
        match self {
            Self::File(file) => {
                if file.source.kind == *path {
                    Some(self)
                } else {
                    None
                }
            }
            Self::Directory(directory) => {
                directory.children.iter().find_map(|child| child.find(path))
            }
        }
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

    pub fn std(ctx: &mut Compiler) -> Self {
        let text = include_str!("./std.rue");

        Self::new(
            ctx,
            "std".to_string(),
            Source::new(Arc::from(text), SourceKind::Std),
        )
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

pub fn normalize_path(path: &Path) -> Result<SourceKind, Error> {
    Ok(SourceKind::File(
        path.canonicalize()?.to_string_lossy().to_string(),
    ))
}

fn codegen(
    ctx: &mut Compiler,
    allocator: &mut Allocator,
    symbol: SymbolId,
) -> Result<NodePtr, Error> {
    let options = *ctx.options();
    let graph = DependencyGraph::build(ctx, symbol, options);

    let mut arena = Arena::new();
    let mut lowerer = Lowerer::new(ctx, &mut arena, &graph, options, symbol);
    let mut lir = lowerer.lower_symbol_value(&Environment::default(), symbol);

    if options.optimize_lir {
        lir = rue_lir::optimize(&mut arena, lir);
    }

    Ok(rue_lir::codegen(&arena, allocator, lir)?)
}

fn create_prelude(ctx: &mut Compiler) -> ScopeId {
    let std = File::std(ctx);
    let tree = FileTree::File(std.clone());
    tree.compile(ctx);
    let std_scope = std.module(ctx).scope;

    let prelude = ctx.alloc_child_scope();

    for (name, symbol) in ctx
        .scope(std_scope)
        .exported_symbols()
        .map(|(name, symbol)| (name.to_string(), symbol))
        .collect::<Vec<_>>()
    {
        ctx.scope_mut(prelude)
            .insert_symbol(name.to_string(), symbol, false);
    }

    for (name, ty) in ctx
        .scope(std_scope)
        .exported_types()
        .map(|(name, ty)| (name.to_string(), ty))
        .collect::<Vec<_>>()
    {
        ctx.scope_mut(prelude)
            .insert_type(name.to_string(), ty, false);
    }

    ctx.push_scope(prelude, std.document.syntax().text_range().start());
    ctx.pop_scope(std.document.syntax().text_range().end());

    prelude
}
