//! Helpers for unit tests that compile an in-memory source and inspect what the compiler made of
//! it: the diagnostics, the symbols it declared, the types of expressions, and the values of
//! const expressions.

use std::sync::Arc;

use num_bigint::BigInt;
use rue_diagnostic::{Diagnostic, DiagnosticSeverity, Source, SourceKind};
use rue_hir::{Hir, HirId, ModuleDeclarations, Statement, Symbol, SymbolId};
use rue_options::CompilerOptions;
use rue_types::TypeId;

use crate::{Compiler, File, FileTree};

/// A source file that has been compiled, without the standard library.
pub(crate) struct Compiled {
    pub ctx: Compiler,
    pub module: SymbolId,
    pub diagnostics: Vec<Diagnostic>,
}

pub(crate) fn compile(source: &str) -> Compiled {
    compile_with(source, |_| {})
}

pub(crate) fn compile_with(source: &str, configure: impl FnOnce(&mut CompilerOptions)) -> Compiled {
    let mut options = CompilerOptions {
        std: false,
        ..CompilerOptions::default()
    };
    configure(&mut options);

    let mut ctx = Compiler::new(options);

    let file = File::new(
        &mut ctx,
        "main".to_string(),
        Source::new(Arc::from(source), SourceKind::File("main.rue".to_string())),
    );
    let module = file.module;

    FileTree::File(file).compile(&mut ctx);

    let diagnostics = ctx.take_diagnostics();

    Compiled {
        ctx,
        module,
        diagnostics,
    }
}

impl Compiled {
    /// Each error as `line:column Kind`, with the kind's `Debug` form, such as
    /// `3:5 UndeclaredSymbol("missing")`. Lines and columns start at 1.
    pub fn errors(&self) -> Vec<String> {
        self.diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.severity == DiagnosticSeverity::Error)
            .map(|diagnostic| format!("{} {:?}", diagnostic.start(), diagnostic.kind))
            .collect()
    }

    /// Panics with the diagnostics if there are any errors.
    pub fn assert_no_errors(&self) {
        assert_eq!(self.errors(), Vec::<String>::new());
    }

    /// The symbol that a path like `inner::value` names, looked up in the scope of each module
    /// along the way, starting from the file's module. This includes symbols that were imported
    /// into a module, whether or not they're exported from it.
    pub fn symbol(&self, path: &str) -> SymbolId {
        let mut module = self.module;
        let mut segments = path.split("::").peekable();

        while let Some(name) = segments.next() {
            let scope = self.ctx.module(module).scope;
            let symbol = self
                .ctx
                .scope(scope)
                .symbol(name)
                .unwrap_or_else(|| panic!("`{name}` isn't declared in the scope of `{path}`"));

            if segments.peek().is_none() {
                return symbol;
            }

            module = symbol;
        }

        unreachable!("paths have at least one segment")
    }

    /// The path of the module that declares the symbol a path names, which differs from the
    /// path's own module when the symbol was imported. The file's module is `(root)`.
    pub fn symbol_origin(&self, path: &str) -> String {
        let symbol = self.symbol(path);
        self.find_module(self.module, "(root)", &|declarations| {
            declarations.symbols.contains(&symbol)
        })
        .unwrap_or_else(|| panic!("no module declares `{path}`"))
    }

    /// The path of the module that declares the type a path names, like
    /// [`symbol_origin`](Self::symbol_origin).
    pub fn type_origin(&self, path: &str) -> String {
        let ty = self.ty(path);
        self.find_module(self.module, "(root)", &|declarations| {
            declarations.types.iter().any(|(other, _)| *other == ty)
        })
        .unwrap_or_else(|| panic!("no module declares type `{path}`"))
    }

    /// The path of the first module, in this one and the ones nested inside of it, whose
    /// declarations match.
    fn find_module(
        &self,
        module: SymbolId,
        path: &str,
        matches: &impl Fn(&ModuleDeclarations) -> bool,
    ) -> Option<String> {
        let declarations = &self.ctx.module(module).declarations;

        if matches(declarations) {
            return Some(path.to_string());
        }

        declarations.modules.iter().find_map(|&child| {
            let name = self.ctx.symbol(child).name()?;
            let child_path = if module == self.module {
                name.text().to_string()
            } else {
                format!("{path}::{}", name.text())
            };
            self.find_module(child, &child_path, matches)
        })
    }

    /// The type that a path like `inner::Point` names, looked up like [`symbol`](Self::symbol).
    pub fn ty(&self, path: &str) -> TypeId {
        let (module, name) = match path.rsplit_once("::") {
            Some((module, name)) => (self.symbol(module), name),
            None => (self.module, path),
        };
        let scope = self.ctx.module(module).scope;
        self.ctx
            .scope(scope)
            .ty(name)
            .unwrap_or_else(|| panic!("type `{path}` isn't declared"))
    }

    /// The name of the type of the symbol that a path names, such as `fn(value: Int) -> Int`.
    pub fn symbol_type_name(&mut self, path: &str) -> String {
        let symbol = self.symbol(path);
        let ty = self.ctx.symbol_type(symbol);
        self.ctx.type_name(ty)
    }

    /// The body of the function that a path names.
    pub fn function_body(&self, path: &str) -> HirId {
        match self.ctx.symbol(self.symbol(path)) {
            Symbol::Function(function) => function.body,
            symbol => panic!("`{path}` isn't a function: {symbol:?}"),
        }
    }

    /// The type of each reference to a symbol named `name` in the body of the function that
    /// `function` names, in source order, as `` `text`: Type `` pairs. This includes references
    /// in the values of `let` bindings. Narrowing changes the type of a reference, so this shows
    /// what a value was narrowed to at each place it's used.
    pub fn reference_types(&mut self, function: &str, name: &str) -> Vec<String> {
        let body = self.function_body(function);

        let mut references = Vec::new();
        self.collect_references(body, name, &mut references);

        let mut references = references
            .into_iter()
            .map(|hir| {
                let srcloc = self
                    .ctx
                    .hir_srcloc(hir)
                    .expect("references have a source location")
                    .clone();
                let ty = self.ctx.hir_type(hir).expect("references have a type");
                (srcloc, ty)
            })
            .collect::<Vec<_>>();

        references.sort_by_key(|(srcloc, _)| srcloc.span.start);

        references
            .into_iter()
            .map(|(srcloc, ty)| {
                let text = &srcloc.source.text[srcloc.span.clone()];
                format!("`{text}`: {}", self.ctx.type_name(ty))
            })
            .collect()
    }

    fn collect_references(&self, hir: HirId, name: &str, references: &mut Vec<HirId>) {
        match self.ctx.hir(hir) {
            Hir::Reference(symbol) => {
                if self
                    .ctx
                    .symbol(*symbol)
                    .name()
                    .is_some_and(|symbol_name| symbol_name.text() == name)
                {
                    references.push(hir);
                }
            }
            Hir::Block(block) => {
                for statement in &block.statements {
                    if let Statement::Let(symbol, _) = statement
                        && let Symbol::Binding(binding) = self.ctx.symbol(*symbol)
                    {
                        self.collect_references(binding.value.hir, name, references);
                    }
                }
            }
            _ => {}
        }

        for child in self.ctx.hir(hir).children().collect::<Vec<_>>() {
            self.collect_references(child, name, references);
        }
    }

    /// The value of each const expression, in source order, as `` `text` = value `` pairs. Atoms
    /// are shown as signed integers (the empty atom as `()`), and pairs as `(first . rest)`. A
    /// const expression that wasn't evaluated, because the source has errors, is shown as
    /// `unevaluated`, and one that failed to evaluate as `()`, since that's what it's replaced
    /// with.
    pub fn const_values(&self) -> Vec<String> {
        let mut values = self
            .ctx
            .const_exprs()
            .map(|hir| {
                let Hir::Const(expr) = self.ctx.hir(hir) else {
                    unreachable!("const expressions are never replaced");
                };
                let value = expr
                    .result
                    .map_or_else(|| "unevaluated".to_string(), |result| self.value(result));
                (expr.loc.clone(), value)
            })
            .collect::<Vec<_>>();

        values.sort_by_key(|(srcloc, _)| srcloc.span.start);

        values
            .into_iter()
            .map(|(srcloc, value)| {
                let text = srcloc.source.text[srcloc.span.clone()]
                    .split_whitespace()
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("`{text}` = {value}")
            })
            .collect()
    }

    fn value(&self, hir: HirId) -> String {
        match self.ctx.hir(hir) {
            Hir::Bytes(bytes) if bytes.is_empty() => "()".to_string(),
            Hir::Bytes(bytes) => BigInt::from_signed_bytes_be(bytes).to_string(),
            Hir::Nil => "()".to_string(),
            Hir::Pair(first, rest) => format!("({} . {})", self.value(*first), self.value(*rest)),
            hir => panic!("unexpected const result {hir:?}"),
        }
    }
}
