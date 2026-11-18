use std::{
    cmp::Reverse,
    collections::{HashMap, HashSet},
    fmt, mem,
    ops::{Deref, DerefMut, Range},
    path::{Path, PathBuf},
    rc::Rc,
    sync::Arc,
};

use rowan::{TextRange, TextSize};
use rue_ast::{AstDocument, AstNode};
use rue_diagnostic::{
    Diagnostic, DiagnosticKind, DiagnosticSeverity, Name, Source, SourceKind, SrcLoc,
};
use rue_hir::{
    Builtins, Constraint, Database, Declaration, ImportId, ImportUser, Item, Items, Mappings,
    Namespace, Scope, ScopeId, Symbol, SymbolId, TypePath, Value, merge_mappings, replace_type,
};
use rue_lint::{EnclosedPreludeReference, LintInput, LintSites};
use rue_options::CompilerOptions;
use rue_parser::{SyntaxKind, SyntaxNode, SyntaxToken};
use rue_types::{CallSolver, Check, CheckError, Comparison, Inference, TooComplex, Type, TypeId};

use crate::{FileTree, SyntaxItem, SyntaxItemKind, SyntaxMap};

#[derive(Debug, Clone)]
pub struct Compiler {
    options: CompilerOptions,
    source: Source,
    diagnostics: Vec<Diagnostic>,
    db: Database,
    syntax_map: SyntaxMap,
    scope_stack: Vec<(TextSize, ScopeId)>,
    /// The directory modules that the file being compiled is in.
    module_stack: Vec<SymbolId>,
    /// The module of the file being compiled, and the scope outside of it (with the prelude), if
    /// a file is being compiled.
    file: Option<(SymbolId, ScopeId)>,
    /// The inline modules that the code being compiled is in, in the file being compiled.
    inline_modules: Vec<SymbolId>,
    /// The scope with the sibling modules of each file, keyed by the scope of the file.
    sibling_scopes: HashMap<ScopeId, ScopeId>,
    builtins: Builtins,
    field_types: HashMap<TypeId, Vec<TypeId>>,
    inferred_fields: HashSet<(TypeId, String)>,
    declaration_stack: Vec<Declaration>,
    registered_scopes: HashSet<ScopeId>,
    external_programs: HashMap<PathBuf, Vec<u8>>,
    return_stack: Vec<(Option<ReturnTarget>, usize)>,
    return_count: usize,
    /// The calls whose generics are being inferred from their arguments, innermost last.
    call_frames: Vec<CallFrame>,
    lint_sites: LintSites,
    ambiguous_imports: HashSet<(ImportId, Declaration)>,
    /// The symbols whose type is overridden in any scope, so that looking up the type of any other
    /// symbol doesn't need to walk every enclosing scope (and each `let` statement opens one).
    overridden_symbols: HashSet<SymbolId>,
    /// The type items compiled since their contractiveness was last rechecked, which happens once
    /// every type item in a tree has been compiled (see `recheck_contractive_types`).
    unchecked_type_items: Vec<TypeId>,
    cancel: Option<Cancel>,
    cancelled: bool,
}

/// Says whether a compile should stop partway, because nothing needs its result anymore (for
/// example, because the language server's files were edited while it ran). It's asked between
/// phases and before each declaration's body is compiled, so it should be cheap.
#[derive(Clone)]
pub struct Cancel(Arc<dyn Fn() -> bool + Send + Sync>);

impl Cancel {
    pub fn new(cancelled: impl Fn() -> bool + Send + Sync + 'static) -> Self {
        Self(Arc::new(cancelled))
    }
}

impl fmt::Debug for Cancel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Cancel")
    }
}

/// A call whose generics are being inferred from its arguments.
#[derive(Debug, Clone)]
struct CallFrame {
    generics: Vec<TypeId>,
    /// The argument being compiled, if any.
    argument: Option<ArgumentFrame>,
}

/// An argument of a call being compiled, while the call's generics are inferred. The elements of
/// a pair or list in it add what they infer to `infer`, so that a later element with a lambda in
/// it, or a call with no arguments, can be compiled against it (#350).
#[derive(Debug, Clone)]
pub struct ArgumentFrame {
    /// The candidates of the arguments compiled before this one, and of this one's elements so far.
    pub infer: Inference,
    pub solver: Rc<CallSolver>,
}

/// The function or lambda body that a `return` statement exits from.
#[derive(Debug, Clone)]
pub struct ReturnTarget {
    /// The declared return type, if there is one.
    pub expected_type: Option<TypeId>,
    /// The types of every `return` statement compiled for this target so far.
    pub types: Vec<TypeId>,
}

impl Deref for Compiler {
    type Target = Database;

    fn deref(&self) -> &Self::Target {
        &self.db
    }
}

impl DerefMut for Compiler {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.db
    }
}

impl Compiler {
    pub fn new(options: CompilerOptions) -> Self {
        let mut db = Database::new();

        let builtins = Builtins::new(&mut db);

        let mut ctx = Self {
            options,
            source: Source::new(Arc::from(""), SourceKind::Std("<placeholder>".to_string())),
            diagnostics: Vec::new(),
            db,
            syntax_map: SyntaxMap::new(),
            scope_stack: vec![(TextSize::from(0), builtins.scope)],
            module_stack: Vec::new(),
            file: None,
            inline_modules: Vec::new(),
            sibling_scopes: HashMap::new(),
            builtins,
            field_types: HashMap::new(),
            inferred_fields: HashSet::new(),
            declaration_stack: Vec::new(),
            registered_scopes: HashSet::new(),
            external_programs: HashMap::new(),
            return_stack: Vec::new(),
            return_count: 0,
            call_frames: Vec::new(),
            lint_sites: LintSites::default(),
            ambiguous_imports: HashSet::new(),
            overridden_symbols: HashSet::new(),
            unchecked_type_items: Vec::new(),
            cancel: None,
            cancelled: false,
        };

        if options.std {
            let tree = FileTree::load_std(&mut ctx).unwrap();
            tree.compile_impl(&mut ctx, false);
            let prelude_file = tree
                .find(&SourceKind::Std("prelude.rue".to_string()))
                .unwrap();
            let prelude_scope = ctx.module(prelude_file.module).scope;

            let prelude = ctx.alloc_child_scope();

            for namespace in Namespace::ALL {
                for (name, item) in ctx.scope(prelude_scope).exported_items(namespace) {
                    ctx.scope_mut(prelude).insert_item(name, item, false);
                }
            }

            ctx.push_scope(prelude, prelude_file.document.syntax().text_range().start());
        }

        ctx
    }

    pub fn source(&self) -> &Source {
        &self.source
    }

    pub fn options(&self) -> &CompilerOptions {
        &self.options
    }

    pub(crate) fn external_program(&self, path: &Path) -> Option<&[u8]> {
        self.external_programs.get(path).map(Vec::as_slice)
    }

    pub(crate) fn cache_external_program(&mut self, path: PathBuf, program: Vec<u8>) {
        self.external_programs.insert(path, program);
    }

    /// Sets what's asked whether to stop compiling partway. A compile that stopped is incomplete,
    /// and only good for being thrown away.
    pub fn set_cancel(&mut self, cancel: Option<Cancel>) {
        self.cancel = cancel;
    }

    /// Whether the compile should stop. Once it should, it stays that way, so that every phase
    /// after it stops too.
    pub fn is_cancelled(&mut self) -> bool {
        if !self.cancelled
            && let Some(cancel) = &self.cancel
        {
            self.cancelled = (cancel.0)();
        }

        self.cancelled
    }

    pub fn set_source(&mut self, source: Source) {
        self.source = source;
    }

    pub fn scope_stack(&self) -> Vec<ScopeId> {
        self.scope_stack.iter().map(|(_, scope)| *scope).collect()
    }

    pub fn syntax_map(&self) -> &SyntaxMap {
        &self.syntax_map
    }

    pub fn add_syntax(&mut self, kind: SyntaxItemKind, span: TextRange) {
        self.add_syntax_for_source(kind, span, self.source.kind.clone());
    }

    pub fn add_syntax_for_source(
        &mut self,
        kind: SyntaxItemKind,
        span: TextRange,
        source_kind: SourceKind,
    ) {
        self.syntax_map
            .add_item(SyntaxItem::new(kind, span, source_kind));
    }

    /// Takes the diagnostics reported so far, in the order they appear in each file. Files are in
    /// the order their first diagnostic was reported.
    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = mem::take(&mut self.diagnostics);

        let mut files = HashMap::new();

        for diagnostic in &diagnostics {
            let next = files.len();
            files
                .entry(diagnostic.srcloc.source.kind.clone())
                .or_insert(next);
        }

        // Lints are ordered by where they're reported and then by lint, so that the order doesn't
        // depend on which pass reports them. Other diagnostics come before lints at the same
        // location, and the sort is stable, so they stay in the order they were reported.
        diagnostics.sort_by_cached_key(|diagnostic| {
            (
                files[&diagnostic.srcloc.source.kind],
                diagnostic.srcloc.span.start,
                diagnostic.srcloc.span.end,
                diagnostic.lint(),
            )
        });

        diagnostics
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|diagnostic| diagnostic.severity == DiagnosticSeverity::Error)
    }

    pub fn builtins(&self) -> &Builtins {
        &self.builtins
    }

    pub fn diagnostic(&mut self, node: &impl GetTextRange, kind: DiagnosticKind) {
        let srcloc = self.srcloc(node);
        self.diagnostic_at(srcloc, kind);
    }

    pub(crate) fn push_unchecked_type_item(&mut self, ty: TypeId) {
        self.unchecked_type_items.push(ty);
    }

    pub(crate) fn take_unchecked_type_items(&mut self) -> Vec<TypeId> {
        std::mem::take(&mut self.unchecked_type_items)
    }

    pub fn diagnostic_name(&mut self, name: &Name, kind: DiagnosticKind) {
        if let Some(srcloc) = name.srcloc().cloned() {
            self.diagnostic_at(srcloc, kind);
        }
    }

    /// Reports an error. Lints are reported separately, by the passes in [`rue_lint`].
    pub fn diagnostic_at(&mut self, srcloc: SrcLoc, kind: DiagnosticKind) {
        debug_assert!(
            kind.lint().is_none(),
            "lints must be reported by a lint pass, so that their level is respected"
        );

        self.diagnostics
            .push(Diagnostic::new(srcloc, kind, DiagnosticSeverity::Error));
    }

    pub fn srcloc(&self, node: &impl GetTextRange) -> SrcLoc {
        let range = node.text_range();
        let span: Range<usize> = range.start().into()..range.end().into();
        SrcLoc::new(self.source.clone(), span)
    }

    /// The location of a node without any whitespace or comments at its start or end, which the
    /// parser can include in it. This is exactly the code that was written, so it can be replaced
    /// by a suggestion.
    pub fn trimmed_srcloc(&self, node: &SyntaxNode) -> SrcLoc {
        self.srcloc(&trimmed_range(node))
    }

    pub fn local_name(&self, token: &SyntaxToken) -> Name {
        let srcloc = self.srcloc(token);
        Name::new(token.text().to_string(), Some(srcloc))
    }

    pub fn extend_diagnostics(&mut self, diagnostics: Vec<Diagnostic>) {
        self.diagnostics.extend(diagnostics);
    }

    pub fn alloc_child_scope(&mut self) -> ScopeId {
        let parent_scope = self.last_scope_id();
        self.alloc_scope(Scope::new(Some(parent_scope)))
    }

    pub fn push_scope(&mut self, scope: ScopeId, start: TextSize) {
        self.scope_stack.push((start, scope));
    }

    pub fn pop_scope(&mut self, end: TextSize) {
        let (start, scope) = self.scope_stack.pop().unwrap();

        if !self.registered_scopes.insert(scope) {
            return;
        }

        self.add_syntax(SyntaxItemKind::Scope(scope), TextRange::new(start, end));
    }

    pub fn last_scope(&self) -> &Scope {
        let scope = *self.scope_stack.last().unwrap();
        self.scope(scope.1)
    }

    pub fn last_scope_mut(&mut self) -> &mut Scope {
        let scope = *self.scope_stack.last().unwrap();
        self.scope_mut(scope.1)
    }

    pub fn last_scope_id(&self) -> ScopeId {
        self.scope_stack.last().unwrap().1
    }

    pub fn push_module(&mut self, module: SymbolId) {
        self.module_stack.push(module);
    }

    pub fn pop_module(&mut self) {
        self.module_stack.pop().unwrap();
    }

    /// The modules that `super` refers to from the code being compiled, from the outermost to the
    /// innermost, so that each `super` pops one. In an inline module, `super` is the module it's
    /// in. In a file, it's the parent of the file's directory, since the file can already refer to
    /// the other files in its directory by name.
    pub fn parent_module_stack(&self) -> Vec<SymbolId> {
        let mut stack = self.module_stack[..self.module_stack.len().saturating_sub(1)].to_vec();

        if let Some((_, parents)) = self.inline_modules.split_last() {
            stack.extend(self.file.map(|(module, _)| module));
            stack.extend_from_slice(parents);
        }

        stack
    }

    /// Starts compiling a file, whose module is in the given scope.
    pub fn enter_file(&mut self, module: SymbolId, outer_scope: ScopeId) {
        self.file = Some((module, outer_scope));
    }

    pub fn exit_file(&mut self) {
        self.file = None;
    }

    pub fn push_inline_module(&mut self, module: SymbolId) {
        self.inline_modules.push(module);
    }

    pub fn pop_inline_module(&mut self) {
        self.inline_modules.pop().unwrap();
    }

    /// The module that the code being compiled is in, inline or a file.
    pub fn current_module(&self) -> SymbolId {
        self.inline_modules.last().copied().unwrap_or_else(|| {
            let (module, _) = self.file.expect("code is compiled in a file");
            module
        })
    }

    /// The scope that an inline module's scope is in. An inline module only sees its own items and
    /// imports, and the prelude, like a file, so its scope is in the one outside of the file.
    pub fn inline_module_parent_scope(&self) -> ScopeId {
        let (_, outer_scope) = self.file.expect("inline modules are declared in a file");
        outer_scope
    }

    /// Records a lint site if a name written in an inline module (as the start of a path) resolves
    /// outside of the file, to the prelude or a builtin, while a module that encloses it has a
    /// different item with the same name in the namespace. Before inline modules stopped seeing the
    /// items of the modules they're in, the name meant the enclosing module's item.
    pub fn record_enclosed_prelude_reference(&mut self, namespace: Namespace, name: &SyntaxToken) {
        if self.inline_modules.is_empty() || !self.source.kind.check_unused() {
            return;
        }

        // The nearest enclosing module with the name, and how many `super`s name it. This is the
        // same stack that `super` pops, from the innermost module, and only the inline modules
        // and the file (not the file's directories) enclose the code.
        let stack = self.parent_module_stack();
        let Some((depth, enclosing, import)) = stack
            .iter()
            .rev()
            .take(self.inline_modules.len())
            .enumerate()
            .find_map(|(index, &module)| {
                let scope = self.module(module).scope;

                if self.scope(scope).is_blocked(namespace, name.text()) {
                    return Some(None);
                }

                let (item, import) = self.module_member(scope, namespace, name.text())?;
                Some(Some((index + 1, item, import)))
            })
            .flatten()
        else {
            return;
        };

        // Whether the name resolves in the scope outside of the inline module or one enclosing it,
        // rather than in the module or the code in it, and whether that's the builtins
        let outer_scope = self.inline_module_parent_scope();
        let mut outside = false;

        let Some((resolved, scope)) = self
            .lookup_in(self.last_scope_id(), |id, scope| {
                outside |= id == outer_scope;

                if let Some(item) = scope.item(namespace, name.text()) {
                    Some(Some((item, id)))
                } else {
                    scope.is_blocked(namespace, name.text()).then_some(None)
                }
            })
            .flatten()
        else {
            return;
        };

        // An enclosing module with the same item (such as by importing it from the prelude) means
        // the same thing
        if !outside || resolved == enclosing {
            return;
        }

        let srcloc = match import {
            Some(import) => match &self.import(import).items {
                Items::Named(name) | Items::All(name) => name.srcloc().cloned(),
            },
            None => self
                .declaration_name(enclosing.into())
                .and_then(|name| name.srcloc().cloned()),
        };

        let reference = EnclosedPreludeReference {
            name: self.local_name(name),
            builtin: scope == self.builtins.scope,
            depth,
            enclosing: srcloc,
            imported: import.is_some(),
            field_shorthand: name
                .parent()
                .is_some_and(|parent| parent.kind() == SyntaxKind::StructInitializerField),
        };

        self.lint_sites.enclosed_prelude_references.push(reference);
    }

    /// Records the scope with a file's sibling modules, which are found through `super` from an
    /// inline module in the file like the file's own items.
    pub fn add_sibling_scope(&mut self, file_scope: ScopeId, sibling_scope: ScopeId) {
        self.sibling_scopes.insert(file_scope, sibling_scope);
    }

    /// The item with a name that a module (by its scope) has, including a file's sibling modules,
    /// and the import that brought it into the module, if any. This is what `super::name` finds.
    pub fn module_member(
        &self,
        scope: ScopeId,
        namespace: Namespace,
        name: &str,
    ) -> Option<(Item, Option<ImportId>)> {
        self.scope(scope)
            .item_with_import(namespace, name)
            .or_else(|| self.sibling_item(scope, namespace, name))
    }

    /// The sibling module with a name, if a scope is a file's and the name is in a namespace that
    /// modules are in. A name the file blocks (see [`Scope::block`]) hides the sibling, as it does
    /// for lookups in the file itself.
    pub fn sibling_item(
        &self,
        scope: ScopeId,
        namespace: Namespace,
        name: &str,
    ) -> Option<(Item, Option<ImportId>)> {
        if self.scope(scope).is_blocked(namespace, name) {
            return None;
        }

        let siblings = self.sibling_scopes.get(&scope)?;
        self.scope(*siblings).item_with_import(namespace, name)
    }

    /// The names and items that a module (by its scope) has in a namespace, including a file's
    /// sibling modules. This is what `super::name` can name.
    pub fn module_members(&self, scope: ScopeId, namespace: Namespace) -> Vec<(String, Item)> {
        let mut items = self.scope(scope).items(namespace);
        items.extend(self.sibling_items(scope, namespace));
        items
    }

    /// The names and items of a file's sibling modules in a namespace, if a scope is a file's,
    /// except those the file blocks (see [`Self::sibling_item`]).
    pub fn sibling_items(&self, scope: ScopeId, namespace: Namespace) -> Vec<(String, Item)> {
        let Some(&siblings) = self.sibling_scopes.get(&scope) else {
            return Vec::new();
        };

        let mut items = self.scope(siblings).items(namespace);
        items.retain(|(name, _)| !self.scope(scope).is_blocked(namespace, name));
        items
    }

    /// Looks up a name in one namespace in a scope and then each enclosing scope. The lookup
    /// stops at a scope that blocks the name (see [`Scope::block`]).
    pub fn resolve_item_in(
        &self,
        scope: ScopeId,
        namespace: Namespace,
        name: &str,
    ) -> Option<(Item, Option<ImportId>)> {
        self.lookup_in(scope, |_, scope| {
            if let Some(found) = scope.item_with_import(namespace, name) {
                Some(Some(found))
            } else {
                scope.is_blocked(namespace, name).then_some(None)
            }
        })
        .flatten()
    }

    /// Whether looking up a name in a namespace from a scope stops at a scope that blocks the
    /// name, rather than finding it or reaching the outermost scope.
    pub fn is_blocked_in(&self, scope: ScopeId, namespace: Namespace, name: &str) -> bool {
        self.block_origin_in(scope, namespace, name).is_some()
    }

    /// The origin of the block that looking up a name in a namespace from a scope stops at, if it
    /// stops at a scope that blocks the name (see [`Self::is_blocked_in`] and [`Scope::block`]).
    pub fn block_origin_in(
        &self,
        scope: ScopeId,
        namespace: Namespace,
        name: &str,
    ) -> Option<ImportId> {
        self.lookup_in(scope, |_, scope| {
            if scope.item(namespace, name).is_some() {
                Some(None)
            } else {
                scope.block_origin(namespace, name).map(Some)
            }
        })
        .flatten()
    }

    /// The first result of `find` in a scope and then each enclosing scope.
    pub(crate) fn lookup_in<T>(
        &self,
        scope: ScopeId,
        mut find: impl FnMut(ScopeId, &Scope) -> Option<T>,
    ) -> Option<T> {
        let mut current = Some(scope);

        while let Some(scope) = current {
            if let Some(found) = find(scope, self.scope(scope)) {
                return Some(found);
            }
            current = self.scope(scope).parent();
        }

        None
    }

    pub fn type_name(&mut self, ty: TypeId) -> String {
        let current = self.last_scope_id();
        self.type_name_in(current, ty)
    }

    pub fn type_name_in(&mut self, scope: ScopeId, ty: TypeId) -> String {
        self.db.type_name_in(scope, ty)
    }

    pub fn symbol_name(&self, symbol: SymbolId) -> String {
        let mut current = Some(self.last_scope_id());

        while let Some(scope) = current {
            if let Some(name) = self.scope(scope).symbol_name(symbol) {
                return name.to_string();
            }
            current = self.scope(scope).parent();
        }

        self.symbol(symbol)
            .name()
            .map_or_else(|| "{unknown}".to_string(), |name| name.text().to_string())
    }

    pub fn symbol_type(&self, symbol: SymbolId) -> TypeId {
        let current = self.last_scope_id();
        self.symbol_type_in(current, symbol)
    }

    pub fn symbol_type_in(&self, scope: ScopeId, symbol: SymbolId) -> TypeId {
        let mut current = self.overridden_symbols.contains(&symbol).then_some(scope);

        while let Some(scope) = current {
            if let Some(ty) = self.scope(scope).symbol_override_type(symbol) {
                return ty;
            }
            current = self.scope(scope).parent();
        }

        match self.symbol(symbol) {
            Symbol::Unresolved | Symbol::Module(_) | Symbol::Builtin(_) => {
                self.builtins().unresolved.ty
            }
            Symbol::Function(function) => function.ty,
            Symbol::Parameter(parameter) => parameter.ty,
            Symbol::Constant(constant) => constant.value.ty,
            Symbol::Binding(binding) => binding.value.ty,
        }
    }

    pub fn push_mappings(
        &mut self,
        mappings: HashMap<SymbolId, HashMap<Vec<TypePath>, TypeId>>,
        start: TextSize,
    ) -> usize {
        let scope = self.alloc_child_scope();

        let builtins = self.builtins.types;

        for (symbol, paths) in mappings {
            let mut ty = self.symbol_type(symbol);

            let mut paths = paths.into_iter().collect::<Vec<_>>();
            paths.sort_by_key(|(path, _)| (Reverse(path.len()), path.last().copied()));

            for (path, replacement) in paths {
                ty = replace_type(&mut self.db, &builtins, ty, replacement, &path);
            }

            self.scope_mut(scope).override_symbol_type(symbol, ty);
            self.overridden_symbols.insert(symbol);
        }

        let index = self.scope_stack.len();
        self.push_scope(scope, start);
        index
    }

    pub fn merge_mappings(&mut self, a: &Mappings, b: &Mappings) -> Mappings {
        let builtins = self.builtins.types;
        merge_mappings(&mut self.db, &builtins, a, b)
    }

    /// Enters a context where `return` statements exit to the given target. If the target
    /// is `None`, then `return` statements are not allowed (such as in expression blocks).
    pub fn push_return_target(&mut self, target: Option<ReturnTarget>) {
        self.return_stack.push((target, self.return_count));
    }

    /// Leaves the current return context. The return count is restored, since `return`
    /// statements inside of it can't exit any of the blocks that enclose it.
    pub fn pop_return_target(&mut self) -> Option<ReturnTarget> {
        let (target, return_count) = self
            .return_stack
            .pop()
            .expect("return target stack is unbalanced");
        self.return_count = return_count;
        target
    }

    /// Enters the arguments of a call whose generics are being inferred from them.
    pub fn push_call_frame(&mut self, generics: Vec<TypeId>) {
        self.call_frames.push(CallFrame {
            generics,
            argument: None,
        });
    }

    pub fn pop_call_frame(&mut self) {
        self.call_frames
            .pop()
            .expect("call frame stack is unbalanced");
    }

    /// The generics of the enclosing calls that are still being inferred.
    pub fn inferred_generics(&self) -> Vec<TypeId> {
        self.call_frames
            .iter()
            .flat_map(|frame| frame.generics.iter().copied())
            .collect()
    }

    /// The generics of the innermost call being inferred, if there is one.
    pub fn call_generics(&self) -> Option<&[TypeId]> {
        self.call_frames
            .last()
            .map(|frame| frame.generics.as_slice())
    }

    /// Starts compiling an argument of the innermost call being inferred (or stops, with `None`).
    pub fn set_argument_frame(&mut self, argument: Option<ArgumentFrame>) {
        self.call_frames
            .last_mut()
            .expect("an argument is compiled inside a call")
            .argument = argument;
    }

    /// Takes the argument of the innermost call being inferred that's being compiled, if there is
    /// one, to be put back with `Compiler::set_argument_frame`.
    pub fn take_argument_frame(&mut self) -> Option<ArgumentFrame> {
        self.call_frames.last_mut()?.argument.take()
    }

    pub fn return_target_mut(&mut self) -> Option<&mut ReturnTarget> {
        self.return_stack
            .last_mut()
            .and_then(|(target, _)| target.as_mut())
    }

    /// The number of `return` statements compiled so far that could exit the blocks in
    /// the current return context, which is used to tell whether a block may have exited
    /// early.
    pub fn return_count(&self) -> usize {
        self.return_count
    }

    pub fn increment_return_count(&mut self) {
        self.return_count += 1;
    }

    /// Records the type, location and scope of a compiled expression in the database, so that
    /// they're available after compilation (such as to lints). The builtin values (such as `nil`) share
    /// a HIR node, so the value is given a node of its own first, which is returned.
    pub fn record_value(&mut self, node: &SyntaxNode, mut value: Value) -> Value {
        if self.builtins.is_shared_hir(value.hir) {
            let hir = self.db.hir(value.hir).clone();
            value = value.with_hir(self.db.alloc_hir(hir));
        }

        let srcloc = self.trimmed_srcloc(node);
        self.db.set_hir_type(value.hir, value.ty);
        self.db.set_hir_srcloc(value.hir, srcloc);
        self.db.set_hir_scope(value.hir, self.last_scope_id());

        value
    }

    /// The sites recorded for lints while compiling, which are checked by [`rue_lint`] once
    /// everything has been compiled.
    pub fn lint_sites_mut(&mut self) -> &mut LintSites {
        &mut self.lint_sites
    }

    /// Runs the lint passes over the sites recorded so far and the given documents.
    pub fn run_lints(
        &mut self,
        documents: Vec<(&Source, &AstDocument)>,
        entrypoints: &HashSet<Declaration>,
    ) {
        let input = LintInput::new(
            mem::take(&mut self.lint_sites),
            documents,
            entrypoints,
            self.has_errors(),
        );

        let diagnostics = rue_lint::run_lints(
            &mut self.db,
            &self.builtins.types,
            self.options.lints,
            &input,
        );

        self.diagnostics.extend(diagnostics);
    }

    /// Discards the sites recorded so far without linting them, such as for the standard library.
    pub fn discard_lint_sites(&mut self) {
        self.lint_sites = LintSites::default();
    }

    pub fn mapping_checkpoint(&self) -> usize {
        self.scope_stack.len()
    }

    pub fn revert_mappings(&mut self, index: usize, end: TextSize) {
        while self.scope_stack.len() > index {
            self.pop_scope(end);
        }
    }

    pub fn is_assignable(&mut self, from: TypeId, to: TypeId) -> bool {
        let comparison = rue_types::compare(self.db.types_mut(), &self.builtins.types, from, to);
        comparison == Comparison::Assign
    }

    /// Like `is_assignable`, but returns `TooComplex` if comparing the types took too many steps.
    pub fn try_is_assignable(&mut self, from: TypeId, to: TypeId) -> Result<bool, TooComplex> {
        let comparison =
            rue_types::compare_limited(self.db.types_mut(), &self.builtins.types, from, to)?;
        Ok(comparison == Comparison::Assign)
    }

    /// Reports that comparing the types took too many steps (see `rue_types::compare_limited`).
    pub fn too_complex(&mut self, node: &impl GetTextRange, from: TypeId, to: TypeId) {
        let from = self.type_name(from);
        let to = self.type_name(to);
        self.diagnostic(node, DiagnosticKind::TypeComparisonTooComplex(from, to));
    }

    pub fn is_castable(&mut self, from: TypeId, to: TypeId) -> bool {
        let comparison = rue_types::compare(self.db.types_mut(), &self.builtins.types, from, to);
        matches!(comparison, Comparison::Assign | Comparison::Cast)
    }

    pub fn assign_type(&mut self, node: &impl GetTextRange, from: TypeId, to: TypeId) {
        self.compare_type(node, from, to, false);
    }

    pub fn cast_type(&mut self, node: &impl GetTextRange, from: TypeId, to: TypeId) {
        self.compare_type(node, from, to, true);
    }

    pub fn guard_type(&mut self, node: &impl GetTextRange, from: TypeId, to: TypeId) -> Constraint {
        let check = match rue_types::check(self.db.types_mut(), &self.builtins.types, from, to) {
            Ok(check) => check,
            Err(CheckError::DepthExceeded) => {
                self.diagnostic(node, DiagnosticKind::TypeCheckDepthExceeded);
                return Constraint::new(Check::Impossible);
            }
            Err(CheckError::RecursiveType) => {
                self.diagnostic(node, DiagnosticKind::RecursiveTypeCheck);
                return Constraint::new(Check::Impossible);
            }
            Err(CheckError::FunctionType) => {
                self.diagnostic(node, DiagnosticKind::FunctionTypeCheck);
                return Constraint::new(Check::Impossible);
            }
            Err(CheckError::GenericType) => {
                self.diagnostic(node, DiagnosticKind::GenericTypeCheck);
                return Constraint::new(Check::Impossible);
            }
            Err(CheckError::TooComplex) => {
                self.too_complex(node, from, to);
                return Constraint::new(Check::Impossible);
            }
        };

        if check == Check::Impossible {
            let from_name = self.type_name(from);
            let to_name = self.type_name(to);
            self.diagnostic(node, DiagnosticKind::IncompatibleGuard(from_name, to_name));
        }

        let else_id = rue_types::subtract(self.db.types_mut(), &self.builtins.types, from, to);

        Constraint::new(check).with_else(else_id)
    }

    pub fn check_condition(&mut self, node: &impl GetTextRange, ty: TypeId) {
        // An error has already been reported, and the type is compatible with everything
        if self.is_unresolved(ty) {
            return;
        }

        // Conditions that are always `true` or `false` are reported by the `constant_condition`
        // lint, but they still need to be a `Bool` regardless of whether that lint is enabled
        self.assign_type(node, ty, self.builtins().types.bool);
    }

    /// Collects inference candidates for the generics in `to`, without reporting diagnostics.
    /// The value must be checked against the substituted type afterward.
    pub fn infer_type(&mut self, from: TypeId, to: TypeId, infer: &mut Inference) {
        rue_types::compare_with_inference(
            self.db.types_mut(),
            &self.builtins.types,
            from,
            to,
            Some(infer),
        );
    }

    /// Collects inference candidates for the generics in `ty` from the type it's expected to be.
    pub fn infer_from_expected(&mut self, ty: TypeId, expected: TypeId, infer: &mut Inference) {
        rue_types::infer_from_expected(
            self.db.types_mut(),
            &self.builtins.types,
            ty,
            expected,
            infer,
        );
    }

    /// A solver for a call's generics, given what was inferred from the type expected of it.
    pub fn call_solver(&mut self, expected: Option<Inference>) -> CallSolver {
        CallSolver::new(self.db.types_mut(), &self.builtins.types, expected)
    }

    /// What the arguments of a call compiled so far infer on their own (see
    /// `CallSolver::solve_arguments`).
    pub fn solve_arguments(
        &mut self,
        solver: &CallSolver,
        infer: &Inference,
    ) -> HashMap<TypeId, TypeId> {
        solver.solve_arguments(self.db.types_mut(), &self.builtins.types, infer)
    }

    /// The mappings that the next argument's expected type is substituted with (see
    /// `CallSolver::argument_mappings`).
    pub fn argument_mappings(
        &mut self,
        solver: &CallSolver,
        infer: &Inference,
        inferred: &HashMap<TypeId, TypeId>,
        bound_mapped: bool,
    ) -> HashMap<TypeId, TypeId> {
        solver.argument_mappings(
            self.db.types_mut(),
            &self.builtins.types,
            infer,
            inferred,
            bound_mapped,
        )
    }

    /// The mappings a call is checked with, once its arguments have been compiled (see
    /// `CallSolver::solve`).
    pub fn solve_call(&mut self, solver: &CallSolver, infer: Inference) -> HashMap<TypeId, TypeId> {
        solver.solve(self.db.types_mut(), &self.builtins.types, infer)
    }

    fn compare_type(&mut self, node: &impl GetTextRange, from: TypeId, to: TypeId, cast: bool) {
        let Ok(comparison) =
            rue_types::compare_limited(self.db.types_mut(), &self.builtins.types, from, to)
        else {
            self.too_complex(node, from, to);
            return;
        };

        match comparison {
            Comparison::Assign => {}
            Comparison::Cast => {
                if !cast {
                    let from = self.type_name(from);
                    let to = self.type_name(to);
                    self.diagnostic(node, DiagnosticKind::UnassignableType(from, to));
                }
            }
            Comparison::Invalid => {
                let check =
                    match rue_types::check(self.db.types_mut(), &self.builtins.types, from, to) {
                        Ok(check) => check,
                        Err(
                            CheckError::DepthExceeded
                            | CheckError::RecursiveType
                            | CheckError::FunctionType
                            | CheckError::GenericType
                            | CheckError::TooComplex,
                        ) => Check::Impossible,
                    };

                let from = self.type_name(from);
                let to = self.type_name(to);

                if check != Check::Impossible {
                    self.diagnostic(node, DiagnosticKind::UnconstrainableComparison(from, to));
                } else if cast {
                    self.diagnostic(node, DiagnosticKind::IncompatibleCast(from, to));
                } else {
                    self.diagnostic(node, DiagnosticKind::IncompatibleType(from, to));
                }
            }
        }
    }

    /// Stores the types of a struct's fields in declaration order, so that their default values
    /// can be compiled against them once all types have been resolved.
    pub fn insert_field_types(&mut self, ty: TypeId, field_types: Vec<TypeId>) {
        self.field_types.insert(ty, field_types);
    }

    pub fn field_types(&self, ty: TypeId) -> Vec<TypeId> {
        self.field_types[&ty].clone()
    }

    /// Marks a struct field whose type was inferred from its default value rather than declared.
    pub fn insert_inferred_field(&mut self, ty: TypeId, name: String) {
        self.inferred_fields.insert((ty, name));
    }

    pub fn is_inferred_field(&self, ty: TypeId, name: &str) -> bool {
        self.inferred_fields.contains(&(ty, name.to_string()))
    }

    pub fn push_declaration(&mut self, declaration: Declaration) {
        if let Some(last) = self.declaration_stack.last() {
            self.db.add_declaration(*last, declaration);
        }

        self.declaration_stack.push(declaration);

        if self.source.kind.check_unused() {
            self.db.add_relevant_declaration(declaration);
        }
    }

    pub fn pop_declaration(&mut self) {
        self.declaration_stack.pop().unwrap();
    }

    pub fn reference(&mut self, reference: Declaration, import: Option<ImportId>) {
        if let Some(last) = self.declaration_stack.last() {
            self.db.add_reference(*last, reference);
        }

        if let Some(import) = import {
            let user = self
                .declaration_stack
                .last()
                .map_or(ImportUser::Other, |&last| ImportUser::Declaration(last));

            self.db.add_import_reference(import, reference, user);
        }
    }

    /// Records that a glob import brought a declaration into a scope that another glob import
    /// brings a different declaration with the same name into, or that it brought in a
    /// declaration that was already ambiguous, so using it is ambiguous. Returns whether it
    /// wasn't already recorded.
    pub fn add_ambiguous_import(&mut self, import: ImportId, declaration: Declaration) -> bool {
        self.ambiguous_imports.insert((import, declaration))
    }

    /// Whether a name that refers to a declaration through this import is ambiguous.
    pub fn is_ambiguous_import(&self, import: Option<ImportId>, declaration: Declaration) -> bool {
        import.is_some_and(|import| self.ambiguous_imports.contains(&(import, declaration)))
    }

    /// Records an import that's checked for being unused, unless it's in the standard library,
    /// as with declarations.
    pub fn add_relevant_import(&mut self, import: ImportId) {
        if self.source.kind.check_unused() {
            self.db.add_relevant_import(import);
        }
    }

    pub fn declaration_span(&mut self, declaration: Declaration, span: TextRange) {
        self.add_syntax(
            match declaration {
                Declaration::Symbol(symbol) => SyntaxItemKind::SymbolDeclaration(symbol),
                Declaration::Type(ty) => SyntaxItemKind::TypeDeclaration(ty),
            },
            span,
        );
    }

    pub fn reference_span(&mut self, reference: Declaration, span: TextRange) {
        self.add_syntax(
            match reference {
                Declaration::Symbol(symbol) => SyntaxItemKind::SymbolReference(symbol),
                Declaration::Type(ty) => SyntaxItemKind::TypeReference(ty),
            },
            span,
        );
    }

    pub fn is_unresolved(&mut self, ty: TypeId) -> bool {
        let semantic = rue_types::unwrap_semantic(self.db.types_mut(), ty, true);
        matches!(self.ty(semantic), Type::Unresolved)
    }
}

/// The range of a node without any whitespace or comments at its start or end.
pub fn trimmed_range(node: &SyntaxNode) -> TextRange {
    let range = node.text_range();

    let mut first = node.first_token();
    while let Some(token) = first.as_ref().filter(|token| token.kind().is_trivia()) {
        first = token
            .next_token()
            .filter(|next| next.text_range().end() <= range.end());
    }

    let mut last = node.last_token();

    while let Some(token) = last.as_ref().filter(|token| token.kind().is_trivia()) {
        last = token
            .prev_token()
            .filter(|prev| prev.text_range().start() >= range.start());
    }

    match (first, last) {
        (Some(first), Some(last)) if first.text_range().start() <= last.text_range().end() => {
            TextRange::new(first.text_range().start(), last.text_range().end())
        }
        _ => range,
    }
}

pub trait GetTextRange {
    fn text_range(&self) -> TextRange;
}

impl GetTextRange for TextRange {
    fn text_range(&self) -> TextRange {
        *self
    }
}

impl GetTextRange for SyntaxNode {
    fn text_range(&self) -> TextRange {
        self.text_range()
    }
}

impl GetTextRange for SyntaxToken {
    fn text_range(&self) -> TextRange {
        self.text_range()
    }
}
