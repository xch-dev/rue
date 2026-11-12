use std::{
    cmp::Reverse,
    collections::{HashMap, HashSet},
    mem,
    ops::{Deref, DerefMut, Range},
    path::{Path, PathBuf},
    sync::Arc,
};

use rowan::{TextRange, TextSize};
use rue_ast::{AstDocument, AstNode};
use rue_diagnostic::{
    Diagnostic, DiagnosticKind, DiagnosticSeverity, Name, Source, SourceKind, SrcLoc,
};
use rue_hir::{
    Builtins, Constraint, Database, Declaration, ImportId, ImportUser, Mappings, Scope, ScopeId,
    Symbol, SymbolId, TypePath, Value, merge_mappings, replace_type,
};
use rue_lint::{LintInput, LintSites};
use rue_options::CompilerOptions;
use rue_parser::{SyntaxNode, SyntaxToken};
use rue_types::{Check, CheckError, Comparison, Inference, Type, TypeId};

use crate::{FileTree, SyntaxItem, SyntaxItemKind, SyntaxMap};

#[derive(Debug, Clone)]
pub struct Compiler {
    options: CompilerOptions,
    source: Source,
    diagnostics: Vec<Diagnostic>,
    db: Database,
    syntax_map: SyntaxMap,
    scope_stack: Vec<(TextSize, ScopeId)>,
    module_stack: Vec<SymbolId>,
    builtins: Builtins,
    field_types: HashMap<TypeId, Vec<TypeId>>,
    inferred_fields: HashSet<(TypeId, String)>,
    declaration_stack: Vec<Declaration>,
    registered_scopes: HashSet<ScopeId>,
    external_programs: HashMap<PathBuf, Vec<u8>>,
    return_stack: Vec<(Option<ReturnTarget>, usize)>,
    return_count: usize,
    inferred_generics_stack: Vec<Vec<TypeId>>,
    lint_sites: LintSites,
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
            builtins,
            field_types: HashMap::new(),
            inferred_fields: HashSet::new(),
            declaration_stack: Vec::new(),
            registered_scopes: HashSet::new(),
            external_programs: HashMap::new(),
            return_stack: Vec::new(),
            return_count: 0,
            inferred_generics_stack: Vec::new(),
            lint_sites: LintSites::default(),
        };

        if options.std {
            let tree = FileTree::load_std(&mut ctx).unwrap();
            tree.compile_impl(&mut ctx, false);
            let prelude_file = tree
                .find(&SourceKind::Std("prelude.rue".to_string()))
                .unwrap();
            let prelude_scope = ctx.module(prelude_file.module).scope;

            let prelude = ctx.alloc_child_scope();

            for (name, symbol) in ctx
                .scope(prelude_scope)
                .exported_symbols()
                .map(|(name, symbol)| (name.to_string(), symbol))
                .collect::<Vec<_>>()
            {
                ctx.scope_mut(prelude).insert_symbol(name, symbol, false);
            }

            for (name, ty) in ctx
                .scope(prelude_scope)
                .exported_types()
                .map(|(name, ty)| (name.to_string(), ty))
                .collect::<Vec<_>>()
            {
                ctx.scope_mut(prelude).insert_type(name, ty, false);
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

    pub fn parent_module_stack(&self) -> &[SymbolId] {
        if self.module_stack.is_empty() {
            return &[];
        }

        &self.module_stack[..self.module_stack.len() - 1]
    }

    pub fn resolve_symbol_in(
        &self,
        scope: ScopeId,
        name: &str,
    ) -> Option<(SymbolId, Option<ImportId>)> {
        let mut current = Some(scope);

        while let Some(scope) = current {
            if let Some(symbol) = self.scope(scope).symbol(name) {
                return Some((symbol, self.scope(scope).symbol_import(symbol)));
            }
            current = self.scope(scope).parent();
        }

        None
    }

    pub fn resolve_type_in(
        &self,
        scope: ScopeId,
        name: &str,
    ) -> Option<(TypeId, Option<ImportId>)> {
        let mut current = Some(scope);

        while let Some(scope) = current {
            if let Some(ty) = self.scope(scope).ty(name) {
                return Some((ty, self.scope(scope).type_import(ty)));
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
        let mut current = Some(scope);

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
    pub fn push_inferred_generics(&mut self, generics: Vec<TypeId>) {
        self.inferred_generics_stack.push(generics);
    }

    pub fn pop_inferred_generics(&mut self) {
        self.inferred_generics_stack
            .pop()
            .expect("inferred generics stack is unbalanced");
    }

    /// The generics of the enclosing calls that are still being inferred.
    pub fn inferred_generics(&self) -> Vec<TypeId> {
        self.inferred_generics_stack.concat()
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
            Err(CheckError::FunctionType) => {
                self.diagnostic(node, DiagnosticKind::FunctionTypeCheck);
                return Constraint::new(Check::Impossible);
            }
            Err(CheckError::GenericType) => {
                self.diagnostic(node, DiagnosticKind::GenericTypeCheck);
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

    /// Constrains the generics with what was inferred from the type expected of the call, where
    /// the arguments satisfy it.
    pub fn add_expected_bounds(&mut self, infer: &mut Inference, expected: &Inference) {
        rue_types::add_expected_bounds(self.db.types_mut(), &self.builtins.types, infer, expected);
    }

    pub fn resolve_inference(&mut self, infer: &Inference) -> HashMap<TypeId, TypeId> {
        rue_types::resolve_inference(self.db.types_mut(), &self.builtins.types, infer)
    }

    fn compare_type(&mut self, node: &impl GetTextRange, from: TypeId, to: TypeId, cast: bool) {
        let comparison = rue_types::compare(self.db.types_mut(), &self.builtins.types, from, to);

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
                            | CheckError::FunctionType
                            | CheckError::GenericType,
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
