use std::{
    cmp::Reverse,
    collections::{HashMap, HashSet},
    mem,
    ops::{Deref, DerefMut, Range},
    sync::Arc,
};

use rowan::{TextRange, TextSize};
use rue_ast::AstNode;
use rue_diagnostic::{Diagnostic, DiagnosticKind, Name, Source, SourceKind, SrcLoc};
use rue_hir::{
    Builtins, Constraint, Database, Declaration, ImportId, Scope, ScopeId, Symbol, SymbolId,
    TypePath, Value, replace_type,
};
use rue_options::CompilerOptions;
use rue_parser::{SyntaxNode, SyntaxToken};
use rue_types::{Check, CheckError, Comparison, Type, TypeId};

use crate::{File, FileTree, SyntaxItem, SyntaxItemKind, SyntaxMap};

#[derive(Debug, Clone)]
pub struct Compiler {
    options: CompilerOptions,
    source: Source,
    diagnostics: Vec<Diagnostic>,
    db: Database,
    syntax_maps: HashMap<SourceKind, SyntaxMap>,
    scope_stack: Vec<(TextSize, ScopeId)>,
    builtins: Builtins,
    defaults: HashMap<TypeId, HashMap<String, Value>>,
    declaration_stack: Vec<Declaration>,
    registered_scopes: HashSet<ScopeId>,
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
            source: Source::new(Arc::from(""), SourceKind::Std),
            diagnostics: Vec::new(),
            db,
            syntax_maps: HashMap::new(),
            scope_stack: vec![(TextSize::from(0), builtins.scope)],
            builtins,
            defaults: HashMap::new(),
            declaration_stack: Vec::new(),
            registered_scopes: HashSet::new(),
        };

        let std = File::std(&mut ctx);
        let tree = FileTree::File(std.clone());
        tree.compile_impl(&mut ctx, false);
        let std_scope = std.module(&ctx).scope;

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

        ctx
    }

    pub fn source(&self) -> &Source {
        &self.source
    }

    pub fn options(&self) -> &CompilerOptions {
        &self.options
    }

    pub fn set_source(&mut self, source: Source) {
        self.source = source;
    }

    pub fn scope_stack(&self) -> Vec<ScopeId> {
        self.scope_stack.iter().map(|(_, scope)| *scope).collect()
    }

    pub fn syntax_map(&self, source_kind: &SourceKind) -> Option<&SyntaxMap> {
        self.syntax_maps.get(source_kind)
    }

    pub fn syntax_map_mut(&mut self) -> &mut SyntaxMap {
        self.syntax_maps
            .entry(self.source.kind.clone())
            .or_default()
    }

    pub fn syntax_map_for_source(&mut self, source: SourceKind) -> &mut SyntaxMap {
        self.syntax_maps.entry(source).or_default()
    }

    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        mem::take(&mut self.diagnostics)
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

    pub fn diagnostic_at(&mut self, srcloc: SrcLoc, kind: DiagnosticKind) {
        self.diagnostics.push(Diagnostic::new(srcloc, kind));
    }

    pub fn srcloc(&self, node: &impl GetTextRange) -> SrcLoc {
        let range = node.text_range();
        let span: Range<usize> = range.start().into()..range.end().into();
        SrcLoc::new(self.source.clone(), span)
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

        self.syntax_map_mut().add_item(SyntaxItem::new(
            SyntaxItemKind::Scope(scope),
            TextRange::new(start, end),
        ));
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

    pub fn resolve_symbol(&self, name: &str) -> Option<(SymbolId, Option<ImportId>)> {
        let last = self.last_scope_id();
        self.resolve_symbol_in(last, name)
    }

    pub fn resolve_type(&self, name: &str) -> Option<(TypeId, Option<ImportId>)> {
        let last = self.last_scope_id();
        self.resolve_type_in(last, name)
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
        let mut current = Some(self.last_scope_id());

        while let Some(scope) = current {
            if let Some(name) = self.scope(scope).type_name(ty) {
                return name.to_string();
            }
            current = self.scope(scope).parent();
        }

        rue_types::stringify(self.db.types_mut(), ty)
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
            .to_string()
    }

    pub fn symbol_type(&self, symbol: SymbolId) -> TypeId {
        let mut current = Some(self.last_scope_id());

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

        for (symbol, paths) in mappings {
            let mut ty = self.symbol_type(symbol);

            let mut paths = paths.into_iter().collect::<Vec<_>>();
            paths.sort_by_key(|(path, _)| (Reverse(path.len()), path.last().copied()));

            for (path, replacement) in paths {
                ty = replace_type(&mut self.db, ty, replacement, &path);
            }

            self.scope_mut(scope).override_symbol_type(symbol, ty);
        }

        let index = self.scope_stack.len();
        self.push_scope(scope, start);
        index
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
        self.compare_type(node, from, to, false, None);
    }

    pub fn cast_type(&mut self, node: &impl GetTextRange, from: TypeId, to: TypeId) {
        self.compare_type(node, from, to, true, None);
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
        };

        let from_name = self.type_name(from);
        let to_name = self.type_name(to);

        if check == Check::None {
            self.diagnostic(node, DiagnosticKind::UnnecessaryGuard(from_name, to_name));
        } else if check == Check::Impossible {
            self.diagnostic(node, DiagnosticKind::IncompatibleGuard(from_name, to_name));
        }

        let else_id = rue_types::subtract(self.db.types_mut(), &self.builtins.types, from, to);

        Constraint::new(check).with_else(else_id)
    }

    pub fn check_condition(&mut self, node: &impl GetTextRange, ty: TypeId) {
        if self.is_castable(ty, self.builtins().types.bool_true) {
            self.diagnostic(node, DiagnosticKind::AlwaysTrueCondition);
        } else if self.is_castable(ty, self.builtins().types.bool_false) {
            self.diagnostic(node, DiagnosticKind::AlwaysFalseCondition);
        } else {
            self.assign_type(node, ty, self.builtins().types.bool);
        }
    }

    pub fn infer_type(
        &mut self,
        node: &impl GetTextRange,
        from: TypeId,
        to: TypeId,
        infer: &mut HashMap<TypeId, TypeId>,
    ) {
        self.compare_type(node, from, to, false, Some(infer));
    }

    fn compare_type(
        &mut self,
        node: &impl GetTextRange,
        from: TypeId,
        to: TypeId,
        cast: bool,
        infer: Option<&mut HashMap<TypeId, TypeId>>,
    ) {
        let comparison = rue_types::compare_with_inference(
            self.db.types_mut(),
            &self.builtins.types,
            from,
            to,
            infer,
        );

        match comparison {
            Comparison::Assign => {
                if cast
                    && rue_types::compare_with_inference(
                        self.db.types_mut(),
                        &self.builtins.types,
                        to,
                        from,
                        None,
                    ) == Comparison::Assign
                {
                    let from = self.type_name(from);
                    let to = self.type_name(to);

                    self.diagnostic(node, DiagnosticKind::UnnecessaryCast(from, to));
                }
            }
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
                        Err(CheckError::DepthExceeded | CheckError::FunctionType) => {
                            Check::Impossible
                        }
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

    pub fn insert_default_field(&mut self, ty: TypeId, name: String, value: Value) {
        self.defaults.entry(ty).or_default().insert(name, value);
    }

    pub fn default_field(&self, ty: TypeId, name: &str) -> Option<Value> {
        self.defaults
            .get(&ty)
            .and_then(|map| map.get(name).cloned())
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
            self.db.add_import_reference(import, reference);
        }
    }

    pub fn declaration_span(&mut self, declaration: Declaration, span: TextRange) {
        self.syntax_map_mut().add_item(SyntaxItem::new(
            match declaration {
                Declaration::Symbol(symbol) => SyntaxItemKind::SymbolDeclaration(symbol),
                Declaration::Type(ty) => SyntaxItemKind::TypeDeclaration(ty),
            },
            span,
        ));
    }

    pub fn reference_span(&mut self, reference: Declaration, span: TextRange) {
        self.syntax_map_mut().add_item(SyntaxItem::new(
            match reference {
                Declaration::Symbol(symbol) => SyntaxItemKind::SymbolReference(symbol),
                Declaration::Type(ty) => SyntaxItemKind::TypeReference(ty),
            },
            span,
        ));
    }

    pub fn is_unresolved(&mut self, ty: TypeId) -> bool {
        let semantic = rue_types::unwrap_semantic(self.db.types_mut(), ty, true);
        matches!(self.ty(semantic), Type::Unresolved)
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
