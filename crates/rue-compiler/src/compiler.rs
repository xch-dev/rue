use std::{
    cmp::Reverse,
    collections::HashMap,
    mem,
    ops::{Deref, DerefMut, Range},
    sync::Arc,
};

use rowan::TextRange;
use rue_diagnostic::{Diagnostic, DiagnosticKind, Source, SourceKind, SrcLoc};
use rue_hir::{
    Builtins, Constraint, Database, Declaration, Scope, ScopeId, Symbol, SymbolId, TypePath, Value,
    replace_type,
};
use rue_options::CompilerOptions;
use rue_parser::{SyntaxNode, SyntaxToken};
use rue_types::{Check, CheckError, Comparison, TypeId};

#[derive(Debug, Clone)]
pub struct Compiler {
    _options: CompilerOptions,
    source: Source,
    diagnostics: Vec<Diagnostic>,
    db: Database,
    scope_stack: Vec<ScopeId>,
    mapping_stack: Vec<HashMap<SymbolId, TypeId>>,
    builtins: Builtins,
    defaults: HashMap<TypeId, HashMap<String, Value>>,
    declaration_stack: Vec<Declaration>,
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

        Self {
            _options: options,
            source: Source::new(Arc::from(""), SourceKind::Std),
            diagnostics: Vec::new(),
            db,
            scope_stack: vec![builtins.scope],
            mapping_stack: vec![],
            builtins,
            defaults: HashMap::new(),
            declaration_stack: Vec::new(),
        }
    }

    pub fn source(&self) -> &Source {
        &self.source
    }

    pub fn set_source(&mut self, source: Source) {
        self.source = source;
    }

    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        mem::take(&mut self.diagnostics)
    }

    pub fn builtins(&self) -> &Builtins {
        &self.builtins
    }

    pub fn diagnostic(&mut self, node: &impl GetTextRange, kind: DiagnosticKind) {
        let range = node.text_range();
        let span: Range<usize> = range.start().into()..range.end().into();
        self.diagnostics.push(Diagnostic::new(
            SrcLoc::new(self.source.clone(), span),
            kind,
        ));
    }

    pub fn push_scope(&mut self, scope: ScopeId) {
        self.scope_stack.push(scope);
    }

    pub fn pop_scope(&mut self) {
        self.scope_stack.pop().unwrap();
    }

    pub fn last_scope(&self) -> &Scope {
        let scope = *self.scope_stack.last().unwrap();
        self.scope(scope)
    }

    pub fn last_scope_mut(&mut self) -> &mut Scope {
        let scope = *self.scope_stack.last().unwrap();
        self.scope_mut(scope)
    }

    pub fn resolve_symbol(&self, name: &str) -> Option<SymbolId> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(symbol) = self.scope(*scope).symbol(name) {
                return Some(symbol);
            }
        }
        None
    }

    pub fn resolve_type(&self, name: &str) -> Option<TypeId> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(ty) = self.scope(*scope).ty(name) {
                return Some(ty);
            }
        }
        None
    }

    pub fn type_name(&mut self, ty: TypeId) -> String {
        for scope in self.scope_stack.iter().rev() {
            if let Some(name) = self.scope(*scope).type_name(ty) {
                return name.to_string();
            }
        }

        rue_types::stringify(self.db.types_mut(), ty)
    }

    pub fn symbol_type(&self, symbol: SymbolId) -> TypeId {
        for map in self.mapping_stack.iter().rev() {
            if let Some(ty) = map.get(&symbol) {
                return *ty;
            }
        }

        match self.symbol(symbol) {
            Symbol::Unresolved => self.builtins().unresolved.ty,
            Symbol::Module(_) => self.builtins().unresolved.ty,
            Symbol::Function(function) => function.ty,
            Symbol::Parameter(parameter) => parameter.ty,
            Symbol::Constant(constant) => constant.ty,
            Symbol::Binding(binding) => binding.ty,
        }
    }

    pub fn push_mappings(
        &mut self,
        mappings: HashMap<SymbolId, HashMap<Vec<TypePath>, TypeId>>,
    ) -> usize {
        let mut result = HashMap::new();

        for (symbol, paths) in mappings {
            let mut ty = self.symbol_type(symbol);

            let mut paths = paths.into_iter().collect::<Vec<_>>();
            paths.sort_by_key(|(path, _)| (Reverse(path.len()), path.last().copied()));

            for (path, replacement) in paths {
                ty = replace_type(&mut self.db, ty, replacement, &path);
            }

            result.insert(symbol, ty);
        }

        let index = self.mapping_stack.len();
        self.mapping_stack.push(result);
        index
    }

    pub fn mapping_checkpoint(&self) -> usize {
        self.mapping_stack.len()
    }

    pub fn revert_mappings(&mut self, index: usize) {
        self.mapping_stack.truncate(index);
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
                if cast {
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

    pub fn reference(&mut self, reference: Declaration) {
        if let Some(last) = self.declaration_stack.last() {
            self.db.add_reference(*last, reference);
        }
    }
}

pub trait GetTextRange {
    fn text_range(&self) -> TextRange;
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
