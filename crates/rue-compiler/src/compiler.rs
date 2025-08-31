use std::{
    cmp::Reverse,
    collections::HashMap,
    ops::{Deref, DerefMut, Range},
};

use rowan::TextRange;
use rue_diagnostic::{Diagnostic, DiagnosticKind};
use rue_hir::{
    Builtins, Constraint, Database, Scope, ScopeId, Symbol, SymbolId, TypePath, Value, replace_type,
};
use rue_options::CompilerOptions;
use rue_parser::{SyntaxNode, SyntaxToken};
use rue_types::{Check, CheckError, Comparison, TypeId};

#[derive(Debug, Clone)]
pub struct Compiler {
    _options: CompilerOptions,
    diagnostics: Vec<Diagnostic>,
    db: Database,
    scope_stack: Vec<ScopeId>,
    mapping_stack: Vec<HashMap<SymbolId, TypeId>>,
    builtins: Builtins,
    defaults: HashMap<TypeId, HashMap<String, Value>>,
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
            diagnostics: Vec::new(),
            db,
            scope_stack: vec![builtins.scope],
            mapping_stack: vec![HashMap::new()],
            builtins,
            defaults: HashMap::new(),
        }
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn builtins(&self) -> &Builtins {
        &self.builtins
    }

    pub fn diagnostic(&mut self, node: &impl GetTextRange, kind: DiagnosticKind) {
        let range = node.text_range();
        let span: Range<usize> = range.start().into()..range.end().into();
        self.diagnostics.push(Diagnostic::new(span, kind));
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
        self.type_name_impl(ty, &HashMap::new())
    }

    fn type_name_impl(&mut self, ty: TypeId, map: &HashMap<TypeId, TypeId>) -> String {
        if let Some(ty) = map.get(&ty) {
            return self.type_name_impl(*ty, map);
        }

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
        let comparison = rue_types::compare(self.db.types_mut(), from, to);
        comparison == Comparison::Assign
    }

    pub fn is_castable(&mut self, from: TypeId, to: TypeId) -> bool {
        let comparison = rue_types::compare(self.db.types_mut(), from, to);
        matches!(comparison, Comparison::Assign | Comparison::Cast)
    }

    pub fn assign_type(&mut self, node: &impl GetTextRange, from: TypeId, to: TypeId) {
        self.compare_type(node, from, to, false, None);
    }

    pub fn cast_type(&mut self, node: &impl GetTextRange, from: TypeId, to: TypeId) {
        self.compare_type(node, from, to, true, None);
    }

    pub fn guard_type(&mut self, node: &impl GetTextRange, from: TypeId, to: TypeId) -> Constraint {
        let check = match rue_types::check(self.db.types_mut(), from, to) {
            Ok(check) => check,
            Err(CheckError::DepthExceeded) => {
                self.diagnostic(node, DiagnosticKind::TypeCheckDepthExceeded);
                Check::Impossible
            }
        };

        let from_name = self.type_name(from);
        let to_name = self.type_name(to);

        if check == Check::None {
            self.diagnostic(node, DiagnosticKind::UnnecessaryGuard(from_name, to_name));
        } else if check == Check::Impossible {
            self.diagnostic(node, DiagnosticKind::IncompatibleGuard(from_name, to_name));
        }

        let else_id = rue_types::subtract(self.db.types_mut(), from, to);

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
        let comparison = rue_types::compare_with_inference(self.db.types_mut(), from, to, infer);

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
                let check = match rue_types::check(self.db.types_mut(), from, to) {
                    Ok(check) => check,
                    Err(CheckError::DepthExceeded) => {
                        self.diagnostic(node, DiagnosticKind::TypeCheckDepthExceeded);
                        return;
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
