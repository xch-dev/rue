use std::{
    cmp::Reverse,
    collections::HashMap,
    ops::{Deref, DerefMut, Range},
};

use rowan::TextRange;
use rue_diagnostic::{Diagnostic, DiagnosticKind};
use rue_hir::{
    Builtins, Check, Comparison, ComparisonContext, Constraint, Database, Scope, ScopeId, Symbol,
    SymbolId, Type, TypeId, TypePath, compare_types, replace_type, unwrap_type,
};
use rue_parser::{SyntaxNode, SyntaxToken};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ComparisonKind {
    Assign,
    Cast,
    Guard,
}

#[derive(Debug, Clone)]
pub struct Compiler {
    diagnostics: Vec<Diagnostic>,
    db: Database,
    scope_stack: Vec<ScopeId>,
    mapping_stack: Vec<HashMap<SymbolId, TypeId>>,
    builtins: Builtins,
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

impl Default for Compiler {
    fn default() -> Self {
        let mut db = Database::new();

        let builtins = Builtins::new(&mut db);

        Self {
            diagnostics: Vec::new(),
            db,
            scope_stack: vec![builtins.scope],
            mapping_stack: vec![HashMap::new()],
            builtins,
        }
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self::default()
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

    pub fn type_name(&self, ty: TypeId) -> String {
        self.type_name_impl(ty, &HashMap::new())
    }

    fn type_name_impl(&self, ty: TypeId, map: &HashMap<TypeId, TypeId>) -> String {
        if let Some(ty) = map.get(&ty) {
            return self.type_name_impl(*ty, map);
        }

        for scope in self.scope_stack.iter().rev() {
            if let Some(name) = self.scope(*scope).type_name(ty) {
                return name.to_string();
            }
        }

        match self.ty(ty) {
            Type::Unresolved => "{unknown}".to_string(),
            Type::Atom(atom) => atom.to_string(),
            Type::Pair(first, rest) => {
                let first = self.type_name_impl(*first, map);
                let rest = self.type_name_impl(*rest, map);
                format!("({first}, {rest})")
            }
            Type::Generic(generic) => generic
                .name
                .as_ref()
                .map_or("{generic}".to_string(), |name| name.text().to_string()),
            Type::Alias(alias) => {
                if let Some(name) = alias.name.as_ref() {
                    name.text().to_string()
                } else {
                    self.type_name_impl(alias.inner, map)
                }
            }
            Type::Union(types) => {
                let types = types
                    .iter()
                    .map(|ty| self.type_name_impl(*ty, map))
                    .collect::<Vec<_>>();
                types.join(" | ")
            }
            Type::Fn(function) => {
                let params = function
                    .params
                    .iter()
                    .map(|ty| self.type_name_impl(*ty, map))
                    .collect::<Vec<_>>();
                let ret = self.type_name_impl(function.ret, map);
                format!("fn({}) -> {}", params.join(", "), ret)
            }
        }
    }

    pub fn symbol_type(&self, symbol: SymbolId) -> TypeId {
        for map in self.mapping_stack.iter().rev() {
            if let Some(ty) = map.get(&symbol) {
                return *ty;
            }
        }

        match self.symbol(symbol) {
            Symbol::Binding(binding) => binding.ty,
            Symbol::Function(function) => function.ty,
            Symbol::Parameter(parameter) => parameter.ty,
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
        let mut ctx = ComparisonContext::new(HashMap::new());
        let comparison = compare_types(&mut self.db, &mut ctx, from, to);

        matches!(comparison, Comparison::Assignable)
    }

    pub fn is_castable(&mut self, from: TypeId, to: TypeId) -> bool {
        let mut ctx = ComparisonContext::new(HashMap::new());
        let comparison = compare_types(&mut self.db, &mut ctx, from, to);

        matches!(comparison, Comparison::Assignable | Comparison::Castable)
    }

    pub fn unwrap_type(&mut self, ty: TypeId) -> TypeId {
        unwrap_type(&self.db, &mut ComparisonContext::new(HashMap::new()), ty)
    }

    pub fn assign_type(&mut self, node: &impl GetTextRange, from: TypeId, to: TypeId) {
        self.compare_type(node, from, to, ComparisonKind::Assign);
    }

    pub fn cast_type(&mut self, node: &impl GetTextRange, from: TypeId, to: TypeId) {
        self.compare_type(node, from, to, ComparisonKind::Cast);
    }

    pub fn guard_type(&mut self, node: &impl GetTextRange, from: TypeId, to: TypeId) -> Constraint {
        self.compare_type(node, from, to, ComparisonKind::Guard)
    }

    fn compare_type(
        &mut self,
        node: &impl GetTextRange,
        from: TypeId,
        to: TypeId,
        kind: ComparisonKind,
    ) -> Constraint {
        let comparison = compare_types(
            &mut self.db,
            &mut ComparisonContext::new(HashMap::new()),
            from,
            to,
        );

        match comparison {
            Comparison::Assignable => {
                match kind {
                    ComparisonKind::Assign => {}
                    ComparisonKind::Cast => {
                        self.diagnostic(
                            node,
                            DiagnosticKind::UnnecessaryCast(
                                self.type_name(from),
                                self.type_name(to),
                            ),
                        );
                    }
                    ComparisonKind::Guard => {
                        self.diagnostic(
                            node,
                            DiagnosticKind::UnnecessaryGuard(
                                self.type_name(from),
                                self.type_name(to),
                            ),
                        );
                    }
                }
                Constraint::new(Check::None)
            }
            Comparison::Castable => {
                match kind {
                    ComparisonKind::Assign => {
                        self.diagnostic(
                            node,
                            DiagnosticKind::UnassignableType(
                                self.type_name(from),
                                self.type_name(to),
                            ),
                        );
                    }
                    ComparisonKind::Cast => {}
                    ComparisonKind::Guard => {
                        self.diagnostic(
                            node,
                            DiagnosticKind::UnnecessaryGuard(
                                self.type_name(from),
                                self.type_name(to),
                            ),
                        );
                    }
                }
                Constraint::new(Check::None)
            }
            Comparison::Incompatible(check) => {
                match kind {
                    ComparisonKind::Assign => {
                        self.diagnostic(
                            node,
                            DiagnosticKind::IncompatibleType(
                                self.type_name(from),
                                self.type_name(to),
                            ),
                        );
                    }
                    ComparisonKind::Cast => {
                        self.diagnostic(
                            node,
                            DiagnosticKind::IncompatibleCast(
                                self.type_name(from),
                                self.type_name(to),
                            ),
                        );
                    }
                    ComparisonKind::Guard => {
                        self.diagnostic(
                            node,
                            DiagnosticKind::IncompatibleGuard(
                                self.type_name(from),
                                self.type_name(to),
                            ),
                        );
                    }
                }
                Constraint::new(check)
            }
            Comparison::Constrainable(constraint) => {
                if kind != ComparisonKind::Guard {
                    self.diagnostic(
                        node,
                        DiagnosticKind::UnconstrainableComparison(
                            self.type_name(from),
                            self.type_name(to),
                        ),
                    );
                }
                constraint
            }
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
