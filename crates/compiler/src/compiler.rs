use std::{
    collections::HashMap,
    ops::{Deref, DerefMut, Range},
};

use rowan::TextRange;
use rue_diagnostic::{Diagnostic, DiagnosticKind};
use rue_hir::{
    Builtins, Comparison, ComparisonContext, Database, HirId, Scope, ScopeId, SymbolId, Type,
    TypeId, compare_types,
};
use rue_parser::{SyntaxNode, SyntaxToken};

#[derive(Debug, Clone)]
pub struct Compiler {
    errors: Vec<Diagnostic>,
    db: Database,
    scope_stack: Vec<ScopeId>,
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
            errors: Vec::new(),
            db,
            scope_stack: vec![builtins.scope],
            builtins,
        }
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn errors(&self) -> &[Diagnostic] {
        &self.errors
    }

    pub fn builtins(&self) -> &Builtins {
        &self.builtins
    }

    pub fn diagnostic(&mut self, node: &impl GetTextRange, kind: DiagnosticKind) {
        let range = node.text_range();
        let span: Range<usize> = range.start().into()..range.end().into();
        self.errors.push(Diagnostic::new(span, kind));
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
            Type::Apply(inner, map) => self.type_name_impl(*inner, map),
            Type::Union(types) => {
                let types = types
                    .iter()
                    .map(|ty| self.type_name_impl(*ty, map))
                    .collect::<Vec<_>>();
                types.join(" | ")
            }
        }
    }

    pub fn is_assignable(&mut self, from: TypeId, to: TypeId) -> bool {
        let hir = self.builtins.unresolved.hir;

        let mut ctx = ComparisonContext::new(hir, HashMap::new());
        let comparison = compare_types(&mut self.db, &mut ctx, &self.builtins, from, to);

        matches!(comparison, Comparison::Assignable)
    }

    pub fn assign_type(&mut self, node: &impl GetTextRange, hir: HirId, from: TypeId, to: TypeId) {
        self.compare_type(node, hir, from, to, false);
    }

    pub fn cast_type(&mut self, node: &impl GetTextRange, hir: HirId, from: TypeId, to: TypeId) {
        self.compare_type(node, hir, from, to, true);
    }

    fn compare_type(
        &mut self,
        node: &impl GetTextRange,
        hir: HirId,
        from: TypeId,
        to: TypeId,
        is_cast: bool,
    ) {
        let mut ctx = ComparisonContext::new(hir, HashMap::new());
        let comparison = compare_types(&mut self.db, &mut ctx, &self.builtins, from, to);

        match comparison {
            Comparison::Assignable => {
                if is_cast {
                    self.diagnostic(
                        node,
                        DiagnosticKind::UnnecessaryCast(self.type_name(from), self.type_name(to)),
                    );
                }
            }
            Comparison::Castable => {
                if !is_cast {
                    self.diagnostic(
                        node,
                        DiagnosticKind::UnassignableType(self.type_name(from), self.type_name(to)),
                    );
                }
            }
            Comparison::Incompatible => {
                if is_cast {
                    self.diagnostic(
                        node,
                        DiagnosticKind::IncompatibleCast(self.type_name(from), self.type_name(to)),
                    );
                } else {
                    self.diagnostic(
                        node,
                        DiagnosticKind::IncompatibleType(self.type_name(from), self.type_name(to)),
                    );
                }
            }
            Comparison::Constrainable(..) => {
                self.diagnostic(
                    node,
                    DiagnosticKind::UnconstrainableComparison(
                        self.type_name(from),
                        self.type_name(to),
                    ),
                );
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
