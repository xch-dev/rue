use std::{
    collections::HashMap,
    ops::{Deref, DerefMut, Range},
};

use rowan::TextRange;
use rue_diagnostic::{Diagnostic, DiagnosticKind};
use rue_hir::{
    Atom, Comparison, Database, Hir, HirId, Scope, ScopeId, SymbolId, Type, TypeId, Value,
    compare_types,
};
use rue_parser::{SyntaxNode, SyntaxToken};

#[derive(Debug, Clone)]
pub struct Builtins {
    pub unresolved: Value,
    pub bytes: TypeId,
    pub bytes32: TypeId,
    pub public_key: TypeId,
    pub int: TypeId,
    pub bool: TypeId,
    pub nil: TypeId,
    pub true_type: TypeId,
    pub false_type: TypeId,
}

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

        let unresolved_type = db.alloc_type(Type::Unresolved);
        let unresolved_hir = db.alloc_hir(Hir::Unresolved);

        let bytes = db.alloc_type(Type::Atom(Atom::Bytes));
        let bytes32 = db.alloc_type(Type::Atom(Atom::Bytes32));
        let public_key = db.alloc_type(Type::Atom(Atom::PublicKey));
        let int = db.alloc_type(Type::Atom(Atom::Int));
        let bool = db.alloc_type(Type::Atom(Atom::Bool));
        let nil = db.alloc_type(Type::Atom(Atom::Nil));
        let true_type = db.alloc_type(Type::Atom(Atom::BoolValue(true)));
        let false_type = db.alloc_type(Type::Atom(Atom::BoolValue(false)));

        let builtins = Builtins {
            unresolved: Value::new(unresolved_hir, unresolved_type),
            bytes,
            bytes32,
            public_key,
            int,
            bool,
            nil,
            true_type,
            false_type,
        };

        let mut scope = Scope::new();

        scope.insert_type("Bytes".to_string(), bytes);
        scope.insert_type("Bytes32".to_string(), bytes32);
        scope.insert_type("PublicKey".to_string(), public_key);
        scope.insert_type("Int".to_string(), int);
        scope.insert_type("Bool".to_string(), bool);

        let scope = db.alloc_scope(scope);

        Self {
            errors: Vec::new(),
            db,
            scope_stack: vec![scope],
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
        let comparison = compare_types(
            self,
            hir,
            &HashMap::new(),
            from,
            &HashMap::new(),
            to,
            &mut HashMap::new(),
        );

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
