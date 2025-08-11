use std::ops::{Deref, DerefMut, Range};

use rowan::TextRange;
use rue_diagnostic::{Diagnostic, DiagnosticKind};
use rue_hir::{
    Database, FunctionSymbol, Hir, ParameterSymbol, Scope, ScopeId, Symbol, SymbolId, Type, TypeId,
    UnaryOp, Value,
};
use rue_parser::{SyntaxNode, SyntaxToken};

#[derive(Debug, Clone)]
pub struct Builtins {
    pub unresolved: Value,
    pub bytes: TypeId,
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

        let builtins = Builtins {
            unresolved: Value::new(unresolved_hir, unresolved_type),
            bytes: unresolved_type,
        };

        let mut scope = Scope::new();

        scope.insert_type("Bytes".to_string(), builtins.bytes);

        let listp = {
            let scope = db.alloc_scope(Scope::new());
            let param = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
                name: None,
                ty: unresolved_type,
            }));
            let param_hir = db.alloc_hir(Hir::Reference(param));
            let body = db.alloc_hir(Hir::Unary(UnaryOp::Listp, param_hir));
            db.alloc_symbol(Symbol::Function(FunctionSymbol {
                name: None,
                scope,
                vars: vec![],
                parameters: vec![param],
                return_type: unresolved_type,
                body,
            }))
        };

        let first = {
            let scope = db.alloc_scope(Scope::new());
            let param = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
                name: None,
                ty: unresolved_type,
            }));
            let param_hir = db.alloc_hir(Hir::Reference(param));
            let body = db.alloc_hir(Hir::Unary(UnaryOp::First, param_hir));
            db.alloc_symbol(Symbol::Function(FunctionSymbol {
                name: None,
                scope,
                vars: vec![],
                parameters: vec![param],
                return_type: unresolved_type,
                body,
            }))
        };

        let rest = {
            let scope = db.alloc_scope(Scope::new());
            let param = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
                name: None,
                ty: unresolved_type,
            }));
            let param_hir = db.alloc_hir(Hir::Reference(param));
            let body = db.alloc_hir(Hir::Unary(UnaryOp::Rest, param_hir));
            db.alloc_symbol(Symbol::Function(FunctionSymbol {
                name: None,
                scope,
                vars: vec![],
                parameters: vec![param],
                return_type: unresolved_type,
                body,
            }))
        };

        scope.insert_symbol("listp".to_string(), listp);
        scope.insert_symbol("first".to_string(), first);
        scope.insert_symbol("rest".to_string(), rest);

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

    pub fn builtins(&self) -> &Builtins {
        &self.builtins
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
