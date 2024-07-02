use id_arena::Arena;
use indexmap::IndexMap;
use rowan::TextRange;
use rue_parser::SyntaxToken;

mod comparison;
mod ids;
mod type_system;

pub use comparison::*;
pub use ids::*;

use crate::{
    environment::Environment,
    hir::{Hir, Op},
    lir::Lir,
    mir::Mir,
    scope::Scope,
    symbol::Symbol,
    value::Type,
    Diagnostic, DiagnosticKind, ErrorKind, WarningKind,
};

#[derive(Debug, Default)]
pub struct Database {
    diagnostics: Vec<Diagnostic>,
    scopes: Arena<Scope>,
    symbols: Arena<Symbol>,
    types: Arena<Type>,
    hir: Arena<Hir>,
    mir: Arena<Mir>,
    lir: Arena<Lir>,
    environments: Arena<Environment>,
    symbol_tokens: IndexMap<SymbolId, SyntaxToken>,
    type_tokens: IndexMap<TypeId, SyntaxToken>,
    scope_tokens: IndexMap<ScopeId, SyntaxToken>,
}

impl Database {
    pub fn new() -> Self {
        Self::default()
    }

    pub(crate) fn alloc_scope(&mut self, scope: Scope) -> ScopeId {
        ScopeId(self.scopes.alloc(scope))
    }

    pub(crate) fn alloc_symbol(&mut self, symbol: Symbol) -> SymbolId {
        SymbolId(self.symbols.alloc(symbol))
    }

    pub(crate) fn alloc_type(&mut self, ty: Type) -> TypeId {
        TypeId(self.types.alloc(ty))
    }

    pub(crate) fn alloc_hir(&mut self, hir: Hir) -> HirId {
        HirId(self.hir.alloc(hir))
    }

    pub(crate) fn alloc_mir(&mut self, mir: Mir) -> MirId {
        MirId(self.mir.alloc(mir))
    }

    pub(crate) fn alloc_lir(&mut self, lir: Lir) -> LirId {
        LirId(self.lir.alloc(lir))
    }

    pub(crate) fn alloc_env(&mut self, environment: Environment) -> EnvironmentId {
        EnvironmentId(self.environments.alloc(environment))
    }

    pub fn scope(&self, id: ScopeId) -> &Scope {
        &self.scopes[id.0]
    }

    pub fn symbol(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id.0]
    }

    pub fn ty_raw(&self, id: TypeId) -> &Type {
        &self.types[id.0]
    }

    pub fn ty(&self, mut id: TypeId) -> &Type {
        while let Type::Alias(alias) = self.ty_raw(id) {
            id = *alias;
        }
        self.ty_raw(id)
    }

    pub fn hir(&self, id: HirId) -> &Hir {
        &self.hir[id.0]
    }

    pub fn mir(&self, id: MirId) -> &Mir {
        &self.mir[id.0]
    }

    pub fn lir(&self, id: LirId) -> &Lir {
        &self.lir[id.0]
    }

    pub fn env(&self, id: EnvironmentId) -> &Environment {
        &self.environments[id.0]
    }

    pub(crate) fn ty_mut(&mut self, id: TypeId) -> &mut Type {
        &mut self.types[id.0]
    }

    pub(crate) fn env_mut(&mut self, id: EnvironmentId) -> &mut Environment {
        &mut self.environments[id.0]
    }

    pub(crate) fn scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        &mut self.scopes[id.0]
    }

    pub(crate) fn symbol_mut(&mut self, id: SymbolId) -> &mut Symbol {
        &mut self.symbols[id.0]
    }

    pub(crate) fn insert_symbol_token(&mut self, symbol_id: SymbolId, token: SyntaxToken) {
        self.symbol_tokens.insert(symbol_id, token);
    }

    pub(crate) fn insert_type_token(&mut self, type_id: TypeId, token: SyntaxToken) {
        self.type_tokens.insert(type_id, token);
    }

    pub(crate) fn insert_scope_token(&mut self, scope_id: ScopeId, token: SyntaxToken) {
        self.scope_tokens.insert(scope_id, token);
    }

    pub fn symbol_token(&self, symbol_id: SymbolId) -> Option<&SyntaxToken> {
        self.symbol_tokens.get(&symbol_id)
    }

    pub fn type_token(&self, type_id: TypeId) -> Option<&SyntaxToken> {
        self.type_tokens.get(&type_id)
    }

    pub fn scope_token(&self, scope_id: ScopeId) -> Option<&SyntaxToken> {
        self.scope_tokens.get(&scope_id)
    }

    pub fn named_types(&self) -> Vec<TypeId> {
        self.type_tokens.keys().copied().collect()
    }

    pub fn dbg_hir(&self, id: HirId) -> String {
        match self.hir(id) {
            Hir::Unknown => "<unknown hir>".to_string(),
            Hir::Atom(bytes) => format!("Atom({})", hex::encode(bytes)),
            Hir::BinaryOp(op, lhs, rhs) => {
                format!("{op:?}({}, {})", self.dbg_hir(*lhs), self.dbg_hir(*rhs))
            }
            Hir::Op(op, hir_id) => match op {
                Op::First => format!("First({})", self.dbg_hir(*hir_id)),
                Op::Rest => format!("Rest({})", self.dbg_hir(*hir_id)),
                Op::Sha256 => format!("Sha256({})", self.dbg_hir(*hir_id)),
                Op::Listp => format!("Listp({})", self.dbg_hir(*hir_id)),
                Op::Not => format!("Not({})", self.dbg_hir(*hir_id)),
                Op::Strlen => format!("Strlen({})", self.dbg_hir(*hir_id)),
                Op::PubkeyForExp => format!("PubkeyForExp({})", self.dbg_hir(*hir_id)),
                Op::Exists => format!("Exists({})", self.dbg_hir(*hir_id)),
            },
            Hir::Raise(hir_id) => format!(
                "Raise({})",
                hir_id
                    .map(|hir_id| self.dbg_hir(hir_id))
                    .unwrap_or_default()
            ),
            Hir::Pair(first, rest) => {
                format!("Pair({}, {})", self.dbg_hir(*first), self.dbg_hir(*rest))
            }
            Hir::Reference(symbol_id, ..) => format!("Reference({})", self.dbg_symbol(*symbol_id)),
            Hir::FunctionCall(callee, args, varargs) => format!(
                "Call({}, [{}], Varargs = {})",
                self.dbg_hir(*callee),
                args.iter()
                    .map(|arg| self.dbg_hir(*arg))
                    .collect::<Vec<_>>()
                    .join(", "),
                varargs
            ),
            Hir::Definition(scope_id, hir_id) => format!(
                "Definition({}, {})",
                self.dbg_scope(*scope_id),
                self.dbg_hir(*hir_id)
            ),
            Hir::If(condition, then_block, else_block) => format!(
                "If({}, {}, {})",
                self.dbg_hir(*condition),
                self.dbg_hir(*then_block),
                self.dbg_hir(*else_block)
            ),
        }
    }

    pub fn dbg_symbol(&self, symbol_id: SymbolId) -> String {
        if let Some(symbol_token) = self.symbol_token(symbol_id) {
            return symbol_token.to_string();
        }

        match self.symbol(symbol_id) {
            Symbol::Unknown => format!("<unknown symbol {}>", symbol_id.0.index()),
            Symbol::Module(..) => format!("<module {}>", symbol_id.0.index()),
            Symbol::Parameter(..) => format!("<parameter {}>", symbol_id.0.index()),
            Symbol::Function(..) => format!("<function {}>", symbol_id.0.index()),
            Symbol::InlineFunction(..) => format!("<inline function {}>", symbol_id.0.index()),
            Symbol::Let(..) => format!("<let {}>", symbol_id.0.index()),
            Symbol::Const(..) => format!("<const {}>", symbol_id.0.index()),
            Symbol::InlineConst(..) => format!("<inline const {}>", symbol_id.0.index()),
        }
    }

    pub fn dbg_scope(&self, scope_id: ScopeId) -> String {
        let defined_symbols = self
            .scope(scope_id)
            .local_symbols()
            .into_iter()
            .map(|symbol_id| self.dbg_symbol(symbol_id))
            .collect::<Vec<_>>()
            .join(", ");

        format!(
            "Scope({:?}, Symbols = [{}])",
            self.scope_token(scope_id).map(ToString::to_string),
            defined_symbols
        )
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn error(&mut self, info: ErrorKind, range: TextRange) {
        self.diagnostics.push(Diagnostic::new(
            DiagnosticKind::Error(info),
            range.start().into()..range.end().into(),
        ));
    }

    pub fn warning(&mut self, info: WarningKind, range: TextRange) {
        self.diagnostics.push(Diagnostic::new(
            DiagnosticKind::Warning(info),
            range.start().into()..range.end().into(),
        ));
    }
}
