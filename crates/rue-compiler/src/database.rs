use id_arena::Arena;
use indexmap::IndexMap;
use rowan::TextRange;
use rue_parser::SyntaxToken;

mod comparison;
mod ids;

pub use comparison::*;
pub use ids::*;
use rue_typing::TypeId;

use crate::{
    DependencyGraph, Diagnostic, DiagnosticKind, Environment, ErrorKind, Hir, Lir, Mir, Scope,
    Symbol, WarningKind,
};

#[derive(Debug, Default)]
pub struct Database {
    diagnostics: Vec<Diagnostic>,
    scopes: Arena<Scope>,
    symbols: Arena<Symbol>,
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

    pub fn scopes(&self) -> Vec<ScopeId> {
        self.scopes.iter().map(|(id, _)| ScopeId(id)).collect()
    }

    pub fn dbg_hir(&self, id: HirId) -> String {
        match self.hir(id) {
            Hir::Unknown => "<unknown hir>".to_string(),
            Hir::Atom(bytes) => format!("Atom({})", hex::encode(bytes)),
            Hir::BinaryOp(op, lhs, rhs) => {
                format!("{op:?}({}, {})", self.dbg_hir(*lhs), self.dbg_hir(*rhs))
            }
            Hir::Op(op, hir_id) => {
                format!("{op:?}({})", self.dbg_hir(*hir_id))
            }
            Hir::Substr(start, end, string) => format!(
                "Substr({}, {}, {})",
                self.dbg_hir(*start),
                self.dbg_hir(*end),
                self.dbg_hir(*string)
            ),
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

    pub fn dbg_mir(&self, graph: Option<&DependencyGraph>, mir_id: MirId) -> String {
        match self.mir(mir_id) {
            Mir::Atom(bytes) => format!("Atom({})", hex::encode(bytes)),
            Mir::BinaryOp(op, lhs, rhs) => {
                format!(
                    "{op:?}({}, {})",
                    self.dbg_mir(graph, *lhs),
                    self.dbg_mir(graph, *rhs)
                )
            }
            Mir::Op(op, mir_id) => {
                format!("{op:?}({})", self.dbg_mir(graph, *mir_id))
            }
            Mir::Reference(symbol_id) => format!("Reference({})", self.dbg_symbol(*symbol_id)),
            Mir::Environment(env_id, mir_id) => {
                format!(
                    "Environment({}, {})",
                    self.dbg_env(graph, *env_id),
                    self.dbg_mir(graph, *mir_id)
                )
            }
            Mir::Substr(mir_id, start, end) => format!(
                "Substr({}, {}, {})",
                self.dbg_mir(graph, *mir_id),
                self.dbg_mir(graph, *start),
                self.dbg_mir(graph, *end)
            ),
            Mir::Raise(mir_id) => format!(
                "Raise({})",
                mir_id
                    .map(|mir_id| self.dbg_mir(graph, mir_id))
                    .unwrap_or_default()
            ),
            Mir::Pair(first, rest) => {
                format!(
                    "Pair({}, {})",
                    self.dbg_mir(graph, *first),
                    self.dbg_mir(graph, *rest)
                )
            }
            Mir::Run(mir_id, args) => {
                format!(
                    "Run({}, {})",
                    self.dbg_mir(graph, *mir_id),
                    self.dbg_mir(graph, *args)
                )
            }
            Mir::Closure(mir_id, args) => {
                format!(
                    "Closure({}, [{}])",
                    self.dbg_mir(graph, *mir_id),
                    args.iter()
                        .map(|arg| self.dbg_mir(graph, *arg))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Mir::Curry(mir_id, args) => {
                format!(
                    "Curry({}, [{}])",
                    self.dbg_mir(graph, *mir_id),
                    args.iter()
                        .map(|arg| self.dbg_mir(graph, *arg))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Mir::Quote(mir_id) => format!("Quote({})", self.dbg_mir(graph, *mir_id)),
            Mir::If(condition, then_block, else_block) => format!(
                "If({}, {}, {})",
                self.dbg_mir(graph, *condition),
                self.dbg_mir(graph, *then_block),
                self.dbg_mir(graph, *else_block)
            ),
        }
    }

    pub fn dbg_env(&self, graph: Option<&DependencyGraph>, env_id: EnvironmentId) -> String {
        format!(
            "Env({:?} {}, Parameters = [{}], Definitions = [{}], Captures = [{}], Parent = {})",
            graph
                .and_then(|graph| graph
                    .scopes()
                    .iter()
                    .find(|scope_id| graph.environment_id(**scope_id) == env_id)
                    .copied())
                .map(|scope_id| self.scope_token(scope_id).map(ToString::to_string)),
            env_id.0.index(),
            self.env(env_id)
                .parameters()
                .iter()
                .map(|symbol_id| self.dbg_symbol(*symbol_id))
                .collect::<Vec<_>>()
                .join(", "),
            self.env(env_id)
                .definitions()
                .iter()
                .map(|symbol_id| self.dbg_symbol(*symbol_id))
                .collect::<Vec<_>>()
                .join(", "),
            self.env(env_id)
                .captures()
                .iter()
                .map(|symbol_id| self.dbg_symbol(*symbol_id))
                .collect::<Vec<_>>()
                .join(", "),
            self.env(env_id).parent().map_or_else(
                || "None".to_string(),
                |parent_id| self.dbg_env(graph, parent_id)
            )
        )
    }

    pub fn dbg_symbol(&self, symbol_id: SymbolId) -> String {
        if let Some(symbol_token) = self.symbol_token(symbol_id) {
            return format!("{symbol_token}({})", symbol_id.0.index());
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
