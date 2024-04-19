use std::collections::HashSet;

use rowan::TextRange;

use crate::{
    database::{Database, HirId, LirId, ScopeId, SymbolId},
    dependency_graph::{DependencyGraph, Environment},
    hir::{BinOp, Hir},
    lir::Lir,
    symbol::Symbol,
    symbol_table::GlobalSymbolTable,
    ty::Type,
    Diagnostic, DiagnosticKind, WarningKind,
};

pub struct Optimizer<'a> {
    db: &'a mut Database,
    sym: &'a GlobalSymbolTable,
    diagnostics: Vec<Diagnostic>,
    graph: DependencyGraph,
}

impl<'a> Optimizer<'a> {
    pub fn new(db: &'a mut Database, sym: &'a GlobalSymbolTable, graph: DependencyGraph) -> Self {
        Self {
            db,
            sym,
            diagnostics: Vec::new(),
            graph,
        }
    }

    pub fn finish(self) -> Vec<Diagnostic> {
        self.diagnostics
    }

    fn env(&self, scope_id: ScopeId) -> &Environment {
        self.graph.env(scope_id)
    }

    pub fn opt_main(&mut self, root_scope_id: ScopeId, main: SymbolId) -> LirId {
        let mut used_symbols = HashSet::new();

        for symbol_id in self
            .graph
            .visited_scopes()
            .into_iter()
            .chain([root_scope_id])
            .flat_map(|scope_id| self.db.scope(scope_id).local_symbols())
            .collect::<Vec<SymbolId>>()
        {
            if self.graph.symbol_usages(symbol_id) > 0 || symbol_id == main {
                used_symbols.insert(symbol_id);
                continue;
            }

            let token = self.sym.symbol_token(symbol_id).unwrap();

            if token.text().starts_with('_') {
                continue;
            }

            match self.db.symbol(symbol_id).clone() {
                Symbol::Unknown => unreachable!(),
                Symbol::Function { .. } => self.warning(
                    WarningKind::UnusedFunction(token.to_string()),
                    token.text_range(),
                ),
                Symbol::Parameter { .. } => self.warning(
                    WarningKind::UnusedParameter(token.to_string()),
                    token.text_range(),
                ),
                Symbol::LetBinding { .. } => self.warning(
                    WarningKind::UnusedLet(token.to_string()),
                    token.text_range(),
                ),
                Symbol::ConstBinding { .. } => self.warning(
                    WarningKind::UnusedConst(token.to_string()),
                    token.text_range(),
                ),
            }
        }

        for type_id in self.sym.named_types() {
            if used_symbols
                .iter()
                .any(|symbol_id| self.sym.type_referenced_by_symbol(type_id, *symbol_id))
            {
                continue;
            }

            let token = self.sym.type_token(type_id).unwrap();

            if token.text().starts_with('_') {
                continue;
            }

            match self.db.ty_raw(type_id) {
                Type::Alias(..) => {
                    self.warning(
                        WarningKind::UnusedTypeAlias(token.to_string()),
                        token.text_range(),
                    );
                }
                Type::Struct { .. } => {
                    self.warning(
                        WarningKind::UnusedStruct(token.to_string()),
                        token.text_range(),
                    );
                }
                Type::Enum { .. } => {
                    self.warning(
                        WarningKind::UnusedEnum(token.to_string()),
                        token.text_range(),
                    );
                }
                Type::EnumVariant { .. } => {
                    self.warning(
                        WarningKind::UnusedEnumVariant(token.to_string()),
                        token.text_range(),
                    );
                }
                _ => {}
            }
        }

        let Symbol::Function {
            scope_id, hir_id, ..
        } = self.db.symbol(main).clone()
        else {
            unreachable!();
        };

        let body = self.opt_hir(scope_id, hir_id);

        let mut args = Vec::new();

        for symbol_id in self.env(scope_id).definitions() {
            args.push(self.opt_definition(scope_id, symbol_id));
        }

        for symbol_id in self.env(scope_id).captures() {
            args.push(self.opt_definition(scope_id, symbol_id));
        }

        self.db.alloc_lir(Lir::Curry(body, args))
    }

    fn opt_scope(&mut self, parent_scope_id: ScopeId, scope_id: ScopeId, hir_id: HirId) -> LirId {
        let body = self.opt_hir(scope_id, hir_id);

        let mut args = Vec::new();

        for symbol_id in self.env(scope_id).definitions() {
            args.push(self.opt_definition(parent_scope_id, symbol_id));
        }

        self.db.alloc_lir(Lir::Curry(body, args))
    }

    fn opt_path(&mut self, scope_id: ScopeId, symbol_id: SymbolId) -> LirId {
        let mut current_scope = scope_id;
        let mut environment = self.env(scope_id).build().to_vec();

        while let Some(parent_scope) = self.env(current_scope).parent_scope() {
            current_scope = parent_scope;
            environment.extend(self.env(current_scope).build());
        }

        let index = environment
            .iter()
            .position(|&id| id == symbol_id)
            .expect("symbol not found");

        let mut path = 1;

        if !(index + 1 == environment.len() && self.env(scope_id).varargs()) {
            path *= 2;
        }

        for _ in 0..index {
            path *= 2;
            path += 1;
        }

        self.db.alloc_lir(Lir::Path(path))
    }

    fn opt_definition(&mut self, scope_id: ScopeId, symbol_id: SymbolId) -> LirId {
        match self.db.symbol(symbol_id).clone() {
            Symbol::Unknown => unreachable!(),
            Symbol::Function {
                scope_id: function_scope_id,
                hir_id,
                ..
            } => {
                let mut body = self.opt_hir(function_scope_id, hir_id);
                let mut definitions = Vec::new();

                for symbol_id in self.env(function_scope_id).definitions() {
                    definitions.push(self.opt_definition(function_scope_id, symbol_id));
                }

                if !definitions.is_empty() {
                    body = self.db.alloc_lir(Lir::Curry(body, definitions));
                }

                self.db.alloc_lir(Lir::FunctionBody(body))
            }
            Symbol::Parameter { .. } => {
                unreachable!();
            }
            Symbol::LetBinding { hir_id, .. } => self.opt_hir(scope_id, hir_id),
            Symbol::ConstBinding { .. } => unreachable!(),
        }
    }

    fn opt_hir(&mut self, scope_id: ScopeId, hir_id: HirId) -> LirId {
        match self.db.hir(hir_id) {
            Hir::Unknown => self.db.alloc_lir(Lir::Atom(Vec::new())),
            Hir::Atom(atom) => self.db.alloc_lir(Lir::Atom(atom.clone())),
            Hir::Pair(first, rest) => self.opt_pair(scope_id, *first, *rest),
            Hir::Reference(symbol_id) => self.opt_reference(scope_id, *symbol_id),
            Hir::Scope {
                scope_id: new_scope_id,
                hir_id,
            } => self.opt_scope(scope_id, *new_scope_id, *hir_id),
            Hir::FunctionCall { callee, args } => self.opt_function_call(scope_id, *callee, *args),
            Hir::BinaryOp { op, lhs, rhs } => {
                let handler = match op {
                    BinOp::Add => Self::opt_add,
                    BinOp::Subtract => Self::opt_subtract,
                    BinOp::Multiply => Self::opt_multiply,
                    BinOp::Divide => Self::opt_divide,
                    BinOp::Remainder => Self::opt_remainder,
                    BinOp::LessThan => Self::opt_lt,
                    BinOp::GreaterThan => Self::opt_gt,
                    BinOp::LessThanEquals => Self::opt_lteq,
                    BinOp::GreaterThanEquals => Self::opt_gteq,
                    BinOp::Equals => Self::opt_eq,
                    BinOp::NotEquals => Self::opt_neq,
                    BinOp::Concat => Self::opt_concat,
                    BinOp::PointAdd => Self::opt_point_add,
                };
                handler(self, scope_id, *lhs, *rhs)
            }
            Hir::First(value) => self.opt_first(scope_id, *value),
            Hir::Rest(value) => self.opt_rest(scope_id, *value),
            Hir::Not(value) => self.opt_not(scope_id, *value),
            Hir::Raise(value) => self.opt_raise(scope_id, *value),
            Hir::Sha256(value) => self.opt_sha256(scope_id, *value),
            Hir::IsCons(value) => self.opt_is_cons(scope_id, *value),
            Hir::Strlen(value) => self.opt_strlen(scope_id, *value),
            Hir::PubkeyForExp(value) => self.opt_pubkey_for_exp(scope_id, *value),
            Hir::If {
                condition,
                then_block,
                else_block,
            } => self.opt_if(scope_id, *condition, *then_block, *else_block),
        }
    }

    fn opt_pair(&mut self, scope_id: ScopeId, first: HirId, rest: HirId) -> LirId {
        let first = self.opt_hir(scope_id, first);
        let rest = self.opt_hir(scope_id, rest);
        self.db.alloc_lir(Lir::Pair(first, rest))
    }

    fn opt_first(&mut self, scope_id: ScopeId, hir_id: HirId) -> LirId {
        let lir_id = self.opt_hir(scope_id, hir_id);
        self.db.alloc_lir(Lir::First(lir_id))
    }

    fn opt_rest(&mut self, scope_id: ScopeId, hir_id: HirId) -> LirId {
        let lir_id = self.opt_hir(scope_id, hir_id);
        self.db.alloc_lir(Lir::Rest(lir_id))
    }

    fn opt_sha256(&mut self, scope_id: ScopeId, hir_id: HirId) -> LirId {
        let lir_id = self.opt_hir(scope_id, hir_id);
        self.db.alloc_lir(Lir::Sha256(lir_id))
    }

    fn opt_is_cons(&mut self, scope_id: ScopeId, hir_id: HirId) -> LirId {
        let lir_id = self.opt_hir(scope_id, hir_id);
        self.db.alloc_lir(Lir::IsCons(lir_id))
    }

    fn opt_strlen(&mut self, scope_id: ScopeId, hir_id: HirId) -> LirId {
        let lir_id = self.opt_hir(scope_id, hir_id);
        self.db.alloc_lir(Lir::Strlen(lir_id))
    }

    fn opt_pubkey_for_exp(&mut self, scope_id: ScopeId, hir_id: HirId) -> LirId {
        let lir_id = self.opt_hir(scope_id, hir_id);
        self.db.alloc_lir(Lir::PubkeyForExp(lir_id))
    }

    fn opt_reference(&mut self, scope_id: ScopeId, symbol_id: SymbolId) -> LirId {
        match self.db.symbol(symbol_id).clone() {
            Symbol::Function {
                scope_id: function_scope_id,
                ..
            } => {
                let body = self.opt_path(scope_id, symbol_id);

                let mut captures = Vec::new();

                for symbol_id in self.db.scope(function_scope_id).local_symbols() {
                    if self.db.symbol(symbol_id).is_definition() {
                        captures.push(self.opt_path(scope_id, symbol_id));
                    }
                }

                for symbol_id in self.env(function_scope_id).captures() {
                    captures.push(self.opt_path(scope_id, symbol_id));
                }

                self.db.alloc_lir(Lir::Closure(body, captures))
            }
            Symbol::ConstBinding { hir_id, .. } => self.opt_hir(scope_id, hir_id),
            _ => self.opt_path(scope_id, symbol_id),
        }
    }

    fn opt_function_call(&mut self, scope_id: ScopeId, callee: HirId, args: HirId) -> LirId {
        let mut args = self.opt_hir(scope_id, args);

        let callee = if let Hir::Reference(symbol_id) = self.db.hir(callee).clone() {
            if let Symbol::Function {
                scope_id: callee_scope_id,
                ..
            } = self.db.symbol(symbol_id)
            {
                for symbol_id in self.env(*callee_scope_id).captures().into_iter().rev() {
                    let capture = self.opt_path(scope_id, symbol_id);
                    args = self.db.alloc_lir(Lir::Pair(capture, args));
                }
                self.opt_path(scope_id, symbol_id)
            } else {
                self.opt_hir(scope_id, callee)
            }
        } else {
            self.opt_hir(scope_id, callee)
        };

        self.db.alloc_lir(Lir::Run(callee, args))
    }

    fn opt_add(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(scope_id, lhs);
        let rhs = self.opt_hir(scope_id, rhs);
        self.db.alloc_lir(Lir::Add(vec![lhs, rhs]))
    }

    fn opt_subtract(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(scope_id, lhs);
        let rhs = self.opt_hir(scope_id, rhs);
        self.db.alloc_lir(Lir::Sub(vec![lhs, rhs]))
    }

    fn opt_multiply(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(scope_id, lhs);
        let rhs = self.opt_hir(scope_id, rhs);
        self.db.alloc_lir(Lir::Mul(vec![lhs, rhs]))
    }

    fn opt_divide(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(scope_id, lhs);
        let rhs = self.opt_hir(scope_id, rhs);
        self.db.alloc_lir(Lir::Div(lhs, rhs))
    }

    fn opt_remainder(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(scope_id, lhs);
        let rhs = self.opt_hir(scope_id, rhs);
        let divmod = self.db.alloc_lir(Lir::Divmod(lhs, rhs));
        self.db.alloc_lir(Lir::Rest(divmod))
    }

    fn opt_lt(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> LirId {
        self.opt_gt(scope_id, rhs, lhs)
    }

    fn opt_gt(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(scope_id, lhs);
        let rhs = self.opt_hir(scope_id, rhs);
        self.db.alloc_lir(Lir::Gt(lhs, rhs))
    }

    fn opt_lteq(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> LirId {
        let gt = self.opt_gt(scope_id, lhs, rhs);
        self.db.alloc_lir(Lir::Not(gt))
    }

    fn opt_gteq(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(scope_id, lhs);
        let rhs = self.opt_hir(scope_id, rhs);
        let eq = self.db.alloc_lir(Lir::Eq(lhs, rhs));
        let gt = self.db.alloc_lir(Lir::Gt(lhs, rhs));
        self.db.alloc_lir(Lir::Any(vec![eq, gt]))
    }

    fn opt_eq(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(scope_id, lhs);
        let rhs = self.opt_hir(scope_id, rhs);
        self.db.alloc_lir(Lir::Eq(lhs, rhs))
    }

    fn opt_neq(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> LirId {
        let eq = self.opt_eq(scope_id, lhs, rhs);
        self.db.alloc_lir(Lir::Not(eq))
    }

    fn opt_concat(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(scope_id, lhs);
        let rhs = self.opt_hir(scope_id, rhs);
        self.db.alloc_lir(Lir::Concat(vec![lhs, rhs]))
    }

    fn opt_point_add(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(scope_id, lhs);
        let rhs = self.opt_hir(scope_id, rhs);
        self.db.alloc_lir(Lir::PointAdd(vec![lhs, rhs]))
    }

    fn opt_not(&mut self, scope_id: ScopeId, value: HirId) -> LirId {
        let value = self.opt_hir(scope_id, value);
        self.db.alloc_lir(Lir::Not(value))
    }

    fn opt_raise(&mut self, scope_id: ScopeId, value: Option<HirId>) -> LirId {
        let value = value.map(|value| self.opt_hir(scope_id, value));
        self.db.alloc_lir(Lir::Raise(value))
    }

    fn opt_if(
        &mut self,
        scope_id: ScopeId,
        condition: HirId,
        then_block: HirId,
        else_block: HirId,
    ) -> LirId {
        let condition = self.opt_hir(scope_id, condition);
        let then_branch = self.opt_hir(scope_id, then_block);
        let else_branch = self.opt_hir(scope_id, else_block);
        self.db
            .alloc_lir(Lir::If(condition, then_branch, else_branch))
    }

    fn warning(&mut self, info: WarningKind, range: TextRange) {
        self.diagnostics.push(Diagnostic::new(
            DiagnosticKind::Warning(info),
            range.start().into()..range.end().into(),
        ));
    }
}
