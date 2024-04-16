use std::collections::HashMap;

use indexmap::IndexSet;

use crate::{
    database::{Database, HirId, LirId, ScopeId, SymbolId},
    hir::{Hir, HirBinaryOp},
    lir::Lir,
    symbol::Symbol,
};

pub struct Optimizer<'a> {
    db: &'a mut Database,
    captures: HashMap<ScopeId, IndexSet<SymbolId>>,
    environments: HashMap<ScopeId, IndexSet<SymbolId>>,
    varargs: HashMap<ScopeId, bool>,
    scope_inheritance: HashMap<ScopeId, ScopeId>,
}

impl<'a> Optimizer<'a> {
    pub fn new(db: &'a mut Database) -> Self {
        Self {
            db,
            captures: HashMap::new(),
            environments: HashMap::new(),
            varargs: HashMap::new(),
            scope_inheritance: HashMap::new(),
        }
    }

    fn compute_captures_entrypoint(&mut self, scope_id: ScopeId, hir_id: HirId) {
        if self.captures.contains_key(&scope_id) {
            return;
        }
        self.captures.insert(scope_id, IndexSet::new());
        self.compute_captures_hir(scope_id, hir_id);
    }

    fn compute_captures_hir(&mut self, scope_id: ScopeId, hir_id: HirId) {
        match self.db.hir(hir_id).clone() {
            Hir::Unknown => unreachable!(),
            Hir::Atom(_) => {}
            Hir::Reference(symbol_id) => self.compute_reference_captures(scope_id, symbol_id),
            Hir::Scope {
                scope_id: new_scope_id,
                value,
            } => self.compute_scope_captures(scope_id, new_scope_id, value),
            Hir::FunctionCall { callee, args } => {
                self.compute_captures_hir(scope_id, callee);
                self.compute_captures_hir(scope_id, args);
            }
            Hir::BinaryOp { lhs, rhs, .. } => {
                self.compute_captures_hir(scope_id, lhs);
                self.compute_captures_hir(scope_id, rhs);
            }
            Hir::Raise(value) => {
                if let Some(value) = value {
                    self.compute_captures_hir(scope_id, value);
                }
            }
            Hir::First(value)
            | Hir::Rest(value)
            | Hir::Not(value)
            | Hir::Sha256(value)
            | Hir::IsCons(value)
            | Hir::Strlen(value) => self.compute_captures_hir(scope_id, value),
            Hir::If {
                condition,
                then_block,
                else_block,
            } => {
                self.compute_captures_hir(scope_id, condition);
                self.compute_captures_hir(scope_id, then_block);
                self.compute_captures_hir(scope_id, else_block);
            }
            Hir::Pair(first, rest) => {
                self.compute_captures_hir(scope_id, first);
                self.compute_captures_hir(scope_id, rest);
            }
        }
    }

    fn compute_reference_captures(&mut self, scope_id: ScopeId, symbol_id: SymbolId) {
        let is_capturable = self.db.symbol(symbol_id).is_capturable();
        let is_local = self.db.scope(scope_id).is_local(symbol_id);

        if is_capturable && !is_local {
            self.captures.get_mut(&scope_id).unwrap().insert(symbol_id);
        }

        match self.db.symbol(symbol_id).clone() {
            Symbol::Function {
                scope_id: function_scope_id,
                hir_id,
                ty,
                ..
            } => self.compute_function_captures(scope_id, function_scope_id, hir_id, ty.varargs()),
            Symbol::Parameter { .. } => {}
            Symbol::LetBinding { hir_id, .. } => self.compute_captures_hir(scope_id, hir_id),
            Symbol::ConstBinding { hir_id, .. } => self.compute_captures_hir(scope_id, hir_id),
        }
    }

    fn compute_function_captures(
        &mut self,
        scope_id: ScopeId,
        function_scope_id: ScopeId,
        hir_id: HirId,
        varargs: bool,
    ) {
        self.compute_captures_entrypoint(function_scope_id, hir_id);

        let new_captures: Vec<SymbolId> = self.captures[&function_scope_id]
            .iter()
            .copied()
            .filter(|&id| !self.db.scope(scope_id).is_local(id))
            .collect();

        self.captures
            .get_mut(&scope_id)
            .unwrap()
            .extend(new_captures);

        let mut env = IndexSet::new();

        for symbol_id in self.db.scope(function_scope_id).local_symbols() {
            if self.db.symbol(symbol_id).is_definition() {
                env.insert(symbol_id);
            }
        }

        for symbol_id in self.captures[&function_scope_id].clone() {
            env.insert(symbol_id);
        }

        for symbol_id in self.db.scope(function_scope_id).local_symbols() {
            if self.db.symbol(symbol_id).is_parameter() {
                env.insert(symbol_id);
            }
        }

        self.environments.insert(function_scope_id, env);

        if varargs {
            self.varargs.insert(function_scope_id, true);
        }
    }

    fn compute_scope_captures(&mut self, scope_id: ScopeId, new_scope_id: ScopeId, value: HirId) {
        self.compute_captures_entrypoint(new_scope_id, value);

        let new_captures: Vec<SymbolId> = self.captures[&new_scope_id]
            .iter()
            .copied()
            .filter(|&id| !self.db.scope(scope_id).is_local(id))
            .collect();

        self.captures
            .get_mut(&scope_id)
            .unwrap()
            .extend(new_captures);

        let mut env = IndexSet::new();

        for symbol_id in self.db.scope(new_scope_id).local_symbols() {
            assert!(self.db.symbol(symbol_id).is_definition());
            env.insert(symbol_id);
        }

        self.scope_inheritance.insert(new_scope_id, scope_id);
        self.environments.insert(new_scope_id, env);
    }

    pub fn opt_main(&mut self, main: SymbolId) -> LirId {
        let Symbol::Function {
            scope_id, hir_id, ..
        } = self.db.symbol(main).clone()
        else {
            unreachable!();
        };

        self.compute_captures_entrypoint(scope_id, hir_id);

        let mut env = IndexSet::new();

        for symbol_id in self.db.scope(scope_id).local_symbols() {
            if self.db.symbol(symbol_id).is_definition() {
                env.insert(symbol_id);
            }
        }

        for symbol_id in self.captures[&scope_id].clone() {
            env.insert(symbol_id);
        }

        for symbol_id in self.db.scope(scope_id).local_symbols() {
            if self.db.symbol(symbol_id).is_parameter() {
                env.insert(symbol_id);
            }
        }

        self.environments.insert(scope_id, env);

        let body = self.opt_hir(scope_id, hir_id);

        let mut args = Vec::new();

        for symbol_id in self.db.scope(scope_id).local_symbols() {
            if self.db.symbol(symbol_id).is_definition() {
                args.push(self.opt_definition(scope_id, symbol_id));
            }
        }

        for symbol_id in self.captures[&scope_id].clone() {
            args.push(self.opt_definition(scope_id, symbol_id));
        }

        self.db.alloc_lir(Lir::Curry(body, args))
    }

    fn opt_scope(&mut self, parent_scope_id: ScopeId, scope_id: ScopeId, hir_id: HirId) -> LirId {
        let body = self.opt_hir(scope_id, hir_id);
        let mut args = Vec::new();
        for symbol_id in self.environments[&scope_id].clone() {
            assert!(self.db.symbol(symbol_id).is_definition());
            args.push(self.opt_definition(parent_scope_id, symbol_id));
        }
        self.db.alloc_lir(Lir::Curry(body, args))
    }

    fn opt_path(&mut self, scope_id: ScopeId, symbol_id: SymbolId) -> LirId {
        let mut environment = self.environments[&scope_id].clone();

        let mut current_scope_id = scope_id;

        while self.scope_inheritance.contains_key(&current_scope_id) {
            current_scope_id = self.scope_inheritance[&current_scope_id];
            environment.extend(&self.environments[&current_scope_id]);
        }

        let index = environment
            .iter()
            .position(|&id| id == symbol_id)
            .expect("symbol not found");

        let mut path = 2;
        for _ in 0..index {
            path *= 2;
            path += 1;
        }

        if index + 1 == environment.len() && self.varargs.get(&scope_id).copied().unwrap_or(false) {
            // Undo last index
            path -= 1;
            path /= 2;

            // Make it the rest
            path += 1;
        }

        self.db.alloc_lir(Lir::Path(path))
    }

    fn opt_definition(&mut self, scope_id: ScopeId, symbol_id: SymbolId) -> LirId {
        match self.db.symbol(symbol_id).clone() {
            Symbol::Function {
                scope_id: function_scope_id,
                hir_id,
                ..
            } => {
                let mut body = self.opt_hir(function_scope_id, hir_id);
                let mut definitions = Vec::new();

                for symbol_id in self.db.scope(function_scope_id).local_symbols() {
                    if self.db.symbol(symbol_id).is_definition() {
                        definitions.push(self.opt_definition(function_scope_id, symbol_id));
                    }
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
            Hir::Unknown => unreachable!(),
            Hir::Atom(atom) => self.db.alloc_lir(Lir::Atom(atom.clone())),
            Hir::Pair(first, rest) => self.opt_pair(scope_id, *first, *rest),
            Hir::Reference(symbol_id) => self.opt_reference(scope_id, *symbol_id),
            Hir::Scope {
                scope_id: new_scope_id,
                value,
            } => self.opt_scope(scope_id, *new_scope_id, *value),
            Hir::FunctionCall { callee, args } => self.opt_function_call(scope_id, *callee, *args),
            Hir::BinaryOp { op, lhs, rhs } => {
                let handler = match op {
                    HirBinaryOp::Add => Self::opt_add,
                    HirBinaryOp::Subtract => Self::opt_subtract,
                    HirBinaryOp::Multiply => Self::opt_multiply,
                    HirBinaryOp::Divide => Self::opt_divide,
                    HirBinaryOp::Remainder => Self::opt_remainder,
                    HirBinaryOp::LessThan => Self::opt_lt,
                    HirBinaryOp::GreaterThan => Self::opt_gt,
                    HirBinaryOp::LessThanEquals => Self::opt_lteq,
                    HirBinaryOp::GreaterThanEquals => Self::opt_gteq,
                    HirBinaryOp::Equals => Self::opt_eq,
                    HirBinaryOp::NotEquals => Self::opt_neq,
                    HirBinaryOp::Concat => Self::opt_concat,
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

                for symbol_id in self.captures[&function_scope_id].clone() {
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
                for symbol_id in self.captures[&callee_scope_id].clone().into_iter().rev() {
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
}
