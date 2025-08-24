use std::collections::HashMap;

use id_arena::Arena;
use indexmap::{IndexSet, indexset};
use rue_lir::{Lir, LirId, bigint_atom};

use crate::{
    BinaryOp, BindingSymbol, ConstantSymbol, Database, DependencyGraph, Environment,
    FunctionSymbol, Hir, HirId, Statement, Symbol, SymbolId, UnaryOp, should_inline,
};

pub struct Lowerer<'d, 'a, 'g> {
    db: &'d Database,
    arena: &'a mut Arena<Lir>,
    graph: &'g DependencyGraph,
    inline_symbols: Vec<HashMap<SymbolId, LirId>>,
}

impl<'d, 'a, 'g> Lowerer<'d, 'a, 'g> {
    pub fn new(db: &'d Database, arena: &'a mut Arena<Lir>, graph: &'g DependencyGraph) -> Self {
        Self {
            db,
            arena,
            graph,
            inline_symbols: Vec::new(),
        }
    }

    pub fn lower_symbol(&mut self, env: &Environment, symbol: SymbolId, is_main: bool) -> LirId {
        match self.db.symbol(symbol).clone() {
            Symbol::Function(function) => self.lower_function(env, symbol, function, is_main),
            Symbol::Parameter(_) => unreachable!(),
            Symbol::Constant(constant) => self.lower_constant(env, constant),
            Symbol::Binding(binding) => self.lower_binding(env, binding),
        }
    }

    fn lower_function(
        &mut self,
        parent_env: &Environment,
        symbol: SymbolId,
        function: FunctionSymbol,
        is_main: bool,
    ) -> LirId {
        let captures: Vec<SymbolId> = self
            .graph
            .dependencies(symbol, true)
            .into_iter()
            .filter(|&symbol| !should_inline(self.db, self.graph, symbol))
            .collect();
        let function_env = Environment::new(&captures, &function.parameters);

        let binding_groups = self.group_symbols(if is_main {
            captures.into_iter().collect()
        } else {
            indexset![]
        });

        let mut expr = self.lower_hir(&function_env, function.body);

        for (i, group) in binding_groups.iter().enumerate().rev() {
            expr = self.arena.alloc(Lir::Quote(expr));

            let mut bind_env = parent_env.clone();

            for existing_group in binding_groups.iter().take(i) {
                bind_env = bind_env.with_bindings(existing_group);
            }

            let mut bindings = Vec::new();

            for &symbol in group {
                bindings.push(self.lower_symbol(&bind_env, symbol, false));
            }

            expr = self.arena.alloc(Lir::Curry(expr, bindings));
        }

        if is_main {
            expr
        } else {
            self.arena.alloc(Lir::Quote(expr))
        }
    }

    fn lower_constant(&mut self, env: &Environment, constant: ConstantSymbol) -> LirId {
        self.lower_hir(env, constant.value)
    }

    fn lower_binding(&mut self, env: &Environment, binding: BindingSymbol) -> LirId {
        self.lower_hir(env, binding.value)
    }

    fn lower_hir(&mut self, env: &Environment, hir: HirId) -> LirId {
        match self.db.hir(hir).clone() {
            Hir::Unresolved => unreachable!(),
            Hir::Nil => self.arena.alloc(Lir::Atom(vec![])),
            Hir::String(value) => self.arena.alloc(Lir::Atom(value.as_bytes().to_vec())),
            Hir::Int(value) => self.arena.alloc(Lir::Atom(bigint_atom(value.clone()))),
            Hir::Bool(value) => self
                .arena
                .alloc(Lir::Atom(if value { vec![1] } else { vec![] })),
            Hir::Bytes(atom) => self.arena.alloc(Lir::Atom(atom)),
            Hir::Pair(first, rest) => {
                let first = self.lower_hir(env, first);
                let rest = self.lower_hir(env, rest);
                self.arena.alloc(Lir::Cons(first, rest))
            }
            Hir::Reference(symbol) => self.lower_reference(env, symbol, false),
            Hir::Block(block) => self.lower_block(env, block.statements, block.body),
            Hir::Lambda(lambda) => self.lower_reference(env, lambda, true),
            Hir::If(condition, then, else_) => {
                let condition = self.lower_hir(env, condition);
                let then = self.lower_hir(env, then);
                let else_ = self.lower_hir(env, else_);
                self.arena.alloc(Lir::If(condition, then, else_))
            }
            Hir::FunctionCall(function, args) => self.lower_function_call(env, function, args),
            Hir::Unary(op, hir) => {
                let lir = self.lower_hir(env, hir);
                match op {
                    UnaryOp::Listp => self.arena.alloc(Lir::Listp(lir)),
                    UnaryOp::First => self.arena.alloc(Lir::First(lir)),
                    UnaryOp::Rest => self.arena.alloc(Lir::Rest(lir)),
                    UnaryOp::Strlen => self.arena.alloc(Lir::Strlen(lir)),
                    UnaryOp::Not => self.arena.alloc(Lir::Not(lir)),
                    UnaryOp::Neg => {
                        let zero = self.arena.alloc(Lir::Atom(vec![]));
                        self.arena.alloc(Lir::Sub(vec![zero, lir]))
                    }
                    UnaryOp::BitwiseNot => self.arena.alloc(Lir::Lognot(lir)),
                    UnaryOp::Sha256 => self.arena.alloc(Lir::Sha256(vec![lir])),
                }
            }
            Hir::Binary(op, left, right) => {
                let left = self.lower_hir(env, left);
                let right = self.lower_hir(env, right);
                match op {
                    BinaryOp::Add => self.arena.alloc(Lir::Add(vec![left, right])),
                    BinaryOp::Sub => self.arena.alloc(Lir::Sub(vec![left, right])),
                    BinaryOp::Mul => self.arena.alloc(Lir::Mul(vec![left, right])),
                    BinaryOp::Div => self.arena.alloc(Lir::Div(left, right)),
                    BinaryOp::Mod => self.arena.alloc(Lir::Mod(left, right)),
                    BinaryOp::Concat => self.arena.alloc(Lir::Concat(vec![left, right])),
                    BinaryOp::BitwiseAnd => self.arena.alloc(Lir::Logand(vec![left, right])),
                    BinaryOp::BitwiseOr => self.arena.alloc(Lir::Logior(vec![left, right])),
                    BinaryOp::BitwiseXor => self.arena.alloc(Lir::Logxor(vec![left, right])),
                    BinaryOp::LeftShift => {
                        let zero = self.arena.alloc(Lir::Atom(vec![]));
                        let neg = self.arena.alloc(Lir::Sub(vec![zero, right]));
                        self.arena.alloc(Lir::Ash(left, neg))
                    }
                    BinaryOp::RightShift => self.arena.alloc(Lir::Ash(left, right)),
                    BinaryOp::Gt => self.arena.alloc(Lir::Gt(left, right)),
                    BinaryOp::Lt => self.arena.alloc(Lir::Gt(right, left)),
                    BinaryOp::Gte => {
                        let gt = self.arena.alloc(Lir::Gt(left, right));
                        let eq = self.arena.alloc(Lir::Eq(left, right));
                        self.arena.alloc(Lir::Any(vec![gt, eq]))
                    }
                    BinaryOp::Lte => {
                        let lt = self.arena.alloc(Lir::Gt(right, left));
                        let eq = self.arena.alloc(Lir::Eq(left, right));
                        self.arena.alloc(Lir::Any(vec![lt, eq]))
                    }
                    BinaryOp::GtBytes => self.arena.alloc(Lir::GtBytes(left, right)),
                    BinaryOp::LtBytes => self.arena.alloc(Lir::GtBytes(right, left)),
                    BinaryOp::GteBytes => {
                        let gt = self.arena.alloc(Lir::GtBytes(left, right));
                        let eq = self.arena.alloc(Lir::Eq(left, right));
                        self.arena.alloc(Lir::Any(vec![gt, eq]))
                    }
                    BinaryOp::LteBytes => {
                        let lt = self.arena.alloc(Lir::GtBytes(right, left));
                        let eq = self.arena.alloc(Lir::Eq(left, right));
                        self.arena.alloc(Lir::Any(vec![lt, eq]))
                    }
                    BinaryOp::Eq => self.arena.alloc(Lir::Eq(left, right)),
                    BinaryOp::Ne => {
                        let eq = self.arena.alloc(Lir::Eq(left, right));
                        self.arena.alloc(Lir::Not(eq))
                    }
                    BinaryOp::And => {
                        let true_atom = self.arena.alloc(Lir::Atom(vec![1]));
                        let false_atom = self.arena.alloc(Lir::Atom(vec![]));
                        let right = self.arena.alloc(Lir::If(right, true_atom, false_atom));
                        self.arena.alloc(Lir::If(left, right, false_atom))
                    }
                    BinaryOp::Or => {
                        let true_atom = self.arena.alloc(Lir::Atom(vec![1]));
                        let false_atom = self.arena.alloc(Lir::Atom(vec![]));
                        let right = self.arena.alloc(Lir::If(right, true_atom, false_atom));
                        self.arena.alloc(Lir::If(left, true_atom, right))
                    }
                    BinaryOp::All => self.arena.alloc(Lir::All(vec![left, right])),
                    BinaryOp::Any => self.arena.alloc(Lir::Any(vec![left, right])),
                }
            }
        }
    }

    fn lower_function_call(
        &mut self,
        env: &Environment,
        function: HirId,
        args: Vec<HirId>,
    ) -> LirId {
        if let Hir::Reference(symbol) = self.db.hir(function)
            && should_inline(self.db, self.graph, *symbol)
            && let Symbol::Function(function) = self.db.symbol(*symbol)
        {
            assert_eq!(function.parameters.len(), args.len());

            let mut inline_symbols = HashMap::new();

            for (i, arg) in args.iter().enumerate() {
                inline_symbols.insert(function.parameters[i], self.lower_hir(env, *arg));
            }

            self.inline_symbols.push(inline_symbols);
            let result = self.lower_hir(env, function.body);
            self.inline_symbols.pop().unwrap();

            return result;
        }

        let function = self.lower_hir(env, function);
        let args = args.iter().map(|arg| self.lower_hir(env, *arg)).collect();
        self.arena.alloc(Lir::Run(function, args))
    }

    fn lower_reference(&mut self, env: &Environment, symbol: SymbolId, is_lambda: bool) -> LirId {
        for inline_symbols in self.inline_symbols.iter().rev() {
            if let Some(lir) = inline_symbols.get(&symbol) {
                return *lir;
            }
        }

        let mut reference = if should_inline(self.db, self.graph, symbol) || is_lambda {
            self.lower_symbol(env, symbol, false)
        } else {
            self.arena.alloc(Lir::Path(env.path(symbol)))
        };

        if matches!(self.db.symbol(symbol).clone(), Symbol::Function(_)) {
            let captures: Vec<SymbolId> = self
                .graph
                .dependencies(symbol, true)
                .into_iter()
                .filter(|&symbol| !should_inline(self.db, self.graph, symbol))
                .collect();

            let mut refs = Vec::new();

            for capture in captures {
                refs.push(self.arena.alloc(Lir::Path(env.path(capture))));
            }

            reference = self.arena.alloc(Lir::Closure(reference, refs));
        }

        reference
    }

    fn lower_block(
        &mut self,
        env: &Environment,
        stmts: Vec<Statement>,
        body: Option<HirId>,
    ) -> LirId {
        let Some(stmt) = stmts.first() else {
            return if let Some(body) = body {
                self.lower_hir(env, body)
            } else {
                self.arena.alloc(Lir::Atom(vec![]))
            };
        };

        match stmt.clone() {
            Statement::Let(_) => self.lower_let_stmts(env, stmts, body),
            Statement::Return(hir) => self.lower_block(env, vec![], Some(hir)),
            Statement::Assert(condition) => self.lower_assert(env, stmts, condition, body),
            Statement::Expr(hir) => self.lower_block(env, stmts, body.or(Some(hir))),
            Statement::Raise(hir) => self.lower_raise(env, hir),
            Statement::If(condition, then) => self.lower_if(env, stmts, condition, then, body),
        }
    }

    fn lower_let_stmts(
        &mut self,
        env: &Environment,
        mut stmts: Vec<Statement>,
        body: Option<HirId>,
    ) -> LirId {
        let mut symbols = IndexSet::new();

        while let Some(stmt) = stmts.first()
            && let Statement::Let(symbol) = stmt
        {
            symbols.insert(*symbol);
            stmts.remove(0);
        }

        let binding_groups = self.group_symbols(symbols);

        let all_bindings: Vec<SymbolId> = binding_groups.iter().rev().flatten().copied().collect();

        let body_env = env.with_bindings(&all_bindings);

        let mut expr = self.lower_block(&body_env, stmts, body);

        for (i, group) in binding_groups.iter().enumerate().rev() {
            expr = self.arena.alloc(Lir::Quote(expr));

            let mut bind_env = env.clone();

            for existing_group in binding_groups.iter().take(i) {
                bind_env = bind_env.with_bindings(existing_group);
            }

            let mut bindings = Vec::new();

            for &symbol in group {
                bindings.push(self.lower_symbol(&bind_env, symbol, false));
            }

            expr = self.arena.alloc(Lir::Curry(expr, bindings));
        }

        expr
    }

    fn lower_assert(
        &mut self,
        env: &Environment,
        mut stmts: Vec<Statement>,
        condition: HirId,
        body: Option<HirId>,
    ) -> LirId {
        stmts.remove(0);
        let condition = self.lower_hir(env, condition);
        let then_branch = self.lower_block(env, stmts, body);
        let else_branch = self.arena.alloc(Lir::Raise(vec![]));
        self.arena
            .alloc(Lir::If(condition, then_branch, else_branch))
    }

    fn lower_raise(&mut self, env: &Environment, hir: HirId) -> LirId {
        let lir = self.lower_hir(env, hir);
        self.arena.alloc(Lir::Raise(vec![lir]))
    }

    fn lower_if(
        &mut self,
        env: &Environment,
        mut stmts: Vec<Statement>,
        condition: HirId,
        then: HirId,
        body: Option<HirId>,
    ) -> LirId {
        stmts.remove(0);
        let condition = self.lower_hir(env, condition);
        let then_branch = self.lower_hir(env, then);
        let else_branch = self.lower_block(env, stmts, body);
        self.arena
            .alloc(Lir::If(condition, then_branch, else_branch))
    }

    fn group_symbols(&mut self, mut symbols: IndexSet<SymbolId>) -> Vec<Vec<SymbolId>> {
        let mut groups = Vec::new();

        while !symbols.is_empty() {
            let mut group = Vec::new();

            let remaining = symbols.clone();

            symbols.retain(|&symbol| {
                if self
                    .graph
                    .dependencies(symbol, true)
                    .iter()
                    .all(|symbol| !remaining.contains(symbol))
                    || matches!(self.db.symbol(symbol), Symbol::Function(_))
                {
                    if !should_inline(self.db, self.graph, symbol) {
                        group.push(symbol);
                    }
                    false
                } else {
                    true
                }
            });

            if group.is_empty() {
                continue;
            }

            groups.push(group);
        }

        groups
    }
}
