use std::collections::HashMap;

use id_arena::Arena;
use indexmap::{IndexSet, indexset};
use rue_lir::{Lir, LirId, bigint_atom};
use rue_options::CompilerOptions;

use crate::{
    BinaryOp, BindingSymbol, ConstantSymbol, Database, DependencyGraph, Environment, FunctionCall,
    FunctionSymbol, Hir, HirId, Statement, Symbol, SymbolId, UnaryOp,
};

pub struct Lowerer<'d, 'a, 'g> {
    db: &'d mut Database,
    arena: &'a mut Arena<Lir>,
    graph: &'g DependencyGraph,
    inline_symbols: Vec<HashMap<SymbolId, HirId>>,
    options: CompilerOptions,
}

impl<'d, 'a, 'g> Lowerer<'d, 'a, 'g> {
    pub fn new(
        db: &'d mut Database,
        arena: &'a mut Arena<Lir>,
        graph: &'g DependencyGraph,
        options: CompilerOptions,
    ) -> Self {
        Self {
            db,
            arena,
            graph,
            inline_symbols: Vec::new(),
            options,
        }
    }

    pub fn lower_symbol_value(
        &mut self,
        env: &Environment,
        symbol: SymbolId,
        is_main: bool,
    ) -> LirId {
        for inline_symbols in self.inline_symbols.iter().rev() {
            if let Some(hir) = inline_symbols.get(&symbol) {
                return self.lower_hir(env, *hir);
            }
        }

        match self.db.symbol(symbol).clone() {
            Symbol::Module(_) | Symbol::Parameter(_) => unreachable!(),
            Symbol::Function(function) => self.lower_function(env, symbol, function, is_main),
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
            .filter(|&symbol| !self.should_inline(symbol))
            .collect();
        let function_env =
            Environment::new(&captures, &function.parameters, function.nil_terminated);

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
                bindings.push(self.lower_symbol_value(&bind_env, symbol, false));
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
            Hir::Reference(symbol) => self.lower_symbol(env, symbol, false),
            Hir::Block(block) => self.lower_block(env, block.statements, block.body),
            Hir::Lambda(lambda) => self.lower_symbol(env, lambda, true),
            Hir::If(condition, then, else_) => {
                let condition = self.lower_hir(env, condition);
                let then = self.lower_hir(env, then);
                let else_ = self.lower_hir(env, else_);
                self.arena.alloc(Lir::If(condition, then, else_))
            }
            Hir::FunctionCall(call) => self.lower_function_call(env, call),
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
                    UnaryOp::Sha256Inline => self.arena.alloc(Lir::Sha256Inline(vec![lir])),
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
            Hir::CoinId(parent, puzzle, amount) => {
                let parent = self.lower_hir(env, parent);
                let puzzle = self.lower_hir(env, puzzle);
                let amount = self.lower_hir(env, amount);
                self.arena.alloc(Lir::CoinId(parent, puzzle, amount))
            }
            Hir::Substr(hir, start, end) => {
                let hir = self.lower_hir(env, hir);
                let start = self.lower_hir(env, start);
                let end = end.map(|end| self.lower_hir(env, end));
                self.arena.alloc(Lir::Substr(hir, start, end))
            }
        }
    }

    fn lower_function_call(&mut self, env: &Environment, call: FunctionCall) -> LirId {
        if let Hir::Reference(symbol) = self.db.hir(call.function).clone()
            && self.should_inline(symbol)
            && let Symbol::Function(function) = self.db.symbol(symbol).clone()
        {
            let mut inline_symbols = HashMap::new();

            if function.nil_terminated {
                for (i, arg) in call.args.into_iter().enumerate() {
                    inline_symbols.insert(function.parameters[i], arg);
                }
            } else {
                let mut arg_iter = call.args.into_iter().enumerate();

                for (i, arg) in (&mut arg_iter).take(function.parameters.len() - 1) {
                    inline_symbols.insert(function.parameters[i], arg);
                }

                let mut last_arg = self.db.alloc_hir(Hir::Nil);

                for (i, (_, arg)) in arg_iter.rev().enumerate() {
                    if i == 0 && !call.nil_terminated {
                        last_arg = arg;
                    } else {
                        last_arg = self.db.alloc_hir(Hir::Pair(arg, last_arg));
                    }
                }

                if let Some(last_param) = function.parameters.last() {
                    inline_symbols.insert(*last_param, last_arg);
                }
            }

            self.inline_symbols.push(inline_symbols);
            let result = self.lower_hir(env, function.body);
            self.inline_symbols.pop().unwrap();

            return result;
        } else if let Hir::Reference(symbol) = self.db.hir(call.function).clone()
            && let Symbol::Function(_) = self.db.symbol(symbol).clone()
        {
            let function = self.lower_symbol_reference(env, symbol);

            let mut args = Vec::new();

            for capture in self
                .graph
                .dependencies(symbol, true)
                .into_iter()
                .filter(|&symbol| !self.should_inline(symbol))
                .collect::<Vec<_>>()
            {
                args.push(self.lower_symbol_reference(env, capture));
            }

            for arg in call.args {
                args.push(self.lower_hir(env, arg));
            }

            let mut env = self.arena.alloc(Lir::Atom(Vec::new()));

            for (i, &arg) in args.iter().rev().enumerate() {
                if i == 0 && !call.nil_terminated {
                    env = arg;
                } else {
                    env = self.arena.alloc(Lir::Cons(arg, env));
                }
            }

            return self.arena.alloc(Lir::Run(function, env));
        }

        let function = self.lower_hir(env, call.function);

        let mut args = Vec::new();

        for arg in call.args {
            args.push(self.lower_hir(env, arg));
        }

        let mut env = self.arena.alloc(Lir::Atom(Vec::new()));

        for (i, &arg) in args.iter().rev().enumerate() {
            if i == 0 && !call.nil_terminated {
                env = arg;
            } else {
                env = self.arena.alloc(Lir::Cons(arg, env));
            }
        }

        self.arena.alloc(Lir::Run(function, env))
    }

    fn lower_symbol_reference(&mut self, env: &Environment, symbol: SymbolId) -> LirId {
        for inline_symbols in self.inline_symbols.iter().rev() {
            if let Some(hir) = inline_symbols.get(&symbol) {
                return self.lower_hir(env, *hir);
            }
        }

        self.lower_path(env, symbol)
    }

    fn lower_symbol(&mut self, env: &Environment, symbol: SymbolId, is_lambda: bool) -> LirId {
        let mut reference = if self.should_inline(symbol) || is_lambda {
            self.lower_symbol_value(env, symbol, false)
        } else {
            self.lower_symbol_reference(env, symbol)
        };

        if matches!(self.db.symbol(symbol).clone(), Symbol::Function(_)) {
            let captures: Vec<SymbolId> = self
                .graph
                .dependencies(symbol, true)
                .into_iter()
                .filter(|&symbol| !self.should_inline(symbol))
                .collect();

            let mut refs = Vec::new();

            for capture in captures {
                refs.push(self.lower_symbol_reference(env, capture));
            }

            reference = self.arena.alloc(Lir::Closure(reference, refs));
        }

        reference
    }

    fn lower_path(&mut self, env: &Environment, symbol: SymbolId) -> LirId {
        self.arena
            .alloc(Lir::Path(env.try_path(symbol).unwrap_or_else(|| {
                panic!("symbol not found: {}", self.db.debug_symbol(symbol))
            })))
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
                bindings.push(self.lower_symbol_value(&bind_env, symbol, false));
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
                    if !self.should_inline(symbol) {
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

    fn should_inline(&self, symbol: SymbolId) -> bool {
        if self
            .inline_symbols
            .iter()
            .any(|symbols| symbols.contains_key(&symbol))
        {
            return true;
        }

        let references = self.graph.references(symbol);

        match self.db.symbol(symbol) {
            Symbol::Module(_) | Symbol::Parameter(_) => false,
            Symbol::Function(function) => {
                if self.graph.dependencies(symbol, false).contains(&symbol) {
                    return false;
                }

                if function.inline {
                    return true;
                }

                for &parameter in &function.parameters {
                    if self.graph.references(parameter) > 1 {
                        return false;
                    }
                }

                references <= 1 && self.options.auto_inline
            }
            Symbol::Constant(constant) => {
                if self.graph.dependencies(symbol, false).contains(&symbol) {
                    return false;
                }

                if constant.inline {
                    return true;
                }

                references <= 1 && self.options.auto_inline
            }
            Symbol::Binding(binding) => {
                if self.graph.dependencies(symbol, false).contains(&symbol) {
                    return false;
                }

                if binding.inline {
                    return true;
                }

                references <= 1 && self.options.auto_inline
            }
        }
    }
}
