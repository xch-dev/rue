#![allow(clippy::needless_pass_by_value)]

use std::{collections::HashMap, mem};

use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};
use rue_diagnostic::SrcLoc;
use rue_lir::{Lir, LirId, bigint_atom};
use rue_options::CompilerOptions;

use crate::{
    BinaryOp, BindingSymbol, ConstantSymbol, Database, DependencyGraph, Environment, FunctionCall,
    FunctionKind, FunctionSymbol, Hir, HirId, IfStatement, Statement, Symbol, SymbolId, UnaryOp,
};

#[derive(Debug, Clone)]
enum SymbolGroup {
    Sequential(Vec<SymbolId>),
    Tree(Environment),
}

#[derive(Debug)]
pub struct Lowerer<'d, 'a, 'g> {
    db: &'d mut Database,
    arena: &'a mut Arena<Lir>,
    graph: &'g DependencyGraph,
    inline_symbols: Vec<HashMap<SymbolId, HirId>>,
    options: CompilerOptions,
    main: SymbolId,
}

impl<'d, 'a, 'g> Lowerer<'d, 'a, 'g> {
    pub fn new(
        db: &'d mut Database,
        arena: &'a mut Arena<Lir>,
        graph: &'g DependencyGraph,
        options: CompilerOptions,
        main: SymbolId,
    ) -> Self {
        Self {
            db,
            arena,
            graph,
            inline_symbols: Vec::new(),
            options,
            main,
        }
    }

    pub fn lower_symbol_value(&mut self, env: &Environment, symbol: SymbolId) -> LirId {
        for inline_symbols in self.inline_symbols.iter().rev() {
            if let Some(hir) = inline_symbols.get(&symbol) {
                return self.lower_hir(env, *hir);
            }
        }

        match self.db.symbol(symbol).clone() {
            Symbol::Unresolved | Symbol::Module(_) | Symbol::Parameter(_) | Symbol::Builtin(_) => {
                unreachable!()
            }
            Symbol::Function(function) => self.lower_function(env, symbol, function),
            Symbol::Constant(constant) => self.lower_constant(env, constant),
            Symbol::Binding(binding) => self.lower_binding(env, binding),
        }
    }

    fn function_groups(
        &mut self,
        symbol: SymbolId,
        function: &FunctionSymbol,
    ) -> (Vec<SymbolGroup>, SymbolGroup) {
        let captures: Vec<SymbolId> = self
            .graph
            .dependencies(symbol, true)
            .into_iter()
            .filter(|&symbol| !self.should_inline(symbol))
            .collect();

        let capture_groups =
            self.group_symbols(captures.into_iter().collect(), symbol != self.main);

        let param_group = self.create_group(
            function.parameters.clone(),
            function.kind == FunctionKind::BinaryTree
                && !self.graph.is_closure(symbol)
                && symbol != self.main,
        );

        (capture_groups, param_group)
    }

    fn lower_function(
        &mut self,
        parent_env: &Environment,
        symbol: SymbolId,
        function: FunctionSymbol,
    ) -> LirId {
        let (capture_groups, param_group) = self.function_groups(symbol, &function);

        let mut function_env = Self::apply_group(
            Environment::Nil,
            &param_group,
            function.nil_terminated && matches!(param_group, SymbolGroup::Sequential(_)),
        );

        for group in &capture_groups {
            function_env = Self::apply_group(function_env, group, true);
        }

        let mut expr = self.lower_hir(&function_env, function.body);

        if symbol == self.main {
            for (i, group) in capture_groups.iter().enumerate().rev() {
                expr = self.arena.alloc(Lir::Quote(expr));

                let mut bind_env = parent_env.clone();

                for existing_group in capture_groups.iter().take(i) {
                    bind_env = Self::apply_group(bind_env, existing_group, true);
                }

                let rest = self.arena.alloc(Lir::Path(1));
                let group_env =
                    self.lower_group_environment(&bind_env, group, rest, false, None, true);

                expr = self.arena.alloc(Lir::Run(expr, group_env));
            }

            expr
        } else {
            self.arena.alloc(Lir::Quote(expr))
        }
    }

    fn lower_constant(&mut self, env: &Environment, constant: ConstantSymbol) -> LirId {
        self.lower_hir(env, constant.value.hir)
    }

    fn lower_binding(&mut self, env: &Environment, binding: BindingSymbol) -> LirId {
        self.lower_hir(env, binding.value.hir)
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
            Hir::If(condition, then, else_, inline) => {
                let condition = self.lower_hir(env, condition);
                let then = self.lower_hir(env, then);
                let else_ = self.lower_hir(env, else_);
                self.arena.alloc(Lir::If(condition, then, else_, inline))
            }
            Hir::FunctionCall(call) => self.lower_function_call(env, call),
            Hir::Unary(op, hir) => {
                let lir = self.lower_hir(env, hir);
                match op {
                    UnaryOp::Listp { can_be_truthy } => {
                        self.arena.alloc(Lir::Listp(lir, can_be_truthy))
                    }
                    UnaryOp::First => self.arena.alloc(Lir::First(lir)),
                    UnaryOp::Rest => self.arena.alloc(Lir::Rest(lir)),
                    UnaryOp::Strlen => self.arena.alloc(Lir::Strlen(lir)),
                    UnaryOp::Not => self.arena.alloc(Lir::Not(lir)),
                    UnaryOp::Neg => {
                        let zero = self.arena.alloc(Lir::Atom(vec![]));
                        self.arena.alloc(Lir::Sub(vec![zero, lir]))
                    }
                    UnaryOp::BitwiseNot => self.arena.alloc(Lir::Lognot(lir)),
                    UnaryOp::G1Negate => self.arena.alloc(Lir::G1Negate(lir)),
                    UnaryOp::G2Negate => self.arena.alloc(Lir::G2Negate(lir)),
                    UnaryOp::Sha256 => self.arena.alloc(Lir::Sha256(vec![lir])),
                    UnaryOp::Sha256Inline => self.arena.alloc(Lir::Sha256Inline(vec![lir])),
                    UnaryOp::Keccak256 => self.arena.alloc(Lir::Keccak256(vec![lir])),
                    UnaryOp::Keccak256Inline => self.arena.alloc(Lir::Keccak256Inline(vec![lir])),
                    UnaryOp::PubkeyForExp => self.arena.alloc(Lir::PubkeyForExp(lir)),
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
                    BinaryOp::Divmod => self.arena.alloc(Lir::Divmod(left, right)),
                    BinaryOp::Concat => self.arena.alloc(Lir::Concat(vec![left, right])),
                    BinaryOp::G1Add => self.arena.alloc(Lir::G1Add(vec![left, right])),
                    BinaryOp::G1Subtract => self.arena.alloc(Lir::G1Subtract(vec![left, right])),
                    BinaryOp::G1Multiply => self.arena.alloc(Lir::G1Multiply(left, right)),
                    BinaryOp::G2Add => self.arena.alloc(Lir::G2Add(vec![left, right])),
                    BinaryOp::G2Subtract => self.arena.alloc(Lir::G2Subtract(vec![left, right])),
                    BinaryOp::G2Multiply => self.arena.alloc(Lir::G2Multiply(left, right)),
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
                        let right = self
                            .arena
                            .alloc(Lir::If(right, true_atom, false_atom, false));
                        self.arena.alloc(Lir::If(left, right, false_atom, false))
                    }
                    BinaryOp::Or => {
                        let true_atom = self.arena.alloc(Lir::Atom(vec![1]));
                        let false_atom = self.arena.alloc(Lir::Atom(vec![]));
                        let right = self
                            .arena
                            .alloc(Lir::If(right, true_atom, false_atom, false));
                        self.arena.alloc(Lir::If(left, true_atom, right, false))
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
            Hir::G1Map(data, dst) => {
                let data = self.lower_hir(env, data);
                let dst = dst.map(|dst| self.lower_hir(env, dst));
                self.arena.alloc(Lir::G1Map(data, dst))
            }
            Hir::G2Map(data, dst) => {
                let data = self.lower_hir(env, data);
                let dst = dst.map(|dst| self.lower_hir(env, dst));
                self.arena.alloc(Lir::G2Map(data, dst))
            }
            Hir::Modpow(base, exponent, modulus) => {
                let base = self.lower_hir(env, base);
                let exponent = self.lower_hir(env, exponent);
                let modulus = self.lower_hir(env, modulus);
                self.arena.alloc(Lir::Modpow(base, exponent, modulus))
            }
            Hir::BlsPairingIdentity(args) => {
                let args = args
                    .into_iter()
                    .map(|arg| self.lower_hir(env, arg))
                    .collect();
                self.arena.alloc(Lir::BlsPairingIdentity(args))
            }
            Hir::BlsVerify(sig, args) => {
                let sig = self.lower_hir(env, sig);
                let args = args
                    .into_iter()
                    .map(|arg| self.lower_hir(env, arg))
                    .collect();
                self.arena.alloc(Lir::BlsVerify(sig, args))
            }
            Hir::Secp256K1Verify(sig, pk, msg) => {
                let sig = self.lower_hir(env, sig);
                let pk = self.lower_hir(env, pk);
                let msg = self.lower_hir(env, msg);
                self.arena.alloc(Lir::K1Verify(sig, pk, msg))
            }
            Hir::Secp256R1Verify(sig, pk, msg) => {
                let sig = self.lower_hir(env, sig);
                let pk = self.lower_hir(env, pk);
                let msg = self.lower_hir(env, msg);
                self.arena.alloc(Lir::R1Verify(sig, pk, msg))
            }
            Hir::InfinityG1 => self.arena.alloc(Lir::G1Add(vec![])),
            Hir::InfinityG2 => self.arena.alloc(Lir::G2Add(vec![])),
            Hir::ClvmOp(op, args) => {
                let args = self.lower_hir(env, args);
                self.arena.alloc(Lir::Op(op, args))
            }
        }
    }

    fn lower_function_call(&mut self, env: &Environment, call: FunctionCall) -> LirId {
        if let Hir::Reference(symbol) = self.db.hir(call.function).clone()
            && let Symbol::Function(function) = self.db.symbol(symbol).clone()
        {
            let mut args = HashMap::new();

            if function.nil_terminated {
                for (i, arg) in call.args.into_iter().enumerate() {
                    args.insert(function.parameters[i], arg);
                }
            } else {
                let mut arg_iter = call.args.into_iter().enumerate();

                for (i, arg) in (&mut arg_iter).take(function.parameters.len() - 1) {
                    args.insert(function.parameters[i], arg);
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
                    args.insert(*last_param, last_arg);
                }
            }

            if self.should_inline(symbol) {
                self.inline_symbols.push(args);
                let result = self.lower_hir(env, function.body);
                self.inline_symbols.pop().unwrap();
                return result;
            }

            let (capture_groups, param_group) = self.function_groups(symbol, &function);

            let function_lir = self.lower_symbol_reference(env, symbol);

            let rest = self.arena.alloc(Lir::Atom(vec![]));

            let mut arg_env = self.lower_group_environment(
                env,
                &param_group,
                rest,
                false,
                Some(&args),
                function.nil_terminated && matches!(param_group, SymbolGroup::Sequential(_)),
            );

            for group in &capture_groups {
                arg_env = self.lower_group_environment(env, group, arg_env, true, None, true);
            }

            return self.arena.alloc(Lir::Run(function_lir, arg_env));
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
            self.lower_symbol_value(env, symbol)
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
            .alloc(Lir::Path(env.path(symbol).unwrap_or_else(|| {
                panic!("symbol not found: {}", self.db.debug_symbol(symbol))
            })))
    }

    fn lower_block(
        &mut self,
        env: &Environment,
        stmts: Vec<Statement>,
        body: Option<HirId>,
    ) -> LirId {
        let Some(stmt) = stmts.first().cloned() else {
            return if let Some(body) = body {
                self.lower_hir(env, body)
            } else {
                self.arena.alloc(Lir::Atom(vec![]))
            };
        };

        match stmt {
            Statement::Let(_) => self.lower_let_stmts(env, stmts, body),
            Statement::Return(hir) => self.lower_block(env, vec![], Some(hir)),
            Statement::Assert(condition, srcloc) => {
                self.lower_assert(env, stmts, condition, srcloc, body)
            }
            Statement::Expr(_) => self.lower_expr_stmts(env, stmts, body),
            Statement::Raise(hir, srcloc) => self.lower_raise(env, hir, srcloc),
            Statement::If(stmt) => self.lower_if(env, stmts, stmt, body),
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

        let binding_groups = self.group_symbols(symbols, false);

        let mut body_env = env.clone();

        for group in &binding_groups {
            body_env = Self::apply_group(body_env, group, true);
        }

        let mut expr = self.lower_block(&body_env, stmts, body);

        for (i, group) in binding_groups.iter().enumerate().rev() {
            expr = self.arena.alloc(Lir::Quote(expr));

            let mut bind_env = env.clone();

            for group in binding_groups.iter().take(i) {
                bind_env = Self::apply_group(bind_env, group, true);
            }

            let rest = self.arena.alloc(Lir::Path(1));
            let group_env = self.lower_group_environment(&bind_env, group, rest, false, None, true);

            expr = self.arena.alloc(Lir::Run(expr, group_env));
        }

        expr
    }

    fn lower_group_environment(
        &mut self,
        env: &Environment,
        group: &SymbolGroup,
        rest: LirId,
        by_reference: bool,
        map: Option<&HashMap<SymbolId, HirId>>,
        include_rest: bool,
    ) -> LirId {
        match group {
            SymbolGroup::Sequential(symbols) => {
                let mut result = rest;

                for (i, &symbol) in symbols.iter().rev().enumerate() {
                    let value = if let Some(hir) = map.and_then(|map| map.get(&symbol)) {
                        self.lower_hir(env, *hir)
                    } else if by_reference {
                        self.lower_symbol_reference(env, symbol)
                    } else {
                        self.lower_symbol_value(env, symbol)
                    };
                    if !include_rest && i == 0 {
                        result = value;
                    } else {
                        result = self.arena.alloc(Lir::Cons(value, result));
                    }
                }

                result
            }
            SymbolGroup::Tree(tree) => {
                let tree = self.lower_tree(env, tree, by_reference, map);
                if include_rest {
                    self.arena.alloc(Lir::Cons(tree, rest))
                } else {
                    tree
                }
            }
        }
    }

    fn lower_tree(
        &mut self,
        env: &Environment,
        tree: &Environment,
        by_reference: bool,
        map: Option<&HashMap<SymbolId, HirId>>,
    ) -> LirId {
        match tree {
            Environment::Nil => self.arena.alloc(Lir::Atom(vec![])),
            Environment::Leaf(symbol) => {
                if let Some(hir) = map.and_then(|map| map.get(symbol)) {
                    self.lower_hir(env, *hir)
                } else if by_reference {
                    self.lower_symbol_reference(env, *symbol)
                } else {
                    self.lower_symbol_value(env, *symbol)
                }
            }
            Environment::Pair(first, rest) => {
                let first = self.lower_tree(env, first, by_reference, map);
                let rest = self.lower_tree(env, rest, by_reference, map);
                self.arena.alloc(Lir::Cons(first, rest))
            }
        }
    }

    fn lower_assert(
        &mut self,
        env: &Environment,
        mut stmts: Vec<Statement>,
        condition: HirId,
        srcloc: SrcLoc,
        body: Option<HirId>,
    ) -> LirId {
        stmts.remove(0);

        let raise = if self.options.debug_symbols {
            let error = self.arena.alloc(Lir::Atom(
                format!("assertion failed at {}", srcloc.start()).into_bytes(),
            ));
            vec![error]
        } else {
            vec![]
        };

        let condition = self.lower_hir(env, condition);
        let then_branch = self.lower_block(env, stmts, body);
        let else_branch = self.arena.alloc(Lir::Raise(raise));
        self.arena
            .alloc(Lir::If(condition, then_branch, else_branch, false))
    }

    fn lower_raise(&mut self, env: &Environment, hir: Option<HirId>, srcloc: SrcLoc) -> LirId {
        if !self.options.debug_symbols {
            return self.arena.alloc(Lir::Raise(vec![]));
        }

        let error = self.arena.alloc(Lir::Atom(
            format!("raise called at {}", srcloc.start()).into_bytes(),
        ));
        let lir = hir.map(|hir| self.lower_hir(env, hir));

        let mut args = vec![error];

        if let Some(hir) = lir {
            args.push(hir);
        }

        self.arena.alloc(Lir::Raise(args))
    }

    fn lower_if(
        &mut self,
        env: &Environment,
        mut stmts: Vec<Statement>,
        stmt: IfStatement,
        body: Option<HirId>,
    ) -> LirId {
        stmts.remove(0);
        let condition = self.lower_hir(env, stmt.condition);
        let then_branch = self.lower_hir(env, stmt.then);
        let else_branch = self.lower_block(env, stmts, body);
        self.arena
            .alloc(Lir::If(condition, then_branch, else_branch, stmt.inline))
    }

    fn lower_expr_stmts(
        &mut self,
        env: &Environment,
        mut stmts: Vec<Statement>,
        body: Option<HirId>,
    ) -> LirId {
        let mut ids = Vec::new();

        while let Some(stmt) = stmts.first().cloned()
            && let Statement::Expr(expr) = stmt
        {
            ids.push((self.lower_hir(env, expr.hir), expr.always_nil));
            stmts.remove(0);
        }

        let expr = self.lower_block(env, stmts, body);

        if ids.is_empty() {
            return expr;
        }

        let nil_count = count_nil_in_env(self.arena, expr);

        if nil_count == 0 {
            let (condition, verifications) = if ids.len() == 1 {
                let condition = self.arena.alloc(Lir::Atom(vec![]));

                (condition, ids[0].0)
            } else if let Some(index) = ids.iter().position(|(_, always_nil)| *always_nil) {
                let (condition, _) = ids.remove(index);

                let id = if ids.len() == 1 {
                    ids[0].0
                } else {
                    self.arena
                        .alloc(Lir::All(ids.iter().map(|(id, _)| *id).collect()))
                };

                (condition, id)
            } else {
                let condition = self.arena.alloc(Lir::Atom(vec![]));

                let id = if ids.len() == 1 {
                    ids[0].0
                } else {
                    self.arena
                        .alloc(Lir::All(ids.iter().map(|(id, _)| *id).collect()))
                };

                (condition, id)
            };

            self.arena
                .alloc(Lir::If(condition, verifications, expr, true))
        } else {
            let always_nil = ids.iter().all(|(_, always_nil)| *always_nil);
            let ids: Vec<LirId> = ids.iter().map(|(id, _)| *id).collect();

            let length = ids.len();

            replace_nil_in_env(
                self.arena,
                expr,
                &mut (
                    ids,
                    if nil_count < length {
                        if always_nil {
                            VerificationKind::Grouped
                        } else {
                            VerificationKind::GroupedWithNil
                        }
                    } else {
                        VerificationKind::Individual
                    },
                ),
            )
        }
    }

    fn group_symbols(
        &mut self,
        mut symbols: IndexSet<SymbolId>,
        by_reference: bool,
    ) -> Vec<SymbolGroup> {
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
                    || by_reference
                {
                    if !self.should_inline(symbol) && self.graph.references(symbol) > 0 {
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

            groups.push(self.create_group(group, true));
        }

        groups
    }

    fn create_group(&self, symbols: Vec<SymbolId>, is_tree: bool) -> SymbolGroup {
        if is_tree {
            let mut referenced_symbols = IndexMap::new();

            for symbol in symbols {
                referenced_symbols.insert(symbol, self.graph.references(symbol));
            }

            SymbolGroup::Tree(Environment::tree(referenced_symbols))
        } else {
            SymbolGroup::Sequential(symbols)
        }
    }

    fn apply_group(mut env: Environment, group: &SymbolGroup, include_rest: bool) -> Environment {
        match group {
            SymbolGroup::Sequential(symbols) => {
                for (i, &symbol) in symbols.iter().rev().enumerate() {
                    if i == 0 && !include_rest {
                        env = Environment::Leaf(symbol);
                    } else {
                        env = Environment::Pair(Box::new(Environment::Leaf(symbol)), Box::new(env));
                    }
                }

                env
            }
            SymbolGroup::Tree(tree) => {
                if include_rest {
                    Environment::Pair(Box::new(tree.clone()), Box::new(env))
                } else {
                    tree.clone()
                }
            }
        }
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
            Symbol::Unresolved | Symbol::Module(_) | Symbol::Parameter(_) | Symbol::Builtin(_) => {
                false
            }
            Symbol::Function(function) => {
                if self.graph.dependencies(symbol, false).contains(&symbol) {
                    return false;
                }

                if function.kind == FunctionKind::Inline {
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

fn count_nil_in_env(arena: &Arena<Lir>, id: LirId) -> usize {
    match arena[id].clone() {
        Lir::Atom(atom) => usize::from(atom.is_empty()),
        Lir::Path(_) | Lir::Quote(_) => 0,
        Lir::Run(_, env) => count_nil_in_env(arena, env),
        Lir::Closure(_, env) => env.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::First(arg) => count_nil_in_env(arena, arg),
        Lir::Rest(arg) => count_nil_in_env(arena, arg),
        Lir::Cons(first, rest) => count_nil_in_env(arena, first) + count_nil_in_env(arena, rest),
        Lir::Listp(arg, _) => count_nil_in_env(arena, arg),
        Lir::Add(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::Sub(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::Mul(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::Div(left, right) => count_nil_in_env(arena, left) + count_nil_in_env(arena, right),
        Lir::Divmod(left, right) => count_nil_in_env(arena, left) + count_nil_in_env(arena, right),
        Lir::Mod(left, right) => count_nil_in_env(arena, left) + count_nil_in_env(arena, right),
        Lir::Modpow(base, exponent, modulus) => {
            count_nil_in_env(arena, base)
                + count_nil_in_env(arena, exponent)
                + count_nil_in_env(arena, modulus)
        }
        Lir::Eq(left, right) => count_nil_in_env(arena, left) + count_nil_in_env(arena, right),
        Lir::Gt(left, right) => count_nil_in_env(arena, left) + count_nil_in_env(arena, right),
        Lir::GtBytes(left, right) => count_nil_in_env(arena, left) + count_nil_in_env(arena, right),
        Lir::Not(arg) => count_nil_in_env(arena, arg),
        Lir::All(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::Any(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::If(condition, then, otherwise, inline) => {
            let mut result = count_nil_in_env(arena, condition);
            if inline {
                result += count_nil_in_env(arena, then);
                result += count_nil_in_env(arena, otherwise);
            }
            result
        }
        Lir::Raise(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::Concat(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::Strlen(arg) => count_nil_in_env(arena, arg),
        Lir::Substr(value, from, to) => {
            count_nil_in_env(arena, value)
                + count_nil_in_env(arena, from)
                + to.map_or(0, |to| count_nil_in_env(arena, to))
        }
        Lir::Logand(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::Logior(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::Logxor(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::Lognot(arg) => count_nil_in_env(arena, arg),
        Lir::Ash(value, shift) => count_nil_in_env(arena, value) + count_nil_in_env(arena, shift),
        Lir::Lsh(value, shift) => count_nil_in_env(arena, value) + count_nil_in_env(arena, shift),
        Lir::PubkeyForExp(arg) => count_nil_in_env(arena, arg),
        Lir::G1Add(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::G1Subtract(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::G1Multiply(left, right) => {
            count_nil_in_env(arena, left) + count_nil_in_env(arena, right)
        }
        Lir::G1Negate(arg) => count_nil_in_env(arena, arg),
        Lir::G1Map(data, dst) => {
            count_nil_in_env(arena, data) + dst.map_or(0, |dst| count_nil_in_env(arena, dst))
        }
        Lir::G2Add(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::G2Subtract(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::G2Multiply(left, right) => {
            count_nil_in_env(arena, left) + count_nil_in_env(arena, right)
        }
        Lir::G2Negate(arg) => count_nil_in_env(arena, arg),
        Lir::G2Map(data, dst) => {
            count_nil_in_env(arena, data) + dst.map_or(0, |dst| count_nil_in_env(arena, dst))
        }
        Lir::BlsPairingIdentity(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::BlsVerify(value, args) => {
            count_nil_in_env(arena, value)
                + args
                    .iter()
                    .map(|arg| count_nil_in_env(arena, *arg))
                    .sum::<usize>()
        }
        Lir::Sha256(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::Sha256Inline(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::Keccak256(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::Keccak256Inline(args) => args.iter().map(|arg| count_nil_in_env(arena, *arg)).sum(),
        Lir::CoinId(parent, puzzle, amount) => {
            count_nil_in_env(arena, parent)
                + count_nil_in_env(arena, puzzle)
                + count_nil_in_env(arena, amount)
        }
        Lir::K1Verify(public_key, message, signature) => {
            count_nil_in_env(arena, public_key)
                + count_nil_in_env(arena, message)
                + count_nil_in_env(arena, signature)
        }
        Lir::R1Verify(public_key, message, signature) => {
            count_nil_in_env(arena, public_key)
                + count_nil_in_env(arena, message)
                + count_nil_in_env(arena, signature)
        }
        Lir::Op(_, arg) => count_nil_in_env(arena, arg),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum VerificationKind {
    Individual,
    Grouped,
    GroupedWithNil,
}

fn replace_nil_in_env(
    arena: &mut Arena<Lir>,
    id: LirId,
    verifications: &mut (Vec<LirId>, VerificationKind),
) -> LirId {
    if verifications.0.is_empty() {
        return id;
    }

    match arena[id].clone() {
        Lir::Atom(atom) => {
            if atom.is_empty() {
                match verifications.1 {
                    VerificationKind::Individual => verifications.0.remove(0),
                    VerificationKind::Grouped => {
                        arena.alloc(Lir::All(mem::take(&mut verifications.0)))
                    }
                    VerificationKind::GroupedWithNil => {
                        let mut verifications = mem::take(&mut verifications.0);
                        verifications.push(arena.alloc(Lir::Atom(vec![])));
                        arena.alloc(Lir::All(verifications))
                    }
                }
            } else {
                id
            }
        }
        Lir::Path(_) | Lir::Quote(_) => id,
        Lir::Run(callee, env) => {
            let env = replace_nil_in_env(arena, env, verifications);
            arena.alloc(Lir::Run(callee, env))
        }
        Lir::Closure(callee, env) => {
            let mut result = Vec::new();
            for arg in env {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::Closure(callee, result))
        }
        Lir::First(arg) => {
            let arg = replace_nil_in_env(arena, arg, verifications);
            arena.alloc(Lir::First(arg))
        }
        Lir::Rest(arg) => {
            let arg = replace_nil_in_env(arena, arg, verifications);
            arena.alloc(Lir::Rest(arg))
        }
        Lir::Cons(first, rest) => {
            let first = replace_nil_in_env(arena, first, verifications);
            let rest = replace_nil_in_env(arena, rest, verifications);
            arena.alloc(Lir::Cons(first, rest))
        }
        Lir::Listp(arg, can_be_truthy) => {
            let arg = replace_nil_in_env(arena, arg, verifications);
            arena.alloc(Lir::Listp(arg, can_be_truthy))
        }
        Lir::Add(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::Add(result))
        }
        Lir::Sub(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::Sub(result))
        }
        Lir::Mul(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::Mul(result))
        }
        Lir::Div(left, right) => {
            let left = replace_nil_in_env(arena, left, verifications);
            let right = replace_nil_in_env(arena, right, verifications);
            arena.alloc(Lir::Div(left, right))
        }
        Lir::Divmod(left, right) => {
            let left = replace_nil_in_env(arena, left, verifications);
            let right = replace_nil_in_env(arena, right, verifications);
            arena.alloc(Lir::Divmod(left, right))
        }
        Lir::Mod(left, right) => {
            let left = replace_nil_in_env(arena, left, verifications);
            let right = replace_nil_in_env(arena, right, verifications);
            arena.alloc(Lir::Mod(left, right))
        }
        Lir::Modpow(base, exponent, modulus) => {
            let base = replace_nil_in_env(arena, base, verifications);
            let exponent = replace_nil_in_env(arena, exponent, verifications);
            let modulus = replace_nil_in_env(arena, modulus, verifications);
            arena.alloc(Lir::Modpow(base, exponent, modulus))
        }
        Lir::Eq(left, right) => {
            let left = replace_nil_in_env(arena, left, verifications);
            let right = replace_nil_in_env(arena, right, verifications);
            arena.alloc(Lir::Eq(left, right))
        }
        Lir::Gt(left, right) => {
            let left = replace_nil_in_env(arena, left, verifications);
            let right = replace_nil_in_env(arena, right, verifications);
            arena.alloc(Lir::Gt(left, right))
        }
        Lir::GtBytes(left, right) => {
            let left = replace_nil_in_env(arena, left, verifications);
            let right = replace_nil_in_env(arena, right, verifications);
            arena.alloc(Lir::GtBytes(left, right))
        }
        Lir::Not(arg) => {
            let arg = replace_nil_in_env(arena, arg, verifications);
            arena.alloc(Lir::Not(arg))
        }
        Lir::All(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::All(result))
        }
        Lir::Any(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::Any(result))
        }
        Lir::If(condition, then, otherwise, inline) => {
            let condition = replace_nil_in_env(arena, condition, verifications);
            let then = if inline {
                replace_nil_in_env(arena, then, verifications)
            } else {
                then
            };
            let otherwise = if inline {
                replace_nil_in_env(arena, otherwise, verifications)
            } else {
                otherwise
            };
            arena.alloc(Lir::If(condition, then, otherwise, inline))
        }
        Lir::Raise(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::Raise(result))
        }
        Lir::Concat(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::Concat(result))
        }
        Lir::Strlen(arg) => {
            let arg = replace_nil_in_env(arena, arg, verifications);
            arena.alloc(Lir::Strlen(arg))
        }
        Lir::Substr(value, from, to) => {
            let value = replace_nil_in_env(arena, value, verifications);
            let from = replace_nil_in_env(arena, from, verifications);
            let to = to.map(|to| replace_nil_in_env(arena, to, verifications));
            arena.alloc(Lir::Substr(value, from, to))
        }
        Lir::Logand(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::Logand(result))
        }
        Lir::Logior(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::Logior(result))
        }
        Lir::Logxor(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::Logxor(result))
        }
        Lir::Lognot(arg) => {
            let arg = replace_nil_in_env(arena, arg, verifications);
            arena.alloc(Lir::Lognot(arg))
        }
        Lir::Ash(value, shift) => {
            let value = replace_nil_in_env(arena, value, verifications);
            let shift = replace_nil_in_env(arena, shift, verifications);
            arena.alloc(Lir::Ash(value, shift))
        }
        Lir::Lsh(value, shift) => {
            let value = replace_nil_in_env(arena, value, verifications);
            let shift = replace_nil_in_env(arena, shift, verifications);
            arena.alloc(Lir::Lsh(value, shift))
        }
        Lir::PubkeyForExp(arg) => {
            let arg = replace_nil_in_env(arena, arg, verifications);
            arena.alloc(Lir::PubkeyForExp(arg))
        }
        Lir::G1Add(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::G1Add(result))
        }
        Lir::G1Subtract(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::G1Subtract(result))
        }
        Lir::G1Multiply(left, right) => {
            let left = replace_nil_in_env(arena, left, verifications);
            let right = replace_nil_in_env(arena, right, verifications);
            arena.alloc(Lir::G1Multiply(left, right))
        }
        Lir::G1Negate(arg) => {
            let arg = replace_nil_in_env(arena, arg, verifications);
            arena.alloc(Lir::G1Negate(arg))
        }
        Lir::G1Map(data, dst) => {
            let data = replace_nil_in_env(arena, data, verifications);
            let dst = dst.map(|dst| replace_nil_in_env(arena, dst, verifications));
            arena.alloc(Lir::G1Map(data, dst))
        }
        Lir::G2Add(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::G2Add(result))
        }
        Lir::G2Subtract(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::G2Subtract(result))
        }
        Lir::G2Multiply(left, right) => {
            let left = replace_nil_in_env(arena, left, verifications);
            let right = replace_nil_in_env(arena, right, verifications);
            arena.alloc(Lir::G2Multiply(left, right))
        }
        Lir::G2Negate(arg) => {
            let arg = replace_nil_in_env(arena, arg, verifications);
            arena.alloc(Lir::G2Negate(arg))
        }
        Lir::G2Map(data, dst) => {
            let data = replace_nil_in_env(arena, data, verifications);
            let dst = dst.map(|dst| replace_nil_in_env(arena, dst, verifications));
            arena.alloc(Lir::G2Map(data, dst))
        }
        Lir::BlsPairingIdentity(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::BlsPairingIdentity(result))
        }
        Lir::BlsVerify(value, args) => {
            let value = replace_nil_in_env(arena, value, verifications);
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::BlsVerify(value, result))
        }
        Lir::Sha256(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::Sha256(result))
        }
        Lir::Sha256Inline(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::Sha256Inline(result))
        }
        Lir::Keccak256(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::Keccak256(result))
        }
        Lir::Keccak256Inline(args) => {
            let mut result = Vec::new();
            for arg in args {
                result.push(replace_nil_in_env(arena, arg, verifications));
            }
            arena.alloc(Lir::Keccak256Inline(result))
        }
        Lir::CoinId(parent, puzzle, amount) => {
            let parent = replace_nil_in_env(arena, parent, verifications);
            let puzzle = replace_nil_in_env(arena, puzzle, verifications);
            let amount = replace_nil_in_env(arena, amount, verifications);
            arena.alloc(Lir::CoinId(parent, puzzle, amount))
        }
        Lir::K1Verify(public_key, message, signature) => {
            let public_key = replace_nil_in_env(arena, public_key, verifications);
            let message = replace_nil_in_env(arena, message, verifications);
            let signature = replace_nil_in_env(arena, signature, verifications);
            arena.alloc(Lir::K1Verify(public_key, message, signature))
        }
        Lir::R1Verify(public_key, message, signature) => {
            let public_key = replace_nil_in_env(arena, public_key, verifications);
            let message = replace_nil_in_env(arena, message, verifications);
            let signature = replace_nil_in_env(arena, signature, verifications);
            arena.alloc(Lir::R1Verify(public_key, message, signature))
        }
        Lir::Op(op, arg) => {
            let arg = replace_nil_in_env(arena, arg, verifications);
            arena.alloc(Lir::Op(op, arg))
        }
    }
}
