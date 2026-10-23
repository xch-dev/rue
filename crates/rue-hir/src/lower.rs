#![allow(clippy::needless_pass_by_value)]

use std::{collections::HashMap, path::PathBuf};

use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};
use rue_diagnostic::SrcLoc;
use rue_lir::{Lir, LirId, bigint_atom};
use rue_options::CompilerOptions;

use crate::{
    BinaryOp, BindingSymbol, ConstantSymbol, Database, DependencyGraph, Environment, ExprStatement,
    FunctionCall, FunctionKind, FunctionSymbol, Hir, HirId, IfStatement, PathError, Statement,
    Symbol, SymbolId, UnaryOp,
};

#[derive(Debug, Clone)]
enum SymbolGroup {
    Sequential(Vec<SymbolId>),
    Tree(Environment),
}

impl SymbolGroup {
    pub fn is_empty(&self) -> bool {
        match self {
            SymbolGroup::Sequential(symbols) => symbols.is_empty(),
            SymbolGroup::Tree(Environment::Nil) => true,
            SymbolGroup::Tree(_) => false,
        }
    }
}

#[derive(Debug)]
pub struct Lowerer<'d, 'a, 'g> {
    db: &'d mut Database,
    arena: &'a mut Arena<Lir>,
    graph: &'g DependencyGraph,
    inline_symbols: Vec<HashMap<SymbolId, HirId>>,
    options: CompilerOptions,
    main: SymbolId,
    base_path: PathBuf,
}

impl<'d, 'a, 'g> Lowerer<'d, 'a, 'g> {
    pub fn new(
        db: &'d mut Database,
        arena: &'a mut Arena<Lir>,
        graph: &'g DependencyGraph,
        options: CompilerOptions,
        main: SymbolId,
        base_path: PathBuf,
    ) -> Self {
        Self {
            db,
            arena,
            graph,
            inline_symbols: Vec::new(),
            options,
            main,
            base_path,
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
            .filter(|&symbol| self.is_capture(symbol))
            .collect();

        let capture_groups = self.group_symbols(
            captures.into_iter().collect(),
            symbol != self.main,
            !self.graph.is_closure(symbol),
        );

        let param_group = self.create_group(
            function.parameters.values().copied().collect(),
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

        // Inline arguments are lowered in the caller's environment, so the function body
        // can only reach them through its captures, unless they are static values
        let static_arguments = self
            .inline_symbols
            .iter()
            .flat_map(HashMap::keys)
            .filter_map(|&symbol| {
                let hir = self.inline_argument(symbol)?;
                self.is_static_hir(hir).then_some((symbol, hir))
            })
            .collect();
        let inline_symbols = std::mem::replace(&mut self.inline_symbols, vec![static_arguments]);
        let mut expr = self.lower_hir(&function_env, function.body);
        self.inline_symbols = inline_symbols;

        if symbol == self.main && function.kind == FunctionKind::External {
            let entire_env = self.arena.alloc(Lir::Path(1u8.into()));
            return self.arena.alloc(Lir::Run(expr, entire_env));
        }

        if symbol == self.main {
            let mut map = HashMap::new();

            if self
                .graph
                .dependencies(symbol, true)
                .iter()
                .any(|dependency| *dependency == symbol)
            {
                map.insert(symbol, self.arena.alloc(Lir::Quote(expr)));
                let reference = self.lower_symbol_reference(&function_env, symbol);
                let entire_env = self.arena.alloc(Lir::Path(1u8.into()));
                expr = self.arena.alloc(Lir::Run(reference, entire_env));
            }

            for (i, group) in capture_groups.iter().enumerate().rev() {
                expr = self.arena.alloc(Lir::Quote(expr));

                let mut bind_env = parent_env.clone();

                for existing_group in capture_groups.iter().take(i) {
                    bind_env = Self::apply_group(bind_env, existing_group, true);
                }

                let rest = if param_group.is_empty() {
                    self.arena.alloc(Lir::Atom(vec![]))
                } else {
                    self.arena.alloc(Lir::Path(1u8.into()))
                };

                let group_env =
                    self.lower_group_environment(&bind_env, group, rest, false, Some(&map), true);

                expr = self.arena.alloc(Lir::Run(expr, group_env));
            }

            expr
        } else if function.kind == FunctionKind::External {
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
            Hir::Const(_) => unreachable!("unevaluated const expression reached lowering"),
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
                    BinaryOp::RightLogicalShift => {
                        let zero = self.arena.alloc(Lir::Atom(vec![]));
                        let neg = self.arena.alloc(Lir::Sub(vec![zero, right]));
                        self.arena.alloc(Lir::Lsh(left, neg))
                    }
                    BinaryOp::RightArithmeticShift => {
                        let zero = self.arena.alloc(Lir::Atom(vec![]));
                        let neg = self.arena.alloc(Lir::Sub(vec![zero, right]));
                        self.arena.alloc(Lir::Ash(left, neg))
                    }
                    BinaryOp::LeftArithmeticShift => self.arena.alloc(Lir::Ash(left, right)),
                    BinaryOp::Gt => self.arena.alloc(Lir::Gt(left, right)),
                    BinaryOp::Lt => self.arena.alloc(Lir::Gt(right, left)),
                    BinaryOp::Gte => {
                        let lt = self.arena.alloc(Lir::Gt(right, left));
                        self.arena.alloc(Lir::Not(lt))
                    }
                    BinaryOp::Lte => {
                        let gt = self.arena.alloc(Lir::Gt(left, right));
                        self.arena.alloc(Lir::Not(gt))
                    }
                    BinaryOp::GtBytes => self.arena.alloc(Lir::GtBytes(left, right)),
                    BinaryOp::LtBytes => self.arena.alloc(Lir::GtBytes(right, left)),
                    BinaryOp::GteBytes => {
                        let lt = self.arena.alloc(Lir::GtBytes(right, left));
                        self.arena.alloc(Lir::Not(lt))
                    }
                    BinaryOp::LteBytes => {
                        let gt = self.arena.alloc(Lir::GtBytes(left, right));
                        self.arena.alloc(Lir::Not(gt))
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
                    args.insert(*last_param.1, last_arg);
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

            let mut lir_args = HashMap::new();

            for (symbol, hir) in &args {
                lir_args.insert(*symbol, self.lower_hir(env, *hir));
            }

            let mut arg_env = self.lower_group_environment(
                env,
                &param_group,
                rest,
                false,
                Some(&lir_args),
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

        if let Symbol::Function(function) = self.db.symbol(symbol).clone()
            && function.kind != FunctionKind::External
        {
            let captures: Vec<SymbolId> = self
                .graph
                .dependencies(symbol, true)
                .into_iter()
                .filter(|&symbol| self.is_capture(symbol))
                .collect();

            let mut refs = Vec::new();

            for capture in captures {
                refs.push(self.lower_symbol_reference(env, capture));
            }

            reference = self.arena.alloc(Lir::Closure(
                reference,
                refs,
                !function.parameters.is_empty(),
            ));
        }

        reference
    }

    fn lower_path(&mut self, env: &Environment, symbol: SymbolId) -> LirId {
        self.arena.alloc(Lir::Path(env.path(symbol).unwrap_or_else(
            |error| match error {
                PathError::SymbolNotFound => panic!(
                    "clvm path in environment not found for symbol {}",
                    self.db.debug_symbol(symbol)
                ),
            },
        )))
    }

    fn lower_block(
        &mut self,
        env: &Environment,
        mut stmts: Vec<Statement>,
        body: Option<HirId>,
    ) -> LirId {
        if !self.options.debug_symbols {
            stmts.retain(|stmt| !matches!(stmt, Statement::Debug(..)));
        }

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
            Statement::Expr(stmt) => self.lower_expr_stmts(env, stmt, stmts, body),
            Statement::Raise(hir, srcloc) => self.lower_raise(env, hir, srcloc),
            Statement::If(stmt) => self.lower_if(env, stmts, stmt, body),
            Statement::Debug(hir, srcloc) => self.lower_debug(env, stmts, hir, srcloc, body),
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

        let binding_groups = self.group_symbols(symbols, false, true);

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

            let rest = self.arena.alloc(Lir::Path(1u8.into()));
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
        map: Option<&HashMap<SymbolId, LirId>>,
        include_rest: bool,
    ) -> LirId {
        match group {
            SymbolGroup::Sequential(symbols) => {
                let mut result = rest;

                for (i, &symbol) in symbols.iter().rev().enumerate() {
                    let value = if let Some(lir) = map.and_then(|map| map.get(&symbol)) {
                        *lir
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
        map: Option<&HashMap<SymbolId, LirId>>,
    ) -> LirId {
        match tree {
            Environment::Nil => self.arena.alloc(Lir::Atom(vec![])),
            Environment::Leaf(symbol) => {
                if let Some(lir) = map.and_then(|map| map.get(symbol)) {
                    *lir
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
                format!("assertion failed at {}", srcloc.display(&self.base_path)).into_bytes(),
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
            format!("raise called at {}", srcloc.display(&self.base_path)).into_bytes(),
        ));
        let lir = hir.map(|hir| self.lower_hir(env, hir));

        let mut args = vec![error];

        if let Some(hir) = lir {
            args.push(hir);
        }

        self.arena.alloc(Lir::Raise(args))
    }

    fn lower_debug(
        &mut self,
        env: &Environment,
        mut stmts: Vec<Statement>,
        value: HirId,
        srcloc: SrcLoc,
        body: Option<HirId>,
    ) -> LirId {
        stmts.remove(0);

        let rest = self.lower_block(env, stmts, body);

        let value = self.lower_hir(env, value);
        let print = self
            .arena
            .alloc(Lir::DebugPrint(srcloc.display(&self.base_path), value));
        let nil = self.arena.alloc(Lir::Atom(vec![]));
        self.arena.alloc(Lir::If(print, nil, rest, false))
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
        stmt: ExprStatement,
        mut stmts: Vec<Statement>,
        body: Option<HirId>,
    ) -> LirId {
        if self.options.debug_symbols {
            stmts.remove(0);

            let rest = self.lower_block(env, stmts, body);
            let value = self.lower_hir(env, stmt.hir);
            let one = self.arena.alloc(Lir::Atom(vec![1]));
            return self.arena.alloc(Lir::If(one, rest, value, true));
        }

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
    }

    fn group_symbols(
        &mut self,
        mut symbols: IndexSet<SymbolId>,
        by_reference: bool,
        is_tree: bool,
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
                    if self.is_capture(symbol) && self.graph.references(symbol) > 0 {
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

            groups.push(self.create_group(group, is_tree));
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

    fn inline_argument(&self, symbol: SymbolId) -> Option<HirId> {
        self.inline_symbols
            .iter()
            .rev()
            .find_map(|symbols| symbols.get(&symbol).copied())
    }

    /// Whether the value can be lowered in any environment, since it doesn't depend on it.
    fn is_static_hir(&self, hir: HirId) -> bool {
        match self.db.hir(hir) {
            Hir::Nil | Hir::String(_) | Hir::Int(_) | Hir::Bool(_) | Hir::Bytes(_) => true,
            Hir::Pair(first, rest) => self.is_static_hir(*first) && self.is_static_hir(*rest),
            Hir::Reference(symbol) => {
                if let Some(hir) = self.inline_argument(*symbol) {
                    return self.is_static_hir(hir);
                }

                if !self.should_inline(*symbol) {
                    return false;
                }

                match self.db.symbol(*symbol) {
                    Symbol::Constant(constant) => self.is_static_hir(constant.value.hir),
                    Symbol::Binding(binding) => self.is_static_hir(binding.value.hir),
                    _ => false,
                }
            }
            _ => false,
        }
    }

    /// Whether a function needs to capture the symbol rather than inlining its value.
    /// Inline arguments are captured by value, unless they are static.
    fn is_capture(&self, symbol: SymbolId) -> bool {
        match self.inline_argument(symbol) {
            Some(hir) => !self.is_static_hir(hir),
            None => !self.should_inline(symbol),
        }
    }

    fn should_inline(&self, symbol: SymbolId) -> bool {
        if self.inline_argument(symbol).is_some() {
            return true;
        }

        let references = self.graph.references(symbol);

        match self.db.symbol(symbol) {
            Symbol::Unresolved | Symbol::Module(_) | Symbol::Parameter(_) | Symbol::Builtin(_) => {
                false
            }
            Symbol::Function(function) => {
                if function.kind == FunctionKind::External {
                    return false;
                }

                if self.graph.dependencies(symbol, false).contains(&symbol) {
                    return false;
                }

                if function.kind == FunctionKind::Inline {
                    return true;
                }

                for (_, parameter) in &function.parameters {
                    if self.graph.references(*parameter) > 1 {
                        return false;
                    }
                }

                references <= 1 && self.options.auto_inline
            }
            Symbol::Constant(constant) => {
                if constant.inline {
                    return true;
                }

                references <= 1 && self.options.auto_inline
            }
            Symbol::Binding(binding) => {
                if binding.inline {
                    return true;
                }

                references <= 1 && self.options.auto_inline
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use rue_types::BuiltinTypes;

    use crate::{ParameterSymbol, Scope};

    use super::*;

    fn function(
        db: &mut Database,
        builtins: &BuiltinTypes,
        parameters: IndexMap<String, SymbolId>,
        body: HirId,
        kind: FunctionKind,
    ) -> FunctionSymbol {
        FunctionSymbol {
            name: None,
            ty: builtins.unresolved,
            scope: db.alloc_scope(Scope::new(None)),
            vars: vec![],
            parameters,
            nil_terminated: true,
            return_type: builtins.unresolved,
            body,
            kind,
        }
    }

    fn parameter(db: &mut Database, builtins: &BuiltinTypes) -> SymbolId {
        db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
            name: None,
            ty: builtins.int,
        }))
    }

    // fn main(x: Int) -> Int { f(<argument>)() }
    // inline fn f(y: Int) -> fn() -> Int { fn() => y }
    // Returns the captures of the closure created by the lambda in `f`
    fn lambda_captures(argument: impl FnOnce(&mut Database, SymbolId) -> HirId) -> Vec<Lir> {
        let mut db = Database::new();
        let builtins = BuiltinTypes::new(db.types_mut());

        let y = parameter(&mut db, &builtins);
        let lambda_body = db.alloc_hir(Hir::Reference(y));
        let lambda = function(
            &mut db,
            &builtins,
            IndexMap::new(),
            lambda_body,
            FunctionKind::Sequential,
        );
        let lambda = db.alloc_symbol(Symbol::Function(lambda));

        let f_body = db.alloc_hir(Hir::Lambda(lambda));
        let f = function(
            &mut db,
            &builtins,
            IndexMap::from([("y".to_string(), y)]),
            f_body,
            FunctionKind::Inline,
        );
        let f = db.alloc_symbol(Symbol::Function(f));

        let x = parameter(&mut db, &builtins);
        let f_reference = db.alloc_hir(Hir::Reference(f));
        let argument = argument(&mut db, x);
        let f_call = db.alloc_hir(Hir::FunctionCall(FunctionCall {
            function: f_reference,
            args: vec![argument],
            nil_terminated: true,
        }));
        let main_body = db.alloc_hir(Hir::FunctionCall(FunctionCall {
            function: f_call,
            args: vec![],
            nil_terminated: true,
        }));
        let main = function(
            &mut db,
            &builtins,
            IndexMap::from([("x".to_string(), x)]),
            main_body,
            FunctionKind::Sequential,
        );
        let main = db.alloc_symbol(Symbol::Function(main));

        let options = CompilerOptions::default();
        let graph = DependencyGraph::build(&db, main, options);
        let mut arena = Arena::new();
        let mut lowerer = Lowerer::new(&mut db, &mut arena, &graph, options, main, PathBuf::new());
        lowerer.lower_symbol_value(&Environment::default(), main);

        let mut closures = arena
            .iter()
            .filter_map(|(_, lir)| match lir {
                Lir::Closure(_, captures, _) => Some(captures.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(closures.len(), 1);

        closures
            .remove(0)
            .into_iter()
            .map(|capture| arena[capture].clone())
            .collect()
    }

    #[test]
    fn closure_captures_inline_argument() {
        let captures = lambda_captures(|db, x| db.alloc_hir(Hir::Reference(x)));
        assert!(matches!(captures.as_slice(), [Lir::Path(path)] if *path == 2u8.into()));
    }

    #[test]
    fn closure_inlines_static_argument() {
        let captures = lambda_captures(|db, _| db.alloc_hir(Hir::Int(5.into())));
        assert!(captures.is_empty());
    }
}
