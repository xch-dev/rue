use std::collections::HashMap;

use crate::{
    database::{Database, HirId, LirId, SymbolId},
    hir::{BinOp, Hir, Op},
    lir::Lir,
    symbol::{Function, Symbol},
    value::{FunctionType, Rest, Value},
    EnvironmentId, ScopeId,
};

mod dependency_graph;
mod environment;

pub use dependency_graph::*;
pub use environment::*;
use indexmap::IndexSet;

pub struct Optimizer<'a> {
    db: &'a mut Database,
    graph: DependencyGraph,
    inline_parameter_stack: Vec<HashMap<SymbolId, LirId>>,
}

impl<'a> Optimizer<'a> {
    pub fn new(db: &'a mut Database, graph: DependencyGraph) -> Self {
        Self {
            db,
            graph,
            inline_parameter_stack: Vec::new(),
        }
    }

    pub fn opt_main(&mut self, main: SymbolId) -> LirId {
        let Symbol::Function(fun) = self.db.symbol(main).clone() else {
            unreachable!();
        };
        let env_id = self.graph.env(fun.scope_id);
        let mut definitions = self.db.env(env_id).definitions();
        definitions.extend(self.db.env(env_id).captures());
        self.opt_definitions(env_id, definitions, fun.hir_id)
    }

    fn opt_path(&mut self, env_id: EnvironmentId, symbol_id: SymbolId) -> LirId {
        for inline_parameter_map in self.inline_parameter_stack.iter().rev() {
            if let Some(lir_id) = inline_parameter_map.get(&symbol_id) {
                return *lir_id;
            }
        }

        let mut current_env_id = env_id;
        let mut environment: Vec<SymbolId> = self.db.env(env_id).build().into_iter().collect();

        while let Some(parent_env_id) = self.db.env(current_env_id).parent() {
            assert!(self.db.env(current_env_id).parameters().is_empty());
            assert!(!self.db.env(current_env_id).rest_parameter());

            current_env_id = parent_env_id;
            environment.extend(self.db.env(current_env_id).build());
        }

        let index = environment
            .iter()
            .position(|&id| id == symbol_id)
            .unwrap_or_else(|| {
                panic!(
                    "Symbol `{}` not found in environment.",
                    self.db.dbg_symbol(symbol_id)
                );
            });

        let mut path = 1;

        if !(index + 1 == environment.len() && self.db.env(env_id).rest_parameter()) {
            path *= 2;
        }

        for _ in 0..index {
            path *= 2;
            path += 1;
        }

        self.db.alloc_lir(Lir::Path(path))
    }

    fn opt_definition(&mut self, env_id: EnvironmentId, symbol_id: SymbolId) -> LirId {
        match self.db.symbol(symbol_id).clone() {
            Symbol::Function(Function {
                hir_id, scope_id, ..
            }) => {
                let function_env_id = self.graph.env(scope_id);
                let function = self.opt_definitions(
                    function_env_id,
                    self.db.env(function_env_id).definitions(),
                    hir_id,
                );
                self.db.alloc_lir(Lir::Quote(function))
            }
            Symbol::Const(Value { hir_id, .. }) => self.opt_hir(env_id, hir_id),
            Symbol::Let(symbol) if self.graph.symbol_usages(symbol_id) > 0 => {
                self.opt_hir(env_id, symbol.hir_id)
            }
            Symbol::Unknown
            | Symbol::Module(..)
            | Symbol::Parameter(..)
            | Symbol::Let(..)
            | Symbol::InlineFunction(..)
            | Symbol::InlineConst(..) => unreachable!(),
        }
    }

    fn opt_definitions(
        &mut self,
        mut env_id: EnvironmentId,
        definitions: IndexSet<SymbolId>,
        body: HirId,
    ) -> LirId {
        let mut remaining: IndexSet<SymbolId> = definitions.into_iter().collect();
        let mut curries = Vec::new();

        while !remaining.is_empty() {
            let no_references = remaining
                .iter()
                .filter(|&symbol_id| {
                    !self.db.symbol(*symbol_id).is_constant()
                        || self
                            .graph
                            .constant_references(*symbol_id)
                            .intersection(&remaining)
                            .count()
                            == 0
                })
                .copied()
                .collect::<Vec<_>>();

            let mut args = Vec::new();

            for &symbol_id in &no_references {
                args.push(self.opt_definition(env_id, symbol_id));
            }

            curries.push(args);
            remaining.retain(|&symbol_id| !no_references.contains(&symbol_id));
            if !remaining.is_empty() {
                env_id = self.db.alloc_env(Environment::binding(env_id));
            }
        }

        let mut body = self.opt_hir(env_id, body);

        for args in curries.into_iter().rev() {
            body = self.db.alloc_lir(Lir::Curry(body, args));
        }

        body
    }

    fn opt_hir(&mut self, env_id: EnvironmentId, hir_id: HirId) -> LirId {
        match self.db.hir(hir_id).clone() {
            Hir::Unknown => self.db.alloc_lir(Lir::Atom(Vec::new())),
            Hir::Atom(atom) => self.db.alloc_lir(Lir::Atom(atom.clone())),
            Hir::Pair(first, rest) => self.opt_pair(env_id, first, rest),
            Hir::Reference(symbol_id, ..) => self.opt_reference(env_id, symbol_id),
            Hir::Op(Op::First, value) => self.opt_first(env_id, value),
            Hir::Op(Op::Rest, value) => self.opt_rest(env_id, value),
            Hir::Op(Op::Not, value) => self.opt_not(env_id, value),
            Hir::Op(Op::Sha256, value) => self.opt_sha256(env_id, value),
            Hir::Op(Op::Listp, value) => self.opt_listp(env_id, value),
            Hir::Op(Op::Strlen, value) => self.opt_strlen(env_id, value),
            Hir::Op(Op::PubkeyForExp, value) => self.opt_pubkey_for_exp(env_id, value),
            Hir::Op(Op::Exists, value) => self.opt_check_exists(env_id, value),
            Hir::Definition { scope_id, hir_id } => {
                self.opt_env_definition(env_id, scope_id, hir_id)
            }
            Hir::Raise(value) => self.opt_raise(env_id, value),
            Hir::FunctionCall {
                callee,
                args,
                varargs,
            } => {
                if let Hir::Reference(symbol_id, ..) = self.db.hir(callee) {
                    if let Symbol::InlineFunction(Function {
                        scope_id,
                        ty,
                        hir_id,
                        ..
                    }) = self.db.symbol(*symbol_id)
                    {
                        let function_env_id = self.graph.env(*scope_id);
                        return self.opt_inline_function_call(
                            env_id,
                            function_env_id,
                            &ty.clone(),
                            *hir_id,
                            args.clone(),
                            varargs,
                        );
                    }
                }
                self.opt_function_call(env_id, callee, args, varargs)
            }
            Hir::BinaryOp(op, lhs, rhs) => {
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
                    BinOp::LogicalAnd => Self::opt_logical_and,
                    BinOp::LogicalOr => Self::opt_logical_or,
                };
                handler(self, env_id, lhs, rhs)
            }
            Hir::If {
                condition,
                then_block,
                else_block,
            } => self.opt_if(env_id, condition, then_block, else_block),
        }
    }

    fn opt_check_exists(&mut self, env_id: EnvironmentId, hir_id: HirId) -> LirId {
        let value = self.opt_hir(env_id, hir_id);
        let Lir::Path(path) = self.db.lir(value).clone() else {
            panic!("invalid state, expected path for existence check");
        };
        self.db.alloc_lir(Lir::Path(Self::pack_bits(path)))
    }

    fn pack_bits(mut n: u32) -> u32 {
        let mut result = 0;
        let mut position = 0;

        while n > 0 {
            if n & 1 != 0 {
                result |= 1 << position;
                position += 1;
            }
            n >>= 1;
        }

        result
    }

    fn opt_env_definition(
        &mut self,
        env_id: EnvironmentId,
        scope_id: ScopeId,
        hir_id: HirId,
    ) -> LirId {
        let definition_env_id = self.graph.env(scope_id);
        for symbol_id in self.db.env_mut(definition_env_id).definitions() {
            let Symbol::Let(..) = self.db.symbol(symbol_id) else {
                continue;
            };
            if self.graph.symbol_usages(symbol_id) == 1 {
                self.db
                    .env_mut(definition_env_id)
                    .remove_definition(symbol_id);
            }
        }
        let child_env_id = self.graph.env(scope_id);
        let body = self.opt_hir(child_env_id, hir_id);

        let mut args = Vec::new();

        for symbol_id in self.db.env(child_env_id).definitions() {
            args.push(self.opt_definition(env_id, symbol_id));
        }

        self.db.alloc_lir(Lir::Curry(body, args))
    }

    fn opt_pair(&mut self, env_id: EnvironmentId, first: HirId, rest: HirId) -> LirId {
        let first = self.opt_hir(env_id, first);
        let rest = self.opt_hir(env_id, rest);
        self.db.alloc_lir(Lir::Pair(first, rest))
    }

    fn opt_first(&mut self, env_id: EnvironmentId, hir_id: HirId) -> LirId {
        let lir_id = self.opt_hir(env_id, hir_id);
        if let Lir::Path(path) = self.db.lir(lir_id) {
            return self.db.alloc_lir(Lir::Path(f(*path)));
        }
        self.db.alloc_lir(Lir::First(lir_id))
    }

    fn opt_rest(&mut self, env_id: EnvironmentId, hir_id: HirId) -> LirId {
        let lir_id = self.opt_hir(env_id, hir_id);
        if let Lir::Path(path) = self.db.lir(lir_id) {
            return self.db.alloc_lir(Lir::Path(r(*path)));
        }
        self.db.alloc_lir(Lir::Rest(lir_id))
    }

    fn opt_sha256(&mut self, env_id: EnvironmentId, hir_id: HirId) -> LirId {
        let lir_id = self.opt_hir(env_id, hir_id);
        if let Lir::Concat(args) = self.db.lir(lir_id).clone() {
            return self.db.alloc_lir(Lir::Sha256(args));
        }
        self.db.alloc_lir(Lir::Sha256(vec![lir_id]))
    }

    fn opt_listp(&mut self, env_id: EnvironmentId, hir_id: HirId) -> LirId {
        let lir_id = self.opt_hir(env_id, hir_id);
        self.db.alloc_lir(Lir::Listp(lir_id))
    }

    fn opt_strlen(&mut self, env_id: EnvironmentId, hir_id: HirId) -> LirId {
        let lir_id = self.opt_hir(env_id, hir_id);
        self.db.alloc_lir(Lir::Strlen(lir_id))
    }

    fn opt_pubkey_for_exp(&mut self, env_id: EnvironmentId, hir_id: HirId) -> LirId {
        let lir_id = self.opt_hir(env_id, hir_id);
        self.db.alloc_lir(Lir::PubkeyForExp(lir_id))
    }

    fn opt_reference(&mut self, env_id: EnvironmentId, symbol_id: SymbolId) -> LirId {
        match self.db.symbol(symbol_id).clone() {
            Symbol::Function(Function { scope_id, .. }) => {
                let function_env_id = self.graph.env(scope_id);
                let body = self.opt_path(env_id, symbol_id);

                let mut captures = Vec::new();

                for symbol_id in self.db.env(function_env_id).definitions() {
                    captures.push(self.opt_path(env_id, symbol_id));
                }

                for symbol_id in self.db.env(function_env_id).captures() {
                    captures.push(self.opt_path(env_id, symbol_id));
                }

                self.db.alloc_lir(Lir::Closure(body, captures))
            }
            Symbol::InlineFunction(..) => self.db.alloc_lir(Lir::Atom(vec![])),
            Symbol::InlineConst(Value { hir_id, .. }) => self.opt_hir(env_id, hir_id),
            Symbol::Let(symbol) if self.graph.symbol_usages(symbol_id) == 1 => {
                self.opt_hir(env_id, symbol.hir_id)
            }
            Symbol::Let(..) | Symbol::Const(..) | Symbol::Parameter(..) => {
                self.opt_path(env_id, symbol_id)
            }
            Symbol::Unknown | Symbol::Module(..) => unreachable!(),
        }
    }

    fn opt_function_call(
        &mut self,
        env_id: EnvironmentId,
        callee: HirId,
        args: Vec<HirId>,
        varargs: bool,
    ) -> LirId {
        let mut lir_id = self.db.alloc_lir(Lir::Atom(Vec::new()));

        for (i, arg) in args.into_iter().rev().enumerate() {
            let arg = self.opt_hir(env_id, arg);

            if i == 0 && varargs {
                lir_id = arg;
                continue;
            }

            lir_id = self.db.alloc_lir(Lir::Pair(arg, lir_id));
        }

        let callee = if let Hir::Reference(symbol_id, ..) = self.db.hir(callee).clone() {
            if let Symbol::Function(Function { scope_id, .. }) = self.db.symbol(symbol_id) {
                let callee_env_id = self.graph.env(*scope_id);
                for symbol_id in self.db.env(callee_env_id).captures().into_iter().rev() {
                    let capture = self.opt_path(env_id, symbol_id);
                    lir_id = self.db.alloc_lir(Lir::Pair(capture, lir_id));
                }
                self.opt_path(env_id, symbol_id)
            } else {
                self.opt_hir(env_id, callee)
            }
        } else {
            self.opt_hir(env_id, callee)
        };

        self.db.alloc_lir(Lir::Run(callee, Some(lir_id)))
    }

    fn opt_inline_function_call(
        &mut self,
        env_id: EnvironmentId,
        function_env_id: EnvironmentId,
        ty: &FunctionType,
        hir_id: HirId,
        args: Vec<HirId>,
        varargs: bool,
    ) -> LirId {
        let mut inline_parameter_map = HashMap::new();
        let mut args = args;

        let param_len = self.db.env(function_env_id).parameters().len();

        for (i, symbol_id) in self
            .db
            .env(function_env_id)
            .parameters()
            .into_iter()
            .enumerate()
        {
            if i + 1 == param_len && ty.rest == Rest::Spread {
                let mut rest = self.db.alloc_hir(Hir::Atom(Vec::new()));
                for (i, arg) in args.clone().into_iter().rev().enumerate() {
                    if i == 0 && varargs {
                        rest = arg;
                        continue;
                    }
                    rest = self.db.alloc_hir(Hir::Pair(arg, rest));
                }
                inline_parameter_map.insert(symbol_id, self.opt_hir(env_id, rest));
                continue;
            }

            let hir_id = args.remove(0);
            inline_parameter_map.insert(symbol_id, self.opt_hir(env_id, hir_id));
        }

        self.inline_parameter_stack.push(inline_parameter_map);
        let result = self.opt_hir(env_id, hir_id);
        self.inline_parameter_stack.pop().unwrap();

        result
    }

    fn opt_add(&mut self, env_id: EnvironmentId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(env_id, lhs);
        let rhs = self.opt_hir(env_id, rhs);
        self.db.alloc_lir(Lir::Add(vec![lhs, rhs]))
    }

    fn opt_subtract(&mut self, env_id: EnvironmentId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(env_id, lhs);
        let rhs = self.opt_hir(env_id, rhs);
        self.db.alloc_lir(Lir::Sub(vec![lhs, rhs]))
    }

    fn opt_multiply(&mut self, env_id: EnvironmentId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(env_id, lhs);
        let rhs = self.opt_hir(env_id, rhs);
        self.db.alloc_lir(Lir::Mul(vec![lhs, rhs]))
    }

    fn opt_divide(&mut self, env_id: EnvironmentId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(env_id, lhs);
        let rhs = self.opt_hir(env_id, rhs);
        self.db.alloc_lir(Lir::Div(lhs, rhs))
    }

    fn opt_remainder(&mut self, env_id: EnvironmentId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(env_id, lhs);
        let rhs = self.opt_hir(env_id, rhs);
        let divmod = self.db.alloc_lir(Lir::Divmod(lhs, rhs));
        self.db.alloc_lir(Lir::Rest(divmod))
    }

    fn opt_lt(&mut self, env_id: EnvironmentId, lhs: HirId, rhs: HirId) -> LirId {
        self.opt_gt(env_id, rhs, lhs)
    }

    fn opt_gt(&mut self, env_id: EnvironmentId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(env_id, lhs);
        let rhs = self.opt_hir(env_id, rhs);
        self.db.alloc_lir(Lir::Gt(lhs, rhs))
    }

    fn opt_lteq(&mut self, env_id: EnvironmentId, lhs: HirId, rhs: HirId) -> LirId {
        let gt = self.opt_gt(env_id, lhs, rhs);
        self.db.alloc_lir(Lir::Not(gt))
    }

    fn opt_gteq(&mut self, env_id: EnvironmentId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(env_id, lhs);
        let rhs = self.opt_hir(env_id, rhs);
        let eq = self.db.alloc_lir(Lir::Eq(lhs, rhs));
        let gt = self.db.alloc_lir(Lir::Gt(lhs, rhs));
        self.db.alloc_lir(Lir::Any(vec![eq, gt]))
    }

    fn opt_eq(&mut self, env_id: EnvironmentId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(env_id, lhs);
        let rhs = self.opt_hir(env_id, rhs);
        self.db.alloc_lir(Lir::Eq(lhs, rhs))
    }

    fn opt_neq(&mut self, env_id: EnvironmentId, lhs: HirId, rhs: HirId) -> LirId {
        let eq = self.opt_eq(env_id, lhs, rhs);
        self.db.alloc_lir(Lir::Not(eq))
    }

    fn opt_concat(&mut self, env_id: EnvironmentId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(env_id, lhs);
        let rhs = self.opt_hir(env_id, rhs);
        self.db.alloc_lir(Lir::Concat(vec![lhs, rhs]))
    }

    fn opt_point_add(&mut self, env_id: EnvironmentId, lhs: HirId, rhs: HirId) -> LirId {
        let lhs = self.opt_hir(env_id, lhs);
        let rhs = self.opt_hir(env_id, rhs);
        self.db.alloc_lir(Lir::PointAdd(vec![lhs, rhs]))
    }

    fn opt_logical_and(&mut self, env_id: EnvironmentId, lhs: HirId, rhs: HirId) -> LirId {
        let nil = self.db.alloc_hir(Hir::Atom(Vec::new()));
        self.opt_if(env_id, lhs, rhs, nil)
    }

    fn opt_logical_or(&mut self, env_id: EnvironmentId, lhs: HirId, rhs: HirId) -> LirId {
        let one = self.db.alloc_hir(Hir::Atom(vec![1]));
        self.opt_if(env_id, lhs, one, rhs)
    }

    fn opt_not(&mut self, env_id: EnvironmentId, value: HirId) -> LirId {
        let value = self.opt_hir(env_id, value);
        self.db.alloc_lir(Lir::Not(value))
    }

    fn opt_raise(&mut self, env_id: EnvironmentId, value: Option<HirId>) -> LirId {
        let value = value.map(|value| self.opt_hir(env_id, value));
        self.db.alloc_lir(Lir::Raise(value))
    }

    fn opt_if(
        &mut self,
        env_id: EnvironmentId,
        condition: HirId,
        then_block: HirId,
        else_block: HirId,
    ) -> LirId {
        let condition = self.opt_hir(env_id, condition);
        let then_branch = self.opt_hir(env_id, then_block);
        let else_branch = self.opt_hir(env_id, else_block);

        let then_branch = self.db.alloc_lir(Lir::Quote(then_branch));
        let else_branch = self.db.alloc_lir(Lir::Quote(else_branch));
        let if_expr = self
            .db
            .alloc_lir(Lir::If(condition, then_branch, else_branch));

        self.db.alloc_lir(Lir::Run(if_expr, None))
    }
}

fn compose_paths(a: u32, mut b: u32) -> u32 {
    let mut mask = 1;
    let mut temp_path = a;
    while temp_path > 1 {
        b <<= 1;
        mask <<= 1;
        temp_path >>= 1;
    }

    mask -= 1;
    b | (a & mask)
}

const FIRST: u32 = 2;
const REST: u32 = 3;

fn f(path: u32) -> u32 {
    compose_paths(path, FIRST)
}

fn r(path: u32) -> u32 {
    compose_paths(path, REST)
}
