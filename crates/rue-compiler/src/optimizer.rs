use id_arena::Arena;
use num_bigint::BigInt;
use num_traits::One;
use rue_clvm::{first_path, rest_path};
use rue_codegen::{Lir, LirId};
use rue_typing::bigint_to_bytes;

use crate::{
    database::{Database, SymbolId},
    hir::{BinOp, Op},
    mir::Mir,
    EnvironmentId, MirId,
};

#[derive(Debug)]
pub struct Optimizer<'a> {
    db: &'a mut Database,
    lir: &'a mut Arena<Lir>,
}

impl<'a> Optimizer<'a> {
    pub fn new(db: &'a mut Database, lir: &'a mut Arena<Lir>) -> Self {
        Self { db, lir }
    }

    pub fn opt_mir(&mut self, env_id: EnvironmentId, mir_id: MirId) -> LirId {
        match self.db.mir(mir_id).clone() {
            Mir::Atom(atom) => self.lir.alloc(Lir::Atom(atom.clone())),
            Mir::Pair(first, rest) => self.opt_pair(env_id, first, rest),
            Mir::Reference(symbol_id) => self.opt_path(env_id, symbol_id),
            Mir::Op(Op::First, value) => self.opt_first(env_id, value),
            Mir::Op(Op::Rest, value) => self.opt_rest(env_id, value),
            Mir::Op(Op::Not, value) => self.opt_not(env_id, value),
            Mir::Op(Op::Sha256, value) => self.opt_sha256(env_id, value),
            Mir::Op(Op::Listp, value) => self.opt_listp(env_id, value),
            Mir::Op(Op::Strlen, value) => self.opt_strlen(env_id, value),
            Mir::Op(Op::PubkeyForExp, value) => self.opt_pubkey_for_exp(env_id, value),
            Mir::Op(Op::BitwiseNot, value) => self.opt_bitwise_not(env_id, value),
            Mir::Raise(value) => self.opt_raise(env_id, value),
            Mir::BinaryOp(op, lhs, rhs) => {
                let handler = match op {
                    BinOp::BitwiseAnd => Self::opt_bitwise_and,
                    BinOp::BitwiseOr => Self::opt_bitwise_or,
                    BinOp::BitwiseXor => Self::opt_bitwise_xor,
                    BinOp::LeftArithShift => Self::opt_left_arith_shift,
                    BinOp::RightArithShift => Self::opt_right_arith_shift,
                    BinOp::Add => Self::opt_add,
                    BinOp::Subtract => Self::opt_subtract,
                    BinOp::Multiply => Self::opt_multiply,
                    BinOp::Divide => Self::opt_divide,
                    BinOp::Remainder => Self::opt_remainder,
                    BinOp::DivMod => Self::opt_divmod,
                    BinOp::LessThan => Self::opt_lt,
                    BinOp::GreaterThan => Self::opt_gt,
                    BinOp::LessThanEquals => Self::opt_lteq,
                    BinOp::GreaterThanEquals => Self::opt_gteq,
                    BinOp::LessThanBytes => Self::opt_lt_bytes,
                    BinOp::GreaterThanBytes => Self::opt_gt_bytes,
                    BinOp::LessThanBytesEquals => Self::opt_lteq_bytes,
                    BinOp::GreaterThanBytesEquals => Self::opt_gteq_bytes,
                    BinOp::Equals => Self::opt_eq,
                    BinOp::NotEquals => Self::opt_neq,
                    BinOp::Concat => Self::opt_concat,
                    BinOp::PointAdd => Self::opt_point_add,
                    BinOp::LogicalAnd => Self::opt_logical_and,
                    BinOp::LogicalOr => Self::opt_logical_or,
                    BinOp::All => Self::opt_all,
                    BinOp::Any => Self::opt_any,
                };
                handler(self, env_id, lhs, rhs)
            }
            Mir::Substr(value, start, end) => {
                let value = self.opt_mir(env_id, value);
                let start = self.opt_mir(env_id, start);
                let end = self.opt_mir(env_id, end);
                self.lir.alloc(Lir::Substr(value, start, end))
            }
            Mir::If(condition, then_block, else_block) => {
                self.opt_if(env_id, condition, then_block, else_block)
            }
            Mir::Closure(body, args) => self.opt_closure(env_id, body, args),
            Mir::Curry(body, args) => self.opt_curry(env_id, body, args),
            Mir::Run(body, args) => self.opt_run(env_id, body, args),
            Mir::Quote(value) => self.opt_quote(env_id, value),
            Mir::Environment(env_id, mir_id) => self.opt_mir(env_id, mir_id),
        }
    }

    fn opt_closure(&mut self, env_id: EnvironmentId, body: MirId, args: Vec<MirId>) -> LirId {
        let body = self.opt_mir(env_id, body);
        let args = args
            .into_iter()
            .map(|arg| self.opt_mir(env_id, arg))
            .collect();
        self.lir.alloc(Lir::Closure(body, args))
    }

    fn opt_curry(&mut self, env_id: EnvironmentId, body: MirId, args: Vec<MirId>) -> LirId {
        let body = self.opt_mir(env_id, body);
        let args = args
            .into_iter()
            .map(|arg| self.opt_mir(env_id, arg))
            .collect();
        self.lir.alloc(Lir::Curry(body, args))
    }

    fn opt_run(&mut self, env_id: EnvironmentId, body: MirId, args: MirId) -> LirId {
        let body = self.opt_mir(env_id, body);
        let args = self.opt_mir(env_id, args);
        self.lir.alloc(Lir::Run(body, Some(args)))
    }

    fn opt_quote(&mut self, env_id: EnvironmentId, mir_id: MirId) -> LirId {
        let value = self.opt_mir(env_id, mir_id);
        self.lir.alloc(Lir::Quote(value))
    }

    fn opt_path(&mut self, mut env_id: EnvironmentId, symbol_id: SymbolId) -> LirId {
        let mut environment = Vec::new();
        let mut rest;

        loop {
            environment.extend(self.db.env(env_id).build());
            rest = self.db.env(env_id).rest();

            let Some(parent_env_id) = self.db.env(env_id).parent() else {
                break;
            };

            assert!(self.db.env(env_id).parameters().is_empty());
            assert!(!self.db.env(env_id).rest());
            env_id = parent_env_id;
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

        let mut path = BigInt::one();

        if !(index + 1 == environment.len() && rest) {
            path *= 2;
        }

        for _ in 0..index {
            path *= 2;
            path += 1;
        }

        self.lir.alloc(Lir::Path(path))
    }

    fn opt_pair(&mut self, env_id: EnvironmentId, first: MirId, rest: MirId) -> LirId {
        let first = self.opt_mir(env_id, first);
        let rest = self.opt_mir(env_id, rest);
        self.lir.alloc(Lir::Pair(first, rest))
    }

    fn opt_first(&mut self, env_id: EnvironmentId, mir_id: MirId) -> LirId {
        let lir_id = self.opt_mir(env_id, mir_id);
        match &self.lir[lir_id] {
            Lir::Path(path) => self.lir.alloc(Lir::Path(first_path(path.clone()))),
            Lir::Pair(first, _) => *first,
            _ => self.lir.alloc(Lir::First(lir_id)),
        }
    }

    fn opt_rest(&mut self, env_id: EnvironmentId, mir_id: MirId) -> LirId {
        let lir_id = self.opt_mir(env_id, mir_id);
        match &self.lir[lir_id] {
            Lir::Path(path) => self.lir.alloc(Lir::Path(rest_path(path.clone()))),
            Lir::Pair(_, rest) => *rest,
            _ => self.lir.alloc(Lir::Rest(lir_id)),
        }
    }

    fn opt_sha256(&mut self, env_id: EnvironmentId, mir_id: MirId) -> LirId {
        let lir_id = self.opt_mir(env_id, mir_id);
        if let Lir::Concat(args) = self.lir[lir_id].clone() {
            return self.lir.alloc(Lir::Sha256(args));
        }
        self.lir.alloc(Lir::Sha256(vec![lir_id]))
    }

    fn opt_listp(&mut self, env_id: EnvironmentId, mir_id: MirId) -> LirId {
        let lir_id = self.opt_mir(env_id, mir_id);
        match self.lir[lir_id] {
            Lir::Pair(..) => self.lir.alloc(Lir::Atom(vec![1])),
            Lir::Atom(..) => self.lir.alloc(Lir::Atom(Vec::new())),
            _ => self.lir.alloc(Lir::Listp(lir_id)),
        }
    }

    fn opt_strlen(&mut self, env_id: EnvironmentId, mir_id: MirId) -> LirId {
        let lir_id = self.opt_mir(env_id, mir_id);
        match &self.lir[lir_id] {
            Lir::Atom(atom) => self
                .lir
                .alloc(Lir::Atom(bigint_to_bytes(BigInt::from(atom.len())))),
            _ => self.lir.alloc(Lir::Strlen(lir_id)),
        }
    }

    fn opt_pubkey_for_exp(&mut self, env_id: EnvironmentId, mir_id: MirId) -> LirId {
        let lir_id = self.opt_mir(env_id, mir_id);
        self.lir.alloc(Lir::PubkeyForExp(lir_id))
    }

    fn opt_bitwise_not(&mut self, env_id: EnvironmentId, mir_id: MirId) -> LirId {
        let lir_id = self.opt_mir(env_id, mir_id);
        self.lir.alloc(Lir::LogNot(lir_id))
    }

    fn opt_bitwise_and(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.lir.alloc(Lir::LogAnd(vec![lhs, rhs]))
    }

    fn opt_bitwise_or(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.lir.alloc(Lir::LogIor(vec![lhs, rhs]))
    }

    fn opt_bitwise_xor(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.lir.alloc(Lir::LogXor(vec![lhs, rhs]))
    }

    fn opt_right_arith_shift(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        let nil = self.lir.alloc(Lir::Atom(Vec::new()));
        let neg_rhs = self.lir.alloc(Lir::Sub(vec![nil, rhs]));
        self.lir.alloc(Lir::Ash(lhs, neg_rhs))
    }

    fn opt_left_arith_shift(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.lir.alloc(Lir::Ash(lhs, rhs))
    }

    fn opt_add(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        let mut args = Vec::new();
        if let Lir::Add(lhs) = &self.lir[lhs] {
            args.extend(lhs);
        } else {
            args.push(lhs);
        }
        if let Lir::Add(rhs) = &self.lir[rhs] {
            args.extend(rhs);
        } else {
            args.push(rhs);
        }
        self.lir.alloc(Lir::Add(args))
    }

    fn opt_subtract(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.lir.alloc(Lir::Sub(vec![lhs, rhs]))
    }

    fn opt_multiply(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        let mut args = Vec::new();
        if let Lir::Mul(lhs) = &self.lir[lhs] {
            args.extend(lhs);
        } else {
            args.push(lhs);
        }
        if let Lir::Mul(rhs) = &self.lir[rhs] {
            args.extend(rhs);
        } else {
            args.push(rhs);
        }
        self.lir.alloc(Lir::Mul(args))
    }

    fn opt_divide(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.lir.alloc(Lir::Div(lhs, rhs))
    }

    fn opt_remainder(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.lir.alloc(Lir::Rem(lhs, rhs))
    }

    fn opt_lt(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        self.opt_gt(env_id, rhs, lhs)
    }

    fn opt_gt(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.lir.alloc(Lir::Gt(lhs, rhs))
    }

    fn opt_lteq(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let gt = self.opt_gt(env_id, lhs, rhs);
        self.lir.alloc(Lir::Not(gt))
    }

    fn opt_gteq(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        let eq = self.lir.alloc(Lir::Eq(lhs, rhs));
        let gt = self.lir.alloc(Lir::Gt(lhs, rhs));
        self.lir.alloc(Lir::Any(vec![eq, gt]))
    }

    fn opt_lt_bytes(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        self.opt_gt_bytes(env_id, rhs, lhs)
    }

    fn opt_gt_bytes(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.lir.alloc(Lir::GtBytes(lhs, rhs))
    }

    fn opt_lteq_bytes(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let gt = self.opt_gt_bytes(env_id, lhs, rhs);
        self.lir.alloc(Lir::Not(gt))
    }

    fn opt_gteq_bytes(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        let eq = self.lir.alloc(Lir::Eq(lhs, rhs));
        let gt = self.lir.alloc(Lir::GtBytes(lhs, rhs));
        self.lir.alloc(Lir::Any(vec![eq, gt]))
    }

    fn opt_eq(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.lir.alloc(Lir::Eq(lhs, rhs))
    }

    fn opt_neq(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let eq = self.opt_eq(env_id, lhs, rhs);
        self.lir.alloc(Lir::Not(eq))
    }

    fn opt_concat(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        let mut args = Vec::new();
        if let Lir::Concat(lhs) = &self.lir[lhs] {
            args.extend(lhs);
        } else {
            args.push(lhs);
        }
        if let Lir::Concat(rhs) = &self.lir[rhs] {
            args.extend(rhs);
        } else {
            args.push(rhs);
        }
        self.lir.alloc(Lir::Concat(args))
    }

    fn opt_point_add(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.lir.alloc(Lir::PointAdd(vec![lhs, rhs]))
    }

    fn opt_logical_and(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let nil = self.db.alloc_mir(Mir::Atom(Vec::new()));
        self.opt_if(env_id, lhs, rhs, nil)
    }

    fn opt_logical_or(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let one = self.db.alloc_mir(Mir::Atom(vec![1]));
        self.opt_if(env_id, lhs, one, rhs)
    }

    fn opt_any(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        let mut args = Vec::new();
        if let Lir::Any(lhs) = &self.lir[lhs] {
            args.extend(lhs);
        } else {
            args.push(lhs);
        }
        if let Lir::Any(rhs) = &self.lir[rhs] {
            args.extend(rhs);
        } else {
            args.push(rhs);
        }
        self.lir.alloc(Lir::Any(args))
    }

    fn opt_all(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        let mut args = Vec::new();
        if let Lir::All(lhs) = &self.lir[lhs] {
            args.extend(lhs);
        } else {
            args.push(lhs);
        }
        if let Lir::All(rhs) = &self.lir[rhs] {
            args.extend(rhs);
        } else {
            args.push(rhs);
        }
        self.lir.alloc(Lir::All(args))
    }

    fn opt_not(&mut self, env_id: EnvironmentId, value: MirId) -> LirId {
        let value = self.opt_mir(env_id, value);
        match self.lir[value] {
            Lir::Not(value) => value,
            _ => self.lir.alloc(Lir::Not(value)),
        }
    }

    fn opt_divmod(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.lir.alloc(Lir::Divmod(lhs, rhs))
    }

    fn opt_raise(&mut self, env_id: EnvironmentId, value: Option<MirId>) -> LirId {
        let value = value.map(|value| self.opt_mir(env_id, value));
        self.lir.alloc(Lir::Raise(value))
    }

    fn opt_if(
        &mut self,
        env_id: EnvironmentId,
        condition: MirId,
        then_block: MirId,
        else_block: MirId,
    ) -> LirId {
        let condition = self.opt_mir(env_id, condition);
        let then_branch = self.opt_mir(env_id, then_block);
        let else_branch = self.opt_mir(env_id, else_block);

        let then_branch = self.lir.alloc(Lir::Quote(then_branch));
        let else_branch = self.lir.alloc(Lir::Quote(else_branch));
        let if_expr = self.lir.alloc(Lir::If(condition, then_branch, else_branch));

        self.lir.alloc(Lir::Run(if_expr, None))
    }
}
