use crate::{
    database::{Database, LirId, SymbolId},
    hir::{BinOp, Op},
    lir::Lir,
    mir::Mir,
    EnvironmentId, MirId,
};

#[derive(Debug)]
pub struct Optimizer<'a> {
    db: &'a mut Database,
}

impl<'a> Optimizer<'a> {
    pub fn new(db: &'a mut Database) -> Self {
        Self { db }
    }

    pub fn opt_mir(&mut self, env_id: EnvironmentId, mir_id: MirId) -> LirId {
        match self.db.mir(mir_id).clone() {
            Mir::Atom(atom) => self.db.alloc_lir(Lir::Atom(atom.clone())),
            Mir::Pair(first, rest) => self.opt_pair(env_id, first, rest),
            Mir::Reference(symbol_id) => self.opt_path(env_id, symbol_id),
            Mir::Op(Op::First, value) => self.opt_first(env_id, value),
            Mir::Op(Op::Rest, value) => self.opt_rest(env_id, value),
            Mir::Op(Op::Not, value) => self.opt_not(env_id, value),
            Mir::Op(Op::Sha256, value) => self.opt_sha256(env_id, value),
            Mir::Op(Op::Listp, value) => self.opt_listp(env_id, value),
            Mir::Op(Op::Strlen, value) => self.opt_strlen(env_id, value),
            Mir::Op(Op::PubkeyForExp, value) => self.opt_pubkey_for_exp(env_id, value),
            Mir::Op(Op::Exists, value) => self.opt_check_exists(env_id, value),
            Mir::Raise(value) => self.opt_raise(env_id, value),
            Mir::BinaryOp(op, lhs, rhs) => {
                let handler = match op {
                    BinOp::BitwiseAnd => Self::opt_bitwise_and,
                    BinOp::BitwiseOr => Self::opt_bitwise_or,
                    BinOp::BitwiseXor => Self::opt_bitwise_xor,
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
        self.db.alloc_lir(Lir::Closure(body, args))
    }

    fn opt_curry(&mut self, env_id: EnvironmentId, body: MirId, args: Vec<MirId>) -> LirId {
        let body = self.opt_mir(env_id, body);
        let args = args
            .into_iter()
            .map(|arg| self.opt_mir(env_id, arg))
            .collect();
        self.db.alloc_lir(Lir::Curry(body, args))
    }

    fn opt_run(&mut self, env_id: EnvironmentId, body: MirId, args: MirId) -> LirId {
        let body = self.opt_mir(env_id, body);
        let args = self.opt_mir(env_id, args);
        self.db.alloc_lir(Lir::Run(body, Some(args)))
    }

    fn opt_quote(&mut self, env_id: EnvironmentId, mir_id: MirId) -> LirId {
        let value = self.opt_mir(env_id, mir_id);
        self.db.alloc_lir(Lir::Quote(value))
    }

    fn opt_path(&mut self, env_id: EnvironmentId, symbol_id: SymbolId) -> LirId {
        let mut current_env_id = env_id;
        let mut environment: Vec<SymbolId> = self.db.env(env_id).build().into_iter().collect();

        while let Some(parent_env_id) = self.db.env(current_env_id).parent() {
            assert!(self.db.env(current_env_id).parameters().is_empty());
            assert!(!self.db.env(current_env_id).rest());

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

        if !(index + 1 == environment.len() && self.db.env(env_id).rest()) {
            path *= 2;
        }

        for _ in 0..index {
            path *= 2;
            path += 1;
        }

        self.db.alloc_lir(Lir::Path(path))
    }

    fn opt_check_exists(&mut self, env_id: EnvironmentId, mir_id: MirId) -> LirId {
        let value = self.opt_mir(env_id, mir_id);
        let Lir::Path(path) = self.db.lir(value).clone() else {
            panic!("invalid LIR, expected path for existence check");
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

    fn opt_pair(&mut self, env_id: EnvironmentId, first: MirId, rest: MirId) -> LirId {
        let first = self.opt_mir(env_id, first);
        let rest = self.opt_mir(env_id, rest);
        self.db.alloc_lir(Lir::Pair(first, rest))
    }

    fn opt_first(&mut self, env_id: EnvironmentId, mir_id: MirId) -> LirId {
        let lir_id = self.opt_mir(env_id, mir_id);
        match self.db.lir(lir_id) {
            Lir::Path(path) => self.db.alloc_lir(Lir::Path(f(*path))),
            Lir::Pair(first, _) => *first,
            _ => self.db.alloc_lir(Lir::First(lir_id)),
        }
    }

    fn opt_rest(&mut self, env_id: EnvironmentId, mir_id: MirId) -> LirId {
        let lir_id = self.opt_mir(env_id, mir_id);
        match self.db.lir(lir_id) {
            Lir::Path(path) => self.db.alloc_lir(Lir::Path(r(*path))),
            Lir::Pair(_, rest) => *rest,
            _ => self.db.alloc_lir(Lir::Rest(lir_id)),
        }
    }

    fn opt_sha256(&mut self, env_id: EnvironmentId, mir_id: MirId) -> LirId {
        let lir_id = self.opt_mir(env_id, mir_id);
        if let Lir::Concat(args) = self.db.lir(lir_id).clone() {
            return self.db.alloc_lir(Lir::Sha256(args));
        }
        self.db.alloc_lir(Lir::Sha256(vec![lir_id]))
    }

    fn opt_listp(&mut self, env_id: EnvironmentId, mir_id: MirId) -> LirId {
        let lir_id = self.opt_mir(env_id, mir_id);
        self.db.alloc_lir(Lir::Listp(lir_id))
    }

    fn opt_strlen(&mut self, env_id: EnvironmentId, mir_id: MirId) -> LirId {
        let lir_id = self.opt_mir(env_id, mir_id);
        self.db.alloc_lir(Lir::Strlen(lir_id))
    }

    fn opt_pubkey_for_exp(&mut self, env_id: EnvironmentId, mir_id: MirId) -> LirId {
        let lir_id = self.opt_mir(env_id, mir_id);
        self.db.alloc_lir(Lir::PubkeyForExp(lir_id))
    }

    fn opt_bitwise_and(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.db.alloc_lir(Lir::LogAnd(vec![lhs, rhs]))
    }

    fn opt_bitwise_or(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.db.alloc_lir(Lir::LogIor(vec![lhs, rhs]))
    }

    fn opt_bitwise_xor(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.db.alloc_lir(Lir::LogXor(vec![lhs, rhs]))
    }

    fn opt_add(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.db.alloc_lir(Lir::Add(vec![lhs, rhs]))
    }

    fn opt_subtract(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.db.alloc_lir(Lir::Sub(vec![lhs, rhs]))
    }

    fn opt_multiply(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.db.alloc_lir(Lir::Mul(vec![lhs, rhs]))
    }

    fn opt_divide(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.db.alloc_lir(Lir::Div(lhs, rhs))
    }

    fn opt_remainder(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        let divmod = self.db.alloc_lir(Lir::Divmod(lhs, rhs));
        self.db.alloc_lir(Lir::Rest(divmod))
    }

    fn opt_lt(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        self.opt_gt(env_id, rhs, lhs)
    }

    fn opt_gt(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.db.alloc_lir(Lir::Gt(lhs, rhs))
    }

    fn opt_lteq(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let gt = self.opt_gt(env_id, lhs, rhs);
        self.db.alloc_lir(Lir::Not(gt))
    }

    fn opt_gteq(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        let eq = self.db.alloc_lir(Lir::Eq(lhs, rhs));
        let gt = self.db.alloc_lir(Lir::Gt(lhs, rhs));
        self.db.alloc_lir(Lir::Any(vec![eq, gt]))
    }

    fn opt_eq(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.db.alloc_lir(Lir::Eq(lhs, rhs))
    }

    fn opt_neq(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let eq = self.opt_eq(env_id, lhs, rhs);
        self.db.alloc_lir(Lir::Not(eq))
    }

    fn opt_concat(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.db.alloc_lir(Lir::Concat(vec![lhs, rhs]))
    }

    fn opt_point_add(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let lhs = self.opt_mir(env_id, lhs);
        let rhs = self.opt_mir(env_id, rhs);
        self.db.alloc_lir(Lir::PointAdd(vec![lhs, rhs]))
    }

    fn opt_logical_and(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let nil = self.db.alloc_mir(Mir::Atom(Vec::new()));
        self.opt_if(env_id, lhs, rhs, nil)
    }

    fn opt_logical_or(&mut self, env_id: EnvironmentId, lhs: MirId, rhs: MirId) -> LirId {
        let one = self.db.alloc_mir(Mir::Atom(vec![1]));
        self.opt_if(env_id, lhs, one, rhs)
    }

    fn opt_not(&mut self, env_id: EnvironmentId, value: MirId) -> LirId {
        let value = self.opt_mir(env_id, value);
        self.db.alloc_lir(Lir::Not(value))
    }

    fn opt_raise(&mut self, env_id: EnvironmentId, value: Option<MirId>) -> LirId {
        let value = value.map(|value| self.opt_mir(env_id, value));
        self.db.alloc_lir(Lir::Raise(value))
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
