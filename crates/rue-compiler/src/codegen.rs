use clvmr::{Allocator, NodePtr};

use crate::{
    database::{Database, LirId},
    lir::Lir,
};

pub struct Codegen<'a> {
    db: &'a mut Database,
    allocator: &'a mut Allocator,
    ops: Ops,
}

struct Ops {
    q: NodePtr,
    a: NodePtr,
    i: NodePtr,
    c: NodePtr,
    f: NodePtr,
    r: NodePtr,
    l: NodePtr,
    x: NodePtr,
    eq: NodePtr,
    sha256: NodePtr,
    strlen: NodePtr,
    concat: NodePtr,
    add: NodePtr,
    sub: NodePtr,
    mul: NodePtr,
    div: NodePtr,
    divmod: NodePtr,
    gt: NodePtr,
    not: NodePtr,
    any: NodePtr,
}

impl<'a> Codegen<'a> {
    pub fn new(db: &'a mut Database, allocator: &'a mut Allocator) -> Self {
        let ops = Ops {
            q: allocator.one(),
            a: allocator.new_small_number(2).unwrap(),
            i: allocator.new_small_number(3).unwrap(),
            c: allocator.new_small_number(4).unwrap(),
            f: allocator.new_small_number(5).unwrap(),
            r: allocator.new_small_number(6).unwrap(),
            l: allocator.new_small_number(7).unwrap(),
            x: allocator.new_small_number(8).unwrap(),
            eq: allocator.new_small_number(9).unwrap(),
            sha256: allocator.new_small_number(11).unwrap(),
            strlen: allocator.new_small_number(13).unwrap(),
            concat: allocator.new_small_number(14).unwrap(),
            add: allocator.new_small_number(16).unwrap(),
            sub: allocator.new_small_number(17).unwrap(),
            mul: allocator.new_small_number(18).unwrap(),
            div: allocator.new_small_number(19).unwrap(),
            divmod: allocator.new_small_number(20).unwrap(),
            gt: allocator.new_small_number(21).unwrap(),
            not: allocator.new_small_number(32).unwrap(),
            any: allocator.new_small_number(33).unwrap(),
        };
        Self { db, allocator, ops }
    }

    pub fn gen_lir(&mut self, lir_id: LirId) -> NodePtr {
        match self.db.lir(lir_id).clone() {
            Lir::Atom(atom) => self.gen_atom(atom.clone()),
            Lir::Pair(first, rest) => self.gen_pair(first, rest),
            Lir::Path(path) => self.allocator.new_small_number(path).unwrap(),
            Lir::Run(program, args) => self.gen_run(program, args),
            Lir::Curry(body, args) => self.gen_apply(body, args),
            Lir::Closure(body, args) => self.gen_closure(body, args),
            Lir::FunctionBody(body) => self.gen_quote(body),
            Lir::First(value) => self.gen_first(value),
            Lir::Rest(value) => self.gen_rest(value),
            Lir::Raise(value) => self.gen_raise(value),
            Lir::Sha256(value) => self.gen_sha256(value),
            Lir::IsCons(value) => self.gen_is_cons(value),
            Lir::Strlen(value) => self.gen_strlen(value),
            Lir::Concat(values) => self.gen_concat(values),
            Lir::If(condition, then_branch, else_branch) => {
                self.gen_if(condition, then_branch, else_branch)
            }
            Lir::Not(value) => self.gen_not(value),
            Lir::Any(values) => self.gen_any(values),
            Lir::Add(values) => self.gen_add(values),
            Lir::Sub(values) => self.gen_sub(values),
            Lir::Mul(values) => self.gen_mul(values),
            Lir::Div(lhs, rhs) => self.gen_div(lhs, rhs),
            Lir::Divmod(lhs, rhs) => self.gen_divmod(lhs, rhs),
            Lir::Eq(lhs, rhs) => self.gen_eq(lhs, rhs),
            Lir::Gt(lhs, rhs) => self.gen_gt(lhs, rhs),
        }
    }

    fn gen_atom(&mut self, value: Vec<u8>) -> NodePtr {
        let int_ptr = self.allocator.new_atom(&value).unwrap();
        self.quote(int_ptr)
    }

    fn gen_pair(&mut self, first: LirId, rest: LirId) -> NodePtr {
        let first = self.gen_lir(first);
        let rest = self.gen_lir(rest);
        self.list(&[self.ops.c, first, rest])
    }

    fn gen_run(&mut self, program: LirId, args: LirId) -> NodePtr {
        let program = self.gen_lir(program);
        let args = self.gen_lir(args);
        self.list(&[self.ops.a, program, args])
    }

    fn gen_apply(&mut self, body: LirId, args: Vec<LirId>) -> NodePtr {
        let body = self.gen_quote(body);
        let args: Vec<NodePtr> = args.into_iter().map(|arg| self.gen_lir(arg)).collect();
        let args = self.runtime_list(&args, self.ops.q);
        self.list(&[self.ops.a, body, args])
    }

    fn gen_closure(&mut self, body: LirId, args: Vec<LirId>) -> NodePtr {
        let body = self.gen_lir(body);
        let args: Vec<NodePtr> = args.into_iter().map(|arg| self.gen_lir(arg)).collect();
        self.gen_closure_wrapper(body, &args)
    }

    fn gen_quote(&mut self, body: LirId) -> NodePtr {
        let body = self.gen_lir(body);
        self.quote(body)
    }

    fn gen_first(&mut self, value: LirId) -> NodePtr {
        let value = self.gen_lir(value);
        self.list(&[self.ops.f, value])
    }

    fn gen_rest(&mut self, value: LirId) -> NodePtr {
        let value = self.gen_lir(value);
        self.list(&[self.ops.r, value])
    }

    fn gen_raise(&mut self, value: Option<LirId>) -> NodePtr {
        if let Some(value) = value {
            let value = self.gen_lir(value);
            self.list(&[self.ops.x, value])
        } else {
            self.list(&[self.ops.x])
        }
    }

    fn gen_sha256(&mut self, value: LirId) -> NodePtr {
        let value = self.gen_lir(value);
        self.list(&[self.ops.sha256, value])
    }

    fn gen_is_cons(&mut self, value: LirId) -> NodePtr {
        let value = self.gen_lir(value);
        self.list(&[self.ops.l, value])
    }

    fn gen_strlen(&mut self, value: LirId) -> NodePtr {
        let value = self.gen_lir(value);
        self.list(&[self.ops.strlen, value])
    }

    fn gen_concat(&mut self, values: Vec<LirId>) -> NodePtr {
        let mut args = vec![self.ops.concat];
        for value in values {
            args.push(self.gen_lir(value));
        }
        self.list(&args)
    }

    fn gen_if(&mut self, condition: LirId, then_branch: LirId, else_branch: LirId) -> NodePtr {
        let condition = self.gen_lir(condition);
        let then_branch = self.gen_lir(then_branch);
        let else_branch = self.gen_lir(else_branch);

        let then_branch = self.quote(then_branch);
        let else_branch = self.quote(else_branch);

        let conditional = self.list(&[self.ops.i, condition, then_branch, else_branch]);
        self.list(&[self.ops.a, conditional, self.ops.q])
    }

    fn gen_not(&mut self, value: LirId) -> NodePtr {
        let value = self.gen_lir(value);
        self.list(&[self.ops.not, value])
    }

    fn gen_any(&mut self, values: Vec<LirId>) -> NodePtr {
        let mut args = vec![self.ops.any];
        for value in values {
            args.push(self.gen_lir(value));
        }
        self.list(&args)
    }

    fn gen_add(&mut self, values: Vec<LirId>) -> NodePtr {
        let mut args = vec![self.ops.add];
        for value in values {
            args.push(self.gen_lir(value));
        }
        self.list(&args)
    }

    fn gen_sub(&mut self, values: Vec<LirId>) -> NodePtr {
        let mut args = vec![self.ops.sub];
        for value in values {
            args.push(self.gen_lir(value));
        }
        self.list(&args)
    }

    fn gen_mul(&mut self, values: Vec<LirId>) -> NodePtr {
        let mut args = vec![self.ops.mul];
        for value in values {
            args.push(self.gen_lir(value));
        }
        self.list(&args)
    }

    fn gen_div(&mut self, lhs: LirId, rhs: LirId) -> NodePtr {
        let lhs = self.gen_lir(lhs);
        let rhs = self.gen_lir(rhs);
        self.list(&[self.ops.div, lhs, rhs])
    }

    fn gen_divmod(&mut self, lhs: LirId, rhs: LirId) -> NodePtr {
        let lhs = self.gen_lir(lhs);
        let rhs = self.gen_lir(rhs);
        self.list(&[self.ops.divmod, lhs, rhs])
    }

    fn gen_gt(&mut self, lhs: LirId, rhs: LirId) -> NodePtr {
        let lhs = self.gen_lir(lhs);
        let rhs = self.gen_lir(rhs);
        self.list(&[self.ops.gt, lhs, rhs])
    }

    fn gen_eq(&mut self, lhs: LirId, rhs: LirId) -> NodePtr {
        let lhs = self.gen_lir(lhs);
        let rhs = self.gen_lir(rhs);
        self.list(&[self.ops.eq, lhs, rhs])
    }

    fn gen_closure_wrapper(&mut self, body: NodePtr, captures: &[NodePtr]) -> NodePtr {
        let runtime_a = self.quote(self.ops.a);
        let runtime_quoted_body = self.runtime_quote(body);

        let mut args = Vec::new();

        for &capture in captures {
            let runtime_quoted_arg = self.runtime_quote(capture);
            args.push(runtime_quoted_arg);
        }

        let quoted_one = self.quote(self.ops.q);
        let runtime_args = self.runtime_runtime_list(&args, quoted_one);

        self.runtime_list(
            &[runtime_a, runtime_quoted_body, runtime_args],
            NodePtr::NIL,
        )
    }

    fn quote(&mut self, ptr: NodePtr) -> NodePtr {
        if ptr.is_atom() && self.allocator.atom(ptr).as_ref().is_empty() {
            return ptr;
        }
        self.allocator.new_pair(self.ops.q, ptr).unwrap()
    }

    fn list(&mut self, items: &[NodePtr]) -> NodePtr {
        let mut ptr = self.allocator.nil();
        for &item in items.iter().rev() {
            ptr = self.allocator.new_pair(item, ptr).unwrap();
        }
        ptr
    }

    fn runtime_list(&mut self, items: &[NodePtr], end: NodePtr) -> NodePtr {
        let mut ptr = end;

        for &item in items.iter().rev() {
            ptr = self.list(&[self.ops.c, item, ptr]);
        }
        ptr
    }

    fn runtime_runtime_list(&mut self, items: &[NodePtr], end: NodePtr) -> NodePtr {
        let mut ptr = end;

        let quoted_c = self.quote(self.ops.c);
        for &item in items.iter().rev() {
            ptr = self.runtime_list(&[quoted_c, item, ptr], NodePtr::NIL);
        }
        ptr
    }

    fn runtime_quote(&mut self, ptr: NodePtr) -> NodePtr {
        let quoted_q = self.quote(self.ops.q);
        self.list(&[self.ops.c, quoted_q, ptr])
    }
}
