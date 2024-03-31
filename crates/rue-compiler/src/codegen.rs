use std::collections::HashMap;

use clvmr::{Allocator, NodePtr};
use indexmap::IndexSet;
use num_bigint::BigInt;

use crate::{
    database::{Database, ScopeId, SymbolId},
    Symbol, Value,
};

pub struct Codegen<'a> {
    db: Database,
    allocator: &'a mut Allocator,
    captures: HashMap<ScopeId, IndexSet<SymbolId>>,
}

impl<'a> Codegen<'a> {
    pub fn new(db: Database, allocator: &'a mut Allocator) -> Self {
        Self {
            db,
            allocator,
            captures: HashMap::new(),
        }
    }

    fn compute_captures(&mut self, scope_id: ScopeId) {
        if self.captures.contains_key(&scope_id) {
            return;
        }

        self.captures.insert(scope_id, IndexSet::new());

        for used_id in self.db.scope(scope_id).used_symbols().clone() {
            if !self.db.scope(scope_id).definitions().contains(&used_id) {
                self.captures
                    .get_mut(&scope_id)
                    .expect("cannot capture from unknown scope")
                    .insert(used_id);
            }

            let Symbol::Function {
                scope_id: function_scope_id,
                ..
            } = self.db.symbol(used_id)
            else {
                continue;
            };

            let function_scope_id = *function_scope_id;

            self.compute_captures(function_scope_id);

            if !self.db.scope(scope_id).definitions().contains(&used_id) {
                let new_captures = self.captures[&function_scope_id].clone();
                self.captures
                    .get_mut(&scope_id)
                    .expect("cannot capture from unknown scope")
                    .extend(new_captures);
            }
        }
    }

    pub fn gen_main(&mut self, main: SymbolId) -> NodePtr {
        let (scope_id, value) = {
            let Symbol::Function {
                scope_id, value, ..
            } = self.db.symbol(main)
            else {
                unreachable!();
            };
            (*scope_id, value.clone())
        };

        self.compute_captures(scope_id);

        let body = self.gen_value(scope_id, value.clone());
        let quoted_body = self.quote(body);
        let rest = self.allocator.one();
        let a = self
            .allocator
            .new_small_number(2)
            .expect("could not allocate `a`");

        let mut args = Vec::new();

        for symbol_id in self.captures[&scope_id].clone() {
            args.push(self.gen_symbol(scope_id, symbol_id));
        }

        let arg_list = self.runtime_list(&args, rest);

        self.list(&[a, quoted_body, arg_list])
    }

    fn gen_symbol(&mut self, scope_id: ScopeId, symbol_id: SymbolId) -> NodePtr {
        match self.db.symbol(symbol_id) {
            Symbol::Function { .. } => self.gen_value(scope_id, Value::Function(symbol_id)),
            Symbol::Parameter { .. } => todo!(),
        }
    }

    fn gen_value(&mut self, scope_id: ScopeId, value: Value) -> NodePtr {
        match value {
            Value::Nil => self.allocator.nil(),
            Value::Int(int) => self.gen_int(int),
            Value::Reference(symbol_id) => {
                if let Symbol::Function {
                    scope_id: function_scope_id,
                    value: _,
                    ..
                } = self.db.symbol(symbol_id).clone()
                {
                    let q = self.allocator.one();
                    let one = q;
                    let a = self
                        .allocator
                        .new_small_number(2)
                        .expect("could not allocate `a`");

                    let body = self.gen_path(scope_id, symbol_id);

                    let runtime_a = self.quote(a);
                    let runtime_quoted_body = self.runtime_quote(body);

                    let mut args = Vec::new();

                    for symbol_id in self.captures[&function_scope_id].clone() {
                        let path = self.gen_path(scope_id, symbol_id);
                        let runtime_quoted_arg = self.runtime_quote(path);
                        args.push(runtime_quoted_arg);
                    }

                    let quoted_one = self.quote(one);
                    let runtime_args = self.runtime_runtime_list(&args, quoted_one);

                    return self.runtime_list(
                        &[runtime_a, runtime_quoted_body, runtime_args],
                        NodePtr::NIL,
                    );
                }

                self.gen_path(scope_id, symbol_id)
            }
            Value::FunctionCall {
                callee,
                args: arg_values,
            } => {
                let a = self
                    .allocator
                    .new_small_number(2)
                    .expect("could not allocate `a`");

                let mut args = Vec::new();

                let callee = if let Value::Reference(symbol_id) = callee.as_ref() {
                    if let Symbol::Function {
                        scope_id: callee_scope_id,
                        ..
                    } = self.db.symbol(*symbol_id)
                    {
                        for symbol_id in self.captures[&callee_scope_id].clone() {
                            args.push(self.gen_path(scope_id, symbol_id));
                        }
                        self.gen_path(scope_id, *symbol_id)
                    } else {
                        self.gen_value(scope_id, *callee)
                    }
                } else {
                    self.gen_value(scope_id, *callee)
                };

                for arg_value in arg_values {
                    args.push(self.gen_value(scope_id, arg_value));
                }
                let arg_list = self.runtime_list(&args, NodePtr::NIL);

                self.list(&[a, callee, arg_list])
            }
            Value::Function(symbol_id) => {
                let Symbol::Function {
                    scope_id: function_scope_id,
                    value,
                } = &self.db.symbol(symbol_id)
                else {
                    unreachable!();
                };
                let body = self.gen_value(*function_scope_id, value.clone());
                self.quote(body)
            }
            Value::Add(operands) => {
                let plus = self
                    .allocator
                    .new_small_number(16)
                    .expect("could not allocate `+`");

                let mut args = vec![plus];
                for operand in operands {
                    args.push(self.gen_value(scope_id, operand));
                }
                self.list(&args)
            }
            Value::Subtract(operands) => {
                let minus = self
                    .allocator
                    .new_small_number(17)
                    .expect("could not allocate `-`");

                let mut args = vec![minus];
                for operand in operands {
                    args.push(self.gen_value(scope_id, operand));
                }
                self.list(&args)
            }
            Value::Multiply(operands) => {
                let star = self
                    .allocator
                    .new_small_number(18)
                    .expect("could not allocate `*`");

                let mut args = vec![star];
                for operand in operands {
                    args.push(self.gen_value(scope_id, operand));
                }
                self.list(&args)
            }
            Value::Divide(operands) => {
                let slash = self
                    .allocator
                    .new_small_number(19)
                    .expect("could not allocate `/`");

                let mut args = vec![slash];
                for operand in operands {
                    args.push(self.gen_value(scope_id, operand));
                }
                self.list(&args)
            }
            Value::LessThan(lhs, rhs) => {
                let not = self
                    .allocator
                    .new_small_number(32)
                    .expect("could not allocate `not`");
                let any = self
                    .allocator
                    .new_small_number(33)
                    .expect("could not allocate `any`");
                let gt = self
                    .allocator
                    .new_small_number(21)
                    .expect("could not allocate `>`");
                let eq = self
                    .allocator
                    .new_small_number(9)
                    .expect("could not allocate `=`");

                let lhs = self.gen_value(scope_id, *lhs);
                let rhs = self.gen_value(scope_id, *rhs);
                let operands = self.list(&[lhs, rhs]);

                let eq_list = self
                    .allocator
                    .new_pair(eq, operands)
                    .expect("could not allocate `=` list");
                let gt_list = self
                    .allocator
                    .new_pair(gt, operands)
                    .expect("could not allocate `>` list");
                let any_list = self.list(&[any, eq_list, gt_list]);
                self.list(&[not, any_list])
            }
            Value::GreaterThan(lhs, rhs) => {
                let gt = self
                    .allocator
                    .new_small_number(21)
                    .expect("could not allocate `>`");

                let mut args = vec![gt];
                args.push(self.gen_value(scope_id, *lhs));
                args.push(self.gen_value(scope_id, *rhs));
                self.list(&args)
            }
            Value::If {
                condition,
                then_block,
                else_block,
            } => {
                let a = self
                    .allocator
                    .new_small_number(2)
                    .expect("could not allocate `a`");
                let i = self
                    .allocator
                    .new_small_number(3)
                    .expect("could not allocate `i`");

                let all_env = self.allocator.one();

                let condition = self.gen_value(scope_id, *condition);
                let then_block = self.gen_value(scope_id, *then_block);
                let else_block = self.gen_value(scope_id, *else_block);

                let then_block = self.quote(then_block);
                let else_block = self.quote(else_block);

                let conditional = self.list(&[i, condition, then_block, else_block]);
                self.list(&[a, conditional, all_env])
            }
        }
    }

    fn gen_path(&mut self, scope_id: ScopeId, symbol_id: SymbolId) -> NodePtr {
        let index = self.captures[&scope_id]
            .iter()
            .chain(self.db.scope(scope_id).definitions().iter())
            .position(|&id| id == symbol_id)
            .expect("symbol not found");

        let mut path = 2;
        for _ in 0..index {
            path *= 2;
            path += 1;
        }

        self.allocator
            .new_small_number(path)
            .expect("could not allocate path")
    }

    fn gen_int(&mut self, value: BigInt) -> NodePtr {
        let int_ptr = self
            .allocator
            .new_number(value)
            .expect("could not allocate number");
        self.quote(int_ptr)
    }

    fn quote(&mut self, ptr: NodePtr) -> NodePtr {
        if ptr.is_atom() && self.allocator.atom(ptr).as_ref().is_empty() {
            return ptr;
        }

        let q = self.allocator.one();
        self.allocator
            .new_pair(q, ptr)
            .expect("could not allocate quote")
    }

    fn list(&mut self, items: &[NodePtr]) -> NodePtr {
        let mut ptr = self.allocator.nil();
        for &item in items.iter().rev() {
            ptr = self
                .allocator
                .new_pair(item, ptr)
                .expect("could not allocate pair");
        }
        ptr
    }

    fn runtime_list(&mut self, items: &[NodePtr], end: NodePtr) -> NodePtr {
        let mut ptr = end;
        let c = self
            .allocator
            .new_small_number(4)
            .expect("could not allocate `c`");
        for &item in items.iter().rev() {
            ptr = self.list(&[c, item, ptr]);
        }
        ptr
    }

    fn runtime_runtime_list(&mut self, items: &[NodePtr], end: NodePtr) -> NodePtr {
        let mut ptr = end;
        let c = self
            .allocator
            .new_small_number(4)
            .expect("could not allocate `c`");
        let quoted_c = self.quote(c);
        for &item in items.iter().rev() {
            ptr = self.runtime_list(&[quoted_c, item, ptr], NodePtr::NIL);
        }
        ptr
    }

    fn runtime_quote(&mut self, ptr: NodePtr) -> NodePtr {
        let q = self.allocator.one();
        let c = self
            .allocator
            .new_small_number(4)
            .expect("could not allocate `c`");
        let quoted_q = self.quote(q);
        self.list(&[c, quoted_q, ptr])
    }
}
