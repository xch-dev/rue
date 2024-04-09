use std::collections::HashMap;

use clvmr::{Allocator, NodePtr};
use indexmap::IndexSet;
use num_bigint::BigInt;
use num_traits::Zero;
use rue_parser::BinaryOp;

use crate::{
    database::{Database, HirId, ScopeId, SymbolId},
    hir::Hir,
    symbol::Symbol,
};

pub struct Codegen<'a> {
    db: &'a mut Database,
    allocator: &'a mut Allocator,
    ops: Ops,
    captures: HashMap<ScopeId, IndexSet<SymbolId>>,
    environments: HashMap<ScopeId, IndexSet<SymbolId>>,
    scope_inheritance: HashMap<ScopeId, ScopeId>,
}

struct Ops {
    q: NodePtr,
    a: NodePtr,
    i: NodePtr,
    c: NodePtr,
    f: NodePtr,
    r: NodePtr,
    eq: NodePtr,
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
            eq: allocator.new_small_number(9).unwrap(),
            add: allocator.new_small_number(16).unwrap(),
            sub: allocator.new_small_number(17).unwrap(),
            mul: allocator.new_small_number(18).unwrap(),
            div: allocator.new_small_number(19).unwrap(),
            divmod: allocator.new_small_number(20).unwrap(),
            gt: allocator.new_small_number(21).unwrap(),
            not: allocator.new_small_number(32).unwrap(),
            any: allocator.new_small_number(33).unwrap(),
        };
        Self {
            db,
            allocator,
            ops,
            captures: HashMap::new(),
            environments: HashMap::new(),
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
                for arg in args {
                    self.compute_captures_hir(scope_id, arg);
                }
            }
            Hir::BinaryOp { lhs, rhs, .. } => {
                self.compute_captures_hir(scope_id, lhs);
                self.compute_captures_hir(scope_id, rhs);
            }
            Hir::Not(value) => {
                self.compute_captures_hir(scope_id, value);
            }
            Hir::If {
                condition,
                then_block,
                else_block,
            } => {
                self.compute_captures_hir(scope_id, condition);
                self.compute_captures_hir(scope_id, then_block);
                self.compute_captures_hir(scope_id, else_block);
            }
            Hir::List(items) => {
                for item in items {
                    self.compute_captures_hir(scope_id, item);
                }
            }
            Hir::ListIndex { value, .. } => {
                self.compute_captures_hir(scope_id, value);
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
                ..
            } => self.compute_function_captures(scope_id, function_scope_id, hir_id),
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

    pub fn gen_main(&mut self, main: SymbolId) -> NodePtr {
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

        let body = self.gen_hir(scope_id, hir_id);
        let quoted_body = self.quote(body);

        let mut args = Vec::new();

        for symbol_id in self.db.scope(scope_id).local_symbols() {
            if self.db.symbol(symbol_id).is_definition() {
                args.push(self.gen_definition(scope_id, symbol_id));
            }
        }

        for symbol_id in self.captures[&scope_id].clone() {
            args.push(self.gen_definition(scope_id, symbol_id));
        }

        let arg_list = self.runtime_list(&args, self.ops.q);

        self.list(&[self.ops.a, quoted_body, arg_list])
    }

    fn gen_scope(&mut self, parent_scope_id: ScopeId, scope_id: ScopeId, hir_id: HirId) -> NodePtr {
        let body = self.gen_hir(scope_id, hir_id);
        let quoted_body = self.quote(body);
        let rest = self.allocator.one();

        let mut args = Vec::new();

        for symbol_id in self.environments[&scope_id].clone() {
            assert!(self.db.symbol(symbol_id).is_definition());
            args.push(self.gen_definition(parent_scope_id, symbol_id));
        }

        let environment = self.runtime_list(&args, rest);

        self.list(&[self.ops.a, quoted_body, environment])
    }

    fn gen_path(&mut self, scope_id: ScopeId, symbol_id: SymbolId) -> NodePtr {
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

        self.allocator
            .new_small_number(path)
            .expect("could not allocate path")
    }

    fn gen_definition(&mut self, scope_id: ScopeId, symbol_id: SymbolId) -> NodePtr {
        match self.db.symbol(symbol_id).clone() {
            Symbol::Function {
                scope_id: function_scope_id,
                hir_id,
                ..
            } => {
                let body = self.gen_hir(function_scope_id, hir_id);

                let mut definitions = Vec::new();

                for symbol_id in self.db.scope(function_scope_id).local_symbols() {
                    if self.db.symbol(symbol_id).is_definition() {
                        definitions.push(self.gen_definition(function_scope_id, symbol_id));
                    }
                }

                let mut definition = self.quote(body);

                if !definitions.is_empty() {
                    let rest = self.allocator.one();

                    let arg_list = self.runtime_list(&definitions, rest);

                    definition = self.list(&[self.ops.a, definition, arg_list]);
                    definition = self.quote(definition);
                }

                definition
            }
            Symbol::Parameter { .. } => {
                unreachable!();
            }
            Symbol::LetBinding { hir_id, .. } => self.gen_hir(scope_id, hir_id),
            Symbol::ConstBinding { .. } => unreachable!(),
        }
    }

    fn gen_hir(&mut self, scope_id: ScopeId, hir_id: HirId) -> NodePtr {
        match self.db.hir(hir_id) {
            Hir::Unknown => unreachable!(),
            Hir::Atom(atom) => self.gen_atom(atom.clone()),
            Hir::List(list) => self.gen_list(scope_id, list.clone()),
            Hir::ListIndex { value, index } => self.gen_list_index(scope_id, *value, index.clone()),
            Hir::Reference(symbol_id) => self.gen_reference(scope_id, *symbol_id),
            Hir::Scope {
                scope_id: new_scope_id,
                value,
            } => self.gen_scope(scope_id, *new_scope_id, *value),
            Hir::FunctionCall { callee, args } => {
                self.gen_function_call(scope_id, *callee, args.clone())
            }
            Hir::BinaryOp { op, lhs, rhs } => {
                let handler = match op {
                    BinaryOp::Add => Self::gen_add,
                    BinaryOp::Subtract => Self::gen_subtract,
                    BinaryOp::Multiply => Self::gen_multiply,
                    BinaryOp::Divide => Self::gen_divide,
                    BinaryOp::Remainder => Self::gen_remainder,
                    BinaryOp::LessThan => Self::gen_lt,
                    BinaryOp::GreaterThan => Self::gen_gt,
                    BinaryOp::LessThanEquals => Self::gen_lteq,
                    BinaryOp::GreaterThanEquals => Self::gen_gteq,
                    BinaryOp::Equals => Self::gen_eq,
                    BinaryOp::NotEquals => Self::gen_neq,
                };
                handler(self, scope_id, *lhs, *rhs)
            }
            Hir::Not(value) => self.gen_not(scope_id, *value),
            Hir::If {
                condition,
                then_block,
                else_block,
            } => self.gen_if(scope_id, *condition, *then_block, *else_block),
        }
    }

    fn gen_list(&mut self, scope_id: ScopeId, items: Vec<HirId>) -> NodePtr {
        let mut args = Vec::new();
        for item in items {
            args.push(self.gen_hir(scope_id, item));
        }
        self.runtime_list(&args, NodePtr::NIL)
    }

    fn gen_list_index(&mut self, scope_id: ScopeId, hir_id: HirId, index: BigInt) -> NodePtr {
        let mut value = self.gen_hir(scope_id, hir_id);
        for _ in num_iter::range(BigInt::zero(), index) {
            value = self.list(&[self.ops.r, value]);
        }
        self.list(&[self.ops.f, value])
    }

    fn gen_reference(&mut self, scope_id: ScopeId, symbol_id: SymbolId) -> NodePtr {
        match self.db.symbol(symbol_id).clone() {
            Symbol::Function {
                scope_id: function_scope_id,
                ..
            } => {
                let body = self.gen_path(scope_id, symbol_id);

                let mut captures = Vec::new();

                for symbol_id in self.db.scope(function_scope_id).local_symbols() {
                    if self.db.symbol(symbol_id).is_definition() {
                        captures.push(self.gen_path(scope_id, symbol_id));
                    }
                }

                for symbol_id in self.captures[&function_scope_id].clone() {
                    captures.push(self.gen_path(scope_id, symbol_id));
                }

                self.gen_closure_wrapper(body, &captures)
            }
            Symbol::ConstBinding { hir_id, .. } => self.gen_hir(scope_id, hir_id),
            _ => self.gen_path(scope_id, symbol_id),
        }
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

    fn gen_function_call(
        &mut self,
        scope_id: ScopeId,
        callee: HirId,
        arg_values: Vec<HirId>,
    ) -> NodePtr {
        let mut args = Vec::new();

        let callee = if let Hir::Reference(symbol_id) = self.db.hir(callee).clone() {
            if let Symbol::Function {
                scope_id: callee_scope_id,
                ..
            } = self.db.symbol(symbol_id)
            {
                for symbol_id in self.captures[&callee_scope_id].clone() {
                    args.push(self.gen_path(scope_id, symbol_id));
                }
                self.gen_path(scope_id, symbol_id)
            } else {
                self.gen_hir(scope_id, callee)
            }
        } else {
            self.gen_hir(scope_id, callee)
        };

        for arg_value in arg_values {
            args.push(self.gen_hir(scope_id, arg_value));
        }
        let arg_list = self.runtime_list(&args, NodePtr::NIL);

        self.list(&[self.ops.a, callee, arg_list])
    }

    fn gen_add(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> NodePtr {
        let lhs = self.gen_hir(scope_id, lhs);
        let rhs = self.gen_hir(scope_id, rhs);
        self.list(&[self.ops.add, lhs, rhs])
    }

    fn gen_subtract(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> NodePtr {
        let lhs = self.gen_hir(scope_id, lhs);
        let rhs = self.gen_hir(scope_id, rhs);
        self.list(&[self.ops.sub, lhs, rhs])
    }

    fn gen_multiply(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> NodePtr {
        let lhs = self.gen_hir(scope_id, lhs);
        let rhs = self.gen_hir(scope_id, rhs);
        self.list(&[self.ops.mul, lhs, rhs])
    }

    fn gen_divide(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> NodePtr {
        let lhs = self.gen_hir(scope_id, lhs);
        let rhs = self.gen_hir(scope_id, rhs);
        self.list(&[self.ops.div, lhs, rhs])
    }

    fn gen_remainder(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> NodePtr {
        let lhs = self.gen_hir(scope_id, lhs);
        let rhs = self.gen_hir(scope_id, rhs);
        let divmod_list = self.list(&[self.ops.divmod, lhs, rhs]);
        self.list(&[self.ops.r, divmod_list])
    }

    fn gen_lt(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> NodePtr {
        self.gen_gt(scope_id, rhs, lhs)
    }

    fn gen_gt(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> NodePtr {
        let lhs = self.gen_hir(scope_id, lhs);
        let rhs = self.gen_hir(scope_id, rhs);
        self.list(&[self.ops.gt, lhs, rhs])
    }

    fn gen_lteq(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> NodePtr {
        let lhs = self.gen_hir(scope_id, lhs);
        let rhs = self.gen_hir(scope_id, rhs);
        let gt_list = self.list(&[self.ops.gt, lhs, rhs]);
        self.list(&[self.ops.not, gt_list])
    }

    fn gen_gteq(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> NodePtr {
        let lhs = self.gen_hir(scope_id, lhs);
        let rhs = self.gen_hir(scope_id, rhs);
        let operands = self.list(&[lhs, rhs]);
        let eq_list = self.allocator.new_pair(self.ops.eq, operands).unwrap();
        let gt_list = self.allocator.new_pair(self.ops.gt, operands).unwrap();
        self.list(&[self.ops.any, gt_list, eq_list])
    }

    fn gen_eq(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> NodePtr {
        let lhs = self.gen_hir(scope_id, lhs);
        let rhs = self.gen_hir(scope_id, rhs);
        self.list(&[self.ops.eq, lhs, rhs])
    }

    fn gen_neq(&mut self, scope_id: ScopeId, lhs: HirId, rhs: HirId) -> NodePtr {
        let lhs = self.gen_hir(scope_id, lhs);
        let rhs = self.gen_hir(scope_id, rhs);
        let eq_list = self.list(&[self.ops.eq, lhs, rhs]);
        self.list(&[self.ops.not, eq_list])
    }

    fn gen_not(&mut self, scope_id: ScopeId, value: HirId) -> NodePtr {
        let value = self.gen_hir(scope_id, value);
        self.list(&[self.ops.not, value])
    }

    fn gen_if(
        &mut self,
        scope_id: ScopeId,
        condition: HirId,
        then_block: HirId,
        else_block: HirId,
    ) -> NodePtr {
        let all_env = self.allocator.one();

        let condition = self.gen_hir(scope_id, condition);
        let then_block = self.gen_hir(scope_id, then_block);
        let else_block = self.gen_hir(scope_id, else_block);

        let then_block = self.quote(then_block);
        let else_block = self.quote(else_block);

        let conditional = self.list(&[self.ops.i, condition, then_block, else_block]);
        self.list(&[self.ops.a, conditional, all_env])
    }

    fn gen_atom(&mut self, value: Vec<u8>) -> NodePtr {
        let int_ptr = self.allocator.new_atom(&value).unwrap();
        self.quote(int_ptr)
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
