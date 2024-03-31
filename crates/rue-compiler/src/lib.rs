#![forbid(clippy::unwrap_used)]

use std::collections::HashMap;

use clvmr::{Allocator, NodePtr};
use id_arena::{Arena, Id};
use indexmap::IndexSet;
use num_bigint::BigInt;
use rue_parser::{
    BinaryExpr, Block, Expr, FunctionCall, FunctionItem, IfExpr, LiteralExpr, Root, SyntaxKind,
    SyntaxToken,
};
use scope::Scope;

mod scope;

pub fn compile(allocator: &mut Allocator, root: Root) -> Output {
    let compiler = Compiler::new(allocator);
    compiler.compile_root(root)
}

#[derive(Debug, Clone)]
enum Value {
    Nil,
    Int(BigInt),
    Function(SymbolId),
    Reference(SymbolId),
    FunctionCall {
        callee: Box<Value>,
        args: Vec<Value>,
    },
    Add(Vec<Value>),
    Subtract(Vec<Value>),
    Multiply(Vec<Value>),
    Divide(Vec<Value>),
    LessThan(Box<Value>, Box<Value>),
    GreaterThan(Box<Value>, Box<Value>),
    If {
        condition: Box<Value>,
        then_block: Box<Value>,
        else_block: Box<Value>,
    },
}

#[derive(Debug)]
enum Symbol {
    Function { scope_id: ScopeId, value: Value },
    Parameter,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct SymbolId(Id<Symbol>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ScopeId(Id<Scope>);

pub struct Output {
    pub errors: Vec<String>,
    pub node_ptr: Option<NodePtr>,
}

struct Compiler<'a> {
    scopes: Arena<Scope>,
    symbols: Arena<Symbol>,
    scope_stack: Vec<ScopeId>,
    captures: HashMap<ScopeId, IndexSet<SymbolId>>,
    errors: Vec<String>,
    allocator: &'a mut Allocator,
}

impl<'a> Compiler<'a> {
    fn new(allocator: &'a mut Allocator) -> Self {
        Self {
            scopes: Arena::new(),
            symbols: Arena::new(),
            scope_stack: Vec::new(),
            captures: HashMap::new(),
            errors: Vec::new(),
            allocator,
        }
    }

    fn compile_root(mut self, root: Root) -> Output {
        self.scope_stack
            .push(ScopeId(self.scopes.alloc(Scope::default())));

        let symbol_ids: Vec<SymbolId> = root
            .function_items()
            .into_iter()
            .map(|function| self.declare_function(function))
            .collect();

        for (i, function) in root.function_items().into_iter().enumerate() {
            self.compile_function(function, symbol_ids[i]);
        }

        let Some(main) = self.scope_mut().get_symbol("main") else {
            self.error("no main function".to_string());

            return Output {
                errors: self.errors,
                node_ptr: None,
            };
        };

        self.scope_mut().use_symbol(main);
        let node_ptr = self.gen_main(main);

        Output {
            errors: self.errors,
            node_ptr: Some(node_ptr),
        }
    }

    fn declare_function(&mut self, function: FunctionItem) -> SymbolId {
        let mut scope = Scope::default();

        for param in function
            .param_list()
            .map(|list| list.params())
            .unwrap_or_default()
        {
            let Some(name) = param.name() else {
                self.error("expected parameter name".to_string());
                continue;
            };

            let symbol_id = SymbolId(self.symbols.alloc(Symbol::Parameter));
            scope.define_symbol(name.to_string(), symbol_id);
        }

        let scope_id = ScopeId(self.scopes.alloc(scope));

        let symbol_id = SymbolId(self.symbols.alloc(Symbol::Function {
            scope_id,
            value: Value::Nil,
        }));

        if let Some(name) = function.name() {
            self.scope_mut().define_symbol(name.to_string(), symbol_id);
        }

        symbol_id
    }

    fn compile_function(&mut self, function: FunctionItem, symbol_id: SymbolId) {
        if let Some(body) = function.body() {
            let body_scope_id = match self.symbols[symbol_id.0] {
                Symbol::Function { scope_id, .. } => scope_id,
                _ => unreachable!(),
            };

            self.scope_stack.push(body_scope_id);
            let body_value = self.compile_block(body);
            self.scope_stack.pop().expect("function not in scope stack");

            match &mut self.symbols[symbol_id.0] {
                Symbol::Function { value, .. } => {
                    *value = body_value;
                }
                _ => unreachable!(),
            };
        }
    }

    fn compile_block(&mut self, block: Block) -> Value {
        let Some(expr) = block.expr() else {
            self.error("expected expr".to_string());
            return Value::Nil;
        };
        self.compile_expr(expr)
    }

    fn compile_expr(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::LiteralExpr(literal) => self.compile_literal_expr(literal),
            Expr::BinaryExpr(binary) => self.compile_binary_expr(binary),
            Expr::IfExpr(if_expr) => self.compile_if_expr(if_expr),
            Expr::FunctionCall(call) => self.compile_function_call(call),
        }
    }

    fn compile_binary_expr(&mut self, binary: BinaryExpr) -> Value {
        let Some(lhs) = binary.lhs() else {
            self.error("expected lhs".to_string());
            return Value::Nil;
        };

        let Some(rhs) = binary.rhs() else {
            self.error("expected rhs".to_string());
            return Value::Nil;
        };

        let Some(op) = binary.op() else {
            self.error("expected op".to_string());
            return Value::Nil;
        };

        let lhs = self.compile_expr(lhs);
        let rhs = self.compile_expr(rhs);

        match op.kind() {
            SyntaxKind::Plus => Value::Add(vec![lhs, rhs]),
            SyntaxKind::Minus => Value::Subtract(vec![lhs, rhs]),
            SyntaxKind::Star => Value::Multiply(vec![lhs, rhs]),
            SyntaxKind::Slash => Value::Divide(vec![lhs, rhs]),
            SyntaxKind::LessThan => Value::LessThan(Box::new(lhs), Box::new(rhs)),
            SyntaxKind::GreaterThan => Value::GreaterThan(Box::new(lhs), Box::new(rhs)),
            _ => {
                self.error(format!("unexpected binary operator `{}`", op.text()));
                Value::Nil
            }
        }
    }

    fn compile_literal_expr(&mut self, literal: LiteralExpr) -> Value {
        let Some(value) = literal.value() else {
            self.error("expected value".to_string());
            return Value::Nil;
        };

        match value.kind() {
            SyntaxKind::Int => self.compile_int(value),
            SyntaxKind::Ident => self.compile_ident(value),
            _ => {
                self.error(format!("unexpected literal: {:?}", value));
                Value::Nil
            }
        }
    }

    fn compile_if_expr(&mut self, if_expr: IfExpr) -> Value {
        let Some(condition) = if_expr.condition() else {
            self.error("expected condition".to_string());
            return Value::Nil;
        };

        let Some(then_block) = if_expr.then_block() else {
            self.error("expected then block".to_string());
            return Value::Nil;
        };

        let Some(else_block) = if_expr.else_block() else {
            self.error("expected else block".to_string());
            return Value::Nil;
        };

        let condition = self.compile_expr(condition);
        let then_block = self.compile_block(then_block);
        let else_block = self.compile_block(else_block);

        Value::If {
            condition: Box::new(condition),
            then_block: Box::new(then_block),
            else_block: Box::new(else_block),
        }
    }

    fn compile_int(&self, int: SyntaxToken) -> Value {
        Value::Int(int.text().parse().expect("failed to parse into BigInt"))
    }

    fn compile_ident(&mut self, ident: SyntaxToken) -> Value {
        let name = ident.text();

        let Some(symbol_id) = self
            .scope_stack
            .iter()
            .rev()
            .find_map(|scope_id| self.scopes[scope_id.0].get_symbol(name))
        else {
            self.error(format!("undefined symbol: {}", name));
            return Value::Nil;
        };

        self.scope_mut().use_symbol(symbol_id);

        Value::Reference(symbol_id)
    }

    fn compile_function_call(&mut self, call: FunctionCall) -> Value {
        let Some(callee) = call.callee() else {
            self.error("expected callee".to_string());
            return Value::Nil;
        };

        let Some(args) = call.args() else {
            self.error("expected args".to_string());
            return Value::Nil;
        };

        let callee = self.compile_expr(callee);
        let args = args
            .exprs()
            .into_iter()
            .map(|arg| self.compile_expr(arg))
            .collect();

        Value::FunctionCall {
            callee: Box::new(callee),
            args,
        }
    }

    fn compute_captures(&mut self, scope_id: ScopeId) {
        if self.captures.contains_key(&scope_id) {
            return;
        }

        self.captures.insert(scope_id, IndexSet::new());

        for used_id in self.scopes[scope_id.0].used_symbols().clone() {
            if !self.scopes[scope_id.0].definitions().contains(&used_id) {
                self.captures
                    .get_mut(&scope_id)
                    .expect("cannot capture from unknown scope")
                    .insert(used_id);
            }

            let Symbol::Function {
                scope_id: function_scope_id,
                ..
            } = &self.symbols[used_id.0]
            else {
                continue;
            };

            let function_scope_id = *function_scope_id;

            self.compute_captures(function_scope_id);

            if !self.scopes[scope_id.0].definitions().contains(&used_id) {
                let new_captures = self.captures[&function_scope_id].clone();
                self.captures
                    .get_mut(&scope_id)
                    .expect("cannot capture from unknown scope")
                    .extend(new_captures);
            }
        }
    }

    fn gen_main(&mut self, main: SymbolId) -> NodePtr {
        let (scope_id, value) = {
            let Symbol::Function {
                scope_id, value, ..
            } = &self.symbols[main.0]
            else {
                self.error("main is not a function".to_string());
                return NodePtr::NIL;
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
        match &self.symbols[symbol_id.0] {
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
                } = self.symbols[symbol_id.0]
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
                    } = self.symbols[symbol_id.0]
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
                } = &self.symbols[symbol_id.0]
                else {
                    self.error("expected function".to_string());
                    return NodePtr::NIL;
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
            .chain(self.scopes[scope_id.0].definitions().iter())
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

    fn scope_mut(&mut self) -> &mut Scope {
        &mut self.scopes[self.scope_stack.last().expect("no scope found").0]
    }

    fn error(&mut self, message: String) {
        self.errors.push(message);
    }
}
