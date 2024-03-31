#![forbid(clippy::unwrap_used)]

use std::mem;

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
struct Environment {
    symbols: IndexSet<SymbolId>,
    external_references: IndexSet<SymbolId>,
}

impl Environment {
    fn from_scope(scope: &Scope) -> Self {
        let references = scope.referenced_symbols();
        let definitions = scope.local_definitions();

        let mut external_references = IndexSet::new();
        let mut parameters = IndexSet::new();

        for &symbol_id in references {
            if !definitions.contains(&symbol_id) {
                external_references.insert(symbol_id);
            } else {
                parameters.insert(symbol_id);
            }
        }
        Self {
            symbols: external_references
                .iter()
                .chain(parameters.iter())
                .copied()
                .collect(),
            external_references,
        }
    }
}

#[derive(Debug, Clone)]
enum Value {
    Nil,
    Int(BigInt),
    Closure(SymbolId),
    Environment {
        environment: Environment,
        value: Box<Value>,
    },
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

enum Symbol {
    Function { scope: Option<Scope>, value: Value },
    Parameter,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct SymbolId(Id<Symbol>);

pub struct Output {
    pub errors: Vec<String>,
    pub node_ptr: Option<NodePtr>,
}

struct Compiler<'a> {
    scopes: Vec<Scope>,
    symbols: Arena<Symbol>,
    errors: Vec<String>,
    allocator: &'a mut Allocator,
}

impl<'a> Compiler<'a> {
    fn new(allocator: &'a mut Allocator) -> Self {
        Self {
            scopes: vec![Scope::default()],
            symbols: Arena::new(),
            errors: Vec::new(),
            allocator,
        }
    }

    fn compile_root(mut self, root: Root) -> Output {
        self.scopes.push(Scope::default());

        let symbol_ids: Vec<SymbolId> = root
            .function_items()
            .into_iter()
            .map(|function| self.declare_function(function))
            .collect();

        for (i, function) in root.function_items().into_iter().enumerate() {
            self.compile_function(function, symbol_ids[i]);
        }

        let Some(main) = self.scope_mut().symbol("main") else {
            self.error("no main function".to_string());

            return Output {
                errors: self.errors,
                node_ptr: None,
            };
        };

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
            let Some(name) = param.name().map(|name| name.to_string()) else {
                self.error("expected parameter name".to_string());
                continue;
            };

            let symbol_id = SymbolId(self.symbols.alloc(Symbol::Parameter));
            scope.define_symbol(name.to_string(), symbol_id);
        }

        let symbol_id = SymbolId(self.symbols.alloc(Symbol::Function {
            scope: Some(scope),
            value: Value::Nil,
        }));

        if let Some(name) = function.name() {
            self.scope_mut().define_symbol(name.to_string(), symbol_id);
        }

        symbol_id
    }

    fn compile_function(&mut self, function: FunctionItem, symbol_id: SymbolId) {
        if let Some(body) = function.body() {
            let body_scope = match &mut self.symbols[symbol_id.0] {
                Symbol::Function { scope, .. } => {
                    mem::take(scope).expect("function is missing scope")
                }
                _ => unreachable!(),
            };
            self.scopes.push(body_scope);

            let body_value = self.compile_block(body);
            let body_scope = self.scopes.pop().expect("function not in scope stack");

            let environment = Value::Environment {
                environment: Environment::from_scope(&body_scope),
                value: Box::new(body_value),
            };

            match &mut self.symbols[symbol_id.0] {
                Symbol::Function { scope, value } => {
                    *scope = Some(body_scope);
                    *value = environment;
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
            .scopes
            .iter()
            .rev()
            .find_map(|scope| scope.symbol(name))
        else {
            self.error(format!("undefined symbol: {}", name));
            return Value::Nil;
        };

        self.scope_mut().reference_symbol(symbol_id);

        match &self.symbols[symbol_id.0] {
            Symbol::Function { .. } => Value::Closure(symbol_id),
            Symbol::Parameter => Value::Reference(symbol_id),
        }
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

        let callee = self.compile_literal_expr(callee);
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

    fn gen_main(&mut self, main: SymbolId) -> NodePtr {
        let Symbol::Function { scope, value } = &self.symbols[main.0] else {
            self.error("main is not a function".to_string());
            return NodePtr::NIL;
        };

        let env = Environment::from_scope(scope.as_ref().expect("main is missing scope"));

        let body = self.gen_value(&env, value.clone());
        let rest = self.allocator.one();
        let a = self
            .allocator
            .new_small_number(2)
            .expect("could not allocate `a`");

        let mut args = Vec::new();
        for &symbol_id in env.symbols.iter() {
            args.push(self.gen_symbol(&env, symbol_id));
        }

        let arg_list = self.runtime_list(&args, rest);

        self.list(&[a, body, arg_list])
    }

    fn gen_symbol(&mut self, env: &Environment, symbol_id: SymbolId) -> NodePtr {
        match &self.symbols[symbol_id.0] {
            Symbol::Function { value, .. } => self.gen_value(env, value.clone()),
            Symbol::Parameter => todo!(),
        }
    }

    fn gen_value(&mut self, env: &Environment, value: Value) -> NodePtr {
        match value {
            Value::Nil => self.allocator.nil(),
            Value::Int(int) => self.gen_int(int),
            Value::Reference(symbol) => {
                let index = env.symbols.get_index_of(&symbol).expect("symbol not found");
                let mut path = 2;
                for _ in 0..index {
                    path *= 2;
                    path += 1;
                }
                self.allocator
                    .new_small_number(path)
                    .expect("could not allocate path")
            }
            Value::FunctionCall {
                callee,
                args: arg_values,
            } => {
                let a = self
                    .allocator
                    .new_small_number(2)
                    .expect("could not allocate `a`");

                let callee = self.gen_value(env, *callee);

                let mut args = Vec::new();
                for arg_value in arg_values {
                    args.push(self.gen_value(env, arg_value));
                }
                let arg_list = self.runtime_list(&args, NodePtr::NIL);

                self.list(&[a, callee, arg_list])
            }
            Value::Environment { environment, value } => {
                let body = self.gen_value(&environment, *value);
                self.quote(body)
            }
            Value::Closure(symbol_id) => {
                let Symbol::Function { scope, value: _ } = &self.symbols[symbol_id.0] else {
                    self.error("closure is not a function".to_string());
                    return NodePtr::NIL;
                };
                let capture_env =
                    Environment::from_scope(scope.as_ref().expect("closure is missing scope"));

                let q = self.allocator.one();
                let one = q;
                let a = self
                    .allocator
                    .new_small_number(2)
                    .expect("could not allocate `a`");

                let body = self.gen_value(env, Value::Reference(symbol_id));

                let runtime_a = self.quote(a);
                let runtime_quoted_body = self.runtime_quote(body);

                let mut args = Vec::new();
                for &symbol_id in capture_env.external_references.iter() {
                    let path = self.gen_value(env, Value::Reference(symbol_id));
                    let runtime_quoted_arg = self.runtime_quote(path);
                    args.push(runtime_quoted_arg);
                }

                let quoted_one = self.quote(one);
                let runtime_args = self.runtime_runtime_list(&args, quoted_one);

                self.runtime_list(
                    &[runtime_a, runtime_quoted_body, runtime_args],
                    NodePtr::NIL,
                )
            }
            Value::Add(operands) => {
                let plus = self
                    .allocator
                    .new_small_number(16)
                    .expect("could not allocate `+`");

                let mut args = vec![plus];
                for operand in operands {
                    args.push(self.gen_value(env, operand));
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
                    args.push(self.gen_value(env, operand));
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
                    args.push(self.gen_value(env, operand));
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
                    args.push(self.gen_value(env, operand));
                }
                self.list(&args)
            }
            Value::LessThan(lhs, rhs) => {
                // (not (any (= A B) (> A B)))
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

                let lhs = self.gen_value(env, *lhs);
                let rhs = self.gen_value(env, *rhs);
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
                args.push(self.gen_value(env, *lhs));
                args.push(self.gen_value(env, *rhs));
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

                let condition = self.gen_value(env, *condition);
                let then_block = self.gen_value(env, *then_block);
                let else_block = self.gen_value(env, *else_block);

                let then_block = self.quote(then_block);
                let else_block = self.quote(else_block);

                let conditional = self.list(&[i, condition, then_block, else_block]);
                self.list(&[a, conditional, all_env])
            }
        }
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
        self.scopes.last_mut().expect("no scope found")
    }

    fn error(&mut self, message: String) {
        self.errors.push(message);
    }
}
