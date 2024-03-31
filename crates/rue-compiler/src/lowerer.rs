use rue_parser::{
    BinaryExpr, Block, Expr, FunctionCall, FunctionItem, IfExpr, LiteralExpr, Root, SyntaxKind,
    SyntaxToken,
};

use crate::{
    database::{Database, ScopeId, SymbolId},
    scope::Scope,
    symbol::Symbol,
    value::Value,
};

pub struct LowerOutput {
    pub db: Database,
    pub errors: Vec<String>,
    pub main_scope_id: ScopeId,
}

pub struct Lowerer {
    db: Database,
    scope_stack: Vec<ScopeId>,
    errors: Vec<String>,
}

impl Lowerer {
    pub fn new() -> Self {
        Self {
            db: Database::default(),
            scope_stack: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn compile_root(mut self, root: Root) -> LowerOutput {
        let scope_id = self.db.alloc_scope(Scope::default());
        self.scope_stack.push(scope_id);

        let symbol_ids: Vec<SymbolId> = root
            .function_items()
            .into_iter()
            .map(|function| self.declare_function(function))
            .collect();

        for (i, function) in root.function_items().into_iter().enumerate() {
            self.compile_function(function, symbol_ids[i]);
        }

        LowerOutput {
            db: self.db,
            errors: self.errors,
            main_scope_id: scope_id,
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

            let symbol_id = self.db.alloc_symbol(Symbol::Parameter);
            scope.define_symbol(name.to_string(), symbol_id);
        }

        let scope_id = self.db.alloc_scope(scope);

        let symbol_id = self.db.alloc_symbol(Symbol::Function {
            scope_id,
            value: Value::Nil,
        });

        if let Some(name) = function.name() {
            self.scope_mut().define_symbol(name.to_string(), symbol_id);
        }

        symbol_id
    }

    fn compile_function(&mut self, function: FunctionItem, symbol_id: SymbolId) {
        if let Some(body) = function.body() {
            let body_scope_id = match self.db.symbol(symbol_id) {
                Symbol::Function { scope_id, .. } => *scope_id,
                _ => unreachable!(),
            };

            self.scope_stack.push(body_scope_id);
            let body_value = self.compile_block(body);
            self.scope_stack.pop().expect("function not in scope stack");

            match &mut self.db.symbol_mut(symbol_id) {
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
            .find_map(|&scope_id| self.db.scope(scope_id).get_symbol(name))
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

    fn scope_mut(&mut self) -> &mut Scope {
        self.db
            .scope_mut(self.scope_stack.last().copied().expect("no scope found"))
    }

    fn error(&mut self, message: String) {
        self.errors.push(message);
    }
}
