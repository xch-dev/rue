use std::collections::HashSet;

use indexmap::IndexSet;
use num_bigint::BigInt;
use rowan::TextRange;
use rue_parser::{
    AstNode, BinaryExpr, BinaryOp, Block, Expr, FunctionCall, FunctionItem, FunctionType, IfExpr,
    Item, LambdaExpr, ListExpr, ListType, LiteralExpr, LiteralType, PrefixOp, Root, SyntaxKind,
    SyntaxToken, TypeAliasItem,
};

use crate::{
    database::{Database, ScopeId, SymbolId, TypeId},
    scope::Scope,
    symbol::Symbol,
    ty::{Type, Typed},
    value::Value,
    Diagnostic, DiagnosticInfo, DiagnosticKind,
};

pub struct LowerOutput {
    pub diagnostics: Vec<Diagnostic>,
    pub main_scope_id: ScopeId,
}

pub fn lower(db: &mut Database, root: Root) -> LowerOutput {
    Lowerer::new(db).compile_root(root)
}

struct Lowerer<'a> {
    db: &'a mut Database,
    scope_stack: Vec<ScopeId>,
    diagnostics: Vec<Diagnostic>,
}

impl<'a> Lowerer<'a> {
    pub fn new(db: &'a mut Database) -> Self {
        Self {
            db,
            scope_stack: Vec::new(),
            diagnostics: Vec::new(),
        }
    }

    pub fn compile_root(mut self, root: Root) -> LowerOutput {
        let scope_id = self.db.alloc_scope(Scope::default());
        self.scope_stack.push(scope_id);

        self.compile_items(root.items());

        LowerOutput {
            diagnostics: self.diagnostics,
            main_scope_id: scope_id,
        }
    }

    fn compile_items(&mut self, items: Vec<Item>) {
        let function_items: Vec<FunctionItem> = items
            .clone()
            .into_iter()
            .filter_map(|item| match item {
                Item::FunctionItem(function) => Some(function),
                _ => None,
            })
            .collect();
        let type_alias_items: Vec<TypeAliasItem> = items
            .clone()
            .into_iter()
            .filter_map(|item| match item {
                Item::TypeAliasItem(ty) => Some(ty),
                _ => None,
            })
            .collect();

        let type_ids: Vec<TypeId> = type_alias_items
            .iter()
            .cloned()
            .map(|item| self.declare_type_alias(item))
            .collect();
        let symbol_ids: Vec<SymbolId> = function_items
            .iter()
            .cloned()
            .map(|item| self.declare_function(item))
            .collect();

        for (i, type_alias) in type_alias_items.into_iter().enumerate() {
            self.compile_type_alias(type_alias, type_ids[i]);
        }

        for (i, function) in function_items.into_iter().enumerate() {
            self.compile_function(function, symbol_ids[i]);
        }
    }

    fn declare_function(&mut self, function: FunctionItem) -> SymbolId {
        let mut scope = Scope::default();

        let ret_ty = function
            .return_ty()
            .map(|ty| self.compile_ty(ty))
            .unwrap_or_else(|| self.db.alloc_type(Type::Unknown));

        let mut param_types = Vec::new();

        for param in function
            .param_list()
            .map(|list| list.params())
            .unwrap_or_default()
        {
            let Some(name) = param.name() else {
                continue;
            };

            let ty = param
                .ty()
                .map(|ty| self.compile_ty(ty))
                .unwrap_or_else(|| self.db.alloc_type(Type::Unknown));

            param_types.push(ty);

            let symbol_id = self.db.alloc_symbol(Symbol::Parameter { ty });
            scope.define_symbol(name.to_string(), symbol_id);
        }

        let scope_id = self.db.alloc_scope(scope);

        let symbol_id = self.db.alloc_symbol(Symbol::Function {
            scope_id,
            value: Value::Unknown,
            ret_type: ret_ty,
            param_types,
        });

        if let Some(name) = function.name() {
            self.scope_mut().define_symbol(name.to_string(), symbol_id);
        }

        symbol_id
    }

    fn declare_type_alias(&mut self, type_alias: TypeAliasItem) -> TypeId {
        let type_id = self.db.alloc_type(Type::Unknown);

        if let Some(name) = type_alias.name() {
            self.scope_mut()
                .define_type_alias(name.to_string(), type_id);
        }

        type_id
    }

    fn compile_function(&mut self, function: FunctionItem, symbol_id: SymbolId) {
        if let Some(body) = function.body() {
            let (body_scope_id, expected_ret_type) = match self.db.symbol(symbol_id) {
                Symbol::Function {
                    scope_id, ret_type, ..
                } => (*scope_id, *ret_type),
                _ => unreachable!(),
            };

            self.scope_stack.push(body_scope_id);
            let ret = self.compile_block_expr(body, None, Some(expected_ret_type));
            self.scope_stack.pop().expect("function not in scope stack");

            if !self.is_assignable_to(self.db.ty(ret.ty), self.db.ty(expected_ret_type)) {
                self.error(
                    DiagnosticInfo::TypeMismatch {
                        expected: self.type_name(expected_ret_type),
                        found: self.type_name(ret.ty),
                    },
                    function.syntax().text_range(),
                );
            }

            match &mut self.db.symbol_mut(symbol_id) {
                Symbol::Function { value, .. } => {
                    *value = ret.value;
                }
                _ => unreachable!(),
            };
        }
    }

    fn compile_type_alias(&mut self, ty: TypeAliasItem, type_id: TypeId) {
        let text_range = ty.syntax().text_range();

        let ty = ty
            .ty()
            .map(|ty| self.compile_ty(ty))
            .unwrap_or_else(|| self.db.alloc_type(Type::Unknown));

        *self.db.ty_mut(type_id) = Type::Alias(ty);

        self.detect_cycle(type_id, text_range);
    }

    fn compile_block_expr(
        &mut self,
        block: Block,
        scope_id: Option<ScopeId>,
        expected_type: Option<TypeId>,
    ) -> Typed {
        let Some(expr) = block.expr() else {
            return Typed {
                value: Value::Unknown,
                ty: self.db.alloc_type(Type::Unknown),
            };
        };

        if let Some(scope_id) = scope_id {
            self.scope_stack.push(scope_id);
        }

        self.compile_items(block.items());

        let mut let_scope_ids = Vec::new();

        for let_stmt in block.let_stmts() {
            let expected_type = let_stmt.ty().map(|ty| self.compile_ty(ty));

            let Some(expr) = let_stmt.expr() else {
                continue;
            };

            let value = self.compile_expr(expr, expected_type);

            let Some(name) = let_stmt.name() else {
                continue;
            };

            let symbol_id = self.db.alloc_symbol(Symbol::Binding {
                ty: value.ty,
                value: value.value,
            });

            let mut let_scope = Scope::default();
            let_scope.define_symbol(name.to_string(), symbol_id);
            let scope_id = self.db.alloc_scope(let_scope);
            self.scope_stack.push(scope_id);

            let_scope_ids.push(scope_id);
        }

        let mut body = self.compile_expr(expr, expected_type);

        for scope_id in let_scope_ids.into_iter().rev() {
            body = Typed {
                value: Value::Scope {
                    scope_id,
                    value: Box::new(body.value),
                },
                ty: body.ty,
            };
            self.scope_stack.pop().expect("let not in scope stack");
        }

        if scope_id.is_some() {
            self.scope_stack.pop().expect("block not in scope stack");
        }

        Typed {
            value: match scope_id {
                Some(scope_id) => Value::Scope {
                    scope_id,
                    value: Box::new(body.value),
                },
                None => body.value,
            },
            ty: body.ty,
        }
    }

    fn compile_expr(&mut self, expr: Expr, expected_type: Option<TypeId>) -> Typed {
        match expr {
            Expr::LiteralExpr(literal) => self.compile_literal_expr(literal),
            Expr::ListExpr(list) => self.compile_list_expr(list, expected_type),
            Expr::Block(block) => {
                let scope_id = self.db.alloc_scope(Scope::default());
                self.compile_block_expr(block, Some(scope_id), expected_type)
            }
            Expr::LambdaExpr(lambda) => self.compile_lambda_expr(lambda, expected_type),
            Expr::PrefixExpr(prefix) => self.compile_prefix_expr(prefix),
            Expr::BinaryExpr(binary) => self.compile_binary_expr(binary),
            Expr::IfExpr(if_expr) => self.compile_if_expr(if_expr, expected_type),
            Expr::FunctionCall(call) => self.compile_function_call(call),
        }
    }

    fn compile_prefix_expr(&mut self, prefix_expr: rue_parser::PrefixExpr) -> Typed {
        let Some(expr) = prefix_expr.expr() else {
            return Typed {
                value: Value::Unknown,
                ty: self.db.alloc_type(Type::Unknown),
            };
        };

        let expr = self.compile_expr(expr, None);

        if !self.is_assignable_to(self.db.ty(expr.ty), &Type::Bool) {
            self.error(
                DiagnosticInfo::TypeMismatch {
                    expected: "Bool".to_string(),
                    found: self.type_name(expr.ty),
                },
                prefix_expr.syntax().text_range(),
            );
        }

        Typed {
            value: match prefix_expr.op() {
                Some(PrefixOp::Not) => Value::Not(Box::new(expr.value)),
                _ => Value::Unknown,
            },
            ty: self.db.alloc_type(Type::Bool),
        }
    }

    fn compile_binary_expr(&mut self, binary: BinaryExpr) -> Typed {
        let lhs = binary.lhs().map(|lhs| self.compile_expr(lhs, None));
        let rhs = binary.rhs().map(|rhs| self.compile_expr(rhs, None));

        if let Some(Typed { ty, .. }) = &lhs {
            if !self.is_assignable_to(self.db.ty(*ty), &Type::Int) {
                self.error(
                    DiagnosticInfo::TypeMismatch {
                        expected: "Int".to_string(),
                        found: self.type_name(*ty),
                    },
                    binary.lhs().unwrap().syntax().text_range(),
                );
            }
        }

        if let Some(Typed { ty, .. }) = &rhs {
            if !self.is_assignable_to(self.db.ty(*ty), &Type::Int) {
                self.error(
                    DiagnosticInfo::TypeMismatch {
                        expected: "Int".to_string(),
                        found: self.type_name(*ty),
                    },
                    binary.rhs().unwrap().syntax().text_range(),
                );
            }
        }

        let ty = binary
            .op()
            .map(|op| match op {
                BinaryOp::Add => Type::Int,
                BinaryOp::Subtract => Type::Int,
                BinaryOp::Multiply => Type::Int,
                BinaryOp::Divide => Type::Int,
                BinaryOp::Remainder => Type::Int,
                BinaryOp::LessThan => Type::Bool,
                BinaryOp::GreaterThan => Type::Bool,
                BinaryOp::LessThanEquals => Type::Bool,
                BinaryOp::GreaterThanEquals => Type::Bool,
                BinaryOp::Equals => Type::Bool,
                BinaryOp::NotEquals => Type::Bool,
            })
            .unwrap_or(Type::Unknown);

        match (lhs, rhs, binary.op()) {
            (Some(Typed { value: lhs, .. }), Some(Typed { value: rhs, .. }), Some(op)) => Typed {
                value: Value::BinaryOp {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                ty: self.db.alloc_type(ty),
            },
            _ => Typed {
                value: Value::Unknown,
                ty: self.db.alloc_type(ty),
            },
        }
    }

    fn compile_literal_expr(&mut self, literal: LiteralExpr) -> Typed {
        let Some(value) = literal.value() else {
            return Typed {
                value: Value::Unknown,
                ty: self.db.alloc_type(Type::Unknown),
            };
        };

        match value.kind() {
            SyntaxKind::Int => self.compile_int(value),
            SyntaxKind::Ident => self.compile_ident(value),
            SyntaxKind::String => self.compile_string(value),
            SyntaxKind::True => Typed {
                value: Value::Atom(vec![1]),
                ty: self.db.alloc_type(Type::Bool),
            },
            SyntaxKind::False => Typed {
                value: Value::Atom(Vec::new()),
                ty: self.db.alloc_type(Type::Bool),
            },
            SyntaxKind::Nil => Typed {
                value: Value::Atom(Vec::new()),
                ty: self.db.alloc_type(Type::Nil),
            },
            _ => unreachable!(),
        }
    }

    fn compile_list_expr(&mut self, list_expr: ListExpr, expected_type: Option<TypeId>) -> Typed {
        let ast_items = list_expr.items();

        let expected_first_type = expected_type.and_then(|ty| self.extract_list_item_type(ty));

        let first_item = ast_items
            .first()
            .map(|first| self.compile_expr(first.clone(), expected_first_type));

        let expected_item_type = first_item.as_ref().map(|item| item.ty);
        let item_type = expected_item_type.unwrap_or_else(|| self.db.alloc_type(Type::Unknown));

        let mut items = first_item.map(|item| vec![item.value]).unwrap_or_default();

        for ast_item in ast_items.into_iter().skip(1) {
            let item = self.compile_expr(ast_item.clone(), expected_item_type);

            if !self.is_assignable_to(self.db.ty(item.ty), self.db.ty(item_type)) {
                self.error(
                    DiagnosticInfo::TypeMismatch {
                        expected: self.type_name(expected_item_type.unwrap()),
                        found: self.type_name(item.ty),
                    },
                    ast_item.syntax().text_range(),
                );
            }

            items.push(item.value);
        }

        Typed {
            value: Value::List(items),
            ty: self.db.alloc_type(Type::List(item_type)),
        }
    }

    fn compile_lambda_expr(
        &mut self,
        lambda_expr: LambdaExpr,
        expected_type: Option<TypeId>,
    ) -> Typed {
        let expected =
            expected_type.and_then(|expected| self.extract_function_type_components(expected));

        let mut scope = Scope::default();
        let mut param_types = Vec::new();

        for (i, param) in lambda_expr
            .param_list()
            .map(|list| list.params())
            .unwrap_or_default()
            .into_iter()
            .enumerate()
        {
            let ty = param
                .ty()
                .map(|ty| self.compile_ty(ty))
                .or(expected
                    .as_ref()
                    .and_then(|expected| expected.0.get(i).copied()))
                .unwrap_or_else(|| self.db.alloc_type(Type::Unknown));

            param_types.push(ty);

            if let Some(name) = param.name() {
                let symbol_id = self.db.alloc_symbol(Symbol::Parameter { ty });
                scope.define_symbol(name.to_string(), symbol_id);
            };
        }

        let scope_id = self.db.alloc_scope(scope);

        let Some(body) = lambda_expr.body() else {
            return Typed {
                value: Value::Unknown,
                ty: self.db.alloc_type(Type::Unknown),
            };
        };

        let expected_ret_type = lambda_expr
            .ty()
            .map(|ty| self.compile_ty(ty))
            .or(expected.map(|expected| expected.1));

        self.scope_stack.push(scope_id);
        let body = self.compile_expr(body, expected_ret_type);
        self.scope_stack.pop().expect("lambda not in scope stack");

        let ret_type = expected_ret_type.unwrap_or(body.ty);

        if !self.is_assignable_to(self.db.ty(body.ty), self.db.ty(ret_type)) {
            self.error(
                DiagnosticInfo::TypeMismatch {
                    expected: self.type_name(ret_type),
                    found: self.type_name(body.ty),
                },
                lambda_expr.body().unwrap().syntax().text_range(),
            );
        }

        let symbol_id = self.db.alloc_symbol(Symbol::Function {
            scope_id,
            value: body.value,
            param_types: param_types.clone(),
            ret_type,
        });

        Typed {
            value: Value::Reference(symbol_id),
            ty: self.db.alloc_type(Type::Function {
                params: param_types,
                ret: ret_type,
            }),
        }
    }

    fn compile_if_expr(&mut self, if_expr: IfExpr, expected_type: Option<TypeId>) -> Typed {
        let condition = if_expr
            .condition()
            .map(|condition| self.compile_expr(condition, None));
        let then_block = if_expr.then_block().map(|then_block| {
            let scope_id = self.db.alloc_scope(Scope::default());
            self.compile_block_expr(then_block, Some(scope_id), expected_type)
        });
        let else_block = if_expr.else_block().map(|else_block| {
            let scope_id = self.db.alloc_scope(Scope::default());
            self.compile_block_expr(else_block, Some(scope_id), expected_type)
        });

        if let Some(condition_type) = condition.as_ref().map(|condition| condition.ty) {
            if !self.is_assignable_to(self.db.ty(condition_type), &Type::Bool) {
                self.error(
                    DiagnosticInfo::TypeMismatch {
                        expected: "Bool".to_string(),
                        found: self.type_name(condition_type),
                    },
                    if_expr.condition().unwrap().syntax().text_range(),
                );
            }
        }

        if let (Some(Typed { ty: then_ty, .. }), Some(Typed { ty: else_ty, .. })) =
            (&then_block, &else_block)
        {
            if !self.is_assignable_to(self.db.ty(*then_ty), self.db.ty(*else_ty)) {
                self.error(
                    DiagnosticInfo::TypeMismatch {
                        expected: self.type_name(*then_ty),
                        found: self.type_name(*else_ty),
                    },
                    if_expr.else_block().unwrap().syntax().text_range(),
                );
            }
        }

        let ty = then_block
            .as_ref()
            .or(else_block.as_ref())
            .map(|block| block.ty)
            .unwrap_or_else(|| self.db.alloc_type(Type::Unknown));

        let value = condition.and_then(|condition| {
            then_block.and_then(|then_block| {
                else_block.map(|else_block| Value::If {
                    condition: Box::new(condition.value),
                    then_block: Box::new(then_block.value),
                    else_block: Box::new(else_block.value),
                })
            })
        });

        Typed {
            value: value.unwrap_or(Value::Unknown),
            ty,
        }
    }

    fn compile_int(&mut self, int: SyntaxToken) -> Typed {
        let num = int
            .text()
            .replace('_', "")
            .parse()
            .expect("failed to parse into BigInt");

        Typed {
            value: Value::Atom(bigint_to_bytes(num)),
            ty: self.db.alloc_type(Type::Int),
        }
    }

    fn compile_string(&mut self, string: SyntaxToken) -> Typed {
        let text = string.text();
        let quote = text.chars().next().unwrap();
        let after_prefix = &text[1..];
        let before_suffix = after_prefix.strip_suffix(quote).unwrap_or(after_prefix);

        Typed {
            value: Value::Atom(before_suffix.as_bytes().to_vec()),
            ty: self.db.alloc_type(Type::Bytes),
        }
    }

    fn compile_ident(&mut self, ident: SyntaxToken) -> Typed {
        let Some(symbol_id) = self
            .scope_stack
            .iter()
            .rev()
            .find_map(|&scope_id| self.db.scope(scope_id).symbol(ident.text()))
        else {
            self.error(
                DiagnosticInfo::UndefinedReference(ident.to_string()),
                ident.text_range(),
            );

            return Typed {
                value: Value::Unknown,
                ty: self.db.alloc_type(Type::Unknown),
            };
        };

        Typed {
            value: Value::Reference(symbol_id),
            ty: match self.db.symbol(symbol_id) {
                Symbol::Function {
                    param_types,
                    ret_type,
                    ..
                } => self.db.alloc_type(Type::Function {
                    params: param_types.clone(),
                    ret: *ret_type,
                }),
                Symbol::Parameter { ty } => *ty,
                Symbol::Binding { ty, .. } => *ty,
            },
        }
    }

    fn compile_function_call(&mut self, call: FunctionCall) -> Typed {
        let Some(callee) = call.callee() else {
            return Typed {
                value: Value::Unknown,
                ty: self.db.alloc_type(Type::Unknown),
            };
        };

        let Some(args) = call.args() else {
            return Typed {
                value: Value::Unknown,
                ty: self.db.alloc_type(Type::Unknown),
            };
        };

        let callee = self.compile_expr(callee, None);

        let expected = self.extract_function_type_components(callee.ty);

        let raw_args = args.exprs();

        let args: Vec<Typed> = args
            .exprs()
            .into_iter()
            .enumerate()
            .map(|(i, arg)| {
                self.compile_expr(
                    arg,
                    expected
                        .as_ref()
                        .and_then(|expected| expected.0.get(i).copied()),
                )
            })
            .collect();

        let arg_types: Vec<TypeId> = args.iter().map(|arg| arg.ty).collect();
        let arg_values: Vec<Value> = args.iter().map(|arg| arg.value.clone()).collect();

        match self.db.ty(callee.ty).clone() {
            Type::Function { params, ret } => {
                if params.len() != arg_types.len() {
                    self.error(
                        DiagnosticInfo::ArgumentMismatch {
                            expected: params.len(),
                            found: arg_types.len(),
                        },
                        call.args().expect("no arguments").syntax().text_range(),
                    );

                    return Typed {
                        value: Value::Unknown,
                        ty: self.db.alloc_type(Type::Unknown),
                    };
                }

                for (i, (param, arg)) in
                    params.clone().into_iter().zip(arg_types.iter()).enumerate()
                {
                    let error = if !self.is_assignable_to(self.db.ty(param), self.db.ty(*arg)) {
                        Some((
                            DiagnosticInfo::TypeMismatch {
                                expected: self.type_name(param),
                                found: self.type_name(*arg),
                            },
                            raw_args[i].syntax().text_range(),
                        ))
                    } else {
                        None
                    };

                    if let Some((error, range)) = error {
                        self.error(error, range);
                    }
                }

                Typed {
                    value: Value::FunctionCall {
                        callee: Box::new(callee.value),
                        args: arg_values,
                    },
                    ty: ret,
                }
            }
            Type::Unknown => Typed {
                value: Value::FunctionCall {
                    callee: Box::new(callee.value),
                    args: arg_values,
                },
                ty: self.db.alloc_type(Type::Unknown),
            },
            _ => {
                self.error(
                    DiagnosticInfo::UncallableType(self.type_name(callee.ty)),
                    call.callee().expect("no callee").syntax().text_range(),
                );
                Typed {
                    value: Value::Unknown,
                    ty: self.db.alloc_type(Type::Unknown),
                }
            }
        }
    }

    fn compile_ty(&mut self, ty: rue_parser::Type) -> TypeId {
        match ty {
            rue_parser::Type::LiteralType(literal) => self.compile_literal_ty(literal),
            rue_parser::Type::ListType(list) => self.compile_list_ty(list),
            rue_parser::Type::FunctionType(function) => self.compile_function_ty(function),
        }
    }

    fn compile_literal_ty(&mut self, literal: LiteralType) -> TypeId {
        let Some(value) = literal.value() else {
            return self.db.alloc_type(Type::Unknown);
        };

        for &scope_id in self.scope_stack.iter().rev() {
            if let Some(ty) = self.db.scope(scope_id).type_alias(value.text()) {
                return ty;
            }
        }

        match value.text() {
            "Int" => self.db.alloc_type(Type::Int),
            "Bool" => self.db.alloc_type(Type::Bool),
            "Bytes" => self.db.alloc_type(Type::Bytes),
            _ => {
                self.error(
                    DiagnosticInfo::UndefinedType(value.to_string()),
                    literal.syntax().text_range(),
                );
                self.db.alloc_type(Type::Unknown)
            }
        }
    }

    fn compile_list_ty(&mut self, list: ListType) -> TypeId {
        let Some(inner) = list.ty() else {
            return self.db.alloc_type(Type::Unknown);
        };

        let inner = self.compile_ty(inner);
        self.db.alloc_type(Type::List(inner))
    }

    fn compile_function_ty(&mut self, function: FunctionType) -> TypeId {
        let params = function
            .params()
            .map(|params| params.types())
            .unwrap_or_default()
            .into_iter()
            .map(|ty| self.compile_ty(ty))
            .collect();

        let ret = function
            .ret()
            .map(|ty| self.compile_ty(ty))
            .unwrap_or_else(|| self.db.alloc_type(Type::Unknown));

        self.db.alloc_type(Type::Function { params, ret })
    }

    fn extract_function_type_components(&self, type_id: TypeId) -> Option<(Vec<TypeId>, TypeId)> {
        match self.db.ty(type_id) {
            Type::Function { params, ret } => Some((params.clone(), *ret)),
            _ => None,
        }
    }

    fn extract_list_item_type(&self, type_id: TypeId) -> Option<TypeId> {
        match self.db.ty(type_id) {
            Type::List(inner) => Some(*inner),
            _ => None,
        }
    }

    fn detect_cycle(&mut self, ty: TypeId, text_range: TextRange) {
        self.detect_cycle_visitor(ty, text_range, &mut HashSet::new())
    }

    fn detect_cycle_visitor(
        &mut self,
        ty: TypeId,
        text_range: TextRange,
        visited_aliases: &mut HashSet<TypeId>,
    ) {
        match self.db.ty(ty).clone() {
            Type::Unknown => {}
            Type::Nil => {}
            Type::Int => {}
            Type::Bool => {}
            Type::Bytes => {}
            Type::List(inner) => {
                self.detect_cycle_visitor(inner, text_range, visited_aliases);
            }
            Type::Function { params, ret } => {
                for &param in params.iter() {
                    self.detect_cycle_visitor(param, text_range, visited_aliases);
                }

                self.detect_cycle_visitor(ret, text_range, visited_aliases);
            }
            Type::Alias(alias) => {
                if !visited_aliases.insert(alias) {
                    self.error(DiagnosticInfo::RecursiveTypeAlias, text_range);
                    return;
                }
                self.detect_cycle_visitor(alias, text_range, visited_aliases);
            }
        }
    }

    fn type_name(&self, ty: TypeId) -> String {
        self.type_name_visitor(ty, &mut IndexSet::new())
    }

    fn type_name_visitor(&self, ty: TypeId, visited_aliases: &mut IndexSet<TypeId>) -> String {
        for &scope_id in self.scope_stack.iter().rev() {
            if let Some(name) = self.db.scope(scope_id).type_name(ty) {
                return name.to_string();
            }
        }

        match self.db.ty(ty) {
            Type::Unknown => "{unknown}".to_string(),
            Type::Nil => "Nil".to_string(),
            Type::Int => "Int".to_string(),
            Type::Bool => "Bool".to_string(),
            Type::Bytes => "Bytes".to_string(),
            Type::List(inner) => format!("{}[]", self.type_name_visitor(*inner, visited_aliases)),
            Type::Function { params, ret } => {
                let params: Vec<String> = params
                    .iter()
                    .map(|&ty| self.type_name_visitor(ty, visited_aliases))
                    .collect();
                let ret = self.type_name_visitor(*ret, visited_aliases);
                format!("fun({}) -> {}", params.join(", "), ret)
            }
            Type::Alias(type_id) => {
                if let Some(index) = visited_aliases.get_index_of(type_id) {
                    format!("<{index}>")
                } else {
                    visited_aliases.insert(*type_id);
                    self.type_name_visitor(*type_id, visited_aliases)
                }
            }
        }
    }

    fn is_assignable_to(&self, a: &Type, b: &Type) -> bool {
        self.is_assignable_to_visitor(a, b, &mut HashSet::new())
    }

    fn is_assignable_to_visitor(
        &self,
        a: &Type,
        b: &Type,
        visited_aliases: &mut HashSet<TypeId>,
    ) -> bool {
        match (a, b) {
            (Type::Unknown, _) | (_, Type::Unknown) => false,
            (Type::Int, Type::Int) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Bytes, Type::Bytes) => true,
            (Type::Nil, Type::Nil) => true,
            (Type::Alias(alias), _) => {
                if !visited_aliases.insert(*alias) {
                    return true;
                }
                let ty = self.db.ty(*alias);
                self.is_assignable_to_visitor(ty, b, visited_aliases)
            }
            (_, Type::Alias(alias)) => {
                if !visited_aliases.insert(*alias) {
                    return true;
                }
                let ty = self.db.ty(*alias);
                self.is_assignable_to_visitor(a, ty, visited_aliases)
            }
            (Type::List(inner_a), Type::List(inner_b)) => self.is_assignable_to_visitor(
                self.db.ty(*inner_a),
                self.db.ty(*inner_b),
                visited_aliases,
            ),
            (
                Type::Function {
                    params: params_a,
                    ret: ret_a,
                },
                Type::Function {
                    params: params_b,
                    ret: ret_b,
                },
            ) => {
                if params_a.len() != params_b.len() {
                    return false;
                }

                for (&a, &b) in params_a.iter().zip(params_b.iter()) {
                    if !self.is_assignable_to_visitor(self.db.ty(a), self.db.ty(b), visited_aliases)
                    {
                        return false;
                    }
                }

                self.is_assignable_to_visitor(
                    self.db.ty(*ret_a),
                    self.db.ty(*ret_b),
                    visited_aliases,
                )
            }
            _ => false,
        }
    }

    fn scope_mut(&mut self) -> &mut Scope {
        self.db
            .scope_mut(self.scope_stack.last().copied().expect("no scope found"))
    }

    fn error(&mut self, info: DiagnosticInfo, range: TextRange) {
        self.diagnostics.push(Diagnostic::new(
            DiagnosticKind::Error,
            info,
            range.start().into()..range.end().into(),
        ));
    }

    fn _warning(&mut self, info: DiagnosticInfo, range: TextRange) {
        self.diagnostics.push(Diagnostic::new(
            DiagnosticKind::Warning,
            info,
            range.start().into()..range.end().into(),
        ));
    }
}

fn bigint_to_bytes(num: BigInt) -> Vec<u8> {
    let bytes: Vec<u8> = num.to_signed_bytes_be();
    let mut slice = bytes.as_slice();
    // make number minimal by removing leading zeros
    while (!slice.is_empty()) && (slice[0] == 0) {
        if slice.len() > 1 && (slice[1] & 0x80 == 0x80) {
            break;
        }
        slice = &slice[1..];
    }
    slice.to_vec()
}
