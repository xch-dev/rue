use std::collections::{HashMap, HashSet};

use indexmap::{IndexMap, IndexSet};
use num_bigint::BigInt;
use rowan::TextRange;
use rue_parser::{
    AstNode, BinaryExpr, BinaryOp, Block, Expr, FieldAccess, FunctionCall, FunctionItem,
    FunctionType, IfExpr, IndexAccess, InitializerExpr, Item, LambdaExpr, ListExpr, ListType,
    LiteralExpr, PathExpr, PrefixExpr, PrefixOp, Root, StructItem, SyntaxKind, SyntaxToken,
    TypeAliasItem,
};

use crate::{
    database::{Database, HirId, ScopeId, SymbolId, TypeId},
    hir::Hir,
    scope::Scope,
    symbol::Symbol,
    ty::{Type, Typed},
    Diagnostic, DiagnosticInfo, DiagnosticKind,
};

pub struct Lowerer<'a> {
    db: &'a mut Database,
    scope_stack: Vec<ScopeId>,
    diagnostics: Vec<Diagnostic>,
    int_type: TypeId,
    bool_type: TypeId,
    bytes_type: TypeId,
    nil_type: TypeId,
    unknown_type: TypeId,
    unknown_hir: HirId,
    unknown: Typed,
}

impl<'a> Lowerer<'a> {
    pub fn new(db: &'a mut Database) -> Self {
        let int_type = db.alloc_type(Type::Int);
        let bool_type = db.alloc_type(Type::Bool);
        let bytes_type = db.alloc_type(Type::Bytes);
        let nil_type = db.alloc_type(Type::Nil);
        let unknown_type = db.alloc_type(Type::Unknown);
        let unknown_hir = db.alloc_hir(Hir::Unknown);

        Self {
            db,
            scope_stack: Vec::new(),
            diagnostics: Vec::new(),
            int_type,
            bool_type,
            bytes_type,
            nil_type,
            unknown_type,
            unknown_hir,
            unknown: Typed {
                ty: unknown_type,
                value: unknown_hir,
            },
        }
    }

    pub fn finish(self) -> Vec<Diagnostic> {
        self.diagnostics
    }

    pub fn compile_root(&mut self, root: Root, scope_id: ScopeId) {
        self.scope_stack.push(scope_id);
        self.compile_items(root.items());
        self.scope_stack.pop();
    }

    #[allow(clippy::single_match)]
    fn compile_items(&mut self, items: Vec<Item>) {
        let mut type_ids = Vec::new();
        for item in items.clone() {
            match item {
                Item::TypeAliasItem(ty) => type_ids.push(self.declare_type_alias(ty)),
                Item::StructItem(struct_item) => type_ids.push(self.declare_struct(struct_item)),
                _ => {}
            }
        }

        let mut symbol_ids = Vec::new();
        for item in items.clone() {
            match item {
                Item::FunctionItem(function) => symbol_ids.push(self.declare_function(function)),
                _ => {}
            }
        }

        let mut i = 0;
        for item in items.clone() {
            match item {
                Item::TypeAliasItem(ty) => self.compile_type_alias(ty, type_ids[i]),
                Item::StructItem(struct_item) => self.compile_struct(struct_item, type_ids[i]),
                _ => continue,
            }
            i += 1;
        }

        let mut i = 0;
        for item in items.clone() {
            match item {
                Item::FunctionItem(function) => self.compile_function(function, symbol_ids[i]),
                _ => continue,
            }
            i += 1;
        }
    }

    fn declare_function(&mut self, function_item: FunctionItem) -> SymbolId {
        let mut scope = Scope::default();

        let return_type = function_item
            .return_type()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.unknown_type);

        let mut parameter_types = Vec::new();

        for param in function_item.params() {
            let type_id = param
                .ty()
                .map(|ty| self.compile_type(ty))
                .unwrap_or(self.unknown_type);

            parameter_types.push(type_id);

            let symbol_id = self.db.alloc_symbol(Symbol::Parameter { type_id });

            if let Some(name) = param.name() {
                scope.define_symbol(name.to_string(), symbol_id);
            }
        }

        let scope_id = self.db.alloc_scope(scope);
        let hir_id = self.db.alloc_hir(Hir::Unknown);

        let symbol_id = self.db.alloc_symbol(Symbol::Function {
            scope_id,
            hir_id,
            return_type,
            parameter_types,
        });

        if let Some(name) = function_item.name() {
            self.scope_mut().define_symbol(name.to_string(), symbol_id);
        }

        symbol_id
    }

    fn declare_type_alias(&mut self, type_alias: TypeAliasItem) -> TypeId {
        let type_id = self.db.alloc_type(Type::Unknown);
        if let Some(name) = type_alias.name() {
            self.scope_mut().define_type(name.to_string(), type_id);
        }
        type_id
    }

    fn declare_struct(&mut self, struct_item: StructItem) -> TypeId {
        let type_id = self.db.alloc_type(Type::Unknown);
        if let Some(name) = struct_item.name() {
            self.scope_mut().define_type(name.to_string(), type_id);
        }
        type_id
    }

    fn compile_function(&mut self, function: FunctionItem, symbol_id: SymbolId) {
        let Some(body) = function.body() else {
            return;
        };

        let Symbol::Function {
            scope_id,
            return_type,
            ..
        } = self.db.symbol(symbol_id).clone()
        else {
            unreachable!();
        };

        self.scope_stack.push(scope_id);
        let output = self.compile_block_expr(body, None, Some(return_type));
        self.scope_stack.pop().unwrap();

        self.type_check(
            output.ty,
            return_type,
            function.body().unwrap().syntax().text_range(),
        );

        let Symbol::Function { hir_id, .. } = self.db.symbol_mut(symbol_id) else {
            unreachable!();
        };
        *hir_id = output.value;
    }

    fn compile_type_alias(&mut self, ty: TypeAliasItem, alias_type_id: TypeId) {
        let type_id = ty
            .ty()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.unknown_type);
        *self.db.ty_mut(alias_type_id) = Type::Alias(type_id);
        self.detect_cycle(type_id, ty.syntax().text_range());
    }

    fn compile_struct(&mut self, struct_item: StructItem, type_id: TypeId) {
        let mut named_fields = IndexMap::new();
        let mut fields = Vec::new();

        for field in struct_item.fields() {
            let type_id = field
                .ty()
                .map(|ty| self.compile_type(ty))
                .unwrap_or(self.unknown_type);

            fields.push(type_id);

            if let Some(name) = field.name() {
                named_fields.insert(name.to_string(), type_id);
            };
        }

        *self.db.ty_mut(type_id) = Type::Struct {
            named_fields,
            fields,
        };
    }

    fn compile_block_expr(
        &mut self,
        block: Block,
        scope_id: Option<ScopeId>,
        expected_type: Option<TypeId>,
    ) -> Typed {
        if let Some(scope_id) = scope_id {
            self.scope_stack.push(scope_id);
        }

        self.compile_items(block.items());

        let mut let_scope_ids = Vec::new();

        for let_stmt in block.let_stmts() {
            let expected_type = let_stmt.ty().map(|ty| self.compile_type(ty));

            let value = let_stmt
                .expr()
                .map(|expr| self.compile_expr(expr, expected_type))
                .unwrap_or(self.unknown);

            let Some(name) = let_stmt.name() else {
                continue;
            };

            let symbol_id = self.db.alloc_symbol(Symbol::Binding {
                type_id: value.ty,
                hir_id: value.value,
            });

            let mut let_scope = Scope::default();
            let_scope.define_symbol(name.to_string(), symbol_id);
            let scope_id = self.db.alloc_scope(let_scope);
            self.scope_stack.push(scope_id);

            let_scope_ids.push(scope_id);
        }

        let mut body = block
            .expr()
            .map(|expr| self.compile_expr(expr, expected_type))
            .unwrap_or(self.unknown);

        for scope_id in let_scope_ids.into_iter().rev() {
            body = Typed {
                value: self.db.alloc_hir(Hir::Scope {
                    scope_id,
                    value: body.value,
                }),
                ty: body.ty,
            };
            self.scope_stack.pop().unwrap();
        }

        if scope_id.is_some() {
            self.scope_stack.pop().unwrap();
        }

        body
    }

    fn compile_expr(&mut self, expr: Expr, expected_type: Option<TypeId>) -> Typed {
        match expr {
            Expr::PathExpr(path) => self.compile_path_expr(path),
            Expr::InitializerExpr(initializer) => self.compile_initializer_expr(initializer),
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
            Expr::FieldAccess(field_access) => self.compile_field_access(field_access),
            Expr::IndexAccess(index_access) => self.compile_index_access(index_access),
        }
    }

    fn compile_initializer_expr(&mut self, initializer: InitializerExpr) -> Typed {
        let ty = initializer.path().map(|path| {
            let Some(value) = path.name() else {
                return self.unknown_type;
            };
            self.compile_path_type(value)
        });

        let struct_fields = ty
            .and_then(|ty| match self.db.ty(ty) {
                Type::Struct { named_fields, .. } => Some(named_fields.clone()),
                _ => {
                    self.error(
                        DiagnosticInfo::UninitializableType(self.type_name(ty)),
                        initializer.path().unwrap().syntax().text_range(),
                    );
                    None
                }
            })
            .unwrap_or_default();

        let mut specified_fields = HashMap::new();

        for field in initializer.fields() {
            let expected_type = field
                .name()
                .and_then(|name| struct_fields.get(name.text()).copied());

            let value = field
                .expr()
                .map(|expr| self.compile_expr(expr, expected_type))
                .unwrap_or(self.unknown);

            if let Some(expected_type) = expected_type {
                self.type_check(value.ty, expected_type, field.syntax().text_range());
            }

            if let Some(name) = field.name() {
                if specified_fields.contains_key(name.text()) {
                    self.error(
                        DiagnosticInfo::DuplicateField(name.to_string()),
                        name.text_range(),
                    );
                } else if !struct_fields.contains_key(name.text()) {
                    self.error(
                        DiagnosticInfo::UndefinedField(name.to_string()),
                        name.text_range(),
                    );
                } else {
                    specified_fields.insert(name.to_string(), value.value);
                }
            }
        }

        let missing_fields = struct_fields
            .keys()
            .filter(|name| !specified_fields.contains_key(*name))
            .cloned()
            .collect::<Vec<String>>();

        if !missing_fields.is_empty() {
            self.error(
                DiagnosticInfo::MissingFields(missing_fields),
                initializer.syntax().text_range(),
            );
        }

        let mut args = Vec::new();

        for field in struct_fields.keys() {
            args.push(
                specified_fields
                    .get(field)
                    .cloned()
                    .unwrap_or(self.unknown_hir),
            );
        }

        match ty {
            Some(struct_type) => Typed {
                value: self.db.alloc_hir(Hir::List(args)),
                ty: struct_type,
            },
            None => self.unknown,
        }
    }

    fn compile_field_access(&mut self, field_access: FieldAccess) -> Typed {
        let Some(value) = field_access
            .expr()
            .map(|expr| self.compile_expr(expr, None))
        else {
            return self.unknown;
        };

        let Some(field_name) = field_access.field() else {
            return self.unknown;
        };

        match self.db.ty(value.ty) {
            Type::Struct {
                named_fields,
                fields,
            } => {
                if let Some(&field_ty) = named_fields.get(field_name.text()) {
                    Typed {
                        value: self.db.alloc_hir(Hir::ListIndex {
                            value: value.value,
                            index: fields
                                .iter()
                                .position(|&field| field == field_ty)
                                .unwrap()
                                .into(),
                        }),
                        ty: field_ty,
                    }
                } else {
                    self.error(
                        DiagnosticInfo::UndefinedField(field_name.to_string()),
                        field_name.text_range(),
                    );
                    self.unknown
                }
            }
            _ => {
                self.error(
                    DiagnosticInfo::FieldAccess(self.type_name(value.ty)),
                    field_access.expr().unwrap().syntax().text_range(),
                );
                self.unknown
            }
        }
    }

    fn compile_index_access(&mut self, index_access: IndexAccess) -> Typed {
        let Some(value) = index_access
            .expr()
            .map(|expr| self.compile_expr(expr, None))
        else {
            return self.unknown;
        };

        let Some(index_token) = index_access.index() else {
            return self.unknown;
        };
        let index = self.compile_int_raw(index_token);

        match self.db.ty(value.ty).clone() {
            Type::List(inner_type) => Typed {
                value: self.db.alloc_hir(Hir::ListIndex {
                    value: value.value,
                    index,
                }),
                ty: inner_type,
            },
            _ => {
                self.error(
                    DiagnosticInfo::IndexAccess(self.type_name(value.ty)),
                    index_access.expr().unwrap().syntax().text_range(),
                );
                self.unknown
            }
        }
    }

    fn compile_prefix_expr(&mut self, prefix_expr: PrefixExpr) -> Typed {
        let Some(expr) = prefix_expr.expr() else {
            return self.unknown;
        };

        let expr = self.compile_expr(expr, None);

        self.type_check(expr.ty, self.bool_type, prefix_expr.syntax().text_range());

        Typed {
            value: match prefix_expr.op() {
                Some(PrefixOp::Not) => self.db.alloc_hir(Hir::Not(expr.value)),
                _ => self.unknown_hir,
            },
            ty: self.bool_type,
        }
    }

    fn compile_binary_expr(&mut self, binary: BinaryExpr) -> Typed {
        let lhs = binary.lhs().map(|lhs| self.compile_expr(lhs, None));
        let rhs = binary.rhs().map(|rhs| self.compile_expr(rhs, None));

        if let Some(Typed { ty, .. }) = &lhs {
            self.type_check(
                *ty,
                self.int_type,
                binary.lhs().unwrap().syntax().text_range(),
            );
        }

        if let Some(Typed { ty, .. }) = &rhs {
            self.type_check(
                *ty,
                self.int_type,
                binary.rhs().unwrap().syntax().text_range(),
            );
        }

        let ty = binary
            .op()
            .map(|op| match op {
                BinaryOp::Add => self.int_type,
                BinaryOp::Subtract => self.int_type,
                BinaryOp::Multiply => self.int_type,
                BinaryOp::Divide => self.int_type,
                BinaryOp::Remainder => self.int_type,
                BinaryOp::LessThan => self.bool_type,
                BinaryOp::GreaterThan => self.bool_type,
                BinaryOp::LessThanEquals => self.bool_type,
                BinaryOp::GreaterThanEquals => self.bool_type,
                BinaryOp::Equals => self.bool_type,
                BinaryOp::NotEquals => self.bool_type,
            })
            .unwrap_or(self.unknown_type);

        match (lhs, rhs, binary.op()) {
            (Some(Typed { value: lhs, .. }), Some(Typed { value: rhs, .. }), Some(op)) => Typed {
                value: self.db.alloc_hir(Hir::BinaryOp { op, lhs, rhs }),
                ty,
            },
            _ => Typed {
                value: self.unknown_hir,
                ty,
            },
        }
    }

    fn compile_literal_expr(&mut self, literal: LiteralExpr) -> Typed {
        let Some(value) = literal.value() else {
            return self.unknown;
        };

        match value.kind() {
            SyntaxKind::Int => self.compile_int(value),
            SyntaxKind::String => self.compile_string(value),
            SyntaxKind::True => Typed {
                value: self.db.alloc_hir(Hir::Atom(vec![1])),
                ty: self.bool_type,
            },
            SyntaxKind::False => Typed {
                value: self.db.alloc_hir(Hir::Atom(Vec::new())),
                ty: self.bool_type,
            },
            SyntaxKind::Nil => Typed {
                value: self.db.alloc_hir(Hir::Atom(Vec::new())),
                ty: self.nil_type,
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
        let item_type = expected_item_type.unwrap_or(self.unknown_type);

        let mut items = first_item.map(|item| vec![item.value]).unwrap_or_default();

        for ast_item in ast_items.into_iter().skip(1) {
            let item = self.compile_expr(ast_item.clone(), expected_item_type);

            self.type_check(item.ty, item_type, ast_item.syntax().text_range());

            items.push(item.value);
        }

        Typed {
            value: self.db.alloc_hir(Hir::List(items)),
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
        let mut parameter_types = Vec::new();

        for (i, param) in lambda_expr
            .param_list()
            .map(|list| list.params())
            .unwrap_or_default()
            .into_iter()
            .enumerate()
        {
            let type_id = param
                .ty()
                .map(|ty| self.compile_type(ty))
                .or(expected
                    .as_ref()
                    .and_then(|expected| expected.0.get(i).copied()))
                .unwrap_or(self.unknown_type);

            parameter_types.push(type_id);

            if let Some(name) = param.name() {
                let symbol_id = self.db.alloc_symbol(Symbol::Parameter { type_id });
                scope.define_symbol(name.to_string(), symbol_id);
            };
        }

        let scope_id = self.db.alloc_scope(scope);

        let Some(body) = lambda_expr.body() else {
            return self.unknown;
        };

        let expected_return_type = lambda_expr
            .ty()
            .map(|ty| self.compile_type(ty))
            .or(expected.map(|expected| expected.1));

        self.scope_stack.push(scope_id);
        let body = self.compile_expr(body, expected_return_type);
        self.scope_stack.pop().expect("lambda not in scope stack");

        let return_type = expected_return_type.unwrap_or(body.ty);

        self.type_check(
            body.ty,
            return_type,
            lambda_expr.body().unwrap().syntax().text_range(),
        );

        let symbol_id = self.db.alloc_symbol(Symbol::Function {
            scope_id,
            hir_id: body.value,
            parameter_types: parameter_types.clone(),
            return_type,
        });

        Typed {
            value: self.db.alloc_hir(Hir::Reference(symbol_id)),
            ty: self.db.alloc_type(Type::Function {
                parameter_types,
                return_type,
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
            self.type_check(
                condition_type,
                self.bool_type,
                if_expr.condition().unwrap().syntax().text_range(),
            );
        }

        if let (Some(Typed { ty: then_type, .. }), Some(Typed { ty: else_type, .. })) =
            (&then_block, &else_block)
        {
            self.type_check(
                *else_type,
                *then_type,
                if_expr.else_block().unwrap().syntax().text_range(),
            );
        }

        let ty = then_block
            .as_ref()
            .or(else_block.as_ref())
            .map(|block| block.ty)
            .unwrap_or(self.unknown_type);

        let value = condition.and_then(|condition| {
            then_block.and_then(|then_block| {
                else_block.map(|else_block| {
                    self.db.alloc_hir(Hir::If {
                        condition: condition.value,
                        then_block: then_block.value,
                        else_block: else_block.value,
                    })
                })
            })
        });

        Typed {
            value: value.unwrap_or(self.unknown_hir),
            ty,
        }
    }

    fn compile_int_raw(&mut self, int: SyntaxToken) -> BigInt {
        int.text()
            .replace('_', "")
            .parse()
            .expect("failed to parse into BigInt")
    }

    fn compile_int(&mut self, int: SyntaxToken) -> Typed {
        let num = self.compile_int_raw(int);
        Typed {
            value: self.db.alloc_hir(Hir::Atom(bigint_to_bytes(num))),
            ty: self.int_type,
        }
    }

    fn compile_string(&mut self, string: SyntaxToken) -> Typed {
        let text = string.text();
        let quote = text.chars().next().unwrap();
        let after_prefix = &text[1..];
        let before_suffix = after_prefix.strip_suffix(quote).unwrap_or(after_prefix);

        Typed {
            value: self
                .db
                .alloc_hir(Hir::Atom(before_suffix.as_bytes().to_vec())),
            ty: self.db.alloc_type(Type::Bytes),
        }
    }

    fn compile_path_expr(&mut self, path: PathExpr) -> Typed {
        let Some(name) = path.name() else {
            return self.unknown;
        };

        let Some(symbol_id) = self
            .scope_stack
            .iter()
            .rev()
            .find_map(|&scope_id| self.db.scope(scope_id).symbol(name.text()))
        else {
            self.error(
                DiagnosticInfo::UndefinedReference(name.to_string()),
                name.text_range(),
            );
            return self.unknown;
        };

        Typed {
            value: self.db.alloc_hir(Hir::Reference(symbol_id)),
            ty: match self.db.symbol(symbol_id) {
                Symbol::Function {
                    parameter_types,
                    return_type,
                    ..
                } => self.db.alloc_type(Type::Function {
                    parameter_types: parameter_types.clone(),
                    return_type: *return_type,
                }),
                Symbol::Parameter { type_id } => *type_id,
                Symbol::Binding { type_id, .. } => *type_id,
            },
        }
    }

    fn compile_function_call(&mut self, call: FunctionCall) -> Typed {
        let Some(callee) = call.callee() else {
            return self.unknown;
        };

        let Some(args) = call.args() else {
            return self.unknown;
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
        let arg_values: Vec<HirId> = args.iter().map(|arg| arg.value).collect();

        match self.db.ty(callee.ty).clone() {
            Type::Function {
                parameter_types,
                return_type,
            } => {
                if parameter_types.len() != arg_types.len() {
                    self.error(
                        DiagnosticInfo::ArgumentMismatch {
                            expected: parameter_types.len(),
                            found: arg_types.len(),
                        },
                        call.args().expect("no arguments").syntax().text_range(),
                    );

                    return self.unknown;
                }

                for (i, (param, &arg)) in parameter_types
                    .clone()
                    .into_iter()
                    .zip(arg_types.iter())
                    .enumerate()
                {
                    self.type_check(arg, param, raw_args[i].syntax().text_range());
                }

                Typed {
                    value: self.db.alloc_hir(Hir::FunctionCall {
                        callee: callee.value,
                        args: arg_values,
                    }),
                    ty: return_type,
                }
            }
            Type::Unknown => Typed {
                value: self.db.alloc_hir(Hir::FunctionCall {
                    callee: callee.value,
                    args: arg_values,
                }),
                ty: self.unknown_type,
            },
            _ => {
                self.error(
                    DiagnosticInfo::UncallableType(self.type_name(callee.ty)),
                    call.callee().expect("no callee").syntax().text_range(),
                );
                self.unknown
            }
        }
    }

    fn compile_type(&mut self, ty: rue_parser::Type) -> TypeId {
        match ty {
            rue_parser::Type::PathType(literal) => {
                let Some(value) = literal.name() else {
                    return self.unknown_type;
                };
                self.compile_path_type(value)
            }
            rue_parser::Type::ListType(list) => self.compile_list_type(list),
            rue_parser::Type::FunctionType(function) => self.compile_function_type(function),
        }
    }

    fn compile_path_type(&mut self, token: SyntaxToken) -> TypeId {
        for &scope_id in self.scope_stack.iter().rev() {
            if let Some(ty) = self.db.scope(scope_id).type_alias(token.text()) {
                return ty;
            }
        }

        match token.text() {
            "Int" => self.int_type,
            "Bool" => self.bool_type,
            "Bytes" => self.bytes_type,
            _ => {
                self.error(
                    DiagnosticInfo::UndefinedType(token.to_string()),
                    token.text_range(),
                );
                self.unknown_type
            }
        }
    }

    fn compile_list_type(&mut self, list: ListType) -> TypeId {
        let Some(inner) = list.ty() else {
            return self.unknown_type;
        };

        let inner = self.compile_type(inner);
        self.db.alloc_type(Type::List(inner))
    }

    fn compile_function_type(&mut self, function: FunctionType) -> TypeId {
        let parameter_types = function
            .params()
            .map(|params| params.types())
            .unwrap_or_default()
            .into_iter()
            .map(|ty| self.compile_type(ty))
            .collect();

        let return_type = function
            .ret()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.unknown_type);

        self.db.alloc_type(Type::Function {
            parameter_types,
            return_type,
        })
    }

    fn extract_function_type_components(&self, type_id: TypeId) -> Option<(Vec<TypeId>, TypeId)> {
        match self.db.ty(type_id) {
            Type::Function {
                parameter_types,
                return_type,
            } => Some((parameter_types.clone(), *return_type)),
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
            Type::List(inner) => {
                self.detect_cycle_visitor(inner, text_range, visited_aliases);
            }
            Type::Struct { fields, .. } => {
                for &field in fields.iter() {
                    self.detect_cycle_visitor(field, text_range, visited_aliases);
                }
            }
            Type::Function {
                parameter_types,
                return_type,
            } => {
                for &param in parameter_types.iter() {
                    self.detect_cycle_visitor(param, text_range, visited_aliases);
                }

                self.detect_cycle_visitor(return_type, text_range, visited_aliases);
            }
            Type::Alias(alias) => {
                if !visited_aliases.insert(alias) {
                    self.error(DiagnosticInfo::RecursiveTypeAlias, text_range);
                    return;
                }
                self.detect_cycle_visitor(alias, text_range, visited_aliases);
            }
            Type::Unknown | Type::Nil | Type::Int | Type::Bool | Type::Bytes => {}
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
            Type::Struct { named_fields, .. } => {
                let fields: Vec<String> = named_fields
                    .iter()
                    .map(|(name, ty)| {
                        format!("{}: {}", name, self.type_name_visitor(*ty, visited_aliases))
                    })
                    .collect();
                format!("{{ {} }}", fields.join(", "))
            }
            Type::Function {
                parameter_types,
                return_type,
            } => {
                let params: Vec<String> = parameter_types
                    .iter()
                    .map(|&ty| self.type_name_visitor(ty, visited_aliases))
                    .collect();
                let ret = self.type_name_visitor(*return_type, visited_aliases);
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

    fn type_check(&self, value_type_id: TypeId, assign_to_type_id: TypeId, range: TextRange) {
        if !self.is_assignable_to(value_type_id, assign_to_type_id, &mut HashSet::new()) {
            Some((
                DiagnosticInfo::TypeMismatch {
                    expected: self.type_name(assign_to_type_id),
                    found: self.type_name(value_type_id),
                },
                range,
            ))
        } else {
            None
        };
    }

    fn is_assignable_to(
        &self,
        a: TypeId,
        b: TypeId,
        visited_aliases: &mut HashSet<TypeId>,
    ) -> bool {
        if a == b {
            return true;
        }

        match (self.db.ty(a).clone(), self.db.ty(b).clone()) {
            (Type::Unknown, Type::Unknown) => true,
            (Type::Int, Type::Int) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Bytes, Type::Bytes) => true,
            (Type::Nil, Type::Nil) => true,
            (Type::Alias(alias), _) => {
                if !visited_aliases.insert(alias) {
                    return true;
                }
                self.is_assignable_to(alias, b, visited_aliases)
            }
            (_, Type::Alias(alias)) => {
                if !visited_aliases.insert(alias) {
                    return true;
                }
                self.is_assignable_to(a, alias, visited_aliases)
            }
            (Type::List(inner_a), Type::List(inner_b)) => {
                self.is_assignable_to(inner_a, inner_b, visited_aliases)
            }
            (
                Type::Function {
                    parameter_types: params_a,
                    return_type: ret_a,
                },
                Type::Function {
                    parameter_types: params_b,
                    return_type: ret_b,
                },
            ) => {
                if params_a.len() != params_b.len() {
                    return false;
                }

                for (a, b) in params_a.into_iter().zip(params_b.into_iter()) {
                    if !self.is_assignable_to(a, b, visited_aliases) {
                        return false;
                    }
                }

                self.is_assignable_to(ret_a, ret_b, visited_aliases)
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
