use std::{
    collections::{HashMap, HashSet},
    fmt,
    str::FromStr,
};

use indexmap::{IndexMap, IndexSet};
use num_bigint::BigInt;
use rowan::TextRange;
use rue_parser::{
    AstNode, BinaryExpr, BinaryOp, Block, ConstItem, EnumItem, Expr, FieldAccess, FunctionCall,
    FunctionItem, FunctionType, IfExpr, IndexAccess, InitializerExpr, InitializerField, Item,
    LambdaExpr, ListExpr, ListType, LiteralExpr, Path, PrefixExpr, PrefixOp, Root, Stmt,
    StructField, StructItem, SyntaxKind, SyntaxToken, TupleExpr, TupleType, Type as AstType,
    TypeAliasItem,
};

use crate::{
    database::{Database, HirId, ScopeId, SymbolId, TypeId},
    hir::Hir,
    scope::Scope,
    symbol::Symbol,
    ty::{Type, Value},
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
    nil_hir: HirId,
    unknown_type: TypeId,
    unknown_hir: HirId,
}

impl<'a> Lowerer<'a> {
    pub fn new(db: &'a mut Database) -> Self {
        let int_type = db.alloc_type(Type::Int);
        let bool_type = db.alloc_type(Type::Bool);
        let bytes_type = db.alloc_type(Type::Bytes);
        let nil_type = db.alloc_type(Type::Tuple(Vec::new()));
        let nil_hir = db.alloc_hir(Hir::Atom(Vec::new()));
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
            nil_hir,
            unknown_type,
            unknown_hir,
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

    fn compile_items(&mut self, items: Vec<Item>) {
        let mut type_ids = Vec::new();
        for item in items.clone() {
            match item {
                Item::TypeAliasItem(ty) => type_ids.push(self.declare_type_alias(ty)),
                Item::StructItem(struct_item) => type_ids.push(self.declare_struct(struct_item)),
                Item::EnumItem(enum_item) => type_ids.push(self.declare_enum(enum_item)),
                _ => {}
            }
        }

        let mut symbol_ids = Vec::new();
        for item in items.clone() {
            match item {
                Item::FunctionItem(function) => symbol_ids.push(self.declare_function(function)),
                Item::ConstItem(const_item) => symbol_ids.push(self.declare_const(const_item)),
                _ => {}
            }
        }

        let mut i = 0;
        for item in items.clone() {
            match item {
                Item::TypeAliasItem(ty) => self.compile_type_alias(ty, type_ids[i]),
                Item::StructItem(struct_item) => self.compile_struct(struct_item, type_ids[i]),
                Item::EnumItem(enum_item) => self.compile_enum(enum_item, type_ids[i]),
                _ => continue,
            }
            i += 1;
        }

        let mut i = 0;
        for item in items.clone() {
            match item {
                Item::FunctionItem(function) => self.compile_function(function, symbol_ids[i]),
                Item::ConstItem(const_item) => self.compile_const(const_item, symbol_ids[i]),
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

    fn declare_const(&mut self, const_item: ConstItem) -> SymbolId {
        let type_id = const_item
            .ty()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.unknown_type);
        let hir_id = self.db.alloc_hir(Hir::Unknown);

        let symbol_id = self
            .db
            .alloc_symbol(Symbol::ConstBinding { type_id, hir_id });

        if let Some(name) = const_item.name() {
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

    fn declare_enum(&mut self, enum_item: EnumItem) -> TypeId {
        let mut variants = IndexMap::new();

        for variant in enum_item.variants() {
            let Some(name) = variant.name() else {
                continue;
            };

            if variants.contains_key(name.text()) {
                continue;
            }

            variants.insert(name.to_string(), self.db.alloc_type(Type::Unknown));
        }

        let type_id = self.db.alloc_type(Type::Enum { variants });

        if let Some(name) = enum_item.name() {
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
            output.ty(),
            return_type,
            function.body().unwrap().syntax().text_range(),
        );

        let Symbol::Function { hir_id, .. } = self.db.symbol_mut(symbol_id) else {
            unreachable!();
        };
        *hir_id = output.hir();
    }

    fn compile_const(&mut self, const_item: ConstItem, symbol_id: SymbolId) {
        let Some(expr) = const_item.expr() else {
            return;
        };

        let Symbol::ConstBinding { type_id, .. } = self.db.symbol(symbol_id).clone() else {
            unreachable!();
        };

        let output = self.compile_expr(expr, Some(type_id));

        self.type_check(output.ty(), type_id, const_item.syntax().text_range());

        let Symbol::ConstBinding { hir_id, .. } = self.db.symbol_mut(symbol_id) else {
            unreachable!();
        };
        *hir_id = output.hir();
    }

    fn compile_type_alias(&mut self, ty: TypeAliasItem, alias_type_id: TypeId) {
        let type_id = ty
            .ty()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.unknown_type);

        *self.db.ty_mut(alias_type_id) = Type::Alias(type_id);

        // A cycle has been detected, so prevent this type from causing issues later.
        if self.detect_cycle(alias_type_id, ty.syntax().text_range(), &mut HashSet::new()) {
            *self.db.ty_mut(alias_type_id) = Type::Unknown;
        }
    }

    fn compile_struct(&mut self, struct_item: StructItem, type_id: TypeId) {
        let fields = self.compile_struct_fields(struct_item.fields());
        *self.db.ty_mut(type_id) = Type::Struct { fields };
    }

    fn compile_struct_fields(&mut self, fields: Vec<StructField>) -> IndexMap<String, TypeId> {
        let mut named_fields = IndexMap::new();

        for field in fields {
            let type_id = field
                .ty()
                .map(|ty| self.compile_type(ty))
                .unwrap_or(self.unknown_type);

            if let Some(name) = field.name() {
                named_fields.insert(name.to_string(), type_id);
            };
        }

        named_fields
    }

    fn compile_enum(&mut self, enum_item: EnumItem, type_id: TypeId) {
        let Type::Enum { variants } = self.db.ty(type_id).clone() else {
            unreachable!();
        };

        let mut visited_variants = IndexSet::new();

        for variant in enum_item.variants() {
            let Some(name) = variant.name() else {
                continue;
            };

            if !visited_variants.insert(name.to_string()) {
                self.error(
                    DiagnosticInfo::DuplicateEnumVariant(name.to_string()),
                    name.text_range(),
                );
                continue;
            }

            let variant_type = variants[name.text()];

            let fields = self.compile_struct_fields(variant.fields());

            let discriminant = variant
                .discriminant()
                .map(|discriminant| self.compile_int(discriminant).hir())
                .unwrap_or(self.unknown_hir);

            *self.db.ty_mut(variant_type) = Type::EnumVariant {
                name: name.to_string(),
                enum_type: type_id,
                fields,
                discriminant,
            };
        }
    }

    fn compile_block_expr(
        &mut self,
        block: Block,
        scope_id: Option<ScopeId>,
        expected_type: Option<TypeId>,
    ) -> Value {
        if let Some(scope_id) = scope_id {
            self.scope_stack.push(scope_id);
        }

        self.compile_items(block.items());

        let mut let_scope_ids = Vec::new();

        for stmt in block.stmts() {
            match stmt {
                Stmt::LetStmt(let_stmt) => {
                    let expected_type = let_stmt.ty().map(|ty| self.compile_type(ty));

                    let value = let_stmt
                        .expr()
                        .map(|expr| self.compile_expr(expr, expected_type))
                        .unwrap_or(self.unknown());

                    if let Some(expected_type) = expected_type {
                        self.type_check(value.ty(), expected_type, let_stmt.syntax().text_range());
                    }

                    let Some(name) = let_stmt.name() else {
                        continue;
                    };

                    let symbol_id = self.db.alloc_symbol(Symbol::LetBinding {
                        type_id: expected_type.unwrap_or(value.ty()),
                        hir_id: value.hir(),
                    });

                    let mut let_scope = Scope::default();
                    let_scope.define_symbol(name.to_string(), symbol_id);
                    let scope_id = self.db.alloc_scope(let_scope);
                    self.scope_stack.push(scope_id);

                    let_scope_ids.push(scope_id);
                }
            }
        }

        let mut body = block
            .expr()
            .map(|expr| self.compile_expr(expr, expected_type))
            .unwrap_or(self.unknown());

        for scope_id in let_scope_ids.into_iter().rev() {
            body = Value::typed(
                self.db.alloc_hir(Hir::Scope {
                    scope_id,
                    value: body.hir(),
                }),
                body.ty(),
            );
            self.scope_stack.pop().unwrap();
        }

        if scope_id.is_some() {
            self.scope_stack.pop().unwrap();
        }

        body
    }

    fn compile_expr(&mut self, expr: Expr, expected_type: Option<TypeId>) -> Value {
        match expr {
            Expr::Path(path) => self.compile_path_expr(path),
            Expr::InitializerExpr(initializer) => self.compile_initializer_expr(initializer),
            Expr::LiteralExpr(literal) => self.compile_literal_expr(literal),
            Expr::ListExpr(list) => self.compile_list_expr(list, expected_type),
            Expr::TupleExpr(list) => self.compile_tuple_expr(list, expected_type),
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

    fn compile_initializer_expr(&mut self, initializer: InitializerExpr) -> Value {
        let ty = initializer.path().map(|path| self.compile_path_type(path));

        match ty.map(|ty| self.db.ty(ty)).cloned() {
            Some(Type::Struct { fields }) => {
                let hir_id = self.compile_initializer_fields(
                    fields,
                    initializer.fields(),
                    initializer.syntax().text_range(),
                );

                match ty {
                    Some(struct_type) => Value::typed(hir_id, struct_type),
                    None => self.unknown(),
                }
            }
            Some(Type::EnumVariant {
                fields,
                discriminant,
                ..
            }) => {
                let fields_hir_id = self.compile_initializer_fields(
                    fields,
                    initializer.fields(),
                    initializer.syntax().text_range(),
                );

                let hir_id = self.db.alloc_hir(Hir::Pair(discriminant, fields_hir_id));

                match ty {
                    Some(struct_type) => Value::typed(hir_id, struct_type),
                    None => self.unknown(),
                }
            }
            Some(_) => {
                self.error(
                    DiagnosticInfo::UninitializableType(self.type_name(ty.unwrap())),
                    initializer.path().unwrap().syntax().text_range(),
                );
                self.unknown()
            }
            _ => self.unknown(),
        }
    }

    fn compile_initializer_fields(
        &mut self,
        struct_fields: IndexMap<String, TypeId>,
        initializer_fields: Vec<InitializerField>,
        text_range: TextRange,
    ) -> HirId {
        let mut specified_fields = HashMap::new();

        for field in initializer_fields {
            let expected_type = field
                .name()
                .and_then(|name| struct_fields.get(name.text()).copied());

            let value = field
                .expr()
                .map(|expr| self.compile_expr(expr, expected_type))
                .unwrap_or(self.unknown());

            if let Some(expected_type) = expected_type {
                self.type_check(value.ty(), expected_type, field.syntax().text_range());
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
                    specified_fields.insert(name.to_string(), value.hir());
                }
            }
        }

        let missing_fields = struct_fields
            .keys()
            .filter(|name| !specified_fields.contains_key(*name))
            .cloned()
            .collect::<Vec<String>>();

        if !missing_fields.is_empty() {
            self.error(DiagnosticInfo::MissingFields(missing_fields), text_range);
        }

        let mut hir_id = self.nil_hir;

        for field in struct_fields.keys().rev() {
            let field = specified_fields
                .get(field)
                .cloned()
                .unwrap_or(self.unknown_hir);

            hir_id = self.db.alloc_hir(Hir::Pair(field, hir_id));
        }

        hir_id
    }

    fn compile_field_access(&mut self, field_access: FieldAccess) -> Value {
        let Some(value) = field_access
            .expr()
            .map(|expr| self.compile_expr(expr, None))
        else {
            return self.unknown();
        };

        let Some(token) = field_access.field() else {
            return self.unknown();
        };

        match token.kind() {
            SyntaxKind::Ident => self.compile_struct_field_access(value, token),
            SyntaxKind::Int => self.compile_tuple_field_access(value, token),
            _ => unreachable!(),
        }
    }

    fn compile_struct_field_access(&mut self, value: Value, field_name: SyntaxToken) -> Value {
        match self.db.ty(value.ty()) {
            Type::Struct { fields } => {
                if let Some((index, _name, &field_type)) = fields.get_full(field_name.text()) {
                    Value::typed(
                        self.db.alloc_hir(Hir::Index {
                            value: value.hir(),
                            index: index as u32,
                            rest: false,
                        }),
                        field_type,
                    )
                } else {
                    self.error(
                        DiagnosticInfo::UndefinedField(field_name.to_string()),
                        field_name.text_range(),
                    );
                    self.unknown()
                }
            }
            _ => {
                self.error(
                    DiagnosticInfo::StructFieldAccess(self.type_name(value.ty())),
                    field_name.text_range(),
                );
                self.unknown()
            }
        }
    }

    fn compile_tuple_field_access(&mut self, value: Value, index_token: SyntaxToken) -> Value {
        let index = self.compile_int_raw(index_token.clone());

        let Some(items) = self.extract_tuple_types(value.ty()) else {
            self.error(
                DiagnosticInfo::TupleFieldAccess(self.type_name(value.ty())),
                index_token.text_range(),
            );
            return self.unknown();
        };

        if index >= items.len() as u32 {
            self.error(
                DiagnosticInfo::IndexOutOfBounds(index, items.len() as u32),
                index_token.text_range(),
            );
            return self.unknown();
        }

        Value::typed(
            self.db.alloc_hir(Hir::Index {
                value: value.hir(),
                index,
                rest: index + 1 == items.len() as u32,
            }),
            items[index as usize],
        )
    }

    fn compile_index_access(&mut self, index_access: IndexAccess) -> Value {
        let Some(value) = index_access
            .expr()
            .map(|expr| self.compile_expr(expr, None))
        else {
            return self.unknown();
        };

        let Some(index_token) = index_access.index() else {
            return self.unknown();
        };
        let index = self.compile_int_raw(index_token.clone());

        let Some(ty) = self.extract_list_type(value.ty()) else {
            self.error(
                DiagnosticInfo::IndexAccess(self.type_name(value.ty())),
                index_access.expr().unwrap().syntax().text_range(),
            );
            return self.unknown();
        };

        Value::typed(
            self.db.alloc_hir(Hir::Index {
                value: value.hir(),
                index,
                rest: false,
            }),
            ty,
        )
    }

    fn compile_prefix_expr(&mut self, prefix_expr: PrefixExpr) -> Value {
        let Some(expr) = prefix_expr.expr() else {
            return self.unknown();
        };

        let expr = self.compile_expr(expr, None);

        self.type_check(expr.ty(), self.bool_type, prefix_expr.syntax().text_range());

        Value::typed(
            match prefix_expr.op() {
                Some(PrefixOp::Not) => self.db.alloc_hir(Hir::Not(expr.hir())),
                _ => self.unknown_hir,
            },
            self.bool_type,
        )
    }

    fn compile_binary_expr(&mut self, binary: BinaryExpr) -> Value {
        let lhs = binary.lhs().map(|lhs| self.compile_expr(lhs, None));
        let rhs = binary.rhs().map(|rhs| self.compile_expr(rhs, None));

        if let Some(lhs) = &lhs {
            self.type_check(
                lhs.ty(),
                self.int_type,
                binary.lhs().unwrap().syntax().text_range(),
            );
        }

        if let Some(rhs) = &rhs {
            self.type_check(
                rhs.ty(),
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
            (Some(lhs), Some(rhs), Some(op)) => Value::typed(
                self.db.alloc_hir(Hir::BinaryOp {
                    op,
                    lhs: lhs.hir(),
                    rhs: rhs.hir(),
                }),
                ty,
            ),
            _ => Value::typed(self.unknown_hir, ty),
        }
    }

    fn compile_literal_expr(&mut self, literal: LiteralExpr) -> Value {
        let Some(value) = literal.value() else {
            return self.unknown();
        };

        match value.kind() {
            SyntaxKind::Int => self.compile_int(value),
            SyntaxKind::String => self.compile_string(value),
            SyntaxKind::True => Value::typed(self.db.alloc_hir(Hir::Atom(vec![1])), self.bool_type),
            SyntaxKind::False => {
                Value::typed(self.db.alloc_hir(Hir::Atom(Vec::new())), self.bool_type)
            }
            SyntaxKind::Nil => {
                Value::typed(self.db.alloc_hir(Hir::Atom(Vec::new())), self.nil_type)
            }
            _ => unreachable!(),
        }
    }

    fn compile_list_expr(
        &mut self,
        list_expr: ListExpr,
        expected_expr_type: Option<TypeId>,
    ) -> Value {
        let mut items = Vec::new();
        let mut nil_terminated = true;

        let mut list_type = expected_expr_type;
        let mut item_type = expected_expr_type.and_then(|ty| self.extract_list_type(ty));

        let len = list_expr.items().len();

        for (i, item) in list_expr.items().into_iter().enumerate() {
            let expected_item_type = if item.op().is_some() {
                list_type
            } else {
                item_type
            };

            let output = item
                .expr()
                .map(|expr| self.compile_expr(expr, expected_item_type))
                .unwrap_or(self.unknown());

            if let Some(expected_item_type) = expected_item_type {
                self.type_check(output.ty(), expected_item_type, item.syntax().text_range());
            }

            if i + 1 == len && list_type.is_none() {
                if item.op().is_some() {
                    list_type = Some(output.ty());
                    item_type = self.extract_list_type(output.ty());
                } else {
                    list_type = Some(self.db.alloc_type(Type::List(output.ty())));
                    item_type = Some(output.ty());
                }
            }

            if let Some(spread) = item.op() {
                if i + 1 == len {
                    nil_terminated = false;
                } else {
                    self.error(DiagnosticInfo::NonFinalSpread, spread.text_range());
                }
            }

            items.push(output.hir());
        }

        let mut hir_id = self.nil_hir;

        for (i, item) in items.into_iter().rev().enumerate() {
            if i == 0 && !nil_terminated {
                hir_id = item;
            } else {
                hir_id = self.db.alloc_hir(Hir::Pair(item, hir_id));
            }
        }

        Value::typed(
            hir_id,
            list_type.unwrap_or_else(|| self.db.alloc_type(Type::List(self.unknown_type))),
        )
    }

    fn compile_tuple_expr(
        &mut self,
        tuple_expr: TupleExpr,
        expected_type: Option<TypeId>,
    ) -> Value {
        let mut hir_id = self.nil_hir;
        let mut types = Vec::new();

        let tuple = expected_type
            .and_then(|ty| self.extract_tuple_types(ty))
            .unwrap_or_default();

        let len = tuple_expr.items().len();

        for (i, item) in tuple_expr.items().into_iter().enumerate().rev() {
            let expected_type = tuple.get(i).copied();
            let output = self.compile_expr(item.clone(), expected_type);

            if let Some(expected_type) = expected_type {
                self.type_check(output.ty(), expected_type, item.syntax().text_range());
            }

            types.push(output.ty());

            if i + 1 == len {
                hir_id = output.hir();
                continue;
            }

            hir_id = self.db.alloc_hir(Hir::Pair(output.hir(), hir_id));
        }

        // We added these in inverse order.
        types.reverse();

        Value::typed(hir_id, self.db.alloc_type(Type::Tuple(types)))
    }

    fn compile_lambda_expr(
        &mut self,
        lambda_expr: LambdaExpr,
        expected_type: Option<TypeId>,
    ) -> Value {
        let expected = expected_type.and_then(|expected| self.extract_function_type(expected));

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
            return self.unknown();
        };

        let expected_return_type = lambda_expr
            .ty()
            .map(|ty| self.compile_type(ty))
            .or(expected.map(|expected| expected.1));

        self.scope_stack.push(scope_id);
        let body = self.compile_expr(body, expected_return_type);
        self.scope_stack.pop().expect("lambda not in scope stack");

        let return_type = expected_return_type.unwrap_or(body.ty());

        self.type_check(
            body.ty(),
            return_type,
            lambda_expr.body().unwrap().syntax().text_range(),
        );

        let symbol_id = self.db.alloc_symbol(Symbol::Function {
            scope_id,
            hir_id: body.hir(),
            parameter_types: parameter_types.clone(),
            return_type,
        });

        Value::typed(
            self.db.alloc_hir(Hir::Reference(symbol_id)),
            self.db.alloc_type(Type::Function {
                parameter_types,
                return_type,
            }),
        )
    }

    fn compile_if_expr(&mut self, if_expr: IfExpr, expected_type: Option<TypeId>) -> Value {
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

        if let Some(condition_type) = condition.as_ref().map(|condition| condition.ty()) {
            self.type_check(
                condition_type,
                self.bool_type,
                if_expr.condition().unwrap().syntax().text_range(),
            );
        }

        if let (Some(then_block), Some(else_block)) = (&then_block, &else_block) {
            self.type_check(
                else_block.ty(),
                then_block.ty(),
                if_expr.else_block().unwrap().syntax().text_range(),
            );
        }

        let ty = then_block
            .as_ref()
            .or(else_block.as_ref())
            .map(|block| block.ty())
            .unwrap_or(self.unknown_type);

        let value = condition.and_then(|condition| {
            then_block.and_then(|then_block| {
                else_block.map(|else_block| {
                    self.db.alloc_hir(Hir::If {
                        condition: condition.hir(),
                        then_block: then_block.hir(),
                        else_block: else_block.hir(),
                    })
                })
            })
        });

        Value::typed(value.unwrap_or(self.unknown_hir), ty)
    }

    fn compile_int_raw<T, E>(&mut self, int: SyntaxToken) -> T
    where
        T: FromStr<Err = E>,
        E: fmt::Debug,
    {
        int.text()
            .replace('_', "")
            .parse()
            .expect("failed to parse into BigInt")
    }

    fn compile_int(&mut self, int: SyntaxToken) -> Value {
        let num = self.compile_int_raw(int);

        Value::typed(
            self.db.alloc_hir(Hir::Atom(bigint_to_bytes(num))),
            self.int_type,
        )
    }

    fn compile_string(&mut self, string: SyntaxToken) -> Value {
        let text = string.text();
        let quote = text.chars().next().unwrap();
        let after_prefix = &text[1..];
        let before_suffix = after_prefix.strip_suffix(quote).unwrap_or(after_prefix);

        Value::typed(
            self.db
                .alloc_hir(Hir::Atom(before_suffix.as_bytes().to_vec())),
            self.db.alloc_type(Type::Bytes),
        )
    }

    fn compile_path_expr(&mut self, path: Path) -> Value {
        let mut idents = path.idents();

        if idents.len() > 1 {
            self.error(DiagnosticInfo::PathNotAllowed, path.syntax().text_range());
            return self.unknown();
        }

        let name = idents.remove(0);

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
            return self.unknown();
        };

        Value::typed(
            self.db.alloc_hir(Hir::Reference(symbol_id)),
            match self.db.symbol(symbol_id) {
                Symbol::Function {
                    parameter_types,
                    return_type,
                    ..
                } => self.db.alloc_type(Type::Function {
                    parameter_types: parameter_types.clone(),
                    return_type: *return_type,
                }),
                Symbol::Parameter { type_id } => *type_id,
                Symbol::LetBinding { type_id, .. } => *type_id,
                Symbol::ConstBinding { type_id, .. } => *type_id, //todo
            },
        )
    }

    fn compile_function_call(&mut self, call: FunctionCall) -> Value {
        let Some(callee) = call.callee() else {
            return self.unknown();
        };

        let Some(args) = call.args() else {
            return self.unknown();
        };

        let callee = self.compile_expr(callee, None);

        let expected = self.extract_function_type(callee.ty());

        let raw_args = args.exprs();

        let args: Vec<Value> = args
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

        let arg_types: Vec<TypeId> = args.iter().map(|arg| arg.ty()).collect();
        let arg_values: Vec<HirId> = args.iter().map(|arg| arg.hir()).collect();

        match self.db.ty(callee.ty()).clone() {
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

                    return self.unknown();
                }

                for (i, (param, &arg)) in parameter_types
                    .clone()
                    .into_iter()
                    .zip(arg_types.iter())
                    .enumerate()
                {
                    self.type_check(arg, param, raw_args[i].syntax().text_range());
                }

                Value::typed(
                    self.db.alloc_hir(Hir::FunctionCall {
                        callee: callee.hir(),
                        args: arg_values,
                    }),
                    return_type,
                )
            }
            Type::Unknown => Value::typed(
                self.db.alloc_hir(Hir::FunctionCall {
                    callee: callee.hir(),
                    args: arg_values,
                }),
                self.unknown_type,
            ),
            _ => {
                self.error(
                    DiagnosticInfo::UncallableType(self.type_name(callee.ty())),
                    call.callee().expect("no callee").syntax().text_range(),
                );
                self.unknown()
            }
        }
    }

    fn compile_type(&mut self, ty: AstType) -> TypeId {
        match ty {
            AstType::Path(path) => self.compile_path_type(path),
            AstType::ListType(list) => self.compile_list_type(list),
            AstType::FunctionType(function) => self.compile_function_type(function),
            AstType::TupleType(tuple) => self.compile_tuple_type(tuple),
        }
    }

    fn compile_path_type(&mut self, path: Path) -> TypeId {
        let mut idents = path.idents();

        let name = idents.remove(0);

        let mut ty = 'initial_type: {
            for &scope_id in self.scope_stack.iter().rev() {
                if let Some(ty) = self.db.scope(scope_id).type_alias(name.text()) {
                    break 'initial_type ty;
                }
            }

            match name.text() {
                "Int" => self.int_type,
                "Bool" => self.bool_type,
                "Bytes" => self.bytes_type,
                _ => {
                    self.error(
                        DiagnosticInfo::UndefinedType(name.to_string()),
                        name.text_range(),
                    );
                    self.unknown_type
                }
            }
        };

        for name in idents {
            ty = self.path_into_type(ty, name.text(), name.text_range());
        }

        ty
    }

    fn path_into_type(&mut self, ty: TypeId, name: &str, range: TextRange) -> TypeId {
        match self.db.ty(ty) {
            Type::Enum { variants } => {
                if let Some(&variant_type) = variants.get(name) {
                    return variant_type;
                }
                self.error(DiagnosticInfo::UnknownEnumVariant(name.to_string()), range);
                self.unknown_type
            }
            Type::Alias(alias) => self.path_into_type(*alias, name, range),
            _ => {
                self.error(DiagnosticInfo::PathIntoNonEnum(self.type_name(ty)), range);
                self.unknown_type
            }
        }
    }

    fn compile_list_type(&mut self, list: ListType) -> TypeId {
        let Some(inner) = list.ty() else {
            return self.unknown_type;
        };

        let item_type = self.compile_type(inner);
        self.db.alloc_type(Type::List(item_type))
    }

    fn compile_tuple_type(&mut self, tuple_type: TupleType) -> TypeId {
        let items = tuple_type
            .items()
            .into_iter()
            .map(|ty| self.compile_type(ty))
            .collect();

        self.db.alloc_type(Type::Tuple(items))
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

    fn extract_function_type(&self, type_id: TypeId) -> Option<(Vec<TypeId>, TypeId)> {
        match self.db.ty(type_id) {
            Type::Function {
                parameter_types,
                return_type,
            } => Some((parameter_types.clone(), *return_type)),
            Type::Alias(alias_type_id) => self.extract_function_type(*alias_type_id),
            _ => None,
        }
    }

    fn extract_tuple_types(&self, type_id: TypeId) -> Option<Vec<TypeId>> {
        match self.db.ty(type_id) {
            Type::Tuple(items) => Some(items.clone()),
            Type::Alias(alias_type_id) => self.extract_tuple_types(*alias_type_id),
            _ => None,
        }
    }

    fn extract_list_type(&self, type_id: TypeId) -> Option<TypeId> {
        match self.db.ty(type_id) {
            Type::List(inner) => Some(*inner),
            Type::Alias(alias_type_id) => self.extract_list_type(*alias_type_id),
            _ => None,
        }
    }

    fn detect_cycle(
        &mut self,
        ty: TypeId,
        text_range: TextRange,
        visited_aliases: &mut HashSet<TypeId>,
    ) -> bool {
        match self.db.ty(ty).clone() {
            Type::Union(items) => {
                for &item in items.iter() {
                    if self.detect_cycle(item, text_range, visited_aliases) {
                        return true;
                    }
                }
                false
            }
            Type::List(..) => false,
            Type::Tuple(items) => {
                if items.len() != 1 {
                    return false;
                }
                self.detect_cycle(items[0], text_range, visited_aliases)
            }
            Type::Struct { .. } => false,
            Type::Enum { .. } => false,
            Type::EnumVariant { .. } => false,
            Type::Function { .. } => false,
            Type::Alias(alias) => {
                if !visited_aliases.insert(alias) {
                    self.error(DiagnosticInfo::RecursiveTypeAlias, text_range);
                    return true;
                }
                self.detect_cycle(alias, text_range, visited_aliases)
            }
            Type::Unknown | Type::Int | Type::Bool | Type::Bytes => false,
        }
    }

    fn type_name(&self, ty: TypeId) -> String {
        self.type_name_visitor(ty, &mut IndexSet::new())
    }

    fn type_name_visitor(&self, ty: TypeId, stack: &mut IndexSet<TypeId>) -> String {
        for &scope_id in self.scope_stack.iter().rev() {
            if let Some(name) = self.db.scope(scope_id).type_name(ty) {
                return name.to_string();
            }
        }

        if stack.contains(&ty) {
            return "<recursive>".to_string();
        }

        stack.insert(ty);

        let name = match self.db.ty(ty) {
            Type::Unknown => "{unknown}".to_string(),
            Type::Int => "Int".to_string(),
            Type::Bool => "Bool".to_string(),
            Type::Bytes => "Bytes".to_string(),
            Type::Union(items) => {
                let item_names: Vec<String> = items
                    .iter()
                    .map(|&ty| self.type_name_visitor(ty, stack))
                    .collect();

                item_names.join(" | ")
            }
            Type::List(items) => {
                let inner = self.type_name_visitor(*items, stack);
                format!("{}[]", inner)
            }
            Type::Tuple(items) => {
                let item_names: Vec<String> = items
                    .iter()
                    .map(|&ty| self.type_name_visitor(ty, stack))
                    .collect();

                format!("({})", item_names.join(", "))
            }
            Type::Struct { fields } => {
                let fields: Vec<String> = fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, self.type_name_visitor(*ty, stack)))
                    .collect();

                format!("{{ {} }}", fields.join(", "))
            }
            Type::Enum { .. } => "<unnamed enum>".to_string(),
            Type::EnumVariant {
                name,
                enum_type,
                fields,
                ..
            } => {
                let enum_name = self.type_name_visitor(*enum_type, stack);

                let fields: Vec<String> = fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, self.type_name_visitor(*ty, stack)))
                    .collect();

                format!("{}::{} {{ {} }}", enum_name, name, fields.join(", "))
            }
            Type::Function {
                parameter_types,
                return_type,
            } => {
                let params: Vec<String> = parameter_types
                    .iter()
                    .map(|&ty| self.type_name_visitor(ty, stack))
                    .collect();

                let ret = self.type_name_visitor(*return_type, stack);

                format!("fun({}) -> {}", params.join(", "), ret)
            }
            Type::Alias(type_id) => self.type_name_visitor(*type_id, stack),
        };

        stack.pop().unwrap();

        name
    }

    fn type_check(&mut self, from: TypeId, to: TypeId, range: TextRange) {
        if !self.is_assignable_to(from, to, &mut HashSet::new()) {
            self.error(
                DiagnosticInfo::TypeMismatch {
                    expected: self.type_name(to),
                    found: self.type_name(from),
                },
                range,
            );
        }
    }

    fn is_assignable_to(
        &self,
        a: TypeId,
        b: TypeId,
        visited: &mut HashSet<(TypeId, TypeId)>,
    ) -> bool {
        let key = (a, b);

        if a == b || visited.contains(&key) {
            return true;
        }
        visited.insert(key);

        match (self.db.ty(a).clone(), self.db.ty(b).clone()) {
            // Primitive types.
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            (Type::Int, Type::Int) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Bytes, Type::Bytes) => true,

            // Reduce the type alias on left hand side.
            (Type::Alias(alias), _) => self.is_assignable_to(alias, b, visited),

            // Reduce the type alias on right hand side.
            (_, Type::Alias(alias)) => self.is_assignable_to(a, alias, visited),

            // Anything can be assigned to a union if it can be assigned to any of its items.
            (_, Type::Union(items)) => {
                for &item in items.iter() {
                    if self.is_assignable_to(a, item, visited) {
                        return true;
                    }
                }
                false
            }

            // Unions can be assigned to anything if all of its items can be assigned to it.
            (Type::Union(items), _) => items
                .iter()
                .all(|&item| self.is_assignable_to(item, b, visited)),

            // List types with compatible items are also assignable.
            (Type::List(a), Type::List(b)) => self.is_assignable_to(a, b, visited),

            (Type::Tuple(a), Type::Tuple(b)) => {
                if a.len() != b.len() {
                    return false;
                }

                for (a, b) in a.iter().zip(b.iter()) {
                    if !self.is_assignable_to(*a, *b, visited) {
                        return false;
                    }
                }

                true
            }

            // Enum variants are assignable to their enum type.
            (Type::EnumVariant { enum_type, .. }, _) if b == enum_type => true,

            // Functions with compatible parameters and return type.
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
                    if !self.is_assignable_to(a, b, visited) {
                        return false;
                    }
                }

                self.is_assignable_to(ret_a, ret_b, visited)
            }

            _ => false,
        }
    }

    fn unknown(&self) -> Value {
        Value::typed(self.unknown_hir, self.unknown_type)
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
