use std::{
    collections::{HashMap, HashSet},
    fmt,
    str::FromStr,
};

use indexmap::{IndexMap, IndexSet};
use num_bigint::BigInt;
use rowan::TextRange;
use rue_parser::{
    AstNode, BinaryExpr, BinaryOp, Block, CastExpr, ConstItem, EnumItem, Expr, FieldAccess,
    FunctionCall, FunctionItem, FunctionType as AstFunctionType, GroupExpr, GuardExpr, IfExpr,
    IndexAccess, InitializerExpr, InitializerField, Item, LambdaExpr, LetStmt, ListExpr, ListType,
    LiteralExpr, PairExpr, PairType, Path, PrefixExpr, PrefixOp, Root, Stmt, StructField,
    StructItem, SyntaxKind, SyntaxToken, Type as AstType, TypeAliasItem,
};

use crate::{
    database::{Database, HirId, ScopeId, SymbolId, TypeId},
    hir::{Hir, HirBinaryOp},
    scope::Scope,
    symbol::Symbol,
    ty::{EnumType, EnumVariant, FunctionType, Guard, StructType, Type, Value},
    Diagnostic, DiagnosticInfo, DiagnosticKind,
};

pub struct Lowerer<'a> {
    db: &'a mut Database,
    scope_stack: Vec<ScopeId>,
    type_guards: Vec<HashMap<SymbolId, TypeId>>,
    diagnostics: Vec<Diagnostic>,
    any_type: TypeId,
    int_type: TypeId,
    bool_type: TypeId,
    bytes_type: TypeId,
    bytes32_type: TypeId,
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
        let bytes32_type = db.alloc_type(Type::Bytes32);
        let any_type = db.alloc_type(Type::Any);
        let nil_type = db.alloc_type(Type::Nil);
        let nil_hir = db.alloc_hir(Hir::Atom(Vec::new()));
        let unknown_type = db.alloc_type(Type::Unknown);
        let unknown_hir = db.alloc_hir(Hir::Unknown);

        let mut builtins = Scope::default();
        builtins.define_type("Nil".to_string(), nil_type);
        builtins.define_type("Int".to_string(), int_type);
        builtins.define_type("Bool".to_string(), bool_type);
        builtins.define_type("Bytes".to_string(), bytes_type);
        builtins.define_type("Bytes32".to_string(), bytes32_type);
        builtins.define_type("Any".to_string(), any_type);

        {
            let mut scope = Scope::default();
            let param = db.alloc_symbol(Symbol::Parameter {
                type_id: bytes_type,
            });
            scope.define_symbol("bytes".to_string(), param);
            let param_ref = db.alloc_hir(Hir::Reference(param));
            let hir_id = db.alloc_hir(Hir::Sha256(param_ref));
            let scope_id = db.alloc_scope(scope);

            builtins.define_symbol(
                "sha256".to_string(),
                db.alloc_symbol(Symbol::Function {
                    scope_id,
                    hir_id,
                    ty: FunctionType::new(vec![bytes_type], bytes32_type, false),
                }),
            );
        }

        let builtins_id = db.alloc_scope(builtins);

        Self {
            db,
            scope_stack: vec![builtins_id],
            type_guards: Vec::new(),
            diagnostics: Vec::new(),
            any_type,
            int_type,
            bool_type,
            bytes_type,
            bytes32_type,
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
        let mut symbol_ids = Vec::new();

        for item in items.clone() {
            match item {
                Item::TypeAliasItem(ty) => type_ids.push(self.declare_type_alias(ty)),
                Item::StructItem(struct_item) => type_ids.push(self.declare_struct(struct_item)),
                Item::EnumItem(enum_item) => type_ids.push(self.declare_enum(enum_item)),
                _ => {}
            }
        }

        for item in items.clone() {
            match item {
                Item::FunctionItem(function) => symbol_ids.push(self.declare_function(function)),
                Item::ConstItem(const_item) => symbol_ids.push(self.declare_const(const_item)),
                _ => {}
            }
        }

        for item in items.clone() {
            match item {
                Item::TypeAliasItem(ty) => {
                    self.compile_type_alias(ty, type_ids.remove(0));
                }
                Item::StructItem(struct_item) => {
                    self.compile_struct(struct_item, type_ids.remove(0));
                }
                Item::EnumItem(enum_item) => {
                    self.compile_enum(enum_item, type_ids.remove(0));
                }
                _ => {}
            }
        }

        for item in items {
            match item {
                Item::FunctionItem(function) => {
                    self.compile_function(function, symbol_ids.remove(0));
                }
                Item::ConstItem(const_item) => {
                    self.compile_const(const_item, symbol_ids.remove(0));
                }
                _ => {}
            }
        }
    }

    fn declare_function(&mut self, function_item: FunctionItem) -> SymbolId {
        let mut scope = Scope::default();

        let return_type = function_item
            .return_type()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.unknown_type);

        let mut parameter_types = Vec::new();
        let mut varargs = false;
        let len = function_item.params().len();

        for (i, param) in function_item.params().into_iter().enumerate() {
            let type_id = param
                .ty()
                .map(|ty| self.compile_type(ty))
                .unwrap_or(self.unknown_type);

            parameter_types.push(type_id);

            let symbol_id = self.db.alloc_symbol(Symbol::Parameter { type_id });

            if let Some(name) = param.name() {
                scope.define_symbol(name.to_string(), symbol_id);
            }

            if param.spread().is_some() {
                if i + 1 == len {
                    varargs = true;
                } else {
                    self.error(DiagnosticInfo::NonFinalSpread, param.syntax().text_range());
                }
            }
        }

        let scope_id = self.db.alloc_scope(scope);
        let hir_id = self.db.alloc_hir(Hir::Unknown);

        let ty = FunctionType::new(parameter_types, return_type, varargs);

        let symbol_id = self.db.alloc_symbol(Symbol::Function {
            scope_id,
            hir_id,
            ty,
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

        let type_id = self.db.alloc_type(Type::Enum(EnumType::new(variants)));

        if let Some(name) = enum_item.name() {
            self.scope_mut().define_type(name.to_string(), type_id);
        }

        type_id
    }

    fn compile_function(&mut self, function: FunctionItem, symbol_id: SymbolId) {
        let Some(body) = function.body() else {
            return;
        };

        let Symbol::Function { scope_id, ty, .. } = self.db.symbol(symbol_id).clone() else {
            unreachable!();
        };

        self.scope_stack.push(scope_id);
        let (output, _explicit_return) =
            self.compile_block_expr(body, None, Some(ty.return_type()));
        self.scope_stack.pop().unwrap();

        self.type_check(
            output.ty(),
            ty.return_type(),
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
        *self.db.ty_mut(type_id) = Type::Struct(StructType::new(fields));
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
        let Type::Enum(enum_type) = self.db.ty(type_id).clone() else {
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

            let variant_type = enum_type.variants()[name.text()];

            let fields = self.compile_struct_fields(variant.fields());

            let discriminant = variant
                .discriminant()
                .map(|discriminant| self.compile_int(discriminant).hir())
                .unwrap_or(self.unknown_hir);

            *self.db.ty_mut(variant_type) = Type::EnumVariant(EnumVariant::new(
                name.to_string(),
                type_id,
                fields,
                discriminant,
            ));
        }
    }

    fn compile_let_stmt(&mut self, let_stmt: LetStmt) -> Option<ScopeId> {
        let expected_type = let_stmt.ty().map(|ty| self.compile_type(ty));

        let value = let_stmt
            .expr()
            .map(|expr| self.compile_expr(expr, expected_type))
            .unwrap_or(self.unknown());

        if let Some(expected_type) = expected_type {
            self.type_check(value.ty(), expected_type, let_stmt.syntax().text_range());
        }

        let Some(name) = let_stmt.name() else {
            return None;
        };

        let symbol_id = self.db.alloc_symbol(Symbol::LetBinding {
            type_id: expected_type.unwrap_or(value.ty()),
            hir_id: value.hir(),
        });

        let mut let_scope = Scope::default();
        let_scope.define_symbol(name.to_string(), symbol_id);
        let scope_id = self.db.alloc_scope(let_scope);
        self.scope_stack.push(scope_id);

        Some(scope_id)
    }

    fn compile_block_expr(
        &mut self,
        block: Block,
        scope_id: Option<ScopeId>,
        expected_type: Option<TypeId>,
    ) -> (Value, bool) {
        if let Some(scope_id) = scope_id {
            self.scope_stack.push(scope_id);
        }

        self.compile_items(block.items());

        enum Statement {
            Let(ScopeId),
            If(HirId, HirId),
            Return(Value),
        }

        let mut statements = Vec::new();
        let mut explicit_return = false;

        for stmt in block.stmts() {
            match stmt {
                Stmt::LetStmt(let_stmt) => {
                    let Some(scope_id) = self.compile_let_stmt(let_stmt) else {
                        continue;
                    };
                    statements.push(Statement::Let(scope_id));
                }
                Stmt::IfStmt(if_stmt) => {
                    let condition = if_stmt
                        .condition()
                        .map(|condition| self.compile_expr(condition, Some(self.bool_type)));

                    if let Some(condition) = condition.as_ref() {
                        self.type_guards.push(condition.then_guards());
                    }

                    let then_block = if_stmt
                        .then_block()
                        .map(|then_block| {
                            let scope_id = self.db.alloc_scope(Scope::default());
                            let (value, explicit_return) = self.compile_block_expr(
                                then_block.clone(),
                                Some(scope_id),
                                expected_type,
                            );
                            if !explicit_return {
                                self.error(
                                    DiagnosticInfo::ImplicitReturnInIf,
                                    then_block.syntax().text_range(),
                                );
                            }
                            value
                        })
                        .unwrap_or_else(|| self.unknown());

                    if condition.is_some() {
                        self.type_guards.pop().unwrap();
                    }

                    self.type_check(
                        then_block.ty(),
                        expected_type.unwrap_or(self.unknown_type),
                        block.syntax().text_range(),
                    );

                    let else_guards = condition
                        .as_ref()
                        .map(|condition| condition.else_guards())
                        .unwrap_or_default();

                    self.type_guards.push(else_guards);

                    statements.push(Statement::If(
                        condition
                            .map(|condition| condition.hir())
                            .unwrap_or(self.unknown_hir),
                        then_block.hir(),
                    ));
                }
                Stmt::ReturnStmt(return_stmt) => {
                    let value = return_stmt
                        .expr()
                        .map(|expr| self.compile_expr(expr, expected_type))
                        .unwrap_or_else(|| self.unknown());

                    explicit_return = true;

                    statements.push(Statement::Return(value));
                }
                Stmt::RaiseStmt(raise_stmt) => {
                    let value = raise_stmt
                        .expr()
                        .map(|expr| self.compile_expr(expr, Some(self.bytes_type)).hir());

                    let value = self.db.alloc_hir(Hir::Raise(value));

                    statements.push(Statement::Return(Value::typed(value, self.unknown_type)));
                }
                Stmt::AssertStmt(assert_stmt) => {
                    let condition = assert_stmt
                        .expr()
                        .map(|condition| self.compile_expr(condition, Some(self.bool_type)))
                        .unwrap_or_else(|| self.unknown());

                    self.type_guards.push(condition.then_guards());

                    self.type_check(
                        condition.ty(),
                        self.bool_type,
                        assert_stmt.syntax().text_range(),
                    );

                    let not_condition = self.db.alloc_hir(Hir::Not(condition.hir()));
                    let raise = self.db.alloc_hir(Hir::Raise(None));

                    statements.push(Statement::If(not_condition, raise))
                }
            }
        }

        let mut body = block
            .expr()
            .map(|expr| self.compile_expr(expr, expected_type))
            .unwrap_or(self.unknown());

        for statement in statements.into_iter().rev() {
            match statement {
                Statement::Let(scope_id) => {
                    body = Value::typed(
                        self.db.alloc_hir(Hir::Scope {
                            scope_id,
                            value: body.hir(),
                        }),
                        body.ty(),
                    );
                    self.scope_stack.pop().unwrap();
                }
                Statement::Return(value) => {
                    body = value;
                }
                Statement::If(condition, then_block) => {
                    self.type_guards.pop().unwrap();

                    body = Value::typed(
                        self.db.alloc_hir(Hir::If {
                            condition,
                            then_block,
                            else_block: body.hir(),
                        }),
                        body.ty(),
                    );
                }
            }
        }

        if scope_id.is_some() {
            self.scope_stack.pop().unwrap();
        }

        (body, explicit_return)
    }

    fn compile_expr(&mut self, expr: Expr, expected_type: Option<TypeId>) -> Value {
        match expr {
            Expr::Path(path) => self.compile_path_expr(path),
            Expr::InitializerExpr(initializer) => self.compile_initializer_expr(initializer),
            Expr::LiteralExpr(literal) => self.compile_literal_expr(literal),
            Expr::ListExpr(list) => self.compile_list_expr(list, expected_type),
            Expr::PairExpr(pair) => self.compile_pair_expr(pair, expected_type),
            Expr::Block(block) => {
                let scope_id = self.db.alloc_scope(Scope::default());
                let (value, explicit_return) =
                    self.compile_block_expr(block.clone(), Some(scope_id), expected_type);
                if explicit_return {
                    self.error(
                        DiagnosticInfo::ExplicitReturnInExpr,
                        block.syntax().text_range(),
                    );
                }
                value
            }
            Expr::LambdaExpr(lambda) => self.compile_lambda_expr(lambda, expected_type),
            Expr::PrefixExpr(prefix) => self.compile_prefix_expr(prefix),
            Expr::BinaryExpr(binary) => self.compile_binary_expr(binary),
            Expr::GroupExpr(expr) => self.compile_group_expr(expr, expected_type),
            Expr::CastExpr(cast) => self.compile_cast_expr(cast, expected_type),
            Expr::GuardExpr(guard) => self.compile_guard_expr(guard, expected_type),
            Expr::IfExpr(if_expr) => self.compile_if_expr(if_expr, expected_type),
            Expr::FunctionCall(call) => self.compile_function_call(call),
            Expr::FieldAccess(field_access) => self.compile_field_access(field_access),
            Expr::IndexAccess(index_access) => self.compile_index_access(index_access),
        }
    }

    fn compile_initializer_expr(&mut self, initializer: InitializerExpr) -> Value {
        let ty = initializer.path().map(|path| self.compile_path_type(path));

        match ty.map(|ty| self.db.ty(ty)).cloned() {
            Some(Type::Struct(struct_type)) => {
                let hir_id = self.compile_initializer_fields(
                    struct_type.fields(),
                    initializer.fields(),
                    initializer.syntax().text_range(),
                );

                match ty {
                    Some(struct_type) => Value::typed(hir_id, struct_type),
                    None => self.unknown(),
                }
            }
            Some(Type::EnumVariant(enum_variant)) => {
                let fields_hir_id = self.compile_initializer_fields(
                    enum_variant.fields(),
                    initializer.fields(),
                    initializer.syntax().text_range(),
                );

                let hir_id = self
                    .db
                    .alloc_hir(Hir::Pair(enum_variant.discriminant(), fields_hir_id));

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
        struct_fields: &IndexMap<String, TypeId>,
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

        let Some(field_name) = field_access.field() else {
            return self.unknown();
        };

        match self.db.ty(value.ty()).clone() {
            Type::Struct(struct_type) => {
                if let Some((index, _name, &field_type)) =
                    struct_type.fields().get_full(field_name.text())
                {
                    Value::typed(self.compile_index(value.hir(), index, false), field_type)
                } else {
                    self.error(
                        DiagnosticInfo::UndefinedField(field_name.to_string()),
                        field_name.text_range(),
                    );
                    self.unknown()
                }
            }
            Type::Pair(left, right) => match field_name.text() {
                "first" => Value::typed(self.db.alloc_hir(Hir::First(value.hir())), left),
                "rest" => Value::typed(self.db.alloc_hir(Hir::Rest(value.hir())), right),
                _ => {
                    self.error(
                        DiagnosticInfo::PairFieldAccess(field_name.to_string()),
                        field_name.text_range(),
                    );
                    self.unknown()
                }
            },
            _ => {
                self.error(
                    DiagnosticInfo::StructFieldAccess(self.type_name(value.ty())),
                    field_name.text_range(),
                );
                self.unknown()
            }
        }
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

        let Type::List(item_type) = self.db.ty(value.ty()).clone() else {
            self.error(
                DiagnosticInfo::IndexAccess(self.type_name(value.ty())),
                index_access.expr().unwrap().syntax().text_range(),
            );
            return self.unknown();
        };

        Value::typed(self.compile_index(value.hir(), index, false), item_type)
    }

    fn compile_index(&mut self, value: HirId, index: usize, rest: bool) -> HirId {
        let mut result = value;
        for _ in 0..index {
            result = self.db.alloc_hir(Hir::Rest(result));
        }
        if !rest {
            result = self.db.alloc_hir(Hir::First(result));
        }
        result
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

        let lhs_ty = lhs
            .as_ref()
            .map(|lhs| lhs.ty())
            .unwrap_or(self.unknown_type);

        let rhs_ty = rhs
            .as_ref()
            .map(|rhs| rhs.ty())
            .unwrap_or(self.unknown_type);

        let mut op = binary.op().map(HirBinaryOp::from);

        let ty = if binary.op() == Some(BinaryOp::Add)
            && self.is_assignable_to(lhs_ty, self.bytes_type, false, &mut HashSet::new())
        {
            self.type_check(rhs_ty, self.bytes_type, binary.syntax().text_range());

            op = Some(HirBinaryOp::Concat);

            Some(self.bytes_type)
        } else {
            self.type_check(lhs_ty, self.int_type, binary.syntax().text_range());

            self.type_check(rhs_ty, self.int_type, binary.syntax().text_range());

            None
        };

        let ty = ty.unwrap_or_else(|| {
            binary
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
                .unwrap_or(self.unknown_type)
        });

        match (lhs, rhs, op) {
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

    fn compile_group_expr(
        &mut self,
        group_expr: GroupExpr,
        expected_type: Option<TypeId>,
    ) -> Value {
        let Some(expr) = group_expr
            .expr()
            .map(|expr| self.compile_expr(expr, expected_type))
        else {
            return self.unknown();
        };

        expr
    }

    fn compile_cast_expr(&mut self, cast: CastExpr, expected_type: Option<TypeId>) -> Value {
        let Some(expr) = cast
            .expr()
            .map(|expr| self.compile_expr(expr, expected_type))
        else {
            return self.unknown();
        };

        let ty = cast
            .ty()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.unknown_type);

        self.cast_check(expr.ty(), ty, cast.expr().unwrap().syntax().text_range());

        Value::typed(expr.hir(), ty)
    }

    fn compile_guard_expr(&mut self, guard: GuardExpr, expected_type: Option<TypeId>) -> Value {
        let Some(expr) = guard
            .expr()
            .map(|expr| self.compile_expr(expr, expected_type))
        else {
            return self.unknown();
        };

        let ty = guard
            .ty()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.unknown_type);

        let Some((guard, hir_id)) =
            self.guard_into(expr.ty(), ty, expr.hir(), guard.syntax().text_range())
        else {
            return Value::typed(self.unknown_hir, ty);
        };

        let mut value = Value::typed(hir_id, self.bool_type);

        if let Hir::Reference(symbol_id) = self.db.hir(expr.hir()) {
            value.guard(*symbol_id, guard);
        }

        value
    }

    fn guard_into(
        &mut self,
        from: TypeId,
        to: TypeId,
        hir_id: HirId,
        text_range: TextRange,
    ) -> Option<(Guard, HirId)> {
        if self.types_equal(from, to) {
            self.warning(
                DiagnosticInfo::RedundantTypeGuard(self.type_name(from)),
                text_range,
            );
            return Some((Guard::new(to, self.bool_type), hir_id));
        }

        match (self.db.ty(from).clone(), self.db.ty(to).clone()) {
            (Type::Any, Type::Pair(first, rest)) => {
                if !self.types_equal(first, self.any_type) {
                    self.error(DiagnosticInfo::NonAnyPairTypeGuard, text_range);
                }

                if !self.types_equal(rest, self.any_type) {
                    self.error(DiagnosticInfo::NonAnyPairTypeGuard, text_range);
                }

                let hir_id = self.db.alloc_hir(Hir::IsCons(hir_id));
                Some((Guard::new(to, self.bytes_type), hir_id))
            }
            (Type::Any, Type::Bytes) => {
                let pair_type = self.db.alloc_type(Type::Pair(self.any_type, self.any_type));
                let is_cons = self.db.alloc_hir(Hir::IsCons(hir_id));
                let hir_id = self.db.alloc_hir(Hir::Not(is_cons));
                Some((Guard::new(to, pair_type), hir_id))
            }
            (Type::List(inner), Type::Pair(first, rest)) => {
                if !self.types_equal(first, inner) {
                    self.error(DiagnosticInfo::NonListPairTypeGuard, text_range);
                }

                if !self.types_equal(rest, from) {
                    self.error(DiagnosticInfo::NonListPairTypeGuard, text_range);
                }

                let hir_id = self.db.alloc_hir(Hir::IsCons(hir_id));
                Some((Guard::new(to, self.nil_type), hir_id))
            }
            (Type::List(inner), Type::Nil) => {
                let pair_type = self.db.alloc_type(Type::Pair(inner, from));
                let is_cons = self.db.alloc_hir(Hir::IsCons(hir_id));
                let hir_id = self.db.alloc_hir(Hir::Not(is_cons));
                Some((Guard::new(to, pair_type), hir_id))
            }
            (Type::Bytes, Type::Bytes32) => {
                let strlen = self.db.alloc_hir(Hir::Strlen(hir_id));
                let length = self.db.alloc_hir(Hir::Atom(vec![32]));
                let hir_id = self.db.alloc_hir(Hir::BinaryOp {
                    op: HirBinaryOp::Equals,
                    lhs: strlen,
                    rhs: length,
                });
                Some((Guard::new(to, from), hir_id))
            }
            _ => {
                self.error(
                    DiagnosticInfo::UnsupportedTypeGuard {
                        from: self.type_name(from),
                        to: self.type_name(to),
                    },
                    text_range,
                );
                None
            }
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
        let mut item_type = expected_expr_type.and_then(|ty| match self.db.ty(ty) {
            Type::List(ty) => Some(*ty),
            _ => None,
        });

        let len = list_expr.items().len();

        for (i, item) in list_expr.items().into_iter().enumerate() {
            let expected_item_type = if item.spread().is_some() {
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
                if item.spread().is_some() {
                    list_type = Some(output.ty());
                    item_type = match self.db.ty(output.ty()) {
                        Type::List(ty) => Some(*ty),
                        _ => None,
                    };
                } else {
                    list_type = Some(self.db.alloc_type(Type::List(output.ty())));
                    item_type = Some(output.ty());
                }
            }

            if let Some(spread) = item.spread() {
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

    fn compile_pair_expr(&mut self, pair_expr: PairExpr, expected_type: Option<TypeId>) -> Value {
        let expected_first = expected_type.and_then(|ty| match self.db.ty(ty) {
            Type::Pair(first, _) => Some(*first),
            _ => None,
        });

        let expected_rest = expected_type.and_then(|ty| match self.db.ty(ty) {
            Type::Pair(_, rest) => Some(*rest),
            _ => None,
        });

        let first = if let Some(first) = pair_expr.first() {
            let value = self.compile_expr(first.clone(), expected_first);
            self.type_check(
                value.ty(),
                expected_first.unwrap_or(self.unknown_type),
                first.syntax().text_range(),
            );
            value
        } else {
            self.unknown()
        };

        let rest = if let Some(rest) = pair_expr.rest() {
            let value = self.compile_expr(rest.clone(), expected_rest);
            self.type_check(
                value.ty(),
                expected_rest.unwrap_or(self.unknown_type),
                rest.syntax().text_range(),
            );
            value
        } else {
            self.unknown()
        };

        let hir_id = self.db.alloc_hir(Hir::Pair(first.hir(), rest.hir()));
        let type_id = self.db.alloc_type(Type::Pair(first.ty(), rest.ty()));

        Value::typed(hir_id, type_id)
    }

    fn compile_lambda_expr(
        &mut self,
        lambda_expr: LambdaExpr,
        expected_type: Option<TypeId>,
    ) -> Value {
        let expected = expected_type.and_then(|ty| match self.db.ty(ty) {
            Type::Function(function) => Some(function.clone()),
            _ => None,
        });

        let mut scope = Scope::default();
        let mut parameter_types = Vec::new();
        let mut varargs = false;

        let len = lambda_expr.params().len();

        for (i, param) in lambda_expr.params().into_iter().enumerate() {
            let type_id = param
                .ty()
                .map(|ty| self.compile_type(ty))
                .or(expected
                    .as_ref()
                    .and_then(|expected| expected.parameter_types().get(i).copied()))
                .unwrap_or(self.unknown_type);

            parameter_types.push(type_id);

            if let Some(name) = param.name() {
                let symbol_id = self.db.alloc_symbol(Symbol::Parameter { type_id });
                scope.define_symbol(name.to_string(), symbol_id);
            };

            if param.spread().is_some() {
                if i + 1 == len {
                    varargs = true;
                } else {
                    self.error(DiagnosticInfo::NonFinalSpread, param.syntax().text_range());
                }
            }
        }

        let scope_id = self.db.alloc_scope(scope);

        let Some(body) = lambda_expr.body() else {
            return self.unknown();
        };

        let expected_return_type = lambda_expr
            .ty()
            .map(|ty| self.compile_type(ty))
            .or(expected.map(|expected| expected.return_type()));

        self.scope_stack.push(scope_id);
        let body = self.compile_expr(body, expected_return_type);
        self.scope_stack.pop().expect("lambda not in scope stack");

        let return_type = expected_return_type.unwrap_or(body.ty());

        self.type_check(
            body.ty(),
            return_type,
            lambda_expr.body().unwrap().syntax().text_range(),
        );

        let ty = FunctionType::new(parameter_types.clone(), return_type, varargs);

        let symbol_id = self.db.alloc_symbol(Symbol::Function {
            scope_id,
            hir_id: body.hir(),
            ty: ty.clone(),
        });

        Value::typed(
            self.db.alloc_hir(Hir::Reference(symbol_id)),
            self.db.alloc_type(Type::Function(ty)),
        )
    }

    fn compile_if_expr(&mut self, if_expr: IfExpr, expected_type: Option<TypeId>) -> Value {
        let condition = if_expr
            .condition()
            .map(|condition| self.compile_expr(condition, Some(self.bool_type)));

        if let Some(condition) = condition.as_ref() {
            self.type_guards.push(condition.then_guards());
        }

        let then_block = if_expr.then_block().map(|then_block| {
            let scope_id = self.db.alloc_scope(Scope::default());
            self.compile_block_expr(then_block, Some(scope_id), expected_type)
                .0
        });

        if condition.is_some() {
            self.type_guards.pop().unwrap();
        }

        if let Some(condition) = condition.as_ref() {
            self.type_guards.push(condition.else_guards());
        }

        let expected_type =
            expected_type.or_else(|| then_block.as_ref().map(|then_block| then_block.ty()));

        let else_block = if_expr.else_block().map(|else_block| {
            let scope_id = self.db.alloc_scope(Scope::default());
            self.compile_block_expr(else_block, Some(scope_id), expected_type)
                .0
        });

        if condition.is_some() {
            self.type_guards.pop().unwrap();
        }

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

        let bytes = before_suffix.as_bytes();

        Value::typed(
            self.db.alloc_hir(Hir::Atom(bytes.to_vec())),
            if bytes.len() == 32 {
                self.bytes32_type
            } else {
                self.bytes_type
            },
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
            self.symbol_type(symbol_id)
                .unwrap_or_else(|| match self.db.symbol(symbol_id) {
                    Symbol::Function { ty, .. } => self.db.alloc_type(Type::Function(ty.clone())),
                    Symbol::Parameter { type_id } => *type_id,
                    Symbol::LetBinding { type_id, .. } => *type_id,
                    Symbol::ConstBinding { type_id, .. } => *type_id, //todo
                }),
        )
    }

    fn expected_param_type(
        &self,
        function_type: FunctionType,
        index: usize,
        spread: bool,
    ) -> Option<TypeId> {
        let params = function_type.parameter_types();
        let len = params.len();

        if index + 1 < len {
            return Some(params[index]);
        }

        if !function_type.varargs() {
            if index + 1 == len {
                return Some(params[index]);
            } else {
                return None;
            }
        }

        if spread {
            return Some(params[len - 1]);
        }

        match self.db.ty(params[len - 1]) {
            Type::List(list_type) => Some(*list_type),
            _ => None,
        }
    }

    fn compile_function_call(&mut self, call: FunctionCall) -> Value {
        let Some(callee) = call.callee() else {
            return self.unknown();
        };

        let callee = self.compile_expr(callee, None);
        let expected = match self.db.ty(callee.ty()) {
            Type::Function(function) => Some(function.clone()),
            _ => {
                self.error(
                    DiagnosticInfo::UncallableType(self.type_name(callee.ty())),
                    call.callee().unwrap().syntax().text_range(),
                );
                None
            }
        };

        let mut args = self.nil_hir;
        let mut arg_types = Vec::new();
        let mut spread = false;

        let arg_len = call.args().len();

        for (i, arg) in call.args().into_iter().enumerate().rev() {
            let expected_type = expected.as_ref().and_then(|expected| {
                self.expected_param_type(
                    expected.clone(),
                    i,
                    i + 1 == arg_len && arg.spread().is_some(),
                )
            });

            let value = arg
                .expr()
                .map(|expr| self.compile_expr(expr, expected_type))
                .unwrap_or_else(|| self.unknown());

            arg_types.push(value.ty());

            if arg.spread().is_some() {
                if i + 1 == arg_len {
                    args = value.hir();
                    spread = true;
                    continue;
                } else {
                    self.error(DiagnosticInfo::NonFinalSpread, arg.syntax().text_range());
                }
            }

            args = self.db.alloc_hir(Hir::Pair(value.hir(), args));
        }

        arg_types.reverse();

        if let Some(expected) = expected.as_ref() {
            let param_len = expected.parameter_types().len();

            let too_few_args = arg_types.len() < param_len;
            let too_many_args = arg_types.len() > param_len && !expected.varargs();

            if too_few_args || too_many_args {
                self.error(
                    DiagnosticInfo::ArgumentMismatch {
                        expected: param_len,
                        found: arg_types.len(),
                    },
                    call.syntax().text_range(),
                );
            }

            for (i, arg) in arg_types.into_iter().enumerate() {
                if i + 1 == arg_len && spread && !expected.varargs() {
                    self.error(
                        DiagnosticInfo::NonVarargSpread,
                        call.args()[i].syntax().text_range(),
                    );
                    continue;
                }

                if i + 1 >= param_len && i + 1 < arg_len && expected.varargs() {
                    match self
                        .db
                        .ty(expected.parameter_types().last().copied().unwrap())
                    {
                        Type::List(list_type) => {
                            self.type_check(arg, *list_type, call.args()[i].syntax().text_range());
                        }
                        _ => {
                            self.error(
                                DiagnosticInfo::NonListVararg,
                                call.args()[i].syntax().text_range(),
                            );
                        }
                    }
                    continue;
                }

                if i + 1 == arg_len && spread && expected.varargs() {
                    self.type_check(
                        arg,
                        expected.parameter_types()[param_len - 1],
                        call.args()[i].syntax().text_range(),
                    );
                    continue;
                }

                self.type_check(
                    arg,
                    expected
                        .parameter_types()
                        .get(i)
                        .copied()
                        .unwrap_or(self.unknown_type),
                    call.args()[i].syntax().text_range(),
                );
            }
        }

        let hir_id = self.db.alloc_hir(Hir::FunctionCall {
            callee: callee.hir(),
            args,
        });

        let type_id = expected
            .map(|expected| expected.return_type())
            .unwrap_or(self.unknown_type);

        Value::typed(hir_id, type_id)
    }

    fn compile_type(&mut self, ty: AstType) -> TypeId {
        match ty {
            AstType::Path(path) => self.compile_path_type(path),
            AstType::ListType(list) => self.compile_list_type(list),
            AstType::FunctionType(function) => self.compile_function_type(function),
            AstType::PairType(tuple) => self.compile_pair_type(tuple),
        }
    }

    fn compile_path_type(&mut self, path: Path) -> TypeId {
        let mut idents = path.idents();

        let name = idents.remove(0);
        let mut ty = None;

        for &scope_id in self.scope_stack.iter().rev() {
            if let Some(found_type_id) = self.db.scope(scope_id).type_alias(name.text()) {
                ty = Some(found_type_id);
                break;
            }
        }

        let Some(mut ty) = ty else {
            self.error(
                DiagnosticInfo::UndefinedType(name.to_string()),
                name.text_range(),
            );
            return self.unknown_type;
        };

        for name in idents {
            ty = self.path_into_type(ty, name.text(), name.text_range());
        }

        ty
    }

    fn path_into_type(&mut self, ty: TypeId, name: &str, range: TextRange) -> TypeId {
        match self.db.ty(ty) {
            Type::Enum(enum_type) => {
                if let Some(&variant_type) = enum_type.variants().get(name) {
                    return variant_type;
                }
                self.error(DiagnosticInfo::UnknownEnumVariant(name.to_string()), range);
                self.unknown_type
            }
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

    fn compile_pair_type(&mut self, pair_type: PairType) -> TypeId {
        let first = pair_type
            .first()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.unknown_type);

        let rest = pair_type
            .rest()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.unknown_type);

        self.db.alloc_type(Type::Pair(first, rest))
    }

    fn compile_function_type(&mut self, function: AstFunctionType) -> TypeId {
        let mut parameter_types = Vec::new();
        let mut vararg = false;

        let len = function.params().len();

        for (i, param) in function.params().into_iter().enumerate() {
            let type_id = param
                .ty()
                .map(|ty| self.compile_type(ty))
                .unwrap_or(self.unknown_type);

            parameter_types.push(type_id);

            if param.spread().is_some() {
                if i + 1 == len {
                    vararg = true;
                } else {
                    self.error(DiagnosticInfo::NonFinalSpread, param.syntax().text_range());
                }
            }
        }

        let return_type = function
            .ret()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.unknown_type);

        self.db.alloc_type(Type::Function(FunctionType::new(
            parameter_types,
            return_type,
            vararg,
        )))
    }

    fn detect_cycle(
        &mut self,
        ty: TypeId,
        text_range: TextRange,
        visited_aliases: &mut HashSet<TypeId>,
    ) -> bool {
        match self.db.ty_raw(ty).clone() {
            Type::List(..) => false,
            Type::Pair(left, right) => {
                self.detect_cycle(left, text_range, visited_aliases)
                    || self.detect_cycle(right, text_range, visited_aliases)
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
            Type::Unknown
            | Type::Nil
            | Type::Any
            | Type::Int
            | Type::Bool
            | Type::Bytes
            | Type::Bytes32 => false,
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
            Type::Nil => "Nil".to_string(),
            Type::Any => "Any".to_string(),
            Type::Int => "Int".to_string(),
            Type::Bool => "Bool".to_string(),
            Type::Bytes => "Bytes".to_string(),
            Type::Bytes32 => "Bytes32".to_string(),
            Type::List(items) => {
                let inner = self.type_name_visitor(*items, stack);
                format!("{}[]", inner)
            }
            Type::Pair(left, right) => {
                let left = self.type_name_visitor(*left, stack);
                let right = self.type_name_visitor(*right, stack);
                format!("({left}, {right})")
            }
            Type::Struct(struct_type) => {
                let fields: Vec<String> = struct_type
                    .fields()
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, self.type_name_visitor(*ty, stack)))
                    .collect();

                format!("{{ {} }}", fields.join(", "))
            }
            Type::Enum { .. } => "<unnamed enum>".to_string(),
            Type::EnumVariant(enum_variant) => {
                let enum_name = self.type_name_visitor(enum_variant.enum_type(), stack);

                let fields: Vec<String> = enum_variant
                    .fields()
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, self.type_name_visitor(*ty, stack)))
                    .collect();

                format!(
                    "{}::{} {{ {} }}",
                    enum_name,
                    enum_variant.name(),
                    fields.join(", ")
                )
            }
            Type::Function(function_type) => {
                let params: Vec<String> = function_type
                    .parameter_types()
                    .iter()
                    .map(|&ty| self.type_name_visitor(ty, stack))
                    .collect();

                let ret = self.type_name_visitor(function_type.return_type(), stack);

                format!("fun({}) -> {}", params.join(", "), ret)
            }
            Type::Alias(..) => unreachable!(),
        };

        stack.pop().unwrap();

        name
    }

    fn type_check(&mut self, from: TypeId, to: TypeId, range: TextRange) {
        if !self.is_assignable_to(from, to, false, &mut HashSet::new()) {
            self.error(
                DiagnosticInfo::TypeMismatch {
                    expected: self.type_name(to),
                    found: self.type_name(from),
                },
                range,
            );
        }
    }

    fn cast_check(&mut self, from: TypeId, to: TypeId, range: TextRange) {
        if !self.is_assignable_to(from, to, true, &mut HashSet::new()) {
            self.error(
                DiagnosticInfo::CastMismatch {
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
        cast: bool,
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
            (Type::Nil, Type::Nil) => true,
            (Type::Any, Type::Any) => true,
            (Type::Int, Type::Int) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Bytes, Type::Bytes) => true,
            (Type::Bytes32, Type::Bytes32 | Type::Bytes) => true,
            (_, Type::Any) => true,

            // Primitive casts.
            (Type::Nil, Type::Bytes | Type::Bool | Type::Int) if cast => true,
            (Type::Int, Type::Bytes) if cast => true,
            (Type::Bytes | Type::Bytes32, Type::Int) if cast => true,
            (Type::Bool, Type::Int | Type::Bytes) if cast => true,
            (Type::Any, _) if cast => true,

            // List types with compatible items are also assignable.
            (Type::List(a), Type::List(b)) => self.is_assignable_to(a, b, cast, visited),

            (Type::Pair(a_left, a_right), Type::Pair(b_left, b_right)) => {
                self.is_assignable_to(a_left, b_left, cast, visited)
                    && self.is_assignable_to(a_right, b_right, cast, visited)
            }

            // Enum variants are assignable to their enum type.
            (Type::EnumVariant(enum_variant), _) if b == enum_variant.enum_type() => true,

            // Functions with compatible parameters and return type.
            (Type::Function(fun_a), Type::Function(fun_b)) => {
                if fun_a.parameter_types().len() != fun_b.parameter_types().len() {
                    return false;
                }

                for (a, b) in fun_a
                    .parameter_types()
                    .iter()
                    .zip(fun_b.parameter_types().iter())
                {
                    if !self.is_assignable_to(*a, *b, cast, visited) {
                        return false;
                    }
                }

                self.is_assignable_to(fun_a.return_type(), fun_b.return_type(), cast, visited)
            }

            _ => false,
        }
    }

    fn types_equal(&self, a: TypeId, b: TypeId) -> bool {
        self.types_equal_visitor(a, b, &mut HashSet::new())
    }

    fn types_equal_visitor(
        &self,
        a_id: TypeId,
        b_id: TypeId,
        visited: &mut HashSet<(TypeId, TypeId)>,
    ) -> bool {
        let key = (a_id, b_id);

        if a_id == b_id || visited.contains(&key) {
            return true;
        }
        visited.insert(key);

        let b = self.db.ty(b_id).clone();

        let equal = match self.db.ty(a_id).clone() {
            Type::Unknown => matches!(b, Type::Unknown),
            Type::Any => matches!(b, Type::Any),
            Type::Nil => matches!(b, Type::Nil),
            Type::Int => matches!(b, Type::Int),
            Type::Bool => matches!(b, Type::Bool),
            Type::Bytes => matches!(b, Type::Bytes),
            Type::Bytes32 => matches!(b, Type::Bytes32),
            Type::Enum(..) | Type::EnumVariant(..) | Type::Struct(..) => a_id == b_id,
            Type::List(inner) => {
                if let Type::List(other_inner) = b {
                    self.types_equal_visitor(inner, other_inner, visited)
                } else {
                    false
                }
            }
            Type::Pair(left, right) => {
                if let Type::Pair(other_left, other_right) = b {
                    self.types_equal_visitor(left, other_left, visited)
                        && self.types_equal_visitor(right, other_right, visited)
                } else {
                    false
                }
            }
            Type::Function(fun) => {
                if let Type::Function(other_fun) = b {
                    if fun.parameter_types().len() != other_fun.parameter_types().len() {
                        return false;
                    }

                    if fun.varargs() != other_fun.varargs() {
                        return false;
                    }

                    for (a, b) in fun
                        .parameter_types()
                        .iter()
                        .zip(other_fun.parameter_types().iter())
                    {
                        if !self.types_equal_visitor(*a, *b, visited) {
                            return false;
                        }
                    }

                    self.types_equal_visitor(fun.return_type(), other_fun.return_type(), visited)
                } else {
                    false
                }
            }
            Type::Alias(..) => unreachable!(),
        };

        visited.remove(&key);

        equal
    }

    fn unknown(&self) -> Value {
        Value::typed(self.unknown_hir, self.unknown_type)
    }

    fn symbol_type(&self, symbol_id: SymbolId) -> Option<TypeId> {
        for guards in self.type_guards.iter() {
            if let Some(guard) = guards.get(&symbol_id) {
                return Some(*guard);
            }
        }
        None
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

    fn warning(&mut self, info: DiagnosticInfo, range: TextRange) {
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
