use std::{collections::HashMap, fmt, str::FromStr};

pub(crate) use builtins::{builtins, Builtins};
use clvmr::Allocator;
use declarations::Declarations;
use indexmap::{IndexMap, IndexSet};
use rowan::TextRange;
use rue_parser::{
    AstNode, BinaryExpr, BinaryOp, Block, CastExpr, ConstItem, EnumItem, Expr, FieldAccess,
    FunctionCall, FunctionItem, FunctionType as AstFunctionType, GroupExpr, GuardExpr, IfExpr,
    IfStmt, IndexAccess, InitializerExpr, InitializerField, Item, LambdaExpr, LetStmt, ListExpr,
    ListType, LiteralExpr, OptionalType, PairExpr, PairType as AstPairType, Path, PrefixExpr,
    PrefixOp, Root, Stmt, StructField, StructItem, SyntaxKind, SyntaxToken, Type as AstType,
    TypeAliasItem,
};
use symbol_table::SymbolTable;

use crate::{
    database::{Database, HirId, ScopeId, SymbolId, TypeId},
    hir::{BinOp, Hir},
    scope::Scope,
    symbol::{Const, Function, Let, Symbol},
    ty::{EnumType, EnumVariant, FunctionType, Guard, PairType, Rest, StructType, Type, Value},
    Comparison, ErrorKind, TypeSystem, WarningKind,
};

mod builtins;
mod declarations;
mod symbol_table;
mod unused;

/// Responsible for lowering the AST into the HIR.
/// Performs name resolution and type checking.
pub struct Compiler<'a> {
    // The database is mutable because we need to allocate new symbols and types.
    db: &'a mut Database,

    // The scope stack is used to keep track of the current scope.
    scope_stack: Vec<ScopeId>,

    // The symbol stack is used for calculating types referenced in symbols.
    symbol_stack: Vec<SymbolId>,

    // The type definition stack is used for calculating types referenced in types.
    type_definition_stack: Vec<TypeId>,

    // The type guard stack is used for overriding types in certain contexts.
    type_guard_stack: Vec<HashMap<SymbolId, TypeId>>,

    // Whether the current expression is directly the callee of a function call.
    is_callee: bool,

    // The symbol table is used for storing all named symbols and types.
    // It also stored types referenced by symbols.
    sym: SymbolTable,

    // Common types and other values that are built-in to the compiler.
    builtins: Builtins,
}

impl<'a> Compiler<'a> {
    pub fn new(db: &'a mut Database) -> Self {
        let mut builtin_scope = Scope::default();
        let builtins = builtins(db, &mut builtin_scope);
        let builtins_id = db.alloc_scope(builtin_scope);

        Self {
            db,
            scope_stack: vec![builtins_id],
            symbol_stack: Vec::new(),
            type_definition_stack: Vec::new(),
            type_guard_stack: Vec::new(),
            is_callee: false,
            sym: SymbolTable::default(),
            builtins,
        }
    }

    /// Lowering is completed, extract the diagnostics.
    pub fn finish(self) -> SymbolTable {
        self.sym
    }

    /// Declares all of the items in a root.
    pub fn declare_root(&mut self, root: Root, scope_id: ScopeId) -> Declarations {
        self.scope_stack.push(scope_id);
        let declarations = self.declare_items(root.items());
        self.scope_stack.pop().unwrap();
        declarations
    }

    /// Declare all items into scope without compiling their body.
    /// This ensures no circular references are resolved at this time.
    pub fn declare_items(&mut self, items: Vec<Item>) -> Declarations {
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

        Declarations {
            type_ids,
            symbol_ids,
        }
    }

    /// Compile the root by lowering all items into scope.
    pub fn compile_root(&mut self, root: Root, scope_id: ScopeId, declarations: Declarations) {
        self.scope_stack.push(scope_id);
        self.compile_items(root.items(), declarations);
        self.scope_stack.pop().unwrap();
    }

    /// Lower all of the items in the list in the proper order.
    /// This is done in two passes to handle forward references.
    fn compile_items(&mut self, items: Vec<Item>, mut declarations: Declarations) {
        for item in items.clone() {
            match item {
                Item::TypeAliasItem(ty) => {
                    let type_id = declarations.type_ids.remove(0);
                    self.type_definition_stack.push(type_id);
                    self.compile_type_alias(ty, type_id);
                    self.type_definition_stack.pop().unwrap();
                }
                Item::StructItem(struct_item) => {
                    let type_id = declarations.type_ids.remove(0);
                    self.type_definition_stack.push(type_id);
                    self.compile_struct(struct_item, type_id);
                    self.type_definition_stack.pop().unwrap();
                }
                Item::EnumItem(enum_item) => {
                    let type_id = declarations.type_ids.remove(0);
                    self.type_definition_stack.push(type_id);
                    self.compile_enum(enum_item, type_id);
                    self.type_definition_stack.pop().unwrap();
                }
                _ => {}
            }
        }

        for item in items {
            match item {
                Item::FunctionItem(function) => {
                    let symbol_id = declarations.symbol_ids.remove(0);
                    self.symbol_stack.push(symbol_id);
                    self.compile_function(function, symbol_id);
                    self.symbol_stack.pop().unwrap();
                }
                Item::ConstItem(const_item) => {
                    let symbol_id = declarations.symbol_ids.remove(0);
                    self.symbol_stack.push(symbol_id);
                    self.compile_const(const_item, symbol_id);
                    self.symbol_stack.pop().unwrap();
                }
                _ => {}
            }
        }
    }

    /// Define a function in the current scope.
    /// This does not compile the function body, but it creates a new scope for it.
    /// Parameter symbols are defined now in the inner function scope.
    /// The function body is compiled later to allow for forward references.
    fn declare_function(&mut self, function_item: FunctionItem) -> SymbolId {
        // Add the symbol to the stack early so you can track type references.
        let symbol_id = self.db.alloc_symbol(Symbol::Unknown);
        self.symbol_stack.push(symbol_id);

        let mut scope = Scope::default();

        let return_type = function_item
            .return_type()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.builtins.unknown);

        let mut param_types = Vec::new();
        let mut rest = Rest::Nil;

        let len = function_item.params().len();

        for (i, param) in function_item.params().into_iter().enumerate() {
            // Add the symbol to the stack early so you can track type references.
            let symbol_id = self.db.alloc_symbol(Symbol::Unknown);
            self.symbol_stack.push(symbol_id);

            let type_id = param
                .ty()
                .map(|ty| self.compile_type(ty))
                .unwrap_or(self.builtins.unknown);

            param_types.push(type_id);

            *self.db.symbol_mut(symbol_id) = Symbol::Parameter(type_id);

            if let Some(name) = param.name() {
                scope.define_symbol(name.to_string(), symbol_id);
                self.db.insert_symbol_token(symbol_id, name);
            }

            if param.spread().is_some() {
                if i + 1 == len {
                    rest = Rest::Parameter;
                } else {
                    self.db
                        .error(ErrorKind::NonFinalSpread, param.syntax().text_range());
                }
            }

            self.symbol_stack.pop().unwrap();
        }

        let scope_id = self.db.alloc_scope(scope);
        let hir_id = self.db.alloc_hir(Hir::Unknown);

        let ty = FunctionType {
            param_types,
            rest,
            return_type,
        };

        *self.db.symbol_mut(symbol_id) = Symbol::Function(Function {
            scope_id,
            hir_id,
            ty,
            inline: function_item.inline().is_some(),
        });

        if let Some(name) = function_item.name() {
            self.scope_mut().define_symbol(name.to_string(), symbol_id);
            self.db.insert_scope_token(scope_id, name.clone());
            self.db.insert_symbol_token(symbol_id, name);
        }

        self.symbol_stack.pop().unwrap()
    }

    /// Define a constant in the current scope, but don't lower its body.
    fn declare_const(&mut self, const_item: ConstItem) -> SymbolId {
        // Add the symbol to the stack early so you can track type references.
        let symbol_id = self.db.alloc_symbol(Symbol::Unknown);
        self.symbol_stack.push(symbol_id);

        let type_id = const_item
            .ty()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.builtins.unknown);

        let hir_id = self.db.alloc_hir(Hir::Unknown);

        *self.db.symbol_mut(symbol_id) = Symbol::Const(Const {
            type_id,
            hir_id,
            inline: const_item.inline().is_some(),
        });

        if let Some(name) = const_item.name() {
            self.scope_mut().define_symbol(name.to_string(), symbol_id);
            self.db.insert_symbol_token(symbol_id, name);
        }

        self.symbol_stack.pop().unwrap()
    }

    /// Define a type for an alias in the current scope, but leave it as unknown for now.
    fn declare_type_alias(&mut self, type_alias: TypeAliasItem) -> TypeId {
        let type_id = self.db.alloc_type(Type::Unknown);
        if let Some(name) = type_alias.name() {
            self.scope_mut().define_type(name.to_string(), type_id);
            self.db.insert_type_token(type_id, name);
        }
        type_id
    }

    /// Define a type for a struct in the current scope, but leave it as unknown for now.
    fn declare_struct(&mut self, struct_item: StructItem) -> TypeId {
        let type_id = self.db.alloc_type(Type::Unknown);
        if let Some(name) = struct_item.name() {
            self.scope_mut().define_type(name.to_string(), type_id);
            self.db.insert_type_token(type_id, name);
        }
        type_id
    }

    /// Define a type for an enum in the current scope.
    /// This creates the enum variants as well, but they are left as unknown types.
    fn declare_enum(&mut self, enum_item: EnumItem) -> TypeId {
        let mut variants = IndexMap::new();

        for variant in enum_item.variants() {
            let Some(name) = variant.name() else {
                continue;
            };

            // Silently ignore duplicate variants, since they will be caught later.
            if variants.contains_key(name.text()) {
                continue;
            }

            let type_id = self.db.alloc_type(Type::Unknown);
            variants.insert(name.to_string(), type_id);
            self.db.insert_type_token(type_id, name);
        }

        let type_id = self.db.alloc_type(Type::Enum(EnumType { variants }));

        if let Some(name) = enum_item.name() {
            self.scope_mut().define_type(name.to_string(), type_id);
            self.db.insert_type_token(type_id, name);
        }

        type_id
    }

    /// Compiles the body of a function within the function's scope.
    fn compile_function(&mut self, function: FunctionItem, symbol_id: SymbolId) {
        let Some(body) = function.body() else {
            return;
        };

        let Symbol::Function(Function { scope_id, ty, .. }) = self.db.symbol(symbol_id).clone()
        else {
            unreachable!();
        };

        // We don't care about explicit returns in this context.
        self.scope_stack.push(scope_id);
        let value = self.compile_block(body, Some(ty.return_type)).0;
        self.scope_stack.pop().unwrap();

        // Ensure that the body is assignable to the return type.
        self.type_check(
            value.type_id,
            ty.return_type,
            function.body().unwrap().syntax().text_range(),
        );

        // We ignore type guards here for now.
        // Just set the function body HIR.
        let Symbol::Function(Function { hir_id, .. }) = self.db.symbol_mut(symbol_id) else {
            unreachable!();
        };
        *hir_id = value.hir_id;
    }

    /// Compiles a constant's value.
    fn compile_const(&mut self, const_item: ConstItem, symbol_id: SymbolId) {
        let Some(expr) = const_item.expr() else {
            return;
        };

        let Symbol::Const(Const { type_id, .. }) = self.db.symbol(symbol_id).clone() else {
            unreachable!();
        };

        let output = self.compile_expr(expr, Some(type_id));

        // Ensure that the expression is assignable to the constant's type.
        self.type_check(output.type_id, type_id, const_item.syntax().text_range());

        // We ignore type guards here for now.
        // Just set the constant HIR.
        let Symbol::Const(Const { hir_id, .. }) = self.db.symbol_mut(symbol_id) else {
            unreachable!();
        };
        *hir_id = output.hir_id;
    }

    /// Compile and resolve the type that the alias points to.
    fn compile_type_alias(&mut self, ty: TypeAliasItem, alias_type_id: TypeId) {
        self.type_definition_stack.push(alias_type_id);

        let type_id = ty
            .ty()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.builtins.unknown);

        // Set the alias type to the resolved type.
        *self.db.ty_mut(alias_type_id) = Type::Alias(type_id);

        // A cycle between type aliases has been detected.
        // We set it to unknown to prevent stack overflow issues later.
        if self.detect_cycle(alias_type_id, ty.syntax().text_range()) {
            *self.db.ty_mut(alias_type_id) = Type::Unknown;
        }

        self.type_definition_stack.pop().unwrap();
    }

    /// Compile and resolve a struct type.
    fn compile_struct(&mut self, struct_item: StructItem, type_id: TypeId) {
        self.type_definition_stack.push(type_id);
        let fields = self.compile_struct_fields(struct_item.fields());
        *self.db.ty_mut(type_id) = Type::Struct(StructType { fields });
        self.type_definition_stack.pop().unwrap();
    }

    /// Compile and resolve the fields of a struct.
    fn compile_struct_fields(&mut self, fields: Vec<StructField>) -> IndexMap<String, TypeId> {
        let mut named_fields = IndexMap::new();

        for field in fields {
            let type_id = field
                .ty()
                .map(|ty| self.compile_type(ty))
                .unwrap_or(self.builtins.unknown);

            if let Some(name) = field.name() {
                named_fields.insert(name.to_string(), type_id);
            };
        }

        named_fields
    }

    /// Compile and resolve an enum type, and each of its variants' struct fields.
    fn compile_enum(&mut self, enum_item: EnumItem, type_id: TypeId) {
        self.type_definition_stack.push(type_id);

        let Type::Enum(enum_type) = self.db.ty(type_id).clone() else {
            unreachable!();
        };

        let mut visited_variants = IndexSet::new();

        for variant in enum_item.variants() {
            let Some(name) = variant.name() else {
                continue;
            };

            // If the variant is a duplicate, we don't want to overwrite the existing variant.
            if !visited_variants.insert(name.to_string()) {
                self.db.error(
                    ErrorKind::DuplicateEnumVariant(name.to_string()),
                    name.text_range(),
                );
                continue;
            }

            let variant_type = enum_type.variants[name.text()];

            self.type_definition_stack.push(variant_type);

            let fields = self.compile_struct_fields(variant.fields());

            let discriminant = variant
                .discriminant()
                .map(|discriminant| self.compile_int(discriminant).hir_id)
                .unwrap_or(self.builtins.unknown_hir);

            *self.db.ty_mut(variant_type) = Type::EnumVariant(EnumVariant {
                name: name.to_string(),
                enum_type: type_id,
                fields,
                discriminant,
            });

            self.type_definition_stack.pop().unwrap();
        }

        self.type_definition_stack.pop().unwrap();
    }

    /// Compiles a let statement and returns its new scope id.
    fn compile_let_stmt(&mut self, let_stmt: LetStmt) -> Option<ScopeId> {
        // Add the symbol to the stack early so you can track type references.
        let symbol_id = self.db.alloc_symbol(Symbol::Unknown);
        self.symbol_stack.push(symbol_id);

        // This doesn't default to unknown, since we want to infer the type if possible.
        let expected_type = let_stmt.ty().map(|ty| self.compile_type(ty));

        // Compile the expression.
        let value = let_stmt
            .expr()
            .map(|expr| self.compile_expr(expr, expected_type))
            .unwrap_or(self.unknown());

        // Check that the expression's type matches the type annotation, if present.
        if let Some(expected_type) = expected_type {
            self.type_check(value.type_id, expected_type, let_stmt.syntax().text_range());
        }

        // If the name can't be resolved, there's no reason to continue compiling it.
        // We only do the above steps first to catch any other errors that may occur.
        let Some(name) = let_stmt.name() else {
            self.symbol_stack.pop().unwrap();
            return None;
        };

        *self.db.symbol_mut(symbol_id) = Symbol::Let(Let {
            type_id: expected_type.unwrap_or(value.type_id),
            hir_id: value.hir_id,
        });

        // Every let binding is a new scope for now, to ensure references are resolved in the proper order.
        let mut let_scope = Scope::default();
        let_scope.define_symbol(name.to_string(), symbol_id);
        self.db.insert_symbol_token(symbol_id, name.clone());

        let scope_id = self.db.alloc_scope(let_scope);
        self.db.insert_scope_token(scope_id, name);
        self.scope_stack.push(scope_id);
        self.symbol_stack.pop().unwrap();

        Some(scope_id)
    }

    /// Compiles an if statement, returning the condition HIR, then block HIR, and else block guards.
    fn compile_if_stmt(
        &mut self,
        if_stmt: IfStmt,
        expected_type: Option<TypeId>,
    ) -> (HirId, HirId, HashMap<SymbolId, TypeId>) {
        // Compile the condition expression.
        let condition = if_stmt
            .condition()
            .map(|condition| self.compile_expr(condition, Some(self.builtins.bool)))
            .unwrap_or_else(|| self.unknown());

        // Check that the condition is a boolean.
        self.type_check(
            condition.type_id,
            self.builtins.bool,
            if_stmt.syntax().text_range(),
        );

        let then_block = if let Some(then_block) = if_stmt.then_block() {
            // We create a new scope for the then block.
            let scope_id = self.db.alloc_scope(Scope::default());

            // We can apply any type guards from the condition.
            self.type_guard_stack.push(condition.then_guards());

            // Compile the then block.
            self.scope_stack.push(scope_id);
            let (value, explicit_return) = self.compile_block(then_block.clone(), expected_type);
            self.scope_stack.pop().unwrap();

            // Pop the type guards, since we've left the scope.
            self.type_guard_stack.pop().unwrap();

            // If there's an implicit return, we want to raise an error.
            // This could technically work but makes the intent of the code unclear.
            if !explicit_return {
                self.db.error(
                    ErrorKind::ImplicitReturnInIf,
                    then_block.syntax().text_range(),
                );
            }

            value
        } else {
            self.unknown()
        };

        // Check that the output matches the expected type.
        self.type_check(
            then_block.type_id,
            expected_type.unwrap_or(self.builtins.unknown),
            if_stmt.syntax().text_range(),
        );

        (condition.hir_id, then_block.hir_id, condition.else_guards())
    }

    /// Compile a block expression into the current scope, returning the HIR and whether there was an explicit return.
    fn compile_block(&mut self, block: Block, expected_type: Option<TypeId>) -> (Value, bool) {
        // Compile all of the items in the block first.
        // This means that statements can use item symbols in any order,
        // but items cannot use statement symbols.
        let declarations = self.declare_items(block.items());
        self.compile_items(block.items(), declarations);

        enum Statement {
            Let(ScopeId),
            If(HirId, HirId),
            Return(Value),
        }

        let mut statements = Vec::new();
        let mut explicit_return = false;
        let mut is_terminated = block.expr().is_some();

        for stmt in block.stmts() {
            match stmt {
                Stmt::LetStmt(let_stmt) => {
                    let Some(scope_id) = self.compile_let_stmt(let_stmt) else {
                        continue;
                    };
                    statements.push(Statement::Let(scope_id));
                }
                Stmt::IfStmt(if_stmt) => {
                    let (condition_hir, then_hir, else_guards) =
                        self.compile_if_stmt(if_stmt, expected_type);

                    // Push the type guards onto the stack.
                    // This will be popped in reverse order later after all statements have been lowered.
                    self.type_guard_stack.push(else_guards);

                    statements.push(Statement::If(condition_hir, then_hir));
                }
                Stmt::ReturnStmt(return_stmt) => {
                    let value = return_stmt
                        .expr()
                        .map(|expr| self.compile_expr(expr, expected_type))
                        .unwrap_or_else(|| self.unknown());

                    // Make sure that the return value matches the expected type.
                    self.type_check(
                        value.type_id,
                        expected_type.unwrap_or(self.builtins.unknown),
                        return_stmt.syntax().text_range(),
                    );

                    explicit_return = true;
                    is_terminated = true;

                    statements.push(Statement::Return(value));
                }
                Stmt::RaiseStmt(raise_stmt) => {
                    // You can raise any value as an error, so we don't need to check the type.
                    // The value is also optional.
                    let value = raise_stmt
                        .expr()
                        .map(|expr| self.compile_expr(expr, None).hir_id);

                    let hir_id = self.db.alloc_hir(Hir::Raise(value));

                    is_terminated = true;

                    statements.push(Statement::Return(Value::new(hir_id, self.builtins.unknown)));
                }
                Stmt::AssertStmt(assert_stmt) => {
                    // Compile the condition expression.
                    let condition = assert_stmt
                        .expr()
                        .map(|condition| self.compile_expr(condition, Some(self.builtins.bool)))
                        .unwrap_or_else(|| self.unknown());

                    // Make sure that the condition is a boolean.
                    self.type_check(
                        condition.type_id,
                        self.builtins.bool,
                        assert_stmt.syntax().text_range(),
                    );

                    // If the condition is false, we raise an error.
                    // So we can assume that the condition is true from this point on.
                    // This will be popped in reverse order later after all statements have been lowered.
                    self.type_guard_stack.push(condition.then_guards());

                    let not_condition = self.db.alloc_hir(Hir::Not(condition.hir_id));
                    let raise = self.db.alloc_hir(Hir::Raise(None));

                    // We lower this down to an inverted if statement.
                    statements.push(Statement::If(not_condition, raise))
                }
            }
        }

        // Compile the expression of the block, if present.
        let mut body = block
            .expr()
            .map(|expr| self.compile_expr(expr, expected_type))
            .unwrap_or(self.unknown());

        // Ensure that the block terminates.
        if !is_terminated {
            self.db
                .error(ErrorKind::EmptyBlock, block.syntax().text_range());
        }

        // Pop each statement in reverse order and mutate the body.
        for statement in statements.into_iter().rev() {
            match statement {
                Statement::Let(scope_id) => {
                    body = Value::new(
                        self.db.alloc_hir(Hir::Definition {
                            scope_id,
                            hir_id: body.hir_id,
                        }),
                        body.type_id,
                    );
                    self.scope_stack.pop().unwrap();
                }
                Statement::Return(value) => {
                    body = value;
                }
                Statement::If(condition, then_block) => {
                    self.type_guard_stack.pop().unwrap();

                    body = Value::new(
                        self.db.alloc_hir(Hir::If {
                            condition,
                            then_block,
                            else_block: body.hir_id,
                        }),
                        body.type_id,
                    );
                }
            }
        }

        (body, explicit_return)
    }

    /// Compile a block in the context of an expression.
    fn compile_block_expr(&mut self, block: Block, expected_type: Option<TypeId>) -> Value {
        let scope_id = self.db.alloc_scope(Scope::default());

        self.scope_stack.push(scope_id);
        let (value, explicit_return) = self.compile_block(block.clone(), expected_type);
        self.scope_stack.pop().unwrap();

        if explicit_return {
            self.db
                .error(ErrorKind::ExplicitReturnInExpr, block.syntax().text_range());
        }

        value
    }

    /// Compiles any expression.
    fn compile_expr(&mut self, expr: Expr, expected_type: Option<TypeId>) -> Value {
        match &expr {
            Expr::Path(..) => {}
            _ => self.is_callee = false,
        }

        let value = match expr {
            Expr::Path(path) => self.compile_path_expr(path),
            Expr::InitializerExpr(initializer) => self.compile_initializer_expr(initializer),
            Expr::LiteralExpr(literal) => self.compile_literal_expr(literal),
            Expr::ListExpr(list) => self.compile_list_expr(list, expected_type),
            Expr::PairExpr(pair) => self.compile_pair_expr(pair, expected_type),
            Expr::Block(block) => self.compile_block_expr(block.clone(), expected_type),
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
        };

        self.is_callee = false;

        value
    }

    /// Compiles an initializer expression.
    fn compile_initializer_expr(&mut self, initializer: InitializerExpr) -> Value {
        let ty = initializer.path().map(|path| self.compile_path_type(path));

        match ty.map(|ty| self.db.ty(ty)).cloned() {
            Some(Type::Struct(struct_type)) => {
                let hir_id = self.compile_initializer_fields(
                    ty.unwrap(),
                    &struct_type.fields,
                    initializer.fields(),
                    initializer.syntax().text_range(),
                );

                match ty {
                    Some(struct_type) => Value::new(hir_id, struct_type),
                    None => self.unknown(),
                }
            }
            Some(Type::EnumVariant(enum_variant)) => {
                let fields_hir_id = self.compile_initializer_fields(
                    ty.unwrap(),
                    &enum_variant.fields,
                    initializer.fields(),
                    initializer.syntax().text_range(),
                );

                let hir_id = self
                    .db
                    .alloc_hir(Hir::Pair(enum_variant.discriminant, fields_hir_id));

                match ty {
                    Some(struct_type) => Value::new(hir_id, struct_type),
                    None => self.unknown(),
                }
            }
            Some(_) => {
                self.db.error(
                    ErrorKind::UninitializableType(self.type_name(ty.unwrap())),
                    initializer.path().unwrap().syntax().text_range(),
                );
                self.unknown()
            }
            _ => self.unknown(),
        }
    }

    /// Compiles the fields of an initializer into a list.
    fn compile_initializer_fields(
        &mut self,
        struct_type: TypeId,
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

            // Check the type of the field initializer.
            self.type_check(
                value.type_id,
                expected_type.unwrap_or(self.builtins.unknown),
                field.syntax().text_range(),
            );

            let Some(name) = field.name() else {
                continue;
            };

            // Insert the field if it exists and hasn't already been assigned.
            if specified_fields.contains_key(name.text()) {
                self.db.error(
                    ErrorKind::DuplicateField(name.to_string()),
                    name.text_range(),
                );
            } else if !struct_fields.contains_key(name.text()) {
                self.db.error(
                    ErrorKind::UndefinedField {
                        field: name.to_string(),
                        ty: self.type_name(struct_type),
                    },
                    name.text_range(),
                );
            } else {
                specified_fields.insert(name.to_string(), value.hir_id);
            }
        }

        // Check for any missing fields and report them.
        let missing_fields: Vec<String> = struct_fields
            .keys()
            .filter(|name| !specified_fields.contains_key(*name))
            .cloned()
            .collect();

        if !missing_fields.is_empty() {
            self.db.error(
                ErrorKind::MissingFields {
                    fields: missing_fields,
                    ty: self.type_name(struct_type),
                },
                text_range,
            );
        }

        let mut hir_id = self.builtins.nil_hir;

        // Construct a nil-terminated list from the arguments.
        for field in struct_fields.keys().rev() {
            let field = specified_fields
                .get(field)
                .cloned()
                .unwrap_or(self.builtins.unknown_hir);

            hir_id = self.db.alloc_hir(Hir::Pair(field, hir_id));
        }

        hir_id
    }

    /// Compiles a field access expression, or special properties for certain types.
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

        match self.db.ty(value.type_id).clone() {
            Type::Struct(struct_type) => {
                if let Some(field) = struct_type.fields.get_full(field_name.text()) {
                    let (index, _, field_type) = field;
                    return Value::new(self.compile_index(value.hir_id, index, false), *field_type);
                } else {
                    self.db.error(
                        ErrorKind::UndefinedField {
                            field: field_name.to_string(),
                            ty: self.type_name(value.type_id),
                        },
                        field_name.text_range(),
                    );
                    return self.unknown();
                }
            }
            Type::Pair(PairType { first, rest }) => match field_name.text() {
                "first" => {
                    return Value::new(self.db.alloc_hir(Hir::First(value.hir_id)), first);
                }
                "rest" => {
                    return Value::new(self.db.alloc_hir(Hir::Rest(value.hir_id)), rest);
                }
                _ => {}
            },
            Type::Bytes | Type::Bytes32 if field_name.text() == "length" => {
                return Value::new(
                    self.db.alloc_hir(Hir::Strlen(value.hir_id)),
                    self.builtins.int,
                );
            }
            _ => {}
        }

        self.db.error(
            ErrorKind::InvalidFieldAccess {
                field: field_name.to_string(),
                ty: self.type_name(value.type_id),
            },
            field_name.text_range(),
        );
        self.unknown()
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

        let Type::List(item_type) = self.db.ty(value.type_id).clone() else {
            self.db.error(
                ErrorKind::IndexAccess(self.type_name(value.type_id)),
                index_access.expr().unwrap().syntax().text_range(),
            );
            return self.unknown();
        };

        Value::new(self.compile_index(value.hir_id, index, false), item_type)
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

        let Some(op) = prefix_expr.op() else {
            return self.unknown();
        };

        match op {
            PrefixOp::Not => {
                self.type_check(
                    expr.type_id,
                    self.builtins.bool,
                    prefix_expr.syntax().text_range(),
                );

                let mut value =
                    Value::new(self.db.alloc_hir(Hir::Not(expr.hir_id)), self.builtins.bool);

                for (symbol_id, guard) in expr.guards {
                    value.guards.insert(symbol_id, !guard);
                }

                value
            }
            PrefixOp::Neg => {
                self.type_check(
                    expr.type_id,
                    self.builtins.int,
                    prefix_expr.syntax().text_range(),
                );

                Value::new(
                    self.db.alloc_hir(Hir::BinaryOp {
                        op: BinOp::Subtract,
                        lhs: self.builtins.nil_hir,
                        rhs: expr.hir_id,
                    }),
                    self.builtins.int,
                )
            }
        }
    }

    fn compile_binary_expr(&mut self, binary: BinaryExpr) -> Value {
        let Some(op) = binary.op() else {
            return self.unknown();
        };

        let text_range = binary.syntax().text_range();
        let mut value = self.unknown();

        macro_rules! lhs {
            () => {
                binary
                    .lhs()
                    .map(|lhs| self.compile_expr(lhs, None))
                    .unwrap_or_else(|| self.unknown())
            };
        }

        macro_rules! rhs {
            () => {
                binary
                    .rhs()
                    .map(|rhs| self.compile_expr(rhs, None))
                    .unwrap_or_else(|| self.unknown())
            };
        }

        let (op, lhs, rhs, type_id) = match op {
            BinaryOp::Add => {
                let lhs = lhs!();
                let rhs = rhs!();

                if self.db.compare_type(lhs.type_id, self.builtins.public_key) == Comparison::Equal
                {
                    self.type_check(rhs.type_id, self.builtins.public_key, text_range);
                    (
                        BinOp::PointAdd,
                        lhs.hir_id,
                        rhs.hir_id,
                        self.builtins.public_key,
                    )
                } else if self.db.compare_type(lhs.type_id, self.builtins.bytes)
                    == Comparison::Equal
                {
                    self.type_check(rhs.type_id, self.builtins.bytes, text_range);
                    (BinOp::Concat, lhs.hir_id, rhs.hir_id, self.builtins.bytes)
                } else {
                    self.type_check(lhs.type_id, self.builtins.int, text_range);
                    self.type_check(rhs.type_id, self.builtins.int, text_range);
                    (BinOp::Add, lhs.hir_id, rhs.hir_id, self.builtins.int)
                }
            }
            BinaryOp::Subtract => {
                let lhs = lhs!();
                let rhs = rhs!();
                self.type_check(lhs.type_id, self.builtins.int, text_range);
                self.type_check(rhs.type_id, self.builtins.int, text_range);
                (BinOp::Subtract, lhs.hir_id, rhs.hir_id, self.builtins.int)
            }
            BinaryOp::Multiply => {
                let lhs = lhs!();
                let rhs = rhs!();
                self.type_check(lhs.type_id, self.builtins.int, text_range);
                self.type_check(rhs.type_id, self.builtins.int, text_range);
                (BinOp::Multiply, lhs.hir_id, rhs.hir_id, self.builtins.int)
            }
            BinaryOp::Divide => {
                let lhs = lhs!();
                let rhs = rhs!();
                self.type_check(lhs.type_id, self.builtins.int, text_range);
                self.type_check(rhs.type_id, self.builtins.int, text_range);
                (BinOp::Divide, lhs.hir_id, rhs.hir_id, self.builtins.int)
            }
            BinaryOp::Remainder => {
                let lhs = lhs!();
                let rhs = rhs!();
                self.type_check(lhs.type_id, self.builtins.int, text_range);
                self.type_check(rhs.type_id, self.builtins.int, text_range);
                (BinOp::Remainder, lhs.hir_id, rhs.hir_id, self.builtins.int)
            }
            BinaryOp::Equals => {
                let lhs = lhs!();
                let rhs = rhs!();

                if self.db.compare_type(lhs.type_id, self.builtins.bytes) > Comparison::Castable
                    || self.db.compare_type(rhs.type_id, self.builtins.bytes) > Comparison::Castable
                {
                    self.db.error(
                        ErrorKind::NonAtomEquality(self.type_name(lhs.type_id)),
                        text_range,
                    );
                } else if self.db.compare_type(lhs.type_id, self.builtins.nil) == Comparison::Equal
                {
                    if let Hir::Reference(symbol_id) = self.db.hir(rhs.hir_id) {
                        value.guards.insert(
                            *symbol_id,
                            Guard::new(self.builtins.nil, self.try_unwrap_optional(rhs.type_id)),
                        );
                    }
                } else if self.db.compare_type(rhs.type_id, self.builtins.nil) == Comparison::Equal
                {
                    if let Hir::Reference(symbol_id) = self.db.hir(lhs.hir_id) {
                        value.guards.insert(
                            *symbol_id,
                            Guard::new(self.builtins.nil, self.try_unwrap_optional(lhs.type_id)),
                        );
                    }
                } else {
                    self.type_check(rhs.type_id, lhs.type_id, text_range);
                }

                (BinOp::Equals, lhs.hir_id, rhs.hir_id, self.builtins.bool)
            }
            BinaryOp::NotEquals => {
                let lhs = lhs!();
                let rhs = rhs!();

                if self.db.compare_type(lhs.type_id, self.builtins.bytes) > Comparison::Castable
                    || self.db.compare_type(rhs.type_id, self.builtins.bytes) > Comparison::Castable
                {
                    self.db.error(
                        ErrorKind::NonAtomEquality(self.type_name(lhs.type_id)),
                        text_range,
                    );
                } else if self.db.compare_type(lhs.type_id, self.builtins.nil) == Comparison::Equal
                {
                    if let Hir::Reference(symbol_id) = self.db.hir(rhs.hir_id) {
                        value.guards.insert(
                            *symbol_id,
                            Guard::new(self.try_unwrap_optional(rhs.type_id), self.builtins.nil),
                        );
                    }
                } else if self.db.compare_type(rhs.type_id, self.builtins.nil) == Comparison::Equal
                {
                    if let Hir::Reference(symbol_id) = self.db.hir(lhs.hir_id) {
                        value.guards.insert(
                            *symbol_id,
                            Guard::new(self.try_unwrap_optional(lhs.type_id), self.builtins.nil),
                        );
                    }
                } else {
                    self.type_check(rhs.type_id, lhs.type_id, text_range);
                }

                (BinOp::NotEquals, lhs.hir_id, rhs.hir_id, self.builtins.bool)
            }
            BinaryOp::GreaterThan => {
                let lhs = lhs!();
                let rhs = rhs!();
                self.type_check(lhs.type_id, self.builtins.int, text_range);
                self.type_check(rhs.type_id, self.builtins.int, text_range);
                (
                    BinOp::GreaterThan,
                    lhs.hir_id,
                    rhs.hir_id,
                    self.builtins.bool,
                )
            }
            BinaryOp::LessThan => {
                let lhs = lhs!();
                let rhs = rhs!();
                self.type_check(lhs.type_id, self.builtins.int, text_range);
                self.type_check(rhs.type_id, self.builtins.int, text_range);
                (BinOp::LessThan, lhs.hir_id, rhs.hir_id, self.builtins.bool)
            }
            BinaryOp::GreaterThanEquals => {
                let lhs = lhs!();
                let rhs = rhs!();
                self.type_check(lhs.type_id, self.builtins.int, text_range);
                self.type_check(rhs.type_id, self.builtins.int, text_range);
                (
                    BinOp::GreaterThanEquals,
                    lhs.hir_id,
                    rhs.hir_id,
                    self.builtins.bool,
                )
            }
            BinaryOp::LessThanEquals => {
                let lhs = lhs!();
                let rhs = rhs!();
                self.type_check(lhs.type_id, self.builtins.int, text_range);
                self.type_check(rhs.type_id, self.builtins.int, text_range);
                (
                    BinOp::LessThanEquals,
                    lhs.hir_id,
                    rhs.hir_id,
                    self.builtins.bool,
                )
            }
            BinaryOp::And => {
                let lhs = lhs!();
                self.type_guard_stack.push(lhs.then_guards());
                let rhs = rhs!();
                self.type_guard_stack.pop().unwrap();

                self.type_check(lhs.type_id, self.builtins.bool, text_range);
                self.type_check(rhs.type_id, self.builtins.bool, text_range);

                value.type_id = self.builtins.bool;
                value.guards.extend(lhs.guards);
                value.guards.extend(rhs.guards);

                (
                    BinOp::LogicalAnd,
                    lhs.hir_id,
                    rhs.hir_id,
                    self.builtins.bool,
                )
            }
            BinaryOp::Or => {
                let lhs = lhs!();
                self.type_guard_stack.push(lhs.else_guards());
                let rhs = rhs!();
                self.type_guard_stack.pop().unwrap();

                self.type_check(lhs.type_id, self.builtins.bool, text_range);
                self.type_check(rhs.type_id, self.builtins.bool, text_range);

                value.type_id = self.builtins.bool;
                value.guards.extend(lhs.guards);
                value.guards.extend(rhs.guards);

                (BinOp::LogicalOr, lhs.hir_id, rhs.hir_id, self.builtins.bool)
            }
        };

        value.type_id = type_id;
        value.hir_id = self.db.alloc_hir(Hir::BinaryOp { op, lhs, rhs });
        value
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
            .unwrap_or(self.builtins.unknown);

        self.cast_check(expr.type_id, ty, cast.expr().unwrap().syntax().text_range());

        Value::new(expr.hir_id, ty)
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
            .unwrap_or(self.builtins.unknown);

        let Some((guard, hir_id)) =
            self.guard_into(expr.type_id, ty, expr.hir_id, guard.syntax().text_range())
        else {
            return Value::new(self.builtins.unknown_hir, ty);
        };

        let mut value = Value::new(hir_id, self.builtins.bool);

        if let Hir::Reference(symbol_id) = self.db.hir(expr.hir_id) {
            value.guards.insert(*symbol_id, guard);
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
        if self.db.compare_type(from, to) == Comparison::Equal {
            self.db.warning(
                WarningKind::RedundantTypeCheck(self.type_name(from)),
                text_range,
            );
            return Some((Guard::new(to, self.builtins.bool), hir_id));
        }

        match (self.db.ty(from).clone(), self.db.ty(to).clone()) {
            (Type::Any, Type::Pair(PairType { first, rest })) => {
                if self.db.compare_type(first, self.builtins.any) != Comparison::Equal {
                    self.db.error(ErrorKind::NonAnyPairTypeGuard, text_range);
                }

                if self.db.compare_type(rest, self.builtins.any) != Comparison::Equal {
                    self.db.error(ErrorKind::NonAnyPairTypeGuard, text_range);
                }

                let hir_id = self.db.alloc_hir(Hir::IsCons(hir_id));
                Some((Guard::new(to, self.builtins.bytes), hir_id))
            }
            (Type::Any, Type::Bytes) => {
                let pair_type = self.db.alloc_type(Type::Pair(PairType {
                    first: self.builtins.any,
                    rest: self.builtins.any,
                }));
                let is_cons = self.db.alloc_hir(Hir::IsCons(hir_id));
                let hir_id = self.db.alloc_hir(Hir::Not(is_cons));
                Some((Guard::new(to, pair_type), hir_id))
            }
            (Type::List(inner), Type::Pair(PairType { first, rest })) => {
                if self.db.compare_type(first, inner) != Comparison::Equal {
                    self.db.error(ErrorKind::NonListPairTypeGuard, text_range);
                }

                if self.db.compare_type(rest, from) != Comparison::Equal {
                    self.db.error(ErrorKind::NonListPairTypeGuard, text_range);
                }

                let hir_id = self.db.alloc_hir(Hir::IsCons(hir_id));
                Some((Guard::new(to, self.builtins.nil), hir_id))
            }
            (Type::List(inner), Type::Nil) => {
                let pair_type = self.db.alloc_type(Type::Pair(PairType {
                    first: inner,
                    rest: from,
                }));
                let is_cons = self.db.alloc_hir(Hir::IsCons(hir_id));
                let hir_id = self.db.alloc_hir(Hir::Not(is_cons));
                Some((Guard::new(to, pair_type), hir_id))
            }
            (Type::Bytes, Type::Bytes32) => {
                let strlen = self.db.alloc_hir(Hir::Strlen(hir_id));
                let length = self.db.alloc_hir(Hir::Atom(vec![32]));
                let hir_id = self.db.alloc_hir(Hir::BinaryOp {
                    op: BinOp::Equals,
                    lhs: strlen,
                    rhs: length,
                });
                Some((Guard::new(to, from), hir_id))
            }
            (Type::Bytes, Type::PublicKey) => {
                let strlen = self.db.alloc_hir(Hir::Strlen(hir_id));
                let length = self.db.alloc_hir(Hir::Atom(vec![48]));
                let hir_id = self.db.alloc_hir(Hir::BinaryOp {
                    op: BinOp::Equals,
                    lhs: strlen,
                    rhs: length,
                });
                Some((Guard::new(to, from), hir_id))
            }
            _ => {
                self.db.error(
                    ErrorKind::UnsupportedTypeGuard {
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
            SyntaxKind::Hex => self.compile_hex(value),
            SyntaxKind::String => self.compile_string(value),
            SyntaxKind::True => {
                Value::new(self.db.alloc_hir(Hir::Atom(vec![1])), self.builtins.bool)
            }
            SyntaxKind::False => {
                Value::new(self.db.alloc_hir(Hir::Atom(Vec::new())), self.builtins.bool)
            }
            SyntaxKind::Nil => {
                Value::new(self.db.alloc_hir(Hir::Atom(Vec::new())), self.builtins.nil)
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
                self.type_check(
                    output.type_id,
                    expected_item_type,
                    item.syntax().text_range(),
                );
            }

            if i == 0 && item_type.is_none() {
                if item.spread().is_some() {
                    list_type = Some(output.type_id);
                    item_type = match self.db.ty(output.type_id) {
                        Type::List(ty) => Some(*ty),
                        _ => None,
                    };
                } else {
                    list_type = Some(self.db.alloc_type(Type::List(output.type_id)));
                    item_type = Some(output.type_id);
                }
            }

            if let Some(spread) = item.spread() {
                if i + 1 == len {
                    nil_terminated = false;
                } else {
                    self.db
                        .error(ErrorKind::NonFinalSpread, spread.text_range());
                }
            }

            items.push(output.hir_id);
        }

        let mut hir_id = self.builtins.nil_hir;

        for (i, item) in items.into_iter().rev().enumerate() {
            if i == 0 && !nil_terminated {
                hir_id = item;
            } else {
                hir_id = self.db.alloc_hir(Hir::Pair(item, hir_id));
            }
        }

        Value::new(
            hir_id,
            self.db
                .alloc_type(Type::List(item_type.unwrap_or(self.builtins.unknown))),
        )
    }

    fn compile_pair_expr(&mut self, pair_expr: PairExpr, expected_type: Option<TypeId>) -> Value {
        let expected_first = expected_type.and_then(|ty| match self.db.ty(ty) {
            Type::Pair(pair) => Some(pair.first),
            _ => None,
        });

        let expected_rest = expected_type.and_then(|ty| match self.db.ty(ty) {
            Type::Pair(pair) => Some(pair.rest),
            _ => None,
        });

        let first = if let Some(first) = pair_expr.first() {
            let value = self.compile_expr(first.clone(), expected_first);
            self.type_check(
                value.type_id,
                expected_first.unwrap_or(self.builtins.unknown),
                first.syntax().text_range(),
            );
            value
        } else {
            self.unknown()
        };

        let rest = if let Some(rest) = pair_expr.rest() {
            let value = self.compile_expr(rest.clone(), expected_rest);
            self.type_check(
                value.type_id,
                expected_rest.unwrap_or(self.builtins.unknown),
                rest.syntax().text_range(),
            );
            value
        } else {
            self.unknown()
        };

        let hir_id = self.db.alloc_hir(Hir::Pair(first.hir_id, rest.hir_id));
        let type_id = self.db.alloc_type(Type::Pair(PairType {
            first: first.type_id,
            rest: rest.type_id,
        }));

        Value::new(hir_id, type_id)
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
        let mut param_types = Vec::new();
        let mut rest = Rest::Nil;

        let len = lambda_expr.params().len();

        for (i, param) in lambda_expr.params().into_iter().enumerate() {
            let type_id = param
                .ty()
                .map(|ty| self.compile_type(ty))
                .or(expected
                    .as_ref()
                    .and_then(|expected| expected.param_types.get(i).copied()))
                .unwrap_or(self.builtins.unknown);

            param_types.push(type_id);

            if let Some(name) = param.name() {
                let symbol_id = self.db.alloc_symbol(Symbol::Parameter(type_id));
                scope.define_symbol(name.to_string(), symbol_id);
                self.db.insert_symbol_token(symbol_id, name);
            };

            if param.spread().is_some() {
                if i + 1 == len {
                    rest = Rest::Parameter;
                } else {
                    self.db
                        .error(ErrorKind::NonFinalSpread, param.syntax().text_range());
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
            .or(expected.map(|expected| expected.return_type));

        self.scope_stack.push(scope_id);
        let body = self.compile_expr(body, expected_return_type);
        self.scope_stack.pop().expect("lambda not in scope stack");

        let return_type = expected_return_type.unwrap_or(body.type_id);

        self.type_check(
            body.type_id,
            return_type,
            lambda_expr.body().unwrap().syntax().text_range(),
        );

        let ty = FunctionType {
            param_types: param_types.clone(),
            rest,
            return_type,
        };

        let symbol_id = self.db.alloc_symbol(Symbol::Function(Function {
            scope_id,
            hir_id: body.hir_id,
            ty: ty.clone(),
            inline: false,
        }));

        Value::new(
            self.db.alloc_hir(Hir::Reference(symbol_id)),
            self.db.alloc_type(Type::Function(ty)),
        )
    }

    fn compile_if_expr(&mut self, if_expr: IfExpr, expected_type: Option<TypeId>) -> Value {
        let condition = if_expr
            .condition()
            .map(|condition| self.compile_expr(condition, Some(self.builtins.bool)));

        if let Some(condition) = condition.as_ref() {
            self.type_guard_stack.push(condition.then_guards());
        }

        let then_block = if_expr
            .then_block()
            .map(|then_block| self.compile_block_expr(then_block, expected_type));

        if condition.is_some() {
            self.type_guard_stack.pop().unwrap();
        }

        if let Some(condition) = condition.as_ref() {
            self.type_guard_stack.push(condition.else_guards());
        }

        let expected_type =
            expected_type.or_else(|| then_block.as_ref().map(|then_block| then_block.type_id));

        let else_block = if_expr
            .else_block()
            .map(|else_block| self.compile_block_expr(else_block, expected_type));

        if condition.is_some() {
            self.type_guard_stack.pop().unwrap();
        }

        if let Some(condition_type) = condition.as_ref().map(|condition| condition.type_id) {
            self.type_check(
                condition_type,
                self.builtins.bool,
                if_expr.condition().unwrap().syntax().text_range(),
            );
        }

        if let (Some(then_block), Some(else_block)) = (&then_block, &else_block) {
            self.type_check(
                else_block.type_id,
                then_block.type_id,
                if_expr.else_block().unwrap().syntax().text_range(),
            );
        }

        let ty = then_block
            .as_ref()
            .or(else_block.as_ref())
            .map(|block| block.type_id)
            .unwrap_or(self.builtins.unknown);

        let value = condition.and_then(|condition| {
            then_block.and_then(|then_block| {
                else_block.map(|else_block| {
                    self.db.alloc_hir(Hir::If {
                        condition: condition.hir_id,
                        then_block: then_block.hir_id,
                        else_block: else_block.hir_id,
                    })
                })
            })
        });

        Value::new(value.unwrap_or(self.builtins.unknown_hir), ty)
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

        let mut allocator = Allocator::new();
        let ptr = allocator
            .new_number(num)
            .expect("number is too large to be represented in memory in an Allocator instance");

        Value::new(
            self.db
                .alloc_hir(Hir::Atom(allocator.atom(ptr).as_ref().to_vec())),
            self.builtins.int,
        )
    }

    fn compile_hex(&mut self, hex: SyntaxToken) -> Value {
        let Ok(bytes) = hex::decode(
            hex.text()
                .replace("0x", "")
                .replace("0X", "")
                .replace('_', ""),
        ) else {
            return Value::new(self.builtins.unknown_hir, self.builtins.bytes);
        };

        let bytes_len = bytes.len();

        Value::new(
            self.db.alloc_hir(Hir::Atom(bytes)),
            if bytes_len == 32 {
                self.builtins.bytes32
            } else if bytes_len == 48 {
                self.builtins.public_key
            } else {
                self.builtins.bytes
            },
        )
    }

    fn compile_string(&mut self, string: SyntaxToken) -> Value {
        let text = string.text();
        let quote = text.chars().next().unwrap();
        let after_prefix = &text[1..];
        let before_suffix = after_prefix.strip_suffix(quote).unwrap_or(after_prefix);

        let bytes = before_suffix.as_bytes();

        Value::new(
            self.db.alloc_hir(Hir::Atom(bytes.to_vec())),
            if bytes.len() == 32 {
                self.builtins.bytes32
            } else {
                self.builtins.bytes
            },
        )
    }

    fn compile_path_expr(&mut self, path: Path) -> Value {
        let mut idents = path.idents();

        if idents.len() > 1 {
            self.db
                .error(ErrorKind::PathNotAllowed, path.syntax().text_range());
            return self.unknown();
        }

        let name = idents.remove(0);

        let Some(symbol_id) = self
            .scope_stack
            .iter()
            .rev()
            .find_map(|&scope_id| self.db.scope(scope_id).symbol(name.text()))
        else {
            self.db.error(
                ErrorKind::UndefinedReference(name.to_string()),
                name.text_range(),
            );
            return self.unknown();
        };

        if !self.is_callee
            && matches!(
                self.db.symbol(symbol_id),
                Symbol::Function(Function { inline: true, .. })
            )
        {
            self.db.error(
                ErrorKind::InlineFunctionOutsideCall,
                path.syntax().text_range(),
            );
            return self.unknown();
        }

        Value::new(
            self.db.alloc_hir(Hir::Reference(symbol_id)),
            self.symbol_type(symbol_id)
                .unwrap_or_else(|| match self.db.symbol(symbol_id) {
                    Symbol::Unknown => unreachable!(),
                    Symbol::Function(Function { ty, .. }) => {
                        self.db.alloc_type(Type::Function(ty.clone()))
                    }
                    Symbol::Parameter(type_id) => *type_id,
                    Symbol::Let(Let { type_id, .. }) => *type_id,
                    Symbol::Const(Const { type_id, .. }) => *type_id,
                }),
        )
    }

    fn expected_param_type(
        &self,
        function_type: FunctionType,
        index: usize,
        spread: bool,
    ) -> Option<TypeId> {
        let param_types = function_type.param_types;
        let len = param_types.len();

        if index + 1 < len {
            return Some(param_types[index]);
        }

        if function_type.rest == Rest::Nil {
            if index + 1 == len {
                return Some(param_types[index]);
            } else {
                return None;
            }
        }

        if spread {
            return Some(param_types[len - 1]);
        }

        match self.db.ty(param_types[len - 1]) {
            Type::List(list_type) => Some(*list_type),
            _ => None,
        }
    }

    fn compile_function_call(&mut self, call: FunctionCall) -> Value {
        let Some(callee) = call.callee() else {
            return self.unknown();
        };

        self.is_callee = true;
        let callee = self.compile_expr(callee, None);

        let expected = match self.db.ty(callee.type_id) {
            Type::Function(function) => Some(function.clone()),
            _ => {
                self.db.error(
                    ErrorKind::UncallableType(self.type_name(callee.type_id)),
                    call.callee().unwrap().syntax().text_range(),
                );
                None
            }
        };

        let mut args = Vec::new();
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

            arg_types.push(value.type_id);

            if arg.spread().is_some() {
                if i + 1 == arg_len {
                    spread = true;
                } else {
                    self.db
                        .error(ErrorKind::NonFinalSpread, arg.syntax().text_range());
                }
            }

            args.push(value.hir_id);
        }

        args.reverse();
        arg_types.reverse();

        if let Some(expected) = expected.as_ref() {
            let param_len = expected.param_types.len();

            let too_few_args = arg_types.len() < param_len
                && !(expected.rest == Rest::Parameter && arg_types.len() == param_len - 1);
            let too_many_args = arg_types.len() > param_len && expected.rest == Rest::Nil;

            if too_few_args && expected.rest == Rest::Parameter {
                self.db.error(
                    ErrorKind::TooFewArgumentsWithVarargs {
                        expected: param_len - 1,
                        found: arg_types.len(),
                    },
                    call.syntax().text_range(),
                );
            } else if too_few_args || too_many_args {
                self.db.error(
                    ErrorKind::ArgumentMismatch {
                        expected: param_len,
                        found: arg_types.len(),
                    },
                    call.syntax().text_range(),
                );
            }

            for (i, arg) in arg_types.into_iter().enumerate() {
                if i + 1 == arg_len && spread && expected.rest == Rest::Nil {
                    self.db.error(
                        ErrorKind::NonVarargSpread,
                        call.args()[i].syntax().text_range(),
                    );
                    continue;
                }

                if i + 1 >= param_len
                    && (i + 1 < arg_len || !spread)
                    && expected.rest == Rest::Parameter
                {
                    match self.db.ty(expected.param_types.last().copied().unwrap()) {
                        Type::List(list_type) => {
                            self.type_check(arg, *list_type, call.args()[i].syntax().text_range());
                        }
                        _ => {
                            self.db.error(
                                ErrorKind::NonListVararg,
                                call.args()[i].syntax().text_range(),
                            );
                        }
                    }
                    continue;
                }

                if i + 1 == arg_len && spread && expected.rest == Rest::Parameter {
                    self.type_check(
                        arg,
                        expected.param_types[param_len - 1],
                        call.args()[i].syntax().text_range(),
                    );
                    continue;
                }

                self.type_check(
                    arg,
                    expected
                        .param_types
                        .get(i)
                        .copied()
                        .unwrap_or(self.builtins.unknown),
                    call.args()[i].syntax().text_range(),
                );
            }
        }

        let hir_id = self.db.alloc_hir(Hir::FunctionCall {
            callee: callee.hir_id,
            args,
            varargs: spread,
        });

        let type_id = expected
            .map(|expected| expected.return_type)
            .unwrap_or(self.builtins.unknown);

        Value::new(hir_id, type_id)
    }

    fn compile_type(&mut self, ty: AstType) -> TypeId {
        match ty {
            AstType::Path(path) => self.compile_path_type(path),
            AstType::ListType(list) => self.compile_list_type(list),
            AstType::FunctionType(function) => self.compile_function_type(function),
            AstType::PairType(tuple) => self.compile_pair_type(tuple),
            AstType::OptionalType(optional) => self.compile_optional_type(optional),
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
            self.db.error(
                ErrorKind::UndefinedType(name.to_string()),
                name.text_range(),
            );
            return self.builtins.unknown;
        };

        self.type_reference(ty);

        for name in idents {
            ty = self.path_into_type(ty, name.text(), name.text_range());
            self.type_reference(ty);
        }

        ty
    }

    fn type_reference(&mut self, referenced_type_id: TypeId) {
        if let Some(symbol_id) = self.symbol_stack.last() {
            self.sym
                .insert_symbol_type_reference(*symbol_id, referenced_type_id);
        }

        if let Some(type_id) = self.type_definition_stack.last() {
            self.sym
                .insert_type_type_reference(*type_id, referenced_type_id);
        }
    }

    fn path_into_type(&mut self, ty: TypeId, name: &str, range: TextRange) -> TypeId {
        match self.db.ty(ty) {
            Type::Enum(enum_type) => {
                if let Some(&variant_type) = enum_type.variants.get(name) {
                    return variant_type;
                }
                self.db
                    .error(ErrorKind::UnknownEnumVariant(name.to_string()), range);
                self.builtins.unknown
            }
            _ => {
                self.db
                    .error(ErrorKind::PathIntoNonEnum(self.type_name(ty)), range);
                self.builtins.unknown
            }
        }
    }

    fn compile_list_type(&mut self, list: ListType) -> TypeId {
        let Some(inner) = list.ty() else {
            return self.builtins.unknown;
        };

        let item_type = self.compile_type(inner);
        self.db.alloc_type(Type::List(item_type))
    }

    fn compile_pair_type(&mut self, pair_type: AstPairType) -> TypeId {
        let first = pair_type
            .first()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.builtins.unknown);

        let rest = pair_type
            .rest()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.builtins.unknown);

        self.db.alloc_type(Type::Pair(PairType { first, rest }))
    }

    fn compile_function_type(&mut self, function: AstFunctionType) -> TypeId {
        let mut param_types = Vec::new();
        let mut rest = Rest::Nil;

        let len = function.params().len();

        for (i, param) in function.params().into_iter().enumerate() {
            let type_id = param
                .ty()
                .map(|ty| self.compile_type(ty))
                .unwrap_or(self.builtins.unknown);

            param_types.push(type_id);

            if param.spread().is_some() {
                if i + 1 == len {
                    rest = Rest::Parameter;
                } else {
                    self.db
                        .error(ErrorKind::NonFinalSpread, param.syntax().text_range());
                }
            }
        }

        let return_type = function
            .ret()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.builtins.unknown);

        self.db.alloc_type(Type::Function(FunctionType {
            param_types,
            rest,
            return_type,
        }))
    }

    fn compile_optional_type(&mut self, optional: OptionalType) -> TypeId {
        let ty = optional
            .ty()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.builtins.unknown);

        if let Type::Optional(inner) = self.db.ty_raw(ty).clone() {
            self.db.warning(
                WarningKind::UselessOptionalType,
                optional.syntax().text_range(),
            );
            return inner;
        }

        self.db.alloc_type(Type::Optional(ty))
    }

    fn try_unwrap_optional(&mut self, ty: TypeId) -> TypeId {
        match self.db.ty(ty) {
            Type::Optional(inner) => self.try_unwrap_optional(*inner),
            _ => ty,
        }
    }

    fn detect_cycle(&mut self, type_id: TypeId, text_range: TextRange) -> bool {
        if self.db.is_cyclic(type_id) {
            self.db.error(ErrorKind::RecursiveTypeAlias, text_range);
            true
        } else {
            false
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
            Type::PublicKey => "PublicKey".to_string(),
            Type::List(items) => {
                let inner = self.type_name_visitor(*items, stack);
                format!("{}[]", inner)
            }
            Type::Pair(PairType { first, rest }) => {
                let first = self.type_name_visitor(*first, stack);
                let rest = self.type_name_visitor(*rest, stack);
                format!("({first}, {rest})")
            }
            Type::Struct(struct_type) => {
                let fields: Vec<String> = struct_type
                    .fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, self.type_name_visitor(*ty, stack)))
                    .collect();

                format!("{{ {} }}", fields.join(", "))
            }
            Type::Enum { .. } => "<unnamed enum>".to_string(),
            Type::EnumVariant(enum_variant) => {
                let enum_name = self.type_name_visitor(enum_variant.enum_type, stack);

                let fields: Vec<String> = enum_variant
                    .fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, self.type_name_visitor(*ty, stack)))
                    .collect();

                format!(
                    "{}::{} {{ {} }}",
                    enum_name,
                    enum_variant.name,
                    fields.join(", ")
                )
            }
            Type::Function(function_type) => {
                let params: Vec<String> = function_type
                    .param_types
                    .iter()
                    .map(|&ty| self.type_name_visitor(ty, stack))
                    .collect();

                let ret = self.type_name_visitor(function_type.return_type, stack);

                format!("fun({}) -> {}", params.join(", "), ret)
            }
            Type::Alias(..) => unreachable!(),
            Type::Optional(ty) => {
                let inner = self.type_name_visitor(*ty, stack);
                format!("{}?", inner)
            }
        };

        stack.pop().unwrap();

        name
    }

    fn type_check(&mut self, from: TypeId, to: TypeId, range: TextRange) {
        if self.db.compare_type(from, to) > Comparison::Assignable {
            self.db.error(
                ErrorKind::TypeMismatch {
                    expected: self.type_name(to),
                    found: self.type_name(from),
                },
                range,
            );
        }
    }

    fn cast_check(&mut self, from: TypeId, to: TypeId, range: TextRange) {
        if self.db.compare_type(from, to) > Comparison::Castable {
            self.db.error(
                ErrorKind::CastMismatch {
                    expected: self.type_name(to),
                    found: self.type_name(from),
                },
                range,
            );
        }
    }

    fn unknown(&self) -> Value {
        Value::new(self.builtins.unknown_hir, self.builtins.unknown)
    }

    fn symbol_type(&self, symbol_id: SymbolId) -> Option<TypeId> {
        for guards in self.type_guard_stack.iter() {
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
}
