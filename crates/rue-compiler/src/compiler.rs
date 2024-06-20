use std::{
    collections::{HashMap, HashSet},
    fmt,
    str::FromStr,
};

use builtins::{builtins, Builtins};
use declarations::Declarations;
use indexmap::{IndexMap, IndexSet};
use num_bigint::BigInt;
use rowan::TextRange;
use rue_parser::{
    AstNode, BinaryExpr, BinaryOp, Block, CastExpr, ConstItem, EnumItem, Expr, FieldAccess,
    FunctionCall, FunctionItem, FunctionType as AstFunctionType, GroupExpr, GuardExpr, IfExpr,
    IfStmt, IndexAccess, InitializerExpr, InitializerField, Item, LambdaExpr, LetStmt, ListExpr,
    ListType, LiteralExpr, OptionalType, PairExpr, PairType, Path, PrefixExpr, PrefixOp, Root,
    Stmt, StructField, StructItem, SyntaxKind, SyntaxToken, Type as AstType, TypeAliasItem,
};
use symbol_table::SymbolTable;

use crate::{
    database::{Database, HirId, ScopeId, SymbolId, TypeId},
    hir::{BinOp, Hir},
    scope::Scope,
    symbol::Symbol,
    ty::{EnumType, EnumVariant, FunctionType, Guard, StructType, Type, Value},
    Diagnostic, DiagnosticKind, ErrorKind, WarningKind,
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

    // Diagnostics keep track of errors and warnings throughout the code.
    diagnostics: Vec<Diagnostic>,

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
            diagnostics: Vec::new(),
            sym: SymbolTable::default(),
            builtins,
        }
    }

    /// Lowering is completed, extract the diagnostics.
    pub fn finish(self) -> (SymbolTable, Vec<Diagnostic>) {
        (self.sym, self.diagnostics)
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

        let mut parameter_types = Vec::new();
        let mut varargs = false;

        let len = function_item.params().len();

        for (i, param) in function_item.params().into_iter().enumerate() {
            // Add the symbol to the stack early so you can track type references.
            let symbol_id = self.db.alloc_symbol(Symbol::Unknown);
            self.symbol_stack.push(symbol_id);

            let type_id = param
                .ty()
                .map(|ty| self.compile_type(ty))
                .unwrap_or(self.builtins.unknown);

            parameter_types.push(type_id);

            *self.db.symbol_mut(symbol_id) = Symbol::Parameter { type_id };

            if let Some(name) = param.name() {
                scope.define_symbol(name.to_string(), symbol_id);
                self.db.insert_symbol_token(symbol_id, name);
            }

            if param.spread().is_some() {
                if i + 1 == len {
                    varargs = true;
                } else {
                    self.error(ErrorKind::NonFinalSpread, param.syntax().text_range());
                }
            }

            self.symbol_stack.pop().unwrap();
        }

        let scope_id = self.db.alloc_scope(scope);
        let hir_id = self.db.alloc_hir(Hir::Unknown);

        let ty = FunctionType::new(parameter_types, return_type, varargs);

        *self.db.symbol_mut(symbol_id) = Symbol::Function {
            scope_id,
            hir_id,
            ty,
            inline: function_item.inline().is_some(),
        };

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

        *self.db.symbol_mut(symbol_id) = Symbol::ConstBinding {
            type_id,
            hir_id,
            inline: const_item.inline().is_some(),
        };

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

        let type_id = self.db.alloc_type(Type::Enum(EnumType::new(variants)));

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

        let Symbol::Function { scope_id, ty, .. } = self.db.symbol(symbol_id).clone() else {
            unreachable!();
        };

        // We don't care about explicit returns in this context.
        self.scope_stack.push(scope_id);
        let value = self.compile_block(body, Some(ty.return_type())).0;
        self.scope_stack.pop().unwrap();

        // Ensure that the body is assignable to the return type.
        self.type_check(
            value.ty(),
            ty.return_type(),
            function.body().unwrap().syntax().text_range(),
        );

        // We ignore type guards here for now.
        // Just set the function body HIR.
        let Symbol::Function { hir_id, .. } = self.db.symbol_mut(symbol_id) else {
            unreachable!();
        };
        *hir_id = value.hir();
    }

    /// Compiles a constant's value.
    fn compile_const(&mut self, const_item: ConstItem, symbol_id: SymbolId) {
        let Some(expr) = const_item.expr() else {
            return;
        };

        let Symbol::ConstBinding { type_id, .. } = self.db.symbol(symbol_id).clone() else {
            unreachable!();
        };

        let output = self.compile_expr(expr, Some(type_id));

        // Ensure that the expression is assignable to the constant's type.
        self.type_check(output.ty(), type_id, const_item.syntax().text_range());

        // We ignore type guards here for now.
        // Just set the constant HIR.
        let Symbol::ConstBinding { hir_id, .. } = self.db.symbol_mut(symbol_id) else {
            unreachable!();
        };
        *hir_id = output.hir();
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
        if self.detect_cycle(alias_type_id, ty.syntax().text_range(), &mut HashSet::new()) {
            *self.db.ty_mut(alias_type_id) = Type::Unknown;
        }

        self.type_definition_stack.pop().unwrap();
    }

    /// Compile and resolve a struct type.
    fn compile_struct(&mut self, struct_item: StructItem, type_id: TypeId) {
        self.type_definition_stack.push(type_id);
        let fields = self.compile_struct_fields(struct_item.fields());
        *self.db.ty_mut(type_id) = Type::Struct(StructType::new(fields));
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
                self.error(
                    ErrorKind::DuplicateEnumVariant(name.to_string()),
                    name.text_range(),
                );
                continue;
            }

            let variant_type = enum_type.variants()[name.text()];

            self.type_definition_stack.push(variant_type);

            let fields = self.compile_struct_fields(variant.fields());

            let discriminant = variant
                .discriminant()
                .map(|discriminant| self.compile_int(discriminant).hir())
                .unwrap_or(self.builtins.unknown_hir);

            *self.db.ty_mut(variant_type) = Type::EnumVariant(EnumVariant::new(
                name.to_string(),
                type_id,
                fields,
                discriminant,
            ));

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
            self.type_check(value.ty(), expected_type, let_stmt.syntax().text_range());
        }

        // If the name can't be resolved, there's no reason to continue compiling it.
        // We only do the above steps first to catch any other errors that may occur.
        let Some(name) = let_stmt.name() else {
            self.symbol_stack.pop().unwrap();
            return None;
        };

        *self.db.symbol_mut(symbol_id) = Symbol::LetBinding {
            type_id: expected_type.unwrap_or(value.ty()),
            hir_id: value.hir(),
        };

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
            condition.ty(),
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
                self.error(
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
            then_block.ty(),
            expected_type.unwrap_or(self.builtins.unknown),
            if_stmt.syntax().text_range(),
        );

        (condition.hir(), then_block.hir(), condition.else_guards())
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
                        value.ty(),
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
                        .map(|expr| self.compile_expr(expr, None).hir());

                    let hir_id = self.db.alloc_hir(Hir::Raise(value));

                    is_terminated = true;

                    statements.push(Statement::Return(Value::typed(
                        hir_id,
                        self.builtins.unknown,
                    )));
                }
                Stmt::AssertStmt(assert_stmt) => {
                    // Compile the condition expression.
                    let condition = assert_stmt
                        .expr()
                        .map(|condition| self.compile_expr(condition, Some(self.builtins.bool)))
                        .unwrap_or_else(|| self.unknown());

                    // Make sure that the condition is a boolean.
                    self.type_check(
                        condition.ty(),
                        self.builtins.bool,
                        assert_stmt.syntax().text_range(),
                    );

                    // If the condition is false, we raise an error.
                    // So we can assume that the condition is true from this point on.
                    // This will be popped in reverse order later after all statements have been lowered.
                    self.type_guard_stack.push(condition.then_guards());

                    let not_condition = self.db.alloc_hir(Hir::Not(condition.hir()));
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
            self.error(ErrorKind::EmptyBlock, block.syntax().text_range());
        }

        // Pop each statement in reverse order and mutate the body.
        for statement in statements.into_iter().rev() {
            match statement {
                Statement::Let(scope_id) => {
                    body = Value::typed(
                        self.db.alloc_hir(Hir::Definition {
                            scope_id,
                            hir_id: body.hir(),
                        }),
                        body.ty(),
                    );
                    self.scope_stack.pop().unwrap();
                }
                Statement::Return(value) => {
                    body = value;
                }
                Statement::If(condition, then_block) => {
                    self.type_guard_stack.pop().unwrap();

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

        (body, explicit_return)
    }

    /// Compile a block in the context of an expression.
    fn compile_block_expr(&mut self, block: Block, expected_type: Option<TypeId>) -> Value {
        let scope_id = self.db.alloc_scope(Scope::default());

        self.scope_stack.push(scope_id);
        let (value, explicit_return) = self.compile_block(block.clone(), expected_type);
        self.scope_stack.pop().unwrap();

        if explicit_return {
            self.error(ErrorKind::ExplicitReturnInExpr, block.syntax().text_range());
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
                    ty.unwrap(),
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
                value.ty(),
                expected_type.unwrap_or(self.builtins.unknown),
                field.syntax().text_range(),
            );

            let Some(name) = field.name() else {
                continue;
            };

            // Insert the field if it exists and hasn't already been assigned.
            if specified_fields.contains_key(name.text()) {
                self.error(
                    ErrorKind::DuplicateField(name.to_string()),
                    name.text_range(),
                );
            } else if !struct_fields.contains_key(name.text()) {
                self.error(
                    ErrorKind::UndefinedField {
                        field: name.to_string(),
                        ty: self.type_name(struct_type),
                    },
                    name.text_range(),
                );
            } else {
                specified_fields.insert(name.to_string(), value.hir());
            }
        }

        // Check for any missing fields and report them.
        let missing_fields: Vec<String> = struct_fields
            .keys()
            .filter(|name| !specified_fields.contains_key(*name))
            .cloned()
            .collect();

        if !missing_fields.is_empty() {
            self.error(
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

        match self.db.ty(value.ty()).clone() {
            Type::Struct(struct_type) => {
                if let Some(field) = struct_type.fields().get_full(field_name.text()) {
                    let (index, _, field_type) = field;
                    return Value::typed(
                        self.compile_index(value.hir(), index, false),
                        *field_type,
                    );
                } else {
                    self.error(
                        ErrorKind::UndefinedField {
                            field: field_name.to_string(),
                            ty: self.type_name(value.ty()),
                        },
                        field_name.text_range(),
                    );
                    return self.unknown();
                }
            }
            Type::Pair(left, right) => match field_name.text() {
                "first" => {
                    return Value::typed(self.db.alloc_hir(Hir::First(value.hir())), left);
                }
                "rest" => {
                    return Value::typed(self.db.alloc_hir(Hir::Rest(value.hir())), right);
                }
                _ => {}
            },
            Type::Bytes | Type::Bytes32 if field_name.text() == "length" => {
                return Value::typed(
                    self.db.alloc_hir(Hir::Strlen(value.hir())),
                    self.builtins.int,
                );
            }
            _ => {}
        }

        self.error(
            ErrorKind::InvalidFieldAccess {
                field: field_name.to_string(),
                ty: self.type_name(value.ty()),
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

        let Type::List(item_type) = self.db.ty(value.ty()).clone() else {
            self.error(
                ErrorKind::IndexAccess(self.type_name(value.ty())),
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

        let Some(op) = prefix_expr.op() else {
            return self.unknown();
        };

        match op {
            PrefixOp::Not => {
                self.type_check(
                    expr.ty(),
                    self.builtins.bool,
                    prefix_expr.syntax().text_range(),
                );

                let mut value =
                    Value::typed(self.db.alloc_hir(Hir::Not(expr.hir())), self.builtins.bool);

                for (symbol_id, guard) in expr.guards() {
                    value.guard(*symbol_id, guard.to_reversed());
                }

                value
            }
        }
    }

    fn compile_binary_expr(&mut self, binary: BinaryExpr) -> Value {
        let lhs = binary
            .lhs()
            .map(|lhs| self.compile_expr(lhs, None))
            .unwrap_or_else(|| self.unknown());

        let rhs = binary
            .rhs()
            .map(|rhs| self.compile_expr(rhs, None))
            .unwrap_or_else(|| self.unknown());

        let Some(op) = binary.op() else {
            return self.unknown();
        };

        let text_range = binary.syntax().text_range();

        let mut guards = HashMap::new();

        let (op, ty) = match op {
            BinaryOp::Add => {
                if self.is_assignable_to(
                    lhs.ty(),
                    self.builtins.public_key,
                    false,
                    &mut HashSet::new(),
                ) {
                    self.type_check(rhs.ty(), self.builtins.public_key, text_range);
                    (BinOp::PointAdd, self.builtins.public_key)
                } else if self.is_assignable_to(
                    lhs.ty(),
                    self.builtins.bytes,
                    false,
                    &mut HashSet::new(),
                ) {
                    self.type_check(rhs.ty(), self.builtins.bytes, text_range);
                    (BinOp::Concat, self.builtins.bytes)
                } else {
                    self.type_check(lhs.ty(), self.builtins.int, text_range);
                    self.type_check(rhs.ty(), self.builtins.int, text_range);
                    (BinOp::Add, self.builtins.int)
                }
            }
            BinaryOp::Subtract => {
                self.type_check(lhs.ty(), self.builtins.int, text_range);
                self.type_check(rhs.ty(), self.builtins.int, text_range);
                (BinOp::Subtract, self.builtins.int)
            }
            BinaryOp::Multiply => {
                self.type_check(lhs.ty(), self.builtins.int, text_range);
                self.type_check(rhs.ty(), self.builtins.int, text_range);
                (BinOp::Multiply, self.builtins.int)
            }
            BinaryOp::Divide => {
                self.type_check(lhs.ty(), self.builtins.int, text_range);
                self.type_check(rhs.ty(), self.builtins.int, text_range);
                (BinOp::Divide, self.builtins.int)
            }
            BinaryOp::Remainder => {
                self.type_check(lhs.ty(), self.builtins.int, text_range);
                self.type_check(rhs.ty(), self.builtins.int, text_range);
                (BinOp::Remainder, self.builtins.int)
            }
            BinaryOp::Equals => {
                if !self.is_atomic(lhs.ty(), &mut IndexSet::new())
                    || !self.is_atomic(rhs.ty(), &mut IndexSet::new())
                {
                    self.error(
                        ErrorKind::NonAtomEquality(self.type_name(lhs.ty())),
                        text_range,
                    );
                } else if self.is_assignable_to(
                    lhs.ty(),
                    self.builtins.nil,
                    false,
                    &mut HashSet::new(),
                ) {
                    if let Hir::Reference(symbol_id) = self.db.hir(rhs.hir()) {
                        guards.insert(
                            *symbol_id,
                            Guard::new(self.builtins.nil, self.try_unwrap_optional(rhs.ty())),
                        );
                    }
                } else if self.is_assignable_to(
                    rhs.ty(),
                    self.builtins.nil,
                    false,
                    &mut HashSet::new(),
                ) {
                    if let Hir::Reference(symbol_id) = self.db.hir(lhs.hir()) {
                        guards.insert(
                            *symbol_id,
                            Guard::new(self.builtins.nil, self.try_unwrap_optional(lhs.ty())),
                        );
                    }
                } else {
                    self.type_check(rhs.ty(), lhs.ty(), text_range);
                }

                (BinOp::Equals, self.builtins.bool)
            }
            BinaryOp::NotEquals => {
                if !self.is_atomic(lhs.ty(), &mut IndexSet::new())
                    || !self.is_atomic(rhs.ty(), &mut IndexSet::new())
                {
                    self.error(
                        ErrorKind::NonAtomEquality(self.type_name(lhs.ty())),
                        text_range,
                    );
                } else if self.is_assignable_to(
                    lhs.ty(),
                    self.builtins.nil,
                    false,
                    &mut HashSet::new(),
                ) {
                    if let Hir::Reference(symbol_id) = self.db.hir(rhs.hir()) {
                        guards.insert(
                            *symbol_id,
                            Guard::new(self.try_unwrap_optional(rhs.ty()), self.builtins.nil),
                        );
                    }
                } else if self.is_assignable_to(
                    rhs.ty(),
                    self.builtins.nil,
                    false,
                    &mut HashSet::new(),
                ) {
                    if let Hir::Reference(symbol_id) = self.db.hir(lhs.hir()) {
                        guards.insert(
                            *symbol_id,
                            Guard::new(self.try_unwrap_optional(lhs.ty()), self.builtins.nil),
                        );
                    }
                } else {
                    self.type_check(rhs.ty(), lhs.ty(), text_range);
                }

                (BinOp::NotEquals, self.builtins.bool)
            }
            BinaryOp::GreaterThan => {
                self.type_check(lhs.ty(), self.builtins.int, text_range);
                self.type_check(rhs.ty(), self.builtins.int, text_range);
                (BinOp::GreaterThan, self.builtins.bool)
            }
            BinaryOp::LessThan => {
                self.type_check(lhs.ty(), self.builtins.int, text_range);
                self.type_check(rhs.ty(), self.builtins.int, text_range);
                (BinOp::LessThan, self.builtins.bool)
            }
            BinaryOp::GreaterThanEquals => {
                self.type_check(lhs.ty(), self.builtins.int, text_range);
                self.type_check(rhs.ty(), self.builtins.int, text_range);
                (BinOp::GreaterThanEquals, self.builtins.bool)
            }
            BinaryOp::LessThanEquals => {
                self.type_check(lhs.ty(), self.builtins.int, text_range);
                self.type_check(rhs.ty(), self.builtins.int, text_range);
                (BinOp::LessThanEquals, self.builtins.bool)
            }
        };

        let mut value = Value::typed(
            self.db.alloc_hir(Hir::BinaryOp {
                op,
                lhs: lhs.hir(),
                rhs: rhs.hir(),
            }),
            ty,
        );

        for (symbol_id, guard) in guards {
            value.guard(symbol_id, guard);
        }

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
            .unwrap_or(self.builtins.unknown);

        let Some((guard, hir_id)) =
            self.guard_into(expr.ty(), ty, expr.hir(), guard.syntax().text_range())
        else {
            return Value::typed(self.builtins.unknown_hir, ty);
        };

        let mut value = Value::typed(hir_id, self.builtins.bool);

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
                WarningKind::RedundantTypeCheck(self.type_name(from)),
                text_range,
            );
            return Some((Guard::new(to, self.builtins.bool), hir_id));
        }

        match (self.db.ty(from).clone(), self.db.ty(to).clone()) {
            (Type::Any, Type::Pair(first, rest)) => {
                if !self.types_equal(first, self.builtins.any) {
                    self.error(ErrorKind::NonAnyPairTypeGuard, text_range);
                }

                if !self.types_equal(rest, self.builtins.any) {
                    self.error(ErrorKind::NonAnyPairTypeGuard, text_range);
                }

                let hir_id = self.db.alloc_hir(Hir::IsCons(hir_id));
                Some((Guard::new(to, self.builtins.bytes), hir_id))
            }
            (Type::Any, Type::Bytes) => {
                let pair_type = self
                    .db
                    .alloc_type(Type::Pair(self.builtins.any, self.builtins.any));
                let is_cons = self.db.alloc_hir(Hir::IsCons(hir_id));
                let hir_id = self.db.alloc_hir(Hir::Not(is_cons));
                Some((Guard::new(to, pair_type), hir_id))
            }
            (Type::List(inner), Type::Pair(first, rest)) => {
                if !self.types_equal(first, inner) {
                    self.error(ErrorKind::NonListPairTypeGuard, text_range);
                }

                if !self.types_equal(rest, from) {
                    self.error(ErrorKind::NonListPairTypeGuard, text_range);
                }

                let hir_id = self.db.alloc_hir(Hir::IsCons(hir_id));
                Some((Guard::new(to, self.builtins.nil), hir_id))
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
                self.error(
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
                Value::typed(self.db.alloc_hir(Hir::Atom(vec![1])), self.builtins.bool)
            }
            SyntaxKind::False => {
                Value::typed(self.db.alloc_hir(Hir::Atom(Vec::new())), self.builtins.bool)
            }
            SyntaxKind::Nil => {
                Value::typed(self.db.alloc_hir(Hir::Atom(Vec::new())), self.builtins.nil)
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

            if i == 0 && item_type.is_none() {
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
                    self.error(ErrorKind::NonFinalSpread, spread.text_range());
                }
            }

            items.push(output.hir());
        }

        let mut hir_id = self.builtins.nil_hir;

        for (i, item) in items.into_iter().rev().enumerate() {
            if i == 0 && !nil_terminated {
                hir_id = item;
            } else {
                hir_id = self.db.alloc_hir(Hir::Pair(item, hir_id));
            }
        }

        Value::typed(
            hir_id,
            self.db
                .alloc_type(Type::List(item_type.unwrap_or(self.builtins.unknown))),
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
                value.ty(),
                expected_rest.unwrap_or(self.builtins.unknown),
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
                .unwrap_or(self.builtins.unknown);

            parameter_types.push(type_id);

            if let Some(name) = param.name() {
                let symbol_id = self.db.alloc_symbol(Symbol::Parameter { type_id });
                scope.define_symbol(name.to_string(), symbol_id);
                self.db.insert_symbol_token(symbol_id, name);
            };

            if param.spread().is_some() {
                if i + 1 == len {
                    varargs = true;
                } else {
                    self.error(ErrorKind::NonFinalSpread, param.syntax().text_range());
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
            inline: false,
        });

        Value::typed(
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
            expected_type.or_else(|| then_block.as_ref().map(|then_block| then_block.ty()));

        let else_block = if_expr
            .else_block()
            .map(|else_block| self.compile_block_expr(else_block, expected_type));

        if condition.is_some() {
            self.type_guard_stack.pop().unwrap();
        }

        if let Some(condition_type) = condition.as_ref().map(|condition| condition.ty()) {
            self.type_check(
                condition_type,
                self.builtins.bool,
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
            .unwrap_or(self.builtins.unknown);

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

        Value::typed(value.unwrap_or(self.builtins.unknown_hir), ty)
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
            return Value::typed(self.builtins.unknown_hir, self.builtins.bytes);
        };

        let bytes_len = bytes.len();

        Value::typed(
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

        Value::typed(
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
            self.error(ErrorKind::PathNotAllowed, path.syntax().text_range());
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
                ErrorKind::UndefinedReference(name.to_string()),
                name.text_range(),
            );
            return self.unknown();
        };

        if !self.is_callee && self.db.symbol(symbol_id).is_inline_function() {
            self.error(
                ErrorKind::InlineFunctionOutsideCall,
                path.syntax().text_range(),
            );
            return self.unknown();
        }

        Value::typed(
            self.db.alloc_hir(Hir::Reference(symbol_id)),
            self.symbol_type(symbol_id)
                .unwrap_or_else(|| match self.db.symbol(symbol_id) {
                    Symbol::Unknown => unreachable!(),
                    Symbol::Function { ty, .. } => self.db.alloc_type(Type::Function(ty.clone())),
                    Symbol::Parameter { type_id } => *type_id,
                    Symbol::LetBinding { type_id, .. } => *type_id,
                    Symbol::ConstBinding { type_id, .. } => *type_id,
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

        self.is_callee = true;
        let callee = self.compile_expr(callee, None);

        let expected = match self.db.ty(callee.ty()) {
            Type::Function(function) => Some(function.clone()),
            _ => {
                self.error(
                    ErrorKind::UncallableType(self.type_name(callee.ty())),
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

            arg_types.push(value.ty());

            if arg.spread().is_some() {
                if i + 1 == arg_len {
                    spread = true;
                } else {
                    self.error(ErrorKind::NonFinalSpread, arg.syntax().text_range());
                }
            }

            args.push(value.hir());
        }

        args.reverse();
        arg_types.reverse();

        if let Some(expected) = expected.as_ref() {
            let param_len = expected.parameter_types().len();

            let too_few_args = arg_types.len() < param_len
                && !(expected.varargs() && arg_types.len() == param_len - 1);
            let too_many_args = arg_types.len() > param_len && !expected.varargs();

            if too_few_args && expected.varargs() {
                self.error(
                    ErrorKind::TooFewArgumentsWithVarargs {
                        expected: param_len - 1,
                        found: arg_types.len(),
                    },
                    call.syntax().text_range(),
                );
            } else if too_few_args || too_many_args {
                self.error(
                    ErrorKind::ArgumentMismatch {
                        expected: param_len,
                        found: arg_types.len(),
                    },
                    call.syntax().text_range(),
                );
            }

            for (i, arg) in arg_types.into_iter().enumerate() {
                if i + 1 == arg_len && spread && !expected.varargs() {
                    self.error(
                        ErrorKind::NonVarargSpread,
                        call.args()[i].syntax().text_range(),
                    );
                    continue;
                }

                if i + 1 >= param_len && (i + 1 < arg_len || !spread) && expected.varargs() {
                    match self
                        .db
                        .ty(expected.parameter_types().last().copied().unwrap())
                    {
                        Type::List(list_type) => {
                            self.type_check(arg, *list_type, call.args()[i].syntax().text_range());
                        }
                        _ => {
                            self.error(
                                ErrorKind::NonListVararg,
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
                        .unwrap_or(self.builtins.unknown),
                    call.args()[i].syntax().text_range(),
                );
            }
        }

        let hir_id = self.db.alloc_hir(Hir::FunctionCall {
            callee: callee.hir(),
            args,
            varargs: spread,
        });

        let type_id = expected
            .map(|expected| expected.return_type())
            .unwrap_or(self.builtins.unknown);

        Value::typed(hir_id, type_id)
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
            self.error(
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
                if let Some(&variant_type) = enum_type.variants().get(name) {
                    return variant_type;
                }
                self.error(ErrorKind::UnknownEnumVariant(name.to_string()), range);
                self.builtins.unknown
            }
            _ => {
                self.error(ErrorKind::PathIntoNonEnum(self.type_name(ty)), range);
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

    fn compile_pair_type(&mut self, pair_type: PairType) -> TypeId {
        let first = pair_type
            .first()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.builtins.unknown);

        let rest = pair_type
            .rest()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.builtins.unknown);

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
                .unwrap_or(self.builtins.unknown);

            parameter_types.push(type_id);

            if param.spread().is_some() {
                if i + 1 == len {
                    vararg = true;
                } else {
                    self.error(ErrorKind::NonFinalSpread, param.syntax().text_range());
                }
            }
        }

        let return_type = function
            .ret()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.builtins.unknown);

        self.db.alloc_type(Type::Function(FunctionType::new(
            parameter_types,
            return_type,
            vararg,
        )))
    }

    fn compile_optional_type(&mut self, optional: OptionalType) -> TypeId {
        let ty = optional
            .ty()
            .map(|ty| self.compile_type(ty))
            .unwrap_or(self.builtins.unknown);

        if let Type::Optional(inner) = self.db.ty_raw(ty).clone() {
            self.warning(
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
                    self.error(ErrorKind::RecursiveTypeAlias, text_range);
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
            | Type::Bytes32
            | Type::PublicKey => false,
            Type::Optional(ty) => self.detect_cycle(ty, text_range, visited_aliases),
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
            Type::Optional(ty) => {
                let inner = self.type_name_visitor(*ty, stack);
                format!("{}?", inner)
            }
        };

        stack.pop().unwrap();

        name
    }

    fn type_check(&mut self, from: TypeId, to: TypeId, range: TextRange) {
        if !self.is_assignable_to(from, to, false, &mut HashSet::new()) {
            self.error(
                ErrorKind::TypeMismatch {
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
                ErrorKind::CastMismatch {
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
            (Type::Bytes | Type::Nil, Type::Bytes) => true,
            (Type::Bytes32, Type::Bytes32 | Type::Bytes) => true,
            (Type::PublicKey, Type::PublicKey) => true,
            (_, Type::Any) => true,

            // Primitive casts.
            (Type::Nil, Type::Bool | Type::Int) if cast => true,
            (Type::Int, Type::Bytes) if cast => true,
            (Type::Bytes | Type::Bytes32 | Type::PublicKey, Type::Int) if cast => true,
            (Type::PublicKey, Type::Bytes) if cast => true,
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

            // Optional types are assignable to themselves.
            (Type::Optional(inner_a), Type::Optional(inner_b)) => {
                self.is_assignable_to(inner_a, inner_b, cast, visited)
            }

            // Either nil or inner type is assignable to optionals.
            (_, Type::Optional(inner_b)) => {
                self.types_equal(a, self.builtins.nil)
                    || self.is_assignable_to(a, inner_b, cast, visited)
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
            Type::PublicKey => matches!(b, Type::PublicKey),
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
            Type::Optional(ty) => {
                if let Type::Optional(other_ty) = b {
                    self.types_equal_visitor(ty, other_ty, visited)
                } else {
                    false
                }
            }
        };

        visited.remove(&key);

        equal
    }

    fn is_atomic(&self, ty: TypeId, stack: &mut IndexSet<TypeId>) -> bool {
        if !stack.insert(ty) {
            return false;
        }

        let is_atomic = match self.db.ty(ty) {
            Type::Nil | Type::Int | Type::Bool | Type::Bytes | Type::Bytes32 | Type::PublicKey => {
                true
            }
            Type::Optional(ty) => self.is_atomic(*ty, stack),
            _ => false,
        };

        stack.pop().unwrap();

        is_atomic
    }

    fn unknown(&self) -> Value {
        Value::typed(self.builtins.unknown_hir, self.builtins.unknown)
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

    fn error(&mut self, info: ErrorKind, range: TextRange) {
        self.diagnostics.push(Diagnostic::new(
            DiagnosticKind::Error(info),
            range.start().into()..range.end().into(),
        ));
    }

    fn warning(&mut self, info: WarningKind, range: TextRange) {
        self.diagnostics.push(Diagnostic::new(
            DiagnosticKind::Warning(info),
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