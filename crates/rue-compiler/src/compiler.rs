#![allow(clippy::map_unwrap_or)]

use std::collections::HashMap;

pub(crate) use builtins::{builtins, Builtins};
use declarations::Declarations;
use indexmap::{IndexMap, IndexSet};
use rowan::TextRange;
use rue_parser::{
    AstNode, ConstItem, EnumItem, FunctionItem, Item, ModuleItem, Root, StructField, StructItem,
    TypeAliasItem,
};
use symbol_table::SymbolTable;

use crate::{
    database::{Database, HirId, ScopeId, SymbolId, TypeId},
    hir::Hir,
    scope::Scope,
    symbol::{Const, Function, Module, Symbol},
    ty::{EnumType, EnumVariant, FunctionType, PairType, Rest, StructType, Type, Value},
    Comparison, ErrorKind, TypeSystem,
};

mod block;
mod builtins;
mod declarations;
mod expr;
mod stmt;
mod symbol_table;
mod ty;
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

    /// Declare all items into scope without compiling their body.
    /// This ensures no circular references are resolved at this time.
    pub fn declare_items(&mut self, items: &[Item]) -> Declarations {
        let mut type_ids = Vec::new();
        let mut symbol_ids = Vec::new();
        let mut exported_types = Vec::new();
        let mut exported_symbols = Vec::new();

        for item in items {
            match item {
                Item::TypeAliasItem(ty) => type_ids.push(self.declare_type_alias(ty)),
                Item::StructItem(struct_item) => type_ids.push(self.declare_struct(struct_item)),
                Item::EnumItem(enum_item) => type_ids.push(self.declare_enum(enum_item)),
                Item::ModuleItem(..)
                | Item::FunctionItem(..)
                | Item::ConstItem(..)
                | Item::ImportItem(..) => continue,
            }

            if item.export().is_some() {
                exported_types.push(*type_ids.last().unwrap());
            }
        }

        for item in items {
            match item {
                Item::ModuleItem(module) => symbol_ids.push(self.declare_module(module)),
                Item::FunctionItem(function) => symbol_ids.push(self.declare_function(function)),
                Item::ConstItem(const_item) => symbol_ids.push(self.declare_const(const_item)),
                Item::TypeAliasItem(..)
                | Item::StructItem(..)
                | Item::EnumItem(..)
                | Item::ImportItem(..) => continue,
            }

            if item.export().is_some() {
                exported_symbols.push(*symbol_ids.last().unwrap());
            }
        }

        Declarations {
            type_ids,
            symbol_ids,
            exported_types,
            exported_symbols,
        }
    }

    pub fn declare_root(&mut self, root: &Root) -> (SymbolId, Declarations) {
        let scope_id = self.db.alloc_scope(Scope::default());
        let module_id = self.db.alloc_symbol(Symbol::Module(Module {
            scope_id,
            exported_symbols: IndexSet::new(),
            exported_types: IndexSet::new(),
        }));

        self.scope_stack.push(scope_id);
        let declarations = self.declare_items(&root.items());
        self.scope_stack.pop().unwrap();

        (module_id, declarations)
    }

    /// Compile the root by lowering all items into scope.
    pub fn compile_root(&mut self, root: &Root, module_id: SymbolId, declarations: Declarations) {
        let Symbol::Module(Module { scope_id, .. }) = self.db.symbol_mut(module_id).clone() else {
            unreachable!();
        };
        self.scope_stack.push(scope_id);
        self.compile_items(&root.items(), declarations);
        self.scope_stack.pop().unwrap();
    }

    /// Lower all of the items in the list in the proper order.
    /// This is done in two passes to handle forward references.
    fn compile_items(&mut self, items: &[Item], mut declarations: Declarations) {
        for item in items {
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
                Item::ModuleItem(..)
                | Item::FunctionItem(..)
                | Item::ConstItem(..)
                | Item::ImportItem(..) => {}
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
                Item::ModuleItem(..) => {
                    declarations.symbol_ids.remove(0);
                }
                Item::TypeAliasItem(..)
                | Item::StructItem(..)
                | Item::EnumItem(..)
                | Item::ImportItem(..) => {}
            }
        }
    }

    /// Define a module in the current scope.
    /// This creates a new scope for the module, and declares its items.
    /// The exports are added during this phase too.
    fn declare_module(&mut self, module_item: &ModuleItem) -> SymbolId {
        // Add the symbol to the stack early so you can track type references.
        let scope_id = self.db.alloc_scope(Scope::default());
        let symbol_id = self.db.alloc_symbol(Symbol::Module(Module {
            scope_id,
            exported_symbols: IndexSet::new(),
            exported_types: IndexSet::new(),
        }));

        if let Some(name) = module_item.name() {
            self.scope_mut().define_symbol(name.to_string(), symbol_id);
            self.db.insert_scope_token(scope_id, name.clone());
            self.db.insert_symbol_token(symbol_id, name);
        }

        // Add the symbol to the stack early so you can track type references.
        self.symbol_stack.push(symbol_id);
        self.scope_stack.push(scope_id);

        let items = module_item.items();
        let declarations = self.declare_items(&items);
        self.compile_items(&items, declarations.clone());

        self.scope_stack.pop().unwrap();
        self.symbol_stack.pop().unwrap();

        let Symbol::Module(Module {
            exported_symbols,
            exported_types,
            ..
        }) = self.db.symbol_mut(symbol_id)
        else {
            unreachable!();
        };
        exported_types.extend(declarations.exported_types);
        exported_symbols.extend(declarations.exported_symbols);

        symbol_id
    }

    /// Define a function in the current scope.
    /// This does not compile the function body, but it creates a new scope for it.
    /// Parameter symbols are defined now in the inner function scope.
    /// The function body is compiled later to allow for forward references.
    fn declare_function(&mut self, function_item: &FunctionItem) -> SymbolId {
        // Add the symbol to the stack early so you can track type references.
        let symbol_id = self.db.alloc_symbol(Symbol::Unknown);
        self.symbol_stack.push(symbol_id);

        let mut scope = Scope::default();

        let return_type = function_item
            .return_type()
            .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

        let mut param_types = Vec::new();
        let mut rest = Rest::Nil;

        let len = function_item.params().len();

        for (i, param) in function_item.params().into_iter().enumerate() {
            // Add the symbol to the stack early so you can track type references.
            let symbol_id = self.db.alloc_symbol(Symbol::Unknown);
            self.symbol_stack.push(symbol_id);

            let type_id = param
                .ty()
                .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

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

        if function_item.inline().is_some() {
            *self.db.symbol_mut(symbol_id) = Symbol::InlineFunction(Function {
                scope_id,
                hir_id,
                ty,
            });
        } else {
            *self.db.symbol_mut(symbol_id) = Symbol::Function(Function {
                scope_id,
                hir_id,
                ty,
            });
        }

        if let Some(name) = function_item.name() {
            self.scope_mut().define_symbol(name.to_string(), symbol_id);
            self.db.insert_scope_token(scope_id, name.clone());
            self.db.insert_symbol_token(symbol_id, name);
        }

        self.symbol_stack.pop().unwrap()
    }

    /// Define a constant in the current scope, but don't lower its body.
    fn declare_const(&mut self, const_item: &ConstItem) -> SymbolId {
        // Add the symbol to the stack early so you can track type references.
        let symbol_id = self.db.alloc_symbol(Symbol::Unknown);
        self.symbol_stack.push(symbol_id);

        let type_id = const_item
            .ty()
            .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

        let hir_id = self.db.alloc_hir(Hir::Unknown);

        if const_item.inline().is_some() {
            *self.db.symbol_mut(symbol_id) = Symbol::InlineConst(Const { type_id, hir_id });
        } else {
            *self.db.symbol_mut(symbol_id) = Symbol::Const(Const { type_id, hir_id });
        }

        if let Some(name) = const_item.name() {
            self.scope_mut().define_symbol(name.to_string(), symbol_id);
            self.db.insert_symbol_token(symbol_id, name);
        }

        self.symbol_stack.pop().unwrap()
    }

    /// Define a type for an alias in the current scope, but leave it as unknown for now.
    fn declare_type_alias(&mut self, type_alias: &TypeAliasItem) -> TypeId {
        let type_id = self.db.alloc_type(Type::Unknown);
        if let Some(name) = type_alias.name() {
            self.scope_mut().define_type(name.to_string(), type_id);
            self.db.insert_type_token(type_id, name);
        }
        type_id
    }

    /// Define a type for a struct in the current scope, but leave it as unknown for now.
    fn declare_struct(&mut self, struct_item: &StructItem) -> TypeId {
        let type_id = self.db.alloc_type(Type::Unknown);
        if let Some(name) = struct_item.name() {
            self.scope_mut().define_type(name.to_string(), type_id);
            self.db.insert_type_token(type_id, name);
        }
        type_id
    }

    /// Define a type for an enum in the current scope.
    /// This creates the enum variants as well, but they are left as unknown types.
    fn declare_enum(&mut self, enum_item: &EnumItem) -> TypeId {
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
    fn compile_function(&mut self, function: &FunctionItem, symbol_id: SymbolId) {
        let Some(body) = function.body() else {
            return;
        };

        let (Symbol::Function(Function { scope_id, ty, .. })
        | Symbol::InlineFunction(Function { scope_id, ty, .. })) =
            self.db.symbol(symbol_id).clone()
        else {
            unreachable!();
        };

        // We don't care about explicit returns in this context.
        self.scope_stack.push(scope_id);
        let value = self.compile_block(&body, Some(ty.return_type)).0;
        self.scope_stack.pop().unwrap();

        // Ensure that the body is assignable to the return type.
        self.type_check(
            value.type_id,
            ty.return_type,
            function.body().unwrap().syntax().text_range(),
        );

        // We ignore type guards here for now.
        // Just set the function body HIR.
        let (Symbol::Function(Function { hir_id, .. })
        | Symbol::InlineFunction(Function { hir_id, .. })) = self.db.symbol_mut(symbol_id)
        else {
            unreachable!();
        };
        *hir_id = value.hir_id;
    }

    /// Compiles a constant's value.
    fn compile_const(&mut self, const_item: &ConstItem, symbol_id: SymbolId) {
        let Some(expr) = const_item.expr() else {
            return;
        };

        let (Symbol::Const(Const { type_id, .. }) | Symbol::InlineConst(Const { type_id, .. })) =
            self.db.symbol(symbol_id).clone()
        else {
            unreachable!();
        };

        let output = self.compile_expr(&expr, Some(type_id));

        // Ensure that the expression is assignable to the constant's type.
        self.type_check(output.type_id, type_id, const_item.syntax().text_range());

        // We ignore type guards here for now.
        // Just set the constant HIR.
        let (Symbol::Const(Const { hir_id, .. }) | Symbol::InlineConst(Const { hir_id, .. })) =
            self.db.symbol_mut(symbol_id)
        else {
            unreachable!();
        };
        *hir_id = output.hir_id;
    }

    /// Compile and resolve the type that the alias points to.
    fn compile_type_alias(&mut self, ty: &TypeAliasItem, alias_type_id: TypeId) {
        self.type_definition_stack.push(alias_type_id);

        let type_id = ty
            .ty()
            .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

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
    fn compile_struct(&mut self, struct_item: &StructItem, type_id: TypeId) {
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
                .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

            if let Some(name) = field.name() {
                named_fields.insert(name.to_string(), type_id);
            };
        }

        named_fields
    }

    /// Compile and resolve an enum type, and each of its variants' struct fields.
    fn compile_enum(&mut self, enum_item: &EnumItem, type_id: TypeId) {
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
                .map_or(self.builtins.unknown_hir, |discriminant| {
                    self.compile_int(&discriminant).hir_id
                });

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
            }
            return None;
        }

        if spread {
            return Some(param_types[len - 1]);
        }

        match self.db.ty(param_types[len - 1]) {
            Type::List(list_type) => Some(*list_type),
            _ => None,
        }
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
                format!("{inner}[]")
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
                format!("{inner}?")
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
        for guards in &self.type_guard_stack {
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
