#![allow(clippy::map_unwrap_or)]

use std::collections::HashMap;

pub(crate) use builtins::{builtins, Builtins};
use indexmap::{IndexMap, IndexSet};
use rowan::TextRange;
use rue_parser::{AstNode, StructField, StructItem, TypeAliasItem};
use symbol_table::SymbolTable;

use crate::{
    database::{Database, HirId, ScopeId, SymbolId, TypeId},
    hir::Hir,
    scope::Scope,
    ty::{FunctionType, PairType, Rest, StructType, Type, Value},
    Comparison, ErrorKind, TypeSystem,
};

mod block;
mod builtins;
mod expr;
mod item;
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
