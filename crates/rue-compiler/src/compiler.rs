#![allow(clippy::map_unwrap_or)]

use std::collections::HashMap;

pub(crate) use builtins::Builtins;
use indexmap::IndexSet;
use rowan::TextRange;
use symbol_table::SymbolTable;

use crate::{
    database::{Database, HirId, ScopeId, SymbolId, TypeId},
    hir::Hir,
    scope::Scope,
    value::{PairType, Type, Value},
    ErrorKind,
};

mod block;
mod builtins;
mod context;
mod expr;
mod generic_types;
mod item;
mod path;
mod stmt;
mod symbol_table;
mod ty;

#[cfg(test)]
pub use builtins::*;
pub use context::*;

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

    // The generic type stack is used for overriding generic types that are being checked against.
    generic_type_stack: Vec<HashMap<TypeId, TypeId>>,

    // Whether or not generic type inference is allowed.
    allow_generic_inference_stack: Vec<bool>,

    // Whether the current expression is directly the callee of a function call.
    is_callee: bool,

    // The symbol table is used for storing all named symbols and types.
    // It also stored types referenced by symbols.
    sym: SymbolTable,

    // Common types and other values that are built-in to the compiler.
    builtins: Builtins,
}

impl<'a> Compiler<'a> {
    pub fn new(db: &'a mut Database, builtins: Builtins) -> Self {
        Self {
            db,
            scope_stack: vec![builtins.scope_id],
            symbol_stack: Vec::new(),
            type_definition_stack: Vec::new(),
            type_guard_stack: Vec::new(),
            generic_type_stack: Vec::new(),
            allow_generic_inference_stack: vec![false],
            is_callee: false,
            sym: SymbolTable::default(),
            builtins,
        }
    }

    /// Lowering is completed, extract the diagnostics.
    pub fn finish(self) -> SymbolTable {
        self.sym
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
            Type::Generic => "{generic}".to_string(),
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
                    match self.db.ty(enum_variant.enum_type) {
                        Type::Enum(enum_type) => {
                            enum_type
                                .variants
                                .iter()
                                .find(|item| *item.1 == ty)
                                .expect("enum type is missing variant")
                                .0
                                .clone()
                        }
                        _ => unreachable!(),
                    },
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
            Type::PossiblyUndefined(ty) => {
                let inner = self.type_name_visitor(*ty, stack);
                format!("possibly undefined {inner}")
            }
        };

        stack.pop().unwrap();

        name
    }

    fn type_check(&mut self, from: TypeId, to: TypeId, range: TextRange) {
        let comparison = if self.allow_generic_inference_stack.last().copied().unwrap() {
            self.db
                .compare_type_with_generics(from, to, &mut self.generic_type_stack)
        } else {
            self.db.compare_type(from, to)
        };

        if !comparison.is_assignable() {
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
        let comparison = if self.allow_generic_inference_stack.last().copied().unwrap() {
            self.db
                .compare_type_with_generics(from, to, &mut self.generic_type_stack)
        } else {
            self.db.compare_type(from, to)
        };

        if !comparison.is_castable() {
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

    fn scope(&self) -> &Scope {
        self.db
            .scope(self.scope_stack.last().copied().expect("no scope found"))
    }

    fn scope_mut(&mut self) -> &mut Scope {
        self.db
            .scope_mut(self.scope_stack.last().copied().expect("no scope found"))
    }
}
