use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use id_arena::Id;
use indexmap::{IndexMap, IndexSet};
use log::warn;
use rue_types::TypeId;

use crate::{Declaration, ImportId, SymbolId};

pub type ScopeId = Id<Scope>;

/// The kinds of names, each of which a scope binds separately, so that a value, a type and a
/// module can have the same name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Namespace {
    /// Functions, constants and bindings.
    Value,
    Type,
    Module,
}

impl Namespace {
    pub const ALL: [Self; 3] = [Self::Value, Self::Type, Self::Module];
}

/// The names bound in one namespace of a scope.
#[derive(Debug, Clone)]
struct Bindings<T> {
    items: IndexMap<String, T>,
    names: IndexMap<T, String>,
    imports: HashMap<T, ImportId>,
    exported: IndexSet<T>,
    blocked: HashSet<String>,
}

impl<T> Default for Bindings<T> {
    fn default() -> Self {
        Self {
            items: IndexMap::new(),
            names: IndexMap::new(),
            imports: HashMap::new(),
            exported: IndexSet::new(),
            blocked: HashSet::new(),
        }
    }
}

impl<T: Copy + Eq + Hash> Bindings<T> {
    fn insert(&mut self, name: String, item: T, exported: bool) {
        if self.items.contains_key(&name) {
            warn!("skipping duplicate item {name}");
            return;
        }

        self.items.insert(name.clone(), item);
        self.names.insert(item, name);

        if exported {
            self.exported.insert(item);
        }
    }

    fn get(&self, name: &str) -> Option<T> {
        self.items.get(name).copied()
    }

    fn get_with_import(&self, name: &str) -> Option<(T, Option<ImportId>)> {
        let item = self.get(name)?;
        Some((item, self.imports.get(&item).copied()))
    }

    fn name(&self, item: T) -> Option<&str> {
        self.names.get(&item).map(String::as_str)
    }

    fn names(&self) -> impl Iterator<Item = &str> {
        self.names.values().map(String::as_str)
    }

    fn exported(&self) -> impl Iterator<Item = (&str, T)> {
        self.exported
            .iter()
            .map(|item| (self.names.get(item).unwrap().as_str(), *item))
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    parent: Option<ScopeId>,
    symbols: Bindings<SymbolId>,
    types: Bindings<TypeId>,
    modules: Bindings<SymbolId>,
    symbol_types: HashMap<SymbolId, TypeId>,
    imports: Vec<ImportId>,
}

impl Scope {
    pub fn new(parent: Option<ScopeId>) -> Self {
        Self {
            parent,
            symbols: Bindings::default(),
            types: Bindings::default(),
            modules: Bindings::default(),
            symbol_types: HashMap::new(),
            imports: Vec::new(),
        }
    }

    pub fn parent(&self) -> Option<ScopeId> {
        self.parent
    }

    pub fn insert_symbol(&mut self, name: String, symbol: SymbolId, exported: bool) {
        self.symbols.insert(name, symbol, exported);
    }

    pub fn insert_type(&mut self, name: String, ty: TypeId, exported: bool) {
        self.types.insert(name, ty, exported);
    }

    pub fn insert_module(&mut self, name: String, module: SymbolId, exported: bool) {
        self.modules.insert(name, module, exported);
    }

    pub fn override_symbol_type(&mut self, symbol: SymbolId, ty: TypeId) {
        self.symbol_types.insert(symbol, ty);
    }

    pub fn is_symbol_exported(&self, symbol: SymbolId) -> bool {
        self.symbols.exported.contains(&symbol)
    }

    pub fn is_type_exported(&self, ty: TypeId) -> bool {
        self.types.exported.contains(&ty)
    }

    pub fn is_module_exported(&self, module: SymbolId) -> bool {
        self.modules.exported.contains(&module)
    }

    pub fn export_symbol(&mut self, symbol: SymbolId) {
        self.symbols.exported.insert(symbol);
    }

    pub fn export_type(&mut self, ty: TypeId) {
        self.types.exported.insert(ty);
    }

    pub fn export_module(&mut self, module: SymbolId) {
        self.modules.exported.insert(module);
    }

    pub fn exported_symbols(&self) -> impl Iterator<Item = (&str, SymbolId)> {
        self.symbols.exported()
    }

    pub fn exported_types(&self) -> impl Iterator<Item = (&str, TypeId)> {
        self.types.exported()
    }

    pub fn exported_modules(&self) -> impl Iterator<Item = (&str, SymbolId)> {
        self.modules.exported()
    }

    pub fn symbol(&self, name: &str) -> Option<SymbolId> {
        self.symbols.get(name)
    }

    /// The symbol with a name, and the import that brought it into this scope, if any.
    pub fn symbol_with_import(&self, name: &str) -> Option<(SymbolId, Option<ImportId>)> {
        self.symbols.get_with_import(name)
    }

    pub fn symbol_name(&self, symbol: SymbolId) -> Option<&str> {
        self.symbols.name(symbol)
    }

    pub fn ty(&self, name: &str) -> Option<TypeId> {
        self.types.get(name)
    }

    /// The type with a name, and the import that brought it into this scope, if any.
    pub fn type_with_import(&self, name: &str) -> Option<(TypeId, Option<ImportId>)> {
        self.types.get_with_import(name)
    }

    pub fn type_name(&self, ty: TypeId) -> Option<&str> {
        self.types.name(ty)
    }

    pub fn module(&self, name: &str) -> Option<SymbolId> {
        self.modules.get(name)
    }

    /// The module with a name, and the import that brought it into this scope, if any.
    pub fn module_with_import(&self, name: &str) -> Option<(SymbolId, Option<ImportId>)> {
        self.modules.get_with_import(name)
    }

    pub fn module_name(&self, module: SymbolId) -> Option<&str> {
        self.modules.name(module)
    }

    pub fn symbol_override_type(&self, symbol: SymbolId) -> Option<TypeId> {
        self.symbol_types.get(&symbol).copied()
    }

    pub fn symbol_names(&self) -> impl Iterator<Item = &str> {
        self.symbols.names()
    }

    pub fn type_names(&self) -> impl Iterator<Item = &str> {
        self.types.names()
    }

    pub fn module_names(&self) -> impl Iterator<Item = &str> {
        self.modules.names()
    }

    pub fn add_import(&mut self, import: ImportId) {
        self.imports.push(import);
    }

    pub fn imports(&self) -> impl Iterator<Item = ImportId> {
        self.imports.iter().copied()
    }

    pub fn add_symbol_import(&mut self, symbol: SymbolId, import: ImportId) {
        self.symbols.imports.insert(symbol, import);
    }

    pub fn add_type_import(&mut self, ty: TypeId, import: ImportId) {
        self.types.imports.insert(ty, import);
    }

    pub fn add_module_import(&mut self, module: SymbolId, import: ImportId) {
        self.modules.imports.insert(module, import);
    }

    /// Stops a symbol name from being looked up in enclosing scopes, because an import of it
    /// into this scope failed and has already been reported.
    pub fn block_symbol(&mut self, name: String) {
        self.symbols.blocked.insert(name);
    }

    /// Stops a type name from being looked up in enclosing scopes, because an import of it into
    /// this scope failed and has already been reported.
    pub fn block_type(&mut self, name: String) {
        self.types.blocked.insert(name);
    }

    /// Stops a module name from being looked up in enclosing scopes, because an import of it
    /// into this scope failed and has already been reported.
    pub fn block_module(&mut self, name: String) {
        self.modules.blocked.insert(name);
    }

    pub fn is_symbol_blocked(&self, name: &str) -> bool {
        self.symbols.blocked.contains(name)
    }

    pub fn is_type_blocked(&self, name: &str) -> bool {
        self.types.blocked.contains(name)
    }

    pub fn is_module_blocked(&self, name: &str) -> bool {
        self.modules.blocked.contains(name)
    }

    pub fn symbol_import(&self, symbol: SymbolId) -> Option<ImportId> {
        self.symbols.imports.get(&symbol).copied()
    }

    pub fn type_import(&self, ty: TypeId) -> Option<ImportId> {
        self.types.imports.get(&ty).copied()
    }

    pub fn module_import(&self, module: SymbolId) -> Option<ImportId> {
        self.modules.imports.get(&module).copied()
    }

    pub fn item(&self, namespace: Namespace, name: &str) -> Option<Declaration> {
        self.item_with_import(namespace, name).map(|(item, _)| item)
    }

    /// The item with a name in a namespace, and the import that brought it into this scope, if
    /// any.
    pub fn item_with_import(
        &self,
        namespace: Namespace,
        name: &str,
    ) -> Option<(Declaration, Option<ImportId>)> {
        match namespace {
            Namespace::Value => self
                .symbols
                .get_with_import(name)
                .map(|(symbol, via)| (Declaration::Symbol(symbol), via)),
            Namespace::Type => self
                .types
                .get_with_import(name)
                .map(|(ty, via)| (Declaration::Type(ty), via)),
            Namespace::Module => self
                .modules
                .get_with_import(name)
                .map(|(module, via)| (Declaration::Symbol(module), via)),
        }
    }

    /// The names and items in a namespace, in the order they were added.
    pub fn items(&self, namespace: Namespace) -> Vec<(String, Declaration)> {
        match namespace {
            Namespace::Value => collect(&self.symbols.items, Declaration::Symbol),
            Namespace::Type => collect(&self.types.items, Declaration::Type),
            Namespace::Module => collect(&self.modules.items, Declaration::Symbol),
        }
    }

    /// The names and exported items in a namespace, in the order they were exported.
    pub fn exported_items(&self, namespace: Namespace) -> Vec<(String, Declaration)> {
        match namespace {
            Namespace::Value => collect_exported(&self.symbols, Declaration::Symbol),
            Namespace::Type => collect_exported(&self.types, Declaration::Type),
            Namespace::Module => collect_exported(&self.modules, Declaration::Symbol),
        }
    }

    pub fn insert_item(
        &mut self,
        namespace: Namespace,
        name: String,
        item: Declaration,
        exported: bool,
    ) {
        match (namespace, item) {
            (Namespace::Value, Declaration::Symbol(symbol)) => {
                self.insert_symbol(name, symbol, exported);
            }
            (Namespace::Type, Declaration::Type(ty)) => self.insert_type(name, ty, exported),
            (Namespace::Module, Declaration::Symbol(module)) => {
                self.insert_module(name, module, exported);
            }
            _ => unreachable!("an item is in the namespace for its kind"),
        }
    }

    pub fn is_item_exported(&self, namespace: Namespace, item: Declaration) -> bool {
        match (namespace, item) {
            (Namespace::Value, Declaration::Symbol(symbol)) => self.is_symbol_exported(symbol),
            (Namespace::Type, Declaration::Type(ty)) => self.is_type_exported(ty),
            (Namespace::Module, Declaration::Symbol(module)) => self.is_module_exported(module),
            _ => unreachable!("an item is in the namespace for its kind"),
        }
    }

    pub fn export_item(&mut self, namespace: Namespace, item: Declaration) {
        match (namespace, item) {
            (Namespace::Value, Declaration::Symbol(symbol)) => self.export_symbol(symbol),
            (Namespace::Type, Declaration::Type(ty)) => self.export_type(ty),
            (Namespace::Module, Declaration::Symbol(module)) => self.export_module(module),
            _ => unreachable!("an item is in the namespace for its kind"),
        }
    }

    pub fn item_import(&self, namespace: Namespace, item: Declaration) -> Option<ImportId> {
        match (namespace, item) {
            (Namespace::Value, Declaration::Symbol(symbol)) => self.symbol_import(symbol),
            (Namespace::Type, Declaration::Type(ty)) => self.type_import(ty),
            (Namespace::Module, Declaration::Symbol(module)) => self.module_import(module),
            _ => unreachable!("an item is in the namespace for its kind"),
        }
    }

    pub fn add_item_import(&mut self, namespace: Namespace, item: Declaration, import: ImportId) {
        match (namespace, item) {
            (Namespace::Value, Declaration::Symbol(symbol)) => {
                self.add_symbol_import(symbol, import);
            }
            (Namespace::Type, Declaration::Type(ty)) => self.add_type_import(ty, import),
            (Namespace::Module, Declaration::Symbol(module)) => {
                self.add_module_import(module, import);
            }
            _ => unreachable!("an item is in the namespace for its kind"),
        }
    }

    /// Stops a name from being looked up in enclosing scopes, because an import of it into this
    /// scope failed and has already been reported.
    pub fn block(&mut self, namespace: Namespace, name: String) {
        match namespace {
            Namespace::Value => self.block_symbol(name),
            Namespace::Type => self.block_type(name),
            Namespace::Module => self.block_module(name),
        }
    }

    pub fn is_blocked(&self, namespace: Namespace, name: &str) -> bool {
        match namespace {
            Namespace::Value => self.is_symbol_blocked(name),
            Namespace::Type => self.is_type_blocked(name),
            Namespace::Module => self.is_module_blocked(name),
        }
    }
}

fn collect<T: Copy>(
    items: &IndexMap<String, T>,
    item: fn(T) -> Declaration,
) -> Vec<(String, Declaration)> {
    items
        .iter()
        .map(|(name, &value)| (name.clone(), item(value)))
        .collect()
}

fn collect_exported<T: Copy + Eq + Hash>(
    bindings: &Bindings<T>,
    item: fn(T) -> Declaration,
) -> Vec<(String, Declaration)> {
    bindings
        .exported()
        .map(|(name, value)| (name.to_string(), item(value)))
        .collect()
}
