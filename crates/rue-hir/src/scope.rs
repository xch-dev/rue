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

/// An item bound in a scope, along with the namespace it's bound in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Item {
    Value(SymbolId),
    Type(TypeId),
    Module(SymbolId),
}

impl Item {
    pub fn namespace(self) -> Namespace {
        match self {
            Self::Value(_) => Namespace::Value,
            Self::Type(_) => Namespace::Type,
            Self::Module(_) => Namespace::Module,
        }
    }
}

impl From<Item> for Declaration {
    fn from(item: Item) -> Self {
        match item {
            Item::Value(symbol) | Item::Module(symbol) => Self::Symbol(symbol),
            Item::Type(ty) => Self::Type(ty),
        }
    }
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

    pub fn is_symbol_blocked(&self, name: &str) -> bool {
        self.symbols.blocked.contains(name)
    }

    pub fn is_type_blocked(&self, name: &str) -> bool {
        self.types.blocked.contains(name)
    }

    pub fn is_module_blocked(&self, name: &str) -> bool {
        self.modules.blocked.contains(name)
    }

    pub fn item(&self, namespace: Namespace, name: &str) -> Option<Item> {
        self.item_with_import(namespace, name).map(|(item, _)| item)
    }

    /// The item with a name in a namespace, and the import that brought it into this scope, if
    /// any.
    pub fn item_with_import(
        &self,
        namespace: Namespace,
        name: &str,
    ) -> Option<(Item, Option<ImportId>)> {
        match namespace {
            Namespace::Value => self
                .symbols
                .get_with_import(name)
                .map(|(symbol, via)| (Item::Value(symbol), via)),
            Namespace::Type => self
                .types
                .get_with_import(name)
                .map(|(ty, via)| (Item::Type(ty), via)),
            Namespace::Module => self
                .modules
                .get_with_import(name)
                .map(|(module, via)| (Item::Module(module), via)),
        }
    }

    /// The names and items in a namespace, in the order they were added.
    pub fn items(&self, namespace: Namespace) -> Vec<(String, Item)> {
        match namespace {
            Namespace::Value => collect(
                self.symbols.items.iter().map(|(n, &i)| (n.as_str(), i)),
                Item::Value,
            ),
            Namespace::Type => collect(
                self.types.items.iter().map(|(n, &i)| (n.as_str(), i)),
                Item::Type,
            ),
            Namespace::Module => collect(
                self.modules.items.iter().map(|(n, &i)| (n.as_str(), i)),
                Item::Module,
            ),
        }
    }

    /// The names and exported items in a namespace, in the order they were exported.
    pub fn exported_items(&self, namespace: Namespace) -> Vec<(String, Item)> {
        match namespace {
            Namespace::Value => collect(self.symbols.exported(), Item::Value),
            Namespace::Type => collect(self.types.exported(), Item::Type),
            Namespace::Module => collect(self.modules.exported(), Item::Module),
        }
    }

    pub fn insert_item(&mut self, name: String, item: Item, exported: bool) {
        match item {
            Item::Value(symbol) => self.symbols.insert(name, symbol, exported),
            Item::Type(ty) => self.types.insert(name, ty, exported),
            Item::Module(module) => self.modules.insert(name, module, exported),
        }
    }

    pub fn is_item_exported(&self, item: Item) -> bool {
        match item {
            Item::Value(symbol) => self.is_symbol_exported(symbol),
            Item::Type(ty) => self.is_type_exported(ty),
            Item::Module(module) => self.is_module_exported(module),
        }
    }

    pub fn export_item(&mut self, item: Item) {
        match item {
            Item::Value(symbol) => self.symbols.exported.insert(symbol),
            Item::Type(ty) => self.types.exported.insert(ty),
            Item::Module(module) => self.modules.exported.insert(module),
        };
    }

    /// The import that brought an item into this scope, if any.
    pub fn item_import(&self, item: Item) -> Option<ImportId> {
        match item {
            Item::Value(symbol) => self.symbols.imports.get(&symbol),
            Item::Type(ty) => self.types.imports.get(&ty),
            Item::Module(module) => self.modules.imports.get(&module),
        }
        .copied()
    }

    pub fn add_item_import(&mut self, item: Item, import: ImportId) {
        match item {
            Item::Value(symbol) => self.symbols.imports.insert(symbol, import),
            Item::Type(ty) => self.types.imports.insert(ty, import),
            Item::Module(module) => self.modules.imports.insert(module, import),
        };
    }

    /// Stops a name from being looked up in enclosing scopes, because an import of it into this
    /// scope failed and has already been reported.
    pub fn block(&mut self, namespace: Namespace, name: String) {
        self.blocked_names(namespace).insert(name);
    }

    pub fn is_blocked(&self, namespace: Namespace, name: &str) -> bool {
        match namespace {
            Namespace::Value => self.is_symbol_blocked(name),
            Namespace::Type => self.is_type_blocked(name),
            Namespace::Module => self.is_module_blocked(name),
        }
    }

    fn blocked_names(&mut self, namespace: Namespace) -> &mut HashSet<String> {
        match namespace {
            Namespace::Value => &mut self.symbols.blocked,
            Namespace::Type => &mut self.types.blocked,
            Namespace::Module => &mut self.modules.blocked,
        }
    }
}

fn collect<'a, T>(
    items: impl Iterator<Item = (&'a str, T)>,
    item: fn(T) -> Item,
) -> Vec<(String, Item)> {
    items
        .map(|(name, value)| (name.to_string(), item(value)))
        .collect()
}
