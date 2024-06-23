use std::collections::HashSet;

use rue_parser::Item;

use crate::{symbol::Symbol, ty::Type, ErrorKind, SymbolId, TypeId};

use super::Compiler;

mod const_item;
mod enum_item;
mod function_item;
mod module_item;
mod struct_item;
mod type_alias_item;

#[derive(Debug, Clone)]
pub struct Declarations {
    pub type_ids: Vec<TypeId>,
    pub symbol_ids: Vec<SymbolId>,
    pub exported_types: Vec<TypeId>,
    pub exported_symbols: Vec<SymbolId>,
}

impl Compiler<'_> {
    /// Declare all items into scope without compiling their body.
    /// This ensures no circular references are resolved at this time.
    pub fn declare_items(&mut self, items: &[Item]) -> Declarations {
        self.check_item_names(items);

        let mut type_ids = Vec::new();
        let mut symbol_ids = Vec::new();
        let mut exported_types = Vec::new();
        let mut exported_symbols = Vec::new();

        for item in items {
            match item {
                Item::TypeAliasItem(ty) => type_ids.push(self.declare_type_alias_item(ty)),
                Item::StructItem(struct_item) => {
                    type_ids.push(self.declare_struct_item(struct_item));
                }
                Item::EnumItem(enum_item) => type_ids.push(self.declare_enum_item(enum_item)),
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
                Item::ModuleItem(module) => symbol_ids.push(self.declare_module_item(module)),
                Item::FunctionItem(function) => {
                    symbol_ids.push(self.declare_function_item(function));
                }
                Item::ConstItem(const_item) => symbol_ids.push(self.declare_const_item(const_item)),
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

    /// Lower all of the items in the list in the proper order.
    /// This is done in two passes to handle forward references.
    pub fn compile_items(&mut self, items: &[Item], mut declarations: Declarations) {
        for item in items {
            match item {
                Item::TypeAliasItem(ty) => {
                    let type_id = declarations.type_ids.remove(0);
                    self.type_definition_stack.push(type_id);
                    self.compile_type_alias_item(ty, type_id);
                    self.type_definition_stack.pop().unwrap();
                }
                Item::StructItem(struct_item) => {
                    let type_id = declarations.type_ids.remove(0);
                    self.type_definition_stack.push(type_id);
                    self.compile_struct_item(struct_item, type_id);
                    self.type_definition_stack.pop().unwrap();
                }
                Item::EnumItem(enum_item) => {
                    let type_id = declarations.type_ids.remove(0);
                    self.type_definition_stack.push(type_id);
                    self.compile_enum_item(enum_item, type_id);
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
                    self.compile_function_item(function, symbol_id);
                    self.symbol_stack.pop().unwrap();
                }
                Item::ConstItem(const_item) => {
                    let symbol_id = declarations.symbol_ids.remove(0);
                    self.symbol_stack.push(symbol_id);
                    self.compile_const_item(const_item, symbol_id);
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

    fn check_item_names(&mut self, items: &[Item]) {
        let mut type_names: HashSet<String> = self
            .scope()
            .local_types()
            .into_iter()
            .map(|type_id| self.scope().type_name(type_id).unwrap().to_string())
            .collect();

        let mut symbol_names: HashSet<String> = self
            .scope()
            .local_symbols()
            .into_iter()
            .map(|symbol_id| self.scope().symbol_name(symbol_id).unwrap().to_string())
            .collect();

        let mut symbol_namespaces: HashSet<String> = self
            .scope()
            .local_symbols()
            .into_iter()
            .filter_map(|symbol_id| {
                if let Symbol::Module(..) = self.db.symbol(symbol_id).clone() {
                    Some(self.scope().symbol_name(symbol_id).unwrap().to_string())
                } else {
                    None
                }
            })
            .collect();

        let mut type_namespaces: HashSet<String> = self
            .scope()
            .local_types()
            .into_iter()
            .filter_map(|type_id| {
                if let Type::Enum(..) = self.db.ty(type_id).clone() {
                    Some(self.scope().type_name(type_id).unwrap().to_string())
                } else {
                    None
                }
            })
            .collect();

        for item in items {
            match item {
                Item::ConstItem(item) => {
                    let Some(name) = item.name() else {
                        continue;
                    };

                    if !symbol_names.insert(name.to_string()) {
                        self.db.error(
                            ErrorKind::DuplicateSymbol(name.to_string()),
                            name.text_range(),
                        );
                    }
                }
                Item::FunctionItem(item) => {
                    let Some(name) = item.name() else {
                        continue;
                    };

                    if !symbol_names.insert(name.to_string()) {
                        self.db.error(
                            ErrorKind::DuplicateSymbol(name.to_string()),
                            name.text_range(),
                        );
                    }
                }
                Item::ModuleItem(item) => {
                    let Some(name) = item.name() else {
                        continue;
                    };

                    if !symbol_names.insert(name.to_string()) {
                        self.db.error(
                            ErrorKind::DuplicateSymbol(name.to_string()),
                            name.text_range(),
                        );
                    }

                    if type_namespaces.contains(&name.to_string()) {
                        self.db.error(
                            ErrorKind::NamespaceTakenType(name.to_string()),
                            name.text_range(),
                        );
                    }

                    symbol_namespaces.insert(name.to_string());
                }
                Item::EnumItem(item) => {
                    let Some(name) = item.name() else {
                        continue;
                    };

                    if !type_names.insert(name.to_string()) {
                        self.db.error(
                            ErrorKind::DuplicateType(name.to_string()),
                            name.text_range(),
                        );
                    }

                    if symbol_namespaces.contains(&name.to_string()) {
                        self.db.error(
                            ErrorKind::NamespaceTakenSymbol(name.to_string()),
                            name.text_range(),
                        );
                    }

                    type_namespaces.insert(name.to_string());
                }
                Item::StructItem(item) => {
                    let Some(name) = item.name() else {
                        continue;
                    };

                    if !type_names.insert(name.to_string()) {
                        self.db.error(
                            ErrorKind::DuplicateType(name.to_string()),
                            name.text_range(),
                        );
                    }
                }
                Item::TypeAliasItem(item) => {
                    let Some(name) = item.name() else {
                        continue;
                    };

                    if !type_names.insert(name.to_string()) {
                        self.db.error(
                            ErrorKind::DuplicateType(name.to_string()),
                            name.text_range(),
                        );
                    }
                }
                Item::ImportItem(_item) => todo!(),
            }
        }
    }
}
