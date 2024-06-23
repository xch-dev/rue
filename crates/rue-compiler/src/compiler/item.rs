use rue_parser::Item;

use crate::{SymbolId, TypeId};

use super::Compiler;

mod const_item;
mod enum_item;
mod function_item;
mod module_item;

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
}
