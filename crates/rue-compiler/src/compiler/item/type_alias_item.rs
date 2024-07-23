use rue_parser::TypeAliasItem;
use rue_typing::{Alias, Type, TypeId};

use crate::compiler::Compiler;

impl Compiler<'_> {
    /// Define a type for an alias in the current scope, but leave it as unknown for now.
    pub fn declare_type_alias_item(&mut self, type_alias: &TypeAliasItem) -> TypeId {
        let type_id = self.ty.alloc(Type::Unknown);
        if let Some(name) = type_alias.name() {
            self.scope_mut().define_type(name.to_string(), type_id);
            self.db.insert_type_token(type_id, name);
        }
        type_id
    }

    /// Compile and resolve the type that the alias points to.
    pub fn compile_type_alias_item(&mut self, type_alias: &TypeAliasItem, alias_type_id: TypeId) {
        self.type_definition_stack.push(alias_type_id);

        let type_id = type_alias
            .ty()
            .map_or(self.ty.std().unknown, |ty| self.compile_type(ty));

        // Set the alias type to the resolved type.
        *self.ty.get_mut(alias_type_id) = Type::Alias(Alias {
            original_type_id: None,
            type_id,
            generic_types: Vec::new(),
        });

        self.type_definition_stack.pop().unwrap();
    }
}
