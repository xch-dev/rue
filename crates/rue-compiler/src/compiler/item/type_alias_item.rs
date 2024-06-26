use rue_parser::TypeAliasItem;

use crate::{compiler::Compiler, value::Type, ErrorKind, TypeId};

impl Compiler<'_> {
    /// Define a type for an alias in the current scope, but leave it as unknown for now.
    pub fn declare_type_alias_item(&mut self, type_alias: &TypeAliasItem) -> TypeId {
        let type_id = self.db.alloc_type(Type::Unknown);
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
            .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

        // Set the alias type to the resolved type.
        *self.db.ty_mut(alias_type_id) = Type::Alias(type_id);

        // A cycle between type aliases has been detected.
        // We set it to unknown to prevent stack overflow issues later.
        if self.db.is_cyclic(alias_type_id) {
            let name = type_alias
                .name()
                .expect("the name should exist if it's in a cyclic reference");

            self.db.error(
                ErrorKind::RecursiveTypeAlias(name.to_string()),
                name.text_range(),
            );

            *self.db.ty_mut(alias_type_id) = Type::Unknown;
        }

        self.type_definition_stack.pop().unwrap();
    }
}
