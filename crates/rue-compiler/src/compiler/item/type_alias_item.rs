use rue_parser::TypeAliasItem;

use crate::{
    compiler::Compiler,
    scope::Scope,
    value::{AliasType, Type},
    ErrorKind, TypeId,
};

impl Compiler<'_> {
    /// Define a type for an alias in the current scope, but leave it as unknown for now.
    pub fn declare_type_alias_item(&mut self, type_alias: &TypeAliasItem) -> TypeId {
        // Add the scope so you can track generic types.
        let scope_id = self.db.alloc_scope(Scope::default());
        self.scope_stack.push(scope_id);

        let mut generic_types = Vec::new();

        // Add the generic types to the scope.
        for generic_type in type_alias
            .generic_params()
            .map(|generics| generics.idents())
            .unwrap_or_default()
        {
            // Create the generic type id.
            let type_id = self.db.alloc_type(Type::Generic);

            // Check for duplicate generic types.
            if self.scope().ty(generic_type.text()).is_some() {
                self.db.error(
                    ErrorKind::DuplicateType(generic_type.text().to_string()),
                    generic_type.text_range(),
                );
            }

            // Add the generic type to the scope and define the token for the generic type.
            self.scope_mut()
                .define_type(generic_type.to_string(), type_id);

            self.db.insert_type_token(type_id, generic_type);

            // Add the generic type to the list so it can be added to the function type.
            generic_types.push(type_id);
        }

        self.scope_stack.pop().unwrap();

        let body_type_id = self.db.alloc_type(Type::Ref(self.builtins.unknown));

        let type_id = self.db.alloc_type(Type::Alias(AliasType {
            type_id: body_type_id,
            generic_types,
            scope_id,
        }));

        if let Some(name) = type_alias.name() {
            self.scope_mut().define_type(name.to_string(), type_id);
            self.db.insert_type_token(type_id, name);
        }

        type_id
    }

    /// Compile and resolve the type that the alias points to.
    pub fn compile_type_alias_item(&mut self, type_alias: &TypeAliasItem, type_id: TypeId) {
        self.type_definition_stack.push(type_id);

        let Type::Alias(alias_type) = self.db.ty(type_id).clone() else {
            unreachable!();
        };

        self.scope_stack.push(alias_type.scope_id);

        let body_type_id = type_alias
            .ty()
            .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

        self.scope_stack.pop().unwrap();

        // Set the alias type to the resolved type.
        let Type::Ref(inner) = self.db.ty_mut(alias_type.type_id) else {
            unreachable!();
        };

        *inner = body_type_id;

        self.type_definition_stack.pop().unwrap();
    }
}
