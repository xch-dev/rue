use rue_parser::TypeAliasItem;
use rue_typing::{Alias, Type, TypeId};

use crate::{Compiler, ErrorKind, Scope, ScopeId};

impl Compiler<'_> {
    /// Define a type for an alias in the current scope, but leave it as unknown for now.
    pub fn declare_type_alias_item(&mut self, type_alias: &TypeAliasItem) -> (TypeId, ScopeId) {
        // Add the scope so you can track generic types.
        let scope_id = self.db.alloc_scope(Scope::default());
        self.scope_stack.push(scope_id);

        let mut generic_types = Vec::new();

        // Add the generic types to the scope.
        for name in type_alias
            .generic_params()
            .map(|generics| generics.names())
            .unwrap_or_default()
        {
            // Create the generic type id.
            let type_id = self.ty.alloc(Type::Generic);

            // Check for duplicate generic types.
            if self.scope().ty(name.text()).is_some() {
                self.db.error(
                    ErrorKind::DuplicateType(name.text().to_string()),
                    name.text_range(),
                );
            }

            // Add the generic type to the scope and define the token for the generic type.
            self.scope_mut().define_type(name.to_string(), type_id);

            self.db.insert_type_token(type_id, name);

            // Add the generic type to the list so it can be added to the function type.
            generic_types.push(type_id);
        }

        self.scope_stack.pop().unwrap();

        // Create the alias type.
        let ref_type_id = self.ty.alloc(Type::Ref(self.ty.std().unknown));

        let type_id = self.ty.alloc(Type::Unknown);

        *self.ty.get_mut(type_id) = Type::Alias(Alias {
            original_type_id: type_id,
            type_id: ref_type_id,
            generic_types,
        });

        if let Some(name) = type_alias.name() {
            self.scope_mut().define_type(name.to_string(), type_id);
            self.db.insert_type_token(type_id, name);
        }

        (type_id, scope_id)
    }

    /// Compile and resolve the type that the alias points to.
    pub fn compile_type_alias_item(
        &mut self,
        type_alias: &TypeAliasItem,
        alias_type_id: TypeId,
        scope_id: ScopeId,
    ) {
        self.type_definition_stack.push(alias_type_id);

        // Add the scope so you can use generic types.
        self.scope_stack.push(scope_id);
        let type_id = type_alias
            .ty()
            .map_or(self.ty.std().unknown, |ty| self.compile_type(ty));
        self.scope_stack.pop().unwrap();

        // Set the alias type to the resolved type.
        let Type::Alias(Alias {
            type_id: ref_type_id,
            ..
        }) = self.ty.get(alias_type_id).clone()
        else {
            unreachable!();
        };

        let Type::Ref(reference) = self.ty.get_raw_mut(ref_type_id) else {
            unreachable!();
        };

        *reference = type_id;

        self.type_definition_stack.pop().unwrap();
    }
}
