use rue_parser::{AstNode, FunctionItem};

use crate::{
    compiler::Compiler,
    hir::Hir,
    scope::Scope,
    symbol::{Function, Symbol},
    ty::{FunctionType, Rest, Type},
    ErrorKind, SymbolId,
};

impl Compiler<'_> {
    pub fn declare_function_item(&mut self, function_item: &FunctionItem) -> SymbolId {
        // Add the symbol to the stack so you can track type references.
        let symbol_id = self.db.alloc_symbol(Symbol::Unknown);
        self.symbol_stack.push(symbol_id);

        // Add the scope so you can track generic types.
        let scope_id = self.db.alloc_scope(Scope::default());
        self.scope_stack.push(scope_id);

        let mut generic_types = Vec::new();

        // Add the generic types to the scope.
        for generic_type in function_item
            .generic_types()
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

        let mut param_types = Vec::new();
        let mut rest = Rest::Nil;

        let params = function_item.params();
        let len = params.len();

        // Add the parameters to the scope and collect the parameter types.
        // Also keep track of and check the spread or optional parameter.
        for (i, param) in params.into_iter().enumerate() {
            // Add the symbol to the stack so you can track type references.
            let symbol_id = self.db.alloc_symbol(Symbol::Unknown);
            self.symbol_stack.push(symbol_id);

            // Compile the parameter's type, if present.
            // Otherwise, it's a parser error.
            let type_id = param
                .ty()
                .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

            // Add the parameter type to the list and update the parameter symbol.
            param_types.push(type_id);

            *self.db.symbol_mut(symbol_id) = Symbol::Parameter(if param.optional().is_some() {
                // If the parameter is optional, wrap the type in a possibly undefined type.
                // This prevents referencing the parameter until it's checked for undefined.
                self.db.alloc_type(Type::PossiblyUndefined(type_id))
            } else {
                // Otherwise, just use the type.
                type_id
            });

            // Add the parameter to the scope and define the token for the parameter.
            if let Some(name) = param.name() {
                if self.scope().symbol(name.text()).is_some() {
                    self.db.error(
                        ErrorKind::DuplicateSymbol(name.text().to_string()),
                        name.text_range(),
                    );
                }

                self.scope_mut().define_symbol(name.to_string(), symbol_id);
                self.db.insert_symbol_token(symbol_id, name);
            }

            // Check if it's a spread or optional parameter.
            let last = i + 1 == len;
            let spread = param.spread().is_some();
            let optional = param.optional().is_some();

            if spread && optional {
                self.db.error(
                    ErrorKind::OptionalParameterSpread,
                    param.syntax().text_range(),
                );
            } else if spread && !last {
                self.db.error(
                    ErrorKind::InvalidSpreadParameter,
                    param.syntax().text_range(),
                );
            } else if optional && !last {
                self.db.error(
                    ErrorKind::InvalidOptionalParameter,
                    param.syntax().text_range(),
                );
            } else if spread {
                rest = Rest::Spread;
            } else if optional {
                rest = Rest::Optional;
            }

            self.symbol_stack.pop().unwrap();
        }

        // Compile the return type, if present.
        // Otherwise, it's a parser error.
        let return_type = function_item
            .return_type()
            .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

        self.scope_stack.pop().unwrap();

        // We don't know the body yet, so we just allocate a placeholder HIR node.
        let hir_id = self.db.alloc_hir(Hir::Unknown);

        // Create the function's type.
        let ty = FunctionType {
            generic_types,
            param_types,
            rest,
            return_type,
        };

        // Update the symbol with the function.
        if function_item.inline().is_some() {
            *self.db.symbol_mut(symbol_id) = Symbol::InlineFunction(Function {
                scope_id,
                hir_id,
                ty,
            });
        } else {
            *self.db.symbol_mut(symbol_id) = Symbol::Function(Function {
                scope_id,
                hir_id,
                ty,
            });
        }

        // Add the function to the scope and define the token for the function and scope.
        if let Some(name) = function_item.name() {
            self.scope_mut().define_symbol(name.to_string(), symbol_id);
            self.db.insert_scope_token(scope_id, name.clone());
            self.db.insert_symbol_token(symbol_id, name);
        }

        self.symbol_stack.pop().unwrap()
    }

    pub fn compile_function_item(&mut self, function: &FunctionItem, symbol_id: SymbolId) {
        // The body is already unknown, so we don't need to do anything if it's not present.
        // This is a parser error, of course.
        let Some(body) = function.body() else {
            return;
        };

        // Get the function's scope and type.
        let (Symbol::Function(Function { scope_id, ty, .. })
        | Symbol::InlineFunction(Function { scope_id, ty, .. })) =
            self.db.symbol(symbol_id).clone()
        else {
            unreachable!();
        };

        // We don't care about explicit returns in this context.
        self.scope_stack.push(scope_id);
        self.allow_generic_inference_stack.push(false);
        let value = self.compile_block(&body, Some(ty.return_type)).0;
        self.allow_generic_inference_stack.pop().unwrap();
        self.scope_stack.pop().unwrap();

        // Ensure that the body is assignable to the return type.
        self.type_check(
            value.type_id,
            ty.return_type,
            function.body().unwrap().syntax().text_range(),
        );

        // Update the function's HIR with the body's HIR, for code generation purposes.
        let (Symbol::Function(Function { hir_id, .. })
        | Symbol::InlineFunction(Function { hir_id, .. })) = self.db.symbol_mut(symbol_id)
        else {
            unreachable!();
        };
        *hir_id = value.hir_id;
    }
}
