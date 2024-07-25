use rue_parser::{AstNode, FunctionItem};
use rue_typing::{construct_items, Callable, Rest, Type};

use crate::{
    compiler::Compiler,
    hir::Hir,
    scope::Scope,
    symbol::{Function, Symbol},
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
        for name in function_item
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

        let mut param_types = Vec::new();
        let mut param_names = Vec::new();
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
                .map_or(self.ty.std().unknown, |ty| self.compile_type(ty));

            // Add the parameter type to the list and update the parameter symbol.
            param_types.push(type_id);

            *self.db.symbol_mut(symbol_id) = Symbol::Parameter(if param.optional().is_some() {
                // If the parameter is optional, wrap the type in a possibly undefined type.
                // This prevents referencing the parameter until it's checked for undefined.
                // TODO: self.ty.alloc(Type::Optional(type_id))
                todo!()
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

                param_names.push(name.to_string());
                self.scope_mut().define_symbol(name.to_string(), symbol_id);
                self.db.insert_symbol_token(symbol_id, name);
            } else {
                param_names.push(format!("#{i}"));
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
            .map_or(self.ty.std().unknown, |ty| self.compile_type(ty));

        self.scope_stack.pop().unwrap();

        // We don't know the body yet, so we just allocate a placeholder HIR node.
        let hir_id = self.db.alloc_hir(Hir::Unknown);

        // Create the function's type.
        let type_id = self.ty.alloc(Type::Unknown);

        *self.ty.get_mut(type_id) = Type::Callable(Callable {
            original_type_id: type_id,
            parameter_names: param_names.into_iter().collect(),
            parameters: construct_items(self.ty, param_types.into_iter(), rest),
            return_type,
            rest,
            generic_types,
        });

        // Update the symbol with the function.
        if function_item.inline().is_some() {
            *self.db.symbol_mut(symbol_id) = Symbol::InlineFunction(Function {
                scope_id,
                hir_id,
                type_id,
                rest,
            });
        } else {
            *self.db.symbol_mut(symbol_id) = Symbol::Function(Function {
                scope_id,
                hir_id,
                type_id,
                rest,
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
        let (Symbol::Function(Function {
            scope_id, type_id, ..
        })
        | Symbol::InlineFunction(Function {
            scope_id, type_id, ..
        })) = self.db.symbol(symbol_id).clone()
        else {
            unreachable!();
        };

        let return_type = self
            .ty
            .get_callable(type_id)
            .map(|callable| callable.return_type);

        // We don't care about explicit returns in this context.
        self.scope_stack.push(scope_id);
        self.allow_generic_inference_stack.push(false);
        let value = self.compile_block(&body, return_type).value;
        self.allow_generic_inference_stack.pop().unwrap();
        self.scope_stack.pop().unwrap();

        // Ensure that the body is assignable to the return type.
        if let Some(return_type) = return_type {
            self.type_check(
                value.type_id,
                return_type,
                function.body().unwrap().syntax().text_range(),
            );
        }

        // Update the function's HIR with the body's HIR, for code generation purposes.
        let (Symbol::Function(Function { hir_id, .. })
        | Symbol::InlineFunction(Function { hir_id, .. })) = self.db.symbol_mut(symbol_id)
        else {
            unreachable!();
        };
        *hir_id = value.hir_id;
    }
}
