use std::collections::HashMap;

use indexmap::IndexSet;
use rue_parser::{AstNode, LambdaExpr};
use rue_typing::{construct_items, deconstruct_items, Callable, Rest, Type, TypeId};

use crate::{
    compiler::Compiler,
    hir::Hir,
    scope::Scope,
    symbol::{Function, Symbol},
    value::Value,
    ErrorKind,
};

impl Compiler<'_> {
    pub fn compile_lambda_expr(
        &mut self,
        lambda_expr: &LambdaExpr,
        expected_type: Option<TypeId>,
    ) -> Value {
        // Precompute the substitutions based on generic types.
        let mut substitutions = HashMap::new();

        for generics in &self.generic_type_stack {
            substitutions.extend(generics);
        }

        // Determine the expected type of the lambda expression.
        let expected = expected_type.and_then(|ty| self.ty.get_callable(ty).cloned());
        let expected_params = expected.as_ref().and_then(|callable| {
            deconstruct_items(
                self.ty,
                callable.parameters,
                callable.parameter_names.len(),
                callable.rest,
            )
        });

        // Add the scope so you can track generic types.
        let scope_id = self.db.alloc_scope(Scope::default());
        self.scope_stack.push(scope_id);

        let mut generic_types = Vec::new();

        // Add the generic types to the scope.
        for generic_type in lambda_expr
            .generic_params()
            .map(|generics| generics.names())
            .unwrap_or_default()
        {
            // Create the generic type id.
            let type_id = self.ty.alloc(Type::Generic);

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
        let mut param_names = IndexSet::new();
        let mut rest = Rest::Nil;

        let len = lambda_expr.params().len();

        for (i, param) in lambda_expr.params().into_iter().enumerate() {
            // Determine the expected type of the parameter.
            let type_id = param
                .ty()
                .map(|ty| self.compile_type(ty))
                .or(expected_params
                    .as_ref()
                    .and_then(|expected| expected.get(i).copied()))
                .unwrap_or_else(|| {
                    self.db
                        .error(ErrorKind::CannotInferType, param.syntax().text_range());
                    self.ty.std().unknown
                });

            // Substitute generic types in the parameter type.
            let type_id = self.ty.substitute(type_id, substitutions.clone());

            param_types.push(type_id);

            if let Some(name) = param.name() {
                let param_type_id = if param.optional().is_some() {
                    // If the parameter is optional, wrap the type in a possibly undefined type.
                    // This prevents referencing the parameter until it's checked for undefined.
                    // TODO: self.ty.alloc(Type::Optional(type_id))
                    todo!()
                } else {
                    type_id
                };

                let symbol_id = self.db.alloc_symbol(Symbol::Parameter(param_type_id));
                self.scope_mut().define_symbol(name.to_string(), symbol_id);
                param_names.insert(name.to_string());
                self.db.insert_symbol_token(symbol_id, name);
            } else {
                param_names.insert(format!("#{i}"));
            };

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
        }

        let Some(body) = lambda_expr.body() else {
            return self.unknown();
        };

        let expected_return_type = lambda_expr
            .ty()
            .map(|ty| self.compile_type(ty))
            .or(expected.map(|expected| expected.return_type));

        self.allow_generic_inference_stack.push(false);
        let body = self.compile_expr(&body, expected_return_type);
        self.allow_generic_inference_stack.pop().unwrap();

        let return_type = expected_return_type.unwrap_or(body.type_id);

        self.scope_stack.pop().unwrap();

        self.type_check(
            body.type_id,
            return_type,
            lambda_expr.body().unwrap().syntax().text_range(),
        );

        let type_id = self.ty.alloc(Type::Unknown);
        let parameters = construct_items(self.ty, param_types.into_iter(), rest);

        *self.ty.get_mut(type_id) = Type::Callable(Callable {
            original_type_id: type_id,
            parameter_names: param_names,
            parameters,
            rest,
            return_type,
            generic_types: Vec::new(),
        });

        let symbol_id = self.db.alloc_symbol(Symbol::Function(Function {
            scope_id,
            hir_id: body.hir_id,
            type_id,
            rest,
        }));

        Value::new(
            self.db
                .alloc_hir(Hir::Reference(symbol_id, lambda_expr.syntax().text_range())),
            type_id,
        )
    }
}
