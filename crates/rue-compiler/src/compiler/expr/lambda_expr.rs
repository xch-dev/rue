use std::collections::HashMap;

use rue_parser::{AstNode, LambdaExpr};

use crate::{
    compiler::Compiler,
    hir::Hir,
    scope::Scope,
    symbol::{Function, Symbol},
    value::{FunctionType, Rest, Type, Value},
    ErrorKind, TypeId,
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
        let expected = expected_type.and_then(|ty| match self.db.ty(ty) {
            Type::Function(function) => Some(function.clone()),
            _ => None,
        });

        let mut scope = Scope::default();
        let mut param_types = Vec::new();
        let mut rest = Rest::Nil;

        let len = lambda_expr.params().len();

        for (i, param) in lambda_expr.params().into_iter().enumerate() {
            // Determine the expected type of the parameter.
            let type_id = param
                .ty()
                .map(|ty| self.compile_type(ty))
                .or(expected
                    .as_ref()
                    .and_then(|expected| expected.param_types.get(i).copied()))
                .unwrap_or_else(|| {
                    self.db
                        .error(ErrorKind::CannotInferType, param.syntax().text_range());
                    self.builtins.unknown
                });

            // Substitute generic types in the parameter type.
            let type_id = self.db.substitute_type(type_id, &substitutions);

            param_types.push(type_id);

            if let Some(name) = param.name() {
                let param_type_id = if param.optional().is_some() {
                    // If the parameter is optional, wrap the type in a possibly undefined type.
                    // This prevents referencing the parameter until it's checked for undefined.
                    self.db.alloc_type(Type::Optional(type_id))
                } else {
                    type_id
                };

                let symbol_id = self.db.alloc_symbol(Symbol::Parameter(param_type_id));
                scope.define_symbol(name.to_string(), symbol_id);
                self.db.insert_symbol_token(symbol_id, name);
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

        let scope_id = self.db.alloc_scope(scope);

        let Some(body) = lambda_expr.body() else {
            return self.unknown();
        };

        let expected_return_type = lambda_expr
            .ty()
            .map(|ty| self.compile_type(ty))
            .or(expected.map(|expected| expected.return_type));

        self.scope_stack.push(scope_id);
        self.allow_generic_inference_stack.push(false);
        let body = self.compile_expr(&body, expected_return_type);
        self.allow_generic_inference_stack.pop().unwrap();
        self.scope_stack.pop().expect("lambda not in scope stack");

        let return_type = expected_return_type.unwrap_or(body.type_id);

        self.type_check(
            body.type_id,
            return_type,
            lambda_expr.body().unwrap().syntax().text_range(),
        );

        let ty = FunctionType {
            param_types: param_types.clone(),
            rest,
            return_type,
            generic_types: Vec::new(),
        };

        let symbol_id = self.db.alloc_symbol(Symbol::Function(Function {
            scope_id,
            hir_id: body.hir_id,
            ty: ty.clone(),
        }));

        Value::new(
            self.db
                .alloc_hir(Hir::Reference(symbol_id, lambda_expr.syntax().text_range())),
            self.db.alloc_type(Type::Function(ty)),
        )
    }
}
