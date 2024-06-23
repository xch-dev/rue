use rue_parser::{AstNode, LambdaExpr};

use crate::{
    compiler::Compiler,
    hir::Hir,
    scope::Scope,
    symbol::{Function, Symbol},
    ty::{FunctionType, Rest, Type, Value},
    ErrorKind, TypeId,
};

impl Compiler<'_> {
    pub fn compile_lambda_expr(
        &mut self,
        lambda_expr: &LambdaExpr,
        expected_type: Option<TypeId>,
    ) -> Value {
        let expected = expected_type.and_then(|ty| match self.db.ty(ty) {
            Type::Function(function) => Some(function.clone()),
            _ => None,
        });

        let mut scope = Scope::default();
        let mut param_types = Vec::new();
        let mut rest = Rest::Nil;

        let len = lambda_expr.params().len();

        for (i, param) in lambda_expr.params().into_iter().enumerate() {
            let type_id = param
                .ty()
                .map(|ty| self.compile_type(ty))
                .or(expected
                    .as_ref()
                    .and_then(|expected| expected.param_types.get(i).copied()))
                .unwrap_or(self.builtins.unknown);

            param_types.push(type_id);

            if let Some(name) = param.name() {
                let symbol_id = self.db.alloc_symbol(Symbol::Parameter(type_id));
                scope.define_symbol(name.to_string(), symbol_id);
                self.db.insert_symbol_token(symbol_id, name);
            };

            if param.spread().is_some() {
                if i + 1 == len {
                    rest = Rest::Parameter;
                } else {
                    self.db
                        .error(ErrorKind::NonFinalSpread, param.syntax().text_range());
                }
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
        let body = self.compile_expr(&body, expected_return_type);
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
        };

        let symbol_id = self.db.alloc_symbol(Symbol::Function(Function {
            scope_id,
            hir_id: body.hir_id,
            ty: ty.clone(),
        }));

        Value::new(
            self.db.alloc_hir(Hir::Reference(symbol_id)),
            self.db.alloc_type(Type::Function(ty)),
        )
    }
}
