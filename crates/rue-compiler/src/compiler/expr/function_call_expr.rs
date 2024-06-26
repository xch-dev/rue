use std::collections::HashMap;

use rue_parser::{AstNode, FunctionCallExpr};

use crate::{
    compiler::Compiler,
    hir::Hir,
    value::{FunctionType, Rest, Type, Value},
    ErrorKind, TypeId,
};

impl Compiler<'_> {
    pub fn compile_function_call_expr(&mut self, call: &FunctionCallExpr) -> Value {
        // Compile the callee expression.
        // We mark this expression as a callee to allow inline function references.
        self.is_callee = true;
        let callee = call.callee().map(|callee| self.compile_expr(&callee, None));

        // Get the function type of the callee.
        let function_type =
            callee
                .as_ref()
                .and_then(|callee| match self.db.ty(callee.type_id).clone() {
                    Type::Function(function_type) => Some(function_type),
                    _ => None,
                });

        // Make sure the callee is callable, if present.
        if let Some(callee) = callee.as_ref() {
            if function_type.is_none() {
                self.db.error(
                    ErrorKind::UncallableType(self.type_name(callee.type_id)),
                    call.callee().unwrap().syntax().text_range(),
                );
            }
        }

        // Push a generic type context for the function, and allow inference.
        self.generic_type_stack.push(HashMap::new());
        self.allow_generic_inference_stack.push(true);

        // Compile the arguments naively, and defer type checking until later.
        let mut args = Vec::new();
        let mut spread = false;

        let call_args = call.args();
        let len = call_args.len();

        for (i, arg) in call_args.into_iter().enumerate() {
            // Determine the expected type.
            let expected_type = function_type.as_ref().and_then(|ty| {
                if i < ty.param_types.len() {
                    Some(ty.param_types[i])
                } else if ty.rest == Rest::Spread {
                    self.db.unwrap_list(*ty.param_types.last().unwrap())
                } else {
                    None
                }
            });

            // Compile the argument expression, if present.
            // Otherwise, it's a parser error
            let expr = arg
                .expr()
                .map(|expr| self.compile_expr(&expr, expected_type))
                .unwrap_or_else(|| self.unknown());

            // Add the argument to the list.
            args.push(expr);

            // Check if it's a spread argument.
            if arg.spread().is_some() {
                if i == len - 1 {
                    spread = true;
                } else {
                    self.db
                        .error(ErrorKind::InvalidSpreadArgument, arg.syntax().text_range());
                }
            }
        }

        // Check that the arguments match the parameters.
        if let Some(function_type) = function_type.as_ref() {
            let arg_types = args.iter().map(|arg| arg.type_id).collect::<Vec<_>>();
            self.check_arguments(call, function_type, &arg_types, spread);
        }

        // The generic type context is no longer needed.
        let generic_types = self.generic_type_stack.pop().unwrap();
        self.allow_generic_inference_stack.pop().unwrap();

        // Calculate the return type.
        let mut type_id =
            function_type.map_or(self.builtins.unknown, |expected| expected.return_type);

        if !generic_types.is_empty() {
            type_id = self.db.substitute_type(type_id, &generic_types);
        }

        // Build the HIR for the function call.

        let hir_id = self.db.alloc_hir(Hir::FunctionCall {
            callee: callee.map_or(self.builtins.unknown_hir, |callee| callee.hir_id),
            args: args.iter().map(|arg| arg.hir_id).collect(),
            varargs: spread,
        });

        Value::new(hir_id, type_id)
    }

    fn check_arguments(
        &mut self,
        ast: &FunctionCallExpr,
        function: &FunctionType,
        args: &[TypeId],
        spread: bool,
    ) {
        match function.rest {
            Rest::Nil => {
                if args.len() != function.param_types.len() {
                    self.db.error(
                        ErrorKind::ArgumentMismatch(args.len(), function.param_types.len()),
                        ast.syntax().text_range(),
                    );
                }
            }
            Rest::Optional => {
                if args.len() != function.param_types.len()
                    && args.len() != function.param_types.len() - 1
                {
                    self.db.error(
                        ErrorKind::ArgumentMismatchOptional(args.len(), function.param_types.len()),
                        ast.syntax().text_range(),
                    );
                }
            }
            Rest::Spread => {
                if self
                    .db
                    .unwrap_list(*function.param_types.last().unwrap())
                    .is_some()
                {
                    if args.len() < function.param_types.len() - 1 {
                        self.db.error(
                            ErrorKind::ArgumentMismatchSpread(
                                args.len(),
                                function.param_types.len(),
                            ),
                            ast.syntax().text_range(),
                        );
                    }
                } else if args.len() != function.param_types.len() {
                    self.db.error(
                        ErrorKind::ArgumentMismatch(args.len(), function.param_types.len()),
                        ast.syntax().text_range(),
                    );
                }
            }
        }

        let ast_args = ast.args();

        for (i, &arg) in args.iter().enumerate() {
            let last = i == args.len() - 1;

            if last && spread {
                if function.rest != Rest::Spread {
                    self.db.error(
                        ErrorKind::UnsupportedFunctionSpread,
                        ast_args[i].syntax().text_range(),
                    );
                } else if i >= function.param_types.len() - 1 {
                    let expected_type = *function.param_types.last().unwrap();
                    self.type_check(arg, expected_type, ast_args[i].syntax().text_range());
                }
            } else if function.rest == Rest::Spread && i >= function.param_types.len() - 1 {
                if let Some(inner_list_type) =
                    self.db.unwrap_list(*function.param_types.last().unwrap())
                {
                    self.type_check(arg, inner_list_type, ast_args[i].syntax().text_range());
                } else if i == function.param_types.len() - 1 && !spread {
                    self.db.error(
                        ErrorKind::RequiredFunctionSpread,
                        ast_args[i].syntax().text_range(),
                    );
                }
            } else if i < function.param_types.len() {
                let param_type = function.param_types[i];
                self.type_check(arg, param_type, ast_args[i].syntax().text_range());
            }
        }
    }
}
