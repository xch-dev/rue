use rue_parser::{AstNode, FunctionCall};

use crate::{
    compiler::Compiler,
    hir::Hir,
    ty::{Rest, Type, Value},
    ErrorKind,
};

impl Compiler<'_> {
    pub fn compile_function_call_expr(&mut self, call: &FunctionCall) -> Value {
        let Some(callee) = call.callee() else {
            return self.unknown();
        };

        self.is_callee = true;
        let callee = self.compile_expr(&callee, None);

        let expected = if let Type::Function(fun) = self.db.ty(callee.type_id) {
            Some(fun.clone())
        } else {
            self.db.error(
                ErrorKind::UncallableType(self.type_name(callee.type_id)),
                call.callee().unwrap().syntax().text_range(),
            );
            None
        };

        let mut args = Vec::new();
        let mut arg_types = Vec::new();
        let mut spread = false;

        let arg_len = call.args().len();

        for (i, arg) in call.args().into_iter().enumerate().rev() {
            let expected_type = expected.as_ref().and_then(|expected| {
                self.expected_param_type(
                    expected.clone(),
                    i,
                    i + 1 == arg_len && arg.spread().is_some(),
                )
            });

            let value = arg
                .expr()
                .map(|expr| self.compile_expr(&expr, expected_type))
                .unwrap_or_else(|| self.unknown());

            arg_types.push(value.type_id);

            if arg.spread().is_some() {
                if i + 1 == arg_len {
                    spread = true;
                } else {
                    self.db
                        .error(ErrorKind::NonFinalSpread, arg.syntax().text_range());
                }
            }

            args.push(value.hir_id);
        }

        args.reverse();
        arg_types.reverse();

        if let Some(expected) = expected.as_ref() {
            let param_len = expected.param_types.len();

            let too_few_args = arg_types.len() < param_len
                && !(expected.rest == Rest::Parameter && arg_types.len() == param_len - 1);
            let too_many_args = arg_types.len() > param_len && expected.rest == Rest::Nil;

            if too_few_args && expected.rest == Rest::Parameter {
                self.db.error(
                    ErrorKind::TooFewArgumentsWithVarargs {
                        expected: param_len - 1,
                        found: arg_types.len(),
                    },
                    call.syntax().text_range(),
                );
            } else if too_few_args || too_many_args {
                self.db.error(
                    ErrorKind::ArgumentMismatch {
                        expected: param_len,
                        found: arg_types.len(),
                    },
                    call.syntax().text_range(),
                );
            }

            for (i, arg) in arg_types.into_iter().enumerate() {
                if i + 1 == arg_len && spread && expected.rest == Rest::Nil {
                    self.db.error(
                        ErrorKind::NonVarargSpread,
                        call.args()[i].syntax().text_range(),
                    );
                    continue;
                }

                if i + 1 >= param_len
                    && (i + 1 < arg_len || !spread)
                    && expected.rest == Rest::Parameter
                {
                    match self.db.ty(expected.param_types.last().copied().unwrap()) {
                        Type::List(list_type) => {
                            self.type_check(arg, *list_type, call.args()[i].syntax().text_range());
                        }
                        _ => {
                            self.db.error(
                                ErrorKind::NonListVararg,
                                call.args()[i].syntax().text_range(),
                            );
                        }
                    }
                    continue;
                }

                if i + 1 == arg_len && spread && expected.rest == Rest::Parameter {
                    self.type_check(
                        arg,
                        expected.param_types[param_len - 1],
                        call.args()[i].syntax().text_range(),
                    );
                    continue;
                }

                self.type_check(
                    arg,
                    expected
                        .param_types
                        .get(i)
                        .copied()
                        .unwrap_or(self.builtins.unknown),
                    call.args()[i].syntax().text_range(),
                );
            }
        }

        let hir_id = self.db.alloc_hir(Hir::FunctionCall {
            callee: callee.hir_id,
            args,
            varargs: spread,
        });

        let type_id = expected.map_or(self.builtins.unknown, |expected| expected.return_type);

        Value::new(hir_id, type_id)
    }
}
