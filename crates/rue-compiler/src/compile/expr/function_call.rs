use rue_ast::{AstFunctionCallExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{FunctionCall, Hir, Value};
use rue_types::{Type, Union};

use crate::{Compiler, compile_expr};

pub fn compile_function_call_expr(ctx: &mut Compiler, call: &AstFunctionCallExpr) -> Value {
    let Some(expr) = call.expr() else {
        return ctx.builtins().unresolved.clone();
    };

    let expr = compile_expr(ctx, &expr, None);

    let expected_functions = rue_types::extract_functions(ctx.types_mut(), expr.ty);

    if expected_functions.len() > 1 {
        let name = ctx.type_name(expr.ty);
        ctx.diagnostic(
            call.syntax(),
            DiagnosticKind::CannotDisambiguateFunctionTypes(name),
        );
    }

    if expected_functions.is_empty() {
        let name = ctx.type_name(expr.ty);
        ctx.diagnostic(call.syntax(), DiagnosticKind::InvalidFunctionCall(name));
    }

    let expected_function = expected_functions.first();

    let mut args = Vec::new();
    let mut nil_terminated = true;
    let mut next_spreadable_type = None;

    let len = call.args().count();

    for (i, arg) in call.args().enumerate() {
        let is_spread = if let Some(spread) = arg.spread() {
            if i == len - 1 {
                if let Some(function) = expected_function {
                    if function.nil_terminated {
                        ctx.diagnostic(&spread, DiagnosticKind::InvalidSpread);
                    } else {
                        nil_terminated = false;
                    }
                }
                true
            } else {
                ctx.diagnostic(&spread, DiagnosticKind::NonFinalSpread);
                false
            }
        } else {
            false
        };

        let expected_type = if let Some(function) = expected_function {
            if function.nil_terminated || i < len - 1 {
                function.params.get(i).copied()
            } else {
                if next_spreadable_type.is_none() {
                    next_spreadable_type = Some(function.params.last().copied());
                }

                match next_spreadable_type {
                    None => unreachable!(),
                    Some(Some(ty)) if is_spread => {
                        next_spreadable_type = None;
                        Some(ty)
                    }
                    Some(Some(ty)) => {
                        let pairs = rue_types::extract_pairs(ctx.types_mut(), ty);

                        if pairs.is_empty() {
                            next_spreadable_type = None;
                            None
                        } else if pairs.len() == 1 {
                            next_spreadable_type = Some(Some(pairs[0].rest));

                            Some(pairs[0].first)
                        } else {
                            next_spreadable_type = Some(Some(ctx.alloc_type(Type::Union(
                                Union::new(pairs.iter().map(|pair| pair.rest).collect()),
                            ))));

                            Some(ctx.alloc_type(Type::Union(Union::new(
                                pairs.iter().map(|pair| pair.first).collect(),
                            ))))
                        }
                    }
                    Some(None) => None,
                }
            }
        } else {
            None
        };

        let value = if let Some(expr) = arg.expr() {
            compile_expr(ctx, &expr, expected_type)
        } else {
            ctx.builtins().unresolved.clone()
        };

        args.push(value.hir);
    }

    let ty = if expected_functions.is_empty() {
        ctx.builtins().unresolved.ty
    } else if expected_functions.len() == 1 {
        let function = expected_functions[0].clone();

        if !function.nil_terminated {
            if args.len() < function.params.len().saturating_sub(1) {
                ctx.diagnostic(
                    call.syntax(),
                    DiagnosticKind::ExpectedArgumentsBeforeSpread(
                        function.params.len(),
                        args.len(),
                    ),
                );
            }
        } else if args.len() != function.params.len() {
            ctx.diagnostic(
                call.syntax(),
                DiagnosticKind::ExpectedArgumentsExact(function.params.len(), args.len()),
            );
        }

        function.ret
    } else {
        ctx.alloc_type(Type::Union(Union::new(
            expected_functions
                .iter()
                .map(|function| function.ret)
                .collect(),
        )))
    };

    let hir = ctx.alloc_hir(Hir::FunctionCall(FunctionCall {
        function: expr.hir,
        args,
        nil_terminated,
    }));

    Value::new(hir, ty)
}
