use std::{
    collections::{HashMap, VecDeque},
    mem,
};

use log::debug;
use rowan::TextRange;
use rue_ast::{AstExpr, AstFunctionCallExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{FunctionCall, Hir, HirId, Value};
use rue_types::{Pair, Type, TypeId, Union, substitute_with_mappings};

use crate::{Compiler, compile_expr};

pub fn compile_function_call_expr(ctx: &mut Compiler, call: &AstFunctionCallExpr) -> Value {
    let Some(expr) = call.expr() else {
        debug!("Unresolved function call expr");
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
    } else if expected_functions.is_empty() {
        let name = ctx.type_name(expr.ty);
        ctx.diagnostic(call.syntax(), DiagnosticKind::InvalidFunctionCall(name));
    }

    let expected_function = expected_functions.first();

    let len = call.args().count();

    let mut nil_terminated = true;
    let mut args = Vec::new();

    for (i, arg) in call.args().enumerate() {
        if let Some(spread) = arg.spread() {
            if i == len - 1 {
                nil_terminated = false;
            } else {
                ctx.diagnostic(&spread, DiagnosticKind::NonFinalSpread);
            }
        }

        args.extend(arg.expr());
    }

    let (args, mappings) = if let Some(function) = expected_function {
        resolve_call(
            ctx,
            call.syntax().text_range(),
            function.params.clone(),
            function.nil_terminated,
            args,
            nil_terminated,
        )
    } else {
        (vec![], HashMap::new())
    };

    let ty = if expected_functions.is_empty() {
        debug!("Unresolved function call return type due to unresolved function type");
        ctx.alloc_type(Type::Unresolved)
    } else if expected_functions.len() == 1 {
        expected_functions[0].ret
    } else {
        ctx.alloc_type(Type::Union(Union::new(
            expected_functions
                .iter()
                .map(|function| function.ret)
                .collect(),
        )))
    };

    let ty = substitute_with_mappings(ctx.types_mut(), ty, &mappings);

    let hir = ctx.alloc_hir(Hir::FunctionCall(FunctionCall {
        function: expr.hir,
        args,
        nil_terminated,
    }));

    Value::new(hir, ty)
}

fn resolve_call(
    ctx: &mut Compiler,
    range: TextRange,
    original_parameters: Vec<TypeId>,
    param_nil_terminated: bool,
    original_args: Vec<AstExpr>,
    arg_nil_terminated: bool,
) -> (Vec<HirId>, HashMap<TypeId, TypeId>) {
    let last_param_nullable = original_parameters.last().is_some_and(|&param| {
        ctx.is_assignable(ctx.builtins().nil.ty, param) && !param_nil_terminated
    });

    let minimum_parameters = original_parameters
        .len()
        .saturating_sub(usize::from(last_param_nullable));

    let mut remaining_parameters = VecDeque::from(original_parameters);
    let mut remaining_args = VecDeque::from(original_args);
    let mut flattened_arg_index = 0;
    let mut param_mismatch = false;
    let mut mappings = HashMap::new();
    let mut results = Vec::new();

    while !remaining_parameters.is_empty() || !remaining_args.is_empty() {
        let this_param_nil_terminated = param_nil_terminated || remaining_parameters.len() > 1;
        let this_arg_nil_terminated = arg_nil_terminated || remaining_args.len() > 1;

        let parameter = remaining_parameters.pop_front();
        let arg = remaining_args.pop_front();

        match (parameter, &arg) {
            (None, None) => unreachable!(),
            (Some(parameter), Some(expr)) => {
                match (this_param_nil_terminated, this_arg_nil_terminated) {
                    (true, true) | (false, false) => {
                        let value = compile_expr(ctx, expr, Some(parameter));
                        results.push(value.hir);
                        ctx.infer_type(expr.syntax(), value.ty, parameter, &mut mappings);
                    }
                    (true, false) => {
                        let mut remaining_ty = ctx.builtins().nil.ty;

                        for param in mem::take(&mut remaining_parameters).into_iter().rev() {
                            if param_nil_terminated {
                                remaining_ty =
                                    ctx.alloc_type(Type::Pair(Pair::new(param, remaining_ty)));
                            } else {
                                remaining_ty = param;
                            }
                        }

                        remaining_ty =
                            ctx.alloc_type(Type::Pair(Pair::new(parameter, remaining_ty)));

                        let value = compile_expr(ctx, expr, Some(remaining_ty));
                        results.push(value.hir);
                        ctx.infer_type(expr.syntax(), value.ty, remaining_ty, &mut mappings);
                    }
                    (false, true) => {
                        let pairs = rue_types::extract_pairs(ctx.types_mut(), parameter, false);

                        if pairs.is_empty() {
                            let type_name = ctx.type_name(parameter);
                            ctx.diagnostic(
                                expr.syntax(),
                                DiagnosticKind::CannotDestructureParameter(type_name),
                            );
                        } else if pairs.len() == 1 {
                            let value = compile_expr(ctx, expr, Some(pairs[0].first));
                            ctx.infer_type(expr.syntax(), value.ty, pairs[0].first, &mut mappings);
                            results.push(value.hir);
                            remaining_parameters.push_front(pairs[0].rest);
                        } else {
                            let first = ctx.alloc_type(Type::Union(Union::new(
                                pairs.iter().map(|pair| pair.first).collect(),
                            )));
                            let value = compile_expr(ctx, expr, Some(first));
                            results.push(value.hir);
                            ctx.infer_type(expr.syntax(), value.ty, first, &mut mappings);

                            let mut rests = Vec::new();

                            for pair in pairs {
                                if ctx.is_castable(value.ty, pair.first) {
                                    rests.push(pair.rest);
                                }
                            }

                            remaining_parameters.push_front(if rests.is_empty() {
                                ctx.alloc_type(Type::Never)
                            } else if rests.len() == 1 {
                                rests[0]
                            } else {
                                ctx.alloc_type(Type::Union(Union::new(rests)))
                            });
                        }
                    }
                }
            }
            (Some(parameter), None) => {
                if this_param_nil_terminated {
                    param_mismatch = true;
                } else {
                    ctx.infer_type(&range, ctx.builtins().nil.ty, parameter, &mut mappings);
                }
            }
            (None, Some(_)) => {
                param_mismatch = true;
            }
        }

        if arg.is_some() {
            flattened_arg_index += 1;
        }
    }

    if param_mismatch {
        if last_param_nullable {
            ctx.diagnostic(
                &range,
                DiagnosticKind::ExpectedArgumentsMinimum(minimum_parameters, flattened_arg_index),
            );
        } else {
            ctx.diagnostic(
                &range,
                DiagnosticKind::ExpectedArgumentsExact(minimum_parameters, flattened_arg_index),
            );
        }
    }

    (results, mappings)
}
