use std::collections::HashMap;

use log::debug;
use rue_ast::{AstFunctionCallExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{FunctionCall, Hir, Symbol, Value, VerificationFunctionSymbol};
use rue_types::{Type, Union, substitute_with_mappings};

use crate::{Compiler, compile_expr};

pub fn compile_function_call_expr(ctx: &mut Compiler, call: &AstFunctionCallExpr) -> Value {
    let Some(expr) = call.expr() else {
        debug!("Unresolved function call expr");
        return ctx.builtins().unresolved.clone();
    };

    let expr = compile_expr(ctx, &expr, None);

    if let Hir::Reference(symbol) = ctx.hir(expr.hir).clone()
        && let Symbol::VerificationFunction(verification) = ctx.symbol(symbol).clone()
    {
        return compile_verification_function_call(ctx, call, verification);
    }

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
        if function.nil_terminated != nil_terminated {
            ctx.diagnostic(call.syntax(), DiagnosticKind::InvalidSpread);
        }

        if function.params.len() != args.len() {
            ctx.diagnostic(
                call.syntax(),
                DiagnosticKind::ExpectedArguments(function.params.len(), args.len()),
            );
        }

        let mut mappings = HashMap::new();
        let mut results = Vec::new();

        for (i, &param) in function.params.iter().enumerate() {
            if let Some(expr) = args.get(i) {
                let value = compile_expr(ctx, expr, Some(param));
                results.push(value.hir);
                ctx.infer_type(expr.syntax(), value.ty, param, &mut mappings);
            } else {
                debug!("Unresolved function call argument");
                results.push(ctx.builtins().unresolved.hir);
            }
        }

        (results, mappings)
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

fn compile_verification_function_call(
    ctx: &mut Compiler,
    call: &AstFunctionCallExpr,
    verification: VerificationFunctionSymbol,
) -> Value {
    let mut args = Vec::new();

    for arg in call.args() {
        if arg.spread().is_some() {
            ctx.diagnostic(arg.syntax(), DiagnosticKind::InvalidSpread);
        }

        let Some(expr) = arg.expr() else {
            continue;
        };

        let value = compile_expr(ctx, &expr, Some(ctx.builtins().types.atom));
        ctx.assign_type(expr.syntax(), value.ty, ctx.builtins().types.atom);
        args.push(value.hir);
    }

    let hir = match verification {
        VerificationFunctionSymbol::BlsPairingIdentity => Hir::BlsPairingIdentity(args),
        VerificationFunctionSymbol::BlsVerify => {
            if args.is_empty() {
                // ctx.diagnostic(
                //     call.syntax(),
                //     DiagnosticKind::ExpectedArgumentsMinimum(1, 0),
                // );
                return ctx.builtins().unresolved.clone();
            }
            Hir::BlsVerify(args[0], args[1..].to_vec())
        }
        VerificationFunctionSymbol::Secp256K1Verify => {
            if args.len() != 3 {
                ctx.diagnostic(
                    call.syntax(),
                    DiagnosticKind::ExpectedArguments(3, args.len()),
                );
                return ctx.builtins().unresolved.clone();
            }
            Hir::Secp256K1Verify(args[0], args[1], args[2])
        }
        VerificationFunctionSymbol::Secp256R1Verify => {
            if args.len() != 3 {
                ctx.diagnostic(
                    call.syntax(),
                    DiagnosticKind::ExpectedArguments(3, args.len()),
                );
                return ctx.builtins().unresolved.clone();
            }
            Hir::Secp256R1Verify(args[0], args[1], args[2])
        }
    };

    let hir = ctx.alloc_hir(hir);

    Value::new(hir, ctx.builtins().types.nil)
}
