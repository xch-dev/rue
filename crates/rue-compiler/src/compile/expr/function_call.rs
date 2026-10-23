use std::collections::HashMap;

use log::debug;
use rue_ast::{AstExpr, AstFunctionCallExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{BinaryOp, Builtin, FunctionCall, Hir, Symbol, UnaryOp, Value};
use rue_lir::ClvmOp;
use rue_types::{
    FunctionType, Inference, Pair, Type, TypeId, Union, contains_generic, substitute_with_mappings,
};

use crate::{Compiler, CompletionContext, SyntaxItemKind, compile_expr, compile_path_value};

pub fn compile_function_call_expr(
    ctx: &mut Compiler,
    call: &AstFunctionCallExpr,
    expected_type: Option<TypeId>,
) -> Value {
    let Some(expr) = call.expr() else {
        debug!("Unresolved function call expr");
        return ctx.builtins().unresolved.clone();
    };

    // Builtins can only be referenced when they're called directly
    let expr = if let AstExpr::PathExpr(path) = &expr {
        ctx.add_syntax(
            SyntaxItemKind::CompletionContext(CompletionContext::Expression),
            path.syntax().text_range(),
        );
        compile_path_value(ctx, path, true)
    } else {
        compile_expr(ctx, &expr, None)
    };

    if let Hir::Reference(symbol) = ctx.hir(expr.hir).clone()
        && let Symbol::Builtin(builtin) = ctx.symbol(symbol).clone()
    {
        return compile_builtin(ctx, call, builtin);
    }

    let is_unresolved = ctx.is_unresolved(expr.ty);

    let expected_functions = rue_types::extract_functions(ctx.types_mut(), expr.ty);

    if expected_functions.len() > 1 {
        let name = ctx.type_name(expr.ty);
        ctx.diagnostic(
            call.syntax(),
            DiagnosticKind::CannotDisambiguateFunctionTypes(name),
        );
    } else if expected_functions.is_empty() && !is_unresolved {
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

        let mut infer = Inference::new(function.generics.iter().copied());

        // The type expected of the call is used for the generics that the arguments don't
        // constrain, such as `T` in `let value: Int = unchecked_cast(nil);`, and to keep the
        // ones they do from being widened past it
        let expected = expected_type.map(|expected_type| {
            let mut expected = infer.clone();
            ctx.infer_from_expected(function.ret, expected_type, &mut expected);

            // The generics of an enclosing call that are still being inferred can't be inferred
            // from this call, so they're no help in inferring its generics either
            let enclosing = ctx.inferred_generics();
            expected.retain_bounds(|ty| {
                !enclosing.iter().any(|&generic| {
                    !is_type_in_scope(ctx, generic)
                        && contains_generic(ctx.types_mut(), ty, generic)
                })
            });

            expected
        });

        let fallback = if let Some(expected) = &expected {
            ctx.resolve_inference(expected)
        } else {
            HashMap::new()
        };

        let mut results = vec![ctx.builtins().unresolved.hir; function.params.len()];
        let mut checks = Vec::new();

        // Lambdas are compiled after the other arguments, since their parameter types may
        // need to be inferred from what the other arguments have contributed
        let (mut lambdas, others): (Vec<_>, Vec<_>) = (0..function.params.len())
            .filter(|&i| i < args.len())
            .partition(|&i| matches!(args[i], AstExpr::LambdaExpr(_)));

        let mut others = others.into_iter();

        ctx.push_inferred_generics(function.generics.clone());

        loop {
            let inferred = ctx.resolve_inference(&infer);

            let i = if let Some(i) = others.next() {
                i
            } else if lambdas.is_empty() {
                break;
            } else {
                // A lambda's parameter types may only be inferred from a later lambda, so the
                // first one whose parameter types have been inferred is compiled next
                let index = lambdas
                    .iter()
                    .position(|&i| {
                        !has_uninferred_parameters(ctx, function.params[i], function, &inferred)
                    })
                    .unwrap_or(0);

                lambdas.remove(index)
            };

            let expr = &args[i];
            let param = function.params[i];

            // Use what has been inferred from the previous arguments as the expected type
            let mut mappings = inferred;

            for (&generic, &ty) in &fallback {
                mappings.entry(generic).or_insert(ty);
            }

            let expected = substitute_with_mappings(ctx.types_mut(), param, &mappings);

            let value = compile_expr(ctx, expr, Some(expected));
            results[i] = value.hir;

            // Candidates are collected against the unsubstituted parameter type, so that every
            // argument contributes to the generics rather than only the first one to mention them
            ctx.infer_type(value.ty, param, &mut infer);
            checks.push((expr.clone(), value.ty, param));
        }

        ctx.pop_inferred_generics();

        if args.len() < function.params.len() {
            debug!("Unresolved function call argument");
        }

        if let Some(expected) = &expected {
            ctx.add_expected_bounds(&mut infer, expected);
        }

        let mut mappings = ctx.resolve_inference(&infer);

        let has_generic_arguments = matches!(
            call.expr(),
            Some(AstExpr::PathExpr(path))
                if path.segments().any(|segment| segment
                    .generic_arguments()
                    .is_some_and(|generic_arguments| generic_arguments.types().next().is_some()))
        );

        for &generic in &function.generics {
            if mappings.contains_key(&generic) {
                continue;
            }

            if let Some(&ty) = fallback.get(&generic) {
                mappings.insert(generic, ty);
                continue;
            }

            // A generic that's in scope is being passed through a recursive call, so it's
            // fine for it to stay as is. Otherwise, the callee's generic would leak out, unless
            // the function's type doesn't reference it at all.
            if is_type_in_scope(ctx, generic)
                || !std::iter::once(function.ret)
                    .chain(function.params.values().copied())
                    .any(|ty| contains_generic(ctx.types_mut(), ty, generic))
            {
                continue;
            }

            // An argument that it would have been inferred from has already failed to compile
            let from_unresolved_argument = checks.iter().any(|&(_, ty, param)| {
                ctx.is_unresolved(ty) && contains_generic(ctx.types_mut(), param, generic)
            });

            // If generic arguments were given, a mismatch in their number was already reported
            if !has_generic_arguments && !from_unresolved_argument {
                let generic_name = ctx.type_name(generic);
                let function_name = callee_name(call);

                ctx.diagnostic(
                    call.syntax(),
                    DiagnosticKind::CannotInferGeneric(generic_name, function_name),
                );
            }

            mappings.insert(generic, ctx.builtins().unresolved.ty);
        }

        // Inference only proposes types for the generics, so each argument must be checked
        // against the fully substituted parameter type. This catches arguments that constrain
        // a generic inconsistently, such as `(Int, fn(x: Bytes32) -> Bytes32)` being passed to
        // a parameter of type `(T, fn(x: T) -> Bytes32)`.
        for (expr, ty, param) in checks {
            let param = substitute_with_mappings(ctx.types_mut(), param, &mappings);
            ctx.assign_type(expr.syntax(), ty, param);
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

/// The name of the function being called, without any generic arguments.
fn callee_name(call: &AstFunctionCallExpr) -> String {
    match call.expr() {
        Some(AstExpr::PathExpr(path)) => path
            .segments()
            .filter_map(|segment| segment.name())
            .map(|name| name.text().to_string())
            .collect::<Vec<_>>()
            .join("::"),
        Some(expr) => expr.syntax().text().to_string(),
        None => String::new(),
    }
}

/// Whether the parameters of the function type expected of a lambda reference any of the
/// callee's generics that haven't been inferred yet.
fn has_uninferred_parameters(
    ctx: &mut Compiler,
    param: TypeId,
    callee: &FunctionType,
    mappings: &HashMap<TypeId, TypeId>,
) -> bool {
    let expected = substitute_with_mappings(ctx.types_mut(), param, mappings);

    rue_types::extract_functions(ctx.types_mut(), expected)
        .into_iter()
        .flat_map(|function| function.params.into_values())
        .any(|ty| {
            callee.generics.iter().any(|&generic| {
                !mappings.contains_key(&generic) && contains_generic(ctx.types_mut(), ty, generic)
            })
        })
}

fn is_type_in_scope(ctx: &Compiler, ty: TypeId) -> bool {
    let mut current = Some(ctx.last_scope_id());

    while let Some(scope) = current {
        if ctx.scope(scope).type_name(ty).is_some() {
            return true;
        }
        current = ctx.scope(scope).parent();
    }

    false
}

fn compile_builtin(ctx: &mut Compiler, call: &AstFunctionCallExpr, builtin: Builtin) -> Value {
    let mut args = Vec::new();
    let mut spread = None;

    let len = call.args().count();

    for (i, arg) in call.args().enumerate() {
        let Some(expr) = arg.expr() else {
            debug!("Unresolved clvm op argument");
            continue;
        };

        if let Some(op) = arg.spread() {
            if i == len - 1 {
                spread = Some(op);
            } else {
                ctx.diagnostic(arg.syntax(), DiagnosticKind::NonFinalSpread);
            }
        }

        let value = compile_expr(ctx, &expr, None);

        args.push((value, expr));
    }

    match builtin {
        Builtin::Sha256 { inline } | Builtin::Keccak256 { inline } => {
            if args.len() != 1 {
                ctx.diagnostic(
                    call.syntax(),
                    DiagnosticKind::ExpectedArguments(1, args.len()),
                );
            }

            let value = if let Some((value, expr)) = args.first() {
                let ty = if let Some(spread) = &spread {
                    if inline {
                        ctx.diagnostic(spread, DiagnosticKind::InvalidSpreadBuiltin);
                    }

                    let list = ctx.builtins().types.list;

                    let mappings = HashMap::from_iter([(
                        ctx.builtins().types.list_generic,
                        ctx.builtins().types.bytes,
                    )]);

                    rue_types::substitute_with_mappings(ctx.types_mut(), list, &mappings)
                } else {
                    ctx.builtins().types.bytes
                };
                ctx.assign_type(expr.syntax(), value.ty, ty);
                value.hir
            } else {
                ctx.builtins().unresolved.hir
            };

            let hir = match (builtin, spread) {
                (Builtin::Sha256 { inline }, None) => {
                    if inline {
                        ctx.alloc_hir(Hir::Unary(UnaryOp::Sha256Inline, value))
                    } else {
                        ctx.alloc_hir(Hir::Unary(UnaryOp::Sha256, value))
                    }
                }
                (Builtin::Keccak256 { inline }, None) => {
                    if inline {
                        ctx.alloc_hir(Hir::Unary(UnaryOp::Keccak256Inline, value))
                    } else {
                        ctx.alloc_hir(Hir::Unary(UnaryOp::Keccak256, value))
                    }
                }
                (Builtin::Sha256 { .. }, Some(_)) => {
                    ctx.alloc_hir(Hir::ClvmOp(ClvmOp::Sha256, value))
                }
                (Builtin::Keccak256 { .. }, Some(_)) => {
                    ctx.alloc_hir(Hir::ClvmOp(ClvmOp::Keccak256, value))
                }
                _ => unreachable!(),
            };

            Value::new(hir, ctx.builtins().types.bytes32)
        }
        Builtin::Concat => {
            if args.len() != 1 {
                ctx.diagnostic(
                    call.syntax(),
                    DiagnosticKind::ExpectedArguments(1, args.len()),
                );
            }

            if let Some(spread) = &spread {
                ctx.diagnostic(spread, DiagnosticKind::InvalidSpreadBuiltin);
            }

            let value = if let Some((value, expr)) = args.first() {
                let list = ctx.builtins().types.list;

                let mappings = HashMap::from_iter([(
                    ctx.builtins().types.list_generic,
                    ctx.builtins().types.bytes,
                )]);

                let ty = rue_types::substitute_with_mappings(ctx.types_mut(), list, &mappings);
                ctx.assign_type(expr.syntax(), value.ty, ty);
                value.hir
            } else {
                ctx.builtins().unresolved.hir
            };

            let hir = ctx.alloc_hir(Hir::ClvmOp(ClvmOp::Concat, value));

            Value::new(hir, ctx.builtins().types.bytes)
        }
        Builtin::CoinId => {
            if args.len() != 3 {
                ctx.diagnostic(
                    call.syntax(),
                    DiagnosticKind::ExpectedArguments(3, args.len()),
                );
            }

            if let Some(spread) = &spread {
                ctx.diagnostic(spread, DiagnosticKind::InvalidSpreadBuiltin);
            }

            let hir = if args.len() == 3 {
                ctx.assign_type(args[0].1.syntax(), args[0].0.ty, ctx.builtins().types.bytes);
                ctx.assign_type(args[1].1.syntax(), args[1].0.ty, ctx.builtins().types.bytes);
                ctx.assign_type(args[2].1.syntax(), args[2].0.ty, ctx.builtins().types.int);
                ctx.alloc_hir(Hir::CoinId(args[0].0.hir, args[1].0.hir, args[2].0.hir))
            } else {
                ctx.builtins().unresolved.hir
            };

            Value::new(hir, ctx.builtins().types.bytes32)
        }
        Builtin::Substr => {
            if args.len() != 2 && args.len() != 3 {
                ctx.diagnostic(
                    call.syntax(),
                    DiagnosticKind::ExpectedArgumentsBetween(2, 3, args.len()),
                );
            }

            if let Some(spread) = &spread {
                ctx.diagnostic(spread, DiagnosticKind::InvalidSpreadBuiltin);
            }

            let hir = if args.len() >= 2 {
                ctx.assign_type(args[0].1.syntax(), args[0].0.ty, ctx.builtins().types.bytes);
                ctx.assign_type(args[1].1.syntax(), args[1].0.ty, ctx.builtins().types.int);

                if args.len() == 3 {
                    ctx.assign_type(args[2].1.syntax(), args[2].0.ty, ctx.builtins().types.int);
                }

                ctx.alloc_hir(Hir::Substr(
                    args[0].0.hir,
                    args[1].0.hir,
                    args.get(2).map(|arg| arg.0.hir),
                ))
            } else {
                ctx.builtins().unresolved.hir
            };

            Value::new(hir, ctx.builtins().types.bytes)
        }
        Builtin::Sum | Builtin::Difference | Builtin::Product => {
            if args.len() != 1 {
                ctx.diagnostic(
                    call.syntax(),
                    DiagnosticKind::ExpectedArguments(1, args.len()),
                );
            }

            if let Some(spread) = &spread {
                ctx.diagnostic(spread, DiagnosticKind::InvalidSpreadBuiltin);
            }

            let value = if let Some((value, expr)) = args.first() {
                let list = ctx.builtins().types.list;

                let mappings = HashMap::from_iter([(
                    ctx.builtins().types.list_generic,
                    ctx.builtins().types.int,
                )]);

                let ty = rue_types::substitute_with_mappings(ctx.types_mut(), list, &mappings);
                ctx.assign_type(expr.syntax(), value.ty, ty);
                value.hir
            } else {
                ctx.builtins().unresolved.hir
            };

            let hir = ctx.alloc_hir(Hir::ClvmOp(
                match builtin {
                    Builtin::Sum => ClvmOp::Add,
                    Builtin::Difference => ClvmOp::Sub,
                    Builtin::Product => ClvmOp::Mul,
                    _ => unreachable!(),
                },
                value,
            ));

            Value::new(hir, ctx.builtins().types.int)
        }
        Builtin::Divmod => {
            if args.len() != 2 {
                ctx.diagnostic(
                    call.syntax(),
                    DiagnosticKind::ExpectedArguments(2, args.len()),
                );
            }

            if let Some(spread) = &spread {
                ctx.diagnostic(spread, DiagnosticKind::InvalidSpreadBuiltin);
            }

            let hir = if args.len() == 2 {
                ctx.assign_type(args[0].1.syntax(), args[0].0.ty, ctx.builtins().types.int);
                ctx.assign_type(args[1].1.syntax(), args[1].0.ty, ctx.builtins().types.int);
                ctx.alloc_hir(Hir::Binary(BinaryOp::Divmod, args[0].0.hir, args[1].0.hir))
            } else {
                ctx.builtins().unresolved.hir
            };

            let int = ctx.builtins().types.int;
            let pair = ctx.alloc_type(Type::Pair(Pair::new(int, int)));

            Value::new(hir, pair)
        }
        Builtin::Modpow => {
            if args.len() != 3 {
                ctx.diagnostic(
                    call.syntax(),
                    DiagnosticKind::ExpectedArguments(3, args.len()),
                );
            }

            if let Some(spread) = &spread {
                ctx.diagnostic(spread, DiagnosticKind::InvalidSpreadBuiltin);
            }

            let hir = if args.len() == 3 {
                ctx.assign_type(args[0].1.syntax(), args[0].0.ty, ctx.builtins().types.int);
                ctx.assign_type(args[1].1.syntax(), args[1].0.ty, ctx.builtins().types.int);
                ctx.assign_type(args[2].1.syntax(), args[2].0.ty, ctx.builtins().types.int);
                ctx.alloc_hir(Hir::Modpow(args[0].0.hir, args[1].0.hir, args[2].0.hir))
            } else {
                ctx.builtins().unresolved.hir
            };

            Value::new(hir, ctx.builtins().types.int)
        }
        Builtin::Any | Builtin::All => {
            if args.len() != 1 {
                ctx.diagnostic(
                    call.syntax(),
                    DiagnosticKind::ExpectedArguments(1, args.len()),
                );
            }

            if let Some(spread) = &spread {
                ctx.diagnostic(spread, DiagnosticKind::InvalidSpreadBuiltin);
            }

            let value = if let Some((value, expr)) = args.first() {
                let list = ctx.builtins().types.list;

                let mappings = HashMap::from_iter([(
                    ctx.builtins().types.list_generic,
                    ctx.builtins().types.bool,
                )]);

                let ty = rue_types::substitute_with_mappings(ctx.types_mut(), list, &mappings);
                ctx.assign_type(expr.syntax(), value.ty, ty);
                value.hir
            } else {
                ctx.builtins().unresolved.hir
            };

            let hir = ctx.alloc_hir(Hir::ClvmOp(
                match builtin {
                    Builtin::Any => ClvmOp::Any,
                    Builtin::All => ClvmOp::All,
                    _ => unreachable!(),
                },
                value,
            ));

            Value::new(hir, ctx.builtins().types.bool)
        }
        Builtin::PubkeyForExp => {
            if args.len() != 1 {
                ctx.diagnostic(
                    call.syntax(),
                    DiagnosticKind::ExpectedArguments(1, args.len()),
                );
            }

            if let Some(spread) = &spread {
                ctx.diagnostic(spread, DiagnosticKind::InvalidSpreadBuiltin);
            }

            let hir = if args.len() == 1 {
                // The exponent is interpreted as a signed integer and reduced modulo the group
                // order, so both integers and 32 byte secret keys are accepted.
                let types = ctx.builtins().types;
                let exponent =
                    ctx.alloc_type(Type::Union(Union::new(vec![types.int, types.bytes32])));
                ctx.assign_type(args[0].1.syntax(), args[0].0.ty, exponent);
                ctx.alloc_hir(Hir::Unary(UnaryOp::PubkeyForExp, args[0].0.hir))
            } else {
                ctx.builtins().unresolved.hir
            };

            Value::new(hir, ctx.builtins().types.public_key)
        }
        Builtin::G1Sum | Builtin::G1Difference => {
            if args.len() != 1 {
                ctx.diagnostic(
                    call.syntax(),
                    DiagnosticKind::ExpectedArguments(1, args.len()),
                );
            }

            if let Some(spread) = &spread {
                ctx.diagnostic(spread, DiagnosticKind::InvalidSpreadBuiltin);
            }

            let value = if let Some((value, expr)) = args.first() {
                let list = ctx.builtins().types.list;

                let mappings = HashMap::from_iter([(
                    ctx.builtins().types.list_generic,
                    ctx.builtins().types.public_key,
                )]);

                let ty = rue_types::substitute_with_mappings(ctx.types_mut(), list, &mappings);
                ctx.assign_type(expr.syntax(), value.ty, ty);
                value.hir
            } else {
                ctx.builtins().unresolved.hir
            };

            let hir = ctx.alloc_hir(Hir::ClvmOp(
                match builtin {
                    Builtin::G1Sum => ClvmOp::G1Add,
                    Builtin::G1Difference => ClvmOp::G1Subtract,
                    _ => unreachable!(),
                },
                value,
            ));

            Value::new(hir, ctx.builtins().types.public_key)
        }
        Builtin::G2Sum | Builtin::G2Difference => {
            if args.len() != 1 {
                ctx.diagnostic(
                    call.syntax(),
                    DiagnosticKind::ExpectedArguments(1, args.len()),
                );
            }

            if let Some(spread) = &spread {
                ctx.diagnostic(spread, DiagnosticKind::InvalidSpreadBuiltin);
            }

            let value = if let Some((value, expr)) = args.first() {
                let list = ctx.builtins().types.list;

                let mappings = HashMap::from_iter([(
                    ctx.builtins().types.list_generic,
                    ctx.builtins().types.signature,
                )]);

                let ty = rue_types::substitute_with_mappings(ctx.types_mut(), list, &mappings);
                ctx.assign_type(expr.syntax(), value.ty, ty);
                value.hir
            } else {
                ctx.builtins().unresolved.hir
            };

            let hir = ctx.alloc_hir(Hir::ClvmOp(
                match builtin {
                    Builtin::G2Sum => ClvmOp::G2Add,
                    Builtin::G2Difference => ClvmOp::G2Subtract,
                    _ => unreachable!(),
                },
                value,
            ));

            Value::new(hir, ctx.builtins().types.signature)
        }
        Builtin::G1Map | Builtin::G2Map => {
            if args.len() != 1 && args.len() != 2 {
                ctx.diagnostic(
                    call.syntax(),
                    DiagnosticKind::ExpectedArgumentsBetween(1, 2, args.len()),
                );
            }

            if let Some(spread) = &spread {
                ctx.diagnostic(spread, DiagnosticKind::InvalidSpreadBuiltin);
            }

            let data = if let Some((value, expr)) = args.first() {
                ctx.assign_type(expr.syntax(), value.ty, ctx.builtins().types.bytes);
                value.hir
            } else {
                ctx.builtins().unresolved.hir
            };

            let dst = args.get(1).map(|(value, expr)| {
                ctx.assign_type(expr.syntax(), value.ty, ctx.builtins().types.bytes);
                value.hir
            });

            let hir = match builtin {
                Builtin::G1Map => ctx.alloc_hir(Hir::G1Map(data, dst)),
                Builtin::G2Map => ctx.alloc_hir(Hir::G2Map(data, dst)),
                _ => unreachable!(),
            };

            Value::new(
                hir,
                match builtin {
                    Builtin::G1Map => ctx.builtins().types.public_key,
                    Builtin::G2Map => ctx.builtins().types.signature,
                    _ => unreachable!(),
                },
            )
        }
        Builtin::BlsPairingIdentity => {
            let hir = if spread.is_some() {
                if args.len() != 1 {
                    ctx.diagnostic(
                        call.syntax(),
                        DiagnosticKind::ExpectedArguments(1, args.len()),
                    );
                }

                let list = ctx.builtins().types.alternating_list;

                let mappings = HashMap::from_iter([
                    (
                        ctx.builtins().types.alternating_list_generic_a,
                        ctx.builtins().types.public_key,
                    ),
                    (
                        ctx.builtins().types.alternating_list_generic_b,
                        ctx.builtins().types.signature,
                    ),
                ]);

                let ty = rue_types::substitute_with_mappings(ctx.types_mut(), list, &mappings);

                if let Some((value, expr)) = args.first() {
                    ctx.assign_type(expr.syntax(), value.ty, ty);
                    ctx.alloc_hir(Hir::ClvmOp(ClvmOp::BlsPairingIdentity, value.hir))
                } else {
                    ctx.builtins().unresolved.hir
                }
            } else {
                if args.len() % 2 != 0 {
                    ctx.diagnostic(call.syntax(), DiagnosticKind::ExpectedEvenArguments);
                }

                for (i, (value, expr)) in args.iter().enumerate() {
                    if i % 2 == 0 {
                        ctx.assign_type(expr.syntax(), value.ty, ctx.builtins().types.public_key);
                    } else {
                        ctx.assign_type(expr.syntax(), value.ty, ctx.builtins().types.signature);
                    }
                }

                ctx.alloc_hir(Hir::BlsPairingIdentity(
                    args.iter().map(|arg| arg.0.hir).collect(),
                ))
            };

            Value::new(hir, ctx.builtins().types.nil)
        }
        Builtin::BlsVerify => {
            let hir = if spread.is_some() {
                if args.len() != 2 {
                    ctx.diagnostic(
                        call.syntax(),
                        DiagnosticKind::ExpectedArguments(2, args.len()),
                    );
                }

                let list = ctx.builtins().types.alternating_list;

                let mappings = HashMap::from_iter([
                    (
                        ctx.builtins().types.alternating_list_generic_a,
                        ctx.builtins().types.public_key,
                    ),
                    (
                        ctx.builtins().types.alternating_list_generic_b,
                        ctx.builtins().types.bytes,
                    ),
                ]);

                let ty = rue_types::substitute_with_mappings(ctx.types_mut(), list, &mappings);

                if args.len() == 2 {
                    ctx.assign_type(
                        args[0].1.syntax(),
                        args[0].0.ty,
                        ctx.builtins().types.signature,
                    );
                    ctx.assign_type(args[1].1.syntax(), args[1].0.ty, ty);

                    let pair = ctx.alloc_hir(Hir::Pair(args[0].0.hir, args[1].0.hir));
                    ctx.alloc_hir(Hir::ClvmOp(ClvmOp::BlsVerify, pair))
                } else {
                    ctx.builtins().unresolved.hir
                }
            } else {
                if args.is_empty() || args.len() % 2 != 1 {
                    ctx.diagnostic(
                        call.syntax(),
                        DiagnosticKind::ExpectedOneArgumentEvenAdditional,
                    );
                }

                for (i, (value, expr)) in args.iter().enumerate() {
                    if i == 0 {
                        ctx.assign_type(expr.syntax(), value.ty, ctx.builtins().types.signature);
                    } else if i % 2 == 1 {
                        ctx.assign_type(expr.syntax(), value.ty, ctx.builtins().types.public_key);
                    } else {
                        ctx.assign_type(expr.syntax(), value.ty, ctx.builtins().types.bytes);
                    }
                }

                if args.is_empty() {
                    ctx.builtins().unresolved.hir
                } else {
                    ctx.alloc_hir(Hir::BlsVerify(
                        args[0].0.hir,
                        args.iter().skip(1).map(|arg| arg.0.hir).collect(),
                    ))
                }
            };

            Value::new(hir, ctx.builtins().types.nil)
        }
        Builtin::Secp256K1Verify | Builtin::Secp256R1Verify => {
            if args.len() != 3 {
                ctx.diagnostic(
                    call.syntax(),
                    DiagnosticKind::ExpectedArguments(3, args.len()),
                );
            }

            if let Some(spread) = &spread {
                ctx.diagnostic(spread, DiagnosticKind::InvalidSpreadBuiltin);
            }

            let hir = if args.len() == 3 {
                ctx.assign_type(
                    args[0].1.syntax(),
                    args[0].0.ty,
                    match builtin {
                        Builtin::Secp256K1Verify => ctx.builtins().types.k1_public_key,
                        Builtin::Secp256R1Verify => ctx.builtins().types.r1_public_key,
                        _ => unreachable!(),
                    },
                );
                ctx.assign_type(
                    args[1].1.syntax(),
                    args[1].0.ty,
                    ctx.builtins().types.bytes32,
                );
                ctx.assign_type(
                    args[2].1.syntax(),
                    args[2].0.ty,
                    match builtin {
                        Builtin::Secp256K1Verify => ctx.builtins().types.k1_signature,
                        Builtin::Secp256R1Verify => ctx.builtins().types.r1_signature,
                        _ => unreachable!(),
                    },
                );

                match builtin {
                    Builtin::Secp256K1Verify => ctx.alloc_hir(Hir::Secp256K1Verify(
                        args[0].0.hir,
                        args[1].0.hir,
                        args[2].0.hir,
                    )),
                    Builtin::Secp256R1Verify => ctx.alloc_hir(Hir::Secp256R1Verify(
                        args[0].0.hir,
                        args[1].0.hir,
                        args[2].0.hir,
                    )),
                    _ => unreachable!(),
                }
            } else {
                ctx.builtins().unresolved.hir
            };

            Value::new(hir, ctx.builtins().types.nil)
        }
    }
}
