use std::collections::HashMap;

use log::debug;
use rue_ast::{AstFunctionCallExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{BinaryOp, Builtin, FunctionCall, Hir, Symbol, UnaryOp, Value};
use rue_lir::ClvmOp;
use rue_types::{Pair, Type, Union, substitute_with_mappings};

use crate::{Compiler, compile_expr};

pub fn compile_function_call_expr(ctx: &mut Compiler, call: &AstFunctionCallExpr) -> Value {
    let Some(expr) = call.expr() else {
        debug!("Unresolved function call expr");
        return ctx.builtins().unresolved.clone();
    };

    let expr = compile_expr(ctx, &expr, None);

    if let Hir::Reference(symbol) = ctx.hir(expr.hir).clone()
        && let Symbol::Builtin(builtin) = ctx.symbol(symbol).clone()
    {
        return compile_builtin(ctx, call, builtin);
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
                ctx.assign_type(
                    args[0].1.syntax(),
                    args[0].0.ty,
                    ctx.builtins().types.bytes32,
                );
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
