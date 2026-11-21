use std::collections::HashMap;
use std::path::PathBuf;

use clvmr::error::EvalErr;
use clvmr::{Allocator, ChiaDialect, NodePtr, SExp, run_program};
use id_arena::Arena;
use num_bigint::Sign;
use num_traits::ToPrimitive;
use rue_diagnostic::{DiagnosticKind, SrcLoc};
use rue_hir::{
    DependencyGraph, Environment, Hir, HirId, Lowerer, MAX_LOWERING_DEPTH, MAX_POWER_EXPONENT,
    Symbol, SymbolId, Value, expand_power, has_reachable_unresolved,
};
use rue_lir::{CodegenOptions, atom_bigint, codegen, optimize};
use rue_options::CompilerOptions;

use crate::{Compiler, clvm_target, entrypoint::synthetic_function};

pub(crate) fn evaluate_const_exprs(ctx: &mut Compiler) {
    if ctx.has_errors() {
        return;
    }

    let const_exprs = ctx.const_exprs().collect::<Vec<_>>();
    let mut states = HashMap::new();

    for hir in const_exprs {
        evaluate_const(ctx, &mut states, hir);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConstState {
    Evaluating,
    Done,
    Failed,
}

enum ConstEvalError {
    RuntimeDependency(String),
    CostExceeded(u64),
    Failed(String),
    Recursive,
    DependencyFailed,
    /// The exponent of a power was evaluated, but the power can't be expanded with it.
    Exponent(DiagnosticKind),
    LoweringTooDeep,
}

/// A const expression that's being evaluated, once the const expressions it depends on are.
struct ConstFrame {
    hir: HirId,
    /// The function that the const expression is evaluated as.
    root: SymbolId,
    /// The dependency graph of the function, if none of the const expressions it depends on had
    /// to be evaluated after it was built, so that it can be reused.
    graph: Option<DependencyGraph>,
    /// The const expressions it depends on that haven't been evaluated yet, in reverse order.
    dependencies: Vec<HirId>,
}

/// Evaluates a const expression after the const expressions it depends on, and stores the result
/// in it.
///
/// The const expressions are evaluated from a stack rather than recursively, so that a long chain
/// of them that each depend on the next doesn't overflow the stack. The stack holds the path from
/// this const expression to the one being evaluated, so the ones being evaluated are exactly the
/// ones on the stack.
fn evaluate_const(ctx: &mut Compiler, states: &mut HashMap<HirId, ConstState>, hir: HirId) {
    let mut stack: Vec<ConstFrame> = Vec::new();
    let mut next = Some(hir);

    loop {
        if let Some(hir) = next.take()
            && !states.contains_key(&hir)
        {
            let Hir::Const(expr) = ctx.hir(hir).clone() else {
                unreachable!("const expressions are never replaced");
            };

            if ctx.is_failed_exponent(hir) {
                states.insert(hir, ConstState::Failed);
            } else if expr.result.is_some() {
                states.insert(hir, ConstState::Done);
            } else {
                states.insert(hir, ConstState::Evaluating);

                // Const expressions reachable through referenced symbols must be evaluated first,
                // so that their cached values are used rather than evaluating them again as part
                // of this one
                let root = const_root(ctx, expr.value);
                let graph = DependencyGraph::build(ctx, root, const_options(ctx));
                let dependencies: Vec<HirId> = graph.const_exprs().collect();

                stack.push(ConstFrame {
                    hir,
                    root,
                    graph: dependencies.is_empty().then_some(graph),
                    dependencies: dependencies.into_iter().rev().collect(),
                });
            }
        }

        let Some(top) = stack.last_mut() else {
            break;
        };

        let result = match next_dependency(states, &mut top.dependencies) {
            Ok(Some(dependency)) => {
                next = Some(dependency);
                continue;
            }
            Ok(None) => evaluate_const_expr(ctx, top.root, top.graph.take()),
            Err(error) => Err(error),
        };

        let top = stack.pop().expect("the stack isn't empty");
        finish_const(ctx, states, top.hir, result);
    }
}

/// The next of the dependencies that hasn't been evaluated yet, if any. The dependencies are
/// evaluated in order, one at a time, and are removed once they've been evaluated.
fn next_dependency(
    states: &HashMap<HirId, ConstState>,
    dependencies: &mut Vec<HirId>,
) -> Result<Option<HirId>, ConstEvalError> {
    while let Some(&dependency) = dependencies.last() {
        match states.get(&dependency) {
            Some(ConstState::Done) => {
                dependencies.pop();
            }
            Some(ConstState::Failed) => return Err(ConstEvalError::DependencyFailed),
            // Only the const expressions on the stack are being evaluated, so this is a cycle
            Some(ConstState::Evaluating) => return Err(ConstEvalError::Recursive),
            None => return Ok(Some(dependency)),
        }
    }

    Ok(None)
}

/// Stores the result of a const expression, or reports why it failed.
fn finish_const(
    ctx: &mut Compiler,
    states: &mut HashMap<HirId, ConstState>,
    hir: HirId,
    result: Result<Hir, ConstEvalError>,
) {
    let Hir::Const(expr) = ctx.hir(hir).clone() else {
        unreachable!("const expressions are never replaced");
    };

    let power = ctx.power_of_exponent(hir);

    // The exponent of a power also has to be one that the power can be expanded with
    let result = result.and_then(|value| match power {
        Some(_) => exponent_value(&value).map(|exponent| (value, Some(exponent))),
        None => Ok((value, None)),
    });

    let diagnostic = match result {
        Ok((value, exponent)) => {
            let value = ctx.alloc_hir(value);
            set_const_result(ctx, hir, value);

            // The power is expanded as soon as its exponent is known, so that it has been by the
            // time anything that contains it is lowered, including a const expression
            if let (Some(power), Some(exponent)) = (power, exponent) {
                expand(ctx, power, exponent, &expr.loc);
            }

            states.insert(hir, ConstState::Done);
            return;
        }
        // A const expression that the compiler generated is only an optimization, so the value is
        // left to be evaluated at runtime, as it was written
        Err(_) if expr.generated => {
            set_const_result(ctx, hir, expr.value);
            states.insert(hir, ConstState::Done);
            return;
        }
        Err(ConstEvalError::RuntimeDependency(symbol)) if power.is_some() => {
            Some(DiagnosticKind::RuntimeExponent(symbol))
        }
        Err(ConstEvalError::RuntimeDependency(symbol)) => {
            Some(DiagnosticKind::ConstRuntimeDependency(symbol))
        }
        Err(ConstEvalError::CostExceeded(limit)) => {
            Some(DiagnosticKind::ConstEvalCostExceeded(limit))
        }
        Err(ConstEvalError::Failed(error)) => Some(DiagnosticKind::ConstEvalFailed(error)),
        Err(ConstEvalError::Recursive) => Some(DiagnosticKind::RecursiveConstExpr),
        Err(ConstEvalError::LoweringTooDeep) => {
            Some(DiagnosticKind::LoweringTooDeep(MAX_LOWERING_DEPTH))
        }
        Err(ConstEvalError::Exponent(diagnostic)) => Some(diagnostic),
        // The dependency has already reported its own diagnostic
        Err(ConstEvalError::DependencyFailed) => None,
    };

    if let Some(diagnostic) = diagnostic {
        ctx.diagnostic_at(expr.loc, diagnostic);
    }

    let nil = ctx.alloc_hir(Hir::Nil);
    set_const_result(ctx, hir, nil);

    // Like the const expression, the power gets a placeholder, so that whatever contains it can
    // still be lowered
    if let Some(power) = power {
        set_expansion(ctx, power, nil);
        ctx.set_exponent_failed(hir);
    }

    states.insert(hir, ConstState::Failed);
}

/// The exponent that a power is expanded with, unless it's negative or too large.
fn exponent_value(exponent: &Hir) -> Result<u32, ConstEvalError> {
    // The exponent is an `Int`, so it's an atom unless its type is wrong, which the compiler
    // trusts as it does at runtime
    let Hir::Bytes(bytes) = exponent else {
        return Err(ConstEvalError::Failed(
            "the exponent isn't an atom".to_string(),
        ));
    };

    let exponent = atom_bigint(bytes);

    if exponent.sign() == Sign::Minus {
        return Err(ConstEvalError::Exponent(DiagnosticKind::NegativeExponent(
            exponent.to_string(),
        )));
    }

    exponent
        .to_u32()
        .filter(|&exponent| exponent <= MAX_POWER_EXPONENT)
        .ok_or_else(|| {
            ConstEvalError::Exponent(DiagnosticKind::ExponentTooLarge(
                exponent.to_string(),
                MAX_POWER_EXPONENT,
            ))
        })
}

/// Expands a power into multiplications, now that its exponent has been evaluated. The location
/// is the exponent's, for a base that doesn't have one.
fn expand(ctx: &mut Compiler, power: HirId, exponent: u32, loc: &SrcLoc) {
    let Hir::Power(node) = *ctx.hir(power) else {
        unreachable!("powers are never replaced");
    };

    let int = ctx.builtins().types.int;
    let base = Value::new(node.base, ctx.hir_type(node.base).unwrap_or(int));
    let srcloc = ctx.hir_srcloc(node.base).unwrap_or(loc).clone();
    let expansion = expand_power(ctx, base, srcloc, exponent);
    set_expansion(ctx, power, expansion);
}

fn set_expansion(ctx: &mut Compiler, power: HirId, expansion: HirId) {
    if let Hir::Power(power) = ctx.hir_mut(power) {
        power.expansion = Some(expansion);
    }
}

/// A function that returns the value of a const expression, which it's evaluated as.
fn const_root(ctx: &mut Compiler, value: HirId) -> SymbolId {
    let unresolved = ctx.builtins().unresolved.ty;
    synthetic_function(ctx, None, unresolved, Vec::new(), true, value)
}

/// Const expressions are always optimized, regardless of the options they're compiled with.
fn const_options(ctx: &Compiler) -> CompilerOptions {
    let outer_options = *ctx.options();

    CompilerOptions {
        std: outer_options.std,
        auto_inline: true,
        optimize_lir: true,
        debug_symbols: false,
        optimize_static_pairs: true,
        const_eval_max_cost: outer_options.const_eval_max_cost,
        // Const expressions are run without `LIMITS`, since they're never run on chain, but with
        // the cost model of the target, which `const_eval_max_cost` is measured in
        operand_limits: false,
        hard_fork_2: outer_options.hard_fork_2,
        lints: outer_options.lints,
    }
}

fn set_const_result(ctx: &mut Compiler, hir: HirId, value: HirId) {
    if let Hir::Const(expr) = ctx.hir_mut(hir) {
        expr.result = Some(value);
    }
}

/// Evaluates a const expression, once the const expressions it depends on have been evaluated.
/// The dependency graph is built if it isn't given.
fn evaluate_const_expr(
    ctx: &mut Compiler,
    root: SymbolId,
    graph: Option<DependencyGraph>,
) -> Result<Hir, ConstEvalError> {
    let options = const_options(ctx);
    let max_cost = options.const_eval_max_cost;
    let graph = graph.unwrap_or_else(|| DependencyGraph::build(ctx, root, options));

    let has_unresolved = has_reachable_unresolved(ctx, root);

    debug_assert!(
        !has_unresolved,
        "unresolved code reached const evaluation without an error"
    );

    if has_unresolved {
        return Err(ConstEvalError::Failed(
            "internal error: unresolved code reached const evaluation".to_string(),
        ));
    }

    for dependency in graph.dependencies(root, true) {
        match ctx.symbol(dependency) {
            Symbol::Function(_) | Symbol::Constant(_) => {}
            Symbol::Binding(_) | Symbol::Parameter(_) => {
                let name = ctx.symbol(dependency).name().map_or_else(
                    || ctx.debug_symbol(dependency),
                    |name| name.text().to_string(),
                );
                return Err(ConstEvalError::RuntimeDependency(name));
            }
            Symbol::Unresolved | Symbol::Module(_) | Symbol::Builtin(_) => {
                return Err(ConstEvalError::Failed(format!(
                    "unsupported dependency `{}`",
                    ctx.debug_symbol(dependency)
                )));
            }
        }
    }

    let mut arena = Arena::new();
    let lir = {
        let mut lowerer = Lowerer::new(ctx, &mut arena, &graph, options, root, PathBuf::new());
        let lir = lowerer.lower_symbol_value(&Environment::default(), root);

        if lowerer.too_deep().is_some() {
            return Err(ConstEvalError::LoweringTooDeep);
        }

        lir
    };
    let target = clvm_target(&options);
    let lir = optimize(&mut arena, lir, target);

    let mut allocator = Allocator::new();
    let program = codegen(
        &arena,
        &mut allocator,
        lir,
        CodegenOptions {
            optimize_static_pairs: options.optimize_static_pairs,
            target,
        },
    )
    .map_err(|error| ConstEvalError::Failed(error.to_string()))?;
    let dialect = ChiaDialect::new(target.flags());

    let output = run_program(&mut allocator, &dialect, program, NodePtr::NIL, max_cost).map_err(
        |error| match error {
            EvalErr::CostExceeded => ConstEvalError::CostExceeded(max_cost),
            error => ConstEvalError::Failed(error.to_string()),
        },
    )?;

    Ok(decode_node(ctx, &allocator, output.1))
}

pub(crate) fn decode_node(ctx: &mut Compiler, allocator: &Allocator, node: NodePtr) -> Hir {
    match allocator.sexp(node) {
        SExp::Atom => Hir::Bytes(allocator.atom(node).to_vec()),
        SExp::Pair(first, rest) => {
            let first = decode_node(ctx, allocator, first);
            let first = ctx.alloc_hir(first);
            let rest = decode_node(ctx, allocator, rest);
            let rest = ctx.alloc_hir(rest);
            Hir::Pair(first, rest)
        }
    }
}
