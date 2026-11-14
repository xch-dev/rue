use std::collections::HashMap;
use std::path::PathBuf;

use clvmr::error::EvalErr;
use clvmr::{Allocator, ChiaDialect, NodePtr, SExp, run_program};
use id_arena::Arena;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{
    DependencyGraph, Environment, FunctionKind, FunctionSymbol, Hir, HirId, Lowerer,
    MAX_LOWERING_DEPTH, Symbol, SymbolId, has_reachable_unresolved,
};
use rue_lir::{CLVM_FLAGS, CodegenOptions, codegen, optimize};
use rue_options::CompilerOptions;

use crate::Compiler;

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

            if expr.result.is_some() {
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

    let diagnostic = match result {
        Ok(value) => {
            set_const_result(ctx, hir, value);
            states.insert(hir, ConstState::Done);
            return;
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
        // The dependency has already reported its own diagnostic
        Err(ConstEvalError::DependencyFailed) => None,
    };

    if let Some(diagnostic) = diagnostic {
        ctx.diagnostic_at(expr.loc, diagnostic);
    }

    set_const_result(ctx, hir, Hir::Nil);
    states.insert(hir, ConstState::Failed);
}

/// A function that returns the value of a const expression, which it's evaluated as.
fn const_root(ctx: &mut Compiler, value: HirId) -> SymbolId {
    let unresolved = ctx.builtins().unresolved.clone();
    let scope = ctx.builtins().scope;

    ctx.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: None,
        ty: unresolved.ty,
        scope,
        vars: Vec::new(),
        parameters: Vec::new(),
        nil_terminated: true,
        return_type: unresolved.ty,
        body: value,
        kind: FunctionKind::Sequential,
    }))
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
        lints: outer_options.lints,
    }
}

fn set_const_result(ctx: &mut Compiler, hir: HirId, value: Hir) {
    let value = ctx.alloc_hir(value);

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
    let lir = optimize(&mut arena, lir);

    let mut allocator = Allocator::new();
    let program = codegen(
        &arena,
        &mut allocator,
        lir,
        CodegenOptions {
            optimize_static_pairs: options.optimize_static_pairs,
        },
    )
    .map_err(|error| ConstEvalError::Failed(error.to_string()))?;
    let dialect = ChiaDialect::new(CLVM_FLAGS);

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
