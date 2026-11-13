use std::collections::{HashMap, VecDeque};
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

/// Evaluates a const expression after the const expressions it depends on, and stores the result
/// in it. Returns whether the evaluation succeeded.
///
/// The const expressions are evaluated from a stack rather than recursively, so that a long chain
/// of them that each depend on the next doesn't overflow the stack. The stack holds the path from
/// this const expression to the one being evaluated, so the ones being evaluated are exactly the
/// ones on the stack.
fn evaluate_const(ctx: &mut Compiler, states: &mut HashMap<HirId, ConstState>, hir: HirId) -> bool {
    let mut stack = vec![hir];

    // The function each const expression on the stack is evaluated as, along with the const
    // expressions it depends on that haven't been evaluated yet
    let mut evaluating: HashMap<HirId, (SymbolId, VecDeque<HirId>)> = HashMap::new();

    while let Some(&current) = stack.last() {
        match states.get(&current) {
            Some(ConstState::Done | ConstState::Failed) => {
                stack.pop();
                continue;
            }
            Some(ConstState::Evaluating) => {}
            None => {
                let Hir::Const(expr) = ctx.hir(current).clone() else {
                    unreachable!("const expressions are never replaced");
                };

                if expr.result.is_some() {
                    states.insert(current, ConstState::Done);
                    stack.pop();
                    continue;
                }

                states.insert(current, ConstState::Evaluating);

                // Const expressions reachable through referenced symbols must be evaluated first,
                // so that their cached values are used rather than evaluating them again as part
                // of this one
                let root = const_root(ctx, expr.value);
                let graph = DependencyGraph::build(ctx, root, const_options(ctx));
                evaluating.insert(current, (root, graph.const_exprs().collect()));
            }
        }

        let (root, dependencies) = evaluating
            .get_mut(&current)
            .expect("const expression is being evaluated");

        let result = match next_dependency(states, dependencies) {
            Ok(Some(dependency)) => {
                stack.push(dependency);
                continue;
            }
            Ok(None) => evaluate_const_expr(ctx, *root),
            Err(error) => Err(error),
        };

        stack.pop();
        evaluating.remove(&current);
        finish_const(ctx, states, current, result);
    }

    states.get(&hir) == Some(&ConstState::Done)
}

/// The first of the dependencies that hasn't been evaluated yet, if any. The dependencies are
/// evaluated in order, one at a time, and are removed once they've been evaluated.
fn next_dependency(
    states: &HashMap<HirId, ConstState>,
    dependencies: &mut VecDeque<HirId>,
) -> Result<Option<HirId>, ConstEvalError> {
    while let Some(&dependency) = dependencies.front() {
        match states.get(&dependency) {
            Some(ConstState::Done) => {
                dependencies.pop_front();
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
fn evaluate_const_expr(ctx: &mut Compiler, root: SymbolId) -> Result<Hir, ConstEvalError> {
    let options = const_options(ctx);
    let max_cost = options.const_eval_max_cost;
    let graph = DependencyGraph::build(ctx, root, options);

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
