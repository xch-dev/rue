use std::path::PathBuf;

use clvmr::error::EvalErr;
use clvmr::{Allocator, ChiaDialect, NodePtr, SExp, run_program};
use id_arena::Arena;
use indexmap::IndexMap;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{
    DependencyGraph, Environment, FunctionKind, FunctionSymbol, Hir, HirId, Lowerer, LoweringMode,
    Symbol, has_reachable_unresolved,
};
use rue_lir::{CLVM_FLAGS, CodegenOptions, codegen, optimize};
use rue_options::CompilerOptions;

use crate::Compiler;

pub(crate) fn evaluate_const_exprs(ctx: &mut Compiler) {
    if ctx.has_errors() {
        return;
    }

    let const_exprs = ctx
        .const_exprs()
        .filter_map(|hir| match ctx.hir(hir) {
            Hir::Const(expr) => Some((hir, expr.clone())),
            _ => None,
        })
        .collect::<Vec<_>>();

    for (hir, expr) in const_exprs {
        match evaluate_const_expr(ctx, expr.value) {
            Ok(value) => *ctx.hir_mut(hir) = value,
            Err(ConstEvalError::RuntimeDependency(symbol)) => {
                ctx.diagnostic_at(expr.loc, DiagnosticKind::ConstRuntimeDependency(symbol));
                *ctx.hir_mut(hir) = Hir::Nil;
            }
            Err(ConstEvalError::CostExceeded(limit)) => {
                ctx.diagnostic_at(expr.loc, DiagnosticKind::ConstEvalCostExceeded(limit));
                *ctx.hir_mut(hir) = Hir::Nil;
            }
            Err(ConstEvalError::Failed(error)) => {
                ctx.diagnostic_at(expr.loc, DiagnosticKind::ConstEvalFailed(error));
                *ctx.hir_mut(hir) = Hir::Nil;
            }
        }
    }
}

enum ConstEvalError {
    RuntimeDependency(String),
    CostExceeded(u64),
    Failed(String),
}

fn evaluate_const_expr(ctx: &mut Compiler, value: HirId) -> Result<Hir, ConstEvalError> {
    let unresolved = ctx.builtins().unresolved.clone();
    let scope = ctx.builtins().scope;
    let root = ctx.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: None,
        ty: unresolved.ty,
        scope,
        vars: Vec::new(),
        parameters: IndexMap::new(),
        nil_terminated: true,
        return_type: unresolved.ty,
        body: value,
        kind: FunctionKind::Sequential,
    }));

    let outer_options = *ctx.options();
    let options = CompilerOptions {
        std: outer_options.std,
        auto_inline: true,
        optimize_lir: true,
        debug_symbols: false,
        optimize_static_pairs: true,
        const_eval_max_cost: outer_options.const_eval_max_cost,
    };
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
        let mut lowerer = Lowerer::new(
            ctx,
            &mut arena,
            &graph,
            options,
            LoweringMode::ConstEval,
            root,
            PathBuf::new(),
        );
        lowerer.lower_symbol_value(&Environment::default(), root)
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
