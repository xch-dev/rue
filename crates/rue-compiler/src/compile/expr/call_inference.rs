//! Inferring a call's generics while its arguments are compiled: the order the arguments are
//! compiled in, and what each one is compiled against. How the generics are solved is up to
//! `CallSolver` in `rue-types`.

use std::collections::HashMap;

use log::debug;
use rue_ast::AstExpr;
use rue_hir::HirId;
use rue_types::{FunctionType, Inference, TypeId, contains_generic, substitute_with_mappings};

use crate::{Compiler, compile_expr};

/// What inferring a call's generics gave.
pub(crate) struct InferredCall {
    /// The compiled arguments, by parameter.
    pub results: Vec<HirId>,
    /// Each argument, its type, and the unsubstituted type of its parameter, for checking it once
    /// the generics are known.
    pub checks: Vec<(AstExpr, TypeId, TypeId)>,
    /// What the callee's generics were inferred as. A generic that couldn't be inferred is left out.
    pub mappings: HashMap<TypeId, TypeId>,
}

/// Compiles the arguments of a call to `function`, inferring its generics from them and from the
/// type expected of the call. `renamed` holds the callee's generics that were renamed because
/// they're in scope (see `rename_generics_in_scope`), and the generics they were renamed from.
pub(crate) fn infer_call(
    ctx: &mut Compiler,
    function: &FunctionType,
    args: &[AstExpr],
    expected_type: Option<TypeId>,
    renamed: &HashMap<TypeId, TypeId>,
) -> InferredCall {
    let mut infer = Inference::new(function.generics.iter().copied());

    for (&copy, &generic) in renamed {
        infer.rename(copy, generic);
    }

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
            !enclosing
                .iter()
                .any(|&generic| contains_generic(ctx.types_mut(), ty, generic))
        });

        expected
    });

    let solver = ctx.call_solver(expected);

    let mut results = vec![ctx.builtins().unresolved.hir; function.params.len()];
    let mut checks = Vec::new();

    let mut order = ArgumentOrder::new(args, function.params.len());

    ctx.push_inferred_generics(function.generics.clone());

    loop {
        let inferred = ctx.solve_arguments(&solver, &infer);

        let Some(i) = order.next(ctx, function, &inferred, renamed) else {
            break;
        };

        let expr = &args[i];
        let param = function.params[i].ty;

        // Use what has been inferred from the previous arguments as the expected type, along
        // with what the type expected of the call implies for the generics they don't map
        let mut mappings = ctx.argument_mappings(&solver, &infer, inferred);
        restore_renamed(ctx, &mut mappings, renamed);

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

    let mut mappings = ctx.solve_call(&solver, infer);
    restore_renamed(ctx, &mut mappings, renamed);

    InferredCall {
        results,
        checks,
        mappings,
    }
}

/// The order a call's arguments are compiled in. Lambdas are compiled after the other arguments,
/// since their parameter types may need to be inferred from what the other arguments have
/// contributed. The others are compiled in order.
struct ArgumentOrder {
    others: std::vec::IntoIter<usize>,
    lambdas: Vec<usize>,
}

impl ArgumentOrder {
    fn new(args: &[AstExpr], params: usize) -> Self {
        let (lambdas, others): (Vec<_>, Vec<_>) = (0..params)
            .filter(|&i| i < args.len())
            .partition(|&i| matches!(args[i], AstExpr::LambdaExpr(_)));

        Self {
            others: others.into_iter(),
            lambdas,
        }
    }

    /// The next argument to compile, given what the arguments compiled so far infer on their own,
    /// or `None` once they've all been compiled. A lambda's parameter types may only be inferred
    /// from a later lambda, so the first one whose parameter types have been inferred is compiled
    /// next, or the first one if none have been.
    fn next(
        &mut self,
        ctx: &mut Compiler,
        function: &FunctionType,
        inferred: &HashMap<TypeId, TypeId>,
        renamed: &HashMap<TypeId, TypeId>,
    ) -> Option<usize> {
        if let Some(i) = self.others.next() {
            return Some(i);
        }

        if self.lambdas.is_empty() {
            return None;
        }

        let index = self
            .lambdas
            .iter()
            .position(|&i| {
                !has_uninferred_parameters(ctx, function.params[i].ty, function, inferred, renamed)
            })
            .unwrap_or(0);

        Some(self.lambdas.remove(index))
    }
}

/// Whether the parameters of the function type expected of a lambda reference any of the
/// callee's generics that haven't been inferred yet. A renamed one (see `rename_generics_in_scope`)
/// also counts where they reference the caller's generic it was renamed from, as it did when the
/// two were the same generic, so that lambdas are compiled in the same order.
fn has_uninferred_parameters(
    ctx: &mut Compiler,
    param: TypeId,
    callee: &FunctionType,
    mappings: &HashMap<TypeId, TypeId>,
    renamed: &HashMap<TypeId, TypeId>,
) -> bool {
    let expected = substitute_with_mappings(ctx.types_mut(), param, mappings);

    rue_types::extract_functions(ctx.types_mut(), expected, true)
        .into_iter()
        .flat_map(|function| function.params)
        .any(|param| {
            callee.generics.iter().any(|&generic| {
                !mappings.contains_key(&generic)
                    && std::iter::once(generic)
                        .chain(renamed.get(&generic).copied())
                        .any(|generic| contains_generic(ctx.types_mut(), param.ty, generic))
            })
        })
}

/// Maps the renamed generics that haven't been inferred back to the caller's generics they were
/// renamed from (see `rename_generics_in_scope`), including where the others are inferred as types
/// that reference them, since that's what they referred to before they were renamed.
fn restore_renamed(
    ctx: &mut Compiler,
    mappings: &mut HashMap<TypeId, TypeId>,
    renamed: &HashMap<TypeId, TypeId>,
) {
    let restored: HashMap<TypeId, TypeId> = renamed
        .iter()
        .filter(|(copy, _)| !mappings.contains_key(copy))
        .map(|(&copy, &generic)| (copy, generic))
        .collect();

    if restored.is_empty() {
        return;
    }

    for ty in mappings.values_mut() {
        if restored
            .keys()
            .any(|&copy| contains_generic(ctx.types_mut(), *ty, copy))
        {
            *ty = substitute_with_mappings(ctx.types_mut(), *ty, &restored);
        }
    }

    mappings.extend(restored);
}
