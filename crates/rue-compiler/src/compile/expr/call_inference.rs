//! Inferring a call's generics while its arguments are compiled: the order the arguments are
//! compiled in, and what each one is compiled against. How the generics are solved is up to
//! `CallSolver` in `rue-types`.

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use indexmap::IndexSet;

use log::debug;
use rowan::TextRange;
use rue_ast::{
    AstBinaryExpr, AstBinding, AstExpr, AstFieldAccessExpr, AstFunctionCallExpr, AstGuardExpr,
    AstLambdaExpr, AstMatchExpr, AstNamedBinding, AstNode, AstPathSegment, AstPrefixExpr,
    AstStructFieldBinding,
};
use rue_hir::{Hir, HirId, Value};
use rue_types::{
    CallSolver, FunctionType, Generic, Inference, InstancePolicy, Type, TypeId, Union,
    contained_generics, contains_generic, substitute_with_mappings,
};

use crate::{
    ArgumentFrame, Compiler, LambdaBody, LambdaParameter, MAX_GENERALIZED_LEVELS, ParameterWrap,
    compile_call_argument, compile_expr, compile_lambda_argument, generalized_call,
    is_type_in_scope, occurs_contravariantly,
};

/// What inferring a call's generics gave.
pub(crate) struct InferredCall {
    /// The compiled arguments, by parameter.
    pub results: Vec<HirId>,
    /// Each compiled argument's check, in the order the arguments come in.
    pub checks: Vec<ArgumentCheck>,
    /// What the callee's generics were inferred as. A generic that couldn't be inferred is left out.
    pub mappings: HashMap<TypeId, TypeId>,
    /// The mappings before the renamed generics that weren't inferred were restored to the
    /// caller's generics they were renamed from (see `restore_renamed`), which leaves those out,
    /// and has them as they are in what the others were inferred as. It's empty for a call
    /// without renamed generics.
    pub unrestored: HashMap<TypeId, TypeId>,
}

/// A compiled argument, with its type and the unsubstituted type of its parameter, for checking it
/// once the generics are known.
#[derive(Debug, Clone)]
pub(crate) struct ArgumentCheck {
    pub expr: AstExpr,
    pub ty: TypeId,
    pub param: TypeId,
}

/// What `infer_call` keeps of each argument while it's compiled and the call is solved.
#[derive(Debug, Clone)]
struct ArgumentState {
    /// The argument's value, which is a placeholder until it's compiled.
    result: HirId,
    /// Its check, once it's compiled. The checks are made in the order the arguments come in (see
    /// `ArgumentOrder::candidate_order`).
    check: Option<ArgumentCheck>,
    /// The untyped parameters of its lambdas, as the references they were compiled as and the
    /// types those refer to (see `compile_lambda`).
    lambda_parameters: Vec<LambdaParameter>,
    /// For each of those parameters, the type with the callee's generics in it that its lambda is
    /// checked with them as what they're inferred as, if it is (see `generic_parameter_types`).
    generic_types: GenericTypes,
    /// The caller's generics that stand for the callee's renamed ones in it (see `ArgumentFrame`),
    /// which its candidates are made with for solving the call.
    restored: HashMap<TypeId, TypeId>,
    /// Those of them that are their own in its lambdas' parameters.
    own: ArgumentOwn,
    /// Those of `own` that come from lambdas that waited, which stay the caller's own when the
    /// call is solved without the others (see `with_own` in `solve_compiled_call`).
    waited_own: ArgumentOwn,
}

impl ArgumentState {
    fn new(result: HirId) -> Self {
        Self {
            result,
            check: None,
            lambda_parameters: Vec::new(),
            generic_types: GenericTypes::default(),
            restored: HashMap::new(),
            own: Vec::new(),
            waited_own: Vec::new(),
        }
    }
}

/// The arguments' checks, by parameter.
fn checks_of(arguments: &[ArgumentState]) -> Vec<Option<ArgumentCheck>> {
    arguments
        .iter()
        .map(|argument| argument.check.clone())
        .collect()
}

/// What solving a call gave: what its generics are solved as, those before the renamed generics
/// were restored (see `InferredCall::unrestored`), and each argument's check, by parameter, as it's
/// checked with them.
struct Solution {
    mappings: HashMap<TypeId, TypeId>,
    unrestored: HashMap<TypeId, TypeId>,
    checks: Vec<Option<ArgumentCheck>>,
}

/// Compiles the arguments of a call to `function`, inferring its generics from them and from the
/// type expected of the call. `renamed` holds the callee's generics that were renamed because
/// they're in scope (see `rename_generics_in_scope`), and the generics they were renamed from.
pub(crate) fn infer_call(
    ctx: &mut Compiler,
    function: &FunctionType,
    args: &[AstExpr],
    (expected_type, requirement): (Option<TypeId>, Option<TypeId>),
    narrower: Option<TypeId>,
    renamed: &HashMap<TypeId, TypeId>,
) -> InferredCall {
    let mut base = Inference::new(function.generics.iter().copied());

    for (&copy, &generic) in renamed {
        base.rename(copy, generic);
    }

    // The type expected of the call is used for the generics that the arguments don't
    // constrain, such as `T` in `let value: Int = unchecked_cast(nil);`, and to keep the
    // ones they do from being widened past it
    //
    // It also gives whether inferring them finished within the step limit: one that gave up
    // records the union members it didn't compare as ones that can't hold the value, which is
    // fine for candidates but not for what's required of every solution (see `required`)
    let infer_expected = |ctx: &mut Compiler, ty: TypeId, bound: &HashMap<TypeId, usize>| {
        let mut expected = base.clone();
        let finished = ctx.infer_from_expected(function.ret, ty, &mut expected);

        // The generics of an enclosing call that are still being inferred can't be inferred
        // from this call, so they're no help in inferring its generics either
        let enclosing = ctx.inferred_generics();
        expected.retain_bounds(|ty| {
            !enclosing
                .iter()
                .any(|&generic| contains_generic(ctx.types_mut(), ty, generic))
        });

        // A generic of a function type expected of the call is only in scope inside of it, so a
        // callee's generic can only be inferred as a type that references it if the function
        // returned at that depth can be generalized over it (see `generalize`). It can't be if a
        // parameter further out references the callee's generic: `B` can't be `List<T>` in
        // `let f: fn<T>(a: T) -> (T, List<T>) = make(nil);` with
        // `fn make<A, B>(v: B) -> fn(a: A) -> (A, B)`, since `v` is passed before there's a `T`.
        // Nor if the call doesn't return a function there, as with a pair of functions
        if !bound.is_empty() {
            let fixed = fixed_depths(ctx, function);

            expected.retain_bounds_of(|generic, ty| {
                let depth = fixed.get(&generic).copied().unwrap_or(usize::MAX);

                rue_types::referenced_generics(ctx.types_mut(), ty)
                    .into_iter()
                    .all(|other| bound.get(&other).is_none_or(|&bound_at| bound_at <= depth))
            });
        }

        (expected, finished)
    };
    let mut bound = expected_type
        .map(|expected_type| expected_generic_depths(ctx, expected_type))
        .unwrap_or_default();

    // A function type can bind a generic that's also in scope here, such as `T` in the type of
    // `foo` inside of `fn foo<T>`, which is then still the caller's own where it's free
    bound.retain(|&generic, _| !is_type_in_scope(ctx, generic));

    let expected = expected_type.map(|expected_type| infer_expected(ctx, expected_type, &bound));
    let expected_finished = expected.as_ref().is_none_or(|&(_, finished)| finished);
    let expected = expected.map(|(expected, _)| expected);

    // A narrower expected type is what the generics nothing else gives a type take, when the
    // expected type was widened to an enclosing call's (see `ArgumentOrder::widens_to_expected`).
    // Only the expected type is generalized over (see `generalize`), so the narrower one's own
    // generics are never in scope for what the call returns
    let narrower = narrower.map(|narrower| {
        let mut bound = bound;

        for generic in rue_types::function_type_generics(ctx.types_mut(), narrower) {
            if !is_type_in_scope(ctx, generic) {
                bound.entry(generic).or_insert(usize::MAX);
            }
        }

        infer_expected(ctx, narrower, &bound).0
    });

    // A call with arguments in an element that's only refined for a call without arguments in it
    // only uses the type expected of it for the generics its arguments don't give a type (see
    // `refine_element_expected`), so it isn't solved again for it either
    let fallback_only = ctx.is_expected_type_fallback() && !args.is_empty();
    // The type expected of the call, if it's a type of its own that the call has to be assignable
    // to. One that's only a hint doesn't count (see `Compiler::with_expected_type_hint`), such as one
    // that comes from what an enclosing call has inferred so far, which it can still widen
    let final_expected = expected_type.filter(|_| !ctx.is_expected_type_hint() && !fallback_only);

    // The type the call has to be assignable to however it's solved, if there is one: its final
    // expected type, unless an enclosing call can still widen that (see
    // `Compiler::with_expected_type_provisional`), in which case it's what the enclosing call
    // requires of it whatever it's solved as, if anything (see
    // `Compiler::with_argument_requirement`). The call's generics are narrowed for it when what
    // they're solved as otherwise isn't (see `CallSolver::solve_narrowed`)
    //
    // One that binds generics of its own, such as `fn<X>(a: X) -> X`, is only checked once the
    // call's return type is generalized over them, as the compiler does after solving (see
    // `generalized_call`). That's only what the call is checked with for its own expected type,
    // so a requirement that binds generics is left out
    let generic_required =
        |ctx: &mut Compiler, ty: TypeId| !expected_generic_depths(ctx, ty).is_empty();
    let required_type = if ctx.is_expected_type_provisional() {
        requirement.filter(|&ty| !generic_required(ctx, ty))
    } else {
        final_expected
    };

    // What the required type infers for the callee's generics, whose upper bounds that every
    // solution has (see `Inference::required_upper_bounds`) are what its arguments are required
    // to be assignable to.
    //
    // A required type that binds generics only gives the generics upper bounds in terms of
    // them, which aren't in scope for an argument, so it requires nothing of them
    let required = required_type
        .filter(|&ty| !generic_required(ctx, ty))
        .and_then(|ty| {
            if Some(ty) == expected_type
                && let Some(expected) = &expected
            {
                return expected_finished.then(|| expected.clone());
            }

            let mut bound = expected_generic_depths(ctx, ty);
            bound.retain(|&generic, _| !is_type_in_scope(ctx, generic));
            let (required, finished) = infer_expected(ctx, ty, &bound);
            finished.then_some(required)
        });

    // The instances of generic functions passed as arguments are solved with the callee's generics
    base.set_instance_policy(InstancePolicy::Joint);

    let solver = ctx.call_solver(expected, narrower.as_ref());
    let solver = Rc::new(if fallback_only {
        solver.fallback_only()
    } else {
        solver
    });

    let mut arguments =
        vec![ArgumentState::new(ctx.builtins().unresolved.hir); function.params.len()];

    let enclosing = ctx.inferred_generics();
    let call = CallCandidates {
        function,
        shared: function
            .generics
            .iter()
            .any(|generic| enclosing.contains(generic)),
        base: &base,
    };

    let mut order = ArgumentOrder::new(ctx, function, args, base.clone());

    // The callee's generics that the type expected of the call can bound (see
    // `ArgumentOrder::deferred`)
    let expected_returns = if expected_type.is_some() {
        generics_in(ctx, function, function.ret)
    } else {
        IndexSet::new()
    };

    // The element lambdas that are waiting for later arguments, with the argument they're in (see
    // `DeferredLambda`)
    let mut waiting: Vec<(usize, DeferredLambda)> = Vec::new();

    ctx.push_call_frame(function.generics.clone());

    while !order.is_done() {
        let inferred = ctx.solve_arguments(&solver, order.merged());

        // Use what has been inferred from the previous arguments as the expected type, along
        // with what the type expected of the call implies
        let mut mappings = ctx.argument_mappings(&solver, order.merged(), &inferred, true);

        let i = order.next(ctx, function, &inferred, &mappings, renamed);

        let expr = &args[i];
        let param = function.params[i].ty;

        // An argument with a lambda of its own in it only takes the expected type's bounds for the
        // generics the arguments map when nothing can add to them afterward (see
        // `ArgumentOrder::takes_expected_bounds` and `CallSolver::argument_mappings`)
        if !order.takes_expected_bounds(i) {
            mappings = ctx.argument_mappings(&solver, order.merged(), &inferred, false);
        }

        // A call that's widened to the expected type's own bounds takes what the arguments so far
        // map as a narrower expected type, for its generics that nothing else gives a type, such
        // as `A` in `nothing<A>(v: Int) -> A` (see `compile_call_argument`)
        let mut narrower_mappings = None;

        if order.widens_to_expected(i) {
            narrower_mappings = Some(mappings.clone());
            ctx.widen_to_expected(&solver, order.merged(), &mut mappings);
        }

        // Only a lambda that's an element of a pair or list that the argument is made of is
        // refined against the callee's renamed generics (see `compile_element`), so the others
        // skip working out what's restored
        let restored = if !renamed.is_empty()
            && matches!(expr, AstExpr::PairExpr(_) | AstExpr::ListExpr(_))
            && has_lambda(expr)
        {
            restorable(ctx, &mappings, renamed)
        } else {
            HashMap::new()
        };

        // Only worked out for an argument with element lambdas, which is all it's used for
        let depends = if order.has_element_lambdas(i) {
            order.dependencies(ctx, function, &inferred, &waiting, renamed, Some(i))
        } else {
            HashMap::new()
        };

        // Decided with what has a type so far, before the caller's generics stand in for what
        // doesn't
        let to_defer = order.deferred(
            i,
            &expected_returns,
            final_expected.is_none(),
            &depends,
            &with_dependencies(&waiting_reads(&waiting), &depends),
            (&mappings, !renamed.is_empty()),
        );

        restore_renamed(ctx, &mut mappings, renamed);
        let restored_here = restored.clone();

        let expected = substitute_with_mappings(ctx.types_mut(), param, &mappings);
        let narrower = narrower_mappings.map(|mut narrower_mappings| {
            restore_renamed(ctx, &mut narrower_mappings, renamed);
            substitute_with_mappings(ctx.types_mut(), param, &narrower_mappings)
        });

        ctx.set_argument_frame(Some(ArgumentFrame {
            infer: order.merged().clone(),
            own_infer: order.merged().clone(),
            solver: solver.clone(),
            renamed: if restored.is_empty() {
                HashMap::new()
            } else {
                renamed.clone()
            },
            restored,
            own_generics: Vec::new(),
            element_refinement: None,
            wrap_lambdas: matches!(expr, AstExpr::PairExpr(_) | AstExpr::ListExpr(_)),
            lambda_parameters: Vec::new(),
            to_defer,
            deferred: Vec::new(),
        }));

        // The parameter's type is only a hint while it references the callee's generics, since
        // what they're inferred as so far can still widen. It isn't one when the call's own expected
        // type is final and those generics only occur where values flow out of the argument: then
        // a solution that fits the call's expected type has the argument fit it, so
        // `wrap(map(fn(x) => x, 5))` with `(5, Int)` expected solves `map` for `5`. Where values
        // flow into the argument, such as `A` in `fn(a: A) -> B`, what they're inferred as so far
        // can be wider than the solution, which would ask more of the argument
        let writes = order.writes(i);
        let hint = !writes.is_empty()
            && (final_expected.is_none()
                || writes
                    .iter()
                    .any(|&generic| occurs_contravariantly(ctx, param, generic)));

        // A lambda expected to be exactly the parameter's function type wraps the callee's
        // generics in its parameters (see `ParameterWrap`)
        let params = match ctx.ty(param).clone() {
            Type::Function(function_param) if function_param.generics.is_empty() => {
                Some(function_param.param_types().collect::<Vec<_>>())
            }
            _ => None,
        };

        // What the argument is required to be assignable to however the generics are solved:
        // the parameter's type with each of them as an upper bound that every solution of the
        // required type has (see `Inference::required_upper_bounds`), if it has one and only
        // occurs where values flow out of the argument. Every solution of the call has the
        // generic at most that, so the argument has to be assignable to it, as in
        // `let result: 5 = identity(first(value));`
        let requirement = required
            .as_ref()
            .filter(|_| !writes.is_empty())
            .and_then(|required| {
                let mut bounds = HashMap::new();

                for &generic in writes {
                    if occurs_contravariantly(ctx, param, generic) {
                        return None;
                    }

                    let upper = required
                        .required_upper_bounds(ctx.types_mut(), generic)
                        .into_iter()
                        .find(|&ty| !references_generics(ctx, ty, &function.generics))?;
                    bounds.insert(generic, upper);
                }

                // A call in parentheses is the argument as much as one without them
                let mut call = expr.clone();

                while let AstExpr::GroupExpr(group) = &call
                    && let Some(inner) = group.expr()
                {
                    call = inner;
                }

                let ty = substitute_with_mappings(ctx.types_mut(), param, &bounds);
                Some((call.syntax().text_range(), ty))
            });

        let (value, mut parameters) = ctx.with_expected_type_fallback(false, |ctx| {
            ctx.with_argument_requirement(requirement, |ctx| {
                ctx.with_expected_type_provisional(!writes.is_empty(), |ctx| {
                    ctx.with_expected_type_hint(hint, |ctx| match expr {
                        AstExpr::LambdaExpr(lambda) => {
                            let wrap = match &params {
                                Some(params) => ParameterWrap::Generics {
                                    params,
                                    generics: &function.generics,
                                    mappings: &mappings,
                                },
                                None => ParameterWrap::Whole,
                            };

                            compile_lambda_argument(
                                ctx,
                                expr,
                                lambda,
                                Some(expected),
                                Some(param),
                                &wrap,
                            )
                        }
                        AstExpr::FunctionCallExpr(call) if narrower.is_some() => (
                            compile_call_argument(ctx, expr, call, Some(expected), narrower),
                            Vec::new(),
                        ),
                        _ => (compile_expr(ctx, expr, Some(expected)), Vec::new()),
                    })
                })
            })
        });

        // The lambdas among the elements of a pair or list argument (see `compile_element`), and
        // the caller's generics that are their own in some of them
        let mut own_generics = Vec::new();

        if let Some(argument) = ctx.argument_frame_mut() {
            parameters.append(&mut argument.lambda_parameters);
            waiting.extend(argument.deferred.drain(..).map(|lambda| (i, lambda)));
            own_generics = std::mem::take(&mut argument.own_generics);
        }

        ctx.take_argument_frame();

        let argument = &mut arguments[i];
        argument.result = value.hir;
        argument.check = Some(ArgumentCheck {
            expr: expr.clone(),
            ty: value.ty,
            param,
        });
        argument.lambda_parameters = parameters;
        argument.restored = restored_here;
        argument.own = own_generics;

        let (generic_types, candidates) = call.argument(ctx, argument, &[]);
        argument.generic_types = generic_types;
        order.add_candidates(i, candidates);

        // The element lambdas that were waiting for the arguments compiled so far, once nothing
        // still to be compiled adds to what they read. Those that are ready together are compiled
        // with what the arguments give then, and their arguments' candidates are replaced once
        // they all are. One isn't ready while another waiting lambda returns a generic it reads,
        // unless they all are, as two that return what each other reads
        while !waiting.is_empty() {
            let inferred = ctx.solve_arguments(&solver, order.merged());
            let depends = order.dependencies(ctx, function, &inferred, &waiting, renamed, None);

            let settled: Vec<usize> = (0..waiting.len())
                .filter(|&index| order.is_settled(&waiting[index].1.lambda.reads, &depends))
                .collect();

            let Some(&first) = settled.first() else {
                break;
            };

            let mut ready: Vec<usize> = settled
                .iter()
                .copied()
                .filter(|&index| {
                    let reads = with_dependencies(&waiting[index].1.lambda.reads, &depends);

                    !waiting.iter().enumerate().any(|(other, (_, lambda))| {
                        other != index
                            && lambda
                                .lambda
                                .returns
                                .iter()
                                .any(|generic| reads.contains(generic))
                    })
                })
                .collect();

            if ready.is_empty() {
                ready.push(first);
            }

            // What the arguments give the generics, with the caller's generics that are the
            // arguments' own in a recursive call (see `ArgumentFrame::own_generics`)
            let merged = merged_with_own(ctx, &order, &call, &arguments);
            let inferred = ctx.solve_arguments(&solver, &merged);
            let mut mappings = ctx.argument_mappings(&solver, &merged, &inferred, true);
            absorb_members(ctx, &mut mappings);

            let mut compiled: IndexSet<usize> = IndexSet::new();

            for index in ready.into_iter().rev() {
                let (argument, lambda) = waiting.remove(index);
                let (parameters, own) = compile_deferred_lambda(
                    ctx,
                    mappings.clone(),
                    &lambda,
                    &arguments[argument].restored,
                    renamed,
                );

                // The caller's generics that are its own in its parameters stay their own however
                // the call is solved (see `with_own`), since they're what it was compiled with
                let state = &mut arguments[argument];
                state.waited_own.extend(own.iter().cloned());
                state.own.extend(own);
                state.lambda_parameters.extend(parameters);
                compiled.insert(argument);
            }

            for argument in compiled {
                let (generic_types, candidates) = call.argument(ctx, &arguments[argument], &[]);
                arguments[argument].generic_types = generic_types;
                order.replace_candidates(argument, candidates);
            }
        }
    }

    debug_assert!(waiting.is_empty(), "every element lambda is compiled");

    ctx.pop_call_frame();

    // The missing arguments are left as placeholders, which the caller has reported
    if args.len() < function.params.len() {
        debug!("Unresolved function call argument");
        ctx.note_placeholder();
    }

    let Solution {
        mappings,
        unrestored,
        mut checks,
    } = solve_compiled_call(
        ctx,
        (&call, &solver),
        &mut order,
        &mut arguments,
        renamed,
        (final_expected, required_type),
    );

    let checks = order
        .candidate_order()
        .into_iter()
        .filter_map(|i| checks[i].take())
        .collect();

    InferredCall {
        results: arguments.iter().map(|argument| argument.result).collect(),
        checks,
        mappings,
        unrestored,
    }
}

/// Solves a call once its arguments are compiled, for its final expected type and the type it's
/// required to be assignable to, if it has them (see `infer_call`).
fn solve_compiled_call(
    ctx: &mut Compiler,
    (call, solver): (&CallCandidates<'_>, &CallSolver),
    order: &mut ArgumentOrder,
    arguments: &mut [ArgumentState],
    renamed: &HashMap<TypeId, TypeId>,
    (expected_type, required_type): (Option<TypeId>, Option<TypeId>),
) -> Solution {
    let function = call.function;

    // Only a call that the arguments don't solve otherwise is solved again with lambdas' untyped
    // parameters as vars, so that nothing that compiled before changes, and only for a final
    // expected type. The checks are made with the renamed generics restored, as the call is checked
    let has_lambda_parameters = arguments
        .iter()
        .any(|argument| !argument.lambda_parameters.is_empty());

    // The caller's generics that are their own in some lambdas' parameters only change those
    // arguments' candidates, and only for solving the call: the arguments are compiled with them
    // as if nothing were the caller's own, so that what they're compiled against is as it was. A
    // lambda's whole type leaves out the own generics of every parameter its body references,
    // since a call in it that sees through them could return any of them (see
    // `restored_for_lambda`), which can be more than it returns, so a call that the candidates
    // don't solve is solved with those it was compiled with
    let has_own = arguments.iter().any(|argument| !argument.own.is_empty());
    let mut with_own = has_own;
    // What the first solve gave, where it didn't solve the call: if the second one doesn't either,
    // the call is reported with the first one's types, in which the caller's own generics are as
    // they are in the lambdas' parameters
    let mut first_unsolved = None;

    let solution = loop {
        if has_own {
            for (i, argument) in arguments.iter_mut().enumerate() {
                if argument.own.is_empty() || argument.check.is_none() {
                    continue;
                }

                let own = if with_own {
                    &argument.own
                } else {
                    &argument.waited_own
                };
                let (generic_types, candidates) = call.argument(ctx, argument, own);
                argument.generic_types = generic_types;
                order.replace_candidates(i, candidates);
            }
        }

        let mappings = ctx.solve_call(solver, order.merged().clone());
        let mut solution = restored_solution(ctx, function, mappings, renamed, arguments);
        let mut solved = true;

        if has_lambda_parameters
            && !solves_call(
                ctx,
                function,
                &solution.checks,
                &solution.mappings,
                expected_type,
            )
        {
            let mut alternative = checks_of(arguments);
            let mut alternative_mappings = solve_with_lambda_parameters(
                ctx,
                function,
                solver,
                order,
                arguments,
                &mut alternative,
            );

            // What the vars are inferred as can reference the renamed generics too
            let alternative_unrestored = unrestored_mappings(&alternative_mappings, renamed);
            let restored = restore_renamed(ctx, &mut alternative_mappings, renamed);

            for check in alternative.iter_mut().flatten() {
                check.ty = substitute_with_mappings(ctx.types_mut(), check.ty, &restored);
            }

            // It's only used if it solves the call, so a call that still fails reports what it
            // did, and if it doesn't infer a generic as a type that references the callee's
            // generics, which a call nested in an argument of a call to the same function
            // shares with it. This is a safety net: no program has been found that reaches it
            if solves_call(
                ctx,
                function,
                &alternative,
                &alternative_mappings,
                expected_type,
            ) && !alternative_mappings
                .values()
                .any(|&ty| references_generics(ctx, ty, &function.generics))
            {
                solution = Solution {
                    mappings: alternative_mappings,
                    unrestored: alternative_unrestored,
                    checks: alternative,
                };
            } else {
                solved = false;
            }
        }

        if solved {
            break solution;
        }

        if !with_own {
            break first_unsolved.unwrap_or(solution);
        }

        first_unsolved = Some(solution);
        with_own = false;
    };

    // A call that what its generics are solved as doesn't solve, for a type it's required to be
    // assignable to, is rejected: an argument isn't assignable to its parameter, or the return
    // type to the required type. So its generics are narrowed (see `CallSolver::solve_narrowed`),
    // and that's used if it solves it. A call that's solved isn't narrowed, so that nothing that
    // compiles changes
    match required_type {
        Some(required_type)
            if !solves_call_as_checked(
                ctx,
                function,
                &solution.checks,
                &solution.mappings,
                required_type,
            ) =>
        {
            let narrowed = ctx.solve_call_narrowed(solver, order.merged().clone());
            let narrowed = restored_solution(ctx, function, narrowed, renamed, arguments);

            if narrowed.mappings != solution.mappings
                && solves_call_as_checked(
                    ctx,
                    function,
                    &narrowed.checks,
                    &narrowed.mappings,
                    required_type,
                )
            {
                narrowed
            } else {
                solution
            }
        }
        _ => solution,
    }
}

/// What solving a call as `mappings` gives once the renamed generics in them are restored (see
/// `restore_renamed`), with its lambda arguments' untyped parameters instantiated with them (see
/// `instantiate_generic_parameters`).
fn restored_solution(
    ctx: &mut Compiler,
    function: &FunctionType,
    mut mappings: HashMap<TypeId, TypeId>,
    renamed: &HashMap<TypeId, TypeId>,
    arguments: &[ArgumentState],
) -> Solution {
    let unrestored = unrestored_mappings(&mappings, renamed);
    restore_renamed(ctx, &mut mappings, renamed);

    let mut checks = checks_of(arguments);

    if arguments
        .iter()
        .any(|argument| !argument.lambda_parameters.is_empty())
    {
        instantiate_generic_parameters(ctx, function, arguments, &mappings, &mut checks);
    }

    Solution {
        mappings,
        unrestored,
        checks,
    }
}

/// The depth of the first function, from the callee (0) through the functions it returns, whose
/// parameters reference each of the callee's generics. A generic that none of them do has the depth
/// of the last function returned, since `generalize` stops there.
fn fixed_depths(ctx: &mut Compiler, function: &FunctionType) -> HashMap<TypeId, usize> {
    let mut depths = HashMap::new();
    let mut current = function.clone();
    let mut reached = 0;

    for depth in 0..=MAX_GENERALIZED_LEVELS {
        reached = depth;

        for &generic in &function.generics {
            if !depths.contains_key(&generic)
                && current
                    .param_types()
                    .any(|param| contains_generic(ctx.types_mut(), param, generic))
            {
                depths.insert(generic, depth);
            }
        }

        let returned = rue_types::unwrap_semantic(ctx.types_mut(), current.ret, true);

        let Type::Function(returned) = ctx.ty(returned).clone() else {
            break;
        };

        current = returned;
    }

    for &generic in &function.generics {
        depths.entry(generic).or_insert(reached);
    }

    depths
}

/// The generics of the function types expected of a call, and of those that they return, each with
/// the depth of the outermost function type that binds it, from 1 for the expected function types.
/// Those of the function types anywhere else in the expected type, such as in a pair, have no depth
/// (`usize::MAX`), since `generalize` never generalizes over them.
fn expected_generic_depths(ctx: &mut Compiler, expected_type: TypeId) -> HashMap<TypeId, usize> {
    let mut bound = HashMap::new();
    let mut expected = vec![expected_type];
    let mut visited = HashSet::from([expected_type]);

    for depth in 1..=MAX_GENERALIZED_LEVELS {
        let functions: Vec<FunctionType> = expected
            .iter()
            .flat_map(|&ty| rue_types::extract_functions(ctx.types_mut(), ty, false))
            .collect();

        if functions.is_empty() {
            break;
        }

        // A recursive function type binds the same generic again at each depth, where the
        // outermost one is what `generalize` copies
        for function in &functions {
            for &generic in &function.generics {
                bound.entry(generic).or_insert(depth);
            }
        }

        // A type that was already walked, such as a recursive alias returned by several of its
        // members, only binds generics that it bound further out, so the walk doesn't repeat it
        expected = functions
            .iter()
            .map(|function| function.ret)
            .filter(|&ret| visited.insert(ret))
            .collect();
    }

    for generic in rue_types::function_type_generics(ctx.types_mut(), expected_type) {
        bound.entry(generic).or_insert(usize::MAX);
    }

    bound
}

/// Whether a member of the type expected of a pair or list could hold it as another pair than the
/// ones its elements are expected to be, such as `Any` in `(5, Int) | Any`. A generic can't, since
/// it's rigid where the value is compiled. The element types that
/// the pairs give are then only a hint (see `Compiler::with_expected_type_hint`).
pub(crate) fn holds_other_pairs(ctx: &mut Compiler, ty: TypeId) -> bool {
    let ty = rue_types::substitute(ctx.types_mut(), ty);
    let mut stack = vec![ty];
    let mut visited = std::collections::HashSet::new();

    while let Some(ty) = stack.pop() {
        if !visited.insert(ty) {
            continue;
        }

        match ctx.ty(ty).clone() {
            Type::Any => return true,
            Type::Ref(inner) => stack.push(inner),
            Type::Alias(alias) => stack.push(alias.inner),
            Type::Struct(ty) => stack.push(ty.inner),
            Type::Union(union) => stack.extend(union.types),
            _ => {}
        }
    }

    false
}

/// What making an argument's candidates for the callee's generics takes that's the same for every
/// argument of a call (see `CallCandidates::argument`).
struct CallCandidates<'a> {
    function: &'a FunctionType,
    /// Whether the callee's generics are an enclosing call's, which is calling the same function.
    shared: bool,
    base: &'a Inference,
}

impl CallCandidates<'_> {
    /// The types of a compiled argument's lambda parameters that its lambdas are checked with (see
    /// `generic_parameter_types`), and its candidates for the callee's generics, with the caller's
    /// generics in `own_generics` as its own (see `ArgumentFrame::own_generics`). They're collected
    /// against the unsubstituted parameter type, so that every argument contributes to the
    /// generics rather than only the first one to mention them.
    fn argument(
        &self,
        ctx: &mut Compiler,
        argument: &ArgumentState,
        own_generics: &OwnGenerics,
    ) -> (GenericTypes, Inference) {
        let check = argument.check.as_ref().expect("the argument is compiled");
        let (ty, param) = (check.ty, check.param);
        let (function, base) = (self.function, self.base);
        let parameters = &argument.lambda_parameters;
        let restored = &argument.restored;

        let generic_types = generic_parameter_types(
            ctx,
            function,
            self.shared,
            parameters,
            restored,
            own_generics,
        );
        // The lambdas' untyped parameters that were compiled as the caller's generics standing for the
        // callee's renamed ones count as those for what the argument infers, as they do where the
        // lambdas are checked (see `generic_parameter_types`), so that it doesn't take the caller's
        // generic as itself, such as `T` from `x7` in `[(fn(x6: Int) => x6, 1), (fn(x7) => x7, 1)]`
        // where the first element of a pair is compiled where it is, which doesn't refine it (see
        // `compile_pair_expr` and `DeferredLambda`). Not where a lambda's body names that generic, as
        // `let y: T = x` does, which keeps it as main had it (#362, 13k6)
        let originals: Vec<TypeId> = restored.keys().copied().collect();
        let references: HashMap<TypeId, TypeId> = parameters
            .iter()
            .zip(&generic_types.types)
            .filter_map(|(parameter, &ty)| {
                ty.filter(|&ty| ty != parameter.ty)
                    .map(|ty| (parameter, ty))
            })
            .filter(|(parameter, _)| !parameter.lambda.names_types(ctx, &originals))
            .map(|(parameter, ty)| (parameter.reference, ty))
            .collect();
        let unrestored = unrestored_lambdas(ctx, &generic_types, &HashMap::new());
        let mut infer = base.clone();

        if !references.is_empty() {
            let mut mappings = unrestored.clone();
            mappings.extend(references);
            let ty = substitute_with_mappings(ctx.types_mut(), ty, &mappings);
            ctx.infer_type(ty, param, &mut infer);

            // Where that gives a generic a candidate with itself in it, such as from
            // `fn(y) => if c { y } else { y }`, which returns the renamed generic twice, the caller's
            // generic it stands for is what's known of it, as where nothing counts as the renamed one
            if !names_its_own_generic(ctx, function, &infer) {
                return (generic_types, infer);
            }

            infer = base.clone();
        }

        let ty = substitute_with_mappings(ctx.types_mut(), ty, &unrestored);
        ctx.infer_type(ty, param, &mut infer);
        (generic_types, infer)
    }
}

/// Whether any of the callee's generics has a candidate with itself in it, directly or through the
/// candidates of the others it names, such as two that are each other's.
fn names_its_own_generic(ctx: &mut Compiler, function: &FunctionType, infer: &Inference) -> bool {
    let names: HashMap<TypeId, Vec<TypeId>> = function
        .generics
        .iter()
        .map(|&generic| {
            let named = infer
                .candidates(generic)
                .map_or_else(Vec::new, |candidates| {
                    let types: Vec<TypeId> = candidates
                        .lower
                        .iter()
                        .chain(&candidates.upper)
                        .chain(&candidates.held)
                        .copied()
                        .collect();

                    function
                        .generics
                        .iter()
                        .copied()
                        .filter(|&other| {
                            types
                                .iter()
                                .any(|&ty| contains_generic(ctx.types_mut(), ty, other))
                        })
                        .collect()
                });
            (generic, named)
        })
        .collect();

    function.generics.iter().any(|&generic| {
        let mut seen = HashSet::new();
        let mut stack = names.get(&generic).cloned().unwrap_or_default();

        while let Some(other) = stack.pop() {
            if other == generic {
                return true;
            }

            if seen.insert(other) {
                stack.extend(names.get(&other).into_iter().flatten().copied());
            }
        }

        false
    })
}

/// The other callee generics that each mapping references (see `ArgumentOrder::is_settled`).
fn mapping_dependencies(
    ctx: &mut Compiler,
    function: &FunctionType,
    mappings: &HashMap<TypeId, TypeId>,
) -> HashMap<TypeId, IndexSet<TypeId>> {
    mappings
        .iter()
        .map(|(&generic, &ty)| {
            let mut others = generics_in(ctx, function, ty);
            others.shift_remove(&generic);
            (generic, others)
        })
        .collect()
}

/// The generics, and the other callee generics that their mappings so far reference, following
/// them through each other (see `mapping_dependencies`), such as `W` for `T` when `T` is mapped to
/// `W | Int`.
fn with_dependencies(
    generics: &IndexSet<TypeId>,
    depends: &HashMap<TypeId, IndexSet<TypeId>>,
) -> IndexSet<TypeId> {
    let mut generics = generics.clone();
    let mut index = 0;

    while let Some(&generic) = generics.get_index(index) {
        if let Some(others) = depends.get(&generic) {
            generics.extend(others);
        }

        index += 1;
    }

    generics
}

/// The generics that the untyped parameters of the element lambdas still waiting read.
fn waiting_reads(waiting: &[(usize, DeferredLambda)]) -> IndexSet<TypeId> {
    waiting
        .iter()
        .flat_map(|(_, lambda)| lambda.lambda.reads.iter().copied())
        .collect()
}

/// Compiles an element lambda that was waiting (see `DeferredLambda`) against what the arguments
/// give the callee's generics now (`mappings`), and puts it in place of its placeholder. It returns
/// its untyped parameters, and in a recursive call, the caller's generics that are the caller's own
/// in each of them (see `ArgumentFrame::own_generics`): those in what the arguments give, which
/// come from the caller's values, and not the ones that stand for a renamed generic that nothing
/// gives a type (`restored`, see `restorable`), which the parameter keeps as that generic.
fn compile_deferred_lambda(
    ctx: &mut Compiler,
    mut mappings: HashMap<TypeId, TypeId>,
    lambda: &DeferredLambda,
    restored: &HashMap<TypeId, TypeId>,
    renamed: &HashMap<TypeId, TypeId>,
) -> (Vec<LambdaParameter>, ArgumentOwn) {
    // One whose body can't use a parameter as a generic (see `ElementLambda::unusable_generics`),
    // where nothing gave the generic a type, is compiled without a type expected of it, so that
    // it's the parameter whose type is reported as not inferred, rather than what its body does
    // with a generic of the callee's, which the call doesn't name. A
    // generic that the arguments give as the caller's generic standing for it is given a type,
    // which is what the body is then reported against
    let uninferred = lambda
        .lambda
        .unusable_generics
        .iter()
        .any(|generic| !mappings.contains_key(generic));

    // A renamed generic that's given a type the caller's generic standing for it takes isn't
    // given one by that, as `Same<T>` for `T`
    for (&generic, copy) in restored {
        if let Some(&ty) = mappings.get(copy)
            && ctx.is_assignable(ty, generic)
        {
            mappings.remove(copy);
        }
    }

    let own = own_generics_of(
        ctx,
        &lambda.expr,
        (lambda.lambda.ty, &mappings),
        restored,
        renamed,
    );

    if !restored.is_empty() {
        restore_renamed(ctx, &mut mappings, renamed);
    }

    let expected = (!uninferred)
        .then(|| substitute_with_mappings(ctx.types_mut(), lambda.lambda.ty, &mappings));

    let (value, mut parameters) = ctx.with_expected_type_fallback(false, |ctx| {
        ctx.with_expected_type_hint(true, |ctx| {
            compile_lambda_argument(
                ctx,
                &AstExpr::LambdaExpr(lambda.expr.clone()),
                &lambda.expr,
                expected,
                Some(lambda.lambda.ty),
                &ParameterWrap::Whole,
            )
        })
    });

    // The placeholder becomes the lambda, in the argument's value and type, so the argument's type
    // keeps the references its parameters were compiled as (see `solve_with_lambda_parameters`)
    let hir = ctx.hir(value.hir).clone();
    *ctx.hir_mut(lambda.hir) = hir;
    let ty = ctx.ty(value.ty).clone();
    *ctx.ty_mut(lambda.ty) = ty;

    for parameter in &mut parameters {
        parameter.lambda.ty = lambda.ty;
    }

    let own = own_by_parameter(&parameters, &own);

    (parameters, own)
}

/// The caller's generics that are a lambda's own (see `own_generics_of`), by the reference each of
/// its untyped parameters was compiled as. Each of them is wrapped, so they're in the same order,
/// unless one of them isn't, such as a spread, and then each one counts all of them as its own.
fn own_by_parameter(parameters: &[LambdaParameter], own: &[Vec<TypeId>]) -> ArgumentOwn {
    let all: Vec<TypeId> = own.iter().flatten().copied().collect();

    parameters
        .iter()
        .enumerate()
        .map(|(index, parameter)| {
            let own = if parameters.len() == own.len() {
                own[index].clone()
            } else {
                all.clone()
            };
            (parameter.reference, own)
        })
        .filter(|(_, own)| !own.is_empty())
        .collect()
}

/// Checks each lambda whose untyped parameters were compiled as the callee's generics that hadn't
/// been inferred yet, such as `x: A` in `map_list([fn(x) => x], 5)`, with them as what they're
/// inferred as (see `generic_parameter_types` for which ones are), and with the generics in them as
/// those wherever else they are in its type, such as its return type when a call in its body sees
/// through a parameter (see `instantiations`). Its body was compiled with the generic as a type it
/// knows nothing about, so it works for whatever the generic is.
fn instantiate_generic_parameters(
    ctx: &mut Compiler,
    function: &FunctionType,
    arguments: &[ArgumentState],
    mappings: &HashMap<TypeId, TypeId>,
    checks: &mut [Option<ArgumentCheck>],
) {
    for (check, argument) in checks.iter_mut().zip(arguments) {
        let Some(check) = check else {
            continue;
        };

        let instantiated = instantiations(
            ctx,
            function,
            &argument.lambda_parameters,
            &argument.generic_types,
            mappings,
        );
        check.ty = instantiated.apply(ctx, check.ty, &HashMap::new());
    }
}

/// What each lambda parameter with a type with the callee's generics in it is instantiated as with
/// the mappings, keyed by its reference, and what each of those generics is within its lambda's
/// type (see `Instantiations`). A solution that still references the generics doesn't say what
/// they are, such as in a call nested in an argument of a call to the same function, which shares
/// them, so that parameter is left out.
fn instantiations(
    ctx: &mut Compiler,
    function: &FunctionType,
    parameters: &[LambdaParameter],
    generic_types: &GenericTypes,
    mappings: &HashMap<TypeId, TypeId>,
) -> Instantiations {
    let mut instantiated = Instantiations::default();

    for (parameter, &generic_type) in parameters.iter().zip(&generic_types.types) {
        let Some(generic_type) = generic_type else {
            continue;
        };

        let solution = substitute_with_mappings(ctx.types_mut(), generic_type, mappings);

        if references_generics(ctx, solution, &function.generics) {
            continue;
        }

        instantiated
            .references
            .insert(parameter.reference, solution);

        let lambda = if let Some(index) = instantiated
            .lambdas
            .iter()
            .position(|(ty, _)| *ty == parameter.lambda.ty)
        {
            index
        } else {
            instantiated
                .lambdas
                .push((parameter.lambda.ty, HashMap::new()));
            instantiated.lambdas.len() - 1
        };

        for generic in generics_in(ctx, function, generic_type) {
            if let Some(&ty) = mappings.get(&generic) {
                instantiated.lambdas[lambda].1.insert(generic, ty);
            }
        }

        // The caller's generic a parameter was compiled as in place of a renamed one is in the
        // lambda's type as itself, so it's what that generic is there (see `GenericTypes`)
        let Some((_, restored)) = generic_types
            .seen_through
            .iter()
            .find(|(ty, _)| *ty == parameter.lambda.ty)
        else {
            continue;
        };

        for (&original, copy) in restored {
            if contains_generic(ctx.types_mut(), generic_type, *copy)
                && let Some(&ty) = mappings.get(copy)
            {
                instantiated.lambdas[lambda].1.insert(original, ty);
            }
        }
    }

    instantiated
}

/// The type of each lambda in `GenericTypes::seen_through`, with the caller's generics that stand
/// for the callee's renamed ones as those, and with the other mappings, keyed by the lambda's type. A call in its body sees through a parameter compiled as a caller's generic, so
/// its return type can be that generic, which is the callee's renamed one there, as in
/// `ml([fn(x) => identity(x)], 5)` inside `ml`. It isn't in the rest of the argument, which can have
/// the caller's generic itself.
fn unrestored_lambdas(
    ctx: &mut Compiler,
    generic_types: &GenericTypes,
    others: &HashMap<TypeId, TypeId>,
) -> HashMap<TypeId, TypeId> {
    generic_types
        .seen_through
        .iter()
        .map(|(lambda, restored)| {
            let mut mappings = others.clone();
            mappings.extend(restored.iter().map(|(&k, &v)| (k, v)));
            (
                *lambda,
                substitute_with_mappings(ctx.types_mut(), *lambda, &mappings),
            )
        })
        .collect()
}

/// What an argument's lambda parameters are instantiated as (see `instantiations`).
#[derive(Default)]
struct Instantiations {
    /// What each parameter is, by its reference.
    references: HashMap<TypeId, TypeId>,
    /// For each lambda, by its type, what the callee's generics in its parameters' types are. A
    /// call in its body sees through a parameter's reference, so its type can have the generic
    /// itself, as the return type of `[fn(x) => id(x)]` does. Its body was compiled with the
    /// generic as a type it knows nothing about, so the generic is instantiated there too, but only
    /// in that lambda's type: another part of the argument, such as a lambda that captures a value
    /// of the generic in a call nested in a call to the same function, can have the generic itself
    lambdas: Vec<(TypeId, HashMap<TypeId, TypeId>)>,
}

impl Instantiations {
    /// The argument's type with its lambdas' parameters, and the parameters in `others`, as what
    /// they're instantiated as.
    fn apply(&self, ctx: &mut Compiler, ty: TypeId, others: &HashMap<TypeId, TypeId>) -> TypeId {
        let mut mappings = others.clone();
        mappings.extend(self.references.iter().map(|(&key, &value)| (key, value)));

        if mappings.is_empty() && self.lambdas.is_empty() {
            return ty;
        }

        // Each lambda's type is substituted on its own, with its generics, and put in its place
        let mut lambdas = HashMap::new();

        for (lambda, generics) in &self.lambdas {
            let mut within = mappings.clone();
            within.extend(generics.iter().map(|(&key, &value)| (key, value)));
            lambdas.insert(
                *lambda,
                substitute_with_mappings(ctx.types_mut(), *lambda, &within),
            );
        }

        mappings.extend(lambdas);
        substitute_with_mappings(ctx.types_mut(), ty, &mappings)
    }
}

/// The type of each of an argument's lambda parameters with the callee's generics in it, with the
/// caller's generics in `restored` that stand for renamed ones as those (see `unrestore`), if it
/// has them and its lambda can be checked with them as what they're inferred as (see
/// `instantiate_generic_parameters`). `shared` is whether the callee's generics are an enclosing
/// call's.
///
/// That's only so if nothing else in the body's scope has the generics. A call nested in an
/// argument of a call to the same function shares its generics, and the enclosing call's lambdas
/// can have values of them in scope, such as `cb` in `ac((fn(x, cb) => ac((fn(y, _) => cb(y),
/// …)), …))`. A parameter compiled as a caller's generic that stands for a renamed one (see
/// `restorable`), such as `x: T` in `f((fn(x) => x, 1))` inside `f<T>`, is the caller's own `T`,
/// which the caller's parameters have too, and which the body can name. Those lambdas are only
/// checked this way if their bodies don't reference a value from outside of them with the
/// generics (see `LambdaBody::captures_generics`), and their types don't name the caller's
/// generics other than through these parameters (see `names_restored_generics`).
fn generic_parameter_types(
    ctx: &mut Compiler,
    function: &FunctionType,
    shared: bool,
    parameters: &[LambdaParameter],
    restored: &HashMap<TypeId, TypeId>,
    own_generics: &OwnGenerics,
) -> GenericTypes {
    let originals: Vec<TypeId> = restored.keys().copied().collect();
    let generics: Vec<TypeId> = function
        .generics
        .iter()
        .copied()
        .chain(originals.iter().copied())
        .collect();

    // Whether each lambda can be checked that way, once per lambda
    let mut lambdas: Vec<(LambdaBody, bool)> = Vec::new();
    let mut seen_through = Vec::new();

    let types = parameters
        .iter()
        .map(|parameter| {
            let restored_here = restored_for_parameter(restored, own_generics, parameter.reference);
            let originals_here: Vec<TypeId> = restored_here.keys().copied().collect();
            let ty = unrestore(ctx, &restored_here, parameter.ty);

            if !references_generics(ctx, ty, &function.generics) {
                return None;
            }

            let is_restored = references_generics(ctx, parameter.ty, &originals_here);

            if !shared && !is_restored {
                return Some(ty);
            }

            let lambda = parameter.lambda;

            let instantiable = if let Some(&(_, instantiable)) =
                lambdas.iter().find(|(body, _)| *body == lambda)
            {
                instantiable
            } else {
                let instantiable = !lambda.captures_generics(ctx, &generics)
                    && match lambda_restored(ctx, lambda, parameters, (restored, own_generics)) {
                        LambdaRestored::Parameters => true,
                        LambdaRestored::SeenThrough(restored) => {
                            seen_through.push((lambda.ty, restored));
                            true
                        }
                        LambdaRestored::Named => false,
                    };
                lambdas.push((lambda, instantiable));
                instantiable
            };

            instantiable.then_some(ty)
        })
        .collect();

    GenericTypes {
        types,
        seen_through,
    }
}

/// The caller's generics that are their own in the types of some of an argument's lambda
/// parameters, by the parameter's reference (see `ArgumentFrame::own_generics`).
type OwnGenerics = [(TypeId, Vec<TypeId>)];

/// The caller's generics that are an argument's own in its lambdas' parameters (see `OwnGenerics`).
type ArgumentOwn = Vec<(TypeId, Vec<TypeId>)>;

/// The caller's generics in `restored` that stand for the callee's renamed ones in the type of the
/// lambda parameter, which are all of them but those that are its own there (see
/// `ArgumentFrame::own_generics`).
fn restored_for_parameter(
    restored: &HashMap<TypeId, TypeId>,
    own_generics: &OwnGenerics,
    reference: TypeId,
) -> HashMap<TypeId, TypeId> {
    without(
        restored,
        own_generics
            .iter()
            .filter(|(other, _)| *other == reference)
            .flat_map(|(_, own)| own),
    )
}

/// The caller's generics in `restored` that stand for the callee's renamed ones in the whole of
/// the lambda's type, where a call in its body that sees through its parameters can return them.
/// They're all of them but those that are their own in the type of any parameter the body
/// references (see `restored_for_parameter`), since a generic it returns could come from any of
/// those, and telling which would mean tracking it through the call.
fn restored_for_lambda(
    ctx: &Compiler,
    restored: &HashMap<TypeId, TypeId>,
    own_generics: &OwnGenerics,
    parameters: &[LambdaParameter],
    lambda: LambdaBody,
) -> HashMap<TypeId, TypeId> {
    without(
        restored,
        own_generics
            .iter()
            .filter(|(reference, _)| {
                parameters.iter().any(|parameter| {
                    parameter.reference == *reference && parameter.lambda == lambda
                }) && lambda.references_parameter(ctx, *reference)
            })
            .flat_map(|(_, own)| own),
    )
}

/// The restored generics other than `own`.
fn without<'a>(
    restored: &HashMap<TypeId, TypeId>,
    own: impl Iterator<Item = &'a TypeId>,
) -> HashMap<TypeId, TypeId> {
    let own: Vec<TypeId> = own.copied().collect();

    restored
        .iter()
        .filter(|(generic, _)| !own.contains(generic))
        .map(|(&generic, &copy)| (generic, copy))
        .collect()
}

/// For each of an argument's lambda parameters, the type with the callee's generics in it that its
/// lambda is checked with them as what they're inferred as, if it is (see
/// `generic_parameter_types`).
#[derive(Debug, Default, Clone)]
struct GenericTypes {
    types: Vec<Option<TypeId>>,
    /// The types of the lambdas whose types have the caller's generics that stand for the callee's
    /// renamed ones (see `restorable`) other than through their parameters, as their return types
    /// can when a call in their bodies sees through a parameter, while the bodies don't name them.
    /// There, those generics are the renamed ones they stand for, each with the generic it stands
    /// for in that lambda (see `restored_for_lambda`).
    seen_through: Vec<(TypeId, HashMap<TypeId, TypeId>)>,
}

/// Where the lambda's type has the caller's generics that stand for the callee's renamed ones (see
/// `lambda_restored`).
enum LambdaRestored {
    /// Only through its parameters.
    Parameters,
    /// Also through calls in its body that see through its parameters, which its body doesn't name
    /// them in, so there they're the renamed generics they stand for, each with the generic it
    /// stands for there (see `restored_for_lambda`).
    SeenThrough(HashMap<TypeId, TypeId>),
    /// Also where its body names them, so it doesn't work for any type in their place.
    Named,
}

/// Where the lambda's type has the caller's generics that stand for the callee's renamed ones,
/// other than through its parameters (see `LambdaRestored`).
fn lambda_restored(
    ctx: &mut Compiler,
    lambda: LambdaBody,
    parameters: &[LambdaParameter],
    (restored, own_generics): (&HashMap<TypeId, TypeId>, &OwnGenerics),
) -> LambdaRestored {
    let in_lambda = restored_for_lambda(ctx, restored, own_generics, parameters, lambda);
    let originals: Vec<TypeId> = in_lambda.keys().copied().collect();

    if !names_restored_generics(
        ctx,
        lambda,
        parameters,
        (restored, own_generics),
        &originals,
    ) {
        LambdaRestored::Parameters
    } else if lambda.names_types(ctx, &originals) {
        LambdaRestored::Named
    } else {
        LambdaRestored::SeenThrough(in_lambda)
    }
}

/// Whether the lambda's type has the generics in `originals` in it other than through its
/// parameters, which it can have if its body names them, as in `fn(x) => x as T`. Then its body
/// has values of them that don't come from its parameters, so it doesn't work for any type in
/// their place.
fn names_restored_generics(
    ctx: &mut Compiler,
    lambda: LambdaBody,
    parameters: &[LambdaParameter],
    (restored, own_generics): (&HashMap<TypeId, TypeId>, &OwnGenerics),
    originals: &[TypeId],
) -> bool {
    if restored.is_empty() {
        return false;
    }

    let own: HashMap<TypeId, TypeId> = parameters
        .iter()
        .filter(|parameter| parameter.lambda == lambda)
        .map(|parameter| {
            let restored = restored_for_parameter(restored, own_generics, parameter.reference);
            let ty = unrestore(ctx, &restored, parameter.ty);
            (parameter.reference, ty)
        })
        .collect();

    let ty = substitute_with_mappings(ctx.types_mut(), lambda.ty, &own);
    references_generics(ctx, ty, originals)
}

/// Whether the type references any of the generics.
fn references_generics(ctx: &mut Compiler, ty: TypeId, generics: &[TypeId]) -> bool {
    generics
        .iter()
        .any(|&generic| contains_generic(ctx.types_mut(), ty, generic))
}

/// Whether the mappings solve the call: each argument is assignable to its parameter, and the
/// call's return type to the type expected of it, if there is one.
fn solves_call(
    ctx: &mut Compiler,
    function: &FunctionType,
    checks: &[Option<ArgumentCheck>],
    mappings: &HashMap<TypeId, TypeId>,
    expected_type: Option<TypeId>,
) -> bool {
    arguments_assignable(ctx, checks, mappings)
        && expected_type.is_none_or(|expected_type| {
            let ret = substitute_with_mappings(ctx.types_mut(), function.ret, mappings);
            ctx.is_assignable(ret, expected_type)
        })
}

/// Whether the mappings solve the call as the compiler checks it once it's solved, for a type it's
/// required to be assignable to (see `solves_call`). For one that binds generics of its own, the
/// mappings are generalized over them first, as for the call's own expected type (see
/// `generalized_call`), and the arguments are checked with what that gives.
fn solves_call_as_checked(
    ctx: &mut Compiler,
    function: &FunctionType,
    checks: &[Option<ArgumentCheck>],
    mappings: &HashMap<TypeId, TypeId>,
    required_type: TypeId,
) -> bool {
    if expected_generic_depths(ctx, required_type).is_empty() {
        return solves_call(ctx, function, checks, mappings, Some(required_type));
    }

    let (mappings, ret) = generalized_call(ctx, function, required_type, mappings);

    arguments_assignable(ctx, checks, &mappings) && ctx.is_assignable(ret, required_type)
}

/// Whether each compiled argument is assignable to its parameter with the mappings.
fn arguments_assignable(
    ctx: &mut Compiler,
    checks: &[Option<ArgumentCheck>],
    mappings: &HashMap<TypeId, TypeId>,
) -> bool {
    checks.iter().flatten().all(|check| {
        let param = substitute_with_mappings(ctx.types_mut(), check.param, mappings);
        ctx.is_assignable(check.ty, param)
    })
}

/// Solves the call again with the untyped parameters of its lambda arguments as vars, each at most
/// the type its body was compiled against, and gives the mappings. Each lambda's check is updated
/// to its type with its parameters as what they're inferred as. A bound on a lambda's return type
/// then reaches its parameters: in `let result: 5 = map(fn(x) => x, 5)`, `x` was compiled as
/// `Int`, the widened `5`, but `B ≤ 5` makes it `5` (#335).
fn solve_with_lambda_parameters(
    ctx: &mut Compiler,
    function: &FunctionType,
    solver: &CallSolver,
    order: &ArgumentOrder,
    arguments: &[ArgumentState],
    checks: &mut [Option<ArgumentCheck>],
) -> HashMap<TypeId, TypeId> {
    // Each lambda's parameters, as their references, the types they refer to and their vars
    let mut lambdas = Vec::new();
    let mut replaced = vec![None; arguments.len()];

    for (i, argument) in arguments.iter().enumerate() {
        let parameters = &argument.lambda_parameters;

        if parameters.is_empty() {
            continue;
        }

        let check = checks[i].as_ref().expect("a lambda argument is checked");
        let (ty, param) = (check.ty, check.param);
        let mut infer = order.base.clone();
        let mut references = HashMap::new();
        let mut vars = Vec::new();

        for (parameter, generic_type) in parameters.iter().zip(&argument.generic_types.types) {
            let (reference, upper) = (parameter.reference, parameter.ty);

            // A parameter that's checked as what the callee's generics in its type are inferred as
            // (see `generic_parameter_types`) is inferred as that type rather than as a var, and
            // instantiated with this solve's mappings below
            if let &Some(generic_type) = generic_type {
                references.insert(reference, generic_type);
                continue;
            }

            if references_generics(ctx, upper, &function.generics) {
                continue;
            }

            let var = ctx.alloc_type(Type::Generic(Generic { name: None }));
            infer.add_var(var, upper);
            references.insert(reference, var);
            vars.push((reference, upper, var));
        }

        let lambdas_unrestored = unrestored_lambdas(ctx, &argument.generic_types, &references);
        references.extend(lambdas_unrestored);
        let ty = substitute_with_mappings(ctx.types_mut(), ty, &references);
        ctx.infer_type(ty, param, &mut infer);
        replaced[i] = Some(infer);
        lambdas.push((i, vars));
    }

    let mut mappings = ctx.solve_call(solver, order.merged_with(&replaced));
    let mut inferred_vars = HashMap::new();
    let mut inferred_lambdas = Vec::new();

    for (i, vars) in lambdas {
        let mut inferred = HashMap::new();

        for (reference, upper, var) in vars {
            // A parameter that isn't inferred as narrower than what its body was compiled against
            // is that type, as it was
            let ty = mappings
                .remove(&var)
                .filter(|&mapped| ctx.is_assignable(mapped, upper))
                .unwrap_or(upper);

            inferred.insert(reference, ty);
            inferred_vars.insert(var, ty);
        }

        inferred_lambdas.push((i, inferred));
    }

    // The callee's generics can be inferred through the vars
    for ty in mappings.values_mut() {
        *ty = substitute_with_mappings(ctx.types_mut(), *ty, &inferred_vars);
    }

    // The parameters that weren't vars are instantiated with what the generics are inferred as
    // here, since substituting the vars would see through their references
    for (i, inferred) in inferred_lambdas {
        let instantiated = instantiations(
            ctx,
            function,
            &arguments[i].lambda_parameters,
            &arguments[i].generic_types,
            &mappings,
        );

        if let Some(check) = &mut checks[i] {
            check.ty = instantiated.apply(ctx, check.ty, &inferred);
        }
    }

    mappings
}

/// The order a call's arguments are compiled in. An argument is compiled once the callee's
/// generics that compiling it depends on (its reads) are settled: no other argument that's still to
/// be compiled can add to them (its writes). So a nested call whose expected type needs a generic
/// that a later argument infers waits for it (#352), and so does an untyped lambda whose
/// parameters do, even if the generic already has a mapping (#351) or is the whole parameter type
/// (#350).
///
/// The rules that limit this keep an argument from being compiled against less than it was before
/// arguments waited for each other: a literal widened by an argument that went ahead of it, or a
/// mapping from the type expected of the call replaced by a weaker one. Each has a test in
/// `tests/types/generic_inference_argument_order.rue`.
///
/// Among the arguments that can go, the other arguments go in order, then the lambdas, the first
/// whose parameter types have been inferred first (#265). When none can, because they depend on
/// each other, they go in that order too.
struct ArgumentOrder {
    arguments: Vec<Argument>,
    /// The arguments still to be compiled, the others in order and then the lambdas in order.
    pending: Vec<usize>,
    /// The lambdas, in the order they were compiled.
    compiled_lambdas: Vec<usize>,
    /// The other arguments that were compiled.
    compiled_others: Vec<usize>,
    /// Whether no argument still to be compiled can add to the generics that the argument `next`
    /// gave last depends on (see `ArgumentOrder::takes_expected_bounds`).
    last_settled: bool,
    /// The candidates of each argument that was compiled, collected on their own.
    candidates: Vec<Option<Inference>>,
    /// The candidates of the compiled arguments, put together in `candidate_order`, starting from
    /// an inference with none.
    base: Inference,
    merged: Inference,
}

/// What `ArgumentOrder` knows about an argument.
struct Argument {
    kind: ArgumentKind,
    /// Whether it is, or has in it, a lambda.
    has_lambda: bool,
    /// Whether it is, or has in it, a lambda that isn't inside a call in it (see `has_own_lambda`).
    has_own_lambda: bool,
    /// Whether it's compiled against the expected type's own bounds (see
    /// `ArgumentOrder::widens_to_expected`). It's a call, since its own arguments add to the
    /// generics once it's compiled against them, as `8` does in `id(8)`, and its generics that
    /// only its expected type constrains, such as `A` in `nothing<A>(v: Int) -> A`, still take the
    /// narrower type (see `compile_call_argument`). It isn't a call without arguments, which has
    /// nothing of its own to add (infer-fuzz generator 2, seed 2855). It has no lambda in it, even
    /// one a call passes through, since a lambda's parameter types keep the narrower mappings.
    /// That's defensive: a call that passes a lambda through leaves its generic to its expected
    /// type, which the narrower type covers, and no program has been found where the two differ.
    widens: bool,
    /// The callee's generics whose solution compiling it depends on (see `reads_of`).
    reads: IndexSet<TypeId>,
    /// The callee's generics that comparing it with its parameter can add to: every one in the
    /// parameter type.
    writes: IndexSet<TypeId>,
    /// The generics that it can bound from above: those in the parameter types of the function
    /// types its parameter type has, such as `A` for `fn(a: A) -> B`.
    bounds_above: IndexSet<TypeId>,
    /// For a lambda, the generics among its reads that are its whole parameter type, or members
    /// of it.
    bare: IndexSet<TypeId>,
    /// For a pair or list, the lambdas among the elements of the pairs and lists it's made of
    /// (see `element_lambdas`).
    elements: ElementLambdas,
}

/// The lambdas among the elements of a pair or list argument (see `element_lambdas`).
#[derive(Debug, Default)]
struct ElementLambdas {
    /// By where they are.
    lambdas: HashMap<TextRange, ElementLambda>,
    /// Whether one of the argument's elements is a pair or list with no lambda in it that the
    /// type expected of it can't be followed into, as `(1, 2)` for `T`. What that one's own
    /// elements give the generics isn't known where a lambda beside it is, so in a call that isn't
    /// recursive, a lambda that can be compiled where it is beside one is (see
    /// `ArgumentOrder::deferred`).
    unfollowed: bool,
}

/// A lambda that's an element of a pair or list argument, or of one it's made of (see
/// `compile_element`).
#[derive(Debug, Clone)]
pub struct ElementLambda {
    /// The type it's expected to have, before it's substituted.
    ty: TypeId,
    /// The callee's generics in the types its untyped parameters are expected to have.
    reads: IndexSet<TypeId>,
    /// The callee's generics in the return types of the function types it's expected to be.
    returns: IndexSet<TypeId>,
    /// Whether it has no return type and its body has a call, lambda or type guard in it, or an
    /// untyped parameter is expected to have a type that has a generic in it without being one, so
    /// that its return type can be what those make of its parameters, which can be a type that no
    /// longer references the generics they were compiled as, as `identity(x)`, `x is nil` and
    /// `p.first` for `p: (T, Int)` can (see `ArgumentOrder::deferred`). A parameter that's the
    /// generic itself can't be used any other way that loses it: its body either keeps it, as
    /// `(x, x)` does, or doesn't compile against it, as `x + 1` doesn't.
    narrows: bool,
    /// The callee's generics that untyped parameters its body can't use as a value of a generic
    /// are expected to be (see `unusable_parameters`), as `x` in `x + 1` and `x.first` is for
    /// `x: T`. Where one of them has no mapping when the argument is compiled, the parameter is
    /// that generic where the lambda is, unless an element before it gives it a type there
    /// (`given_before`), so the lambda can't be compiled there, and it waits, at least for the
    /// rest of its argument, which can give the generic its type, as `5` does in
    /// `(fn(x) => x + 1, 5)` (see `ArgumentOrder::deferred`).
    unusable_generics: IndexSet<TypeId>,
    /// The callee's generics that untyped parameters whose fields the body reads are expected to
    /// be, whether or not a guard on the parameter lets it use them. A lambda with one that has no
    /// mapping waits while an argument still to be compiled can add to the generic, even where an
    /// element before it gives it a type, since what's added can be what has the field.
    field_generics: IndexSet<TypeId>,
    /// The callee's generics that an element before it in its argument gives a type where it is:
    /// one whose expected type has the generic in it, as `5` in `(5, fn(x) => x + 1)`, or a
    /// lambda that returns it (see `refine_element_expected`).
    given_before: IndexSet<TypeId>,
    /// Those of them that count in a recursive call: what a lambda before it whose parameters all
    /// have types returns doesn't reach it where it is, where the generic is one of the caller's
    /// standing for the callee's (see `restorable`), as `fn(x: Int) => 8` doesn't give `U` in
    /// `(fn(x: Int) => 8, (fn(y) => y - 1, 5))` for `(fn(a: T) -> U, (fn(a: U) -> T, T))`.
    given_in_recursive: IndexSet<TypeId>,
    /// Whether its body has a guard or a `match` in it, which is written for a parameter that
    /// can be more than one type (see `ArgumentOrder::deferred`).
    guards: bool,
    /// Whether another element of its argument reads a generic its return type has. What it
    /// returns reaches the other elements, which are compiled before a lambda that waits, as
    /// `fn(y) => y + 1` is in `(fn(_x) => 7, fn(y) => y + 1)` for
    /// `(fn(x: T) -> U, fn(y: U) -> Int)` (see `ArgumentOrder::deferred`).
    read_beside: bool,
}

/// The untyped parameters among `names` that a lambda's body can't use as a value of a generic,
/// those it reads a field of or has as the operand of an operator, as `x.first` and `x + 1` do,
/// and those it reads a field of, whether or not it can. A value of a generic has no fields, and
/// no operator takes one. An operator's operands don't count where the body has a guard or a
/// `match`, since one on a parameter, as in `if x is Int { x + 1 } else { 0 }`, gives it a type
/// the operator takes.
fn unusable_parameters(
    lambda: &AstLambdaExpr,
    names: &HashSet<String>,
) -> (HashSet<String>, HashSet<String>) {
    let Some(body) = lambda.body() else {
        return (HashSet::new(), HashSet::new());
    };

    // A binding with the parameter's name shadows it, as `x` in `let x = 1` and in
    // `let { x } = point` does
    let shadowed: HashSet<String> = body
        .syntax()
        .descendants()
        .filter_map(|node| {
            if let Some(binding) = AstNamedBinding::cast(node.clone()) {
                binding.name()
            } else {
                AstStructFieldBinding::cast(node)
                    .filter(|field| field.binding().is_none())
                    .and_then(|field| field.name())
            }
        })
        .map(|name| name.text().to_string())
        .collect();

    let parameter = |expr: Option<AstExpr>| {
        let mut expr = expr;

        while let Some(AstExpr::GroupExpr(group)) = &expr {
            expr = group.expr();
        }

        let Some(AstExpr::PathExpr(path)) = expr else {
            return None;
        };

        let segments: Vec<AstPathSegment> = path.segments().collect();
        let [segment] = segments.as_slice() else {
            return None;
        };

        let name = segment.name()?.text().to_string();

        (names.contains(&name) && !shadowed.contains(&name)).then_some(name)
    };

    // A guard or a `match` on the parameter itself gives it a type that has what the body uses,
    // as in `if x is (Int, Int) { x.first } else { 0 }`, so the body can use it as the generic,
    // though it still reads a field of it. One on something else, as `x.a` in
    // `if x.a is Int { x.a + 1 } else { 0 }`, only makes the operators not count
    let guarded: HashSet<String> = body
        .syntax()
        .descendants()
        .filter_map(|node| {
            if let Some(guard) = AstGuardExpr::cast(node.clone()) {
                parameter(guard.expr())
            } else {
                AstMatchExpr::cast(node).and_then(|expr| parameter(expr.expr()))
            }
        })
        .collect();

    let mut fields = HashSet::new();
    let mut operands = HashSet::new();
    let operators = !has_guard_or_match(&body);

    for node in body.syntax().descendants() {
        if let Some(access) = AstFieldAccessExpr::cast(node.clone()) {
            fields.extend(parameter(access.expr()));
        } else if !operators {
        } else if let Some(binary) = AstBinaryExpr::cast(node.clone()) {
            operands.extend(parameter(binary.left()));
            operands.extend(parameter(binary.right()));
        } else if let Some(prefix) = AstPrefixExpr::cast(node) {
            operands.extend(parameter(prefix.expr()));
        }
    }

    let mut unusable = operands;
    unusable.extend(
        fields
            .iter()
            .filter(|name| !guarded.contains(*name))
            .cloned(),
    );

    (unusable, fields)
}

/// Whether the expression has a guard or a `match` in it.
fn has_guard_or_match(expr: &AstExpr) -> bool {
    expr.syntax().descendants().any(|node| {
        AstGuardExpr::cast(node.clone()).is_some() || AstMatchExpr::cast(node).is_some()
    })
}

/// The candidates the call's first solve has, with the caller's generics that are the arguments'
/// own in their lambdas' parameters (see `ArgumentFrame::own_generics`).
fn merged_with_own(
    ctx: &mut Compiler,
    order: &ArgumentOrder,
    call: &CallCandidates<'_>,
    arguments: &[ArgumentState],
) -> Inference {
    let replaced: Vec<Option<Inference>> = arguments
        .iter()
        .map(|argument| {
            if argument.own.is_empty() || argument.check.is_none() {
                return None;
            }

            let (_, candidates) = call.argument(ctx, argument, &argument.own);
            Some(candidates)
        })
        .collect();

    order.merged_with(&replaced)
}

/// Leaves each type that's a union without the members that another member already takes, as the
/// second `(1, 2)` in `(1, 2) | (1, 2)`, which two values of the same type give a generic, and
/// `(2, 1)` in `(Int, 1) | (2, 1)`. A waiting lambda's parameter is then the type one of the
/// values gives it alone, whose fields' literals are widened as that one's are, and which can be
/// called where it's a function, as a union of the same function can't. A member with a lambda
/// that's still waiting in it is left, since a placeholder is assignable to anything.
fn absorb_members(ctx: &mut Compiler, mappings: &mut HashMap<TypeId, TypeId>) {
    for ty in mappings.values_mut() {
        let Type::Union(union) = ctx.ty(*ty).clone() else {
            continue;
        };

        if union
            .types
            .iter()
            .any(|&member| rue_types::contains_unresolved(ctx.types(), member))
        {
            continue;
        }

        let mut members: Vec<TypeId> = Vec::new();

        for &member in &union.types {
            if members
                .iter()
                .any(|&other| ctx.is_assignable(member, other))
            {
                continue;
            }

            members.retain(|&other| !ctx.is_assignable(other, member));
            members.push(member);
        }

        if members.len() == union.types.len() {
            continue;
        }

        *ty = if let [member] = members[..] {
            member
        } else {
            ctx.alloc_type(Type::Union(Union::new(members)))
        };
    }
}

/// An element lambda that's compiled once no argument still to be compiled can add to the generics
/// its untyped parameters are expected to have, as a lambda that's the whole argument waits for
/// them (#350, #351), rather than against the generics themselves, which its body can't do anything
/// with, as in `map_list([fn(x) => x + 1], 5)`. The pair or list it's in is compiled where it was,
/// with a placeholder for it, so its other elements are compiled against what they were.
#[derive(Debug, Clone)]
pub struct DeferredLambda {
    expr: AstLambdaExpr,
    lambda: ElementLambda,
    /// The placeholder's type and value.
    ty: TypeId,
    hir: HirId,
}

enum ArgumentKind {
    Lambda {
        /// Whether it holds up the arguments that read what it writes. A generic lambda doesn't,
        /// since what it gives the callee's generics comes through an instance of its own generics,
        /// which is bounded by the callee's other generics rather than by a type of its own, so
        /// waiting for it adds nothing to compile against.
        holds_up: bool,
        /// Whether the return types of the function types expected of it reference a generic it
        /// reads, so that compiling it can add to the generics its parameter types come from.
        returns_reads: bool,
    },
    Other {
        /// Whether it's a call with no arguments, which only its expected type can infer.
        needs_expected: bool,
    },
}

impl Argument {
    fn is_lambda(&self) -> bool {
        matches!(self.kind, ArgumentKind::Lambda { .. })
    }

    fn holds_up(&self) -> bool {
        !matches!(
            self.kind,
            ArgumentKind::Lambda {
                holds_up: false,
                ..
            }
        )
    }

    fn needs_expected(&self) -> bool {
        matches!(
            self.kind,
            ArgumentKind::Other {
                needs_expected: true
            }
        )
    }

    fn returns_reads(&self) -> bool {
        matches!(
            self.kind,
            ArgumentKind::Lambda {
                returns_reads: true,
                ..
            }
        )
    }
}

impl ArgumentOrder {
    fn new(ctx: &mut Compiler, function: &FunctionType, args: &[AstExpr], base: Inference) -> Self {
        let arguments: Vec<Argument> = args
            .iter()
            .zip(&function.params)
            .map(|(arg, param)| {
                let is_lambda = matches!(arg, AstExpr::LambdaExpr(_));
                let reads = reads_of(ctx, function, arg, param.ty);
                let returns_reads = is_lambda
                    && rue_types::extract_functions(ctx.types_mut(), param.ty, true)
                        .into_iter()
                        .any(|expected| {
                            generics_in(ctx, function, expected.ret)
                                .iter()
                                .any(|generic| reads.contains(generic))
                        });

                let kind = match arg {
                    AstExpr::LambdaExpr(lambda) => ArgumentKind::Lambda {
                        holds_up: lambda.generic_parameters().is_none(),
                        returns_reads,
                    },
                    AstExpr::FunctionCallExpr(_) => ArgumentKind::Other {
                        needs_expected: is_call_without_arguments(arg),
                    },
                    _ => ArgumentKind::Other {
                        needs_expected: false,
                    },
                };

                Argument {
                    kind,
                    has_lambda: has_lambda(arg),
                    has_own_lambda: has_own_lambda(arg),
                    widens: matches!(arg, AstExpr::FunctionCallExpr(_))
                        && !is_call_without_arguments(arg)
                        && !has_lambda(arg),
                    reads,
                    writes: generics_in(ctx, function, param.ty),
                    bounds_above: bounded_above(ctx, function, param.ty),
                    bare: if is_lambda {
                        bare_generics(ctx, function, param.ty)
                    } else {
                        IndexSet::new()
                    },
                    elements: element_lambdas(ctx, function, arg, param.ty),
                }
            })
            .collect();

        let mut pending: Vec<usize> = (0..arguments.len())
            .filter(|&i| !arguments[i].is_lambda())
            .collect();
        pending.extend((0..arguments.len()).filter(|&i| arguments[i].is_lambda()));

        Self {
            candidates: vec![None; arguments.len()],
            arguments,
            pending,
            compiled_lambdas: Vec::new(),
            compiled_others: Vec::new(),
            last_settled: false,
            merged: base.clone(),
            base,
        }
    }

    /// The generics that compiling the argument depends on now: its reads, and the generics that
    /// their mappings reference, which they're still waiting on, such as `A` when `B` is mapped to
    /// `(A, A)`. It's worked out once per step, when it's first needed.
    fn reads_now(&self, i: usize, state: &State<'_>) -> IndexSet<TypeId> {
        if let Some(reads) = state.reads.borrow().get(&i) {
            return reads.clone();
        }

        let mut reads = self.arguments[i].reads.clone();
        let mut index = 0;

        while let Some(&generic) = reads.get_index(index) {
            if let Some(others) = state.depends.get(&generic) {
                reads.extend(others);
            }

            index += 1;
        }

        state.reads.borrow_mut().insert(i, reads.clone());
        reads
    }

    /// Whether another argument that's still to be compiled can add to the generic, other than
    /// through an instance of its own generics. For an argument that isn't a lambda, a lambda only
    /// counts for the generics it bounds from above: one that only gives the generic a lower
    /// bound would make its expected type widen a literal that it infers from its own arguments.
    fn is_written(&self, i: usize, generic: TypeId) -> bool {
        self.pending.iter().any(|&other| {
            let argument = &self.arguments[other];

            let writes = if !self.arguments[i].is_lambda() && argument.is_lambda() {
                &argument.bounds_above
            } else {
                &argument.writes
            };

            other != i && argument.holds_up() && writes.contains(&generic)
        })
    }

    /// Whether no other argument that's still to be compiled can add to the generics that
    /// compiling this one depends on. A lambda waits even for a generic that's already mapped,
    /// since the parameter types it's compiled with are fixed and a later argument can add to them
    /// (#351). The exception is a generic that's its whole parameter type (or a member of it): it
    /// waits for that only while it's unmapped (#350), and is compiled against the mapping
    /// otherwise, as it was before, since another argument could replace a mapping that comes from
    /// the type expected of the call with one that only holds a value.
    fn is_ready(&self, i: usize, state: &State<'_>) -> bool {
        self.reads_now(i, state).iter().all(|&generic| {
            (state.mappings.contains_key(&generic) && self.arguments[i].bare.contains(&generic))
                || !self.is_written(i, generic)
        })
    }

    /// Whether compiling the argument now would change the expected type of an argument that
    /// isn't a lambda and comes before it among those still to be compiled, by adding to a generic
    /// that's already mapped in it. That argument would have been compiled first, against that
    /// mapping, which can come from the type expected of the call, so this one waits for it rather
    /// than widen a literal the expected type keeps.
    fn would_change_earlier(&self, i: usize, state: &State<'_>) -> bool {
        self.earlier_others(i).any(|other| {
            self.reads_now(other, state).iter().any(|generic| {
                state.mappings.contains_key(generic) && self.arguments[i].writes.contains(generic)
            })
        })
    }

    /// Whether the argument isn't a lambda, and would go ahead of an earlier argument that isn't
    /// one either, other than a call with no arguments, which only its expected type can infer
    /// (#352). Any other earlier argument infers its own generics, and going ahead of it can only
    /// widen a literal in its expected type.
    fn would_go_ahead_of_others(&self, i: usize) -> bool {
        !self.arguments[i].is_lambda()
            && self
                .earlier_others(i)
                .any(|other| !self.arguments[other].needs_expected())
    }

    /// The arguments that aren't lambdas and are before this one in `pending`.
    fn earlier_others(&self, i: usize) -> impl Iterator<Item = usize> + '_ {
        self.pending
            .iter()
            .copied()
            .take_while(move |&other| other != i)
            .filter(|&other| !self.arguments[other].is_lambda())
    }

    /// Whether the argument can be compiled now: it's ready, compiling it doesn't change an earlier
    /// argument's expected type, and it doesn't go ahead of an earlier argument it shouldn't.
    fn can_go(&self, i: usize, state: &State<'_>) -> bool {
        self.is_ready(i, state)
            && !self.would_change_earlier(i, state)
            && !self.would_go_ahead_of_others(i)
    }

    /// Whether the argument `next` just gave can be compiled against the expected type's bounds
    /// for the generics the arguments map (`CallSolver::argument_mappings`). An argument without a
    /// lambda in it always can. One whose lambdas are all inside calls in it (see `has_own_lambda`)
    /// can once no argument still to be compiled can add to the generics it reads, since the call
    /// can pass a lambda through, as `id(fn(x) => 1)` does.
    ///
    /// A lambda only can if no argument still to be compiled can add to the generics its
    /// parameter types come from, counting every one, unlike the order's readiness, and its own
    /// return type can't either. Otherwise a narrower parameter type could stop a later addition,
    /// since parameters are contravariant. A lambda inside another argument, such as a block or a
    /// pair, never takes them.
    fn takes_expected_bounds(&self, i: usize) -> bool {
        let argument = &self.arguments[i];

        if !argument.has_lambda {
            return true;
        }

        // A lambda inside a call in it is compiled against that call's own generics, but the call
        // can pass it through, as `id(fn(x) => 1)` does, so like a lambda, it only takes them once
        // nothing still to be compiled can add to the generics its expected type comes from
        if !argument.has_own_lambda {
            return self.last_settled;
        }

        argument.is_lambda() && self.last_settled && !argument.returns_reads()
    }

    /// Whether no argument still to be compiled can add to any of the generics. A generic lambda
    /// counts too: it doesn't hold up the order (see `ArgumentKind::Lambda::holds_up`), but it still
    /// adds to them, and an element lambda that waits is compiled against the expected type's
    /// bounds, which would be narrower than what it adds (see
    /// `lambda_before_a_generic_lambda_keeps_it_wide` in `generic_inference_expected_mapped.rue`).
    ///
    /// The generics that their mappings so far reference (`depends`, see `mapping_dependencies`)
    /// count too, as in `ArgumentOrder::reads_now`, such as `W` when `T` is mapped to `W | Int`.
    fn is_settled(
        &self,
        generics: &IndexSet<TypeId>,
        depends: &HashMap<TypeId, IndexSet<TypeId>>,
    ) -> bool {
        let generics = &with_dependencies(generics, depends);

        !self.pending.iter().any(|&other| {
            self.arguments[other]
                .writes
                .iter()
                .any(|generic| generics.contains(generic))
        })
    }

    /// The other callee generics that each generic's type depends on: those its mapping so far
    /// references (see `mapping_dependencies`), and those that a lambda that returns it reads,
    /// for the element lambdas that are still waiting, which aren't in any mapping yet. In a
    /// recursive call that goes for every element lambda of the arguments compiled so far, other
    /// than the one being compiled (`compiling`): one that's compiled where it is there is typed
    /// again as the arguments after it add to what it reads (see `refine_element_expected`), and
    /// what it returns is no longer a type that references the generic.
    fn dependencies(
        &self,
        ctx: &mut Compiler,
        function: &FunctionType,
        mappings: &HashMap<TypeId, TypeId>,
        waiting: &[(usize, DeferredLambda)],
        renamed: &HashMap<TypeId, TypeId>,
        compiling: Option<usize>,
    ) -> HashMap<TypeId, IndexSet<TypeId>> {
        let mut depends = mapping_dependencies(ctx, function, mappings);

        let compiled = self
            .arguments
            .iter()
            .enumerate()
            .filter(|(index, _)| {
                !renamed.is_empty() && !self.pending.contains(index) && compiling != Some(*index)
            })
            .flat_map(|(_, argument)| argument.elements.lambdas.values());

        for lambda in waiting
            .iter()
            .map(|(_, lambda)| &lambda.lambda)
            .chain(compiled)
        {
            for &generic in &lambda.returns {
                depends
                    .entry(generic)
                    .or_default()
                    .extend(lambda.reads.iter().copied().filter(|&read| read != generic));
            }
        }

        depends
    }

    /// The element lambdas of the argument `next` just gave that wait for the arguments still to be
    /// compiled (see `DeferredLambda`). One whose return type has a generic that an argument still
    /// to be compiled reads is compiled where it is, since that argument would be compiled before
    /// it, without what it adds to the generic.
    ///
    /// So is one whose return type can be what a call or type guard in its body makes of its
    /// parameters, while the type expected of the call bounds a generic in it (`expected`): compiled
    /// where it is, its parameters are the generics themselves, which the call passes through, so
    /// the expected type's bound narrows them too, as `5` does `T` in
    /// `let r: 5 = apply_first((fn(x) => identity(x), 1), 5)`, or `5 | nil` does in
    /// `fn(x) => if x is nil { nil } else { x }`. Compiled later, its parameters would be what the
    /// other arguments infer, `Int`, and the call or guard would give that. Which of the two a
    /// lambda needs depends on what its body does with its parameters, which isn't known before
    /// it's compiled, so it's compiled where it was.
    ///
    /// Where the type expected of the call is only a hint (`hinted`), as it is for a call in a
    /// lambda's body in another call's argument, any lambda whose return type it bounds is compiled
    /// where it is too. The call is only solved again with its lambdas' parameters, to narrow them
    /// to what the expected type allows, when the expected type is final (see
    /// `solve_with_lambda_parameters`), so compiled later, `x` in `fn(x) => [x, x]` would stay the
    /// `Int` that `5` widens to where a `List<5>` is expected (infer-fuzz generator 3, seed 408).
    ///
    /// Each of those rules gives way to one: a lambda whose body reads a field of an untyped
    /// parameter whose generic has no mapping yet, or applies an operator to it, can't be compiled
    /// with the generic as the parameter's type (see `ElementLambda::unusable_generics`). It
    /// always waits, unless an element before it gives the generic a type where it is. Then one
    /// that reads a field waits while something can still add to the generic, and one that only
    /// applies an operator follows the rules above.
    ///
    /// In a recursive call, a lambda that reads a generic that nothing has given a type counts as
    /// one that something can still add to (`recursive`), and `mappings` is what the arguments
    /// give before the caller's generics stand in for the generics they don't (see `restorable`).
    /// `waiting` is what the lambdas of earlier arguments that are still waiting read.
    fn deferred(
        &self,
        i: usize,
        expected: &IndexSet<TypeId>,
        hinted: bool,
        depends: &HashMap<TypeId, IndexSet<TypeId>>,
        waiting: &IndexSet<TypeId>,
        (mappings, recursive): (&HashMap<TypeId, TypeId>, bool),
    ) -> HashMap<TextRange, ElementLambda> {
        let elements = &self.arguments[i].elements;

        elements
            .lambdas
            .iter()
            .filter(|(_, lambda)| {
                // In a recursive call, a lambda that reads a generic that nothing has given a
                // type waits like one that a later argument can still add to, for the rest of its
                // argument at least: where it is, the generic is only one of the caller's that
                // stands for it (see `restorable`), and what the elements before it give it (see
                // `refine_element_expected`) is less than what the ones after it can. One whose
                // body has a guard or a `match` doesn't: it's written for a parameter that can
                // be more than the one type the arguments give it, which a guard on it wouldn't
                // overlap, as `x is nil` doesn't `(1, 2)`
                let settled = self.is_settled(&lambda.reads, depends);
                let unsettled = !settled
                    || recursive
                        && !lambda.guards
                        && lambda
                            .reads
                            .iter()
                            .any(|generic| !mappings.contains_key(generic));

                // Something that reads what its return type adds to would be compiled without it
                // if it waited: another element of its argument, an argument still to be
                // compiled, or a lambda of an earlier argument that's still waiting (`waiting`,
                // see `waiting_reads`)
                let return_read_later = lambda.read_beside
                    || lambda
                        .returns
                        .iter()
                        .any(|generic| waiting.contains(generic))
                    || self.pending.iter().any(|&other| {
                        self.arguments[other]
                            .reads
                            .iter()
                            .any(|generic| lambda.returns.contains(generic))
                    });

                // One whose body can't use a parameter as the generic it would be where it is
                // (see `ElementLambda::unusable_generics`) can't be compiled there, so it always
                // waits: for the rest of its argument at least, which can give the generic its
                // type, as `5` does in `(fn(x) => x + 1, 5)`
                let unmapped = |generic: &&TypeId| !mappings.contains_key(*generic);
                let given = if recursive {
                    &lambda.given_in_recursive
                } else {
                    &lambda.given_before
                };

                if lambda
                    .unusable_generics
                    .iter()
                    .filter(unmapped)
                    .any(|generic| !given.contains(generic))
                {
                    return true;
                }

                // Beside an element that can't be followed, what it gives the generics isn't known
                // where the lambda is, so in a call that isn't recursive, one that can be compiled
                // there is (see `ElementLambdas::unfollowed`)
                if elements.unfollowed && !recursive {
                    return false;
                }

                // Where an element before it gives the generic a type, it can be compiled there.
                // One that reads a field of the parameter still waits while an argument still to
                // be compiled can add to the generic, since what's added can be what has the
                // field, whatever reads what it returns
                if lambda
                    .field_generics
                    .iter()
                    .any(|generic| unmapped(&generic))
                {
                    return !settled || unsettled && !return_read_later;
                }

                // One whose return type adds to the generics its parameters read is compiled
                // against them where it is, so that it works for whatever it adds to them: compiled
                // against what the other arguments infer, its parameters would be narrower than
                // what it makes them, as in `fn(x) => (x, nil)` for `fn(a: T) -> T` with `T` from
                // `nil` (infer-fuzz generator 1, seeds 1461, 2955 and 6784)
                let adds_to_reads = lambda
                    .returns
                    .iter()
                    .any(|generic| lambda.reads.contains(generic));

                if adds_to_reads || lambda.read_beside {
                    return false;
                }

                let bounded_by_expected = lambda
                    .returns
                    .iter()
                    .any(|generic| expected.contains(generic));

                // The expected type's bound reaches the generics its parameters read through its
                // return type only once it's compiled, so it's compiled where it is if its body can
                // lose them, or if an argument still to be compiled reads them, as `identity(5)`
                // does in `map_list([fn(x) => x], identity(5))` with `List<5>` expected, or if the
                // expected type is only a hint (`hinted`)
                let read_later = self.pending.iter().any(|&other| {
                    self.arguments[other]
                        .reads
                        .iter()
                        .any(|generic| lambda.reads.contains(generic))
                });

                if bounded_by_expected && (lambda.narrows || read_later || hinted) {
                    return false;
                }

                unsettled && !return_read_later
            })
            .map(|(&range, lambda)| (range, lambda.clone()))
            .collect()
    }

    /// Whether the argument is a pair or list with element lambdas that could wait (see
    /// `element_lambdas`).
    fn has_element_lambdas(&self, i: usize) -> bool {
        !self.arguments[i].elements.lambdas.is_empty()
    }

    /// The callee's generics in the argument's parameter type (see `Argument::writes`).
    fn writes(&self, i: usize) -> &IndexSet<TypeId> {
        &self.arguments[i].writes
    }

    /// Whether the argument is compiled against the expected type's own bounds, where the mappings
    /// fit them, rather than what the arguments so far narrow the generics to (see
    /// `CallSolver::widen_to_expected`, which also leaves the generics an argument bounds from
    /// above as they are), such as `7 | 8` rather than `7` for `id(8)` in
    /// `let v: 7 | 8 = either(7, id(8));` (see `Argument::widens`).
    fn widens_to_expected(&self, i: usize) -> bool {
        self.arguments[i].widens
    }

    /// Whether every argument has been compiled.
    fn is_done(&self) -> bool {
        self.pending.is_empty()
    }

    /// The next argument to compile, given what the arguments compiled so far infer on their own
    /// (`inferred`) and what the next argument's expected type is substituted with (`mappings`).
    /// There must be one left (see `ArgumentOrder::is_done`).
    fn next(
        &mut self,
        ctx: &mut Compiler,
        function: &FunctionType,
        inferred: &HashMap<TypeId, TypeId>,
        mappings: &HashMap<TypeId, TypeId>,
        renamed: &HashMap<TypeId, TypeId>,
    ) -> usize {
        let depends: HashMap<TypeId, IndexSet<TypeId>> = mappings
            .iter()
            .map(|(&generic, &ty)| {
                let mut others = generics_in(ctx, function, ty);
                others.shift_remove(&generic);
                (generic, others)
            })
            .collect();

        let state = State {
            mappings,
            depends,
            reads: RefCell::new(HashMap::new()),
        };

        // A lambda's parameter types may only be inferred from a later lambda, so the first
        // one whose parameter types have been inferred goes first, as #265 ordered them
        let mut inferred_parameters = |i: usize| {
            !has_uninferred_parameters(ctx, function.params[i].ty, function, inferred, renamed)
        };

        // The pending arguments are the others and then the lambdas, so the first that can go is
        // a lambda only if none of the others can
        let can_go = |position: usize| self.can_go(self.pending[position], &state);

        let first = self.pending[0];
        let ready = (0..self.pending.len()).find(|&position| can_go(position));

        let position = match ready {
            Some(position) if !self.arguments[self.pending[position]].is_lambda() => position,
            Some(position) => (position..self.pending.len())
                .find(|&q| can_go(q) && inferred_parameters(self.pending[q]))
                .unwrap_or(position),
            None if !self.arguments[first].is_lambda() => 0,
            None => (0..self.pending.len())
                .find(|&q| inferred_parameters(self.pending[q]))
                .unwrap_or(0),
        };

        let i = self.pending.remove(position);

        // Following mappings (`reads_now`) rather than only the argument's own reads is a defensive
        // choice: no program has been found where the two differ
        self.last_settled = self.reads_now(i, &state).iter().all(|generic| {
            !self
                .pending
                .iter()
                .any(|&other| self.arguments[other].writes.contains(generic))
        });

        if self.arguments[i].is_lambda() {
            self.compiled_lambdas.push(i);
        } else {
            self.compiled_others.push(i);
        }

        i
    }

    /// The order the compiled arguments' candidates are put together in: the other arguments in
    /// order, then the lambdas in the order they were compiled. That's the order they were
    /// compiled in before arguments waited for each other, so that the order of a union's
    /// members, which shows in diagnostics, doesn't depend on which argument waited.
    fn candidate_order(&self) -> Vec<usize> {
        let mut others = self.compiled_others.clone();
        others.sort_unstable();
        others.extend(&self.compiled_lambdas);
        others
    }

    /// The candidates of the compiled arguments, put together in `candidate_order`.
    fn merged(&self) -> &Inference {
        &self.merged
    }

    /// Records the candidates of an argument that was compiled (the last one `next` gave). They're
    /// added to the end of the merged candidates when that's where `candidate_order` puts them,
    /// as it does unless an argument that isn't a lambda was compiled after one, or after a later
    /// argument. Otherwise they're all put together again.
    fn add_candidates(&mut self, i: usize, candidates: Inference) {
        let last = if self.arguments[i].is_lambda() {
            true
        } else {
            self.compiled_lambdas.is_empty() && self.compiled_others.iter().all(|&other| other <= i)
        };

        if last {
            self.merged.append(&candidates);
            self.candidates[i] = Some(candidates);
            return;
        }

        self.candidates[i] = Some(candidates);
        self.merged = self.merged_with(&[]);
    }

    /// Replaces the candidates of an argument that was compiled, once a lambda in it that waited
    /// was (see `DeferredLambda`).
    fn replace_candidates(&mut self, i: usize, candidates: Inference) {
        self.candidates[i] = Some(candidates);
        self.merged = self.merged_with(&[]);
    }

    /// The candidates of the compiled arguments put together in `candidate_order`, with those of
    /// the arguments that `replaced` has in place of theirs.
    fn merged_with(&self, replaced: &[Option<Inference>]) -> Inference {
        let mut merged = self.base.clone();

        for i in self.candidate_order() {
            let candidates = replaced.get(i).and_then(Option::as_ref);

            if let Some(candidates) = candidates.or(self.candidates[i].as_ref()) {
                merged.append(candidates);
            }
        }

        merged
    }
}

/// What deciding which argument goes next depends on.
struct State<'a> {
    /// The other callee generics that each mapping references.
    depends: HashMap<TypeId, IndexSet<TypeId>>,
    /// What the next argument's expected type is substituted with, before renamed generics are
    /// restored (see `restore_renamed`), so a renamed generic that isn't inferred yet counts as
    /// unmapped.
    mappings: &'a HashMap<TypeId, TypeId>,
    /// What compiling each argument that's been asked about depends on now (see
    /// `ArgumentOrder::reads_now`).
    reads: RefCell<HashMap<usize, IndexSet<TypeId>>>,
}

/// Whether the expression is, or has in it, a call or a lambda, whose types can depend on the type
/// it's expected to have.
fn has_call_or_lambda(expr: &AstExpr) -> bool {
    expr.syntax().descendants().any(|node| {
        AstFunctionCallExpr::cast(node.clone()).is_some() || AstLambdaExpr::cast(node).is_some()
    })
}

/// The callee's generics that are the type, or members of it if it's a union, looking through
/// aliases such as `type Same<T> = T` and `type Maybe<T> = T | nil`.
fn bare_generics(ctx: &mut Compiler, function: &FunctionType, ty: TypeId) -> IndexSet<TypeId> {
    let ty = rue_types::unwrap_semantic(ctx.types_mut(), ty, true);

    let members = match ctx.ty(ty) {
        Type::Union(union) => union.types.clone(),
        _ => vec![ty],
    };

    members
        .into_iter()
        .map(|member| rue_types::unwrap_semantic(ctx.types_mut(), member, true))
        .filter(|member| function.generics.contains(member))
        .collect()
}

/// The callee's generics in the parameter types of the function types the type has.
fn bounded_above(ctx: &mut Compiler, function: &FunctionType, ty: TypeId) -> IndexSet<TypeId> {
    let mut generics = IndexSet::new();

    for expected in rue_types::extract_functions(ctx.types_mut(), ty, false) {
        for parameter in expected.params {
            generics.extend(generics_in(ctx, function, parameter.ty));
        }
    }

    generics
}

/// The callee's generics that the type references.
fn generics_in(ctx: &mut Compiler, function: &FunctionType, ty: TypeId) -> IndexSet<TypeId> {
    contained_generics(ctx.types_mut(), ty, function.generics.iter().copied())
        .into_iter()
        .collect()
}

/// The callee's generics whose solution compiling the argument depends on, through the expected
/// type it's compiled against:
///
/// - For a lambda, the generics in the parameter types of the function types expected of it,
///   where its own parameters have no type. A parameter type that's a generic, or a union with one,
///   counts too, since that generic's solution is where the function type comes from.
/// - For a lambda without a return type whose body has a call or lambda in it, also the generics
///   in the expected return types, which the body is compiled against (#354).
/// - For a call, or a pair, list, group, block, `const` block or `if` with a lambda or call in it,
///   every generic in the parameter type, since the expected type can infer its generics. A
///   struct initializer is compiled against its own field types, not an expected type.
/// - Otherwise none, since the expected type doesn't change how it compiles.
fn reads_of(
    ctx: &mut Compiler,
    function: &FunctionType,
    arg: &AstExpr,
    param: TypeId,
) -> IndexSet<TypeId> {
    match arg {
        AstExpr::LambdaExpr(lambda) => {
            let untyped = untyped_parameters(lambda);

            let reads_return = lambda.ty().is_none()
                && lambda.body().is_some_and(|body| has_call_or_lambda(&body));

            if untyped.is_empty() && !reads_return {
                return IndexSet::new();
            }

            let mut reads = bare_generics(ctx, function, param);

            for expected in rue_types::extract_functions(ctx.types_mut(), param, true) {
                if reads_return {
                    reads.extend(generics_in(ctx, function, expected.ret));
                }

                for &index in &untyped {
                    if let Some(parameter) = expected.params.get(index) {
                        reads.extend(generics_in(ctx, function, parameter.ty));
                    }
                }
            }

            reads
        }
        AstExpr::FunctionCallExpr(_) => generics_in(ctx, function, param),
        AstExpr::PairExpr(_)
        | AstExpr::ListExpr(_)
        | AstExpr::GroupExpr(_)
        | AstExpr::Block(_)
        | AstExpr::ConstExpr(_)
        | AstExpr::IfExpr(_)
            if has_call_or_lambda(arg) =>
        {
            generics_in(ctx, function, param)
        }
        _ => IndexSet::new(),
    }
}

/// The lambdas among the elements of a pair or list argument, and of the pairs and lists it's made
/// of, that can wait for later arguments (see `DeferredLambda`), with the types they're expected to
/// have, found by following the parameter type along those pairs as compiling them does (see
/// `pair_parts`). Those are the ones whose untyped parameters are expected to have the callee's
/// generics, and whose return types have none that they or the other elements read, or whose
/// bodies read a field of, or apply an operator to, an untyped parameter that's expected to be one
/// of the generics (see `ElementLambda::unusable_generics`). Where the pairs can't be followed
/// into an element with a lambda in it, there are none.
fn element_lambdas(
    ctx: &mut Compiler,
    function: &FunctionType,
    arg: &AstExpr,
    param: TypeId,
) -> ElementLambdas {
    /// The elements found so far: each lambda that could wait, with the generics its return type
    /// references, and what each element reads, by where it is: for a lambda, the generics its
    /// untyped parameters read, and for anything else, every generic in its type
    #[derive(Default)]
    struct Walk {
        lambdas: Vec<(TextRange, ElementLambda)>,
        reads: Vec<(TextRange, IndexSet<TypeId>)>,
        /// The generics that the elements so far give a type where the next one is (see
        /// `ElementLambda::given_before`), and those that count in a recursive call.
        given: IndexSet<TypeId>,
        given_in_recursive: IndexSet<TypeId>,
        /// Whether every element was found, which it isn't where the pairs can't be followed. That's
        /// defensive: an element there is compiled without an expected type, or, past the end of a
        /// list's expected pairs, with the type the item before it was expected to have, whose
        /// generics that item already reads, so no program has been found where it matters.
        complete: bool,
        /// Whether a pair or list with no lambda in it couldn't be followed, as `(1, 2)` for `T`
        /// can't (see `ElementLambdas::unfollowed`).
        unfollowed: bool,
    }

    impl Walk {
        /// An element that isn't a lambda reads every generic in the type expected of it, and
        /// gives each of them a type for the elements after it.
        fn plain(&mut self, range: TextRange, generics: IndexSet<TypeId>) {
            self.given.extend(generics.iter().copied());
            self.given_in_recursive.extend(generics.iter().copied());
            self.reads.push((range, generics));
        }
    }

    fn add(
        ctx: &mut Compiler,
        function: &FunctionType,
        expr: &AstExpr,
        ty: TypeId,
        walk: &mut Walk,
    ) {
        let range = expr.syntax().text_range();

        match expr {
            AstExpr::LambdaExpr(lambda) => {
                let untyped = untyped_parameters(lambda);
                let expected = rue_types::extract_functions(ctx.types_mut(), ty, true);
                let mut reads = IndexSet::new();
                let mut returns = IndexSet::new();
                // Whether an untyped parameter is expected to have a type that has a generic in it
                // without being one, such as `(T, Int)`, a part of which doesn't reference `T`
                let mut projected = false;
                // The untyped parameters that are expected to be one of the callee's generics, with
                // the generic
                let mut generic_parameters: HashMap<usize, TypeId> = HashMap::new();

                for expected in &expected {
                    for &index in &untyped {
                        if let Some(parameter) = expected.params.get(index) {
                            let generics = generics_in(ctx, function, parameter.ty);
                            let semantic =
                                rue_types::unwrap_semantic(ctx.types_mut(), parameter.ty, true);
                            projected |=
                                !generics.is_empty() && !function.generics.contains(&semantic);
                            if function.generics.contains(&semantic) {
                                generic_parameters.insert(index, semantic);
                            }
                            reads.extend(generics);
                        }
                    }

                    returns.extend(generics_in(ctx, function, expected.ret));
                }

                let calls = lambda.ty().is_none()
                    && lambda.body().is_some_and(|body| has_call_or_lambda(&body));
                let narrows = lambda.ty().is_none()
                    && (calls
                        || projected
                        || lambda.body().is_some_and(|body| {
                            body.syntax()
                                .descendants()
                                .any(|node| AstGuardExpr::cast(node).is_some())
                        }));

                // One that can be compiled where it is gives what it returns a type for the
                // elements after it. One that can't may wait, so it isn't counted
                let (unusable_generics, field_generics) =
                    unusable_generics(lambda, &generic_parameters);
                let given_before = walk.given.clone();
                let given_in_recursive = walk.given_in_recursive.clone();

                if unusable_generics.is_empty() && field_generics.is_empty() {
                    walk.given.extend(returns.iter().copied());

                    if !reads.is_empty() {
                        walk.given_in_recursive.extend(returns.iter().copied());
                    }
                }

                // Whether another element reads what it returns is known once every element is
                // found, below
                if !reads.is_empty() {
                    let lambda = ElementLambda {
                        ty,
                        reads: reads.clone(),
                        returns: returns.clone(),
                        narrows,
                        unusable_generics,
                        field_generics,
                        given_before,
                        given_in_recursive,
                        guards: lambda.body().is_some_and(|body| has_guard_or_match(&body)),
                        read_beside: false,
                    };
                    walk.lambdas.push((range, lambda));
                }

                // As for a lambda that's the whole argument (see `reads_of`), a call in its body can
                // be inferred from the type its return type is expected to have, as `make()` is in
                // `fn() => make()` for `fn() -> U`
                let mut element_reads = reads;

                if calls {
                    element_reads.extend(returns);
                }

                walk.reads.push((range, element_reads));
            }
            AstExpr::PairExpr(pair) => {
                let Some((first, rest)) = pair_parts(ctx, ty) else {
                    if has_lambda(expr) {
                        walk.complete = false;
                    } else {
                        // A pair with no lambda in it is an element like any other, as `(1, 2)`
                        // is for `T`
                        walk.unfollowed = true;
                        walk.plain(range, generics_in(ctx, function, ty));
                    }

                    return;
                };

                for (expr, ty) in [(pair.first(), first), (pair.rest(), rest)] {
                    if let Some(expr) = expr {
                        add(ctx, function, &expr, ty, walk);
                    }
                }
            }
            AstExpr::ListExpr(list) => {
                let mut ty = ty;

                for item in list.items() {
                    let Some(expr) = item.expr() else {
                        continue;
                    };

                    // A spread item is the rest of the list
                    if item.spread().is_some() {
                        add(ctx, function, &expr, ty, walk);
                        return;
                    }

                    let Some((first, rest)) = pair_parts(ctx, ty) else {
                        let start = expr.syntax().text_range().start();

                        // The items from this one on
                        if list
                            .items()
                            .filter_map(|item| item.expr())
                            .filter(|expr| expr.syntax().text_range().start() >= start)
                            .any(|expr| has_lambda(&expr))
                        {
                            walk.complete = false;
                        } else {
                            walk.unfollowed = true;
                            walk.plain(range, generics_in(ctx, function, ty));
                        }

                        return;
                    };

                    add(ctx, function, &expr, first, walk);
                    ty = rest;
                }
            }
            _ => walk.plain(range, generics_in(ctx, function, ty)),
        }
    }

    if !matches!(arg, AstExpr::PairExpr(_) | AstExpr::ListExpr(_)) {
        return ElementLambdas::default();
    }

    let mut walk = Walk {
        complete: true,
        ..Walk::default()
    };
    add(ctx, function, arg, param, &mut walk);

    if !walk.complete {
        return ElementLambdas::default();
    }

    let reads = walk.reads;

    let lambdas = walk
        .lambdas
        .into_iter()
        .map(|(range, mut lambda)| {
            lambda.read_beside = reads.iter().any(|(other, reads)| {
                *other != range && lambda.returns.iter().any(|generic| reads.contains(generic))
            });

            (range, lambda)
        })
        .collect();

    ElementLambdas {
        lambdas,
        unfollowed: walk.unfollowed,
    }
}

/// The generics of the untyped parameters, by where they are (`generic_parameters`), that the
/// lambda's body can't use as a value of a generic, and those whose parameter it reads a field
/// of (see `unusable_parameters`).
fn unusable_generics(
    lambda: &AstLambdaExpr,
    generic_parameters: &HashMap<usize, TypeId>,
) -> (IndexSet<TypeId>, IndexSet<TypeId>) {
    let names: HashMap<String, TypeId> = lambda
        .parameters()
        .enumerate()
        .filter_map(|(index, parameter)| {
            let generic = *generic_parameters.get(&index)?;
            match parameter.binding()? {
                AstBinding::NamedBinding(binding) => binding
                    .name()
                    .map(|name| (name.text().to_string(), generic)),
                _ => None,
            }
        })
        .collect();

    if names.is_empty() {
        return (IndexSet::new(), IndexSet::new());
    }

    let (unusable, fields) = unusable_parameters(lambda, &names.keys().cloned().collect());
    let generics = |parameters: &HashSet<String>| -> IndexSet<TypeId> {
        parameters
            .iter()
            .filter_map(|name| names.get(name).copied())
            .collect()
    };

    (generics(&unusable), generics(&fields))
}

/// `List<T>` with the item type in place of `T`.
pub(crate) fn list_of(ctx: &mut Compiler, item: TypeId) -> TypeId {
    let types = ctx.builtins().types;
    let mappings = HashMap::from([(types.list_generic, item)]);
    substitute_with_mappings(ctx.types_mut(), types.list, &mappings)
}

/// The types of the first and rest of the pairs the type can be, each a union if there are
/// several, which a pair or list expected to be the type is compiled against.
pub(crate) fn pair_parts(ctx: &mut Compiler, ty: TypeId) -> Option<(TypeId, TypeId)> {
    let pairs = rue_types::extract_pairs(ctx.types_mut(), ty, false);

    match pairs.as_slice() {
        [] => None,
        [pair] => Some((pair.first, pair.rest)),
        pairs => {
            let first = Union::new(pairs.iter().map(|pair| pair.first).collect());
            let rest = Union::new(pairs.iter().map(|pair| pair.rest).collect());
            Some((
                ctx.alloc_type(Type::Union(first)),
                ctx.alloc_type(Type::Union(rest)),
            ))
        }
    }
}

/// The positions of the lambda's parameters that have no type.
fn untyped_parameters(lambda: &AstLambdaExpr) -> Vec<usize> {
    lambda
        .parameters()
        .enumerate()
        .filter(|(_, parameter)| parameter.ty().is_none())
        .map(|(index, _)| index)
        .collect()
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

/// The mappings as they are before `restore_renamed`, for `InferredCall::unrestored`, which only a
/// call with renamed generics needs.
fn unrestored_mappings(
    mappings: &HashMap<TypeId, TypeId>,
    renamed: &HashMap<TypeId, TypeId>,
) -> HashMap<TypeId, TypeId> {
    if renamed.is_empty() {
        HashMap::new()
    } else {
        mappings.clone()
    }
}

/// Maps the renamed generics that haven't been inferred back to the caller's generics they were
/// renamed from (see `rename_generics_in_scope`), including where the others are inferred as types
/// that reference them, since that's what they referred to before they were renamed. It returns the
/// generics it restored.
fn restore_renamed(
    ctx: &mut Compiler,
    mappings: &mut HashMap<TypeId, TypeId>,
    renamed: &HashMap<TypeId, TypeId>,
) -> HashMap<TypeId, TypeId> {
    let restored: HashMap<TypeId, TypeId> = renamed
        .iter()
        .filter(|(copy, _)| !mappings.contains_key(copy))
        .map(|(&copy, &generic)| (copy, generic))
        .collect();

    if restored.is_empty() {
        return restored;
    }

    for ty in mappings.values_mut() {
        if restored
            .keys()
            .any(|&copy| contains_generic(ctx.types_mut(), *ty, copy))
        {
            *ty = substitute_with_mappings(ctx.types_mut(), *ty, &restored);
        }
    }

    mappings.extend(restored.clone());
    restored
}

/// The caller's generics that `restore_renamed` puts in the argument's expected type in place of
/// the callee's renamed generics that aren't mapped, each with the generic it stands for, so that a
/// lambda in a pair or list in the argument can be refined as in any other call (see
/// `refine_element_expected`). A caller's generic that another mapping already references is left
/// out, since there it's the caller's generic itself, and the two couldn't be told apart.
fn restorable(
    ctx: &mut Compiler,
    mappings: &HashMap<TypeId, TypeId>,
    renamed: &HashMap<TypeId, TypeId>,
) -> HashMap<TypeId, TypeId> {
    renamed
        .iter()
        .filter(|&(copy, &generic)| {
            !mappings.contains_key(copy)
                && !mappings
                    .values()
                    .any(|&ty| contains_generic(ctx.types_mut(), ty, generic))
        })
        .map(|(&copy, &generic)| (generic, copy))
        .collect()
}

/// The type with the caller's generics in `restored` replaced by the callee's renamed generics they
/// stand for (see `restorable`).
fn unrestore(ctx: &mut Compiler, restored: &HashMap<TypeId, TypeId>, ty: TypeId) -> TypeId {
    if restored.is_empty() {
        return ty;
    }

    substitute_with_mappings(ctx.types_mut(), ty, restored)
}

/// The caller's generics in `restored` that are their own in the types of the lambda's untyped
/// parameters, in order, given what it was refined from (see `ArgumentFrame::element_refinement`).
/// What the elements before it infer for a renamed generic in the type expected of a parameter can
/// name the caller's generic as itself, as `(5, U)` from `fn(x6: (5, U)) => x6` does, so that `x7`
/// in a later `fn(x7) => x7` gets the caller's own `U`. In that parameter's type it isn't the
/// renamed generic it stands for in the rest of the argument, which the two couldn't be told apart
/// from.
fn own_generics_of(
    ctx: &mut Compiler,
    lambda: &AstLambdaExpr,
    (unrestored, mappings): (TypeId, &HashMap<TypeId, TypeId>),
    restored: &HashMap<TypeId, TypeId>,
    renamed: &HashMap<TypeId, TypeId>,
) -> Vec<Vec<TypeId>> {
    let functions = rue_types::extract_functions(ctx.types_mut(), unrestored, true);

    untyped_parameters(lambda)
        .into_iter()
        .map(|index| {
            let types: Vec<TypeId> = functions
                .iter()
                .filter_map(|function| function.params.get(index))
                .map(|parameter| parameter.ty)
                .collect();

            restored
                .keys()
                .copied()
                .filter(|&generic| {
                    renamed.keys().any(|&copy| {
                        mappings.get(&copy).is_some_and(|&ty| {
                            contains_generic(ctx.types_mut(), ty, generic)
                                && types
                                    .iter()
                                    .any(|&param| contains_generic(ctx.types_mut(), param, copy))
                        })
                    })
                })
                .collect()
        })
        .collect()
}

/// Compiles an element of a pair or list. The caller's generics in the expected type of the
/// argument being compiled only stand for the callee's renamed generics (see `restorable`) in the
/// pairs and lists the argument is made of, so anything else, such as a lambda's body or a block, is
/// compiled without them: a generic written in an annotation there is the caller's own.
pub(crate) fn compile_element(
    ctx: &mut Compiler,
    expr: &AstExpr,
    expected: Option<TypeId>,
) -> Value {
    // What this element was refined with, if it was (see `refine_element_expected`), which is
    // taken here whatever the element is, so that none is left for the next one
    let refinement = ctx
        .argument_frame_mut()
        .and_then(|argument| argument.element_refinement.take());

    if matches!(expr, AstExpr::PairExpr(_) | AstExpr::ListExpr(_)) {
        return compile_expr(ctx, expr, expected);
    }

    // Only this element's own lambda is wrapped, not those in its body or in a block
    let (restored, wrap) = ctx
        .argument_frame_mut()
        .map(|argument| {
            (
                std::mem::take(&mut argument.restored),
                std::mem::take(&mut argument.wrap_lambdas),
            )
        })
        .unwrap_or_default();

    let deferred = ctx
        .argument_frame_mut()
        .filter(|_| wrap)
        .and_then(|argument| argument.to_defer.remove(&expr.syntax().text_range()));

    let value = if let Some(element) = deferred
        && let AstExpr::LambdaExpr(lambda) = expr
    {
        // A placeholder that's replaced once the lambda is compiled (see `DeferredLambda`)
        let ty = ctx.alloc_type(Type::Unresolved);
        let hir = ctx.alloc_hir(Hir::Unresolved);

        if let Some(argument) = ctx.argument_frame_mut() {
            argument.deferred.push(DeferredLambda {
                expr: lambda.clone(),
                lambda: element,
                ty,
                hir,
            });
        }

        Value::new(hir, ty)
    } else if wrap && let AstExpr::LambdaExpr(lambda) = expr {
        let (value, parameters) =
            // What it's declared to be before the generics are substituted isn't known here, so a
            // parameter that feeds itself isn't looked for (see `compile_lambda_feeding_itself`)
            compile_lambda_argument(ctx, expr, lambda, expected, None, &ParameterWrap::Whole);

        let renamed = ctx
            .argument_frame_mut()
            .map(|argument| argument.renamed.clone())
            .unwrap_or_default();
        let own_generics = refinement.as_ref().map_or_else(Vec::new, |refinement| {
            own_generics_of(
                ctx,
                lambda,
                (refinement.0, &refinement.1),
                &restored,
                &renamed,
            )
        });

        if let Some(argument) = ctx.argument_frame_mut() {
            argument
                .own_generics
                .extend(own_by_parameter(&parameters, &own_generics));
            argument.lambda_parameters.extend(parameters);
        }

        value
    } else {
        compile_expr(ctx, expr, expected)
    };

    if let Some(argument) = ctx.argument_frame_mut() {
        argument.restored = restored;
        argument.wrap_lambdas = wrap;
    }

    value
}

/// The caller's generics in the type expected of a lambda that's an element of a pair or list in
/// the argument, which are mapped back to the callee's renamed generics they stand for (see
/// `refine_element_expected`): those that are, after `unwrap_semantic`, the whole type of one of its
/// untyped parameters in every function type expected of it. That's the type the parameter gets,
/// and a lambda with the caller's generic there only compiles if the callee's generic is inferred
/// as the caller's, so what the elements before it infer can't change the type of a program that
/// compiles. Anywhere else it can, so the caller's generic is kept:
///
/// - `make()` in `recursive((1, fn() => make()))` with `recursive<T>(pair: (T, fn() -> T))` would
///   be `Int` rather than `Int | T`.
/// - With `Callback<T> | Callback<Int>` expected, the parameter is `T | Int`, which compiles with
///   the callee's generic inferred as `Int`.
fn restored_parameters(
    ctx: &mut Compiler,
    restored: &HashMap<TypeId, TypeId>,
    expected: TypeId,
    lambda: &AstLambdaExpr,
) -> HashMap<TypeId, TypeId> {
    if restored.is_empty() {
        return HashMap::new();
    }

    let functions = rue_types::extract_functions(ctx.types_mut(), expected, true);
    let mut parameters = HashMap::new();

    for index in untyped_parameters(lambda) {
        let mut types = functions.iter().map(|function| {
            function
                .params
                .get(index)
                .map(|parameter| rue_types::unwrap_semantic(ctx.types_mut(), parameter.ty, true))
        });

        let Some(Some(ty)) = types.next() else {
            continue;
        };

        if let Some(&copy) = restored.get(&ty)
            && types.all(|other| other == Some(ty))
        {
            parameters.insert(ty, copy);
        }
    }

    parameters
}

/// Records what an element of a pair or list that's part of an argument being compiled infers for
/// the call's generics, given the type it was expected to have, so that a later element can be
/// compiled against it (see `refine_element_expected`).
///
/// In a recursive call, an element that isn't assignable to its expected type, which has the
/// caller's generics in place of the callee's renamed generics that aren't inferred (see
/// `restorable`), is recorded with them mapped back to the callee's. Then a later lambda whose
/// parameter type is one of them (see `restored_parameters`) can't compile with the caller's
/// generic, which it only does if the callee's generic is inferred as the caller's, so it can get
/// what the element infers instead. An element that is assignable, such as `1` expected to be
/// `T | Int`, records nothing for them, so the lambda gets the caller's generic, as it would
/// without being refined. What it infers is also recorded in `own_infer`, from its type with its
/// lambdas' parameters' stand-ins as the renamed generics (see `unrestore_lambdas`), so that a
/// later lambda can tell which of the caller's generics are its own (see `own_generics_of`).
pub(crate) fn record_element(ctx: &mut Compiler, ty: TypeId, expected: Option<TypeId>) {
    let Some(expected) = expected else {
        return;
    };

    let Some(mut argument) = ctx.take_argument_frame() else {
        return;
    };

    let expected = if argument.restored.is_empty() || ctx.is_assignable(ty, expected) {
        expected
    } else {
        unrestore(ctx, &argument.restored, expected)
    };

    if references_call_generics(ctx, expected) {
        ctx.infer_type(ty, expected, &mut argument.infer);

        if !argument.restored.is_empty() {
            let own = unrestore_lambdas(
                ctx,
                ty,
                &argument.lambda_parameters,
                (&argument.restored, &argument.own_generics),
            );
            ctx.infer_type(own, expected, &mut argument.own_infer);
        }
    }

    ctx.set_argument_frame(Some(argument));
}

/// The type of an element of a pair or list with the caller's generics that its lambdas' untyped
/// parameters were compiled as in place of the callee's renamed ones (see `restorable`) as those,
/// in the parameters' references and in the types of the lambdas whose bodies see through them, as
/// `generic_parameter_types` has them.
fn unrestore_lambdas(
    ctx: &mut Compiler,
    ty: TypeId,
    parameters: &[LambdaParameter],
    (restored, own_generics): (&HashMap<TypeId, TypeId>, &OwnGenerics),
) -> TypeId {
    let call_generics: Vec<TypeId> = ctx
        .call_generics()
        .map(<[TypeId]>::to_vec)
        .unwrap_or_default();
    let mut references = HashMap::new();
    // The lambdas with a parameter whose type has the callee's generics in it in place of the
    // caller's, which are the only ones `generic_parameter_types` looks into
    let mut lambdas: Vec<LambdaBody> = Vec::new();

    for parameter in parameters {
        let restored = restored_for_parameter(restored, own_generics, parameter.reference);
        let unrestored = unrestore(ctx, &restored, parameter.ty);

        if unrestored != parameter.ty {
            references.insert(parameter.reference, unrestored);

            if references_generics(ctx, unrestored, &call_generics)
                && !lambdas.contains(&parameter.lambda)
            {
                lambdas.push(parameter.lambda);
            }
        }
    }

    let mut mappings = references.clone();
    let originals: Vec<TypeId> = restored.keys().copied().collect();
    let generics: Vec<TypeId> = call_generics.iter().chain(&originals).copied().collect();

    for lambda in lambdas {
        if lambda.captures_generics(ctx, &generics) {
            continue;
        }

        if let LambdaRestored::SeenThrough(restored) =
            lambda_restored(ctx, lambda, parameters, (restored, own_generics))
        {
            let mut within = references.clone();
            within.extend(restored);
            let unrestored = substitute_with_mappings(ctx.types_mut(), lambda.ty, &within);
            mappings.insert(lambda.ty, unrestored);
        }
    }

    if mappings.is_empty() {
        return ty;
    }

    substitute_with_mappings(ctx.types_mut(), ty, &mappings)
}

/// The type an element of a pair or list that's part of an argument being compiled is expected to
/// have, with the call's generics that it still references replaced by what's inferred so far,
/// from the arguments before it and the elements before this one. A lambda in such an element
/// would otherwise get the generic itself as a parameter type, as in `apply((1, fn(x) => x))`
/// (#350), and a call with no arguments, such as `make()` in `first((5, make()))`, couldn't be
/// inferred. Only those two kinds of element, and pairs and lists with a call with no arguments in
/// them (as a fallback, below), are compiled against this. Any other element is compiled as it was,
/// since a call in it that infers its own generics from its arguments could infer them from the
/// refined type instead, such as `opt(nil)` with `opt<T>(x: T | nil)`.
///
/// In a recursive call, the callee's generics are renamed, and the argument's expected type has
/// those that aren't inferred yet restored to the caller's generics (see `restore_renamed`). They're
/// mapped back to the callee's here where they're a lambda element's parameter types (see
/// `restored_parameters`), and the ones that are still not inferred, or are inferred as a type
/// that's assignable to the caller's generic, are restored again. So the lambda gets the caller's
/// generic unless an element before it can't be assigned to its expected type with the caller's
/// generic (see `record_element`), and what it infers can't be either. What the elements before it
/// infer can name the caller's generic as itself, and then that's the caller's own in the lambda,
/// not the renamed one it stands for elsewhere in the argument (see `ArgumentFrame::own_generics`).
///
/// A pair or list with a call without arguments in it, such as `(3, make())` in
/// `first(((1, 2), (3, make())))`, is compiled against it too, but only as a fallback: a call with
/// arguments in it only uses it for the generics its arguments don't give a type, so `opt(nil)` in
/// `(opt(nil), make())` is as it was. The second value is whether it's only a fallback.
pub(crate) fn refine_element_expected(
    ctx: &mut Compiler,
    expected: TypeId,
    element: &AstExpr,
) -> (TypeId, bool) {
    let direct = has_lambda(element) || is_call_without_arguments(element);
    let fallback = !direct && contains_call_without_arguments(element);

    if !direct && !fallback {
        return (expected, false);
    }

    let Some(mut argument) = ctx.take_argument_frame() else {
        return (expected, false);
    };

    let refinable = match element {
        AstExpr::LambdaExpr(lambda) => {
            restored_parameters(ctx, &argument.restored, expected, lambda)
        }
        _ => HashMap::new(),
    };
    let unrestored = unrestore(ctx, &refinable, expected);

    if !references_call_generics(ctx, unrestored) {
        ctx.set_argument_frame(Some(argument));
        return (expected, false);
    }

    let inferred = ctx.solve_arguments(&argument.solver, &argument.infer);
    // Without the expected type's bounds, as for a lambda argument that something can still add
    // to: a later element can. This is defensive, since the bounds would only matter for a
    // generic an earlier element maps, and no program has been found where it differs
    let mut mappings = ctx.argument_mappings(&argument.solver, &argument.infer, &inferred, false);

    // An element can fail to be assigned for a reason that has nothing to do with the caller's
    // generic, so what it infers can still be assignable to that, such as `Same<T>`, which main
    // compiles with the callee's generic inferred as the caller's
    for (&generic, copy) in &refinable {
        if let Some(&ty) = mappings.get(copy)
            && ctx.is_assignable(ty, generic)
        {
            mappings.remove(copy);
        }
    }

    // Which of the caller's generics are its own in the lambda's parameters' types comes from
    // what's inferred without them standing for the renamed ones, for the renamed generics that
    // this refines (see `ArgumentFrame::own_infer`)
    if !argument.restored.is_empty() {
        let inferred = ctx.solve_arguments(&argument.solver, &argument.own_infer);
        let own = ctx
            .argument_mappings(&argument.solver, &argument.own_infer, &inferred, false)
            .into_iter()
            .filter(|(copy, _)| mappings.contains_key(copy))
            .collect();
        argument.element_refinement = Some((unrestored, own));
    }

    restore_renamed(ctx, &mut mappings, &argument.renamed);
    ctx.set_argument_frame(Some(argument));

    (
        substitute_with_mappings(ctx.types_mut(), unrestored, &mappings),
        fallback,
    )
}

/// Compiles an element of a pair or list after the first, against its refined expected type (see
/// `refine_element_expected`), as only a fallback where that's all it is, and as only a hint if
/// `hint` is set (see `Compiler::with_expected_type_hint`). It returns the element's value and the
/// type it was expected to have.
pub(crate) fn compile_refined_element(
    ctx: &mut Compiler,
    expr: &AstExpr,
    expected: Option<TypeId>,
    hint: bool,
) -> (Value, Option<TypeId>) {
    let (expected, fallback) = expected
        .map(|expected| refine_element_expected(ctx, expected, expr))
        .unzip();
    let fallback = fallback.unwrap_or(false) || ctx.is_expected_type_fallback();

    let value = ctx.with_expected_type_fallback(fallback, |ctx| {
        ctx.with_expected_type_hint(hint, |ctx| compile_element(ctx, expr, expected))
    });

    (value, expected)
}

/// Whether the expression is a call with no arguments, or a pair or list with one in it.
fn contains_call_without_arguments(expr: &AstExpr) -> bool {
    match expr {
        AstExpr::PairExpr(pair) => pair
            .first()
            .into_iter()
            .chain(pair.rest())
            .any(|expr| contains_call_without_arguments(&expr)),
        AstExpr::ListExpr(list) => list
            .items()
            .filter_map(|item| item.expr())
            .any(|expr| contains_call_without_arguments(&expr)),
        _ => is_call_without_arguments(expr),
    }
}

/// Whether the type references a generic of the innermost call being inferred.
pub(crate) fn references_call_generics(ctx: &mut Compiler, ty: TypeId) -> bool {
    let generics = ctx
        .call_generics()
        .map(<[TypeId]>::to_vec)
        .unwrap_or_default();

    generics
        .into_iter()
        .any(|generic| contains_generic(ctx.types_mut(), ty, generic))
}

/// Whether the expression is a call with no arguments, whose generics, if it has any to infer,
/// only its expected type can infer.
fn is_call_without_arguments(expr: &AstExpr) -> bool {
    matches!(expr, AstExpr::FunctionCallExpr(call) if call.args().next().is_none())
}

/// Whether the expression is, or has in it, a lambda that isn't inside a call in it, in the call's
/// arguments or its callee. A lambda in a call's arguments, such as in
/// `map_reversed(5, fn(x) => 7)`, is compiled against that call's own generics, which the call's
/// expected type only bounds through its return type, so the call takes the expected bounds much
/// as a call without a lambda does, once nothing later can add to them (see
/// `ArgumentOrder::takes_expected_bounds`). A lambda that's the callee, as in `(fn() => 7)()`, has
/// its parameter types written out, so the expected bounds can't narrow them either.
fn has_own_lambda(expr: &AstExpr) -> bool {
    let mut stack = vec![expr.syntax().clone()];

    while let Some(node) = stack.pop() {
        if AstLambdaExpr::cast(node.clone()).is_some() {
            return true;
        }

        if AstFunctionCallExpr::cast(node.clone()).is_none() {
            stack.extend(node.children());
        }
    }

    false
}

/// Whether the expression is, or has in it, a lambda.
fn has_lambda(expr: &AstExpr) -> bool {
    expr.syntax()
        .descendants()
        .any(|node| AstLambdaExpr::cast(node).is_some())
}
