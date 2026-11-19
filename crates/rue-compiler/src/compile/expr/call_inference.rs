//! Inferring a call's generics while its arguments are compiled: the order the arguments are
//! compiled in, and what each one is compiled against. How the generics are solved is up to
//! `CallSolver` in `rue-types`.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use indexmap::IndexSet;

use log::debug;
use rue_ast::{AstExpr, AstFunctionCallExpr, AstLambdaExpr, AstNode};
use rue_hir::{HirId, Value};
use rue_types::{
    CallSolver, FunctionType, Generic, Inference, InstancePolicy, Type, TypeId, contained_generics,
    contains_generic, substitute_with_mappings,
};

use crate::{
    ArgumentFrame, Compiler, LambdaBody, LambdaParameter, ParameterWrap, compile_expr,
    compile_lambda_argument, occurs_contravariantly,
};

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
    let mut base = Inference::new(function.generics.iter().copied());

    for (&copy, &generic) in renamed {
        base.rename(copy, generic);
    }

    // The type expected of the call is used for the generics that the arguments don't
    // constrain, such as `T` in `let value: Int = unchecked_cast(nil);`, and to keep the
    // ones they do from being widened past it
    let expected = expected_type.map(|expected_type| {
        let mut expected = base.clone();
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

    // The instances of generic functions passed as arguments are solved with the callee's generics
    base.set_instance_policy(InstancePolicy::Joint);

    // A call with arguments in an element that's only refined for a call without arguments in it
    // only uses the type expected of it for the generics its arguments don't give a type (see
    // `refine_element_expected`), so it isn't solved again for it either
    let fallback_only = ctx.is_expected_type_fallback() && !args.is_empty();
    // The type expected of the call, if it's a type of its own that the call has to be assignable
    // to. One that's only a hint doesn't count (see `Compiler::with_expected_type_hint`), such as one
    // that comes from what an enclosing call has inferred so far, which it can still widen
    let final_expected = expected_type.filter(|_| !ctx.is_expected_type_hint() && !fallback_only);

    let solver = ctx.call_solver(expected);
    let solver = Rc::new(if fallback_only {
        solver.fallback_only()
    } else {
        solver
    });

    let mut results = vec![ctx.builtins().unresolved.hir; function.params.len()];

    // Each argument's check, by parameter, which are made in the order the arguments come in (see
    // `ArgumentOrder::candidate_order`)
    let mut checks: Vec<Option<(AstExpr, TypeId, TypeId)>> = vec![None; function.params.len()];

    // The untyped parameters of each lambda argument, as the references they were compiled as and
    // the types those refer to (see `compile_lambda`)
    let mut lambda_parameters: Vec<Vec<LambdaParameter>> = vec![Vec::new(); function.params.len()];

    // For each of those parameters, the type with the callee's generics in it that its lambda is
    // checked with them as what they're inferred as, if it is (see `generic_parameter_types`)
    let mut generic_types: Vec<Vec<Option<TypeId>>> = vec![Vec::new(); function.params.len()];

    // Whether the callee's generics are an enclosing call's, which is calling the same function
    let enclosing = ctx.inferred_generics();
    let shared = function
        .generics
        .iter()
        .any(|generic| enclosing.contains(generic));

    let mut order = ArgumentOrder::new(ctx, function, args, base.clone());

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
        restore_renamed(ctx, &mut mappings, renamed);
        let restored_here = restored.clone();

        let expected = substitute_with_mappings(ctx.types_mut(), param, &mappings);

        ctx.set_argument_frame(Some(ArgumentFrame {
            infer: order.merged().clone(),
            solver: solver.clone(),
            renamed: if restored.is_empty() {
                HashMap::new()
            } else {
                renamed.clone()
            },
            restored,
            wrap_lambdas: matches!(expr, AstExpr::PairExpr(_) | AstExpr::ListExpr(_)),
            lambda_parameters: Vec::new(),
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

        let (value, mut parameters) = ctx.with_expected_type_fallback(false, |ctx| {
            ctx.with_expected_type_hint(hint, |ctx| {
                if let AstExpr::LambdaExpr(lambda) = expr {
                    let wrap = match &params {
                        Some(params) => ParameterWrap::Generics {
                            params,
                            generics: &function.generics,
                            mappings: &mappings,
                        },
                        None => ParameterWrap::Whole,
                    };

                    compile_lambda_argument(ctx, expr, lambda, Some(expected), &wrap)
                } else {
                    (compile_expr(ctx, expr, Some(expected)), Vec::new())
                }
            })
        });

        // The lambdas among the elements of a pair or list argument (see `compile_element`)
        if let Some(argument) = ctx.argument_frame_mut() {
            parameters.append(&mut argument.lambda_parameters);
        }

        ctx.set_argument_frame(None);
        results[i] = value.hir;

        generic_types[i] =
            generic_parameter_types(ctx, function, shared, &parameters, &restored_here);

        lambda_parameters[i] = parameters;

        // Candidates are collected against the unsubstituted parameter type, so that every
        // argument contributes to the generics rather than only the first one to mention them
        let mut infer = base.clone();
        ctx.infer_type(value.ty, param, &mut infer);
        order.add_candidates(i, infer);
        checks[i] = Some((expr.clone(), value.ty, param));
    }

    ctx.pop_call_frame();

    if args.len() < function.params.len() {
        debug!("Unresolved function call argument");
    }

    let mut mappings = ctx.solve_call(&solver, order.merged().clone());

    // Only a call that the arguments don't solve otherwise is solved again with lambdas' untyped
    // parameters as vars, so that nothing that compiled before changes, and only for a final
    // expected type. The checks are made with the renamed generics restored, as the call is checked
    let expected_type = final_expected;

    restore_renamed(ctx, &mut mappings, renamed);

    if lambda_parameters
        .iter()
        .any(|parameters| !parameters.is_empty())
    {
        let mut instantiated = checks.clone();
        instantiate_generic_parameters(
            ctx,
            function,
            &lambda_parameters,
            &generic_types,
            &mappings,
            &mut instantiated,
        );

        if solves_call(ctx, function, &instantiated, &mappings, expected_type) {
            checks = instantiated;
        } else {
            let mut alternative = checks.clone();
            let mut solved = solve_with_lambda_parameters(
                ctx,
                function,
                &solver,
                &order,
                &lambda_parameters,
                &generic_types,
                &mut alternative,
            );

            // What the vars are inferred as can reference the renamed generics too
            let restored = restore_renamed(ctx, &mut solved, renamed);

            for (_, ty, _) in alternative.iter_mut().flatten() {
                *ty = substitute_with_mappings(ctx.types_mut(), *ty, &restored);
            }

            // It's only used if it solves the call, so a call that still fails reports what it did,
            // and if it doesn't infer a generic as a type that references the callee's generics,
            // which a call nested in an argument of a call to the same function shares with it.
            // This is a safety net: no program has been found that reaches it
            if solves_call(ctx, function, &alternative, &solved, expected_type)
                && !solved
                    .values()
                    .any(|&ty| references_generics(ctx, ty, &function.generics))
            {
                mappings = solved;
                checks = alternative;
            } else {
                checks = instantiated;
            }
        }
    }

    let checks = order
        .candidate_order()
        .into_iter()
        .filter_map(|i| checks[i].take())
        .collect();

    InferredCall {
        results,
        checks,
        mappings,
    }
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

/// Checks each lambda whose untyped parameters were compiled as the callee's generics that hadn't
/// been inferred yet, such as `x: A` in `map_list([fn(x) => x], 5)`, with them as what they're
/// inferred as (see `generic_parameter_types` for which ones are). Its body was compiled with the
/// generic as a type it knows nothing about, so it works for whatever the generic is.
fn instantiate_generic_parameters(
    ctx: &mut Compiler,
    function: &FunctionType,
    lambda_parameters: &[Vec<LambdaParameter>],
    generic_types: &[Vec<Option<TypeId>>],
    mappings: &HashMap<TypeId, TypeId>,
    checks: &mut [Option<(AstExpr, TypeId, TypeId)>],
) {
    for ((check, parameters), generic_types) in
        checks.iter_mut().zip(lambda_parameters).zip(generic_types)
    {
        let Some((_, ty, _)) = check else {
            continue;
        };

        let instantiated = instantiations(ctx, function, parameters, generic_types, mappings);

        if !instantiated.is_empty() {
            *ty = substitute_with_mappings(ctx.types_mut(), *ty, &instantiated);
        }
    }
}

/// What each lambda parameter with a type with the callee's generics in it is instantiated as with
/// the mappings, keyed by its reference. A solution that still references the generics doesn't say
/// what they are, such as in a call nested in an argument of a call to the same function, which
/// shares them, so that parameter is left out.
fn instantiations(
    ctx: &mut Compiler,
    function: &FunctionType,
    parameters: &[LambdaParameter],
    generic_types: &[Option<TypeId>],
    mappings: &HashMap<TypeId, TypeId>,
) -> HashMap<TypeId, TypeId> {
    let mut instantiated = HashMap::new();

    for (parameter, &generic_type) in parameters.iter().zip(generic_types) {
        let Some(generic_type) = generic_type else {
            continue;
        };

        let solution = substitute_with_mappings(ctx.types_mut(), generic_type, mappings);

        if !references_generics(ctx, solution, &function.generics) {
            instantiated.insert(parameter.reference, solution);
        }
    }

    instantiated
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
) -> Vec<Option<TypeId>> {
    let originals: Vec<TypeId> = restored.keys().copied().collect();
    let generics: Vec<TypeId> = function
        .generics
        .iter()
        .copied()
        .chain(originals.iter().copied())
        .collect();

    // Whether each lambda can be checked that way, once per lambda
    let mut lambdas: Vec<(LambdaBody, bool)> = Vec::new();

    parameters
        .iter()
        .map(|parameter| {
            let ty = unrestore(ctx, restored, parameter.ty);

            if !references_generics(ctx, ty, &function.generics) {
                return None;
            }

            let is_restored = references_generics(ctx, parameter.ty, &originals);

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
                    && !names_restored_generics(ctx, lambda, parameters, restored, &originals);
                lambdas.push((lambda, instantiable));
                instantiable
            };

            instantiable.then_some(ty)
        })
        .collect()
}

/// Whether the lambda's type has the caller's generics in `restored` in it other than through its
/// parameters, which it can have if its body names them, as in `fn(x) => x as T`. Then its body
/// has values of them that don't come from its parameters, so it doesn't work for any type in
/// their place.
fn names_restored_generics(
    ctx: &mut Compiler,
    lambda: LambdaBody,
    parameters: &[LambdaParameter],
    restored: &HashMap<TypeId, TypeId>,
    originals: &[TypeId],
) -> bool {
    if restored.is_empty() {
        return false;
    }

    let own: HashMap<TypeId, TypeId> = parameters
        .iter()
        .filter(|parameter| parameter.lambda == lambda)
        .map(|parameter| {
            let ty = unrestore(ctx, restored, parameter.ty);
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
    checks: &[Option<(AstExpr, TypeId, TypeId)>],
    mappings: &HashMap<TypeId, TypeId>,
    expected_type: Option<TypeId>,
) -> bool {
    checks.iter().flatten().all(|&(_, ty, param)| {
        let param = substitute_with_mappings(ctx.types_mut(), param, mappings);
        ctx.is_assignable(ty, param)
    }) && expected_type.is_none_or(|expected_type| {
        let ret = substitute_with_mappings(ctx.types_mut(), function.ret, mappings);
        ctx.is_assignable(ret, expected_type)
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
    lambda_parameters: &[Vec<LambdaParameter>],
    generic_types: &[Vec<Option<TypeId>>],
    checks: &mut [Option<(AstExpr, TypeId, TypeId)>],
) -> HashMap<TypeId, TypeId> {
    // Each lambda's parameters, as their references, the types they refer to and their vars
    let mut lambdas = Vec::new();
    let mut replaced = vec![None; lambda_parameters.len()];

    for (i, parameters) in lambda_parameters.iter().enumerate() {
        if parameters.is_empty() {
            continue;
        }

        let (_, ty, param) = checks[i].as_ref().expect("a lambda argument is checked");
        let (ty, param) = (*ty, *param);
        let mut infer = order.base.clone();
        let mut references = HashMap::new();
        let mut vars = Vec::new();

        for (parameter, generic_type) in parameters.iter().zip(&generic_types[i]) {
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
    for (i, mut inferred) in inferred_lambdas {
        inferred.extend(instantiations(
            ctx,
            function,
            &lambda_parameters[i],
            &generic_types[i],
            &mappings,
        ));

        if let Some((_, ty, _)) = &mut checks[i] {
            *ty = substitute_with_mappings(ctx.types_mut(), *ty, &inferred);
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
/// Among the arguments that can go, it's today's order (#265): the other arguments in order, then
/// the lambdas, the first whose parameter types have been inferred first. When none can, because
/// they depend on each other, it's today's order too.
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
                    reads,
                    writes: generics_in(ctx, function, param.ty),
                    bounds_above: bounded_above(ctx, function, param.ty),
                    bare: if is_lambda {
                        bare_generics(ctx, function, param.ty)
                    } else {
                        IndexSet::new()
                    },
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

    /// Whether compiling the argument now would change the expected type of an argument before it
    /// in today's order that isn't a lambda, by adding to a generic that's already mapped in it.
    /// That argument would have been compiled first, against that mapping, which can come from
    /// the type expected of the call, so this one waits for it rather than widen a literal the
    /// expected type keeps.
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
    /// can pass a lambda through, as `id(fn(x) => 1)` does. A lambda only can if no argument still to be compiled can add to
    /// the generics its parameter types come from, counting every one, unlike the order's
    /// readiness, and its own return type can't either. Otherwise a narrower parameter type could
    /// stop a later addition, since parameters are contravariant. A lambda inside another
    /// argument, such as a block or a pair, never takes them.
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

    /// The callee's generics in the argument's parameter type (see `Argument::writes`).
    fn writes(&self, i: usize) -> &IndexSet<TypeId> {
        &self.arguments[i].writes
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

/// Compiles an element of a pair or list. The caller's generics in the expected type of the
/// argument being compiled only stand for the callee's renamed generics (see `restorable`) in the
/// pairs and lists the argument is made of, so anything else, such as a lambda's body or a block, is
/// compiled without them: a generic written in an annotation there is the caller's own.
pub(crate) fn compile_element(
    ctx: &mut Compiler,
    expr: &AstExpr,
    expected: Option<TypeId>,
) -> Value {
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

    let value = if wrap && let AstExpr::LambdaExpr(lambda) = expr {
        let (value, parameters) =
            compile_lambda_argument(ctx, expr, lambda, expected, &ParameterWrap::Whole);

        if let Some(argument) = ctx.argument_frame_mut() {
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
/// without being refined.
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
    }

    ctx.set_argument_frame(Some(argument));
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
/// generic (see `record_element`), and what it infers can't be either.
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

    let Some(argument) = ctx.take_argument_frame() else {
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
    // to: a later element can. This is defensive, since the bounds would only matter for a generic
    // an earlier element maps, and no program has been found where it differs
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
/// arguments or its callee. A lambda in a call's arguments, such as in `map_reversed(5, fn(x) => 7)`,
/// is compiled against that call's own generics, which the call's expected type only bounds
/// through its return type, so the call takes the expected bounds much as a call without a lambda
/// does, once nothing later can add to them (see `ArgumentOrder::takes_expected_bounds`). A lambda
/// that's the callee, as in `(fn() => 7)()`, has its parameter types written out, so the
/// expected bounds can't narrow them either.
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
