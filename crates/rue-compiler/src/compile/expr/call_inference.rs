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
    CallSolver, FunctionType, Generic, Inference, InstancePolicy, Type, TypeId, contains_generic,
    substitute_with_mappings,
};

use crate::{ArgumentFrame, Compiler, compile_expr};

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

    let solver = Rc::new(ctx.call_solver(expected));

    let mut results = vec![ctx.builtins().unresolved.hir; function.params.len()];

    // Each argument's check, by parameter, which are made in the order the arguments come in (see
    // `ArgumentOrder::candidate_order`)
    let mut checks: Vec<Option<(AstExpr, TypeId, TypeId)>> = vec![None; function.params.len()];

    // For each lambda argument with untyped parameters, the candidates it gives with them as vars
    // (see `solve_with_lambda_parameters`)
    let mut with_parameters: Vec<Option<(Vec<LambdaParameter>, Inference)>> =
        vec![None; function.params.len()];

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

        // An argument with a lambda in it only takes the expected type's bounds for the generics
        // the arguments map when nothing can add to them afterward (see
        // `CallSolver::argument_mappings`)
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
        }));

        if matches!(expr, AstExpr::LambdaExpr(_)) {
            ctx.wrap_lambda_parameters();
        }

        let value = compile_expr(ctx, expr, Some(expected));
        ctx.set_argument_frame(None);
        let parameter_refs = wrapped_lambda_parameters(ctx);
        results[i] = value.hir;

        // Candidates are collected against the unsubstituted parameter type, so that every
        // argument contributes to the generics rather than only the first one to mention them
        let mut infer = base.clone();
        ctx.infer_type(value.ty, param, &mut infer);
        order.add_candidates(i, infer);
        checks[i] = Some((expr.clone(), value.ty, param));

        if !parameter_refs.is_empty() {
            let mut infer = base.clone();
            let ty = lambda_type_with_vars(ctx, value.ty, &parameter_refs, &mut infer);
            ctx.infer_type(ty, param, &mut infer);
            with_parameters[i] = Some((parameter_refs, infer));
        }
    }

    ctx.pop_call_frame();

    if args.len() < function.params.len() {
        debug!("Unresolved function call argument");
    }

    let mut mappings = ctx.solve_call(&solver, order.merged().clone());

    // Only a call that the arguments don't solve otherwise is solved again with lambdas' untyped
    // parameters as vars, so that nothing that compiled before changes. The type expected of a call
    // that's part of an enclosing call's argument comes from what that call has inferred so far,
    // which it can still widen, so it only counts outside of one. In a recursive call, the renamed
    // generics would have to be restored in the vars too, so it's left as is
    let expected_type = expected_type.filter(|_| ctx.call_generics().is_none());

    if renamed.is_empty()
        && with_parameters.iter().any(Option::is_some)
        && !solves_call(ctx, function, &checks, &mappings, expected_type)
    {
        let (solved, solved_checks) =
            solve_with_lambda_parameters(ctx, &solver, &order, &with_parameters, &checks);

        if solves_arguments(ctx, &solved_checks, &solved) {
            mappings = solved;
            checks = solved_checks;
        }
    }

    restore_renamed(ctx, &mut mappings, renamed);

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

/// An untyped parameter of a lambda argument (see `wrapped_lambda_parameters`).
#[derive(Debug, Clone, Copy)]
struct LambdaParameter {
    /// The reference to `ty` that it was compiled as.
    reference: TypeId,
    /// The type its body was compiled against.
    ty: TypeId,
    /// The var it's inferred as along with the callee's generics.
    var: TypeId,
}

/// The untyped parameters of the lambda argument that was just compiled, if it was one, which it
/// compiled as references (see `Compiler::wrap_lambda_parameters`), each with a var to infer it as.
/// That lets a bound on the lambda's return type reach a parameter's type: in
/// `let result: 5 = map(fn(x) => x, 5)`, `x` was compiled as `Int`, the widened `5`, but `B ≤ 5`
/// makes it `5` (#335).
fn wrapped_lambda_parameters(ctx: &mut Compiler) -> Vec<LambdaParameter> {
    ctx.take_lambda_parameters()
        .into_iter()
        .map(|(reference, ty)| LambdaParameter {
            reference,
            ty,
            var: ctx.alloc_type(Type::Generic(Generic { name: None })),
        })
        .collect()
}

/// The type of a lambda argument with its untyped parameters as the vars they're inferred as, each
/// of which is added to the inference as at most the type its body was compiled against.
fn lambda_type_with_vars(
    ctx: &mut Compiler,
    ty: TypeId,
    parameters: &[LambdaParameter],
    infer: &mut Inference,
) -> TypeId {
    if parameters.is_empty() {
        return ty;
    }

    let mut vars = HashMap::new();

    for parameter in parameters {
        infer.add_var(parameter.var, parameter.ty);
        vars.insert(parameter.reference, parameter.var);
    }

    substitute_with_mappings(ctx.types_mut(), ty, &vars)
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
    solves_arguments(ctx, checks, mappings)
        && expected_type.is_none_or(|expected_type| {
            let ret = substitute_with_mappings(ctx.types_mut(), function.ret, mappings);
            ctx.is_assignable(ret, expected_type)
        })
}

/// Whether each argument is assignable to its parameter with the mappings.
fn solves_arguments(
    ctx: &mut Compiler,
    checks: &[Option<(AstExpr, TypeId, TypeId)>],
    mappings: &HashMap<TypeId, TypeId>,
) -> bool {
    checks.iter().flatten().all(|&(_, ty, param)| {
        let param = substitute_with_mappings(ctx.types_mut(), param, mappings);
        ctx.is_assignable(ty, param)
    })
}

/// Solves the call again with the untyped parameters of its lambda arguments as vars (see
/// `wrapped_lambda_parameters`), and gives the mappings and the checks with each lambda's type as
/// it's inferred.
fn solve_with_lambda_parameters(
    ctx: &mut Compiler,
    solver: &CallSolver,
    order: &ArgumentOrder,
    with_parameters: &[Option<(Vec<LambdaParameter>, Inference)>],
    checks: &[Option<(AstExpr, TypeId, TypeId)>],
) -> (
    HashMap<TypeId, TypeId>,
    Vec<Option<(AstExpr, TypeId, TypeId)>>,
) {
    let mut merged = order.base.clone();

    for i in order.candidate_order() {
        if let Some((_, candidates)) = &with_parameters[i] {
            merged.append(candidates);
        } else if let Some(candidates) = &order.candidates[i] {
            merged.append(candidates);
        }
    }

    let mut mappings = ctx.solve_call(solver, merged);
    let mut checks = checks.to_vec();
    let mut vars = HashMap::new();

    for (check, parameters) in checks.iter_mut().zip(with_parameters) {
        let (Some((_, ty, _)), Some((parameters, _))) = (check, parameters) else {
            continue;
        };

        let mut inferred = HashMap::new();

        for parameter in parameters {
            // A parameter that isn't inferred as narrower than what its body was compiled against
            // is that type, as it was
            let narrower = mappings
                .remove(&parameter.var)
                .filter(|&mapped| ctx.is_assignable(mapped, parameter.ty))
                .unwrap_or(parameter.ty);

            inferred.insert(parameter.reference, narrower);
            vars.insert(parameter.var, narrower);
        }

        *ty = substitute_with_mappings(ctx.types_mut(), *ty, &inferred);
    }

    // The callee's generics can be inferred through the vars
    for ty in mappings.values_mut() {
        *ty = substitute_with_mappings(ctx.types_mut(), *ty, &vars);
    }

    (mappings, checks)
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
    /// lambda in it always can. A lambda only can if no argument still to be compiled can add to
    /// the generics its parameter types come from, counting every one, unlike the order's
    /// readiness, and its own return type can't either. Otherwise a narrower parameter type could
    /// stop a later addition, since parameters are contravariant. A lambda inside another
    /// argument, such as a block or a pair, never takes them.
    fn takes_expected_bounds(&self, i: usize) -> bool {
        let argument = &self.arguments[i];

        if !argument.has_lambda {
            return true;
        }

        argument.is_lambda() && self.last_settled && !argument.returns_reads()
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
        self.merged = self.base.clone();

        for other in self.candidate_order() {
            if let Some(candidates) = &self.candidates[other] {
                self.merged.append(candidates);
            }
        }
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
    function
        .generics
        .iter()
        .copied()
        .filter(|&generic| contains_generic(ctx.types_mut(), ty, generic))
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

    let restored = ctx
        .argument_frame_mut()
        .map(|argument| std::mem::take(&mut argument.restored))
        .unwrap_or_default();

    let value = compile_expr(ctx, expr, expected);

    if let Some(argument) = ctx.argument_frame_mut() {
        argument.restored = restored;
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
/// inferred. Only those two kinds of element are compiled against this. Any other element is
/// compiled as it was, since a call in it that infers its own generics from its arguments could
/// infer them from the refined type instead, such as `opt(nil)` with `opt<T>(x: T | nil)`.
///
/// In a recursive call, the callee's generics are renamed, and the argument's expected type has
/// those that aren't inferred yet restored to the caller's generics (see `restore_renamed`). They're
/// mapped back to the callee's here where they're a lambda element's parameter types (see
/// `restored_parameters`), and the ones that are still not inferred, or are inferred as a type
/// that's assignable to the caller's generic, are restored again. So the lambda gets the caller's
/// generic unless an element before it can't be assigned to its expected type with the caller's
/// generic (see `record_element`), and what it infers can't be either.
pub(crate) fn refine_element_expected(
    ctx: &mut Compiler,
    expected: TypeId,
    element: &AstExpr,
) -> TypeId {
    if !(has_lambda(element) || is_call_without_arguments(element)) {
        return expected;
    }

    let Some(argument) = ctx.take_argument_frame() else {
        return expected;
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
        return expected;
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

    substitute_with_mappings(ctx.types_mut(), unrestored, &mappings)
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

/// Whether the expression is, or has in it, a lambda.
fn has_lambda(expr: &AstExpr) -> bool {
    expr.syntax()
        .descendants()
        .any(|node| AstLambdaExpr::cast(node).is_some())
}
