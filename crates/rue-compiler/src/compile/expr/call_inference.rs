//! Inferring a call's generics while its arguments are compiled: the order the arguments are
//! compiled in, and what each one is compiled against. How the generics are solved is up to
//! `CallSolver` in `rue-types`.

use std::collections::HashMap;

use indexmap::IndexSet;

use log::debug;
use rue_ast::{AstExpr, AstFunctionCallExpr, AstLambdaExpr, AstNode};
use rue_hir::HirId;
use rue_types::{
    FunctionType, Inference, Type, TypeId, contains_generic, substitute_with_mappings,
};

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

    let solver = ctx.call_solver(expected);

    let mut results = vec![ctx.builtins().unresolved.hir; function.params.len()];

    // Each argument's candidates and check, by parameter, which are put together in the order
    // the arguments come in (see `ArgumentOrder::candidate_order`)
    let mut candidates: Vec<Option<Inference>> = vec![None; function.params.len()];
    let mut checks: Vec<Option<(AstExpr, TypeId, TypeId)>> = vec![None; function.params.len()];

    let mut order = ArgumentOrder::new(
        ctx,
        function,
        &args[..function.params.len().min(args.len())],
    );

    ctx.push_inferred_generics(function.generics.clone());

    loop {
        let infer = order.merge(&base, &candidates);
        let inferred = ctx.solve_arguments(&solver, &infer);

        // Use what has been inferred from the previous arguments as the expected type, along
        // with what the type expected of the call implies for the generics they don't map
        let mut mappings = ctx.argument_mappings(&solver, &infer, inferred.clone());

        let Some(i) = order.next(ctx, function, &inferred, &mappings, renamed) else {
            break;
        };

        let expr = &args[i];
        let param = function.params[i].ty;

        restore_renamed(ctx, &mut mappings, renamed);

        let expected = substitute_with_mappings(ctx.types_mut(), param, &mappings);

        let value = compile_expr(ctx, expr, Some(expected));
        results[i] = value.hir;

        // Candidates are collected against the unsubstituted parameter type, so that every
        // argument contributes to the generics rather than only the first one to mention them
        let mut infer = base.clone();
        ctx.infer_type(value.ty, param, &mut infer);
        candidates[i] = Some(infer);
        checks[i] = Some((expr.clone(), value.ty, param));
    }

    ctx.pop_inferred_generics();

    if args.len() < function.params.len() {
        debug!("Unresolved function call argument");
    }

    let infer = order.merge(&base, &candidates);
    let mut mappings = ctx.solve_call(&solver, infer);
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
}

/// What `ArgumentOrder` knows about an argument.
struct Argument {
    is_lambda: bool,
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
    /// Whether it's a call with no arguments, which only its expected type can infer.
    needs_expected: bool,
    /// Whether it holds up the arguments that read what it writes. A generic lambda doesn't, since
    /// what it gives the callee's generics comes through an instance of its own generics, which is
    /// bounded by the callee's other generics rather than by a type of its own, so waiting for it
    /// adds nothing to compile against.
    holds_up: bool,
}

impl ArgumentOrder {
    fn new(ctx: &mut Compiler, function: &FunctionType, args: &[AstExpr]) -> Self {
        let arguments: Vec<Argument> = args
            .iter()
            .zip(&function.params)
            .map(|(arg, param)| {
                let is_lambda = matches!(arg, AstExpr::LambdaExpr(_));

                Argument {
                    is_lambda,
                    reads: reads_of(ctx, function, arg, param.ty),
                    writes: generics_in(ctx, function, param.ty),
                    bounds_above: bounded_above(ctx, function, param.ty),
                    bare: if is_lambda {
                        bare_generics(ctx, function, param.ty)
                    } else {
                        IndexSet::new()
                    },
                    needs_expected: matches!(
                        arg,
                        AstExpr::FunctionCallExpr(call) if call.args().next().is_none()
                    ),
                    holds_up: !matches!(
                        arg,
                        AstExpr::LambdaExpr(lambda) if lambda.generic_parameters().is_some()
                    ),
                }
            })
            .collect();

        let mut pending: Vec<usize> = (0..arguments.len())
            .filter(|&i| !arguments[i].is_lambda)
            .collect();
        pending.extend((0..arguments.len()).filter(|&i| arguments[i].is_lambda));

        Self {
            arguments,
            pending,
            compiled_lambdas: Vec::new(),
            compiled_others: Vec::new(),
        }
    }

    /// The generics that compiling the argument depends on now: its reads, and the generics that
    /// their mappings reference, which they're still waiting on, such as `A` when `B` is mapped to
    /// `(A, A)`.
    fn reads_now(&self, i: usize, depends: &HashMap<TypeId, IndexSet<TypeId>>) -> IndexSet<TypeId> {
        let mut reads = self.arguments[i].reads.clone();
        let mut index = 0;

        while let Some(&generic) = reads.get_index(index) {
            if let Some(others) = depends.get(&generic) {
                reads.extend(others);
            }

            index += 1;
        }

        reads
    }

    /// Whether another argument that's still to be compiled can add to the generic, other than
    /// through an instance of its own generics. For an argument that isn't a lambda, a lambda only
    /// counts for the generics it bounds from above: one that only gives the generic a lower
    /// bound would make its expected type widen a literal that it infers from its own arguments.
    fn is_written(&self, i: usize, generic: TypeId) -> bool {
        self.pending.iter().any(|&other| {
            let argument = &self.arguments[other];

            let writes = if !self.arguments[i].is_lambda && argument.is_lambda {
                &argument.bounds_above
            } else {
                &argument.writes
            };

            other != i && argument.holds_up && writes.contains(&generic)
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
        state.reads[i].iter().all(|&generic| {
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
            state.reads[other].iter().any(|generic| {
                state.mappings.contains_key(generic) && self.arguments[i].writes.contains(generic)
            })
        })
    }

    /// Whether the argument isn't a lambda, and would go ahead of an earlier argument that isn't
    /// one either, other than a call with no arguments, which only its expected type can infer
    /// (#352). Any other earlier argument infers its own generics, and going ahead of it can only
    /// widen a literal in its expected type.
    fn would_go_ahead_of_others(&self, i: usize) -> bool {
        !self.arguments[i].is_lambda
            && self
                .earlier_others(i)
                .any(|other| !self.arguments[other].needs_expected)
    }

    /// The arguments that aren't lambdas and are before this one in `pending`.
    fn earlier_others(&self, i: usize) -> impl Iterator<Item = usize> + '_ {
        self.pending
            .iter()
            .copied()
            .take_while(move |&other| other != i)
            .filter(|&other| !self.arguments[other].is_lambda)
    }

    /// Whether the argument can be compiled now: it's ready, compiling it doesn't change an earlier
    /// argument's expected type, and it doesn't go ahead of an earlier argument it shouldn't.
    fn can_go(&self, i: usize, state: &State<'_>) -> bool {
        self.is_ready(i, state)
            && !self.would_change_earlier(i, state)
            && !self.would_go_ahead_of_others(i)
    }

    /// The next argument to compile, given what the arguments compiled so far infer on their own
    /// (`inferred`) and what the next argument's expected type is substituted with (`mappings`), or
    /// `None` once they've all been compiled.
    fn next(
        &mut self,
        ctx: &mut Compiler,
        function: &FunctionType,
        inferred: &HashMap<TypeId, TypeId>,
        mappings: &HashMap<TypeId, TypeId>,
        renamed: &HashMap<TypeId, TypeId>,
    ) -> Option<usize> {
        if self.pending.is_empty() {
            return None;
        }

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
            reads: (0..self.arguments.len())
                .map(|i| self.reads_now(i, &depends))
                .collect(),
        };

        // A lambda's parameter types may only be inferred from a later lambda, so the first
        // one whose parameter types have been inferred goes first, as #265 ordered them
        let mut inferred_parameters = |i: usize| {
            !has_uninferred_parameters(ctx, function.params[i].ty, function, inferred, renamed)
        };

        // The pending arguments are the others and then the lambdas, so the first that can go is
        // a lambda only if none of the others can
        let can_go: Vec<bool> = self
            .pending
            .iter()
            .map(|&i| self.can_go(i, &state))
            .collect();

        let first = self.pending[0];
        let position = match can_go.iter().position(|&can_go| can_go) {
            Some(position) if !self.arguments[self.pending[position]].is_lambda => position,
            Some(position) => (position..self.pending.len())
                .find(|&q| can_go[q] && inferred_parameters(self.pending[q]))
                .unwrap_or(position),
            None if !self.arguments[first].is_lambda => 0,
            None => (0..self.pending.len())
                .find(|&q| inferred_parameters(self.pending[q]))
                .unwrap_or(0),
        };

        let i = self.pending.remove(position);

        if self.arguments[i].is_lambda {
            self.compiled_lambdas.push(i);
        } else {
            self.compiled_others.push(i);
        }

        Some(i)
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
    fn merge(&self, base: &Inference, candidates: &[Option<Inference>]) -> Inference {
        let mut infer = base.clone();

        for i in self.candidate_order() {
            if let Some(candidates) = &candidates[i] {
                infer.append(candidates);
            }
        }

        infer
    }
}

/// What deciding which argument goes next depends on.
struct State<'a> {
    /// What the next argument's expected type is substituted with.
    mappings: &'a HashMap<TypeId, TypeId>,
    /// What compiling each argument depends on now (see `ArgumentOrder::reads_now`).
    reads: Vec<IndexSet<TypeId>>,
}

/// Whether the expression is, or has in it, a call or a lambda, whose types can depend on the type
/// it's expected to have.
fn has_call_or_lambda(expr: &AstExpr) -> bool {
    expr.syntax().descendants().any(|node| {
        AstFunctionCallExpr::cast(node.clone()).is_some() || AstLambdaExpr::cast(node).is_some()
    })
}

/// The callee's generics that are the type, or members of it if it's a union.
fn bare_generics(ctx: &Compiler, function: &FunctionType, ty: TypeId) -> IndexSet<TypeId> {
    let members = match ctx.ty(ty) {
        Type::Union(union) => union.types.clone(),
        _ => vec![ty],
    };

    members
        .into_iter()
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
            let untyped: Vec<usize> = lambda
                .parameters()
                .enumerate()
                .filter(|(_, parameter)| parameter.ty().is_none())
                .map(|(index, _)| index)
                .collect();

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
