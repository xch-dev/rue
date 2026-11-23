use std::collections::HashMap;

use log::debug;
use rue_ast::{AstExpr, AstLambdaExpr, AstNode};
use rue_diagnostic::{DiagnosticKind, SpreadTarget, SrcLoc};
use rue_hir::{FunctionKind, FunctionSymbol, Hir, HirId, ParameterSymbol, Symbol, Value};
use rue_types::{
    FunctionParameter, FunctionType, Generic, Type, TypeId, Union, contained_generics,
    contains_generic, substitute_with_mappings,
};

use crate::{
    BlockKind, Compiler, CompletionContext, ReturnTarget, SyntaxItemKind, compile_block,
    compile_expr, compile_generic_parameters, compile_type, create_binding, list_of, pair_parts,
};

pub fn compile_lambda_expr(
    ctx: &mut Compiler,
    expr: &AstLambdaExpr,
    expected_type: Option<TypeId>,
) -> Value {
    compile_lambda(ctx, expr, expected_type, None).0
}

/// How a lambda that's an argument of a call whose generics are being inferred compiles its untyped
/// parameters, so that the call can infer their types along with its generics (#335). A reference
/// is the same type to the body as the type it refers to, and where the parameter flows into the
/// lambda's return type unchanged, the return type has the reference.
pub(crate) enum ParameterWrap<'a> {
    /// Each untyped parameter is compiled as a reference of its own to the type it would have.
    Whole,
    /// The lambda is expected to be exactly the function type with these parameter types, before
    /// the callee's generics in them are substituted with what they're inferred as so far
    /// (`mappings`, or the generic itself if it isn't mapped). Each of those generics that only
    /// occurs where values flow into the lambda (see `occurs_contravariantly`) is a reference of its
    /// own in them instead, so a part of a parameter, such as `p.first` with `p: (A, Int)`, still has
    /// it. The others are substituted with what they're inferred as so far, as they would be.
    Generics {
        params: &'a [TypeId],
        generics: &'a [TypeId],
        mappings: &'a HashMap<TypeId, TypeId>,
    },
}

/// An untyped parameter of a lambda that's an argument of a call whose generics are being inferred,
/// as the reference it was compiled as (see `ParameterWrap`) and the type that refers to.
#[derive(Debug, Clone, Copy)]
pub struct LambdaParameter {
    pub reference: TypeId,
    pub ty: TypeId,
    pub lambda: LambdaBody,
}

/// The nodes and symbols that were allocated while a lambda was compiled, which are its own: its
/// parameters, its body, and the bindings and lambdas in it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LambdaBody {
    first_hir: HirId,
    end_hir: HirId,
    first_symbol: usize,
    first_named_type: usize,
    end_named_type: usize,
    /// The lambda's type.
    pub(crate) ty: TypeId,
}

impl LambdaBody {
    /// Whether the lambda's body references a value from outside of it whose type has any of the
    /// generics in it, such as a parameter of the function or lambda it's in. Otherwise, the only
    /// values of the generics the body has are the ones its parameters give it, and what it does
    /// with them is the same whatever the generics are, so it works for any type in their place.
    /// Functions and constants are declared outside of any function, so their types can't have
    /// these generics, other than a function's own generics, which a reference to it renames. A
    /// value's declared type is what counts, since narrowing it can't add a generic to it.
    ///
    /// The body's nodes are the ones allocated while the lambda was compiled, rather than the ones
    /// a walk of its body reaches. Any others among them, such as ones compiled and then dropped,
    /// can only make this find more captures, so it errs on the side of not instantiating.
    pub(crate) fn captures_generics(&self, ctx: &mut Compiler, generics: &[TypeId]) -> bool {
        let captured: Vec<TypeId> = ctx
            .hirs_between(self.first_hir, self.end_hir)
            .filter_map(|node| match *node {
                Hir::Reference(symbol) if symbol.index() < self.first_symbol => {
                    match ctx.symbol(symbol) {
                        Symbol::Parameter(parameter) => Some(parameter.ty),
                        Symbol::Binding(binding) => Some(binding.value.ty),
                        _ => None,
                    }
                }
                _ => None,
            })
            .collect();

        captured
            .into_iter()
            .any(|ty| !contained_generics(ctx.types_mut(), ty, generics.iter().copied()).is_empty())
    }

    /// Whether the lambda's body references its parameter that was compiled as the reference (see
    /// `LambdaParameter`), directly or through a binding destructured from it.
    pub(crate) fn references_parameter(&self, ctx: &Compiler, reference: TypeId) -> bool {
        ctx.hirs_between(self.first_hir, self.end_hir)
            .any(|node| match *node {
                Hir::Reference(symbol) if symbol.index() >= self.first_symbol => matches!(
                    ctx.symbol(symbol),
                    Symbol::Parameter(parameter) if parameter.ty == reference
                ),
                _ => false,
            })
    }

    /// Whether the lambda names any of the types, in an annotation in its parameters, its return
    /// type or its body, such as `T` in `fn(x) => x as T`. Otherwise, a type it has them in only
    /// has them through its parameters or a value it captures. As with `captures_generics`, the
    /// types named while it was compiled are what count, so this errs on the side of finding them.
    pub(crate) fn names_types(&self, ctx: &mut Compiler, types: &[TypeId]) -> bool {
        let named = ctx
            .named_types_between(self.first_named_type, self.end_named_type)
            .to_vec();

        named
            .into_iter()
            .any(|ty| !contained_generics(ctx.types_mut(), ty, types.iter().copied()).is_empty())
    }
}

/// Keeps an untyped parameter of a lambda for each generic in its type that's of a call the
/// lambda is an argument of (see `Compiler::push_uninferred_parameter`).
fn register_uninferred_parameter(
    ctx: &mut Compiler,
    inferred_generics: &[TypeId],
    name: &str,
    ty: TypeId,
) {
    for generic in contained_generics(ctx.types_mut(), ty, inferred_generics.iter().copied()) {
        ctx.push_uninferred_parameter(generic, name);
    }
}

/// How many parameters the function types expected of a lambda have, if there are any and they
/// all have as many. A function whose last parameter is the rest of its arguments has no number
/// of them that a lambda can be said to have too many or too few for.
pub(crate) fn expected_parameter_count(expected_functions: &[FunctionType]) -> Option<usize> {
    let (first, rest) = expected_functions.split_first()?;

    if expected_functions
        .iter()
        .any(|function| !function.nil_terminated)
    {
        return None;
    }

    rest.iter()
        .all(|function| function.params.len() == first.params.len())
        .then_some(first.params.len())
}

/// Where a lambda is reported for having another number of parameters than expected: its first
/// parameter that's one too many, or all of it if it has too few.
pub(crate) fn parameter_count_site(
    ctx: &Compiler,
    lambda: &AstLambdaExpr,
    expected: usize,
) -> SrcLoc {
    match lambda.parameters().nth(expected) {
        Some(param) => ctx.srcloc(param.syntax()),
        None => ctx.srcloc(lambda.syntax()),
    }
}

/// Whether a lambda's last parameter is the rest of its arguments, in which case it has no number
/// of parameters to be too many or too few.
pub(crate) fn has_rest_parameter(lambda: &AstLambdaExpr) -> bool {
    lambda.parameters().any(|param| param.spread().is_some())
}

/// Compiles a lambda that's an argument of a call whose generics are being inferred, as
/// `compile_lambda` does, with each untyped parameter that feeds itself typed as the least fixpoint
/// of what it's given joined with what the lambda returns (#362, R4).
///
/// A parameter feeds itself where it's expected to have the type the lambda is expected to return,
/// as `acc` in `fn(acc: A, x: T) -> A` for `fold`, since what the lambda returns is passed back in
/// as it. It's given what the call's generic is inferred as so far, such as `(0, 0)` from the seed
/// in `fold(xs, (0, 0), fn(acc, x) => (acc.first + x, acc.rest + 1))`, but the generic has to
/// hold what the lambda returns too, `(Int, Int)`, and the parameter has to take the generic, so
/// where what it returns isn't assignable to what it was given, the lambda can't be assigned to the
/// call's parameter, and the call is rejected. The least fixpoint is the least type `P` that holds
/// what it was given and what the lambda returns with the parameter as `P`. Compiling the lambda is
/// the first step, and where what it returns is assignable to what it was given, that's the
/// fixpoint, so nothing else happens. Otherwise, it's compiled once more with the parameter as the
/// next step:
/// - what it was given joined with what it returned, as `(0, 0) | (Int, Int)`, which holds what
///   it returns with it for a lambda whose return type doesn't grow with its parameter's;
/// - or, for a lambda that conses onto the list it's given, as `fn(acc, x) => [x, ...acc]` does,
///   returning a pair whose rest is the parameter (after any number of pairs), the list of their
///   firsts, `List<X>`. That's the one step that isn't a join: joining `nil` with `(X, nil)` never
///   stops growing, while `List<X>` is the least solution of `P ⊇ nil ∪ (X, P)`, and of
///   `P ⊇ G ∪ (X, P)` for what it was given `G` where `G` is a list of `X`.
///
/// That's kept if the body compiles against it without errors and what it returns is assignable to
/// it, and the lambda is then as if it had only been compiled with it, since what the first compile
/// added is undone (see `Compiler::checkpoint`). Otherwise the lambda is compiled as it was first
/// once more, and the call is rejected as before. So a lambda is compiled at most twice where the
/// step is kept, and three times where it isn't, which only a call that's rejected anyway does, and
/// a lambda that feeds itself inside another one is compiled at most three times for each time the
/// other one is, down to `FEEDING_DEPTH` of them, past which a lambda is compiled once, as it is
/// without this, and a call that needs a step there is rejected.
///
/// Only a parameter that feeds itself through one of the call's generics does, which is decided from
/// the type the lambda is declared to have (`declared`), before the call's generics are substituted
/// into it: the parameter's declared type and the declared return type are the same generic of the
/// innermost call. Two different generics can be inferred as the same type, as `A` and `B` are as
/// `nil` in `pick(nil, nil, fn(x) => [1, ...x])` for `fn pick<A, B>(a: A, b: B, f: fn(x: A) -> B)`,
/// and then the parameter doesn't feed itself. It only does while the type it's
/// expected to have can still change (see `Compiler::is_expected_type_provisional` and
/// `Compiler::is_expected_type_hint`), and only where neither what it was given nor what the lambda
/// returns has a generic in it, and the lambda compiles without errors as it is. A generic, such
/// as one of the caller's that stands for one of the callee's renamed generics in a recursive call,
/// is only resolved once the call is, which can make what the lambda returns assignable after all.
pub(crate) fn compile_lambda_feeding_itself(
    ctx: &mut Compiler,
    expr: &AstLambdaExpr,
    expected_type: Option<TypeId>,
    declared: Option<TypeId>,
    wrap: &ParameterWrap<'_>,
) -> (Value, Vec<LambdaParameter>) {
    let feeding = expected_type
        .zip(declared)
        .and_then(|(expected, declared)| feeding_parameters(ctx, expr, expected, declared));

    let Some((expected, function, indices)) = feeding else {
        return compile_lambda(ctx, expr, expected_type, Some(wrap));
    };

    // Each lambda that feeds itself inside another one multiplies how often the innermost body is
    // compiled, so past a few levels they're compiled once, as they would be without this
    if ctx.feeding_depth() >= FEEDING_DEPTH {
        return compile_lambda(ctx, expr, Some(expected), Some(wrap));
    }

    ctx.set_feeding_depth(ctx.feeding_depth() + 1);
    let compiled = compile_lambda_in_steps(ctx, expr, expected, &function, &indices, wrap);
    ctx.set_feeding_depth(ctx.feeding_depth() - 1);
    compiled
}

/// How many lambdas that feed themselves are compiled in steps inside one another, which bounds
/// the innermost one's body to being compiled `3^4` times.
const FEEDING_DEPTH: usize = 4;

/// Compiles a lambda whose parameters at `indices` feed themselves (see
/// `compile_lambda_feeding_itself`).
fn compile_lambda_in_steps(
    ctx: &mut Compiler,
    expr: &AstLambdaExpr,
    expected: TypeId,
    function: &FunctionType,
    indices: &[usize],
    wrap: &ParameterWrap<'_>,
) -> (Value, Vec<LambdaParameter>) {
    let before = ctx.checkpoint();
    let count = ctx.diagnostic_count();
    let compiled = compile_lambda(ctx, expr, Some(expected), Some(wrap));

    if ctx.has_errors_since(count) {
        return compiled;
    }

    let Some(next) = next_step(ctx, compiled.0.ty, indices) else {
        return compiled;
    };

    ctx.restore(&before);

    // The lambda's expected type with the parameters that feed themselves, and its return type,
    // as the next step, and the parameters' generics as it too for a wrap that has them
    let mut stepped = function.clone();
    for &index in indices {
        stepped.params[index].ty = next;
    }
    stepped.ret = next;
    let stepped_type = ctx.alloc_type(Type::Function(stepped));

    let mappings: HashMap<TypeId, TypeId> = match wrap {
        ParameterWrap::Whole => HashMap::new(),
        ParameterWrap::Generics {
            params, mappings, ..
        } => {
            let mut mappings = (*mappings).clone();
            for &index in indices {
                mappings.insert(params[index], next);
            }
            mappings
        }
    };
    let stepped_wrap = match wrap {
        ParameterWrap::Whole => ParameterWrap::Whole,
        ParameterWrap::Generics {
            params, generics, ..
        } => ParameterWrap::Generics {
            params,
            generics,
            mappings: &mappings,
        },
    };

    let count = ctx.diagnostic_count();
    let stepped_compile = compile_lambda(ctx, expr, Some(stepped_type), Some(&stepped_wrap));

    if !ctx.has_errors_since(count)
        && lambda_return(ctx, stepped_compile.0.ty).is_some_and(|ret| ctx.is_assignable(ret, next))
    {
        return stepped_compile;
    }

    // Undoing the step undoes what the first compile recorded too, since a checkpoint only knows
    // where things ended, so the lambda is compiled as it was first once more
    ctx.restore(&before);
    compile_lambda(ctx, expr, Some(expected), Some(wrap))
}

/// The lambda's expected type, its one function type, and the untyped parameters that feed
/// themselves through it (see `compile_lambda_feeding_itself`), if there are any.
fn feeding_parameters(
    ctx: &mut Compiler,
    expr: &AstLambdaExpr,
    expected: TypeId,
    declared: TypeId,
) -> Option<(TypeId, FunctionType, Vec<usize>)> {
    if !ctx.is_expected_type_provisional() && !ctx.is_expected_type_hint() {
        return None;
    }

    let functions = rue_types::extract_functions(ctx.types_mut(), expected, true);
    let [function] = functions.as_slice() else {
        return None;
    };

    if !function.generics.is_empty() {
        return None;
    }

    // Whether a parameter feeds itself is decided before the generics are substituted, since two of
    // them can be inferred as the same type
    let Type::Function(declared) = ctx.ty(declared).clone() else {
        return None;
    };

    if !declared.generics.is_empty()
        || declared.params.len() != function.params.len()
        || !ctx
            .call_generics()
            .is_some_and(|generics| generics.contains(&declared.ret))
    {
        return None;
    }

    // A lambda with a different number of parameters isn't assignable whatever they're typed as
    if expr.parameters().count() != declared.params.len() {
        return None;
    }

    let indices: Vec<usize> = expr
        .parameters()
        .enumerate()
        .filter(|(_, parameter)| parameter.ty().is_none())
        .map(|(index, _)| index)
        .filter(|&index| declared.params[index].ty == declared.ret)
        .collect();

    // Only the parameters that feed themselves are typed as the next step, so the generic can't
    // be in the type of any other parameter, as it is in `k: fn(a: A) -> Int`, which would be left
    // as it was first given
    for (index, parameter) in declared.params.iter().enumerate() {
        if !indices.contains(&index)
            && rue_types::referenced_generics(ctx.types_mut(), parameter.ty).contains(&declared.ret)
        {
            return None;
        }
    }

    (!indices.is_empty()).then(|| (expected, function.clone(), indices))
}

/// The type the parameters that feed themselves are compiled as next, from the lambda as it was
/// compiled (see `compile_lambda_feeding_itself`), if it isn't the fixpoint already.
fn next_step(ctx: &mut Compiler, lambda: TypeId, indices: &[usize]) -> Option<TypeId> {
    let functions = rue_types::extract_functions(ctx.types_mut(), lambda, true);
    let [function] = functions.as_slice() else {
        return None;
    };

    let given = function.params.get(indices[0])?.ty;
    let returned = function.ret;

    // A generic in either, the call's or one that stands for one of its renamed generics in a
    // recursive call, is only resolved once the call is, which can make them assignable
    if names_generics(ctx, given)
        || names_generics(ctx, returned)
        || ctx.is_assignable(returned, given)
    {
        return None;
    }

    // A pair, or pairs, whose rest is the parameter, as the lambda conses onto it
    let mut firsts = Vec::new();
    let mut rest = returned;

    for _ in 0..CONS_DEPTH {
        let Some((first, next)) = pair_parts(ctx, rest) else {
            break;
        };

        firsts.push(first);
        rest = next;

        if ctx.is_assignable(rest, given) && ctx.is_assignable(given, rest) {
            let item = ctx.alloc_type(Type::Union(Union::new(firsts)));
            let list = list_of(ctx, item);

            // The list has to hold what was given and what was returned, which a struct that's
            // a pair underneath isn't, so that's joined instead
            if ctx.is_assignable(given, list) && ctx.is_assignable(returned, list) {
                return Some(list);
            }

            break;
        }
    }

    Some(ctx.alloc_type(Type::Union(Union::new(vec![given, returned]))))
}

/// Whether the type has any generic in it.
fn names_generics(ctx: &mut Compiler, ty: TypeId) -> bool {
    !rue_types::referenced_generics(ctx.types_mut(), ty).is_empty()
}

/// How many pairs a lambda that conses onto its parameter is followed through, as `[x, y, ...acc]`
/// is two.
const CONS_DEPTH: usize = 8;

/// The return type of the lambda's one function type.
fn lambda_return(ctx: &mut Compiler, lambda: TypeId) -> Option<TypeId> {
    let functions = rue_types::extract_functions(ctx.types_mut(), lambda, true);
    match functions.as_slice() {
        [function] => Some(function.ret),
        _ => None,
    }
}

/// Compiles a lambda, with its untyped parameters wrapped as `wrap` says, if it's given. It returns
/// the parameters it wrapped.
pub(crate) fn compile_lambda(
    ctx: &mut Compiler,
    expr: &AstLambdaExpr,
    expected_type: Option<TypeId>,
    wrap: Option<&ParameterWrap<'_>>,
) -> (Value, Vec<LambdaParameter>) {
    let first_hir = ctx.next_hir_id();
    let first_symbol = ctx.symbol_count();
    let first_named_type = ctx.named_type_count();
    let mut wrapped = Vec::new();

    // The reference for each of the callee's generics, with `ParameterWrap::Generics`
    let mut references = HashMap::new();

    let mut expected_functions = if let Some(ty) = expected_type {
        rue_types::extract_functions(ctx.types_mut(), ty, true)
    } else {
        vec![]
    };

    let scope = ctx.alloc_child_scope();

    let mut vars = if let Some(generic_parameters) = expr.generic_parameters() {
        compile_generic_parameters(ctx, scope, &generic_parameters)
    } else {
        Vec::new()
    };

    // If the parameter types come from a generic function type, the lambda is also generic over
    // its own copies of that function's generics, so that they don't leak out of it
    for function in &mut expected_functions {
        let mut mappings = HashMap::new();

        for &generic in &function.generics {
            let copy = ctx.ty(generic).clone();
            let copy = ctx.alloc_type(copy);
            mappings.insert(generic, copy);
            vars.push(copy);
        }

        for param in &mut function.params {
            param.ty = substitute_with_mappings(ctx.types_mut(), param.ty, &mappings);
        }

        function.ret = substitute_with_mappings(ctx.types_mut(), function.ret, &mappings);
    }

    let range = expr.syntax().text_range();
    ctx.push_scope(scope, range.start());

    let mut parameters = Vec::new();
    let mut params = Vec::new();
    let mut nil_terminated = true;

    let len = expr.parameters().count();

    // A parameter that the function type expected of the lambda doesn't have is one too many,
    // rather than one that needs a type. That's reported once, for the first of them
    let first_extra_parameter = expected_parameter_count(&expected_functions)
        .filter(|&expected| expected < len && !has_rest_parameter(expr))
        .map(|expected| (expected, parameter_count_site(ctx, expr, expected)));
    let mut too_many_parameters = false;

    // The untyped parameters, by name and type, if the lambda is an argument of a call whose
    // generics are being inferred. The ones whose types have those generics, which weren't
    // inferred by the time the lambda was compiled, are kept for an error about one of them to
    // say so (see `Compiler::name_uninferred_generics`)
    let inferred_generics = ctx.inferred_generics();
    let uninferred_parameters = ctx.uninferred_parameter_count();
    let mut untyped_parameters = Vec::new();

    for (i, param) in expr.parameters().enumerate() {
        let is_spread = if let Some(spread) = param.spread() {
            if i == len - 1 {
                true
            } else {
                ctx.diagnostic(
                    &spread,
                    DiagnosticKind::NonFinalSpread(SpreadTarget::Parameter),
                );
                false
            }
        } else {
            false
        };

        if is_spread {
            nil_terminated = false;
        }

        let ty = if let Some(ty) = param.ty() {
            compile_type(ctx, &ty)
        } else if !expected_functions.is_empty()
            && let params = expected_functions
                .iter()
                .filter_map(|function| function.params.get(i).map(|param| param.ty))
                .collect::<Vec<_>>()
            && !params.is_empty()
        {
            if params.len() == 1 {
                params[0]
            } else {
                ctx.alloc_type(Type::Union(Union::new(params)))
            }
        } else {
            debug!("Unresolved lambda parameter type due to missing inference");

            match &first_extra_parameter {
                Some((expected, site)) => {
                    if !too_many_parameters {
                        ctx.diagnostic_at(
                            site.clone(),
                            DiagnosticKind::LambdaParameterCount(*expected, len),
                        );
                    }

                    too_many_parameters = true;
                }
                None => {
                    ctx.diagnostic(param.syntax(), DiagnosticKind::CannotInferParameterType);
                }
            }

            ctx.unresolved().ty
        };

        // Not for a generic lambda or one expected to be a generic function
        let ty = match wrap {
            Some(wrap)
                if param.ty().is_none()
                    && !is_spread
                    && vars.is_empty()
                    && !matches!(ctx.ty(ty), Type::Unresolved) =>
            {
                wrap_parameter(ctx, wrap, i, ty, &mut references, &mut wrapped)
            }
            _ => ty,
        };

        let symbol = ctx.alloc_symbol(Symbol::Parameter(ParameterSymbol { name: None, ty }));

        let name = param
            .binding()
            .map_or(String::new(), |binding| binding.syntax().text().to_string());

        // Before the parameter is destructured, which is rejected if it was left as a generic
        if param.ty().is_none() && !inferred_generics.is_empty() {
            register_uninferred_parameter(ctx, &inferred_generics, &name, ty);
            untyped_parameters.push((name.clone(), ty));
        }

        if let Some(binding) = param.binding() {
            let count = ctx.diagnostic_count();
            create_binding(ctx, symbol, &binding);

            // A parameter that can't be destructured because its type wasn't inferred, so it
            // was left as a generic of a call that the lambda is an argument of, or as a
            // reference to one, is an error that follows from that
            if param.ty().is_none() && ctx.diagnostic_count() > count {
                let mut generics =
                    contained_generics(ctx.types_mut(), ty, inferred_generics.iter().copied());

                for &(reference, target) in &wrapped {
                    if contains_generic(ctx.types_mut(), ty, reference) {
                        generics.extend(contained_generics(
                            ctx.types_mut(),
                            target,
                            inferred_generics.iter().copied(),
                        ));
                    }
                }

                if !generics.is_empty() {
                    ctx.mark_uninferred_errors_since(count, &generics);
                }
            }
        }

        parameters.push(symbol);
        params.push(FunctionParameter::new(name, ty));
    }

    if matches!(wrap, Some(ParameterWrap::Generics { .. })) {
        make_references(ctx, &wrapped);
    }

    // The generics that a parameter refers to are in its type from here on
    for (name, ty) in untyped_parameters {
        register_uninferred_parameter(ctx, &inferred_generics, &name, ty);
    }

    let return_type = expr.ty().map(|ty| compile_type(ctx, &ty));

    // Without an annotation, the lambda returns whatever its body does, but the body is expected
    // to be what the function type expected of the lambda returns, so that `identity(7)` in
    // `fn(x: Int) => identity(7)` isn't widened to `Int` when a `7` is expected
    let body_type = return_type.or_else(|| expected_return_type(ctx, &expected_functions));

    ctx.push_return_target(Some(ReturnTarget {
        expected_type: return_type,
        types: Vec::new(),
    }));

    // An annotated return type is a type of its own that the body must be assignable to
    let hint = ctx.is_expected_type_hint() && return_type.is_none();

    let body = if let Some(body) = expr.body() {
        let provisional = ctx.is_expected_type_provisional() && return_type.is_none();
        let result = ctx.with_expected_type_provisional(provisional, |ctx| {
            ctx.with_expected_type_hint(hint, |ctx| {
                if let AstExpr::Block(block) = &body {
                    ctx.add_syntax(
                        SyntaxItemKind::CompletionContext(CompletionContext::Expression),
                        block.syntax().text_range(),
                    );
                    compile_block(ctx, block, BlockKind::Body, body_type, true)
                } else {
                    compile_expr(ctx, &body, body_type)
                }
            })
        });
        if let Some(return_type) = return_type {
            ctx.assign_type(expr.syntax(), result.ty, return_type);
        }
        result
    } else {
        debug!("Unresolved lambda body");
        ctx.unresolved()
    };

    ctx.truncate_uninferred_parameters(uninferred_parameters);

    let early_return_types = ctx
        .pop_return_target()
        .map(|target| target.types)
        .unwrap_or_default();

    // If the return type isn't specified, it's inferred from both the body and any
    // `return` statements that exit the lambda early.
    let return_type = return_type.unwrap_or_else(|| {
        if early_return_types.is_empty() {
            body.ty
        } else {
            let mut types = early_return_types;
            types.push(body.ty);
            ctx.alloc_type(Type::Union(Union::new(types)))
        }
    });

    let ty = ctx.alloc_type(Type::Function(FunctionType {
        generics: vars.clone(),
        params,
        nil_terminated,
        ret: return_type,
    }));

    ctx.pop_scope(range.end());

    let symbol = ctx.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: None,
        ty,
        scope,
        vars,
        parameters,
        nil_terminated,
        return_type,
        body: body.hir,
        kind: FunctionKind::Sequential,
    }));

    let hir = ctx.alloc_hir(Hir::Lambda(symbol));

    let lambda = LambdaBody {
        first_hir,
        end_hir: ctx.next_hir_id(),
        first_symbol,
        first_named_type,
        end_named_type: ctx.named_type_count(),
        ty,
    };

    let parameters = wrapped
        .into_iter()
        .map(|(reference, ty)| LambdaParameter {
            reference,
            ty,
            lambda,
        })
        .collect();

    (Value::new(hir, ty), parameters)
}

/// What the body of a lambda without a return annotation is expected to be: the return type of
/// the function type expected of it, or the union of them if it could be one of several, as its
/// parameter types are. A call in the body leaves out what it would infer from generics that an
/// enclosing call is still inferring, such as `B` in `map(fn(x) => identity(x), 5)`, as it does
/// with any expected type.
fn expected_return_type(ctx: &mut Compiler, functions: &[FunctionType]) -> Option<TypeId> {
    match functions {
        [] => None,
        [function] => Some(function.ret),
        functions => {
            let types = functions.iter().map(|function| function.ret).collect();
            Some(ctx.alloc_type(Type::Union(Union::new(types))))
        }
    }
}

/// The type an untyped parameter is compiled as (see `ParameterWrap`), given the type it would have,
/// with each new reference added to `wrapped` along with the type it refers to.
fn wrap_parameter(
    ctx: &mut Compiler,
    wrap: &ParameterWrap<'_>,
    index: usize,
    ty: TypeId,
    references: &mut HashMap<TypeId, TypeId>,
    wrapped: &mut Vec<(TypeId, TypeId)>,
) -> TypeId {
    let ParameterWrap::Generics {
        params,
        generics,
        mappings,
    } = wrap
    else {
        let reference = ctx.alloc_type(Type::Ref(ty));
        ctx.report_type_as(reference, ty);
        wrapped.push((reference, ty));
        return reference;
    };

    let Some(&source) = params.get(index) else {
        return ty;
    };

    let mut substitution = HashMap::new();
    let mut has_reference = false;

    for &generic in *generics {
        if !contains_generic(ctx.types_mut(), source, generic) {
            continue;
        }

        // Only where values of the generic flow into the lambda, which can be narrower than the
        // body was compiled against. Where the lambda passes them out, such as to `cb` in
        // `cb: fn(x: A) -> Int`, a narrower type would let it pass what the caller doesn't accept
        if params
            .iter()
            .any(|&param| occurs_contravariantly(ctx, param, generic))
        {
            let target = mappings.get(&generic).copied().unwrap_or(generic);
            substitution.insert(generic, target);
            continue;
        }

        // It's a generic until every parameter is substituted with it (see `make_references`),
        // since a mapped type is substituted itself, which would see through a reference. It has
        // the name of the generic that it stands for, which is what an error shows it as until
        // then, such as one for a parameter that can't be destructured
        let reference = *references.entry(generic).or_insert_with(|| {
            let target = mappings.get(&generic).copied().unwrap_or(generic);
            let placeholder = match ctx.ty(generic) {
                Type::Generic(generic) => generic.clone(),
                _ => Generic { name: None },
            };
            let reference = ctx.alloc_type(Type::Generic(placeholder));
            wrapped.push((reference, target));
            reference
        });

        substitution.insert(generic, reference);
        has_reference = true;
    }

    // Without a reference in it, it's the type it would have
    if !has_reference {
        return ty;
    }

    substitute_with_mappings(ctx.types_mut(), source, &substitution)
}

/// Turns the placeholders that `wrap_parameter` substituted for the callee's generics, with
/// `ParameterWrap::Generics`, into the references they stand for, once every parameter has been
/// substituted with them.
fn make_references(ctx: &mut Compiler, wrapped: &[(TypeId, TypeId)]) {
    for &(reference, target) in wrapped {
        *ctx.ty_mut(reference) = Type::Ref(target);
        ctx.report_type_as(reference, target);
    }
}

/// Whether the generic occurs in the type where values flow out of whoever holds a value of it,
/// such as a parameter of a function type in it.
pub(crate) fn occurs_contravariantly(ctx: &mut Compiler, ty: TypeId, generic: TypeId) -> bool {
    let ty = rue_types::substitute(ctx.types_mut(), ty);
    let mut visited = std::collections::HashSet::new();
    let mut stack = vec![(ty, true)];

    while let Some((ty, covariant)) = stack.pop() {
        if !visited.insert((ty, covariant)) {
            continue;
        }

        if ty == generic {
            if !covariant {
                return true;
            }
            continue;
        }

        match ctx.ty(ty).clone() {
            Type::Ref(inner) => stack.push((inner, covariant)),
            Type::Pair(pair) => stack.extend([(pair.first, covariant), (pair.rest, covariant)]),
            Type::Union(union) => stack.extend(union.types.into_iter().map(|ty| (ty, covariant))),
            Type::Alias(alias) => stack.push((alias.inner, covariant)),
            Type::Struct(ty) => stack.push((ty.inner, covariant)),
            Type::Function(function) => {
                stack.push((function.ret, covariant));
                stack.extend(function.param_types().map(|ty| (ty, !covariant)));
            }
            Type::Apply(_)
            | Type::Unresolved
            | Type::Generic(_)
            | Type::Any
            | Type::Never
            | Type::Atom(_) => {}
        }
    }

    false
}
