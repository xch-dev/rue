use std::collections::{HashMap, HashSet};

use log::debug;
use rue_ast::{AstExpr, AstFunctionCallExpr, AstLambdaExpr, AstNode};
use rue_diagnostic::{DiagnosticKind, Note, SpreadTarget};
use rue_hir::{FunctionCall, Hir, Symbol, Value};
use rue_types::{
    FunctionType, Type, TypeId, Union, contained_generics, contains_generic,
    substitute_with_mappings,
};

use super::builtin::compile_builtin;
use super::call_inference::{InferredCall, infer_call};
use super::lambda::{expected_parameter_count, has_rest_parameter, parameter_count_site};
use crate::{
    Compiler, CompletionContext, SyntaxCall, SyntaxItemKind, compile_expr, compile_path_value,
    names_type, occurs_contravariantly,
};

pub fn compile_function_call_expr(
    ctx: &mut Compiler,
    call: &AstFunctionCallExpr,
    expected_type: Option<TypeId>,
    narrower: Option<TypeId>,
) -> Value {
    let Some(call_expr) = call.expr() else {
        debug!("Unresolved function call expr");
        return ctx.unresolved();
    };

    // Builtins can only be referenced when they're called directly
    let expr = if let AstExpr::PathExpr(path) = &call_expr {
        ctx.add_syntax(
            SyntaxItemKind::CompletionContext(CompletionContext::Expression),
            path.syntax().text_range(),
        );
        compile_path_value(ctx, path, true)
    } else {
        compile_expr(ctx, &call_expr, None)
    };

    if let Hir::Reference(symbol) = ctx.hir(expr.hir).clone()
        && let Symbol::Builtin(builtin) = ctx.symbol(symbol).clone()
    {
        return compile_builtin(ctx, call, builtin);
    }

    // Paths are compiled directly rather than with `compile_expr`, so their type is recorded here
    let expr = ctx.record_value(call_expr.syntax(), expr);

    let is_unresolved = ctx.is_unresolved(expr.ty);

    let mut expected_functions = rue_types::extract_functions(ctx.types_mut(), expr.ty, true);

    if expected_functions.len() > 1 {
        let name = ctx.type_name(expr.ty);
        ctx.diagnostic(
            call.syntax(),
            DiagnosticKind::CannotDisambiguateFunctionTypes(name),
        );
    } else if expected_functions.is_empty() && !is_unresolved {
        let name = ctx.type_name(expr.ty);
        ctx.diagnostic(call.syntax(), DiagnosticKind::InvalidFunctionCall(name));
    }

    // The function as it's declared, which is what the language server shows, unless the call
    // is rejected because it could be to more than one function. The type of a function that's
    // called directly is taken as it is, since its parts are the types that the code names,
    // which the function that's extracted from it has copies of
    let declared = match expected_functions.as_slice() {
        [function] => {
            let mut ty = expr.ty;

            while let Type::Ref(inner) = ctx.ty(ty) {
                ty = *inner;
            }

            match ctx.ty(ty) {
                Type::Function(declared) if declared.generics.len() == function.generics.len() => {
                    Some(declared.clone())
                }
                _ => Some(function.clone()),
            }
        }
        _ => None,
    };

    // The callee's generics that are also in scope, as in a recursive call, are renamed to fresh
    // ones, so that they aren't confused with the caller's generics with the same ids (#334)
    let renamed = expected_functions
        .first_mut()
        .map(|function| rename_generics_in_scope(ctx, function))
        .unwrap_or_default();

    let expected_function = expected_functions.first();

    let len = call.args().count();

    let mut nil_terminated = true;
    let mut args = Vec::new();

    for (i, arg) in call.args().enumerate() {
        if let Some(spread) = arg.spread() {
            if i == len - 1 {
                nil_terminated = false;
            } else {
                ctx.diagnostic(
                    &spread,
                    DiagnosticKind::NonFinalSpread(SpreadTarget::Argument),
                );
            }
        }

        args.extend(arg.expr());
    }

    let mut generalized = Vec::new();

    let (args, mappings) = if let Some(function) = expected_function {
        // An error in the call from here on, in its arguments or in how many there are, is what
        // a generic that isn't inferred is left to, rather than reported as well
        let reported = ctx.diagnostic_count();

        if function.nil_terminated && !nil_terminated {
            ctx.diagnostic(call.syntax(), DiagnosticKind::UnexpectedSpread);
        } else if !function.nil_terminated && nil_terminated && function.params.len() == args.len()
        {
            ctx.diagnostic(call.syntax(), DiagnosticKind::MissingSpread);
        }

        if function.params.len() != args.len() {
            ctx.diagnostic(
                call.syntax(),
                DiagnosticKind::ExpectedArguments(function.params.len(), args.len()),
            );
        }

        // Substituting a generic alias's expansion renames the generics of the function types in
        // it to fresh copies (see `rue_types::substitute`), so the expected type is substituted
        // once here, for inference and `generalize` to see the same copies. Otherwise, the generics
        // that the callee's are inferred as wouldn't be the ones generalized over, and would leak.
        // A narrower expected type is a parameter type that's already been substituted
        let expected_type = if function.generics.is_empty() {
            expected_type
        } else {
            expected_type.map(|ty| rue_types::substitute(ctx.types_mut(), ty))
        };

        let InferredCall {
            results,
            checks,
            mut mappings,
            unrestored,
        } = infer_call(
            ctx,
            function,
            &args,
            (
                expected_type,
                ctx.argument_requirement(call.syntax().text_range()),
            ),
            narrower,
            &renamed,
        );

        let mut uninferred = HashMap::new();
        let mut unreported = Vec::new();
        let mut from_unresolved = Vec::new();

        if let Some(with_never) = unconstrained_as_never(ctx, function, &checks, &mappings) {
            mappings = with_never;
        }

        // In a recursive call, a renamed generic that nothing gave a type stands in as the
        // caller's generic it was renamed from (see `restore_renamed`). Where an error that's
        // been reported is why nothing did, the generic is treated as one that wasn't inferred,
        // and is left to that error like the others, rather than leaking the caller's generic
        // into what the call returns
        let stand_ins = defer_stand_ins(
            ctx,
            function,
            (&args, &checks),
            (&mut mappings, &unrestored),
            &renamed,
            reported,
        );

        for &generic in &function.generics {
            if mappings.contains_key(&generic) {
                continue;
            }

            // The callee's generic would leak out, unless the function's type doesn't reference
            // it at all
            if !std::iter::once(function.ret)
                .chain(function.param_types())
                .any(|ty| contains_generic(ctx.types_mut(), ty, generic))
            {
                continue;
            }

            // An argument that it would have been inferred from has already failed to compile, or
            // the generic is a stand-in left to an error (see above)
            if stand_ins.contains(&generic) || from_unresolved_argument(ctx, &checks, generic) {
                from_unresolved.push(generic);
            } else {
                unreported.push(generic);
            }

            let unresolved = ctx.unresolved().ty;
            mappings.insert(generic, unresolved);
            uninferred.insert(generic, unresolved);
        }

        // The language server shows what the call's generics were inferred as, which leaves out
        // the ones that weren't. They're the generics as they're declared, which are in the same
        // order as the ones that were inferred
        if let Some(declared) = declared {
            let inferred = function
                .generics
                .iter()
                .zip(&declared.generics)
                .filter(|(generic, _)| !uninferred.contains_key(*generic))
                .filter_map(|(generic, declared)| Some((*declared, *mappings.get(generic)?)))
                .collect();

            ctx.add_syntax(
                SyntaxItemKind::Call(SyntaxCall {
                    function: declared,
                    generics: inferred,
                }),
                call.syntax().text_range(),
            );
        }

        // What the parameters are shown as in an error if a generic wasn't inferred, which is with
        // the generic as it's declared, rather than as the placeholder that it is from here on
        let declared = (!uninferred.is_empty()).then(|| {
            mappings
                .iter()
                .filter(|(generic, _)| !uninferred.contains_key(*generic))
                .map(|(&generic, &ty)| (generic, ty))
                .collect::<HashMap<_, _>>()
        });

        // The other generics can be inferred as types with these in them, such as `U` as `T` in
        // `map_list([fn(x) => x], { raise; })`, where nothing gives `T` a type, so they're
        // unresolved there too, rather than leaking out of the call
        if !uninferred.is_empty() {
            for ty in mappings.values_mut() {
                *ty = substitute_with_mappings(ctx.types_mut(), *ty, &uninferred);
            }
        }

        // The function that's returned can stay generic over the generics of a function type
        // expected of the call
        if let Some(expected_type) = expected_type {
            generalized = generalize(ctx, function, expected_type, &mut mappings);
        }

        // Inference only proposes types for the generics, so each argument must be checked
        // against the fully substituted parameter type. This catches arguments that constrain
        // a generic inconsistently, such as `(Int, fn(x: Bytes32) -> Bytes32)` being passed to
        // a parameter of type `(T, fn(x: T) -> Bytes32)`.
        for (expr, ty, param) in checks {
            let to = substitute_with_mappings(ctx.types_mut(), param, &mappings);

            if let AstExpr::LambdaExpr(lambda) = &expr
                && check_lambda_parameter_count(ctx, lambda, ty, to, reported)
            {
                continue;
            }

            let shown = match &declared {
                Some(declared) => substitute_with_mappings(ctx.types_mut(), param, declared),
                None => to,
            };

            let count = ctx.diagnostic_count();
            ctx.assign_type_shown_as(expr.syntax(), ty, to, shown);

            if ctx.diagnostic_count() == count {
                continue;
            }

            note_inferred_unions(
                ctx,
                count,
                function,
                param,
                declared.as_ref().unwrap_or(&mappings),
            );

            // An argument whose type has one of the callee's generics in it, such as a lambda
            // that returns its parameter, which was left as the generic, is rejected because
            // the generic wasn't inferred for it, rather than the other way around
            let generics = function.generics.iter().copied();
            let generics = contained_generics(ctx.types_mut(), ty, generics);

            if !generics.is_empty() {
                ctx.mark_uninferred_errors_since(count, &generics);
            }
        }

        // A generic often isn't inferred because of an error that's already been reported: one
        // in an argument whose parameter has the generic in its type, such as an argument of the
        // wrong type, or one for the arguments as a whole, such as one that's missing. That error
        // is what to fix, which specifying the generic wouldn't. An error in another argument
        // isn't why, and neither is any error for a generic that's only in the return type
        let deferred = if uninferred.is_empty() {
            Vec::new()
        } else {
            deferred_generics(
                ctx,
                function,
                &args,
                (&uninferred, &from_unresolved),
                reported,
            )
        };

        // The errors that only show those generics, such as one in the body of a lambda whose
        // parameter was left as one, follow from that error as well
        if !deferred.is_empty() {
            ctx.suppress_uninferred_errors_since(reported, &deferred);
        }

        for generic in unreported {
            if deferred.contains(&generic) {
                continue;
            }

            // The generic is the callee's, whatever a type in the arguments was left as
            let scope = ctx.last_scope_id();
            let generic_name = ctx.type_name_in(scope, generic);
            let function_name = callee_name(call);

            ctx.diagnostic(
                call.syntax(),
                DiagnosticKind::CannotInferGeneric(generic_name, function_name),
            );
        }

        // The errors that show this call's generics are its own to report or leave out, rather
        // than a call's that it's an argument of, which has the same generics if it's a call of
        // the same function
        if !function.generics.is_empty() {
            ctx.settle_uninferred_errors_since(reported, &function.generics);
        }

        (results, mappings)
    } else {
        (vec![], HashMap::new())
    };

    let ty = if expected_functions.is_empty() {
        debug!("Unresolved function call return type due to unresolved function type");
        ctx.note_placeholder();
        ctx.alloc_type(Type::Unresolved)
    } else if expected_functions.len() == 1 {
        expected_functions[0].ret
    } else {
        ctx.alloc_type(Type::Union(Union::new(
            expected_functions
                .iter()
                .map(|function| function.ret)
                .collect(),
        )))
    };

    let mut ty = substitute_with_mappings(ctx.types_mut(), ty, &mappings);

    if !generalized.is_empty() {
        ty = add_generics(ctx, ty, &generalized);
    }

    let hir = ctx.alloc_hir(Hir::FunctionCall(FunctionCall {
        function: expr.hir,
        args,
        nil_terminated,
    }));

    Value::new(hir, ty)
}

/// The mappings and return type that a call with the mappings is checked with for the expected
/// type, once the functions it returns are generalized over the generics of the function types
/// expected of them (see `generalize`). The mappings are left as they are.
pub(crate) fn generalized_call(
    ctx: &mut Compiler,
    function: &FunctionType,
    expected_type: TypeId,
    mappings: &HashMap<TypeId, TypeId>,
) -> (HashMap<TypeId, TypeId>, TypeId) {
    let mut mappings = mappings.clone();
    let generalized = generalize(ctx, function, expected_type, &mut mappings);
    let mut ty = substitute_with_mappings(ctx.types_mut(), function.ret, &mappings);

    if !generalized.is_empty() {
        ty = add_generics(ctx, ty, &generalized);
    }

    (mappings, ty)
}

/// Gives the callee's generics that nothing gives a type `Never`, where the arguments hold no
/// value of them: `nil` holds no `T` for `List<T>`, so `length(nil)` and `reverse([])` don't need
/// `T` written out, and `reverse([])` is a `List<Never>`, which is a list of anything. It returns
/// the mappings with those generics added, or `None` if there are none.
///
/// The generics are the ones that a parameter's type references, and that neither a parameter's
/// type nor the return type references where values of them flow in (see
/// `occurs_contravariantly`). They're `Never` together or not at all: only if every argument is
/// assignable to its parameter with them as that, and the call still has a value. So these are
/// reported as they were:
///
/// - A generic that no parameter's type references, such as `T` in `fn empty<T>() -> List<T>`,
///   which is the caller's to pick.
/// - A generic that's a lambda argument's parameter, or anywhere else in what an argument was
///   compiled as, which would still name it, or a parameter of a function that's returned, such as
///   `A` in `fn make<A>(list: List<A>) -> fn(a: A) -> A`, where no type is the most useful one.
/// - A generic that the call would have no value with, such as `T` in `first(nil)` with
///   `fn first<T>(list: List<T>) -> T`: the code after the call would be unreachable.
/// - A generic that an argument which failed to compile, or is missing, would have given a type.
fn unconstrained_as_never(
    ctx: &mut Compiler,
    function: &FunctionType,
    checks: &[(AstExpr, TypeId, TypeId)],
    mappings: &HashMap<TypeId, TypeId>,
) -> Option<HashMap<TypeId, TypeId>> {
    let unmapped: Vec<TypeId> = function
        .generics
        .iter()
        .copied()
        .filter(|generic| !mappings.contains_key(generic))
        .collect();

    // A parameter without an argument, which is reported, could have given them a type
    if unmapped.is_empty() || checks.len() != function.params.len() {
        return None;
    }

    let never = ctx.builtins().types.never;
    let params: Vec<TypeId> = function.param_types().collect();
    let mut with_never = mappings.clone();

    // What the arguments were compiled as, and what the other generics were inferred as from them
    let compiled: Vec<TypeId> = checks
        .iter()
        .map(|&(_, ty, _)| ty)
        .chain(mappings.values().copied())
        .collect();

    for generic in unmapped {
        let in_params = params
            .iter()
            .any(|&ty| contains_generic(ctx.types_mut(), ty, generic));

        let flows_in = params
            .iter()
            .chain([&function.ret])
            .any(|&ty| occurs_contravariantly(ctx, ty, generic));

        if !in_params || flows_in || from_unresolved_argument(ctx, checks, generic) {
            continue;
        }

        // An argument that was compiled against the generic itself still names it, which would
        // leak out of the call
        if compiled
            .iter()
            .any(|&ty| contains_generic(ctx.types_mut(), ty, generic))
        {
            return None;
        }

        with_never.insert(generic, never);
    }

    if with_never.len() == mappings.len() {
        return None;
    }

    let ret = substitute_with_mappings(ctx.types_mut(), function.ret, &with_never);

    if rue_types::is_uninhabited(ctx.types_mut(), ret) {
        return None;
    }

    let assignable = checks.iter().all(|&(_, ty, param)| {
        let param = substitute_with_mappings(ctx.types_mut(), param, &with_never);
        ctx.is_unresolved(ty) || ctx.is_assignable(ty, param)
    });

    assignable.then_some(with_never)
}

/// Takes the renamed generics of a recursive call that `restore_renamed` mapped to the caller's
/// generics they were renamed from, because nothing inferred them, out of the mappings where an
/// error is why nothing did, as `deferred_generics` decides for a generic that isn't inferred, and
/// returns them. What the others were inferred as has them as they were before they were restored,
/// so that they're unresolved there too, rather than the caller's generics.
fn defer_stand_ins(
    ctx: &mut Compiler,
    function: &FunctionType,
    (args, checks): (&[AstExpr], &[(AstExpr, TypeId, TypeId)]),
    (mappings, unrestored): (&mut HashMap<TypeId, TypeId>, &HashMap<TypeId, TypeId>),
    renamed: &HashMap<TypeId, TypeId>,
    reported: usize,
) -> Vec<TypeId> {
    let stand_ins: HashMap<TypeId, TypeId> = renamed
        .iter()
        .filter(|&(copy, generic)| {
            !unrestored.contains_key(copy) && mappings.get(copy) == Some(generic)
        })
        .map(|(&copy, &generic)| (copy, generic))
        .collect();

    if stand_ins.is_empty() {
        return Vec::new();
    }

    // An argument that it would have been inferred from has a part that failed to compile, such
    // as a lambda that returns what it couldn't
    let from_unresolved: Vec<TypeId> = stand_ins
        .keys()
        .copied()
        .filter(|&copy| {
            checks.iter().any(|&(_, ty, param)| {
                rue_types::contains_unresolved(ctx.types(), ty)
                    && contains_generic(ctx.types_mut(), param, copy)
            })
        })
        .collect();

    let deferred = deferred_generics(
        ctx,
        function,
        args,
        (&stand_ins, &from_unresolved),
        reported,
    );

    if deferred.is_empty() {
        return deferred;
    }

    let kept: HashMap<TypeId, TypeId> = stand_ins
        .into_iter()
        .filter(|(copy, _)| !deferred.contains(copy))
        .collect();

    for (generic, &ty) in unrestored {
        if deferred
            .iter()
            .any(|&copy| contains_generic(ctx.types_mut(), ty, copy))
        {
            let ty = substitute_with_mappings(ctx.types_mut(), ty, &kept);
            mappings.insert(*generic, ty);
        }
    }

    for generic in &deferred {
        mappings.remove(generic);
    }

    deferred
}

/// Whether an argument that the generic would have been inferred from has failed to compile.
fn from_unresolved_argument(
    ctx: &mut Compiler,
    checks: &[(AstExpr, TypeId, TypeId)],
    generic: TypeId,
) -> bool {
    checks.iter().any(|&(_, ty, param)| {
        ctx.is_unresolved(ty) && contains_generic(ctx.types_mut(), param, generic)
    })
}

/// The generics that weren't inferred because of an error that's been reported since there were
/// `reported` diagnostics, out of the ones that weren't inferred, in the order they're declared.
///
/// A generic isn't inferred because of an error that's in an argument whose parameter has the
/// generic in its type, or that isn't in any argument, such as the wrong number of them, if a
/// parameter has it at all. The generics that aren't inferred and that a parameter's type has
/// together aren't inferred for the same reason: in `map(5, fn(x) => x + 1)`, `U` is what the
/// lambda returns for a `T`, which the `5` isn't a list of.
fn deferred_generics(
    ctx: &mut Compiler,
    function: &FunctionType,
    args: &[AstExpr],
    (uninferred, from_unresolved): (&HashMap<TypeId, TypeId>, &[TypeId]),
    reported: usize,
) -> Vec<TypeId> {
    let errors = ctx.errors_since(reported);

    // An argument that couldn't be compiled, such as a binding whose value couldn't be, is one
    // that an error has been reported for, which can be before the call
    let from_unresolved = if ctx.has_errors() {
        from_unresolved
    } else {
        &[]
    };

    if errors.is_empty() && from_unresolved.is_empty() {
        return Vec::new();
    }

    let arguments: Vec<_> = args
        .iter()
        .map(|arg| ctx.srcloc(arg.syntax()).span)
        .collect();

    // The generics that weren't inferred in each parameter's type
    let parameters: Vec<Vec<TypeId>> = function
        .param_types()
        .map(|ty| {
            let generics = uninferred.keys().copied();
            contained_generics(ctx.types_mut(), ty, generics)
        })
        .collect();

    let mut deferred: HashSet<TypeId> = from_unresolved.iter().copied().collect();

    for error in errors {
        let argument = arguments
            .iter()
            .position(|arg| arg.start <= error.start && error.end <= arg.end);

        match argument {
            Some(index) => deferred.extend(parameters.get(index).into_iter().flatten()),
            None => deferred.extend(parameters.iter().flatten()),
        }
    }

    // Until no parameter has one that is and one that isn't
    loop {
        let before = deferred.len();

        for generics in &parameters {
            if generics.iter().any(|generic| deferred.contains(generic)) {
                deferred.extend(generics);
            }
        }

        if deferred.len() == before {
            break;
        }
    }

    function
        .generics
        .iter()
        .copied()
        .filter(|generic| deferred.contains(generic))
        .collect()
}

/// Reports a lambda that's passed for a function type with a different number of parameters, if
/// that hasn't been reported while it was compiled (among the diagnostics since there were
/// `reported` of them), and returns whether that's why it can't be assigned to the type.
fn check_lambda_parameter_count(
    ctx: &mut Compiler,
    lambda: &AstLambdaExpr,
    ty: TypeId,
    to: TypeId,
    reported: usize,
) -> bool {
    let functions = rue_types::extract_functions(ctx.types_mut(), to, true);
    let found = lambda.parameters().count();

    let Some(expected) = expected_parameter_count(&functions) else {
        return false;
    };

    if expected == found
        || has_rest_parameter(lambda)
        || !matches!(ctx.try_is_assignable(ty, to), Ok(false))
    {
        return false;
    }

    // Where it's reported while the lambda is compiled, too
    let srcloc = parameter_count_site(ctx, lambda, expected);

    let already_reported = ctx.diagnostics_since(reported).iter().any(|diagnostic| {
        matches!(diagnostic.kind, DiagnosticKind::LambdaParameterCount(..))
            && diagnostic.srcloc.span == srcloc.span
    });

    if !already_reported {
        ctx.diagnostic_at(
            srcloc,
            DiagnosticKind::LambdaParameterCount(expected, found),
        );
    }

    true
}

/// Adds a note to the errors reported since there were `count` diagnostics, for an argument that
/// can't be assigned to its parameter's type, about each generic in that type that was inferred
/// as a union. The argument was only compared with what the generic was inferred as from all of
/// the arguments, so the error doesn't say which generic that is.
fn note_inferred_unions(
    ctx: &mut Compiler,
    count: usize,
    function: &FunctionType,
    param: TypeId,
    inferred: &HashMap<TypeId, TypeId>,
) {
    let mut notes = Vec::new();

    for &generic in &function.generics {
        let Some(&ty) = inferred.get(&generic) else {
            continue;
        };

        if !contains_generic(ctx.types_mut(), param, generic) {
            continue;
        }

        let semantic = rue_types::unwrap_semantic(ctx.types_mut(), ty, true);

        // A union of every value, which `Any` is, isn't one that the arguments were collected into
        let any = ctx.builtins().types.recursive_any;

        if !matches!(ctx.ty(semantic), Type::Union(_)) || ctx.is_assignable(any, ty) {
            continue;
        }

        let scope = ctx.last_scope_id();
        let name = ctx.type_name_in(scope, ty);

        // A union can be displayed as one type, such as `Bool`
        if !name.contains(" | ") {
            continue;
        }

        let generic_name = ctx.type_name_in(scope, generic);

        // In a call of a function by itself, the callee's generic is inferred as a type with the
        // caller's in it, which has the same name
        if names_type(&name, &generic_name) {
            continue;
        }

        notes.push(Note::new(format!(
            "`{generic_name}` is inferred as `{name}` from the arguments of this call"
        )));
    }

    for diagnostic in ctx.diagnostics_since_mut(count) {
        diagnostic.notes.extend(notes.iter().cloned());
    }
}

/// The name of the function being called, without any generic arguments.
fn callee_name(call: &AstFunctionCallExpr) -> String {
    match call.expr() {
        Some(AstExpr::PathExpr(path)) => path
            .segments()
            .filter_map(|segment| segment.name())
            .map(|name| name.text().to_string())
            .collect::<Vec<_>>()
            .join("::"),
        Some(expr) => expr.syntax().text().to_string(),
        None => String::new(),
    }
}

/// Generalizes the functions returned by a call over the generics of the function types expected
/// of them, and returns the generics that each is generalized over, from the outermost. This only
/// applies to the generics that no parameter of the callee, or of a function returned above it,
/// depends on, since the function works for any of them.
///
/// A generic that's inferred as one of the expected generics stays generic, as a copy so that it
/// doesn't capture anything. A generic that's inferred as a type that references them, such as
/// `(T, T)` for `B` in `let f: fn<T>(a: T) -> (T, T) = make();` with
/// `fn make<A, B>() -> fn(a: A) -> B`, references the copy of the first generic inferred as each
/// one instead, so it doesn't leak them (#340). `A` is inferred as `T`, so `B` is `(A, A)`. If no
/// generic is inferred as one of them, it's copied as well. A generic that a parameter depends on
/// is never inferred as a type that references them (see `infer_call`).
///
/// The same goes for the function types nested in the return types, at the same depth on each
/// side, such as `fn<U>(b: U) -> (T, U)` in `fn<T>(a: T) -> fn<U>(b: U) -> (T, U)` (#359).
fn generalize(
    ctx: &mut Compiler,
    function: &FunctionType,
    expected_type: TypeId,
    mappings: &mut HashMap<TypeId, TypeId>,
) -> Vec<Vec<TypeId>> {
    let mut levels = Vec::new();
    let mut returned = function.ret;
    let mut expected = vec![expected_type];

    // The generics that no parameter so far references. Those that one does aren't inferred as a
    // type that references the expected generics at the levels below it (see `infer_call`), so
    // leaving them out is only a backstop
    let mut free = unreferenced_by_params(ctx, &function.generics, function);

    // A generic only ever stops being free, and one that's still in what's returned eventually
    // turns up in a parameter or behind a return type that isn't a function, so the walk ends. The
    // limit is only a backstop
    while levels.len() < MAX_GENERALIZED_LEVELS
        && free
            .iter()
            .any(|&generic| contains_generic(ctx.types_mut(), returned, generic))
    {
        let unwrapped = rue_types::unwrap_semantic(ctx.types_mut(), returned, true);

        let Type::Function(returned_function) = ctx.ty(unwrapped).clone() else {
            break;
        };

        let expected_functions: Vec<FunctionType> = expected
            .iter()
            .flat_map(|&ty| rue_types::extract_functions(ctx.types_mut(), ty, false))
            .collect();

        if expected_functions.is_empty() {
            break;
        }

        let expected_generics: Vec<TypeId> = expected_functions
            .iter()
            .flat_map(|expected| expected.generics.iter().copied())
            .collect();

        levels.push(generalize_level(ctx, &free, &expected_generics, mappings));

        free = unreferenced_by_params(ctx, &free, &returned_function);
        returned = returned_function.ret;

        // Several of the expected functions can return the same type, such as a recursive alias
        // that's a union of them, which would otherwise be walked once for each at every level
        let mut seen = HashSet::new();
        expected = expected_functions
            .iter()
            .map(|expected| expected.ret)
            .filter(|&ret| seen.insert(ret))
            .collect();
    }

    while levels.last().is_some_and(Vec::is_empty) {
        levels.pop();
    }

    levels
}

/// How deeply nested the functions returned by a call can be generalized (see `generalize`).
pub(crate) const MAX_GENERALIZED_LEVELS: usize = 25;

/// The generics that none of the function's parameter types reference, in order.
fn unreferenced_by_params(
    ctx: &mut Compiler,
    generics: &[TypeId],
    function: &FunctionType,
) -> Vec<TypeId> {
    generics
        .iter()
        .copied()
        .filter(|&generic| {
            !function
                .param_types()
                .any(|ty| contains_generic(ctx.types_mut(), ty, generic))
        })
        .collect()
}

/// Generalizes one level of the functions returned by a call (see `generalize`), over the expected
/// generics at that level, for the `free` generics that no parameter above it references.
fn generalize_level(
    ctx: &mut Compiler,
    free: &[TypeId],
    expected_generics: &[TypeId],
    mappings: &mut HashMap<TypeId, TypeId>,
) -> Vec<TypeId> {
    let mut generalized = Vec::new();

    if expected_generics.is_empty() {
        return generalized;
    }

    // The copy that each of the expected generics is generalized as
    let mut copies = HashMap::new();
    let mut compound = Vec::new();

    for &generic in free {
        let Some(&ty) = mappings.get(&generic) else {
            continue;
        };

        if expected_generics.contains(&ty) {
            let copy = copy_generic(ctx, generic);
            mappings.insert(generic, copy);
            generalized.push(copy);
            copies.entry(ty).or_insert(copy);
        } else {
            compound.push((generic, ty));
        }
    }

    for (generic, ty) in compound {
        let mut references = false;

        for &other in expected_generics {
            if !contains_generic(ctx.types_mut(), ty, other) {
                continue;
            }

            references = true;

            copies.entry(other).or_insert_with(|| {
                let copy = copy_generic(ctx, other);
                generalized.push(copy);
                copy
            });
        }

        if references {
            let ty = substitute_with_mappings(ctx.types_mut(), ty, &copies);
            mappings.insert(generic, ty);
        }
    }

    generalized
}

/// Adds the generics that each level of the function type is generalized over (see `generalize`),
/// from the outermost.
fn add_generics(ctx: &mut Compiler, ty: TypeId, levels: &[Vec<TypeId>]) -> TypeId {
    let Some((generics, inner)) = levels.split_first() else {
        return ty;
    };

    let unwrapped = rue_types::unwrap_semantic(ctx.types_mut(), ty, true);

    let Type::Function(mut function) = ctx.ty(unwrapped).clone() else {
        return ty;
    };

    if !inner.is_empty() {
        function.ret = add_generics(ctx, function.ret, inner);
    }

    function.generics.extend(generics.iter().copied());
    ctx.alloc_type(Type::Function(function))
}

/// Renames the function's generics that are in scope to fresh copies of them, and returns the
/// generics that each copy was renamed from.
fn rename_generics_in_scope(
    ctx: &mut Compiler,
    function: &mut FunctionType,
) -> HashMap<TypeId, TypeId> {
    let mut renaming = HashMap::new();

    for generic in &mut function.generics {
        if is_type_in_scope(ctx, *generic) {
            let copy = copy_generic(ctx, *generic);
            renaming.insert(*generic, copy);
            *generic = copy;
        }
    }

    if renaming.is_empty() {
        return renaming;
    }

    for param in &mut function.params {
        param.ty = substitute_with_mappings(ctx.types_mut(), param.ty, &renaming);
    }

    function.ret = substitute_with_mappings(ctx.types_mut(), function.ret, &renaming);

    renaming
        .into_iter()
        .map(|(generic, copy)| (copy, generic))
        .collect()
}

/// A new generic like the given one, which can be bound separately from it.
fn copy_generic(ctx: &mut Compiler, generic: TypeId) -> TypeId {
    let copy = ctx.ty(generic).clone();
    ctx.alloc_type(copy)
}

pub(crate) fn is_type_in_scope(ctx: &Compiler, ty: TypeId) -> bool {
    let mut current = Some(ctx.last_scope_id());

    while let Some(scope) = current {
        if ctx.scope(scope).type_name(ty).is_some() {
            return true;
        }
        current = ctx.scope(scope).parent();
    }

    false
}
