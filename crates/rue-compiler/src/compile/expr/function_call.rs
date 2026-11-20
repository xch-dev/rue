use std::collections::{HashMap, HashSet};

use log::debug;
use rue_ast::{AstExpr, AstFunctionCallExpr, AstNode};
use rue_diagnostic::{DiagnosticKind, SpreadTarget};
use rue_hir::{FunctionCall, Hir, Symbol, Value};
use rue_types::{FunctionType, Type, TypeId, Union, contains_generic, substitute_with_mappings};

use super::builtin::compile_builtin;
use super::call_inference::{InferredCall, infer_call};
use crate::{Compiler, CompletionContext, SyntaxItemKind, compile_expr, compile_path_value};

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

            // An argument that it would have been inferred from has already failed to compile
            let from_unresolved_argument = checks.iter().any(|&(_, ty, param)| {
                ctx.is_unresolved(ty) && contains_generic(ctx.types_mut(), param, generic)
            });

            if !from_unresolved_argument {
                let generic_name = ctx.type_name(generic);
                let function_name = callee_name(call);

                ctx.diagnostic(
                    call.syntax(),
                    DiagnosticKind::CannotInferGeneric(generic_name, function_name),
                );
            }

            let unresolved = ctx.unresolved().ty;
            mappings.insert(generic, unresolved);
            uninferred.insert(generic, unresolved);
        }

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
            let param = substitute_with_mappings(ctx.types_mut(), param, &mappings);
            ctx.assign_type(expr.syntax(), ty, param);
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
