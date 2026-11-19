use std::collections::HashMap;

use log::debug;
use rue_ast::{AstExpr, AstLambdaExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{FunctionKind, FunctionSymbol, Hir, ParameterSymbol, Symbol, Value};
use rue_types::{
    FunctionParameter, FunctionType, Generic, Type, TypeId, Union, contains_generic,
    substitute_with_mappings,
};

use crate::{
    BlockKind, Compiler, CompletionContext, ReturnTarget, SyntaxItemKind, compile_block,
    compile_expr, compile_generic_parameters, compile_type, create_binding,
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
    /// (`mappings`, or the generic itself if it isn't mapped). Each of those generics is a reference
    /// of its own in them instead, so a part of a parameter, such as `p.first` with `p: (A, Int)`,
    /// still has it.
    Generics {
        params: &'a [TypeId],
        generics: &'a [TypeId],
        mappings: &'a HashMap<TypeId, TypeId>,
    },
}

/// Compiles a lambda, with its untyped parameters wrapped as `wrap` says, if it's given. It returns
/// the references and the types they refer to.
pub(crate) fn compile_lambda(
    ctx: &mut Compiler,
    expr: &AstLambdaExpr,
    expected_type: Option<TypeId>,
    wrap: Option<&ParameterWrap<'_>>,
) -> (Value, Vec<(TypeId, TypeId)>) {
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

    for (i, param) in expr.parameters().enumerate() {
        let is_spread = if let Some(spread) = param.spread() {
            if i == len - 1 {
                true
            } else {
                ctx.diagnostic(&spread, DiagnosticKind::NonFinalSpread);
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
            ctx.diagnostic(param.syntax(), DiagnosticKind::CannotInferParameterType);
            ctx.builtins().unresolved.ty
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

        if let Some(binding) = param.binding() {
            create_binding(ctx, symbol, &binding);
        }

        let name = param
            .binding()
            .map_or(String::new(), |binding| binding.syntax().text().to_string());

        parameters.push(symbol);
        params.push(FunctionParameter::new(name, ty));
    }

    make_references(ctx, &references, &wrapped);

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
        let result = ctx.with_expected_type_hint(hint, |ctx| {
            if let AstExpr::Block(block) = &body {
                ctx.add_syntax(
                    SyntaxItemKind::CompletionContext(CompletionContext::Expression),
                    block.syntax().text_range(),
                );
                compile_block(ctx, block, BlockKind::Body, body_type, true)
            } else {
                compile_expr(ctx, &body, body_type)
            }
        });
        if let Some(return_type) = return_type {
            ctx.assign_type(expr.syntax(), result.ty, return_type);
        }
        result
    } else {
        debug!("Unresolved lambda body");
        ctx.builtins().unresolved.clone()
    };

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

    (Value::new(hir, ty), wrapped)
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

    for &generic in *generics {
        if !contains_generic(ctx.types_mut(), source, generic) {
            continue;
        }

        // It's a generic until every parameter is substituted with it (see `make_references`),
        // since a mapped type is substituted itself, which would see through a reference
        let reference = *references.entry(generic).or_insert_with(|| {
            let target = mappings.get(&generic).copied().unwrap_or(generic);
            let reference = ctx.alloc_type(Type::Generic(Generic { name: None }));
            wrapped.push((reference, target));
            reference
        });

        substitution.insert(generic, reference);
    }

    if substitution.is_empty() {
        return ty;
    }

    substitute_with_mappings(ctx.types_mut(), source, &substitution)
}

/// Turns the placeholders that `wrap_parameter` substituted for the callee's generics into the
/// references they stand for, once every parameter has been substituted with them.
fn make_references(
    ctx: &mut Compiler,
    references: &HashMap<TypeId, TypeId>,
    wrapped: &[(TypeId, TypeId)],
) {
    for &(reference, target) in wrapped {
        if references
            .values()
            .any(|&placeholder| placeholder == reference)
        {
            *ctx.ty_mut(reference) = Type::Ref(target);
            ctx.report_type_as(reference, target);
        }
    }
}
