use rue_diagnostic::Name;
use rue_hir::{
    FunctionCall, FunctionKind, FunctionSymbol, Hir, HirId, ParameterSymbol, Symbol, SymbolId,
};
use rue_types::{Type, TypeId};

use crate::Compiler;

/// The function that code is generated for when the symbol is the program, such as `main` or an
/// export.
///
/// A function is the program itself. A constant isn't a function, so lowering it on its own
/// wouldn't bind the functions its value refers to; instead, it's wrapped in a function:
///
/// - If its type is a single function type, the program behaves like that function, called with
///   the program's arguments, just as if the function were `main`.
/// - Otherwise, the program takes no arguments and evaluates to the constant's value, just like a
///   `main` function with no parameters that returns it.
///
/// The wrapper is named after the constant, so that debuggers show it as the constant.
pub(crate) fn entrypoint(ctx: &mut Compiler, symbol: SymbolId) -> SymbolId {
    let Symbol::Constant(constant) = ctx.symbol(symbol) else {
        return symbol;
    };

    let name = constant.name.clone();
    let value = constant.value.hir;
    let ty = constant.value.ty;

    let functions = rue_types::extract_functions(ctx.types_mut(), ty, true);

    let Ok([function_type]) = <[_; 1]>::try_from(functions) else {
        let unresolved = ctx.builtins().unresolved.ty;
        return synthetic_function(ctx, name, unresolved, Vec::new(), true, value);
    };

    // A constant that names a function, or is a lambda, behaves like that function, so it can be
    // the program itself. A constant can't refer to anything local, so a lambda in it only
    // captures items, which the program binds like any function's.
    if let &Hir::Reference(function) | &Hir::Lambda(function) = ctx.hir(ctx.without_casts(value))
        && let Symbol::Function(_) = ctx.symbol(function)
    {
        return function;
    }

    let parameters: Vec<SymbolId> = function_type
        .params
        .iter()
        .map(|param| {
            ctx.alloc_symbol(Symbol::Parameter(ParameterSymbol {
                name: None,
                ty: param.ty,
            }))
        })
        .collect();

    let args = parameters
        .iter()
        .map(|&parameter| ctx.alloc_hir(Hir::Reference(parameter)))
        .collect();

    let body = ctx.alloc_hir(Hir::FunctionCall(FunctionCall {
        function: value,
        args,
        nil_terminated: function_type.nil_terminated,
    }));

    // The call is where the value is, so that it's where the program is when it runs
    if let Some(srcloc) = ctx.hir_srcloc(value).cloned() {
        ctx.set_hir_srcloc(body, srcloc);
    }

    // The wrapper has the function's type, which is where its parameters' names are kept
    let nil_terminated = function_type.nil_terminated;
    let ty = ctx.alloc_type(Type::Function(function_type));

    synthetic_function(ctx, name, ty, parameters, nil_terminated, body)
}

/// A function that isn't written in the source, which code is generated for in place of something
/// that isn't a function on its own.
pub(crate) fn synthetic_function(
    ctx: &mut Compiler,
    name: Option<Name>,
    ty: TypeId,
    parameters: Vec<SymbolId>,
    nil_terminated: bool,
    body: HirId,
) -> SymbolId {
    let unresolved = ctx.builtins().unresolved.ty;
    let scope = ctx.builtins().scope;

    ctx.alloc_symbol(Symbol::Function(FunctionSymbol {
        name,
        ty,
        scope,
        vars: Vec::new(),
        parameters,
        nil_terminated,
        return_type: unresolved,
        body,
        kind: FunctionKind::Sequential,
    }))
}
