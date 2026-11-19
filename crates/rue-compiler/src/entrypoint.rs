use rue_hir::{
    FunctionCall, FunctionKind, FunctionSymbol, Hir, HirId, ParameterSymbol, Symbol, SymbolId,
};

use crate::Compiler;

/// The function that code is generated for when the symbol is the program, such as `main` or an
/// export.
///
/// A function is the program itself. A constant isn't a function, so lowering it on its own
/// wouldn't bind the functions its value refers to; instead, it's wrapped in a function:
///
/// - If its type is a function, the program behaves like that function, called with the
///   program's arguments, just as if the function were `main`.
/// - Otherwise, the program takes no arguments and evaluates to the constant's value, just like a
///   `main` function with no parameters that returns it.
pub(crate) fn entrypoint(ctx: &mut Compiler, symbol: SymbolId) -> SymbolId {
    let Symbol::Constant(constant) = ctx.symbol(symbol).clone() else {
        return symbol;
    };

    let value = constant.value.hir;

    let mut functions = rue_types::extract_functions(ctx.types_mut(), constant.value.ty, true);

    if functions.len() != 1 {
        return synthetic_function(ctx, Vec::new(), true, value);
    }

    // A constant that names a function, or is a lambda, behaves like that function, so it can be
    // the program itself. A constant can't refer to anything local, so a lambda in it only
    // captures items, which the program binds like any function's.
    if let Hir::Reference(function) | Hir::Lambda(function) =
        ctx.hir(ctx.without_casts(value)).clone()
        && let Symbol::Function(_) = ctx.symbol(function)
    {
        return function;
    }

    let function_type = functions.remove(0);

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

    synthetic_function(ctx, parameters, function_type.nil_terminated, body)
}

/// A function with no name that isn't written in the source, which code is generated for in place
/// of something that isn't a function on its own.
pub(crate) fn synthetic_function(
    ctx: &mut Compiler,
    parameters: Vec<SymbolId>,
    nil_terminated: bool,
    body: HirId,
) -> SymbolId {
    let unresolved = ctx.builtins().unresolved.clone();
    let scope = ctx.builtins().scope;

    ctx.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: None,
        ty: unresolved.ty,
        scope,
        vars: Vec::new(),
        parameters,
        nil_terminated,
        return_type: unresolved.ty,
        body,
        kind: FunctionKind::Sequential,
    }))
}
