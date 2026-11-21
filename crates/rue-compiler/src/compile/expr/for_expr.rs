use log::debug;
use rue_ast::{AstBinding, AstForExpr, AstNode, AstStmtOrExpr};
use rue_diagnostic::Name;
use rue_hir::{
    BindingSymbol, Block, Declaration, ExprStatement, FunctionCall, FunctionKind, FunctionSymbol,
    Hir, HirId, ParameterSymbol, Statement, Symbol, SymbolId, UnaryOp, Value,
};
use rue_types::{FunctionParameter, FunctionType, Pair, Type, TypeId};

use super::call_inference::{list_of, pair_parts, references_call_generics};
use super::list::{list_item_types, union_of};
use crate::{BlockKind, Compiler, compile_block, compile_expr, compile_type, create_binding};

/// The result of a fold for the rest of the list.
struct Accumulator {
    /// The binding for it in the body.
    symbol: SymbolId,
    /// The result at the end of the list.
    init: HirId,
    ty: TypeId,
}

/// What a `for` expression makes of the items of its list.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ForKind {
    /// A list of the body's value for each item, such as `for x in xs { x + 1 }`.
    List,
    /// A single value, which the body gives from each item and the value for the rest of the list,
    /// such as `for x in xs fold sum = 0 { sum + x }`.
    Fold,
    /// Nothing: the body is only evaluated for each item, such as `for x in xs { assert x > 0; }`.
    Loop,
}

/// Compiles a `for` expression, which goes through the items of a list.
///
/// It's compiled to a call of a function of its own, which is only in the HIR: the function takes
/// what's left of the list, and if there's an item, evaluates the body for it and calls itself
/// with the rest. The body refers to the values around it as any expression there does, so they're
/// captured by the function, which passes them on to itself (see [`FunctionKind::Loop`]).
///
/// - Without a fold, the result is a list of the body's value for each item, which is built in
///   front of the list for the rest of the items.
/// - With a fold, such as `fold sum = 0`, the body gives the result from the item and the result
///   for the rest of the list, which is what the fold's binding is. At the end of the list, the
///   result is the fold's value. The binding is evaluated where the body refers to it, if that's
///   in one place that's evaluated at most once for the item (see [`is_lazy`]), so a body that
///   doesn't get to it stops there, as `x == item || found` does once an item matches. Otherwise
///   it's evaluated once, before the body.
/// - With a filter, such as `if x > 0`, the items that don't pass it are left out of the list, or
///   skipped by the fold.
/// - Without a fold, if the body has no value, or the expression is a statement, whose value
///   isn't used, the body is only evaluated for each item, and the result is nil. So it is if the
///   body's value is nil, unless the expression is expected to be a list.
pub fn compile_for_expr(
    ctx: &mut Compiler,
    expr: &AstForExpr,
    expected_type: Option<TypeId>,
    statement: bool,
) -> Value {
    let types = ctx.builtins().types;
    let range = expr.syntax().text_range();

    let list = if let Some(list) = expr.list() {
        let value = compile_expr(ctx, &list, None);
        let any_list = list_of(ctx, types.permissive_any);
        ctx.assign_type(list.syntax(), value.ty, any_list);
        value
    } else {
        debug!("Unresolved for list");
        ctx.unresolved()
    };

    // A list with no items, such as nil, has no item to give a type
    let item_types = list_item_types(ctx, list.ty);
    let item_type = if ctx.is_unresolved(list.ty) {
        // A list that isn't there, or that couldn't be compiled, doesn't say what the item is
        list.ty
    } else if item_types.is_empty() {
        types.never
    } else {
        union_of(ctx, item_types)
    };

    let fold = expr.fold();
    let has_value = expr
        .body()
        .is_some_and(|body| matches!(body.items().last(), Some(AstStmtOrExpr::Expr(_))));

    // The fold's value is compiled outside of the body, since it's the result without an item. Its
    // binding is the type it's annotated with. Otherwise it's the type of the value, with literals
    // as the types they're values of, such as `Int` for `0`, since the body gives the next result.
    // A value that's nil doesn't say what the result is, so then it's the type the expression is
    // expected to be, if there is one, as with `fold rest = nil` for a list
    let accumulator = fold.as_ref().map(|fold| {
        let annotation = fold.ty().map(|ty| compile_type(ctx, &ty));

        let init = if let Some(init) = fold.init() {
            let value = compile_expr(ctx, &init, annotation.or(expected_type));

            if let Some(annotation) = annotation {
                ctx.assign_type(init.syntax(), value.ty, annotation);
            }

            value
        } else {
            debug!("Unresolved fold value");
            ctx.unresolved()
        };

        let ty = annotation.unwrap_or_else(|| match expected_type {
            // A value that isn't there, or that couldn't be compiled, doesn't say what the
            // result is
            _ if ctx.is_unresolved(init.ty) => init.ty,
            // Not a type that's only a hint, or that still has the generics of a call being
            // inferred, which the fold can't be: a parameter's type is expected of its argument
            // as it is while no other argument has given its generics a type
            Some(expected)
                if ctx.is_assignable(init.ty, types.nil)
                    && ctx.is_assignable(init.ty, expected)
                    && !ctx.is_expected_type_hint()
                    && !references_call_generics(ctx, expected) =>
            {
                expected
            }
            _ => widen_literals(ctx, init.ty),
        });

        (init.hir, ty)
    });

    let function = ctx.alloc_symbol(Symbol::Unresolved);

    let parameter = ctx.alloc_symbol(Symbol::Parameter(ParameterSymbol {
        name: None,
        ty: list.ty,
    }));

    let first_hir = ctx.next_hir_id();

    // Each use of what's left of the list is a node of its own
    let rest_call = |ctx: &mut Compiler| {
        let reference = ctx.alloc_hir(Hir::Reference(parameter));
        let rest = ctx.alloc_hir(Hir::Unary(UnaryOp::Rest, reference));
        let callee = ctx.alloc_hir(Hir::Reference(function));

        ctx.alloc_hir(Hir::FunctionCall(FunctionCall {
            function: callee,
            args: vec![rest],
            nil_terminated: true,
        }))
    };

    let body_start = expr
        .body()
        .map_or(range.end(), |body| body.syntax().text_range().start());

    // The item is in scope after the list, in the filter and the body. It isn't in scope in the
    // fold's value, which is between them, so its scope ends before that. The body sees it from
    // the scopes of the fold and of its block, which are inside of this one
    let item_end = fold
        .as_ref()
        .map_or(body_start, |fold| fold.syntax().text_range().start());

    let item_start = expr
        .list()
        .map_or(item_end, |list| list.syntax().text_range().end());

    let scope = ctx.alloc_child_scope();
    ctx.push_scope(scope, item_start);

    let mut statements = Vec::new();

    // The item is the first of what's left of the list, which is cheap to get wherever it's used
    let reference = ctx.alloc_hir(Hir::Reference(parameter));
    let first = ctx.alloc_hir(Hir::Unary(UnaryOp::First, reference));
    declare(
        ctx,
        expr.binding().as_ref(),
        Value::new(first, item_type),
        true,
        &mut statements,
    );

    let filter = expr.filter().map(|filter| {
        if let Some(condition) = filter.condition() {
            let value = compile_expr(ctx, &condition, None);
            ctx.check_condition(condition.syntax(), value.ty);
            value
        } else {
            debug!("Unresolved for filter");
            ctx.unresolved()
        }
    });

    // The result for the rest of the list is only in scope in the body. Its let statement is
    // inside the filter, so that an item that's skipped doesn't evaluate it
    let mut accumulator_statements = Vec::new();

    // Only the range up to here is recorded for the item's scope, which stays open for the body
    ctx.pop_scope(item_end);
    ctx.push_scope(scope, body_start);

    let accumulator = accumulator.map(|(init, ty)| {
        let pattern = fold.as_ref().and_then(rue_ast::AstForFold::binding);

        let scope = ctx.alloc_child_scope();
        ctx.push_scope(scope, body_start);

        let rest = rest_call(ctx);
        let symbol = declare(
            ctx,
            pattern.as_ref(),
            Value::new(rest, ty),
            false,
            &mut accumulator_statements,
        );

        Accumulator { symbol, init, ty }
    });

    // The type of an item of the list that the expression is expected to be, if it's one
    let expected_item = expected_type
        .and_then(|ty| pair_parts(ctx, ty))
        .map(|(first, _)| first);

    // A list is a type that's still itself with another item in front of it, unlike a pair or a
    // struct that can also be nil
    let expects_list = match (expected_type, expected_item) {
        (Some(expected), Some(item)) => {
            let longer = ctx.alloc_type(Type::Pair(Pair::new(item, expected)));
            ctx.is_assignable(longer, expected)
        }
        _ => false,
    };

    let expected_body = match &accumulator {
        Some(accumulator) => Some(accumulator.ty),
        None => expected_item,
    };

    let body = if let Some(body) = expr.body() {
        let mappings = filter.as_ref().map(|filter| {
            ctx.push_mappings(filter.then_map.clone(), body.syntax().text_range().start())
        });

        let value = compile_block(ctx, &body, BlockKind::Expr, expected_body, false);

        if let Some(index) = mappings {
            ctx.revert_mappings(index, body.syntax().text_range().end());
        }

        if let Some(accumulator) = &accumulator {
            ctx.assign_type(body.syntax(), value.ty, accumulator.ty);
        }

        value
    } else {
        debug!("Unresolved for body");
        ctx.unresolved()
    };

    if let Some(accumulator) = &accumulator
        && is_lazy(ctx, accumulator.symbol, body.hir, first_hir)
        && let Symbol::Binding(binding) = ctx.symbol_mut(accumulator.symbol)
    {
        binding.inline = true;
    }

    if accumulator.is_some() {
        ctx.pop_scope(range.end());
    }

    ctx.pop_scope(range.end());

    // A body whose value is nil has nothing to make a list of, as with a call to a function that
    // returns nothing, or another loop, so it's a loop too, unless the expression is expected to
    // be a list, which it's then a list of nils for
    let kind = if fold.is_some() {
        ForKind::Fold
    } else if statement || !has_value || (!expects_list && ctx.is_assignable(body.ty, types.nil)) {
        ForKind::Loop
    } else {
        ForKind::List
    };

    // What the function does with an item: the body, and the call for the rest of the list
    let step = match kind {
        ForKind::List => {
            let rest = rest_call(ctx);
            ctx.alloc_hir(Hir::Pair(body.hir, rest))
        }
        ForKind::Fold => ctx.alloc_hir(Hir::Block(Block {
            statements: accumulator_statements,
            body: Some(body.hir),
        })),
        ForKind::Loop => {
            let rest = rest_call(ctx);
            let always_nil = ctx.is_castable(body.ty, types.nil);

            // A value that the body ends with isn't used, which is reported there
            let value = expr.body().and_then(|body| match body.items().last() {
                Some(AstStmtOrExpr::Expr(value)) => Some(value),
                _ => None,
            });
            let srcloc = match &value {
                Some(value) => ctx.trimmed_srcloc(value.syntax()),
                None => ctx.trimmed_srcloc(expr.syntax()),
            };

            ctx.alloc_hir(Hir::Block(Block {
                statements: vec![Statement::Expr(ExprStatement {
                    hir: body.hir,
                    always_nil,
                    srcloc,
                })],
                body: Some(rest),
            }))
        }
    };

    // An item that doesn't pass the filter is skipped, which leaves the result for the rest
    let step = if let Some(filter) = &filter {
        let skip = rest_call(ctx);
        ctx.alloc_hir(Hir::If(filter.hir, step, skip, false))
    } else {
        step
    };

    let step = ctx.alloc_hir(Hir::Block(Block {
        statements,
        body: Some(step),
    }));

    let (end, return_type) = match (kind, &accumulator) {
        (ForKind::Fold, Some(accumulator)) => (accumulator.init, accumulator.ty),
        (ForKind::List, _) => (ctx.builtins().nil.hir, list_of(ctx, body.ty)),
        _ => (ctx.builtins().nil.hir, types.nil),
    };

    // What's left of the list is a pair unless it's nil, which is the end of it
    let condition = ctx.alloc_hir(Hir::Reference(parameter));
    let function_body = ctx.alloc_hir(Hir::If(condition, step, end, false));

    let ty = ctx.alloc_type(Type::Function(FunctionType {
        generics: Vec::new(),
        params: vec![FunctionParameter::new("list", list.ty)],
        nil_terminated: true,
        ret: return_type,
    }));

    *ctx.symbol_mut(function) = Symbol::Function(FunctionSymbol {
        name: Some(Name::new("for", None)),
        ty,
        scope,
        vars: Vec::new(),
        parameters: vec![parameter],
        nil_terminated: true,
        return_type,
        body: function_body,
        kind: FunctionKind::Loop,
    });

    let callee = ctx.alloc_hir(Hir::Reference(function));
    let hir = ctx.alloc_hir(Hir::FunctionCall(FunctionCall {
        function: callee,
        args: vec![list.hir],
        nil_terminated: true,
    }));

    Value::new(hir, return_type)
}

/// Declares the binding of a pattern for the value, with a let statement for it in the body of the
/// function, as a `let` statement does.
fn declare(
    ctx: &mut Compiler,
    pattern: Option<&AstBinding>,
    value: Value,
    inline: bool,
    statements: &mut Vec<Statement>,
) -> SymbolId {
    let symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
        name: None,
        value,
        inline,
    }));

    ctx.push_declaration(Declaration::Symbol(symbol));

    if let Some(pattern) = pattern {
        create_binding(ctx, symbol, pattern);
        statements.push(Statement::Let(symbol, ctx.trimmed_srcloc(pattern.syntax())));
    }

    ctx.pop_declaration();

    symbol
}

/// Whether the binding for the result of the rest of the list is evaluated where the body refers
/// to it, rather than once before the body. That's only if the body refers to it in one place,
/// which is evaluated at most once for the item. So it's not in a lambda or another `for`, which
/// can be evaluated any number of times, nor in a type guard that uses its value more than once.
/// It's not in the value of a `let` or an argument of a call either, since a function that's
/// inlined evaluates its argument wherever it refers to its parameter, and a `let` can be
/// substituted into one. Otherwise each of those would go through the rest of the list again, for
/// every item. A binding that isn't evaluated there is never substituted (see
/// `Database::is_loop_rest_call`).
fn is_lazy(ctx: &Compiler, symbol: SymbolId, body: HirId, first_hir: HirId) -> bool {
    let references = ctx
        .hirs_between(first_hir, ctx.next_hir_id())
        .filter(|hir| matches!(hir, Hir::Reference(reference) if *reference == symbol))
        .count();

    // A struct that's updated from the binding reuses the rest of it with one reference, where a
    // struct that's rebuilt refers to it for each field, and so evaluates it before the body.
    // The update does the same, so that whether the rest of the list is gone through when a
    // branch doesn't use the binding doesn't depend on which of the two is written
    let copied = ctx
        .hirs_between(first_hir, ctx.next_hir_id())
        .any(|hir| match hir {
            Hir::Struct(initializer) => initializer
                .tail
                .is_some_and(|tail| tail.reads > 1 && ctx.path_symbol(tail.value) == Some(symbol)),
            _ => false,
        });

    references <= 1 && !copied && direct_uses(ctx, symbol, body, 1) == references
}

/// How many times the function that the node is in evaluates a reference to the symbol in the
/// node at most, where each evaluation of the node counts as `uses`. A reference in a lambda or
/// another `for` isn't in the function, so it isn't counted.
fn direct_uses(ctx: &Compiler, symbol: SymbolId, hir: HirId, uses: usize) -> usize {
    rue_types::grow_stack(|| match ctx.hir(hir) {
        Hir::Reference(reference) if *reference == symbol => uses,
        Hir::Guard(guard) => {
            direct_uses(ctx, symbol, guard.value, uses * guard.value_uses().max(1))
        }
        Hir::Block(block) => {
            let statements: usize = block
                .statements
                .iter()
                .map(|statement| match statement {
                    // The value is evaluated wherever the binding is substituted
                    Statement::Let(binding, _) => match ctx.symbol(*binding) {
                        Symbol::Binding(binding) => {
                            direct_uses(ctx, symbol, binding.value.hir, uses * 2)
                        }
                        _ => 0,
                    },
                    statement => statement
                        .hirs()
                        .map(|hir| direct_uses(ctx, symbol, hir, uses))
                        .sum(),
                })
                .sum();

            statements
                + block
                    .body
                    .map_or(0, |body| direct_uses(ctx, symbol, body, uses))
        }
        // An argument is evaluated wherever a function that's inlined refers to its parameter
        Hir::FunctionCall(call) => {
            direct_uses(ctx, symbol, call.function, uses)
                + call
                    .args
                    .iter()
                    .map(|&arg| direct_uses(ctx, symbol, arg, uses * 2))
                    .sum::<usize>()
        }
        node => node
            .children()
            .map(|child| direct_uses(ctx, symbol, child, uses))
            .sum(),
    })
}

/// The type with its literals as the types they're values of, such as `Int` for `0`, in the pairs
/// it's made of too, such as `(Int, Int)` for `(0, 0)`.
fn widen_literals(ctx: &mut Compiler, ty: TypeId) -> TypeId {
    let types = ctx.builtins().types;

    match ctx.ty(ty).clone() {
        Type::Pair(pair) => {
            let first = widen_literals(ctx, pair.first);
            let rest = widen_literals(ctx, pair.rest);
            ctx.alloc_type(Type::Pair(Pair::new(first, rest)))
        }
        _ => rue_types::widen_literal(ctx.types_mut(), &types, ty),
    }
}
