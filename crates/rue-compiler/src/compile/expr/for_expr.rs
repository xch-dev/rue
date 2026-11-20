use log::debug;
use rue_ast::{AstBinding, AstForExpr, AstNode, AstStmtOrExpr};
use rue_diagnostic::Name;
use rue_hir::{
    BindingSymbol, Block, Declaration, ExprStatement, FunctionCall, FunctionKind, FunctionSymbol,
    Hir, ParameterSymbol, Statement, Symbol, SymbolId, UnaryOp, Value,
};
use rue_types::{FunctionParameter, FunctionType, Type, TypeId, Union};

use super::call_inference::{list_of, pair_parts};
use super::list::list_item_types;
use crate::{BlockKind, Compiler, compile_block, compile_expr, compile_type, create_binding};

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
///   result is the fold's value. The binding is evaluated where the body refers to it, if it only
///   does once, so a body that doesn't get to it stops there, as `x == item || found` does once an
///   item matches. A body that refers to it more than once has it evaluated before the body.
/// - With a filter, such as `if x > 0`, the items that don't pass it are left out of the list, or
///   skipped by the fold.
/// - If the body has no value, or the expression is a statement, whose value isn't used, the body
///   is only evaluated for each item, and the result is nil.
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

    let item_types = list_item_types(ctx, list.ty);
    let item_type = union_of(ctx, item_types);

    let fold = expr.fold();
    let has_value = expr
        .body()
        .is_some_and(|body| matches!(body.items().last(), Some(AstStmtOrExpr::Expr(_))));

    let kind = if fold.is_some() {
        ForKind::Fold
    } else if statement || !has_value {
        ForKind::Loop
    } else {
        ForKind::List
    };

    // The fold's value is compiled outside of the body, since it's the result without an item. Its
    // binding is the type it's annotated with, or what the expression is expected to be, since
    // that's what the body has to give too. Otherwise it's the type of the value, with a literal
    // as the type it's a value of, such as `Int` for `0`, since the body gives the next result
    let accumulator = fold.as_ref().map(|fold| {
        let annotation = fold.ty().map(|ty| compile_type(ctx, &ty));
        let expected = annotation.or(expected_type);

        let init = if let Some(init) = fold.init() {
            let value = compile_expr(ctx, &init, expected);

            if let Some(expected) = expected {
                ctx.assign_type(init.syntax(), value.ty, expected);
            }

            value
        } else {
            debug!("Unresolved fold value");
            ctx.unresolved()
        };

        let ty =
            expected.unwrap_or_else(|| rue_types::widen_literal(ctx.types_mut(), &types, init.ty));

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

    let scope = ctx.alloc_child_scope();
    ctx.push_scope(scope, range.start());

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

    let accumulator = accumulator.map(|(init, ty)| {
        let pattern = fold.as_ref().and_then(rue_ast::AstForFold::binding);

        let scope = ctx.alloc_child_scope();
        ctx.push_scope(scope, range.start());

        let rest = rest_call(ctx);
        let symbol = declare(
            ctx,
            pattern.as_ref(),
            Value::new(rest, ty),
            false,
            &mut statements,
        );

        (
            symbol,
            init,
            ty,
            matches!(pattern, Some(AstBinding::NamedBinding(_))),
        )
    });

    let expected_body = match kind {
        ForKind::List => expected_type
            .and_then(|ty| pair_parts(ctx, ty))
            .map(|(first, _)| first),
        ForKind::Fold => accumulator.as_ref().map(|&(_, _, ty, _)| ty),
        ForKind::Loop => None,
    };

    let body = if let Some(body) = expr.body() {
        let mappings = filter.as_ref().map(|filter| {
            ctx.push_mappings(filter.then_map.clone(), body.syntax().text_range().start())
        });

        let value = compile_block(ctx, &body, BlockKind::Expr, expected_body, false);

        if let Some(index) = mappings {
            ctx.revert_mappings(index, body.syntax().text_range().end());
        }

        if let Some(&(_, _, ty, _)) = accumulator.as_ref() {
            ctx.assign_type(body.syntax(), value.ty, ty);
        }

        value
    } else {
        debug!("Unresolved for body");
        ctx.unresolved()
    };

    // A fold's binding that the body refers to once is evaluated there, rather than before the
    // body, so that it's only evaluated if the body gets to it
    if let Some(&(symbol, _, _, named)) = accumulator.as_ref() {
        let end_hir = ctx.next_hir_id();
        let references = ctx
            .hirs_between(first_hir, end_hir)
            .filter(|hir| matches!(hir, Hir::Reference(reference) if *reference == symbol))
            .count();

        if named
            && references <= 1
            && let Symbol::Binding(binding) = ctx.symbol_mut(symbol)
        {
            binding.inline = true;
        }
    }

    if accumulator.is_some() {
        ctx.pop_scope(range.end());
    }

    ctx.pop_scope(range.end());

    // What the function does with an item: the body, and the call for the rest of the list
    let step = match kind {
        ForKind::List => {
            let rest = rest_call(ctx);
            ctx.alloc_hir(Hir::Pair(body.hir, rest))
        }
        ForKind::Fold => body.hir,
        ForKind::Loop => {
            let rest = rest_call(ctx);
            let always_nil = ctx.is_castable(body.ty, types.nil);
            let srcloc = ctx.trimmed_srcloc(expr.syntax());

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

    let (end, return_type) = match (kind, accumulator.as_ref()) {
        (ForKind::Fold, Some(&(_, init, ty, _))) => (init, ty),
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

/// A union of the types, with the same type only in it once.
fn union_of(ctx: &mut Compiler, types: Vec<TypeId>) -> TypeId {
    let mut members: Vec<TypeId> = Vec::new();

    for ty in types {
        if !members
            .iter()
            .any(|&member| ctx.is_assignable(ty, member) && ctx.is_assignable(member, ty))
        {
            members.push(ty);
        }
    }

    if members.len() == 1 {
        members[0]
    } else {
        ctx.alloc_type(Type::Union(Union::new(members)))
    }
}
