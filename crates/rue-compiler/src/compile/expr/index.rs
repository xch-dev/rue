use std::collections::HashSet;

use log::debug;
use num_bigint::{BigInt, Sign};
use rue_ast::{AstExpr, AstIndexExpr, AstNode, AstSliceExpr};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{
    BinaryOp, BindingSymbol, Block, FieldKind, Hir, HirId, Statement, Symbol, SymbolId, UnaryOp,
    Value,
};

use crate::{Compiler, compile_expr};

/// An index (`value[index]`) whose index isn't a literal, which might still be a constant.
#[derive(Debug, Clone, Copy)]
pub(crate) struct PendingIndex {
    /// The node that the index expression compiled to.
    hir: HirId,
    value: HirId,
    index: HirId,
}

/// An index (`value[index]`) is the byte at the index, as bytes of length one, which is what
/// `substr(value, index, index + 1)` gives.
pub fn compile_index_expr(ctx: &mut Compiler, expr: &AstIndexExpr) -> Value {
    let value = compile_indexed_value(ctx, expr.expr());
    let index = expr
        .index()
        .map(|index| (compile_bound(ctx, &index), index));
    let bytes = ctx.builtins().types.bytes;

    let (Some(value), Some((start, index))) = (value, index) else {
        debug!("Unresolved index expr");
        return Value::new(ctx.unresolved().hir, bytes);
    };

    let is_literal = int_value(ctx, start, None).is_some();

    let hir = if let Some(end) = next_index(ctx, start) {
        alloc_substr(ctx, value, start, Some(end))
    } else {
        alloc_bound_index(ctx, value, start, &index)
    };

    if !is_literal {
        ctx.push_pending_index(PendingIndex {
            hir,
            value,
            index: start,
        });
    }

    Value::new(hir, bytes)
}

/// Binds the index, which is needed twice, rather than evaluating it twice, like the value that a
/// `match` checks: `{ let index = ...; substr(value, index, index + 1) }`.
fn alloc_bound_index(ctx: &mut Compiler, value: HirId, start: HirId, index: &AstExpr) -> HirId {
    let index_value = Value::new(start, ctx.builtins().types.int);
    let symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
        name: None,
        value: index_value,
        inline: false,
    }));
    let srcloc = ctx.trimmed_srcloc(index.syntax());

    let start = ctx.alloc_hir(Hir::Reference(symbol));
    let one = ctx.alloc_hir(Hir::Int(BigInt::from(1)));
    let end = ctx.alloc_hir(Hir::Binary(BinaryOp::Add, start, one));
    let body = alloc_substr(ctx, value, start, Some(end));

    ctx.alloc_hir(Hir::Block(Block {
        statements: vec![Statement::Let(symbol, srcloc)],
        body: Some(body),
    }))
}

/// Gives each index that turned out to be a constant, such as `value[N]` or `value[N + 1]`, the
/// index after it as a literal, like a literal index has, rather than adding one to it at
/// runtime. This is done once everything is compiled, since a constant can be declared after the
/// code that uses it, and after the const expressions are evaluated, since it can be one.
pub(crate) fn fold_constant_indexes(ctx: &mut Compiler) {
    for pending in ctx.take_pending_indexes() {
        let Some(index) = int_value(ctx, pending.index, Some(&mut HashSet::new())) else {
            continue;
        };

        let end = ctx.alloc_hir(Hir::Int(index + 1));
        *ctx.hir_mut(pending.hir) = Hir::Substr(pending.value, pending.index, Some(end));
        ctx.set_hir_indexed(pending.hir);
    }
}

/// The value of an integer expression that's made of literals with `+` and `-`, and of constants
/// and const expressions if `constants` is given, which is the constants being followed. The
/// expression is known to be an `Int`, so the result of a const expression, which is an atom, is
/// its integer value.
fn int_value(
    ctx: &Compiler,
    hir: HirId,
    mut constants: Option<&mut HashSet<SymbolId>>,
) -> Option<BigInt> {
    match ctx.hir(ctx.without_casts(hir)) {
        Hir::Int(value) => Some(value.clone()),
        Hir::Const(expr) if constants.is_some() => match ctx.hir(expr.result?) {
            Hir::Bytes(value) => Some(BigInt::from_signed_bytes_be(value)),
            _ => None,
        },
        Hir::Reference(symbol) => {
            let constants = constants?;

            let Symbol::Constant(constant) = ctx.symbol(*symbol) else {
                return None;
            };

            // A constant that refers to itself is an error
            if !constants.insert(*symbol) {
                return None;
            }

            let value = int_value(ctx, constant.value.hir, Some(constants));
            constants.remove(symbol);
            value
        }
        Hir::Unary(UnaryOp::Plus, value) => int_value(ctx, *value, constants),
        Hir::Unary(UnaryOp::Neg, value) => Some(-int_value(ctx, *value, constants)?),
        Hir::Binary(op @ (BinaryOp::Add | BinaryOp::Sub), left, right) => {
            let left = int_value(ctx, *left, constants.as_deref_mut())?;
            let right = int_value(ctx, *right, constants)?;

            Some(if *op == BinaryOp::Add {
                left + right
            } else {
                left - right
            })
        }
        _ => None,
    }
}

/// A slice (`value[start..end]`) is the bytes from `start` up to `end`, which is what
/// `substr(value, start, end)` gives. Without `start` it's from the first byte, and without `end`
/// it's up to the last one.
pub fn compile_slice_expr(ctx: &mut Compiler, expr: &AstSliceExpr) -> Value {
    let value = compile_indexed_value(ctx, expr.expr());
    let start = expr.start().map(|start| compile_bound(ctx, &start));
    let end = expr.end().map(|end| compile_bound(ctx, &end));
    let bytes = ctx.builtins().types.bytes;

    let Some(value) = value else {
        debug!("Unresolved slice expr");
        return Value::new(ctx.unresolved().hir, bytes);
    };

    // There's no `substr` call for all of the bytes, which would only check that the value is an
    // atom
    if start.is_none() && end.is_none() {
        // Unless a bound failed to parse, which has been reported
        if let Some(range) = expr.range()
            && expr.is_complete()
        {
            ctx.diagnostic(&range, DiagnosticKind::SliceWithoutBounds);
        }
        return Value::new(ctx.unresolved().hir, bytes);
    }

    let start = start.unwrap_or_else(|| ctx.alloc_hir(Hir::Int(BigInt::ZERO)));

    Value::new(alloc_substr(ctx, value, start, end), bytes)
}

/// Compiles the value that's indexed or sliced, which is anything `substr` takes. A list isn't,
/// since finding an item takes a walk through the list, which an index would hide the cost of.
fn compile_indexed_value(ctx: &mut Compiler, expr: Option<AstExpr>) -> Option<HirId> {
    let expr = expr?;
    let value = compile_expr(ctx, &expr, None);
    let bytes = ctx.builtins().types.bytes;

    if !ctx.is_unresolved(value.ty)
        && !ctx.is_assignable(value.ty, bytes)
        && !rue_types::extract_pairs(ctx.types_mut(), value.ty, false).is_empty()
    {
        let name = ctx.type_name(value.ty);
        ctx.diagnostic(expr.syntax(), DiagnosticKind::CannotIndexPair(name));
    } else {
        ctx.assign_type(expr.syntax(), value.ty, bytes);
    }

    Some(value.hir)
}

fn compile_bound(ctx: &mut Compiler, expr: &AstExpr) -> HirId {
    let value = compile_expr(ctx, expr, None);
    ctx.assign_type(expr.syntax(), value.ty, ctx.builtins().types.int);
    value.hir
}

/// Indexing and slicing are `substr` calls, which are marked as written with brackets, since the
/// value is in a different position in the code than an argument is.
fn alloc_substr(ctx: &mut Compiler, value: HirId, start: HirId, end: Option<HirId>) -> HirId {
    let hir = ctx.alloc_hir(Hir::Substr(value, start, end));
    ctx.set_hir_indexed(hir);
    hir
}

/// The index after this one, if it's as cheap to compute from the index as it would be to bind
/// the index and add one to it: a literal, or a binding or a field of one, with or without a
/// literal added to or subtracted from it. This is what `end` would be in the `substr(value,
/// index, end)` call that the index would otherwise be written as, such as `substr(value, 2, 3)`,
/// `substr(value, i + 1, i + 2)` or `substr(value, i - 1, i)`.
fn next_index(ctx: &mut Compiler, index: HirId) -> Option<HirId> {
    if let Some(value) = int_value(ctx, index, None) {
        return Some(ctx.alloc_hir(Hir::Int(value + 1)));
    }

    let (place, offset) = match *ctx.hir(ctx.without_casts(index)) {
        Hir::Binary(BinaryOp::Add, left, right) if is_place(ctx, left) => {
            (left, int_value(ctx, right, None)?)
        }
        Hir::Binary(BinaryOp::Add, left, right) if is_place(ctx, right) => {
            (right, int_value(ctx, left, None)?)
        }
        Hir::Binary(BinaryOp::Sub, left, right) if is_place(ctx, left) => {
            (left, -int_value(ctx, right, None)?)
        }
        _ if is_place(ctx, index) => (index, BigInt::ZERO),
        _ => return None,
    };

    let offset: BigInt = offset + 1;

    Some(match offset.sign() {
        Sign::NoSign => place,
        Sign::Plus => {
            let offset = ctx.alloc_hir(Hir::Int(offset));
            ctx.alloc_hir(Hir::Binary(BinaryOp::Add, place, offset))
        }
        Sign::Minus => {
            let offset = ctx.alloc_hir(Hir::Int(-offset));
            ctx.alloc_hir(Hir::Binary(BinaryOp::Sub, place, offset))
        }
    })
}

/// Whether the value is a binding, parameter or constant, or a field that's at a path from one,
/// which is found without computing anything.
fn is_place(ctx: &Compiler, hir: HirId) -> bool {
    match ctx.hir(ctx.without_casts(hir)) {
        Hir::Reference(_) => true,
        Hir::FieldAccess(access) => {
            matches!(access.kind, FieldKind::Path(_)) && is_place(ctx, access.value)
        }
        _ => false,
    }
}
