use std::collections::HashSet;

use log::debug;
use num_bigint::BigInt;
use rue_ast::{AstExpr, AstIndexExpr, AstNode, AstSliceExpr};
use rue_diagnostic::{DiagnosticKind, SrcLoc};
use rue_hir::{
    BinaryOp, BindingSymbol, Block, ConstExpr, FieldKind, Hir, HirId, Statement, Symbol, UnaryOp,
    Value,
};

use rue_types::{AtomSemantic, Type, TypeId};

use crate::{Compiler, compile_expr};

/// An index (`value[index]`) is the byte at the index, as bytes of length one: the CLVM operator
/// `substr` from `index` up to `index + 1`.
pub fn compile_index_expr(ctx: &mut Compiler, expr: &AstIndexExpr) -> Value {
    let value = compile_indexed_value(ctx, expr.expr());
    let index = expr
        .index()
        .map(|index| (compile_bound(ctx, &index), index));
    let bytes = ctx.builtins().types.bytes;

    let (Some((value, ty)), Some((start, index))) = (value, index) else {
        debug!("Unresolved index expr");
        return Value::new(ctx.unresolved().hir, bytes);
    };

    let srcloc = ctx.trimmed_srcloc(index.syntax());

    let hir = if let Some(end) = next_index(ctx, start, &srcloc) {
        ctx.alloc_hir(Hir::Substr(value, start, Some(end)))
    } else {
        alloc_bound_index(ctx, value, start, srcloc)
    };

    Value::new(hir, ty)
}

/// A slice (`value[start..end]`) is the bytes from `start` up to `end`, which is the CLVM
/// operator `substr`. Without `start` it's from the first byte, and without `end` it's up to the
/// last one.
pub fn compile_slice_expr(ctx: &mut Compiler, expr: &AstSliceExpr) -> Value {
    let value = compile_indexed_value(ctx, expr.expr());
    let start = expr.start().map(|start| compile_bound(ctx, &start));
    let end = expr.end().map(|end| compile_bound(ctx, &end));
    let bytes = ctx.builtins().types.bytes;

    let Some((value, ty)) = value else {
        debug!("Unresolved slice expr");
        return Value::new(ctx.unresolved().hir, bytes);
    };

    // All of the bytes would only check that the value is an atom
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

    Value::new(ctx.alloc_hir(Hir::Substr(value, start, end)), ty)
}

/// Compiles the value that's indexed or sliced, which is any bytes. A list isn't, since finding
/// an item takes a walk through the list, which an index would hide the cost of.
///
/// This also gives the type of the result, which is a `String` for part of a string and `Bytes`
/// otherwise.
fn compile_indexed_value(ctx: &mut Compiler, expr: Option<AstExpr>) -> Option<(HirId, TypeId)> {
    let expr = expr?;
    let value = compile_expr(ctx, &expr, None);
    let types = ctx.builtins().types;

    if !ctx.is_unresolved(value.ty)
        && !ctx.is_assignable(value.ty, types.bytes)
        && !rue_types::extract_pairs(ctx.types_mut(), value.ty, false).is_empty()
    {
        let name = ctx.type_name(value.ty);
        ctx.diagnostic(expr.syntax(), DiagnosticKind::CannotIndexPair(name));
    } else {
        ctx.assign_type(expr.syntax(), value.ty, types.bytes);
    }

    let ty = if is_string(ctx, value.ty, &mut HashSet::new()) {
        types.string
    } else {
        types.bytes
    };

    Some((value.hir, ty))
}

/// Whether every value of the type is a `String`, rather than only being assignable to one, as
/// `Bytes` are.
fn is_string(ctx: &mut Compiler, ty: TypeId, unions: &mut HashSet<TypeId>) -> bool {
    let ty = rue_types::unwrap_semantic(ctx.types_mut(), ty, true);

    match ctx.ty(ty).clone() {
        Type::Atom(atom) => atom.semantic == AtomSemantic::String,
        Type::Union(union) => {
            // A union that's a member of itself has no values of its own there
            if !unions.insert(ty) {
                return true;
            }

            let is_string =
                !union.types.is_empty() && union.types.iter().all(|&ty| is_string(ctx, ty, unions));
            unions.remove(&ty);
            is_string
        }
        _ => false,
    }
}

fn compile_bound(ctx: &mut Compiler, expr: &AstExpr) -> HirId {
    let value = compile_expr(ctx, expr, None);
    ctx.assign_type(expr.syntax(), value.ty, ctx.builtins().types.int);
    value.hir
}

/// Binds the index, which is needed twice, rather than evaluating it twice, like the value that a
/// `match` checks: `{ let index = ...; value[index..index + 1] }`.
fn alloc_bound_index(ctx: &mut Compiler, value: HirId, index: HirId, srcloc: SrcLoc) -> HirId {
    let index = Value::new(index, ctx.builtins().types.int);
    let symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
        name: None,
        value: index,
        inline: false,
    }));

    let start = ctx.alloc_hir(Hir::Reference(symbol));
    let end = alloc_offset(ctx, start, BinaryOp::Add);
    let body = ctx.alloc_hir(Hir::Substr(value, start, Some(end)));

    ctx.alloc_hir(Hir::Block(Block {
        statements: vec![Statement::Let(symbol, srcloc)],
        body: Some(body),
    }))
}

/// The index after this one, which is `end` in the slice `value[index..end]` that the index
/// stands for, if it doesn't take evaluating the index again:
///
/// - The index is made of constants, so `end` is a constant too: `value[N * 2..7]`.
/// - It's a binding or a field of one, which costs as much to repeat as it would to bind:
///   `value[i..i + 1]`.
/// - It's one of those with a constant added to or subtracted from it, which one more is added
///   to in `end`: `value[i + 1..i + 2]` or `value[i - 1..i]`.
fn next_index(ctx: &mut Compiler, index: HirId, srcloc: &SrcLoc) -> Option<HirId> {
    if is_constant(ctx, index) {
        return Some(alloc_constant_offset(ctx, index, BinaryOp::Add, srcloc));
    }

    let (place, op, offset) = match *ctx.hir(ctx.without_casts(index)) {
        Hir::Binary(op @ (BinaryOp::Add | BinaryOp::Sub), left, right)
            if is_place(ctx, left) && is_constant(ctx, right) =>
        {
            (left, op, right)
        }
        Hir::Binary(BinaryOp::Add, left, right)
            if is_constant(ctx, left) && is_place(ctx, right) =>
        {
            (right, BinaryOp::Add, left)
        }
        _ if is_place(ctx, index) => return Some(alloc_offset(ctx, index, BinaryOp::Add)),
        _ => return None,
    };

    // One more than `i - 1` or `i + -1` is `i`
    let cancels = BigInt::from(if op == BinaryOp::Add { -1 } else { 1 });
    if literal_int(ctx, offset) == Some(cancels) {
        return Some(place);
    }

    // One more than `place + offset` is `place + (offset + 1)`, and one more than
    // `place - offset` is `place - (offset - 1)`
    let offset = alloc_constant_offset(ctx, offset, op, srcloc);
    Some(ctx.alloc_hir(Hir::Binary(op, place, offset)))
}

/// The value with one added to or subtracted from it.
fn alloc_offset(ctx: &mut Compiler, value: HirId, op: BinaryOp) -> HirId {
    let one = ctx.alloc_hir(Hir::Int(BigInt::from(1)));
    ctx.alloc_hir(Hir::Binary(op, value, one))
}

/// A value that's made of constants with one added to or subtracted from it, as a constant: a
/// literal if the value is one, and otherwise a const expression, which is evaluated once every
/// constant has a value, since a constant can be declared after the code that uses it. If it
/// can't be evaluated, it's left to be evaluated at runtime, like the value is.
fn alloc_constant_offset(ctx: &mut Compiler, value: HirId, op: BinaryOp, srcloc: &SrcLoc) -> HirId {
    if let Some(value) = literal_int(ctx, value) {
        return ctx.alloc_hir(Hir::Int(if op == BinaryOp::Add {
            value + 1
        } else {
            value - 1
        }));
    }

    let value = alloc_offset(ctx, value, op);

    ctx.alloc_hir(Hir::Const(ConstExpr {
        value,
        loc: srcloc.clone(),
        result: None,
        generated: true,
    }))
}

/// The value of an integer literal, with or without a sign.
fn literal_int(ctx: &Compiler, hir: HirId) -> Option<BigInt> {
    match ctx.hir(ctx.without_casts(hir)) {
        Hir::Int(value) => Some(value.clone()),
        Hir::Unary(UnaryOp::Plus, value) => literal_int(ctx, *value),
        Hir::Unary(UnaryOp::Neg, value) => Some(-literal_int(ctx, *value)?),
        _ => None,
    }
}

/// Whether the value only depends on literals and constants, such as `N * 2` or `"ab".length`,
/// so it can be evaluated when compiling.
fn is_constant(ctx: &Compiler, hir: HirId) -> bool {
    match ctx.hir(hir) {
        Hir::Nil | Hir::String(_) | Hir::Int(_) | Hir::Bytes(_) | Hir::Bool(_) | Hir::Const(_) => {
            true
        }
        Hir::Reference(symbol) => matches!(ctx.symbol(*symbol), Symbol::Constant(_)),
        Hir::Cast(value) | Hir::Unary(_, value) => is_constant(ctx, *value),
        Hir::Binary(_, left, right) => is_constant(ctx, *left) && is_constant(ctx, *right),
        Hir::FieldAccess(access) => is_constant(ctx, access.value),
        Hir::Block(Block {
            statements,
            body: Some(body),
        }) => statements.is_empty() && is_constant(ctx, *body),
        _ => false,
    }
}

/// Whether the value is a binding or parameter, or a field that's at a path from one, which is
/// found without computing anything. An `inline` binding or parameter stands for its value
/// wherever it's used, so it's repeated like the others.
fn is_place(ctx: &Compiler, hir: HirId) -> bool {
    match ctx.hir(ctx.without_casts(hir)) {
        Hir::Reference(_) => true,
        Hir::FieldAccess(access) => {
            matches!(access.kind, FieldKind::Path(_)) && is_place(ctx, access.value)
        }
        _ => false,
    }
}
