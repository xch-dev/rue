use std::borrow::Cow;

use log::debug;
use rue_ast::{AstNode, AstPrefixExpr};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Hir, UnaryOp, Value};
use rue_lir::{atom_bigint, bigint_atom};
use rue_parser::T;
use rue_types::{Atom, AtomRestriction, AtomSemantic, Atoms, Type};

use crate::{Compiler, compile_expr};

pub fn compile_prefix_expr(ctx: &mut Compiler, prefix: &AstPrefixExpr) -> Value {
    let Some(expr) = prefix.expr() else {
        debug!("Unresolved prefix expr");
        return ctx.builtins().unresolved.clone();
    };

    let value = compile_expr(ctx, &expr, None);

    let Some(op) = prefix.op() else {
        return value;
    };

    match op.kind() {
        T![!] => {
            if ctx.is_assignable(value.ty, ctx.builtins().types.bool) {
                let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::Not, value.hir));
                return value
                    .flip_mappings()
                    .with_hir(hir)
                    .with_type(ctx.builtins().types.bool);
            }
        }
        T![+] => {
            if ctx.is_assignable(value.ty, ctx.builtins().types.int) {
                ctx.diagnostic(prefix.syntax(), DiagnosticKind::UnnecessaryPlus);
                return Value::new(value.hir, ctx.builtins().types.int);
            }
        }
        T![-] => {
            if ctx.is_assignable(value.ty, ctx.builtins().types.int) {
                let ty = if let Some(Atoms::Restricted(restrictions)) =
                    rue_types::extract_atoms(ctx.types_mut(), value.ty, true)
                    && restrictions.len() == 1
                    && let Some(AtomRestriction::Value(value)) = restrictions.iter().next()
                {
                    ctx.alloc_type(Type::Atom(Atom::new(
                        AtomSemantic::Int,
                        Some(AtomRestriction::Value(Cow::Owned(bigint_atom(
                            -atom_bigint(value),
                        )))),
                    )))
                } else {
                    ctx.builtins().types.int
                };

                let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::Neg, value.hir));
                return Value::new(hir, ty);
            }
        }
        T![~] => {
            if ctx.is_assignable(value.ty, ctx.builtins().types.int) {
                let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::BitwiseNot, value.hir));
                return Value::new(hir, ctx.builtins().types.int);
            }
        }
        _ => {}
    }

    debug!("Unresolved prefix expr due to incompatible op");

    let type_name = ctx.type_name(value.ty);
    ctx.diagnostic(
        prefix.syntax(),
        DiagnosticKind::IncompatibleUnaryOp(op.text().to_string(), type_name),
    );
    ctx.builtins().unresolved.clone()
}
