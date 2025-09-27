use std::collections::HashMap;

use log::debug;
use rue_ast::{AstBinaryExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{BinaryOp, Hir, Value, merge_mappings};
use rue_parser::T;

use crate::{Compiler, compile_expr};

pub fn compile_binary_expr(ctx: &mut Compiler, binary: &AstBinaryExpr) -> Value {
    let left = |ctx: &mut Compiler| {
        if let Some(left) = binary.left() {
            compile_expr(ctx, &left, None)
        } else {
            debug!("Unresolved lhs in binary expr");
            ctx.builtins().unresolved.clone()
        }
    };

    let right = |ctx: &mut Compiler| {
        if let Some(right) = binary.right() {
            compile_expr(ctx, &right, None)
        } else {
            debug!("Unresolved rhs in binary expr");
            ctx.builtins().unresolved.clone()
        }
    };

    let Some(op) = binary.op() else {
        debug!("Unresolved op in binary expr");
        return ctx.builtins().unresolved.clone();
    };

    let (left, right) = match op.kind() {
        T![+] => {
            let left = left(ctx);
            let right = right(ctx);

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Add, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.int);
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.bytes) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.bytes);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Concat, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.bytes);
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.public_key) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.public_key);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::G1Add, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.public_key);
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.signature) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.signature);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::G2Add, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.signature);
            }

            (left, right)
        }
        T![-] => {
            let left = left(ctx);
            let right = right(ctx);

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Sub, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.int);
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.public_key) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.public_key);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::G1Subtract, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.public_key);
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.signature) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.signature);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::G2Subtract, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.signature);
            }

            (left, right)
        }
        T![*] => {
            let left = left(ctx);
            let right = right(ctx);

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Mul, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.int);
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.public_key) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.public_key);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::G1Multiply, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.public_key);
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.signature) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.signature);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::G2Multiply, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.signature);
            }

            (left, right)
        }
        T![/] => {
            let left = left(ctx);
            let right = right(ctx);

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Div, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.int);
            }

            (left, right)
        }
        T![%] => {
            let left = left(ctx);
            let right = right(ctx);

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Mod, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.int);
            }

            (left, right)
        }
        T![<<] => {
            let left = left(ctx);
            let right = right(ctx);

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::LeftShift, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.int);
            }

            (left, right)
        }
        T![>>] => {
            let left = left(ctx);
            let right = right(ctx);

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::RightShift, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.int);
            }

            (left, right)
        }
        T![>] => {
            let left = left(ctx);
            let right = right(ctx);

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Gt, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.bool);
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.bytes) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.bytes);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::GtBytes, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.bool);
            }

            (left, right)
        }
        T![<] => {
            let left = left(ctx);
            let right = right(ctx);

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Lt, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.bool);
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.bytes) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.bytes);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::LtBytes, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.bool);
            }

            (left, right)
        }
        T![>=] => {
            let left = left(ctx);
            let right = right(ctx);

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Gte, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.bool);
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.bytes) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.bytes);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::GteBytes, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.bool);
            }

            (left, right)
        }
        T![<=] => {
            let left = left(ctx);
            let right = right(ctx);

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Lte, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.bool);
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.bytes) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.bytes);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::LteBytes, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.bool);
            }

            (left, right)
        }
        T![&&] => {
            let left = left(ctx);

            let index = ctx.push_mappings(left.then_map.clone());
            let right = right(ctx);
            ctx.revert_mappings(index);

            if ctx.is_assignable(left.ty, ctx.builtins().types.bool) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.bool);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::And, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.bool).with_mappings(
                    merge_mappings(&left.then_map, &right.then_map),
                    HashMap::new(),
                );
            }

            (left, right)
        }
        T![||] => {
            let left = left(ctx);

            let index = ctx.push_mappings(left.else_map.clone());
            let right = right(ctx);
            ctx.revert_mappings(index);

            if ctx.is_assignable(left.ty, ctx.builtins().types.bool) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.bool);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Or, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.bool).with_mappings(
                    HashMap::new(),
                    merge_mappings(&left.else_map, &right.else_map),
                );
            }

            (left, right)
        }
        T![&] => {
            let left = left(ctx);
            let right = right(ctx);

            if ctx.is_assignable(left.ty, ctx.builtins().types.bool) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.bool);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::All, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.bool).with_mappings(
                    merge_mappings(&left.then_map, &right.then_map),
                    HashMap::new(),
                );
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::BitwiseAnd, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.int);
            }

            (left, right)
        }
        T![|] => {
            let left = left(ctx);
            let right = right(ctx);

            if ctx.is_assignable(left.ty, ctx.builtins().types.bool) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.bool);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Any, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.bool).with_mappings(
                    HashMap::new(),
                    merge_mappings(&left.then_map, &right.then_map),
                );
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::BitwiseOr, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.int);
            }

            (left, right)
        }
        T![^] => {
            let left = left(ctx);
            let right = right(ctx);

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::BitwiseXor, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.int);
            }

            (left, right)
        }
        T![==] | T![!=] => {
            // TODO: Guard?

            let left = left(ctx);
            let right = right(ctx);

            if ctx.is_castable(left.ty, ctx.builtins().types.bytes)
                && ctx.is_assignable(right.ty, left.ty)
            {
                let hir = if op.kind() == T![==] {
                    ctx.alloc_hir(Hir::Binary(BinaryOp::Eq, left.hir, right.hir))
                } else {
                    ctx.alloc_hir(Hir::Binary(BinaryOp::Ne, left.hir, right.hir))
                };

                let mut value = Value::new(hir, ctx.builtins().types.bool);

                if op.kind() == T![!=] {
                    value = value.flip_mappings();
                }

                return value;
            }

            (left, right)
        }
        _ => {
            let left = left(ctx);
            let right = right(ctx);

            (left, right)
        }
    };

    let left_name = ctx.type_name(left.ty);
    let right_name = ctx.type_name(right.ty);

    debug!("Unresolved binary expr");

    ctx.diagnostic(
        binary.syntax(),
        DiagnosticKind::IncompatibleBinaryOp(op.text().to_string(), left_name, right_name),
    );
    ctx.builtins().unresolved.clone()
}
