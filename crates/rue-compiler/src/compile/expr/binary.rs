use std::collections::HashMap;

use log::debug;
use rue_ast::{AstBinaryExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{BinaryOp, ConstExpr, Declaration, Hir, Mappings, Power, Value};
use rue_parser::T;
use rue_types::{Type, TypeId};

use crate::{Compiler, compile_expr};

pub fn compile_binary_expr(ctx: &mut Compiler, binary: &AstBinaryExpr) -> Value {
    let left = |ctx: &mut Compiler| {
        if let Some(left) = binary.left() {
            compile_expr(ctx, &left, None)
        } else {
            debug!("Unresolved lhs in binary expr");
            ctx.unresolved()
        }
    };

    let right = |ctx: &mut Compiler, mappings: Option<Mappings>| {
        if let Some(right) = binary.right() {
            if let Some(mappings) = mappings {
                let range = right.syntax().text_range();
                let index = ctx.push_mappings(mappings, range.start());
                let value = compile_expr(ctx, &right, None);
                ctx.revert_mappings(index, range.end());
                value
            } else {
                compile_expr(ctx, &right, None)
            }
        } else {
            debug!("Unresolved rhs in binary expr");
            ctx.unresolved()
        }
    };

    let Some(op) = binary.op() else {
        debug!("Unresolved op in binary expr");
        return ctx.unresolved();
    };

    let (left, right) = match op.kind() {
        T![+] => {
            let left = left(ctx);
            let right = right(ctx, None);

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

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
            let right = right(ctx, None);

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

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
            let right = right(ctx, None);

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Mul, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.int);
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.public_key) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::G1Multiply, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.public_key);
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.signature) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::G2Multiply, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.signature);
            }

            (left, right)
        }
        T![**] => {
            let left = left(ctx);
            let right = right(ctx, None);

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);

                // CLVM has no power operator, so the exponent is evaluated at compile time like
                // a const expression, and the power is expanded once it has been (see
                // `expand_power`)
                let loc = binary.right().map_or_else(
                    || ctx.srcloc(&op),
                    |right| ctx.trimmed_srcloc(right.syntax()),
                );
                let exponent = ctx.alloc_hir(Hir::Const(ConstExpr {
                    value: right.hir,
                    loc,
                    result: None,
                    generated: false,
                }));
                let hir = ctx.alloc_hir(Hir::Power(Power {
                    base: left.hir,
                    exponent,
                    expansion: None,
                }));
                return Value::new(hir, ctx.builtins().types.int);
            }

            (left, right)
        }
        T![/] => {
            let left = left(ctx);
            let right = right(ctx, None);

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Div, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.int);
            }

            (left, right)
        }
        T![%] => {
            let left = left(ctx);
            let right = right(ctx, None);

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Mod, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.int);
            }

            (left, right)
        }
        T![<<] => {
            let left = left(ctx);
            let right = right(ctx, None);

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(
                    BinaryOp::LeftArithmeticShift,
                    left.hir,
                    right.hir,
                ));
                return Value::new(hir, ctx.builtins().types.int);
            }

            (left, right)
        }
        T![>>] => {
            let left = left(ctx);
            let right = right(ctx, None);

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(
                    BinaryOp::RightArithmeticShift,
                    left.hir,
                    right.hir,
                ));
                return Value::new(hir, ctx.builtins().types.int);
            }

            (left, right)
        }
        T![>>>] => {
            let left = left(ctx);
            let right = right(ctx, None);

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.int) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.int);
                let hir = ctx.alloc_hir(Hir::Binary(
                    BinaryOp::RightLogicalShift,
                    left.hir,
                    right.hir,
                ));
                return Value::new(hir, ctx.builtins().types.int);
            }

            (left, right)
        }
        T![>] => {
            let left = left(ctx);
            let right = right(ctx, None);

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

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
            let right = right(ctx, None);

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

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
            let right = right(ctx, None);

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

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
            let right = right(ctx, None);

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

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
            let independent = narrows_only_atoms(ctx, &left.then_map);
            let right = right(ctx, Some(left.then_map.clone()));

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.bool) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.bool);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::And, left.hir, right.hir));

                if independent {
                    ctx.set_independent_operands(hir);
                }

                return Value::new(hir, ctx.builtins().types.bool).with_mappings(
                    ctx.merge_mappings(&left.then_map, &right.then_map),
                    HashMap::new(),
                );
            }

            (left, right)
        }
        T![||] => {
            let left = left(ctx);
            let independent = narrows_only_atoms(ctx, &left.else_map);
            let right = right(ctx, Some(left.else_map.clone()));

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.bool) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.bool);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Or, left.hir, right.hir));

                if independent {
                    ctx.set_independent_operands(hir);
                }

                return Value::new(hir, ctx.builtins().types.bool).with_mappings(
                    HashMap::new(),
                    ctx.merge_mappings(&left.else_map, &right.else_map),
                );
            }

            (left, right)
        }
        T![&] => {
            let left = left(ctx);
            let right = right(ctx, None);

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.bool) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.bool);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::All, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.bool).with_mappings(
                    ctx.merge_mappings(&left.then_map, &right.then_map),
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
            let right = right(ctx, None);

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

            if ctx.is_assignable(left.ty, ctx.builtins().types.bool) {
                ctx.assign_type(&op, right.ty, ctx.builtins().types.bool);
                let hir = ctx.alloc_hir(Hir::Binary(BinaryOp::Any, left.hir, right.hir));
                return Value::new(hir, ctx.builtins().types.bool).with_mappings(
                    HashMap::new(),
                    ctx.merge_mappings(&left.else_map, &right.else_map),
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
            let right = right(ctx, None);

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

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
            let right = right(ctx, None);

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

            let types = [
                ctx.builtins().types.bool,
                ctx.builtins().types.int,
                ctx.builtins().types.bytes,
                ctx.builtins().types.public_key,
                ctx.builtins().types.signature,
            ];

            if types
                .iter()
                .any(|ty| ctx.is_assignable(left.ty, *ty) && ctx.is_assignable(right.ty, *ty))
                || atom_enum(ctx, left.ty).is_some_and(|ty| atom_enum(ctx, right.ty) == Some(ty))
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
            let right = right(ctx, None);

            if ctx.is_unresolved(left.ty) || ctx.is_unresolved(right.ty) {
                debug!("Unresolved binary expr operands");
                return ctx.unresolved();
            }

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
    ctx.unresolved()
}

/// The enum that every value of the type is a variant of, if its variants are only their
/// discriminants. Those are atoms, so they can be compared like the integer literals they are.
fn atom_enum(ctx: &mut Compiler, ty: TypeId) -> Option<TypeId> {
    let mut result = None;
    let mut stack = vec![ty];

    while let Some(ty) = stack.pop() {
        let semantic = rue_types::unwrap_semantic(ctx.types_mut(), ty, true);

        match ctx.ty(semantic) {
            Type::Union(union) => stack.extend(union.types.iter().copied()),
            // A variant that's only its discriminant has no other fields, and isn't nil terminated
            Type::Struct(variant) if !variant.nil_terminated && variant.fields.len() == 1 => {
                let enum_type = ctx.enum_of(Declaration::Type(variant.semantic))?;

                if result.is_some_and(|result| result != enum_type) {
                    return None;
                }

                result = Some(enum_type);
            }
            _ => return None,
        }
    }

    result
}

/// Whether the right operand of `&&` or `||` is type checked with the same shapes whether or not
/// the left operand was evaluated first: the left operand narrows nothing for it, or only values
/// that were already atoms, such as a `Mode` to the modes that are left. Then nothing the right
/// operand does can rely on the left one for whether a value is an atom or a pair, so it can be
/// evaluated on its own (see `Database::set_independent_operands`). Narrowing a field, or a value
/// that could be a pair, lets the right operand read what's only there if the left one says so.
fn narrows_only_atoms(ctx: &mut Compiler, mappings: &Mappings) -> bool {
    let atom = ctx.builtins().types.atom;

    for (&symbol, paths) in mappings {
        if !paths.keys().all(Vec::is_empty) {
            return false;
        }

        let ty = ctx.symbol_type(symbol);

        if !ctx.is_castable(ty, atom) {
            return false;
        }
    }

    true
}
