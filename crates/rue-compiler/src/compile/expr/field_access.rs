use log::debug;
use rue_ast::AstFieldAccessExpr;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Hir, TypePath, UnaryOp, Value};
use rue_types::{Type, Union};

use crate::{Compiler, compile_expr};

pub fn compile_field_access_expr(ctx: &mut Compiler, access: &AstFieldAccessExpr) -> Value {
    let expr = if let Some(expr) = access.expr() {
        compile_expr(ctx, &expr, None)
    } else {
        debug!("Unresolved field access expr");
        ctx.builtins().unresolved.clone()
    };

    let Some(name) = access.field() else {
        debug!("Unresolved field access name");
        return ctx.builtins().unresolved.clone();
    };

    let ty = rue_types::unwrap_semantic(ctx.types_mut(), expr.ty, true);

    match ctx.ty(ty).clone() {
        Type::Apply(_) | Type::Alias(_) | Type::Ref(_) => unreachable!(),
        Type::Unresolved => {
            debug!("Unresolved field access due to unresolved type");
            ctx.builtins().unresolved.clone()
        }
        Type::Generic(_) | Type::Function(_) | Type::Never => {
            debug!("Unresolved field access due to unknown field");
            let type_name = ctx.type_name(expr.ty);
            ctx.diagnostic(
                &name,
                DiagnosticKind::UnknownField(name.text().to_string(), type_name),
            );
            ctx.builtins().unresolved.clone()
        }
        Type::Atom(_) => {
            if name.text() == "length" {
                let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::Strlen, expr.hir));
                Value::new(hir, ctx.builtins().types.int)
            } else {
                debug!("Unresolved field access due to unknown field");
                let type_name = ctx.type_name(expr.ty);
                ctx.diagnostic(
                    &name,
                    DiagnosticKind::UnknownField(name.text().to_string(), type_name),
                );
                ctx.builtins().unresolved.clone()
            }
        }
        Type::Pair(_) | Type::Union(_) => {
            let pairs = rue_types::extract_pairs(ctx.types_mut(), ty);

            match name.text() {
                "first" if !pairs.is_empty() => {
                    let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::First, expr.hir));

                    let first = if pairs.len() == 1 {
                        pairs[0].first
                    } else {
                        ctx.alloc_type(Type::Union(Union::new(
                            pairs.iter().map(|pair| pair.first).collect(),
                        )))
                    };

                    let mut value = Value::new(hir, first);

                    if let Some(mut reference) = expr.reference {
                        reference.path.push(TypePath::First);
                        value = value.with_reference(reference);
                    }

                    value
                }
                "rest" if !pairs.is_empty() => {
                    let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::Rest, expr.hir));

                    let rest = if pairs.len() == 1 {
                        pairs[0].rest
                    } else {
                        ctx.alloc_type(Type::Union(Union::new(
                            pairs.iter().map(|pair| pair.rest).collect(),
                        )))
                    };

                    let mut value = Value::new(hir, rest);

                    if let Some(mut reference) = expr.reference {
                        reference.path.push(TypePath::Rest);
                        value = value.with_reference(reference);
                    }

                    value
                }
                _ => {
                    debug!("Unresolved field access due to unknown field");
                    let type_name = ctx.type_name(expr.ty);
                    ctx.diagnostic(
                        &name,
                        DiagnosticKind::UnknownField(name.text().to_string(), type_name),
                    );
                    ctx.builtins().unresolved.clone()
                }
            }
        }
        Type::Struct(ty) => {
            let Some(index) = ty.fields.get_index_of(name.text()) else {
                debug!("Unresolved field access due to unknown field");
                let type_name = ctx.type_name(expr.ty);
                ctx.diagnostic(
                    &name,
                    DiagnosticKind::MissingField(name.text().to_string(), type_name),
                );
                return ctx.builtins().unresolved.clone();
            };

            let mut hir = expr.hir;
            let mut field_type = ty.inner;
            let mut reference = expr.reference;

            let needs_first = index + 1 < ty.fields.len() || ty.nil_terminated;

            for i in 0..index {
                hir = ctx.alloc_hir(Hir::Unary(UnaryOp::Rest, hir));

                let pairs = rue_types::extract_pairs(ctx.types_mut(), field_type);

                if pairs.is_empty() || (pairs.len() > 1 && (i + 1 < index || needs_first)) {
                    debug!("Unresolved field access due to unknown field");
                    let type_name = ctx.type_name(expr.ty);
                    ctx.diagnostic(
                        &name,
                        DiagnosticKind::MissingField(name.text().to_string(), type_name),
                    );
                    return ctx.builtins().unresolved.clone();
                }

                field_type = if pairs.len() == 1 {
                    pairs[0].rest
                } else {
                    ctx.alloc_type(Type::Union(Union::new(
                        pairs.into_iter().map(|pair| pair.rest).collect(),
                    )))
                };

                if let Some(reference) = reference.as_mut() {
                    reference.path.push(TypePath::Rest);
                }
            }

            if needs_first {
                hir = ctx.alloc_hir(Hir::Unary(UnaryOp::First, hir));

                let pairs = rue_types::extract_pairs(ctx.types_mut(), field_type);

                if pairs.is_empty() {
                    debug!("Unresolved field access due to unknown field");
                    let type_name = ctx.type_name(expr.ty);
                    ctx.diagnostic(
                        &name,
                        DiagnosticKind::MissingField(name.text().to_string(), type_name),
                    );
                    return ctx.builtins().unresolved.clone();
                }

                field_type = if pairs.len() == 1 {
                    pairs[0].first
                } else {
                    ctx.alloc_type(Type::Union(Union::new(
                        pairs.into_iter().map(|pair| pair.first).collect(),
                    )))
                };

                if let Some(reference) = reference.as_mut() {
                    reference.path.push(TypePath::First);
                }
            }

            let mut value = Value::new(hir, field_type);

            if let Some(reference) = reference {
                value = value.with_reference(reference);
            }

            value
        }
    }
}
