use std::collections::HashMap;

use rue_ast::{AstNode, AstStructInitializerExpr};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Hir, Value};
use rue_types::{Pair, Type, Union};

use crate::{Compiler, PathKind, PathResult, compile_expr, compile_path};

pub fn compile_struct_initializer_expr(
    ctx: &mut Compiler,
    expr: &AstStructInitializerExpr,
) -> Value {
    let ty = if let Some(path) = expr.path()
        && let PathResult::Type(ty) = compile_path(ctx, path.segments(), PathKind::Type)
    {
        ty
    } else {
        ctx.builtins().unresolved.ty
    };

    let semantic = rue_types::unwrap_semantic(ctx.types_mut(), ty, true);

    let Type::Struct(struct_type) = ctx.ty(semantic).clone() else {
        let name = ctx.type_name(ty);
        ctx.diagnostic(expr.syntax(), DiagnosticKind::NonStructInitializer(name));
        return ctx.builtins().unresolved.clone();
    };

    let mut expected_field_types = HashMap::new();
    let mut current = struct_type.inner;

    for field in &struct_type.fields {
        let pairs = rue_types::extract_pairs(ctx.types_mut(), current);

        if pairs.is_empty() {
            break;
        }

        let first = if pairs.len() == 1 {
            pairs[0].first
        } else {
            ctx.alloc_type(Type::Union(Union::new(
                pairs.iter().map(|pair| pair.first).collect(),
            )))
        };

        let rest = if pairs.len() == 1 {
            pairs[0].rest
        } else {
            ctx.alloc_type(Type::Union(Union::new(
                pairs.iter().map(|pair| pair.rest).collect(),
            )))
        };

        expected_field_types.insert(field.clone(), first);
        current = rest;
    }

    let mut fields = HashMap::new();

    for field in expr.fields() {
        let value = if let Some(expr) = field.expr() {
            compile_expr(
                ctx,
                &expr,
                field
                    .name()
                    .and_then(|name| expected_field_types.get(name.text()).copied()),
            )
        } else {
            ctx.builtins().unresolved.clone()
        };

        let Some(name) = field.name() else {
            continue;
        };

        if fields.insert(name.text().to_string(), value).is_some() {
            ctx.diagnostic(
                &name,
                DiagnosticKind::DuplicateField(name.text().to_string()),
            );
            continue;
        }

        if !struct_type.fields.contains(name.text()) {
            let type_name = ctx.type_name(ty);
            ctx.diagnostic(
                &name,
                DiagnosticKind::UnknownField(name.text().to_string(), type_name),
            );
        }
    }

    let mut hir = ctx.builtins().nil.hir;
    let mut list_type = ctx.builtins().nil.ty;

    let mut missing_fields = vec![];

    for (i, name) in struct_type.fields.into_iter().rev().enumerate() {
        let value = if let Some(value) = fields.remove(&name) {
            value
        } else {
            missing_fields.push(name);
            ctx.builtins().unresolved.clone()
        };

        if !struct_type.nil_terminated && i == 0 {
            hir = value.hir;
            list_type = value.ty;
        } else {
            hir = ctx.alloc_hir(Hir::Pair(value.hir, hir));
            list_type = ctx.alloc_type(Type::Pair(Pair::new(value.ty, list_type)));
        }
    }

    if !missing_fields.is_empty() {
        let type_name = ctx.type_name(ty);
        ctx.diagnostic(
            expr.syntax(),
            DiagnosticKind::MissingRequiredFields(type_name, missing_fields.join(", ")),
        );
    }

    ctx.assign_type(expr.syntax(), list_type, struct_type.inner);

    Value::new(hir, ty)
}
