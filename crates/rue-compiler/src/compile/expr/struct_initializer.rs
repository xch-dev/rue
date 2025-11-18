use std::collections::HashMap;

use log::debug;
use rue_ast::{AstNode, AstStructInitializerExpr};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Hir, SymbolPath, Value};
use rue_types::{Pair, Type, Union};

use crate::{
    Compiler, CompletionContext, PathKind, PathResult, SyntaxField, SyntaxItemKind, compile_expr,
    compile_path,
};

pub fn compile_struct_initializer_expr(
    ctx: &mut Compiler,
    expr: &AstStructInitializerExpr,
) -> Value {
    let ty = if let Some(path) = expr.path()
        && let PathResult::Type(ty, _) =
            compile_path(ctx, path.syntax(), path.segments(), PathKind::Type, true)
    {
        ty
    } else {
        debug!("Unresolved struct initializer path");
        ctx.builtins().unresolved.ty
    };

    let semantic = rue_types::unwrap_semantic(ctx.types_mut(), ty, true);

    ctx.add_syntax(
        SyntaxItemKind::CompletionContext(CompletionContext::StructFields {
            ty: semantic,
            specified_fields: Some(
                expr.fields()
                    .filter_map(|field| field.name())
                    .map(|field| field.text().to_string())
                    .collect(),
            ),
        }),
        expr.syntax().text_range(),
    );

    let Type::Struct(struct_type) = ctx.ty(semantic).clone() else {
        debug!("Unresolved struct initializer due to non struct type");
        let name = ctx.type_name(ty);
        ctx.diagnostic(expr.syntax(), DiagnosticKind::NonStructInitializer(name));
        return ctx.builtins().unresolved.clone();
    };

    let mut expected_field_types = HashMap::new();
    let mut current = struct_type.inner;

    for (i, field) in struct_type.fields.iter().enumerate() {
        if i == struct_type.fields.len() - 1 && !struct_type.nil_terminated {
            expected_field_types.insert(field.clone(), current);
            continue;
        }

        let pairs = rue_types::extract_pairs(ctx.types_mut(), current, false);

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
            let Some(name) = field.name() else {
                continue;
            };

            if let PathResult::Symbol(symbol, override_type, _) = compile_path(
                ctx,
                field.syntax(),
                [name].into_iter(),
                PathKind::Symbol,
                false,
            ) {
                let ty = ctx.symbol_type(symbol);

                let mut value = Value::new(ctx.alloc_hir(Hir::Reference(symbol)), ty)
                    .with_reference(SymbolPath {
                        symbol,
                        path: vec![],
                    });

                if let Some(override_type) = override_type {
                    value = value.with_type(override_type);
                }

                value
            } else {
                debug!("Unresolved field path in struct initializer");
                ctx.builtins().unresolved.clone()
            }
        };

        let Some(name) = field.name() else {
            continue;
        };

        ctx.add_syntax(
            SyntaxItemKind::FieldInitializer(SyntaxField {
                name: name.text().to_string(),
                container: semantic,
                ty: expected_field_types
                    .get(name.text())
                    .copied()
                    .unwrap_or(value.ty),
            }),
            name.text_range(),
        );

        if struct_type.fields.contains(name.text()) {
            if let Some(expected_type) = expected_field_types.get(name.text()) {
                ctx.assign_type(field.syntax(), value.ty, *expected_type);
            }
        } else {
            let type_name = ctx.type_name(ty);
            ctx.diagnostic(
                &name,
                DiagnosticKind::UnknownField(name.text().to_string(), type_name),
            );
            continue;
        }

        if fields.insert(name.text().to_string(), value).is_some() {
            ctx.diagnostic(
                &name,
                DiagnosticKind::DuplicateField(name.text().to_string()),
            );
        }
    }

    let mut hir = ctx.builtins().nil.hir;
    let mut list_type = ctx.builtins().nil.ty;

    let mut missing_fields = vec![];

    for (i, name) in struct_type.fields.into_iter().rev().enumerate() {
        let value = if let Some(value) = fields.remove(&name) {
            value
        } else if let Some(value) = ctx.default_field(struct_type.semantic, &name) {
            value
        } else {
            debug!("Unresolved struct initializer field");
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

    Value::new(hir, ty)
}
