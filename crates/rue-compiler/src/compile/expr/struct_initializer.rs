use std::collections::HashMap;

use log::debug;
use rue_ast::{AstNode, AstStructInitializerExpr};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Hir, StructField, StructInitializer, SymbolPath, Value};
use rue_types::{Type, Union};

use crate::{
    Compiler, CompletionContext, PathKind, PathResult, SyntaxField, SyntaxItemKind,
    check_symbol_value, compile_expr, compile_path,
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
        ctx.unresolved().ty
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
        return ctx.unresolved();
    };

    let mut expected_field_types = HashMap::new();
    let mut current = struct_type.inner;
    let mut unresolved = false;

    for (i, field) in struct_type.fields.iter().enumerate() {
        if i == struct_type.fields.len() - 1 && !struct_type.nil_terminated {
            expected_field_types.insert(field.clone(), current);
            continue;
        }

        let pairs = rue_types::extract_pairs(ctx.types_mut(), current, false);

        // The struct's type is only unresolved if it was rejected, since every type has been
        // resolved by the time expressions are compiled. The fields can't be checked against it.
        if pairs.is_empty() {
            unresolved = true;
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
            ) && check_symbol_value(ctx, field.syntax(), symbol)
            {
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
                ctx.unresolved()
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
            if let Some(expected_type) = expected_field_types.get(name.text()).copied() {
                let inferred = ctx.is_inferred_field(struct_type.semantic, name.text());

                match inferred.then(|| ctx.try_is_assignable(value.ty, expected_type)) {
                    Some(Ok(false)) => {
                        let from = ctx.type_name(value.ty);
                        let to = ctx.type_name(expected_type);
                        ctx.diagnostic(
                            field.syntax(),
                            DiagnosticKind::InferredFieldType(name.text().to_string(), from, to),
                        );
                    }
                    Some(Err(_)) => {
                        ctx.too_complex(field.syntax(), value.ty, expected_type);
                    }
                    Some(Ok(true)) | None => {
                        ctx.assign_type(field.syntax(), value.ty, expected_type);
                    }
                }
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

    if unresolved {
        debug!("Unresolved struct initializer due to unresolved struct type");
        return ctx.unresolved();
    }

    // Each field's value, and whether it's the default. A field without either is missing
    let mut values = Vec::new();

    for name in struct_type.fields.into_iter().rev() {
        let value = if let Some(value) = fields.remove(&name) {
            Some((value, false))
        } else if let Some(value) = ctx.default_field(struct_type.semantic, &name) {
            // Defaults are checked against the declared field type, but check them against the
            // field type of this instantiation as well, in case generics were substituted.
            if let Some(expected_type) = expected_field_types.get(&name) {
                ctx.assign_type(expr.syntax(), value.ty, *expected_type);
            }
            Some((value, true))
        } else {
            None
        };

        values.push((name, value));
    }

    values.reverse();

    let missing_fields = values
        .iter()
        .filter(|(_, value)| value.is_none())
        .map(|(name, _)| name.as_str())
        .collect::<Vec<_>>();

    // Missing fields are reported before they're filled in with placeholders, so that every
    // placeholder comes after the error that explains it
    if !missing_fields.is_empty() {
        let type_name = ctx.type_name(ty);
        ctx.diagnostic(
            expr.syntax(),
            DiagnosticKind::MissingRequiredFields(type_name, missing_fields.join(", ")),
        );
    }

    let initializer_fields = values
        .into_iter()
        .map(|(name, value)| {
            let (value, is_default) = value.unwrap_or_else(|| {
                debug!("Unresolved struct initializer field");
                (ctx.unresolved(), false)
            });

            StructField {
                name,
                value: value.hir,
                is_default,
            }
        })
        .collect();

    let hir = ctx.alloc_hir(Hir::Struct(StructInitializer {
        ty,
        fields: initializer_fields,
        nil_terminated: struct_type.nil_terminated,
    }));

    Value::new(hir, ty)
}
