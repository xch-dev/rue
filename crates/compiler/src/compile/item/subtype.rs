use indexmap::IndexMap;
use rue_ast::{AstNode, AstSubtypeItem};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{
    ParameterSymbol, Scope, Subtype, SubtypeField, SubtypeParent, SubtypeVar, Symbol, Type, TypeId,
    Var,
};

use crate::{Context, compile_expr, compile_type};

pub fn declare_subtype(ctx: &mut Context, subtype: &AstSubtypeItem) -> TypeId {
    let mut fields = IndexMap::new();

    if let Some(field_list) = subtype.fields() {
        let mut is_empty = true;

        for item in field_list.fields() {
            let Some(name) = item.name() else {
                continue;
            };

            let field = SubtypeField {
                name: name.clone(),
                ty: ctx.builtins().unresolved.ty,
                accessor: ctx.builtins().unresolved.hir,
            };

            if let Some(name) = item.name() {
                if fields.contains_key(name.text()) {
                    ctx.diagnostic(
                        &name,
                        DiagnosticKind::DuplicateField(name.text().to_string()),
                    );
                }

                fields.insert(name.text().to_string(), field);
            }

            is_empty = false;
        }

        if is_empty {
            ctx.diagnostic(field_list.syntax(), DiagnosticKind::EmptySubtypeFields);
        }
    }

    let scope = ctx.alloc_scope(Scope::new());

    let vars = if let Some(generic_parameters) = subtype.generic_parameters() {
        let mut vars = Vec::new();

        let mut is_empty = true;

        for generic_parameter in generic_parameters.parameters() {
            let Some(name) = generic_parameter.name() else {
                continue;
            };

            let Some(field) = generic_parameter.field() else {
                continue;
            };

            if !fields.contains_key(field.text()) {
                ctx.diagnostic(
                    &field,
                    DiagnosticKind::UndeclaredSubtypeField(field.text().to_string()),
                );
                continue;
            }

            is_empty = false;

            let ty = ctx.alloc_type(Type::Var(Var {
                name: Some(name.clone()),
            }));

            if ctx.scope(scope).ty(name.text()).is_some() {
                ctx.diagnostic(
                    &name,
                    DiagnosticKind::DuplicateType(name.text().to_string()),
                );
            }

            ctx.scope_mut(scope)
                .insert_type(name.text().to_string(), ty);

            vars.push(SubtypeVar {
                ty,
                field: field.text().to_string(),
            });
        }

        if is_empty {
            ctx.diagnostic(
                generic_parameters.syntax(),
                DiagnosticKind::EmptyGenericParameters,
            );
        }

        vars
    } else {
        vec![]
    };

    let parent = if let Some(parameter) = subtype.parameter() {
        let ty = ctx.builtins().unresolved.ty;

        let symbol = ctx.alloc_symbol(Symbol::Parameter(ParameterSymbol {
            name: parameter.name(),
            ty,
        }));

        if let Some(name) = parameter.name() {
            if ctx.scope(scope).symbol(name.text()).is_some() {
                ctx.diagnostic(
                    &name,
                    DiagnosticKind::DuplicateSymbol(name.text().to_string()),
                );
            }

            ctx.scope_mut(scope)
                .insert_symbol(name.text().to_string(), symbol);
        }

        Some(SubtypeParent { symbol, ty })
    } else {
        None
    };

    let constraint = subtype.constraint().map(|_| ctx.builtins().unresolved.hir);

    let ty = ctx.alloc_type(Type::Subtype(Subtype {
        name: subtype.name(),
        scope,
        vars,
        parent,
        fields,
        constraint,
    }));

    if let Some(name) = subtype.name() {
        if ctx.last_scope().ty(name.text()).is_some() {
            ctx.diagnostic(
                &name,
                DiagnosticKind::DuplicateType(name.text().to_string()),
            );
        }

        ctx.last_scope_mut()
            .insert_type(name.text().to_string(), ty);
    }

    ty
}

pub fn compile_subtype(ctx: &mut Context, subtype: &AstSubtypeItem, ty: TypeId) {
    let (scope, mut fields) = if let Type::Subtype(Subtype { scope, fields, .. }) = ctx.ty(ty) {
        (*scope, fields.clone())
    } else {
        unreachable!();
    };

    ctx.push_scope(scope);

    let resolved_parent = if let Some(parameter) = subtype.parameter()
        && let Some(parent) = parameter.ty()
    {
        compile_type(ctx, &parent)
    } else {
        ctx.builtins().unresolved.ty
    };

    let resolved_constraint = if let Some(constraint) = subtype.constraint()
        && let Some(expr) = constraint.expr()
    {
        Some(compile_expr(ctx, &expr))
    } else {
        None
    };

    if let Some(field_list) = subtype.fields() {
        for item in field_list.fields() {
            let Some(expr) = item.expr() else {
                continue;
            };

            let value = compile_expr(ctx, &expr);

            if let Some(name) = item.name() {
                let field = SubtypeField {
                    name: name.clone(),
                    ty: value.ty,
                    accessor: value.hir,
                };

                fields.insert(name.text().to_string(), field);
            }
        }
    }

    ctx.pop_scope();

    let Type::Subtype(Subtype {
        parent, constraint, ..
    }) = ctx.ty_mut(ty)
    else {
        unreachable!()
    };

    if let Some(parent) = parent {
        parent.ty = resolved_parent;
    }

    // TODO: Check the constraint return type to make sure it's a boolean
    *constraint = resolved_constraint.map(|value| value.hir);
}
