use std::collections::HashMap;

use rue_ast::AstPathType;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Type, TypeId, apply_generics};

use crate::{Compiler, compile_generic_arguments};

pub fn compile_path_type(ctx: &mut Compiler, path: &AstPathType) -> TypeId {
    let mut ty = None;

    for segment in path.segments() {
        let Some(name) = segment.name() else {
            return ctx.builtins().unresolved.ty;
        };

        if ty.is_some() {
            todo!();
        }

        if segment.separator().is_some() && ty.is_some() {
            todo!();
        }

        let Some(mut resolved) = ctx.resolve_type(name.text()) else {
            ctx.diagnostic(
                &name,
                DiagnosticKind::UndeclaredType(name.text().to_string()),
            );
            return ctx.builtins().unresolved.ty;
        };

        let args = if let Some(generic_arguments) = segment.generic_arguments() {
            compile_generic_arguments(ctx, &generic_arguments)
        } else {
            vec![]
        };

        let params = match ctx.ty(resolved) {
            Type::Unresolved => continue,
            Type::Atom(..)
            | Type::Pair(..)
            | Type::Generic(..)
            | Type::Union(..)
            | Type::Fn(..) => vec![],
            Type::Alias(alias) => alias.vars.clone(),
        };

        if params.len() != args.len() {
            ctx.diagnostic(
                &name,
                DiagnosticKind::ExpectedGenericArguments(params.len(), args.len()),
            );
        }

        if !params.is_empty() {
            let mut map = HashMap::new();
            for (i, param) in params.into_iter().enumerate() {
                let arg = args.get(i).copied().unwrap_or(ctx.builtins().unresolved.ty);
                map.insert(param, arg);
            }
            resolved = apply_generics(ctx, resolved, &map);
        }

        ty = Some(resolved);
    }

    ty.unwrap_or(ctx.builtins().unresolved.ty)
}
