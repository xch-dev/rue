use std::collections::HashMap;

use rue_ast::AstPathSegment;
use rue_diagnostic::DiagnosticKind;
use rue_hir::SymbolId;
use rue_types::{Apply, Type, TypeId};

use crate::{Compiler, compile_generic_arguments};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathKind {
    Symbol,
    Type,
}

#[derive(Debug, Clone, Copy)]
pub enum PathResult {
    Unresolved,
    Symbol(SymbolId),
    Type(TypeId),
}

pub fn compile_path(
    ctx: &mut Compiler,
    segments: impl Iterator<Item = AstPathSegment>,
    kind: PathKind,
) -> PathResult {
    let mut value = None;

    for (index, segment) in segments.enumerate() {
        if index == 0 && segment.separator().is_some() {
            ctx.diagnostic(
                &segment.separator().unwrap(),
                DiagnosticKind::PathSeparatorInFirstSegment,
            );
            return PathResult::Unresolved;
        }

        let Some(name) = segment.name() else {
            return PathResult::Unresolved;
        };

        if value.is_some() {
            ctx.diagnostic(
                &segment.separator().unwrap(),
                DiagnosticKind::SubpathNotSupported,
            );
            return PathResult::Unresolved;
        }

        match kind {
            PathKind::Symbol => {
                let Some(symbol) = ctx.resolve_symbol(name.text()) else {
                    ctx.diagnostic(
                        &name,
                        DiagnosticKind::UndeclaredSymbol(name.text().to_string()),
                    );
                    return PathResult::Unresolved;
                };

                let args = if let Some(generic_arguments) = segment.generic_arguments() {
                    compile_generic_arguments(ctx, &generic_arguments)
                } else {
                    vec![]
                };

                if !args.is_empty() {
                    ctx.diagnostic(&name, DiagnosticKind::GenericArgumentsOnSymbolReference);
                    return PathResult::Unresolved;
                }

                value = Some(PathResult::Symbol(symbol));
            }
            PathKind::Type => {
                let Some(mut ty) = ctx.resolve_type(name.text()) else {
                    ctx.diagnostic(
                        &name,
                        DiagnosticKind::UndeclaredType(name.text().to_string()),
                    );
                    return PathResult::Unresolved;
                };

                let args = if let Some(generic_arguments) = segment.generic_arguments() {
                    compile_generic_arguments(ctx, &generic_arguments)
                } else {
                    vec![]
                };

                let semantic = rue_types::unwrap_semantic(ctx.types_mut(), ty, false);

                let params = match ctx.ty(semantic) {
                    Type::Apply(_) => unreachable!(),
                    Type::Unresolved => return PathResult::Unresolved,
                    Type::Ref(..)
                    | Type::Atom(..)
                    | Type::Pair(..)
                    | Type::Function(..)
                    | Type::Generic
                    | Type::Union(..) => {
                        vec![]
                    }
                    Type::Alias(alias) => alias.generics.clone(),
                    Type::Struct(ty) => ty.generics.clone(),
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
                    ty = ctx.alloc_type(Type::Apply(Apply::new(ty, map)));
                }

                value = Some(PathResult::Type(ty));
            }
        }
    }

    value.unwrap_or(PathResult::Unresolved)
}
