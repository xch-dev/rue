use std::collections::HashMap;

use log::debug;
use rowan::TextRange;
use rue_ast::{AstGenericArguments, AstNode, AstPathSegment};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Symbol, SymbolId};
use rue_parser::SyntaxToken;
use rue_types::{Apply, Type, TypeId};

use crate::{Compiler, GetTextRange, compile_generic_arguments};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathKind {
    Symbol,
    Type,
}

#[derive(Debug, Clone, Copy)]
pub enum PathResult {
    Unresolved,
    Symbol(SymbolId, Option<TypeId>),
    Type(TypeId),
}

pub trait PathSegment {
    fn initial_separator(&self) -> Option<TextRange>;
    fn name(&self) -> Option<SyntaxToken>;
    fn generic_arguments(&self) -> Option<AstGenericArguments>;
}

impl PathSegment for AstPathSegment {
    fn initial_separator(&self) -> Option<TextRange> {
        self.initial_separator()
            .map(|separator| separator.syntax().text_range())
    }

    fn name(&self) -> Option<SyntaxToken> {
        self.name()
    }

    fn generic_arguments(&self) -> Option<AstGenericArguments> {
        self.generic_arguments()
    }
}

impl PathSegment for SyntaxToken {
    fn initial_separator(&self) -> Option<TextRange> {
        None
    }

    fn name(&self) -> Option<SyntaxToken> {
        Some(self.clone())
    }

    fn generic_arguments(&self) -> Option<AstGenericArguments> {
        None
    }
}

pub fn compile_path<S>(
    ctx: &mut Compiler,
    range: &impl GetTextRange,
    segments: impl Iterator<Item = S>,
    kind: PathKind,
) -> PathResult
where
    S: PathSegment,
{
    let segments = &segments.collect::<Vec<_>>();

    let mut value = None;
    let mut previous_name = None;

    for (index, segment) in segments.iter().enumerate() {
        if index == 0
            && let Some(separator) = segment.initial_separator()
        {
            ctx.diagnostic(&separator, DiagnosticKind::PathSeparatorInFirstSegment);
            return PathResult::Unresolved;
        }

        let Some(name) = segment.name() else {
            return PathResult::Unresolved;
        };

        let (symbol, ty) = if let Some(value) = value {
            if let PathResult::Symbol(symbol, _) = value
                && let Symbol::Module(module) = ctx.symbol(symbol)
            {
                let scope = ctx.scope(module.scope);
                let symbol = scope
                    .symbol(name.text())
                    .map(|symbol| (symbol, scope.is_symbol_exported(symbol)));
                let ty = scope
                    .ty(name.text())
                    .map(|ty| (ty, scope.is_type_exported(ty)));
                (symbol, ty)
            } else {
                ctx.diagnostic(
                    &name,
                    DiagnosticKind::SubpathNotSupported(previous_name.unwrap()),
                );
                return PathResult::Unresolved;
            }
        } else {
            let symbol = ctx.resolve_symbol(name.text()).map(|symbol| (symbol, true));
            let ty = ctx.resolve_type(name.text()).map(|ty| (ty, true));
            (symbol, ty)
        };

        let initial = match (symbol, ty) {
            (Some((symbol, _)), Some((ty, _))) => match kind {
                PathKind::Symbol => PathResult::Symbol(symbol, None),
                PathKind::Type => PathResult::Type(ty),
            },
            (Some((symbol, _)), None) => PathResult::Symbol(symbol, None),
            (None, Some((ty, _))) => PathResult::Type(ty),
            (None, None) => {
                match kind {
                    PathKind::Symbol => ctx.diagnostic(
                        &name,
                        DiagnosticKind::UndeclaredSymbol(name.text().to_string()),
                    ),
                    PathKind::Type => ctx.diagnostic(
                        &name,
                        DiagnosticKind::UndeclaredType(name.text().to_string()),
                    ),
                }
                PathResult::Unresolved
            }
        };

        if matches!(initial, PathResult::Symbol(_, _))
            && let Some((_, is_exported)) = symbol
            && !is_exported
        {
            ctx.diagnostic(
                &name,
                DiagnosticKind::PrivateSymbol(
                    name.text().to_string(),
                    previous_name.clone().unwrap(),
                ),
            );
        }

        if matches!(initial, PathResult::Type(_))
            && let Some((_, is_exported)) = ty
            && !is_exported
        {
            ctx.diagnostic(
                &name,
                DiagnosticKind::PrivateType(
                    name.text().to_string(),
                    previous_name.clone().unwrap(),
                ),
            );
        }

        match initial {
            PathResult::Unresolved => return PathResult::Unresolved,
            PathResult::Symbol(symbol, mut override_type) => {
                let args = if let Some(generic_arguments) = segment.generic_arguments() {
                    compile_generic_arguments(ctx, &generic_arguments)
                } else {
                    vec![]
                };

                let ty = ctx.symbol_type(symbol);

                if !args.is_empty() {
                    if let Symbol::Function(function) = ctx.symbol(symbol).clone() {
                        if function.vars.len() != args.len() {
                            ctx.diagnostic(
                                &name,
                                DiagnosticKind::ExpectedGenericArguments(
                                    function.vars.len(),
                                    args.len(),
                                ),
                            );
                        }

                        let mut map = HashMap::new();

                        for (i, arg) in args.iter().enumerate() {
                            if i >= function.vars.len() {
                                break;
                            }

                            map.insert(function.vars[i], *arg);
                        }

                        override_type = Some(ctx.alloc_type(Type::Apply(Apply::new(ty, map))));
                    } else {
                        ctx.diagnostic(&name, DiagnosticKind::GenericArgumentsOnSymbolReference);
                        return PathResult::Unresolved;
                    }
                }

                value = Some(PathResult::Symbol(symbol, override_type));
            }
            PathResult::Type(mut ty) => {
                let args = if let Some(generic_arguments) = segment.generic_arguments() {
                    compile_generic_arguments(ctx, &generic_arguments)
                } else {
                    vec![]
                };

                let semantic = rue_types::unwrap_semantic(ctx.types_mut(), ty, false);

                let params = match ctx.ty(semantic) {
                    Type::Apply(_) | Type::Ref(_) => unreachable!(),
                    Type::Unresolved => return PathResult::Unresolved,
                    Type::Atom(..)
                    | Type::Pair(..)
                    | Type::Function(..)
                    | Type::Generic(_)
                    | Type::Union(..)
                    | Type::Never
                    | Type::Any => {
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
                        let arg = args.get(i).copied().unwrap_or_else(|| {
                            debug!("Unresolved type due to missing generic argument");
                            ctx.builtins().unresolved.ty
                        });
                        map.insert(param, arg);
                    }
                    ty = ctx.alloc_type(Type::Apply(Apply::new(ty, map)));
                }

                value = Some(PathResult::Type(ty));
            }
        }

        previous_name = Some(name.text().to_string());
    }

    let value = value.unwrap_or(PathResult::Unresolved);

    match (value, kind) {
        (PathResult::Symbol(_, _), PathKind::Type) => {
            ctx.diagnostic(range, DiagnosticKind::ExpectedType(previous_name.unwrap()));
            PathResult::Unresolved
        }
        (PathResult::Type(_), PathKind::Symbol) => {
            ctx.diagnostic(
                range,
                DiagnosticKind::ExpectedSymbol(previous_name.unwrap()),
            );
            PathResult::Unresolved
        }
        _ => value,
    }
}
