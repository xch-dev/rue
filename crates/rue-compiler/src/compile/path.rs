use std::collections::HashMap;

use log::debug;
use rowan::TextRange;
use rue_ast::{AstGenericArguments, AstNode, AstPathSegment};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Declaration, ImportId, Symbol, SymbolId};
use rue_parser::SyntaxToken;
use rue_types::{Apply, Type, TypeId};

use crate::{Compiler, CompletionContext, GetTextRange, SyntaxItemKind, compile_generic_arguments};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathKind {
    Symbol,
    Type,
}

#[derive(Debug, Clone, Copy)]
pub enum PathResult {
    Unresolved,
    Symbol(SymbolId, Option<TypeId>, Option<ImportId>),
    Type(TypeId, Option<ImportId>),
}

pub trait PathSegment {
    fn separator(&self) -> Option<TextRange>;
    fn text_range(&self) -> TextRange;
    fn name(&self) -> Option<SyntaxToken>;
    fn generic_arguments(&self) -> Option<AstGenericArguments>;
}

impl PathSegment for AstPathSegment {
    fn separator(&self) -> Option<TextRange> {
        self.separator().map(|separator| separator.text_range())
    }

    fn text_range(&self) -> TextRange {
        self.syntax().text_range()
    }

    fn name(&self) -> Option<SyntaxToken> {
        self.name()
    }

    fn generic_arguments(&self) -> Option<AstGenericArguments> {
        self.generic_arguments()
    }
}

impl PathSegment for SyntaxToken {
    fn separator(&self) -> Option<TextRange> {
        None
    }

    fn text_range(&self) -> TextRange {
        self.text_range()
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
    mut completions: bool,
) -> PathResult
where
    S: PathSegment,
{
    let mut segments = segments.collect::<Vec<_>>();
    let mut base_scope = ctx.last_scope_id();
    let mut module_stack = ctx.parent_module_stack().to_vec();
    let mut path_completion_module = None;

    let length = segments.len();

    if let Some(segment) = segments.first()
        && let Some(separator) = segment.separator()
    {
        ctx.diagnostic(&separator, DiagnosticKind::PathSeparatorInFirstSegment);
    }

    while let Some(segment) = segments.first()
        && let Some(name) = segment.name()
        && name.text() == "super"
    {
        if let Some(module) = path_completion_module
            && completions
            && let Some(separator) = segment.separator()
        {
            ctx.add_syntax(
                SyntaxItemKind::CompletionContext(CompletionContext::ModuleExports {
                    module,
                    allow_super: true,
                }),
                TextRange::new(separator.end(), segment.text_range().end()),
            );
        }

        segments.remove(0);

        if let Some(module) = module_stack.pop() {
            base_scope = ctx.module(module).scope;
            path_completion_module = Some(module);

            ctx.add_syntax(SyntaxItemKind::SymbolReference(module), name.text_range());
        } else {
            ctx.diagnostic(&name, DiagnosticKind::UnresolvedSuper);
            completions = false;
        }
    }

    let mut value = None;
    let mut previous_name = None;

    for (index, segment) in segments.iter().enumerate() {
        if let Some(module) = path_completion_module
            && completions
            && let Some(separator) = segment.separator()
        {
            ctx.add_syntax(
                SyntaxItemKind::CompletionContext(CompletionContext::ModuleExports {
                    module,
                    allow_super: index == 0,
                }),
                TextRange::new(separator.end(), segment.text_range().end()),
            );
        }

        if let Some(name) = segment.name()
            && name.text() == "super"
        {
            if index == length - 1 {
                ctx.diagnostic(&name, DiagnosticKind::SuperAtEnd);
            } else {
                ctx.diagnostic(&name, DiagnosticKind::SuperAfterNamedImport);
            }
            return PathResult::Unresolved;
        }

        let Some(name) = segment.name() else {
            return PathResult::Unresolved;
        };

        let (symbol, ty) = if let Some(value) = value {
            if let PathResult::Symbol(symbol, _, _) = value
                && let Symbol::Module(module) = ctx.symbol(symbol)
            {
                let scope = ctx.scope(module.scope);
                let symbol = scope.symbol(name.text()).map(|symbol| {
                    (
                        symbol,
                        scope.is_symbol_exported(symbol),
                        scope.symbol_import(symbol),
                    )
                });
                let ty = scope
                    .ty(name.text())
                    .map(|ty| (ty, scope.is_type_exported(ty), scope.type_import(ty)));
                (symbol, ty)
            } else {
                ctx.diagnostic(
                    &name,
                    DiagnosticKind::SubpathNotSupported(previous_name.unwrap()),
                );
                return PathResult::Unresolved;
            }
        } else if base_scope == ctx.last_scope_id() {
            let symbol = ctx
                .resolve_symbol_in(base_scope, name.text())
                .map(|(symbol, import)| (symbol, true, import));
            let ty = ctx
                .resolve_type_in(base_scope, name.text())
                .map(|(ty, import)| (ty, true, import));
            (symbol, ty)
        } else {
            let base = ctx.scope(base_scope);
            let symbol = base
                .symbol(name.text())
                .filter(|s| base.is_symbol_exported(*s))
                .map(|s| (s, true, base.symbol_import(s)));
            let ty = base
                .ty(name.text())
                .filter(|t| base.is_type_exported(*t))
                .map(|t| (t, true, base.type_import(t)));
            (symbol, ty)
        };

        let initial = match (symbol, ty) {
            (Some((symbol, _, symbol_import)), Some((ty, _, type_import))) => match kind {
                PathKind::Symbol => PathResult::Symbol(symbol, None, symbol_import),
                PathKind::Type => PathResult::Type(ty, type_import),
            },
            (Some((symbol, _, symbol_import)), None) => {
                PathResult::Symbol(symbol, None, symbol_import)
            }
            (None, Some((ty, _, type_import))) => PathResult::Type(ty, type_import),
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

        if matches!(initial, PathResult::Symbol(_, _, _))
            && let Some((_, is_exported, _)) = symbol
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

        if matches!(initial, PathResult::Type(_, _))
            && let Some((_, is_exported, _)) = ty
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
            PathResult::Symbol(symbol, mut override_type, import) => {
                ctx.reference(Declaration::Symbol(symbol), import);
                ctx.reference_span(Declaration::Symbol(symbol), name.text_range());

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

                if matches!(ctx.symbol(symbol), Symbol::Module(_)) {
                    path_completion_module = Some(symbol);
                }

                value = Some(PathResult::Symbol(symbol, override_type, import));
            }
            PathResult::Type(mut ty, import) => {
                ctx.reference(Declaration::Type(ty), import);
                ctx.reference_span(Declaration::Type(ty), name.text_range());

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

                value = Some(PathResult::Type(ty, import));
            }
        }

        previous_name = Some(name.text().to_string());
    }

    let value = value.unwrap_or(PathResult::Unresolved);

    match (value, kind) {
        (PathResult::Symbol(_, _, _), PathKind::Type) => {
            ctx.diagnostic(range, DiagnosticKind::ExpectedType(previous_name.unwrap()));
            PathResult::Unresolved
        }
        (PathResult::Type(_, _), PathKind::Symbol) => {
            ctx.diagnostic(
                range,
                DiagnosticKind::ExpectedSymbol(previous_name.unwrap()),
            );
            PathResult::Unresolved
        }
        _ => value,
    }
}
