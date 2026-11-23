use std::collections::HashMap;

use log::debug;
use rowan::TextRange;
use rue_ast::{AstGenericArguments, AstNode, AstPathSegment};
use rue_diagnostic::{DiagnosticKind, Note};
use rue_hir::{Declaration, ImportId, Item, Namespace, ScopeId, Symbol, SymbolId};
use rue_parser::SyntaxToken;
use rue_types::{Apply, FunctionType, Type, TypeId};

use crate::{
    Compiler, CompletionContext, GetTextRange, SyntaxItemKind, compile_function_generic_arguments,
    compile_generic_arguments,
};

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

/// Where a segment of a path is looked up.
#[derive(Debug, Clone, Copy)]
enum SegmentBase {
    /// In the module that the previous segment names, where private items can be found so that
    /// they're reported.
    Module(ScopeId),
    /// From the scope the path is written in, and then each enclosing scope.
    Local(ScopeId),
    /// In the module that `super` or `project` names. It contains the code with the path, so its
    /// private items can be used, and a file's sibling modules can be too.
    Parent(ScopeId),
}

/// Looks up a segment of a path in one namespace. The item is returned with whether it can be
/// used from where the path is written, and the import that brought it into the scope it was
/// found in, if any.
fn lookup_segment(
    ctx: &Compiler,
    base: SegmentBase,
    namespace: Namespace,
    name: &str,
) -> Option<(Item, bool, Option<ImportId>)> {
    match base {
        SegmentBase::Module(scope) => {
            let scope = ctx.scope(scope);
            let (item, via) = scope.item_with_import(namespace, name)?;
            let exported = scope.is_item_exported(item);

            // A failed import that isn't exported doesn't bring anything in for other modules, so
            // the name is reported as undeclared there
            if ctx.is_error_item(item) && !exported {
                return None;
            }

            Some((item, exported, via))
        }
        SegmentBase::Local(scope) => ctx
            .resolve_item_in(scope, namespace, name)
            .map(|(item, via)| (item, true, via)),
        SegmentBase::Parent(scope) => ctx
            .module_member(scope, namespace, name)
            .map(|(item, via)| (item, true, via)),
    }
}

/// Looks up a segment of a path in one namespace, like [`lookup_segment`], as the kind of ID that
/// the namespace has.
fn lookup_as<T>(
    ctx: &Compiler,
    base: SegmentBase,
    namespace: Namespace,
    name: &str,
    id: fn(Item) -> Option<T>,
) -> Option<(T, bool, Option<ImportId>)> {
    let (item, exported, via) = lookup_segment(ctx, base, namespace, name)?;
    Some((id(item)?, exported, via))
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
    // The modules that `super` can refer to, which are only needed if the path starts with it
    let mut module_stack = None;
    let mut path_completion_module = None;

    let length = segments.len();

    if let Some(segment) = segments.first()
        && let Some(separator) = segment.separator()
    {
        ctx.diagnostic(&separator, DiagnosticKind::PathSeparatorInFirstSegment);
    }

    // The last `super` or `project` that the path starts with, and whether it named a module
    let mut last_prefix = None;
    // Whether the path starts with a `super` or `project` that named a module (containing the code)
    let mut from_parent = false;
    // Whether a `super` can come next, which it can't after `project`, since the root has no parent
    let mut allow_super = true;

    if let Some(segment) = segments.first()
        && let Some(name) = segment.name()
        && name.text() == "project"
    {
        segments.remove(0);

        let root = ctx.project_root();
        base_scope = ctx.module(root).scope;
        module_stack = Some(Vec::new());
        path_completion_module = Some(root);
        from_parent = true;
        allow_super = false;

        ctx.add_syntax(SyntaxItemKind::SymbolReference(root), name.text_range());
        last_prefix = Some((name, true));
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
                    allow_super,
                    members: true,
                }),
                TextRange::new(separator.end(), segment.text_range().end()),
            );
        }

        segments.remove(0);

        if let Some(module) = module_stack
            .get_or_insert_with(|| ctx.parent_module_stack())
            .pop()
        {
            base_scope = ctx.module(module).scope;
            path_completion_module = Some(module);
            from_parent = true;

            ctx.add_syntax(SyntaxItemKind::SymbolReference(module), name.text_range());
            last_prefix = Some((name, true));
        } else {
            ctx.diagnostic(&name, DiagnosticKind::UnresolvedSuper);
            completions = false;
            last_prefix = Some((name, false));
        }
    }

    // A path that's only `super` or `project` names a module, which can't be used as a value or a
    // type
    if segments.is_empty()
        && let Some((name, resolved)) = last_prefix
    {
        if resolved && name.text() == "project" {
            ctx.diagnostic(&name, DiagnosticKind::ProjectAtEnd);
        } else if resolved {
            ctx.diagnostic(&name, DiagnosticKind::SuperAtEnd);
        }

        return PathResult::Unresolved;
    }

    let mut value = None;
    let mut previous: Option<SyntaxToken> = None;

    for (index, segment) in segments.iter().enumerate() {
        if let Some(module) = path_completion_module
            && completions
            && let Some(separator) = segment.separator()
        {
            ctx.add_syntax(
                SyntaxItemKind::CompletionContext(CompletionContext::ModuleExports {
                    module,
                    allow_super: allow_super && index == 0,
                    members: index == 0,
                }),
                TextRange::new(separator.end(), segment.text_range().end()),
            );
        }

        if let Some(name) = segment.name()
            && name.text() == "project"
        {
            ctx.diagnostic(&name, DiagnosticKind::ProjectNotAtStart);
            return PathResult::Unresolved;
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

        // A name that's resolved through a module (rather than from the scope it's written in, or
        // a module that `super` names, which contains it) can only be used if it's exported
        let qualified = value.is_some();

        // Only a module can have a path after it, and modules have their own namespace
        let has_subpath = index + 1 < segments.len();

        let base = if let Some(value) = value {
            if let PathResult::Symbol(symbol, _, _) = value
                && let Symbol::Module(module) = ctx.symbol(symbol)
            {
                SegmentBase::Module(module.scope)
            } else {
                let previous = previous.unwrap();
                ctx.diagnostic(
                    &previous,
                    DiagnosticKind::SubpathNotSupported(previous.text().to_string()),
                );
                return PathResult::Unresolved;
            }
        } else if from_parent {
            SegmentBase::Parent(base_scope)
        } else {
            SegmentBase::Local(base_scope)
        };

        // A name whose import failed has already been reported at the import, so it's
        // unresolved without being reported again. It's checked in every namespace, since a use
        // of the wrong kind of item would be reported too.
        let failed = |ctx: &Compiler| {
            Namespace::ALL.into_iter().any(|namespace| {
                lookup_segment(ctx, base, namespace, name.text())
                    .is_some_and(|(item, _, _)| ctx.is_error_item(item))
            })
        };

        let find_symbol = || lookup_as(ctx, base, Namespace::Value, name.text(), Item::symbol);
        let find_type = || lookup_as(ctx, base, Namespace::Type, name.text(), Item::ty);
        let find_module = || lookup_as(ctx, base, Namespace::Module, name.text(), Item::symbol);

        let (symbol, ty) = if has_subpath {
            let module = find_module();

            if module.is_none_or(|(module, _, _)| ctx.is_error_item(Item::Module(module)))
                && failed(ctx)
            {
                return PathResult::Unresolved;
            }

            if module.is_none() && (find_symbol().is_some() || find_type().is_some()) {
                ctx.diagnostic(
                    &name,
                    DiagnosticKind::SubpathNotSupported(name.text().to_string()),
                );
                return PathResult::Unresolved;
            }

            (module, None)
        } else {
            // When a name is both a symbol and a type, the expected kind wins, so the other is only
            // looked up as a fallback. A lookup that fails walks every enclosing scope, and each
            // `let` statement opens one. A module is only found so that it can be reported.
            let (symbol, ty) = match kind {
                PathKind::Symbol => {
                    let symbol = find_symbol();
                    let ty = symbol.is_none().then(find_type).flatten();
                    (symbol, ty)
                }
                PathKind::Type => {
                    let ty = find_type();
                    let symbol = ty.is_none().then(find_symbol).flatten();
                    (symbol, ty)
                }
            };

            let error = symbol.is_some_and(|(symbol, _, _)| ctx.is_error_item(Item::Value(symbol)))
                || ty.is_some_and(|(ty, _, _)| ctx.is_error_item(Item::Type(ty)));

            if error || (symbol.is_none() && ty.is_none() && failed(ctx)) {
                return PathResult::Unresolved;
            }

            if symbol.is_none() && ty.is_none() {
                (find_module(), None)
            } else {
                (symbol, ty)
            }
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
                    PathKind::Symbol => {
                        ctx.diagnostic(
                            &name,
                            DiagnosticKind::UndeclaredSymbol(name.text().to_string()),
                        );
                    }
                    PathKind::Type if name.text() == "_" => {
                        ctx.diagnostic(&name, DiagnosticKind::PlaceholderType);
                    }
                    PathKind::Type => {
                        ctx.diagnostic(
                            &name,
                            DiagnosticKind::UndeclaredType(name.text().to_string()),
                        );
                    }
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
                    previous.as_ref().unwrap().text().to_string(),
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
                    previous.as_ref().unwrap().text().to_string(),
                ),
            );
        }

        if let SegmentBase::Local(_) = base {
            let namespace = match initial {
                PathResult::Unresolved => None,
                _ if has_subpath => Some(Namespace::Module),
                PathResult::Symbol(..) => Some(Namespace::Value),
                PathResult::Type(..) => Some(Namespace::Type),
            };

            if let Some(namespace) = namespace {
                ctx.record_enclosed_prelude_reference(namespace, &name);
            }
        }

        let declaration = match initial {
            PathResult::Symbol(symbol, _, import) => Some((Declaration::Symbol(symbol), import)),
            PathResult::Type(ty, import) => Some((Declaration::Type(ty), import)),
            PathResult::Unresolved => None,
        };

        if let Some((declaration, import)) = declaration {
            if ctx.is_ambiguous_import(import, declaration) {
                ctx.diagnostic(
                    &name,
                    DiagnosticKind::AmbiguousImport(name.text().to_string()),
                );
            }

            // The standard library can't reference the modules being linted
            if qualified && ctx.source().kind.check_unused() {
                let srcloc = ctx.srcloc(&name);

                ctx.lint_sites_mut()
                    .add_qualified_reference(declaration, srcloc);
            }
        }

        match initial {
            PathResult::Unresolved => return PathResult::Unresolved,
            PathResult::Symbol(symbol, mut override_type, import) => {
                ctx.reference(Declaration::Symbol(symbol), import);

                if has_subpath {
                    ctx.add_ambiguous_path(import, Declaration::Symbol(symbol));
                }
                ctx.reference_span(Declaration::Symbol(symbol), name.text_range());

                let args = if let Some(generic_arguments) = segment.generic_arguments() {
                    compile_function_generic_arguments(ctx, &generic_arguments)
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

                        // A `_` leaves its generic to be inferred, and a generic with no argument
                        // at all is unresolved, since the mismatch was already reported
                        for (i, &var) in function.vars.iter().enumerate() {
                            match args.get(i) {
                                Some(Some(arg)) => {
                                    map.insert(var, *arg);
                                }
                                Some(None) => {}
                                None => {
                                    map.insert(var, ctx.unresolved().ty);
                                }
                            }
                        }

                        // The instance no longer binds the generics that are given, so that
                        // they're substituted throughout the function's type
                        let ty = match ctx.ty(ty).clone() {
                            Type::Function(function) => {
                                ctx.alloc_type(Type::Function(FunctionType {
                                    generics: function
                                        .generics
                                        .into_iter()
                                        .filter(|generic| !map.contains_key(generic))
                                        .collect(),
                                    ..function
                                }))
                            }
                            _ => ty,
                        };

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
                ctx.name_type(ty);
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
                            ctx.unresolved().ty
                        });
                        map.insert(param, arg);
                    }
                    ty = ctx.alloc_type(Type::Apply(Apply::new(ty, map)));
                }

                value = Some(PathResult::Type(ty, import));
            }
        }

        previous = Some(name);
    }

    let value = value.unwrap_or(PathResult::Unresolved);

    match (value, kind) {
        (PathResult::Symbol(_, _, _), PathKind::Type) => {
            ctx.diagnostic(
                range,
                DiagnosticKind::ExpectedType(previous.unwrap().text().to_string()),
            );
            PathResult::Unresolved
        }
        (PathResult::Type(_, _), PathKind::Symbol) => {
            ctx.diagnostic(
                range,
                DiagnosticKind::ExpectedSymbol(previous.unwrap().text().to_string()),
            );
            PathResult::Unresolved
        }
        (PathResult::Symbol(symbol, _, _), PathKind::Symbol) => {
            if let Symbol::Module(_) = ctx.symbol(symbol) {
                let name = previous.unwrap().text().to_string();
                let count = ctx.diagnostic_count();
                ctx.diagnostic(range, DiagnosticKind::CannotReferenceModule(name.clone()));

                // A local binding of the same name would shadow the module, so a name by itself
                // only refers to it when nothing else named that is in scope. A name after a
                // module in a path is only looked up in that module.
                if length == 1 {
                    for diagnostic in ctx.diagnostics_since_mut(count) {
                        diagnostic.notes.push(Note::new(format!(
                            "No value named `{name}` is in scope here, so `{name}` is the module, which can't be used as a value; use one of its items, as in `{name}::item`"
                        )));
                    }
                }
            }
            value
        }
        _ => value,
    }
}
