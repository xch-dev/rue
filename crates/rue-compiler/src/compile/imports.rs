use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
};

use indexmap::IndexMap;
use rowan::TextRange;
use rue_ast::{AstImportItem, AstImportPathSegment};
use rue_diagnostic::{DiagnosticKind, Name};
use rue_hir::{Declaration, Import, ImportId, ImportUser, Items, ScopeId, Symbol, SymbolId};

use crate::{Compiler, SyntaxItemKind};

pub fn declare_import_item(ctx: &mut Compiler, import: &AstImportItem) {
    let Some(path) = import.path() else {
        return;
    };

    let base_scope = ctx.last_scope_id();
    let module_stack = ctx.parent_module_stack().to_vec();

    let imports = construct_imports(
        ctx,
        base_scope,
        module_stack,
        Vec::new(),
        &path.segments().collect::<Vec<_>>(),
        import.export().is_some(),
    );

    for import in imports {
        ctx.last_scope_mut().add_import(import);
        ctx.add_relevant_import(import);
    }
}

fn construct_imports(
    ctx: &mut Compiler,
    mut base_scope: ScopeId,
    mut module_stack: Vec<SymbolId>,
    mut path: Vec<Name>,
    segments: &[AstImportPathSegment],
    exported: bool,
) -> Vec<ImportId> {
    let mut has_non_super = false;
    let mut has_super = false;

    if let Some(segment) = segments.first()
        && let Some(separator) = segment.separator()
    {
        ctx.diagnostic(&separator, DiagnosticKind::PathSeparatorInFirstSegment);
    }

    for segment in segments.iter().take(segments.len() - 1) {
        if let Some(name) = segment.name() {
            if name.text() == "super" {
                if has_non_super {
                    ctx.diagnostic(&name, DiagnosticKind::SuperAfterNamedImport);
                } else if let Some(module) = module_stack.pop() {
                    base_scope = ctx.module(module).scope;

                    ctx.add_syntax(SyntaxItemKind::SymbolReference(module), name.text_range());
                } else {
                    ctx.diagnostic(&name, DiagnosticKind::UnresolvedSuper);
                }

                has_super = true;
            } else {
                path.push(ctx.local_name(&name));
                has_non_super = true;
            }
        }
    }

    let Some(last) = segments.last() else {
        return vec![];
    };

    if let Some(name) = last.name()
        && name.text() == "super"
    {
        // There's nothing to import, so the import isn't also reported as unresolved
        ctx.diagnostic(&name, DiagnosticKind::SuperAtEnd);
        return vec![];
    }

    let source = ctx.source().clone();

    if let Some(name) = last.name() {
        let name = ctx.local_name(&name);

        vec![ctx.alloc_import(Import {
            base_scope,
            source,
            path,
            items: Items::Named(name),
            exported,
            declarations: Vec::new(),
            has_super,
        })]
    } else if let Some(star) = last.star() {
        let star = ctx.local_name(&star);

        vec![ctx.alloc_import(Import {
            base_scope,
            source,
            path,
            items: Items::All(star),
            exported,
            declarations: Vec::new(),
            has_super,
        })]
    } else if let items = last.items().collect::<Vec<_>>()
        && !items.is_empty()
    {
        let mut imports = Vec::new();

        for item in items {
            imports.extend(construct_imports(
                ctx,
                base_scope,
                module_stack.clone(),
                path.clone(),
                &item.segments().collect::<Vec<_>>(),
                exported,
            ));
        }

        imports
    } else {
        vec![]
    }
}

#[derive(Debug, Default)]
pub struct ImportCache {
    scopes: HashMap<(ScopeId, Vec<String>), CachedPath>,
    unused_imports: IndexMap<ImportId, IndexMap<String, Name>>,
    glob_import_counts: IndexMap<ImportId, (Name, usize)>,
    /// The errors in imports, keyed by where they are, so that each is reported once however
    /// many times the import is resolved.
    errors: IndexMap<(ImportId, ImportError), (Name, DiagnosticKind)>,
}

/// An error in an import, which is reported once all imports are resolved.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ImportError {
    /// The segment of the path at this index names a private module.
    PrivateModule(usize),
    PrivateSymbol,
    PrivateType,
    /// The name is already declared in the scope as a different symbol.
    DuplicateSymbol,
    /// The name is already declared in the scope as a different type.
    DuplicateType,
}

/// The module that a path resolves to.
#[derive(Debug, Clone)]
struct CachedPath {
    scope: ScopeId,
    symbol: SymbolId,
    /// The declarations referenced through other imports while resolving the path, so that
    /// they're recorded for every import that uses it.
    references: Vec<(ImportId, Declaration)>,
    /// The indices of the segments of the path that name a private module.
    private_segments: Vec<usize>,
}

fn flatten_imports(
    ctx: &mut Compiler,
    top_level_modules: Vec<SymbolId>,
) -> Vec<(ScopeId, ImportId)> {
    let mut imports = Vec::new();
    let mut stack = vec![(None, top_level_modules)];

    while let Some((scope, modules)) = stack.pop() {
        if let Some(scope) = scope {
            for import in ctx.scope(scope).imports() {
                imports.push((scope, import));
            }
        }

        for module in modules {
            let Symbol::Module(module) = ctx.symbol(module).clone() else {
                unreachable!();
            };

            stack.push((Some(module.scope), module.declarations.modules.clone()));
        }
    }

    imports
}

pub fn resolve_imports(
    ctx: &mut Compiler,
    all_modules: Vec<SymbolId>,
    cache: &mut ImportCache,
    diagnostics: bool,
) {
    let mut imports = flatten_imports(ctx, all_modules);
    imports.sort_by(|(_, left), (_, right)| compare_imports(ctx.import(*left), ctx.import(*right)));
    let targeted_names = imports
        .iter()
        .filter_map(|(scope, import)| match &ctx.import(*import).items {
            Items::Named(name) => Some((*scope, name.text().to_string())),
            Items::All(_) => None,
        })
        .collect::<HashSet<_>>();

    let mut updated = true;
    let mut missing_imports = IndexMap::new();

    while updated {
        updated = false;

        for &(import_scope, import) in &imports {
            updated |= resolve_import(
                ctx,
                import_scope,
                import,
                diagnostics,
                cache,
                &mut missing_imports,
                &targeted_names,
            );
        }
    }

    if diagnostics {
        for missing_imports in missing_imports.into_values() {
            for name in missing_imports.into_values() {
                ctx.diagnostic_name(
                    &name,
                    DiagnosticKind::UnresolvedImport(name.text().to_string()),
                );
            }
        }

        for (name, kind) in cache.errors.values() {
            ctx.diagnostic_name(name, kind.clone());
        }

        for unused_imports in cache.unused_imports.values() {
            for name in unused_imports.values() {
                ctx.lint_sites_mut().empty_imports.push(name.clone());
            }
        }

        for (name, count) in cache.glob_import_counts.values() {
            if *count == 0 {
                ctx.lint_sites_mut().empty_imports.push(name.clone());
            }
        }
    }
}

fn compare_imports(left: &Import, right: &Import) -> Ordering {
    import_priority(&left.items)
        .cmp(&import_priority(&right.items))
        .then_with(|| {
            left.path
                .iter()
                .map(Name::text)
                .cmp(right.path.iter().map(Name::text))
        })
        .then_with(|| import_item_name(&left.items).cmp(import_item_name(&right.items)))
        .then_with(|| left.exported.cmp(&right.exported))
}

fn import_priority(items: &Items) -> u8 {
    match items {
        Items::Named(_) => 0,
        Items::All(_) => 1,
    }
}

fn import_item_name(items: &Items) -> &str {
    match items {
        Items::Named(name) | Items::All(name) => name.text(),
    }
}

fn resolve_import(
    ctx: &mut Compiler,
    import_scope: ScopeId,
    import_id: ImportId,
    diagnostics: bool,
    cache: &mut ImportCache,
    missing_imports: &mut IndexMap<ImportId, IndexMap<String, Name>>,
    targeted_names: &HashSet<(ScopeId, String)>,
) -> bool {
    let import = ctx.import(import_id).clone();
    let has_super = import.has_super;
    let source = import.source.clone();

    let mut base = None;
    let mut path_so_far = Vec::new();
    let mut path_references = Vec::new();
    let mut private_segments = Vec::new();

    for i in (1..=import.path.len()).rev() {
        let subpath = import
            .path
            .iter()
            .take(i)
            .map(|t| t.text().to_string())
            .collect::<Vec<_>>();

        if let Some(cached) = cache.scopes.get(&(import_scope, subpath.clone())) {
            base = Some(cached.scope);
            path_so_far = subpath;
            path_references.clone_from(&cached.references);
            private_segments.clone_from(&cached.private_segments);

            for &(import, declaration) in &cached.references {
                ctx.add_import_reference(import, declaration, ImportUser::Import(import_id));
            }

            for name in import.path.iter().take(path_so_far.len()) {
                if diagnostics && let Some(srcloc) = name.srcloc() {
                    ctx.add_syntax_for_source(
                        SyntaxItemKind::SymbolReference(cached.symbol),
                        TextRange::new(
                            srcloc.span.start.try_into().unwrap(),
                            srcloc.span.end.try_into().unwrap(),
                        ),
                        source.kind.clone(),
                    );
                }
            }

            break;
        }
    }

    for (index, name) in import.path.iter().enumerate().skip(path_so_far.len()) {
        // A private module is reported, but still resolved, like it is in other paths
        let symbol = if let Some(base) = base {
            let base = ctx.scope(base);
            base.symbol(name.text())
                .map(|s| (s, base.symbol_import(s), base.is_symbol_exported(s)))
        } else if import.base_scope == import_scope {
            ctx.resolve_symbol_in(import.base_scope, name.text())
                .map(|(s, import)| (s, import, true))
        } else {
            let base = ctx.scope(import.base_scope);
            base.symbol(name.text())
                .filter(|s| base.is_symbol_exported(*s))
                .map(|s| (s, base.symbol_import(s), true))
        };

        let Some((symbol, import, exported)) = symbol else {
            if diagnostics {
                ctx.diagnostic_name(
                    name,
                    DiagnosticKind::UndeclaredSymbol(name.text().to_string()),
                );
            }
            return false;
        };

        if let Some(import) = import {
            ctx.add_import_reference(
                import,
                Declaration::Symbol(symbol),
                ImportUser::Import(import_id),
            );
            path_references.push((import, Declaration::Symbol(symbol)));
        }

        if diagnostics && let Some(srcloc) = name.srcloc() {
            ctx.add_syntax_for_source(
                SyntaxItemKind::SymbolReference(symbol),
                TextRange::new(
                    srcloc.span.start.try_into().unwrap(),
                    srcloc.span.end.try_into().unwrap(),
                ),
                source.kind.clone(),
            );
        }

        let Symbol::Module(module) = ctx.symbol(symbol) else {
            if diagnostics {
                ctx.diagnostic_name(
                    name,
                    DiagnosticKind::SubpathNotSupported(name.text().to_string()),
                );
            }
            return false;
        };

        base = Some(module.scope);
        path_so_far.push(name.text().to_string());

        if !exported {
            private_segments.push(index);
        }

        if !has_super {
            cache.scopes.insert(
                (import_scope, path_so_far.clone()),
                CachedPath {
                    scope: module.scope,
                    symbol,
                    references: path_references.clone(),
                    private_segments: private_segments.clone(),
                },
            );
        }
    }

    for &index in &private_segments {
        let name = &import.path[index];

        cache.errors.insert(
            (import_id, ImportError::PrivateModule(index)),
            (
                name.clone(),
                DiagnosticKind::PrivateSymbol(
                    name.text().to_string(),
                    import.path[index - 1].text().to_string(),
                ),
            ),
        );
    }

    let mut updated = false;

    match import.items {
        Items::All(star) => {
            let count = &mut cache
                .glob_import_counts
                .entry(import_id)
                .or_insert_with(|| (star.clone(), 0))
                .1;

            let symbols = if let Some(base) = base {
                ctx.scope(base)
                    .exported_symbols()
                    .map(|(name, symbol)| (name.to_string(), symbol))
                    .collect::<Vec<_>>()
            } else {
                let base = ctx.scope(import.base_scope);

                base.symbol_names()
                    .map(|name| (name.to_string(), base.symbol(name).unwrap()))
                    .collect::<Vec<_>>()
            };

            let types = if let Some(base) = base {
                ctx.scope(base)
                    .exported_types()
                    .map(|(name, ty)| (name.to_string(), ty))
                    .collect::<Vec<_>>()
            } else {
                let base = ctx.scope(import.base_scope);

                base.type_names()
                    .map(|name| (name.to_string(), base.ty(name).unwrap()))
                    .collect::<Vec<_>>()
            };

            for (name, symbol) in symbols {
                if targeted_names.contains(&(import_scope, name.clone())) {
                    continue;
                }
                let target = ctx.scope_mut(import_scope);

                if target.symbol(&name).is_none() {
                    target.insert_symbol(name.clone(), symbol, import.exported);
                    target.add_symbol_import(symbol, import_id);
                    ctx.import_mut(import_id)
                        .declarations
                        .push((name, Declaration::Symbol(symbol)));
                    updated = true;
                    *count += 1;
                } else if !target.is_symbol_exported(symbol)
                    && import.exported
                    && target.symbol(&name) == Some(symbol)
                {
                    target.export_symbol(symbol);
                    ctx.import_mut(import_id)
                        .declarations
                        .push((name, Declaration::Symbol(symbol)));
                    updated = true;
                    *count += 1;
                }
            }

            for (name, ty) in types {
                if targeted_names.contains(&(import_scope, name.clone())) {
                    continue;
                }
                let target = ctx.scope_mut(import_scope);

                if target.ty(&name).is_none() {
                    target.insert_type(name.clone(), ty, import.exported);
                    target.add_type_import(ty, import_id);
                    ctx.import_mut(import_id)
                        .declarations
                        .push((name, Declaration::Type(ty)));
                    updated = true;
                    *count += 1;
                } else if !target.is_type_exported(ty)
                    && import.exported
                    && target.ty(&name) == Some(ty)
                {
                    target.export_type(ty);
                    ctx.import_mut(import_id)
                        .declarations
                        .push((name, Declaration::Type(ty)));
                    updated = true;
                    *count += 1;
                }
            }
        }
        Items::Named(item) => {
            let missing_imports = missing_imports.entry(import_id).or_insert_with(|| {
                [item.clone()]
                    .into_iter()
                    .map(|item| (item.text().to_string(), item))
                    .collect()
            });

            let unused_imports = cache.unused_imports.entry(import_id).or_insert_with(|| {
                [item.clone()]
                    .into_iter()
                    .map(|item| (item.text().to_string(), item))
                    .collect()
            });

            let name = item.text();

            let (symbol, ty) = if let Some(base) = base {
                let base = ctx.scope(base);
                let symbol = base.symbol(name);
                let ty = base.ty(name);
                let public_symbol = symbol.filter(|s| base.is_symbol_exported(*s));
                let public_ty = ty.filter(|t| base.is_type_exported(*t));
                let module = import.path.last().unwrap().text();
                let symbol_key = (import_id, ImportError::PrivateSymbol);
                let type_key = (import_id, ImportError::PrivateType);

                // Private items are reported, but still imported, like they are in other paths.
                // If the other namespace has a public item with the name, only it's imported and
                // nothing is reported. Symbols are declared after the first pass, so a private
                // type imported then stays imported once a public symbol with its name appears.
                if public_symbol.is_some() || public_ty.is_some() {
                    cache.errors.shift_remove(&symbol_key);
                    cache.errors.shift_remove(&type_key);
                    (public_symbol, public_ty)
                } else {
                    if symbol.is_some() {
                        cache.errors.insert(
                            symbol_key,
                            (
                                item.clone(),
                                DiagnosticKind::PrivateSymbol(name.to_string(), module.to_string()),
                            ),
                        );
                    }

                    if ty.is_some() {
                        cache.errors.insert(
                            type_key,
                            (
                                item.clone(),
                                DiagnosticKind::PrivateType(name.to_string(), module.to_string()),
                            ),
                        );
                    }

                    (symbol, ty)
                }
            } else {
                let symbol = ctx
                    .resolve_symbol_in(import.base_scope, name)
                    .filter(|s| {
                        import.base_scope == import_scope
                            || ctx.scope(import.base_scope).is_symbol_exported(s.0)
                    })
                    .map(|(s, _)| s);
                let ty = ctx
                    .resolve_type_in(import.base_scope, name)
                    .filter(|t| {
                        import.base_scope == import_scope
                            || ctx.scope(import.base_scope).is_type_exported(t.0)
                    })
                    .map(|(t, _)| t);
                (symbol, ty)
            };

            if let Some(symbol) = symbol {
                if diagnostics && let Some(srcloc) = item.srcloc() {
                    ctx.add_syntax_for_source(
                        SyntaxItemKind::SymbolReference(symbol),
                        TextRange::new(
                            srcloc.span.start.try_into().unwrap(),
                            srcloc.span.end.try_into().unwrap(),
                        ),
                        source.kind.clone(),
                    );
                }

                let target = ctx.scope_mut(import_scope);

                if target.symbol(name).is_none() {
                    target.insert_symbol(name.to_string(), symbol, import.exported);
                    target.add_symbol_import(symbol, import_id);
                    ctx.import_mut(import_id)
                        .declarations
                        .push((name.to_string(), Declaration::Symbol(symbol)));
                    updated = true;
                    unused_imports.shift_remove(name);
                } else if !target.is_symbol_exported(symbol)
                    && import.exported
                    && target.symbol(name) == Some(symbol)
                {
                    target.export_symbol(symbol);
                    ctx.import_mut(import_id)
                        .declarations
                        .push((name.to_string(), Declaration::Symbol(symbol)));
                    updated = true;
                    unused_imports.shift_remove(name);
                } else if diagnostics && target.symbol(name) != Some(symbol) {
                    cache.errors.insert(
                        (import_id, ImportError::DuplicateSymbol),
                        (
                            item.clone(),
                            DiagnosticKind::DuplicateSymbol(name.to_string()),
                        ),
                    );
                    unused_imports.shift_remove(name);
                }
            }

            if let Some(ty) = ty {
                if diagnostics && let Some(srcloc) = item.srcloc() {
                    ctx.add_syntax_for_source(
                        SyntaxItemKind::TypeReference(ty),
                        TextRange::new(
                            srcloc.span.start.try_into().unwrap(),
                            srcloc.span.end.try_into().unwrap(),
                        ),
                        source.kind.clone(),
                    );
                }

                let target = ctx.scope_mut(import_scope);

                if target.ty(name).is_none() {
                    target.insert_type(name.to_string(), ty, import.exported);
                    target.add_type_import(ty, import_id);
                    ctx.import_mut(import_id)
                        .declarations
                        .push((name.to_string(), Declaration::Type(ty)));
                    updated = true;
                    unused_imports.shift_remove(name);
                } else if !target.is_type_exported(ty)
                    && import.exported
                    && target.ty(name) == Some(ty)
                {
                    target.export_type(ty);
                    ctx.import_mut(import_id)
                        .declarations
                        .push((name.to_string(), Declaration::Type(ty)));
                    updated = true;
                    unused_imports.shift_remove(name);
                } else if diagnostics && target.ty(name) != Some(ty) {
                    cache.errors.insert(
                        (import_id, ImportError::DuplicateType),
                        (
                            item.clone(),
                            DiagnosticKind::DuplicateType(name.to_string()),
                        ),
                    );
                    unused_imports.shift_remove(name);
                }
            }

            if symbol.is_some() || ty.is_some() {
                missing_imports.shift_remove(name);
            } else if diagnostics {
                unused_imports.shift_remove(name);
            }
        }
    }

    updated
}
