use std::collections::HashMap;

use indexmap::IndexMap;
use rowan::TextRange;
use rue_ast::{AstImportItem, AstImportPathSegment};
use rue_diagnostic::{DiagnosticKind, Name};
use rue_hir::{Declaration, Import, ImportId, Items, ScopeId, Symbol, SymbolId};

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
        ctx.diagnostic(&name, DiagnosticKind::SuperAtEnd);
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
    scopes: HashMap<Vec<String>, (ScopeId, SymbolId)>,
    unused_imports: IndexMap<ImportId, IndexMap<String, Name>>,
    glob_import_counts: IndexMap<ImportId, (Name, usize)>,
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
    let imports = flatten_imports(ctx, all_modules);

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

        for unused_imports in cache.unused_imports.values() {
            for name in unused_imports.values() {
                ctx.diagnostic_name(name, DiagnosticKind::UnusedImport(name.text().to_string()));
            }
        }

        for (name, count) in cache.glob_import_counts.values() {
            if *count == 0 {
                ctx.diagnostic_name(name, DiagnosticKind::UnusedGlobImport);
            }
        }
    }
}

fn resolve_import(
    ctx: &mut Compiler,
    import_scope: ScopeId,
    import_id: ImportId,
    diagnostics: bool,
    cache: &mut ImportCache,
    missing_imports: &mut IndexMap<ImportId, IndexMap<String, Name>>,
) -> bool {
    let import = ctx.import(import_id).clone();
    let source = import.source.clone();

    let mut base = None;
    let mut path_so_far = Vec::new();

    for i in (1..=import.path.len()).rev() {
        let subpath = import
            .path
            .iter()
            .take(i)
            .map(|t| t.text().to_string())
            .collect::<Vec<_>>();

        if let Some(cached) = cache.scopes.get(&subpath) {
            base = Some(cached.0);
            path_so_far = subpath;

            for name in import.path.iter().take(path_so_far.len()) {
                if diagnostics && let Some(srcloc) = name.srcloc() {
                    ctx.add_syntax_for_source(
                        SyntaxItemKind::SymbolReference(cached.1),
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

    for name in import.path.iter().skip(path_so_far.len()) {
        let symbol = if let Some(base) = base {
            let base = ctx.scope(base);
            base.symbol(name.text())
                .filter(|s| base.is_symbol_exported(*s))
                .map(|s| (s, base.symbol_import(s)))
        } else {
            ctx.resolve_symbol_in(import.base_scope, name.text())
                .filter(|s| {
                    import.base_scope == import_scope
                        || ctx.scope(import.base_scope).is_symbol_exported(s.0)
                })
        };

        let Some((symbol, import)) = symbol else {
            if diagnostics {
                ctx.diagnostic_name(
                    name,
                    DiagnosticKind::UndeclaredSymbol(name.text().to_string()),
                );
            }
            return false;
        };

        if let Some(import) = import {
            ctx.add_import_reference(import, Declaration::Symbol(symbol));
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
        cache
            .scopes
            .insert(path_so_far.clone(), (module.scope, symbol));
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
                let symbol = base.symbol(name).filter(|s| base.is_symbol_exported(*s));
                let ty = base.ty(name).filter(|t| base.is_type_exported(*t));
                (symbol, ty)
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
