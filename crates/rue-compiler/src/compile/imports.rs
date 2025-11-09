use std::collections::HashMap;

use indexmap::IndexMap;
use rue_ast::{AstImportItem, AstImportPathSegment};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Declaration, Import, ImportId, Items, ScopeId, Symbol, SymbolId};
use rue_parser::SyntaxToken;

use crate::{Compiler, SyntaxItem, SyntaxItemKind};

pub fn declare_import_item(ctx: &mut Compiler, import: &AstImportItem) {
    let Some(path) = import.path() else {
        return;
    };

    let imports = construct_imports(
        ctx,
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
    mut path: Vec<SyntaxToken>,
    segments: &[AstImportPathSegment],
    exported: bool,
) -> Vec<ImportId> {
    for segment in segments.iter().take(segments.len() - 1) {
        if let Some(name) = segment.name() {
            path.push(name);
        }
    }

    let Some(last) = segments.last() else {
        return vec![];
    };

    let source = ctx.source().clone();

    if let Some(name) = last.name() {
        vec![ctx.alloc_import(Import {
            source,
            path,
            items: Items::Named(vec![name]),
            exported,
            declarations: Vec::new(),
        })]
    } else if let Some(star) = last.star() {
        vec![ctx.alloc_import(Import {
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
    scopes: HashMap<Vec<String>, ScopeId>,
    unused_imports: IndexMap<ImportId, IndexMap<String, SyntaxToken>>,
    glob_import_counts: IndexMap<ImportId, (SyntaxToken, usize)>,
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

        for &(target_scope, import) in &imports {
            updated |= resolve_import(
                ctx,
                target_scope,
                import,
                diagnostics,
                &mut cache.scopes,
                &mut missing_imports,
                &mut cache.unused_imports,
                &mut cache.glob_import_counts,
            );
        }
    }

    if diagnostics {
        for missing_imports in missing_imports.into_values() {
            for (name, token) in missing_imports {
                ctx.diagnostic(&token, DiagnosticKind::UnresolvedImport(name.to_string()));
            }
        }

        for unused_imports in cache.unused_imports.values() {
            for (name, token) in unused_imports {
                ctx.diagnostic(token, DiagnosticKind::UnusedImport(name.to_string()));
            }
        }

        for (token, count) in cache.glob_import_counts.values() {
            if *count == 0 {
                ctx.diagnostic(token, DiagnosticKind::UnusedGlobImport);
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn resolve_import(
    ctx: &mut Compiler,
    target_scope: ScopeId,
    import_id: ImportId,
    diagnostics: bool,
    cache: &mut HashMap<Vec<String>, ScopeId>,
    missing_imports: &mut IndexMap<ImportId, IndexMap<String, SyntaxToken>>,
    unused_imports: &mut IndexMap<ImportId, IndexMap<String, SyntaxToken>>,
    glob_import_counts: &mut IndexMap<ImportId, (SyntaxToken, usize)>,
) -> bool {
    let import = ctx.import(import_id).clone();
    let source = import.source.kind.clone();

    let mut base = None;
    let mut path_so_far = Vec::new();

    for i in (1..=import.path.len()).rev() {
        let subpath = import
            .path
            .iter()
            .take(i)
            .map(|t| t.text().to_string())
            .collect::<Vec<_>>();

        if let Some(cached) = cache.get(&subpath) {
            base = Some(*cached);
            path_so_far = subpath;
            break;
        }
    }

    for ident in import.path.iter().skip(path_so_far.len()) {
        let name = ident.text();

        let symbol = if let Some(base) = base {
            let base = ctx.scope(base);
            base.symbol(name).map(|s| (s, base.symbol_import(s)))
        } else {
            ctx.resolve_symbol_in(target_scope, name)
        };

        let Some((symbol, import)) = symbol else {
            if diagnostics {
                ctx.diagnostic(ident, DiagnosticKind::UndeclaredSymbol(name.to_string()));
            }
            return false;
        };

        if let Some(import) = import {
            ctx.add_import_reference(import, Declaration::Symbol(symbol));
        }

        ctx.syntax_map_for_source(source.clone())
            .add_item(SyntaxItem::new(
                SyntaxItemKind::SymbolReference(symbol),
                ident.text_range(),
            ));

        let Symbol::Module(module) = ctx.symbol(symbol) else {
            if diagnostics {
                ctx.diagnostic(ident, DiagnosticKind::SubpathNotSupported(name.to_string()));
            }
            return false;
        };

        base = Some(module.scope);
        path_so_far.push(name.to_string());
        cache.insert(path_so_far.clone(), module.scope);
    }

    let mut updated = false;

    match import.items {
        Items::All(star) => {
            let count = &mut glob_import_counts
                .entry(import_id)
                .or_insert_with(|| (star.clone(), 0))
                .1;

            let symbols = if let Some(base) = base {
                ctx.scope(base)
                    .exported_symbols()
                    .map(|(name, symbol)| (name.to_string(), symbol))
                    .collect::<Vec<_>>()
            } else {
                let target = ctx.scope(target_scope);

                target
                    .symbol_names()
                    .map(|name| (name.to_string(), target.symbol(name).unwrap()))
                    .collect::<Vec<_>>()
            };

            let types = if let Some(base) = base {
                ctx.scope(base)
                    .exported_types()
                    .map(|(name, ty)| (name.to_string(), ty))
                    .collect::<Vec<_>>()
            } else {
                let target = ctx.scope(target_scope);

                target
                    .type_names()
                    .map(|name| (name.to_string(), target.ty(name).unwrap()))
                    .collect::<Vec<_>>()
            };

            for (name, symbol) in symbols {
                let target = ctx.scope_mut(target_scope);

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
                let target = ctx.scope_mut(target_scope);

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
        Items::Named(items) => {
            let missing_imports = missing_imports.entry(import_id).or_insert_with(|| {
                items
                    .iter()
                    .map(|item| (item.text().to_string(), item.clone()))
                    .collect()
            });

            let unused_imports = unused_imports.entry(import_id).or_insert_with(|| {
                items
                    .iter()
                    .map(|item| (item.text().to_string(), item.clone()))
                    .collect()
            });

            for item in items {
                let name = item.text();

                let (symbol, ty) = if let Some(base) = base {
                    let base = ctx.scope(base);
                    let symbol = base.symbol(name).filter(|s| base.is_symbol_exported(*s));
                    let ty = base.ty(name).filter(|t| base.is_type_exported(*t));
                    (symbol, ty)
                } else {
                    let symbol = ctx.resolve_symbol_in(target_scope, name).map(|(s, _)| s);
                    let ty = ctx.resolve_type_in(target_scope, name).map(|(t, _)| t);
                    (symbol, ty)
                };

                if let Some(symbol) = symbol {
                    if diagnostics {
                        ctx.syntax_map_for_source(source.clone())
                            .add_item(SyntaxItem::new(
                                SyntaxItemKind::SymbolReference(symbol),
                                item.text_range(),
                            ));
                    }

                    let target = ctx.scope_mut(target_scope);

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
                    if diagnostics {
                        ctx.syntax_map_for_source(source.clone())
                            .add_item(SyntaxItem::new(
                                SyntaxItemKind::TypeReference(ty),
                                item.text_range(),
                            ));
                    }

                    let target = ctx.scope_mut(target_scope);

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
    }

    updated
}
