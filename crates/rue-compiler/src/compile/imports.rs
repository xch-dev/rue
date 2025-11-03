use rue_ast::{AstImportItem, AstImportPathSegment};
use rue_hir::{Import, ImportId, Items, ModuleDeclarations, ScopeId, Symbol};
use rue_parser::SyntaxToken;

use crate::Compiler;

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

    if let Some(name) = last.name() {
        vec![ctx.alloc_import(Import {
            path,
            items: Items::Named(vec![name]),
            exported,
        })]
    } else if let Some(star) = last.star() {
        vec![ctx.alloc_import(Import {
            path,
            items: Items::All(star),
            exported,
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

pub fn resolve_imports(ctx: &mut Compiler, declarations: &ModuleDeclarations) {
    let imports = flatten_imports(ctx, declarations);

    let mut updated = true;

    while updated {
        updated = false;

        for &(target_scope, import) in &imports {
            updated |= resolve_import(ctx, target_scope, import);
        }
    }
}

fn resolve_import(ctx: &mut Compiler, target_scope: ScopeId, import: ImportId) -> bool {
    let import = ctx.import(import).clone();

    let mut base = None;

    for ident in &import.path {
        let name = ident.text();

        let symbol = if let Some(base) = base {
            let Some(symbol) = ctx.scope(base).symbol(name) else {
                return false;
            };
            symbol
        } else {
            let Some(symbol) = ctx.resolve_symbol_in(target_scope, name) else {
                return false;
            };
            symbol
        };

        let Symbol::Module(module) = ctx.symbol(symbol) else {
            return false;
        };

        base = Some(module.scope);
    }

    let mut updated = false;

    match import.items {
        Items::All(_) => {
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
                    target.insert_symbol(name, symbol, import.exported);
                    updated = true;
                } else if !target.is_symbol_exported(symbol) && import.exported {
                    target.export_symbol(symbol);
                    updated = true;
                }
            }

            for (name, ty) in types {
                let target = ctx.scope_mut(target_scope);

                if target.symbol(&name).is_none() {
                    target.insert_type(name, ty, import.exported);
                    updated = true;
                } else if !target.is_type_exported(ty) && import.exported {
                    target.export_type(ty);
                    updated = true;
                }
            }
        }
        Items::Named(items) => {
            for item in items {
                let name = item.text();

                let (symbol, ty) = if let Some(base) = base {
                    let base = ctx.scope(base);
                    let symbol = base.symbol(name).filter(|s| base.is_symbol_exported(*s));
                    let ty = base.ty(name).filter(|t| base.is_type_exported(*t));
                    (symbol, ty)
                } else {
                    let symbol = ctx.resolve_symbol_in(target_scope, name);
                    let ty = ctx.resolve_type_in(target_scope, name);
                    (symbol, ty)
                };

                if let Some(symbol) = symbol {
                    let target = ctx.scope_mut(target_scope);

                    if target.symbol(name).is_none() {
                        target.insert_symbol(name.to_string(), symbol, import.exported);
                        updated = true;
                    } else if !target.is_symbol_exported(symbol) && import.exported {
                        target.export_symbol(symbol);
                        updated = true;
                    }
                }

                if let Some(ty) = ty {
                    let target = ctx.scope_mut(target_scope);

                    if target.ty(name).is_none() {
                        target.insert_type(name.to_string(), ty, import.exported);
                        updated = true;
                    } else if !target.is_type_exported(ty) && import.exported {
                        target.export_type(ty);
                        updated = true;
                    }
                }
            }
        }
    }

    updated
}

fn flatten_imports(
    ctx: &mut Compiler,
    declarations: &ModuleDeclarations,
) -> Vec<(ScopeId, ImportId)> {
    let mut imports = Vec::new();
    let mut stack = vec![(ctx.last_scope_id(), declarations.modules.clone())];

    while let Some((scope, modules)) = stack.pop() {
        for import in ctx.scope(scope).imports() {
            imports.push((scope, import));
        }

        for module in modules {
            let Symbol::Module(module) = ctx.symbol(module).clone() else {
                unreachable!();
            };

            stack.push((module.scope, module.declarations.modules.clone()));
        }
    }

    imports
}
