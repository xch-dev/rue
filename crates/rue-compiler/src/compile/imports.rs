use indexmap::IndexMap;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Declaration, Import, Symbol};

use crate::Compiler;

pub fn resolve_imports(ctx: &mut Compiler, include_diagnostics: bool) {
    let imports = ctx.last_scope().imports();

    resolve_import_map(ctx, None, &imports, include_diagnostics);
}

fn resolve_import_map(
    ctx: &mut Compiler,
    parent: Option<Declaration>,
    imports: &IndexMap<String, Import>,
    include_diagnostics: bool,
) {
    for (name, import) in imports {
        let (scope_id, symbol_names, type_names) = match parent {
            None => (ctx.last_scope_id(), Vec::new(), Vec::new()),
            Some(Declaration::Symbol(symbol)) => {
                let Symbol::Module(module) = ctx.symbol(symbol) else {
                    if include_diagnostics {
                        let symbol_name = ctx.symbol_name(symbol);
                        ctx.diagnostic(
                            &import.name,
                            DiagnosticKind::CannotImportFromSymbol(name.clone(), symbol_name),
                        );
                    }
                    continue;
                };
                let scope = ctx.scope(module.scope);
                let symbol_names = scope
                    .exported_symbols()
                    .map(|(name, _)| name.to_string())
                    .collect::<Vec<_>>();
                let type_names = scope
                    .exported_types()
                    .map(|(name, _)| name.to_string())
                    .collect::<Vec<_>>();
                (module.scope, symbol_names, type_names)
            }
            Some(Declaration::Type(ty)) => {
                if include_diagnostics {
                    let type_name = ctx.type_name(ty);
                    ctx.diagnostic(
                        &import.name,
                        DiagnosticKind::CannotImportFromType(name.clone(), type_name),
                    );
                }
                continue;
            }
        };

        let symbol = if parent.is_none() {
            ctx.resolve_symbol(name)
        } else if symbol_names.contains(name) {
            ctx.scope(scope_id).symbol(name)
        } else {
            None
        };

        let ty = if parent.is_none() {
            ctx.resolve_type(name)
        } else if type_names.contains(name) {
            ctx.scope(scope_id).ty(name)
        } else {
            None
        };

        if import.include_self {
            if let Some(symbol) = symbol {
                if let Some(existing) = ctx.last_scope().symbol(name)
                    && existing != symbol
                    && include_diagnostics
                {
                    ctx.diagnostic(
                        &import.name,
                        DiagnosticKind::DuplicateImportSymbol(name.clone()),
                    );
                }

                if ctx.last_scope().symbol(name).is_none() {
                    ctx.last_scope_mut()
                        .insert_symbol(name.clone(), symbol, import.exported);
                }
            }

            if let Some(ty) = ty {
                if let Some(existing) = ctx.last_scope().ty(name)
                    && existing != ty
                    && include_diagnostics
                {
                    ctx.diagnostic(
                        &import.name,
                        DiagnosticKind::DuplicateImportType(name.clone()),
                    );
                }

                if ctx.last_scope().ty(name).is_none() {
                    ctx.last_scope_mut()
                        .insert_type(name.clone(), ty, import.exported);
                }
            }

            if symbol.is_none() && ty.is_none() && include_diagnostics {
                ctx.diagnostic(&import.name, DiagnosticKind::UnresolvedImport(name.clone()));
            }
        }

        if import.include_all && parent.is_some() {
            for name in symbol_names {
                let symbol = ctx.scope(scope_id).symbol(&name).unwrap();

                if ctx.last_scope().symbol(&name).is_none() {
                    ctx.last_scope_mut()
                        .insert_symbol(name, symbol, import.exported);
                }
            }

            for name in type_names {
                let ty = ctx.scope(scope_id).ty(&name).unwrap();

                if ctx.last_scope().ty(&name).is_none() {
                    ctx.last_scope_mut().insert_type(name, ty, import.exported);
                }
            }
        }

        if let Some(symbol) = symbol {
            resolve_import_map(
                ctx,
                Some(Declaration::Symbol(symbol)),
                &import.children,
                include_diagnostics,
            );
        }

        if let Some(ty) = ty {
            resolve_import_map(
                ctx,
                Some(Declaration::Type(ty)),
                &import.children,
                include_diagnostics,
            );
        }
    }
}
