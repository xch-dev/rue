use indexmap::IndexMap;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Declaration, Import, ScopeId, Symbol};

use crate::Compiler;

pub fn resolve_imports(ctx: &mut Compiler, scope_id: ScopeId, include_diagnostics: bool) {
    let imports = ctx.scope(scope_id).imports();

    resolve_import_map(ctx, scope_id, None, &imports, include_diagnostics);
}

fn resolve_import_map(
    ctx: &mut Compiler,
    base_scope_id: ScopeId,
    parent: Option<Declaration>,
    imports: &IndexMap<String, Import>,
    include_diagnostics: bool,
) {
    for (name, import) in imports {
        let (scope_id, symbol_names, type_names) = match parent {
            None => {
                let scope = ctx.scope(base_scope_id);
                let symbol_names = scope
                    .symbol_names()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>();
                let type_names = scope
                    .type_names()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>();
                (base_scope_id, symbol_names, type_names)
            }
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

        let symbol = if symbol_names.contains(name) {
            ctx.scope(scope_id).symbol(name)
        } else {
            None
        };

        let ty = if type_names.contains(name) {
            ctx.scope(scope_id).ty(name)
        } else {
            None
        };

        if import.include_self {
            if let Some(symbol) = symbol {
                if let Some(existing) = ctx.scope(base_scope_id).symbol(name)
                    && existing != symbol
                    && include_diagnostics
                {
                    ctx.diagnostic(
                        &import.name,
                        DiagnosticKind::DuplicateImportSymbol(name.clone()),
                    );
                }

                if ctx.scope(base_scope_id).symbol(name).is_none() {
                    ctx.scope_mut(base_scope_id).insert_symbol(
                        name.clone(),
                        symbol,
                        import.exported,
                    );
                }
            }

            if let Some(ty) = ty {
                if let Some(existing) = ctx.scope(base_scope_id).ty(name)
                    && existing != ty
                    && include_diagnostics
                {
                    ctx.diagnostic(
                        &import.name,
                        DiagnosticKind::DuplicateImportType(name.clone()),
                    );
                }

                if ctx.scope(base_scope_id).ty(name).is_none() {
                    ctx.scope_mut(base_scope_id)
                        .insert_type(name.clone(), ty, import.exported);
                }
            }

            if symbol.is_none() && ty.is_none() && include_diagnostics {
                ctx.diagnostic(&import.name, DiagnosticKind::UnresolvedImport(name.clone()));
            }
        }

        if import.include_all {
            for name in symbol_names {
                let symbol = ctx.scope(scope_id).symbol(&name).unwrap();

                if ctx.scope(base_scope_id).symbol(&name).is_none() {
                    ctx.scope_mut(base_scope_id)
                        .insert_symbol(name, symbol, import.exported);
                }
            }

            for name in type_names {
                let ty = ctx.scope(scope_id).ty(&name).unwrap();

                if ctx.scope(base_scope_id).ty(&name).is_none() {
                    ctx.scope_mut(base_scope_id)
                        .insert_type(name, ty, import.exported);
                }
            }
        }

        if let Some(symbol) = symbol {
            resolve_import_map(
                ctx,
                base_scope_id,
                Some(Declaration::Symbol(symbol)),
                &import.children,
                include_diagnostics,
            );
        }

        if let Some(ty) = ty {
            resolve_import_map(
                ctx,
                base_scope_id,
                Some(Declaration::Type(ty)),
                &import.children,
                include_diagnostics,
            );
        }
    }
}
