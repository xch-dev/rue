use rue_diagnostic::DiagnosticKind;
use rue_hir::{Declaration, ImportId, Symbol};

use crate::Compiler;

pub fn resolve_imports(ctx: &mut Compiler, include_diagnostics: bool) {
    let imports = ctx.last_scope().imports();

    resolve_import_map(ctx, None, &imports, include_diagnostics);
}

fn resolve_import_map(
    ctx: &mut Compiler,
    parent: Option<Declaration>,
    imports: &[ImportId],
    include_diagnostics: bool,
) {
    for &import in imports {
        let import = ctx.import(import).clone();

        let name = import.name.text();

        let (symbol, ty) = match parent {
            None => {
                let symbol = ctx.resolve_symbol(name);
                let ty = ctx.resolve_type(name);
                (symbol, ty)
            }
            Some(Declaration::Symbol(symbol)) => {
                let Symbol::Module(module) = ctx.symbol(symbol) else {
                    if include_diagnostics {
                        let symbol_name = ctx.symbol_name(symbol);
                        ctx.diagnostic(
                            &import.name,
                            DiagnosticKind::CannotImportFromSymbol(name.to_string(), symbol_name),
                        );
                    }
                    continue;
                };
                let scope = ctx.scope(module.scope);
                let symbol = scope.symbol(name).filter(|s| scope.is_symbol_exported(*s));
                let ty = scope.ty(name).filter(|t| scope.is_type_exported(*t));
                (symbol, ty)
            }
            Some(Declaration::Type(ty)) => {
                if include_diagnostics {
                    let type_name = ctx.type_name(ty);
                    ctx.diagnostic(
                        &import.name,
                        DiagnosticKind::CannotImportFromType(name.to_string(), type_name),
                    );
                }
                continue;
            }
        };

        if symbol.is_none() && ty.is_none() && include_diagnostics {
            ctx.diagnostic(
                &import.name,
                DiagnosticKind::UnresolvedImport(name.to_string()),
            );
        }

        if import.include_self {
            if let Some(symbol) = symbol {
                if let Some(existing) = ctx.last_scope().symbol(name)
                    && existing != symbol
                    && include_diagnostics
                {
                    ctx.diagnostic(
                        &import.name,
                        DiagnosticKind::DuplicateImportSymbol(name.to_string()),
                    );
                }

                if ctx.last_scope().symbol(name).is_none() {
                    ctx.last_scope_mut()
                        .insert_symbol(name.to_string(), symbol, import.exported);
                }
            }

            if let Some(ty) = ty {
                if let Some(existing) = ctx.last_scope().ty(name)
                    && existing != ty
                    && include_diagnostics
                {
                    ctx.diagnostic(
                        &import.name,
                        DiagnosticKind::DuplicateImportType(name.to_string()),
                    );
                }

                if ctx.last_scope().ty(name).is_none() {
                    ctx.last_scope_mut()
                        .insert_type(name.to_string(), ty, import.exported);
                }
            }
        }

        if import.include_all {
            if let Some(symbol) = symbol {
                if let Symbol::Module(module) = ctx.symbol(symbol).clone() {
                    let symbol_names = ctx
                        .scope(module.scope)
                        .exported_symbols()
                        .map(|(name, _)| name.to_string())
                        .collect::<Vec<_>>();
                    let type_names = ctx
                        .scope(module.scope)
                        .exported_types()
                        .map(|(name, _)| name.to_string())
                        .collect::<Vec<_>>();

                    for name in symbol_names {
                        let symbol = ctx.scope(module.scope).symbol(&name).unwrap();

                        if ctx.last_scope().symbol(&name).is_none() {
                            ctx.last_scope_mut()
                                .insert_symbol(name, symbol, import.exported);
                        }
                    }

                    for name in type_names {
                        let ty = ctx.scope(module.scope).ty(&name).unwrap();

                        if ctx.last_scope().ty(&name).is_none() {
                            ctx.last_scope_mut().insert_type(name, ty, import.exported);
                        }
                    }
                } else if include_diagnostics {
                    let symbol_name = ctx.symbol_name(symbol);
                    ctx.diagnostic(
                        &import.name,
                        DiagnosticKind::CannotImportFromSymbol(name.to_string(), symbol_name),
                    );
                }
            } else if let Some(ty) = ty
                && include_diagnostics
            {
                let type_name = ctx.type_name(ty);
                ctx.diagnostic(
                    &import.name,
                    DiagnosticKind::CannotImportFromType("*".to_string(), type_name),
                );
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
