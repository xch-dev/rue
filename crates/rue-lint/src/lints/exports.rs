use std::collections::{HashMap, HashSet};

use rue_ast::{AstItem, AstNode, AstSymbolItem, AstTypeItem};
use rue_diagnostic::{Applicability, Lint, Name, SourceKind, SrcLoc, Suggestion};
use rue_hir::{Database, Declaration, Items, ScopeId, Symbol, SymbolId};
use rue_parser::SyntaxKind;
use rue_types::{Type, TypeId};

use crate::{LintContext, LintInput, srcloc};

pub(crate) const LINTS: &[Lint] = &[Lint::UnusedExport];

/// Reports the exported items of `mod` items that are never used in a way that needs them to be
/// exported, so they don't need to be. Only a reference through the module (such as
/// `module::name`) or an import needs an item to be exported, since code inside of the module (or
/// a module nested inside of it) can reference it by name. Modules that are exported themselves
/// (from a file, or from another module that is) are left alone, since anything that imports the
/// file can use their items.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    let db = cx.db();

    let modules = &input.sites.modules;

    let scopes = modules
        .iter()
        .map(|&module| (db.module(module).scope, module))
        .collect::<HashMap<ScopeId, SymbolId>>();

    let qualified = input
        .sites
        .qualified_references
        .iter()
        .copied()
        .collect::<HashSet<_>>();

    let export_keywords = export_keywords(input);
    let imported = imported(db);

    let mut reports = Vec::new();

    for &module in modules {
        if is_public(db, &scopes, module) {
            continue;
        }

        let module_symbol = db.module(module);
        let scope = db.scope(module_symbol.scope);
        let module_name = module_symbol.name.as_ref().map_or("", |name| name.text());

        let declarations = &module_symbol.declarations;

        let exported_symbols = declarations
            .symbols
            .iter()
            .copied()
            .filter(|&symbol| scope.is_symbol_exported(symbol))
            .collect::<Vec<_>>();

        let exported_types = declarations
            .types
            .iter()
            .map(|&(ty, _)| ty)
            .filter(|&ty| scope.is_type_exported(ty))
            .collect::<Vec<_>>();

        let items = exported_symbols
            .iter()
            .filter_map(|&symbol| Some((Declaration::Symbol(symbol), symbol_name(db, symbol)?)))
            .chain(
                exported_types
                    .iter()
                    .filter_map(|&ty| Some((Declaration::Type(ty), type_name(db, ty)?))),
            );

        for (declaration, name) in items {
            let Some(name_srcloc) = name.srcloc() else {
                continue;
            };

            // Anything that isn't used at all is reported by the `unused` lints instead
            if name.text().starts_with('_')
                || db.reference_parents(declaration).is_empty()
                || qualified.contains(&declaration)
                || imported.contains(&declaration)
            {
                continue;
            }

            // A type in the signature of another export is part of what the module exports, even
            // if nothing outside of it names the type
            if let Declaration::Type(_) = declaration
                && signature_types(db, &exported_symbols, &exported_types)
                    .into_iter()
                    .any(|ty| type_reaches(db, ty, name_srcloc, &mut HashSet::new()))
            {
                continue;
            }

            reports.push((
                name_srcloc.clone(),
                format!(
                    "`{}` is exported, but is never used outside of module `{module_name}`",
                    name.text()
                ),
                export_keywords
                    .get(&(name_srcloc.source.kind.clone(), name_srcloc.span.start))
                    .cloned(),
            ));
        }
    }

    for (srcloc, message, export) in reports {
        let Some(diagnostic) = cx.report(Lint::UnusedExport, srcloc, message) else {
            continue;
        };

        if let Some(export) = export {
            diagnostic.suggestions.push(Suggestion::replace(
                "Remove `export`",
                export,
                "",
                Applicability::MachineApplicable,
            ));
        }
    }
}

/// Whether the module can be used by anything that imports the file it's in, because it's
/// exported from the file, or from another module that is.
fn is_public(db: &Database, scopes: &HashMap<ScopeId, SymbolId>, module: SymbolId) -> bool {
    let Some(parent) = db.scope(db.module(module).scope).parent() else {
        return false;
    };

    if !db.scope(parent).is_symbol_exported(module) {
        return false;
    }

    match scopes.get(&parent) {
        Some(&parent) => is_public(db, scopes, parent),
        // The module is declared directly in a file
        None => true,
    }
}

/// The declarations that an import (or re-export) resolves to, which it can only do if they're
/// exported. A glob import that isn't exported only needs the items that are used through it.
fn imported(db: &Database) -> HashSet<Declaration> {
    let mut imported = HashSet::new();

    for id in db.relevant_imports() {
        let import = db.import(id);

        match &import.items {
            Items::All(_) if !import.exported => imported.extend(db.import_references(id)),
            Items::All(_) | Items::Named(_) => {
                imported.extend(
                    import
                        .declarations
                        .iter()
                        .map(|&(_, declaration)| declaration),
                );
            }
        }
    }

    imported
}

/// The types in the signatures of the exported items of a module, such as the parameters and
/// return type of a function, or the fields of a struct.
fn signature_types(db: &Database, symbols: &[SymbolId], types: &[TypeId]) -> Vec<TypeId> {
    let mut signature = Vec::new();

    for &symbol in symbols {
        match db.symbol(symbol) {
            Symbol::Function(function) => {
                signature.push(function.return_type);

                for &parameter in function.parameters.values() {
                    if let Symbol::Parameter(parameter) = db.symbol(parameter) {
                        signature.push(parameter.ty);
                    }
                }
            }
            Symbol::Constant(constant) => signature.push(constant.value.ty),
            _ => {}
        }
    }

    for &ty in types {
        match db.ty(ty) {
            Type::Struct(ty) => signature.push(ty.inner),
            Type::Alias(alias) => signature.push(alias.inner),
            _ => {}
        }
    }

    signature
}

/// Whether the type is (or contains) the struct or alias declared with the given name.
fn type_reaches(db: &Database, ty: TypeId, name: &SrcLoc, visited: &mut HashSet<TypeId>) -> bool {
    if !visited.insert(ty) {
        return false;
    }

    let is_named = |declared: &Option<Name>| {
        declared
            .as_ref()
            .and_then(Name::srcloc)
            .is_some_and(|srcloc| {
                srcloc.source.kind == name.source.kind && srcloc.span == name.span
            })
    };

    match db.ty(ty) {
        Type::Struct(ty) => {
            is_named(&ty.name)
                || type_reaches(db, ty.inner, name, visited)
                || ty
                    .generics
                    .iter()
                    .any(|&generic| type_reaches(db, generic, name, visited))
        }
        Type::Alias(alias) => {
            is_named(&alias.name)
                || type_reaches(db, alias.inner, name, visited)
                || alias
                    .generics
                    .iter()
                    .any(|&generic| type_reaches(db, generic, name, visited))
        }
        Type::Ref(inner) => type_reaches(db, *inner, name, visited),
        Type::Pair(pair) => {
            type_reaches(db, pair.first, name, visited)
                || type_reaches(db, pair.rest, name, visited)
        }
        Type::Union(union) => union
            .types
            .iter()
            .any(|&ty| type_reaches(db, ty, name, visited)),
        Type::Apply(apply) => {
            type_reaches(db, apply.inner, name, visited)
                || apply
                    .generics
                    .values()
                    .any(|&generic| type_reaches(db, generic, name, visited))
        }
        Type::Function(function) => {
            type_reaches(db, function.ret, name, visited)
                || function
                    .params
                    .values()
                    .any(|&param| type_reaches(db, param, name, visited))
        }
        Type::Unresolved | Type::Generic(_) | Type::Any | Type::Never | Type::Atom(_) => false,
    }
}

fn symbol_name(db: &Database, symbol: SymbolId) -> Option<Name> {
    match db.symbol(symbol) {
        Symbol::Function(_) | Symbol::Constant(_) | Symbol::Binding(_) | Symbol::Module(_) => {
            db.symbol(symbol).name()
        }
        Symbol::Unresolved | Symbol::Builtin(_) | Symbol::Parameter(_) => None,
    }
}

fn type_name(db: &Database, ty: TypeId) -> Option<Name> {
    match db.ty(ty) {
        Type::Struct(ty) => ty.name.clone(),
        Type::Alias(ty) => ty.name.clone(),
        Type::Generic(ty) => ty.name.clone(),
        _ => None,
    }
}

/// The location of the `export` keyword (and the space after it) of each item, by the location of
/// the item's name.
fn export_keywords(input: &LintInput<'_>) -> HashMap<(SourceKind, usize), SrcLoc> {
    let mut keywords = HashMap::new();

    for &(source, document) in &input.documents {
        for item in document.syntax().descendants().filter_map(AstItem::cast) {
            let (export, name) = match item {
                AstItem::SymbolItem(AstSymbolItem::FunctionItem(item)) => {
                    (item.export(), item.name())
                }
                AstItem::SymbolItem(AstSymbolItem::ConstantItem(item)) => {
                    (item.export(), item.name())
                }
                AstItem::SymbolItem(AstSymbolItem::ModuleItem(item)) => {
                    (item.export(), item.name())
                }
                AstItem::TypeItem(AstTypeItem::StructItem(item)) => (item.export(), item.name()),
                AstItem::TypeItem(AstTypeItem::TypeAliasItem(item)) => (item.export(), item.name()),
                AstItem::ImportItem(_) => continue,
            };

            let (Some(export), Some(name)) = (export, name) else {
                continue;
            };

            let start = export.text_range().start();
            let end = export
                .next_token()
                .filter(|token| token.kind() == SyntaxKind::Whitespace)
                .map_or(export.text_range().end(), |token| token.text_range().end());

            keywords.insert(
                (source.kind.clone(), usize::from(name.text_range().start())),
                srcloc(source, rowan::TextRange::new(start, end)),
            );
        }
    }

    keywords
}
