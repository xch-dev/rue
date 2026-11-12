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

    let qualified = &input.sites.qualified_references;
    let export_keywords = export_keywords(input);
    let imported = imported(db);

    let mut reports = Vec::new();

    for &module in modules {
        if is_public(db, &scopes, module) {
            continue;
        }

        let module_symbol = db.module(module);
        let module_name = module_symbol.name.as_ref().map_or("", |name| name.text());

        // The exports of modules that are exported from this one are what it exports too
        let mut exports = Vec::new();
        collect_exports(db, module, true, &mut exports);

        let used = used_exports(db, &exports, |declaration| {
            qualified.contains(&declaration) || imported.contains(&declaration)
        });

        for &(declaration, ref name, direct) in &exports {
            let Some(name_srcloc) = name.srcloc() else {
                continue;
            };

            // The exports of nested modules are reported when checking those modules, and
            // anything that isn't used at all is reported by the `unused` lints instead
            if !direct
                || used.contains(&declaration)
                || name.text().starts_with('_')
                || db.reference_parents(declaration).is_empty()
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

/// The exported items of the module, along with their names and whether they're declared in it
/// directly (rather than in a module that it exports).
fn collect_exports(
    db: &Database,
    module: SymbolId,
    direct: bool,
    exports: &mut Vec<(Declaration, Name, bool)>,
) {
    let module = db.module(module);
    let scope = db.scope(module.scope);
    let declarations = &module.declarations;

    for &symbol in &declarations.symbols {
        if scope.is_symbol_exported(symbol)
            && let Some(name) = symbol_name(db, symbol)
        {
            exports.push((Declaration::Symbol(symbol), name, direct));
        }
    }

    for &(ty, _) in &declarations.types {
        if scope.is_type_exported(ty)
            && let Some(name) = type_name(db, ty)
        {
            exports.push((Declaration::Type(ty), name, direct));
        }
    }

    for &nested in &declarations.modules {
        if scope.is_symbol_exported(nested) {
            collect_exports(db, nested, false, exports);
        }
    }
}

/// The exports that need to be exported: the ones that are used from outside of the module, and
/// the types in the signatures of those (and so on), which are part of what the module exports
/// even if nothing outside of it names them.
fn used_exports(
    db: &Database,
    exports: &[(Declaration, Name, bool)],
    is_used: impl Fn(Declaration) -> bool,
) -> HashSet<Declaration> {
    let mut used = exports
        .iter()
        .map(|&(declaration, ..)| declaration)
        .filter(|&declaration| is_used(declaration))
        .collect::<HashSet<_>>();

    let mut stack = used.iter().copied().collect::<Vec<_>>();

    while let Some(declaration) = stack.pop() {
        for ty in signature_types(db, declaration) {
            for (export, name, _) in exports {
                // A type that refers to itself (such as a recursive struct) isn't kept exported
                // by its own signature
                if *export == declaration
                    || used.contains(export)
                    || !matches!(export, Declaration::Type(_))
                {
                    continue;
                }

                if let Some(srcloc) = name.srcloc()
                    && type_reaches(db, ty, srcloc, &mut HashSet::new())
                {
                    used.insert(*export);
                    stack.push(*export);
                }
            }
        }
    }

    used
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

/// The types in the signature of an item, such as the parameters and return type of a function,
/// or the fields of a struct.
fn signature_types(db: &Database, declaration: Declaration) -> Vec<TypeId> {
    match declaration {
        Declaration::Symbol(symbol) => match db.symbol(symbol) {
            Symbol::Function(function) => std::iter::once(function.return_type)
                .chain(function.parameters.values().filter_map(|&parameter| {
                    match db.symbol(parameter) {
                        Symbol::Parameter(parameter) => Some(parameter.ty),
                        _ => None,
                    }
                }))
                .collect(),
            Symbol::Constant(constant) => vec![constant.value.ty],
            _ => Vec::new(),
        },
        Declaration::Type(ty) => match db.ty(ty) {
            Type::Struct(ty) => vec![ty.inner],
            Type::Alias(alias) => vec![alias.inner],
            _ => Vec::new(),
        },
    }
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
