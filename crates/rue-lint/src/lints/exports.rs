use std::collections::{HashMap, HashSet};

use rue_ast::{AstItem, AstNode, AstSymbolItem, AstTypeItem};
use rue_diagnostic::{Applicability, Lint, Name, SourceKind, SrcLoc, Suggestion};
use rue_hir::{Database, Declaration, ScopeId, Symbol, SymbolId};
use rue_parser::SyntaxKind;
use rue_types::Type;

use crate::{LintContext, LintInput, srcloc};

pub(crate) const LINTS: &[Lint] = &[Lint::UnusedExport];

/// Reports the exported items of `mod` items that are only used inside of the module, so they
/// don't need to be exported. Modules that are exported themselves (from a file, or from another
/// module that is) are left alone, since anything that imports the file can use their items.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    let db = cx.db();

    let modules = &input.sites.modules;

    let scopes = modules
        .iter()
        .map(|&(module, _)| (db.module(module).scope, module))
        .collect::<HashMap<ScopeId, SymbolId>>();

    let export_keywords = export_keywords(input);

    let mut reports = Vec::new();

    for (module, module_srcloc) in modules {
        if is_public(db, &scopes, *module) {
            continue;
        }

        let module_symbol = db.module(*module);
        let scope = db.scope(module_symbol.scope);
        let module_name = module_symbol.name.as_ref().map_or("", |name| name.text());

        let declarations = &module_symbol.declarations;

        let items = declarations
            .symbols
            .iter()
            .filter(|&&symbol| scope.is_symbol_exported(symbol))
            .filter_map(|&symbol| Some((Declaration::Symbol(symbol), symbol_name(db, symbol)?)))
            .chain(
                declarations
                    .types
                    .iter()
                    .filter(|&&(ty, _)| scope.is_type_exported(ty))
                    .filter_map(|&(ty, _)| Some((Declaration::Type(ty), type_name(db, ty)?))),
            );

        for (declaration, name) in items {
            let Some(name_srcloc) = name.srcloc() else {
                continue;
            };

            if name.text().starts_with('_') {
                continue;
            }

            let users = db.reference_parents(declaration);

            // Anything that isn't used at all is reported by the `unused` lints instead
            if users.is_empty() {
                continue;
            }

            if users
                .iter()
                .any(|&user| !is_inside(db, user, module_srcloc, &mut HashSet::new()))
            {
                continue;
            }

            reports.push((
                name_srcloc.clone(),
                format!(
                    "`{}` is exported, but is only used inside of module `{module_name}`",
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

/// Whether the declaration is inside of the module, by where it's declared (or for a declaration
/// without a name, where the declaration it's inside of is).
fn is_inside(
    db: &Database,
    declaration: Declaration,
    module: &SrcLoc,
    visited: &mut HashSet<Declaration>,
) -> bool {
    if !visited.insert(declaration) {
        return false;
    }

    let name = match declaration {
        Declaration::Symbol(symbol) => symbol_name(db, symbol),
        Declaration::Type(ty) => type_name(db, ty),
    };

    if let Some(srcloc) = name.as_ref().and_then(Name::srcloc) {
        return srcloc.source.kind == module.source.kind
            && module.span.start <= srcloc.span.start
            && srcloc.span.end <= module.span.end;
    }

    let parents = db.declaration_parents(declaration);

    !parents.is_empty()
        && parents
            .into_iter()
            .all(|parent| is_inside(db, parent, module, visited))
}

fn symbol_name(db: &Database, symbol: SymbolId) -> Option<Name> {
    match db.symbol(symbol) {
        Symbol::Function(_) | Symbol::Constant(_) | Symbol::Binding(_) | Symbol::Module(_) => {
            db.symbol(symbol).name()
        }
        Symbol::Unresolved | Symbol::Builtin(_) | Symbol::Parameter(_) => None,
    }
}

fn type_name(db: &Database, ty: rue_types::TypeId) -> Option<Name> {
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
