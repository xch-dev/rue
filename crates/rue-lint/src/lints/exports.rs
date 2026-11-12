use std::collections::{HashMap, HashSet};

use rue_ast::{AstItem, AstNode, AstSymbolItem, AstTypeItem};
use rue_diagnostic::{Applicability, Lint, Name, SrcLoc, Suggestion};
use rue_hir::{Database, Declaration, Items, ScopeId, Symbol, SymbolId};
use rue_types::{Type, TypeId};

use crate::{
    LintContext, LintInput, NameKey, name_key, srcloc_key,
    text::{encloses, keyword_locations},
};

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
        .map(|&(module, _)| (db.module(module).scope, module))
        .collect::<HashMap<ScopeId, SymbolId>>();

    let qualified = &input.sites.qualified_references;
    let export_keywords = export_keywords(input);
    let imported = imported(db);

    let mut reports = Vec::new();

    for (module, module_srcloc) in modules {
        let module = *module;

        if is_public(db, &scopes, module) {
            continue;
        }

        let module_symbol = db.module(module);
        let module_name = module_symbol.name.as_ref().map_or("", |name| name.text());

        // The exports of modules that are exported from this one (and the items it re-exports)
        // are what it exports too
        let mut exports = Vec::new();
        collect_exports(db, module, true, &mut exports, &mut HashSet::new());

        // Any use of the module's own items through a path or import needs them to be exported,
        // but the items of nested modules only need this module to export them if they're used
        // outside of it
        let used = used_exports(db, &exports, |declaration, direct| {
            let mut uses = qualified
                .get(&declaration)
                .into_iter()
                .chain(imported.get(&declaration))
                .flatten();

            if direct {
                uses.next().is_some()
            } else {
                uses.any(|srcloc| !encloses(module_srcloc, srcloc))
            }
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
                export_keywords.get(&srcloc_key(name_srcloc)).cloned(),
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
/// directly (rather than in a module that it exports, or re-exported from somewhere else).
fn collect_exports(
    db: &Database,
    module: SymbolId,
    direct: bool,
    exports: &mut Vec<(Declaration, Name, bool)>,
    seen: &mut HashSet<Declaration>,
) {
    let module = db.module(module);
    let scope = db.scope(module.scope);
    let declarations = &module.declarations;

    let declared = declarations
        .symbols
        .iter()
        .map(|&symbol| Declaration::Symbol(symbol))
        .chain(
            declarations
                .types
                .iter()
                .map(|&(ty, _)| Declaration::Type(ty)),
        )
        .collect::<HashSet<_>>();

    let exported = scope
        .exported_symbols()
        .map(|(_, symbol)| symbol)
        .filter(|&symbol| !matches!(db.symbol(symbol), Symbol::Module(_)))
        .map(Declaration::Symbol)
        .chain(scope.exported_types().map(|(_, ty)| Declaration::Type(ty)))
        .collect::<Vec<_>>();

    for declaration in exported {
        let name = match declaration {
            Declaration::Symbol(symbol) => symbol_name(db, symbol),
            Declaration::Type(ty) => type_name(db, ty),
        };

        if let Some(name) = name
            && seen.insert(declaration)
        {
            exports.push((declaration, name, direct && declared.contains(&declaration)));
        }
    }

    for &nested in &declarations.modules {
        if scope.is_symbol_exported(nested) {
            collect_exports(db, nested, false, exports, seen);
        }
    }
}

/// The exports that need to be exported: the ones that are used from outside of the module, and
/// the types in the signatures of those (and so on), which are part of what the module exports
/// even if nothing outside of it names them.
fn used_exports(
    db: &Database,
    exports: &[(Declaration, Name, bool)],
    is_used: impl Fn(Declaration, bool) -> bool,
) -> HashSet<Declaration> {
    let mut used = exports
        .iter()
        .filter(|&&(declaration, _, direct)| is_used(declaration, direct))
        .map(|&(declaration, ..)| declaration)
        .collect::<HashSet<_>>();

    // The types are found by the location of their names, which every copy of them shares
    let types = exports
        .iter()
        .filter(|(declaration, ..)| matches!(declaration, Declaration::Type(_)))
        .filter_map(|(declaration, name, _)| Some((name_key(name)?, *declaration)))
        .collect::<HashMap<_, _>>();

    let mut stack = used.iter().copied().collect::<Vec<_>>();

    while let Some(declaration) = stack.pop() {
        let mut reached = HashSet::new();
        let mut visited = HashSet::new();

        for ty in signature_types(db, declaration) {
            reached_names(db, ty, &mut reached, &mut visited);
        }

        // A type that refers to itself (such as a recursive struct) is already used, so it isn't
        // kept exported by its own signature
        for key in reached {
            if let Some(&export) = types.get(&key)
                && used.insert(export)
            {
                stack.push(export);
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

/// Where each declaration is imported (or re-exported), which it can only be if it's exported. A
/// glob import that isn't exported only needs the items that are used through it.
fn imported(db: &Database) -> HashMap<Declaration, Vec<SrcLoc>> {
    let mut imported: HashMap<Declaration, Vec<SrcLoc>> = HashMap::new();

    for id in db.relevant_imports() {
        let import = db.import(id);

        let (item, declarations) = match &import.items {
            Items::All(star) if !import.exported => (star, db.import_references(id)),
            Items::All(item) | Items::Named(item) => (
                item,
                import
                    .declarations
                    .iter()
                    .map(|&(_, declaration)| declaration)
                    .collect(),
            ),
        };

        let Some(srcloc) = item.srcloc() else {
            continue;
        };

        for declaration in declarations {
            imported
                .entry(declaration)
                .or_default()
                .push(srcloc.clone());
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
                .chain(function.parameters.iter().filter_map(
                    |&parameter| match db.symbol(parameter) {
                        Symbol::Parameter(parameter) => Some(parameter.ty),
                        _ => None,
                    },
                ))
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

/// Adds the names of the structs and aliases that the type is or contains.
fn reached_names(
    db: &Database,
    ty: TypeId,
    reached: &mut HashSet<NameKey>,
    visited: &mut HashSet<TypeId>,
) {
    if !visited.insert(ty) {
        return;
    }

    let mut walk = |ty: TypeId, reached: &mut HashSet<NameKey>| {
        reached_names(db, ty, reached, visited);
    };

    match db.ty(ty) {
        Type::Struct(ty) => {
            reached.extend(ty.name.as_ref().and_then(name_key));
            walk(ty.inner, reached);

            for &generic in &ty.generics {
                walk(generic, reached);
            }
        }
        Type::Alias(alias) => {
            reached.extend(alias.name.as_ref().and_then(name_key));
            walk(alias.inner, reached);

            for &generic in &alias.generics {
                walk(generic, reached);
            }
        }
        Type::Ref(inner) => walk(*inner, reached),
        Type::Pair(pair) => {
            walk(pair.first, reached);
            walk(pair.rest, reached);
        }
        Type::Union(union) => {
            for &ty in &union.types {
                walk(ty, reached);
            }
        }
        Type::Apply(apply) => {
            walk(apply.inner, reached);

            for &generic in apply.generics.values() {
                walk(generic, reached);
            }
        }
        Type::Function(function) => {
            walk(function.ret, reached);

            for ty in function.param_types() {
                walk(ty, reached);
            }
        }
        Type::Unresolved | Type::Generic(_) | Type::Any | Type::Never | Type::Atom(_) => {}
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
fn export_keywords(input: &LintInput<'_>) -> HashMap<NameKey, SrcLoc> {
    keyword_locations(input, |node| {
        let (export, name) = match AstItem::cast(node.clone())? {
            AstItem::SymbolItem(AstSymbolItem::FunctionItem(item)) => (item.export(), item.name()),
            AstItem::SymbolItem(AstSymbolItem::ConstantItem(item)) => (item.export(), item.name()),
            AstItem::SymbolItem(AstSymbolItem::ModuleItem(item)) => (item.export(), item.name()),
            AstItem::TypeItem(AstTypeItem::StructItem(item)) => (item.export(), item.name()),
            AstItem::TypeItem(AstTypeItem::TypeAliasItem(item)) => (item.export(), item.name()),
            AstItem::ImportItem(_) => return None,
        };

        Some((export?, name?))
    })
}
