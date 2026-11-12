use std::collections::{HashMap, HashSet};

use rue_diagnostic::{Lint, Name};
use rue_hir::{
    BindingSymbol, ConstantSymbol, Database, Declaration, FunctionSymbol, Items, ParameterSymbol,
    Symbol,
};
use rue_types::{Alias, Generic, Struct, Type};

use crate::{LintContext, LintInput};

pub(crate) const LINTS: &[Lint] = &[
    Lint::UnusedImport,
    Lint::UnusedFunction,
    Lint::UnusedBinding,
    Lint::UnusedConstant,
    Lint::UnusedParameter,
    Lint::UnusedGenericType,
    Lint::UnusedStruct,
    Lint::UnusedTypeAlias,
];

/// Reports imports and declarations that are never used. A declaration is used if it's live,
/// meaning reachable from an entrypoint, such as `main`, a test or an export. Names starting with
/// `_` are exempt.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    let db = cx.db();
    let live = live_declarations(db, input.entrypoints);

    let mut reports = input
        .sites
        .empty_imports
        .iter()
        .map(|name| {
            (
                Lint::UnusedImport,
                name.clone(),
                format!("Unused import `{}`", name.text()),
            )
        })
        .collect::<Vec<_>>();

    reports.extend(unused_imports(db, &live));

    for declaration in unused_declarations(db, input.entrypoints, &live) {
        if let Some(report) = unused_declaration(db, declaration) {
            reports.push(report);
        }
    }

    for (lint, name, message) in reports {
        if let Some(srcloc) = name.srcloc().cloned() {
            cx.report(lint, srcloc, message);
        }
    }
}

type Report = (Lint, Name, String);

/// Reports the imports whose declarations are never used by live code. A use from an unused
/// function doesn't count, so that the import is reported along with the declarations it imports.
/// Modules are only used in the paths of other imports, which aren't part of any declaration, so
/// any use of an imported module counts.
fn unused_imports(db: &Database, live: &HashSet<Declaration>) -> Vec<Report> {
    let mut reports = Vec::new();

    for import in db.relevant_imports() {
        let references = db
            .import_references(import)
            .into_iter()
            .filter(|&declaration| live.contains(&declaration) || is_module(db, declaration))
            .collect::<HashSet<_>>();

        let import = db.import(import);

        // Unused declarations aren't reported in the standard library either, and its functions
        // are only live if they're called, so its imports would be reported as unused otherwise
        if import.exported || !import.source.kind.check_unused() {
            continue;
        }

        let item = match &import.items {
            Items::All(star) => {
                if !import.declarations.is_empty() && references.is_empty() {
                    reports.push((
                        Lint::UnusedImport,
                        star.clone(),
                        "Unused import `*`".to_string(),
                    ));
                }
                continue;
            }
            Items::Named(item) => item,
        };

        // An item can import declarations from more than one namespace (such as a type and a
        // constant with the same name), so it's only unused if none of them are referenced
        let mut declarations = import
            .declarations
            .iter()
            .filter(|(name, _)| item.text() == name)
            .peekable();

        if declarations.peek().is_some()
            && declarations.all(|(_, declaration)| !references.contains(declaration))
        {
            reports.push((
                Lint::UnusedImport,
                item.clone(),
                format!("Unused import `{}`", item.text()),
            ));
        }
    }

    reports
}

/// The declarations that are never used, with symbols before types.
///
/// A local declaration (a binding, parameter or generic type) is used if anything references it,
/// so that the locals of an unused function aren't reported along with it. Anything else is used
/// if it's live.
fn unused_declarations(
    db: &Database,
    entrypoints: &HashSet<Declaration>,
    live: &HashSet<Declaration>,
) -> Vec<Declaration> {
    let is_used = |declaration: Declaration| {
        if entrypoints.contains(&declaration) {
            return true;
        }

        if db.declaration_parents(declaration).is_empty() {
            live.contains(&declaration)
        } else {
            !db.reference_parents(declaration).is_empty()
        }
    };

    let symbols = db
        .relevant_declarations()
        .filter(|declaration| matches!(declaration, Declaration::Symbol(_)));

    let types = db
        .relevant_declarations()
        .filter(|declaration| matches!(declaration, Declaration::Type(_)));

    symbols
        .chain(types)
        .filter(|&declaration| !is_used(declaration))
        .collect()
}

/// The declarations whose code is evaluated, starting from the entrypoints. A declaration is live
/// if something live references it, and the parameters and generic types of a live function or
/// type are live, since its signature needs their types. A binding is only live if something live
/// references it, since it's only evaluated if it's referenced.
fn live_declarations(db: &Database, entrypoints: &HashSet<Declaration>) -> HashSet<Declaration> {
    // The database only stores the edges from a declaration to the ones that reference or
    // declare it, so the edges in the other direction are collected first. Every declaration on
    // a path from an entrypoint to a relevant or imported declaration is an ancestor of it.
    let mut children = HashMap::<Declaration, Vec<Declaration>>::new();
    let mut visited = HashSet::new();

    let mut stack = db
        .relevant_declarations()
        .chain(
            db.relevant_imports()
                .flat_map(|import| db.import_references(import)),
        )
        .collect::<Vec<_>>();

    while let Some(declaration) = stack.pop() {
        if !visited.insert(declaration) {
            continue;
        }

        for parent in db.reference_parents(declaration) {
            children.entry(parent).or_default().push(declaration);
            stack.push(parent);
        }

        if is_signature_local(db, declaration) {
            for parent in db.declaration_parents(declaration) {
                children.entry(parent).or_default().push(declaration);
                stack.push(parent);
            }
        }
    }

    let mut live = HashSet::new();
    let mut stack = entrypoints.iter().copied().collect::<Vec<_>>();

    while let Some(declaration) = stack.pop() {
        if !live.insert(declaration) {
            continue;
        }

        if let Some(children) = children.get(&declaration) {
            stack.extend(children.iter().copied());
        }
    }

    live
}

fn is_module(db: &Database, declaration: Declaration) -> bool {
    matches!(declaration, Declaration::Symbol(symbol) if matches!(db.symbol(symbol), Symbol::Module(_)))
}

/// Whether the declaration is part of the signature of the function or type that declares it, so
/// that it's live whenever its parent is.
fn is_signature_local(db: &Database, declaration: Declaration) -> bool {
    match declaration {
        Declaration::Symbol(symbol) => matches!(db.symbol(symbol), Symbol::Parameter(_)),
        Declaration::Type(ty) => matches!(db.ty(ty), Type::Generic(_)),
    }
}

fn unused_declaration(db: &Database, declaration: Declaration) -> Option<Report> {
    let (lint, kind, name) = match declaration {
        Declaration::Symbol(symbol) => match db.symbol(symbol) {
            Symbol::Function(FunctionSymbol { name, .. }) => {
                (Lint::UnusedFunction, "function", name.clone()?)
            }
            Symbol::Binding(BindingSymbol { name, .. }) => {
                (Lint::UnusedBinding, "binding", name.clone()?)
            }
            Symbol::Constant(ConstantSymbol { name, .. }) => {
                (Lint::UnusedConstant, "constant", name.clone()?)
            }
            Symbol::Parameter(ParameterSymbol { name, .. }) => {
                (Lint::UnusedParameter, "parameter", name.clone()?)
            }
            Symbol::Unresolved | Symbol::Module(_) | Symbol::Builtin(_) => return None,
        },
        Declaration::Type(ty) => match db.ty(ty) {
            Type::Generic(Generic { name, .. }) => {
                (Lint::UnusedGenericType, "generic type", name.clone()?)
            }
            Type::Struct(Struct { name, .. }) => (Lint::UnusedStruct, "struct", name.clone()?),
            Type::Alias(Alias { name, .. }) => (Lint::UnusedTypeAlias, "type alias", name.clone()?),
            _ => return None,
        },
    };

    if name.text().starts_with('_') {
        return None;
    }

    let message = format!("Unused {kind} `{}`", name.text());

    Some((lint, name, message))
}
