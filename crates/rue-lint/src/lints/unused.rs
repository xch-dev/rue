use std::collections::HashSet;

use indexmap::IndexSet;
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

/// Reports imports and declarations that are never used. A declaration is used if it's reachable
/// from an entrypoint, such as `main`, a test or an export. Names starting with `_` are exempt.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    let db = cx.db();

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

    reports.extend(unused_imports(db));

    for declaration in unused_declarations(db, input.entrypoints) {
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

fn unused_imports(db: &Database) -> Vec<Report> {
    let mut reports = Vec::new();

    for import in db.relevant_imports() {
        let references = db
            .import_references(import)
            .into_iter()
            .collect::<HashSet<_>>();

        let import = db.import(import);

        if import.exported {
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
fn unused_declarations(db: &Database, entrypoints: &HashSet<Declaration>) -> IndexSet<Declaration> {
    let mut used_symbols = HashSet::new();
    let mut unused_declarations = IndexSet::new();

    for declaration in db.relevant_declarations() {
        let Declaration::Symbol(_) = declaration else {
            continue;
        };

        // A local symbol (such as a binding or parameter) is used if anything references it, so
        // that it isn't reported along with the unused function it's declared in
        let is_local_and_referenced = !db.declaration_parents(declaration).is_empty()
            && !db.reference_parents(declaration).is_empty();

        if is_local_and_referenced || is_reachable(db, entrypoints, declaration) {
            used_symbols.insert(declaration);
        } else {
            unused_declarations.insert(declaration);
        }
    }

    // The used symbols whose code is evaluated, which excludes the local symbols of unused
    // functions, since they count as used so that they aren't reported along with the function
    let live_symbols = used_symbols
        .iter()
        .copied()
        .filter(|&symbol| {
            let parents = db.declaration_parents(symbol);

            parents.is_empty()
                || parents
                    .into_iter()
                    .any(|parent| is_live(db, entrypoints, parent, &mut HashSet::new()))
        })
        .collect::<HashSet<_>>();

    for declaration in db.relevant_declarations() {
        let Declaration::Type(ty) = declaration else {
            continue;
        };

        // A generic type is used if a used symbol references it, such as a parameter of a used
        // function, so that it isn't reported along with an unused function. Anything else, such
        // as a struct, must be referenced by a used symbol of a live function, so that a struct
        // that's only used by an unused function is reported as well.
        let used = if matches!(db.ty(ty), Type::Generic(_)) {
            is_referenced_by(db, entrypoints, &used_symbols, declaration)
        } else {
            is_referenced_by(db, entrypoints, &live_symbols, declaration)
        };

        if !used {
            unused_declarations.insert(declaration);
        }
    }

    unused_declarations
}

/// Whether the declaration is an entrypoint, or is referenced by one through any number of other
/// declarations.
fn is_reachable(
    db: &Database,
    entrypoints: &HashSet<Declaration>,
    declaration: Declaration,
) -> bool {
    is_referenced_by(db, entrypoints, &HashSet::new(), declaration)
}

/// Whether the declaration's code is evaluated, because it's reachable from an entrypoint, or it's
/// declared inside of something live (such as a lambda in a live function).
fn is_live(
    db: &Database,
    entrypoints: &HashSet<Declaration>,
    declaration: Declaration,
    visited: &mut HashSet<Declaration>,
) -> bool {
    if !visited.insert(declaration) {
        return false;
    }

    is_reachable(db, entrypoints, declaration)
        || db
            .declaration_parents(declaration)
            .into_iter()
            .any(|parent| is_live(db, entrypoints, parent, visited))
}

/// Whether the declaration is an entrypoint, or is referenced by an entrypoint or one of the
/// symbols through any number of other declarations.
fn is_referenced_by(
    db: &Database,
    entrypoints: &HashSet<Declaration>,
    symbols: &HashSet<Declaration>,
    declaration: Declaration,
) -> bool {
    if entrypoints.contains(&declaration) {
        return true;
    }

    let mut visited = HashSet::new();
    let mut stack = vec![declaration];

    while let Some(current) = stack.pop() {
        if !visited.insert(current) {
            continue;
        }

        for parent in db.reference_parents(current) {
            if entrypoints.contains(&parent) || symbols.contains(&parent) {
                return true;
            }

            stack.push(parent);
        }
    }

    false
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
