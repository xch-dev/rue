use std::collections::HashSet;

use indexmap::IndexSet;
use rue_diagnostic::{Lint, Name};
use rue_hir::{
    BindingSymbol, ConstantSymbol, Database, Declaration, FunctionSymbol, Items, ParameterSymbol,
    Symbol,
};
use rue_types::{Alias, Generic, Struct, Type};

use crate::{LintContext, LintInput};

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

        for (name, declaration) in &import.declarations {
            if !references.contains(declaration) && item.text() == name {
                reports.push((
                    Lint::UnusedImport,
                    item.clone(),
                    format!("Unused import `{}`", item.text()),
                ));
            }
        }
    }

    reports
}

fn unused_declarations(db: &Database, entrypoints: &HashSet<Declaration>) -> IndexSet<Declaration> {
    let mut used_declarations = IndexSet::new();
    let mut unused_declarations = IndexSet::new();

    for declaration in db.relevant_declarations() {
        let Declaration::Symbol(_) = declaration else {
            continue;
        };

        if !db.declaration_parents(declaration).is_empty()
            && !db.reference_parents(declaration).is_empty()
        {
            used_declarations.insert(declaration);
            continue;
        }

        let mut visited = HashSet::new();
        let mut stack = vec![declaration];

        while let Some(current) = stack.pop() {
            if !visited.insert(current) {
                continue;
            }

            for parent in db.reference_parents(current) {
                stack.push(parent);

                if entrypoints.contains(&parent) {
                    used_declarations.insert(declaration);
                }
            }
        }

        if entrypoints.contains(&declaration) {
            used_declarations.insert(declaration);
            continue;
        }

        if !used_declarations.contains(&declaration) {
            unused_declarations.insert(declaration);
        }
    }

    for declaration in db.relevant_declarations() {
        if !matches!(declaration, Declaration::Type(_)) {
            continue;
        }

        if entrypoints.contains(&declaration) {
            used_declarations.insert(declaration);
            continue;
        }

        let mut visited = HashSet::new();
        let mut stack = vec![declaration];

        while let Some(current) = stack.pop() {
            if !visited.insert(current) {
                continue;
            }

            for parent in db.reference_parents(current) {
                stack.push(parent);

                let used = match parent {
                    Declaration::Symbol(_) => used_declarations.contains(&parent),
                    Declaration::Type(_) => entrypoints.contains(&parent),
                };

                if used {
                    used_declarations.insert(declaration);
                    break;
                }
            }

            if used_declarations.contains(&declaration) {
                break;
            }
        }

        if !used_declarations.contains(&declaration) {
            unused_declarations.insert(declaration);
        }
    }

    unused_declarations
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
