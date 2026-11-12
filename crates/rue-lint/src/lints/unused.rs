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

/// Reports the imports that are never used by live code. A use from an unused function doesn't
/// count, so that the import is reported along with the declarations it imports. A use from the
/// path of another import always counts, since it isn't in any declaration.
fn unused_imports(db: &Database, live: &HashSet<Declaration>) -> Vec<Report> {
    let mut reports = Vec::new();

    for import in db.relevant_imports() {
        let references = db
            .import_uses(import)
            .into_iter()
            .filter(|(_, user)| user.is_none_or(|user| live.contains(&user)))
            .map(|(declaration, _)| declaration)
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
///
/// A local declaration (a binding, parameter or generic type) is used if anything references it,
/// so that the locals of an unused function aren't reported along with it. Anything else is used
/// if it's live.
fn unused_declarations(
    db: &Database,
    entrypoints: &HashSet<Declaration>,
    live: &HashSet<Declaration>,
) -> Vec<Declaration> {
    let mut unused = db
        .relevant_declarations()
        .filter(|&declaration| {
            if entrypoints.contains(&declaration) {
                return false;
            }

            if db.declaration_parents(declaration).is_empty() {
                !live.contains(&declaration)
            } else {
                db.reference_parents(declaration).is_empty()
            }
        })
        .collect::<Vec<_>>();

    unused.sort_by_key(|declaration| matches!(declaration, Declaration::Type(_)));
    unused
}

/// The declarations whose code is evaluated, or is needed by code that's kept. A declaration is
/// live if it's an entrypoint, or something live references it. The parameters and generic types
/// of a live function or type are live, since its signature needs their types, but a binding is
/// only evaluated if something live references it. Declarations whose names start with `_` aren't
/// reported, so they're kept (along with what they need) as if they were used.
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

        if is_live_with_parent(db, declaration) {
            for parent in db.declaration_parents(declaration) {
                children.entry(parent).or_default().push(declaration);
                stack.push(parent);
            }
        }
    }

    let mut live = HashSet::new();

    let mut stack = entrypoints
        .iter()
        .copied()
        .chain(db.relevant_declarations().filter(|&declaration| {
            db.declaration_parents(declaration).is_empty() && is_exempt(db, declaration)
        }))
        .collect::<Vec<_>>();

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

/// Whether the declaration is live whenever the function or type that declares it is, because
/// it's part of its signature, or it's a binding whose name starts with `_`.
fn is_live_with_parent(db: &Database, declaration: Declaration) -> bool {
    match declaration {
        Declaration::Symbol(symbol) => match db.symbol(symbol) {
            Symbol::Parameter(_) => true,
            Symbol::Binding(_) => is_exempt(db, declaration),
            _ => false,
        },
        Declaration::Type(ty) => matches!(db.ty(ty), Type::Generic(_)),
    }
}

/// Whether the declaration's name starts with `_`, so that it isn't reported as unused.
fn is_exempt(db: &Database, declaration: Declaration) -> bool {
    describe(db, declaration).is_some_and(|(_, _, name)| name.text().starts_with('_'))
}

fn unused_declaration(db: &Database, declaration: Declaration) -> Option<Report> {
    let (lint, kind, name) = describe(db, declaration)?;

    if name.text().starts_with('_') {
        return None;
    }

    let message = format!("Unused {kind} `{}`", name.text());

    Some((lint, name, message))
}

/// The lint that reports the declaration as unused, along with what kind of declaration it is and
/// its name, if it can be reported.
fn describe(db: &Database, declaration: Declaration) -> Option<(Lint, &'static str, Name)> {
    Some(match declaration {
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
    })
}
