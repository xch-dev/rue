use std::collections::{HashMap, HashSet};

use rue_diagnostic::{Lint, Name};
use rue_hir::{
    BindingSymbol, ConstantSymbol, Database, Declaration, FunctionSymbol, ImportId, ImportUser,
    Items, ParameterSymbol, Symbol,
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
    let live = input.live_declarations(db);

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

    reports.extend(unused_imports(db, live));

    for declaration in unused_declarations(db, input.entrypoints, live) {
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
/// count, so that the import is reported along with the declarations it imports, and neither does
/// a use from the path of another import that's unused.
fn unused_imports(db: &Database, live: &HashSet<Declaration>) -> Vec<Report> {
    let imports = db.relevant_imports().collect::<Vec<_>>();

    // An import that's only used by other imports is used once they are, so this repeats until
    // no more imports are found to be used
    let mut used = HashSet::new();

    loop {
        let count = used.len();

        for &import in &imports {
            if !used.contains(&import) && is_import_used(db, live, &used, import) {
                used.insert(import);
            }
        }

        if used.len() == count {
            break;
        }
    }

    imports
        .into_iter()
        .filter(|import| !used.contains(import))
        .map(|import| {
            let name = match &db.import(import).items {
                Items::All(name) | Items::Named(name) => name,
            };

            (
                Lint::UnusedImport,
                name.clone(),
                format!("Unused import `{}`", name.text()),
            )
        })
        .collect()
}

/// Whether the import is used, given the imports that are already known to be used. An import
/// that doesn't bring anything into scope is reported from the [`LintSites`](crate::LintSites)
/// instead, so it counts as used here.
fn is_import_used(
    db: &Database,
    live: &HashSet<Declaration>,
    used: &HashSet<ImportId>,
    id: ImportId,
) -> bool {
    let import = db.import(id);

    if import.exported {
        return true;
    }

    let references = db
        .import_uses(id)
        .filter(|&(_, user)| match user {
            ImportUser::Declaration(declaration) => live.contains(&declaration),
            ImportUser::Import(import) => used.contains(&import),
            ImportUser::Other => true,
        })
        .map(|(declaration, _)| declaration)
        .collect::<HashSet<_>>();

    match &import.items {
        Items::All(_) => import.declarations.is_empty() || !references.is_empty(),
        // An item can import declarations from more than one namespace (such as a type and a
        // constant with the same name), so it's used if any of them are
        Items::Named(item) => {
            let mut declarations = import
                .declarations
                .iter()
                .filter(|(name, _)| item.text() == name)
                .peekable();

            declarations.peek().is_none()
                || declarations.any(|(_, declaration)| references.contains(declaration))
        }
    }
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
/// only live if something live references it (even though its value is still evaluated if it may
/// fail). Declarations whose names start with `_` aren't reported, so they're kept (along with
/// what they need) as if they were used.
pub(crate) fn live_declarations(
    db: &Database,
    entrypoints: &HashSet<Declaration>,
) -> HashSet<Declaration> {
    let kept = kept_bindings(db);

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

        if is_live_with_parent(db, &kept, declaration) {
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

/// The bindings that are kept whenever the function they're in is live, which are the bindings
/// whose names start with `_`, along with the unnamed bindings they're destructured from (such as
/// the value of `let (_a, _b) = pair;`). A destructured binding references the binding it's
/// destructured from, unlike a binding declared inside of its value.
fn kept_bindings(db: &Database) -> HashSet<Declaration> {
    let mut kept = HashSet::new();

    let mut stack = db
        .relevant_declarations()
        .filter(|&declaration| is_binding(db, declaration) && is_exempt(db, declaration))
        .collect::<Vec<_>>();

    while let Some(declaration) = stack.pop() {
        if !kept.insert(declaration) {
            continue;
        }

        stack.extend(
            db.declaration_parents(declaration)
                .into_iter()
                .filter(|&parent| {
                    is_binding(db, parent)
                        && describe(db, parent).is_none()
                        && db.reference_parents(parent).contains(&declaration)
                }),
        );
    }

    kept
}

/// Whether the declaration is live whenever the function or type that declares it is, because
/// it's part of its signature, or it's a kept binding.
fn is_live_with_parent(
    db: &Database,
    kept: &HashSet<Declaration>,
    declaration: Declaration,
) -> bool {
    match declaration {
        Declaration::Symbol(symbol) => match db.symbol(symbol) {
            Symbol::Parameter(_) => true,
            Symbol::Binding(_) => kept.contains(&declaration),
            _ => false,
        },
        Declaration::Type(ty) => matches!(db.ty(ty), Type::Generic(_)),
    }
}

fn is_binding(db: &Database, declaration: Declaration) -> bool {
    matches!(declaration, Declaration::Symbol(symbol) if matches!(db.symbol(symbol), Symbol::Binding(_)))
}

/// Whether the declaration's name starts with `_`, so that it isn't reported as unused.
fn is_exempt(db: &Database, declaration: Declaration) -> bool {
    describe(db, declaration).is_some_and(|(_, _, name)| is_exempt_name(name))
}

fn is_exempt_name(name: &Name) -> bool {
    name.text().starts_with('_')
}

fn unused_declaration(db: &Database, declaration: Declaration) -> Option<Report> {
    let (lint, kind, name) = describe(db, declaration)?;

    if is_exempt_name(name) {
        return None;
    }

    let message = format!("Unused {kind} `{}`", name.text());

    Some((lint, name.clone(), message))
}

/// The lint that reports the declaration as unused, along with what kind of declaration it is and
/// its name, if it can be reported.
fn describe(db: &Database, declaration: Declaration) -> Option<(Lint, &'static str, &Name)> {
    Some(match declaration {
        Declaration::Symbol(symbol) => match db.symbol(symbol) {
            Symbol::Function(FunctionSymbol { name, .. }) => {
                (Lint::UnusedFunction, "function", name.as_ref()?)
            }
            Symbol::Binding(BindingSymbol { name, .. }) => {
                (Lint::UnusedBinding, "binding", name.as_ref()?)
            }
            Symbol::Constant(ConstantSymbol { name, .. }) => {
                (Lint::UnusedConstant, "constant", name.as_ref()?)
            }
            Symbol::Parameter(ParameterSymbol { name, .. }) => {
                (Lint::UnusedParameter, "parameter", name.as_ref()?)
            }
            Symbol::Unresolved | Symbol::Module(_) | Symbol::Builtin(_) => return None,
        },
        Declaration::Type(ty) => match db.ty(ty) {
            Type::Generic(Generic { name, .. }) => {
                (Lint::UnusedGenericType, "generic type", name.as_ref()?)
            }
            Type::Struct(Struct { name, .. }) => (Lint::UnusedStruct, "struct", name.as_ref()?),
            Type::Alias(Alias { name, .. }) => {
                (Lint::UnusedTypeAlias, "type alias", name.as_ref()?)
            }
            _ => return None,
        },
    })
}
