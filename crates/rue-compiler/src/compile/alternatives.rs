//! What a path could refer to other than what it's compiled as referring to, when glob imports
//! bring in different modules with a name in it. Which of them the name is bound to depends on the
//! order the imports are resolved in, and so on module names, so a use of the path counts as a use
//! of what it could refer to in each of them, and so does an import through it. That way, which of
//! their members are reported as unused doesn't depend on module names either.

use indexmap::IndexSet;
use rue_hir::{Declaration, ImportId, Item, Items, Namespace, ScopeId, Symbol, SymbolId};

use crate::Compiler;

/// An item, with the import it's found through, if any.
type Found = (Item, Option<ImportId>);

/// Where a segment of a path is looked up, for finding what else it could refer to.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Place {
    /// In a scope, and then each enclosing scope.
    Chain(ScopeId),
    /// In a module's scope.
    Scope(ScopeId),
}

/// What a name is bound to in one namespace where a segment of a path is looked up, if anything
/// other than a failed import's error item, along with what else the glob imports there could
/// bring in with the name, if the modules their paths name were bound differently (see
/// [`Database::glob_alternatives`](rue_hir::Database::glob_alternatives)). Those are returned with the glob import. A name that a
/// declaration or a named import binds hides what any glob import could bring in, whichever way
/// they're bound, so only a name that's unbound, or bound by a glob or failed import, has any.
/// With `exported_only`, only what the place exports counts, as when another module imports it.
fn lookup(
    ctx: &Compiler,
    place: Place,
    namespace: Namespace,
    name: &str,
    exported_only: bool,
) -> (Option<Found>, Vec<Found>) {
    let (mut current, chain) = match place {
        Place::Chain(scope) => (Some(scope), true),
        Place::Scope(scope) => (Some(scope), false),
    };

    let mut others = Vec::new();

    while let Some(scope_id) = current {
        let scope = ctx.scope(scope_id);

        let binding = scope
            .item_with_import(namespace, name)
            .filter(|&(item, _)| !exported_only || scope.is_item_exported(item));

        let hidden = binding.is_some_and(|(item, via)| {
            !ctx.is_error_item(item)
                && via.is_none_or(|via| !matches!(ctx.import(via).items, Items::All(_)))
        });

        if !hidden && ctx.has_glob_alternatives() {
            for import in scope.imports() {
                if !matches!(ctx.import(import).items, Items::All(_))
                    || (exported_only && !ctx.import(import).exported)
                {
                    continue;
                }

                others.extend(
                    ctx.glob_alternatives(import, namespace, name)
                        .map(|item| (item, Some(import))),
                );
            }
        }

        if let Some((item, via)) = binding {
            let binding = (!ctx.is_error_item(item)).then_some((item, via));
            return (binding, others);
        }

        current = if chain { scope.parent() } else { None };
    }

    (None, others)
}

/// What a segment of a path could refer to other than what it's bound to where it's looked up,
/// `primary` (the module the path names so far): what the glob imports there could bring in with
/// the name instead (see [`lookup`]), along with what each of the other modules that the segment
/// before it could name, `previous`, has with the name, and what their glob imports could bring
/// in. The namespaces are tried in order in each place, as the segment is resolved, so the first
/// one that has the name, or could, is the one used there. Each item is returned with each import
/// it's found through, if any, since each of them is used.
pub(crate) fn alternative_items(
    ctx: &Compiler,
    primary: Option<Place>,
    previous: &[SymbolId],
    namespaces: &[Namespace],
    name: &str,
    exported_only: bool,
) -> IndexSet<Found> {
    let places = primary.map(|place| (place, false)).into_iter().chain(
        previous
            .iter()
            .map(|&module| (Place::Scope(ctx.module(module).scope), true)),
    );

    let mut items = IndexSet::new();

    for (place, other) in places {
        for &namespace in namespaces {
            let (binding, others) = lookup(ctx, place, namespace, name, exported_only);
            let found = binding.is_some() || !others.is_empty();

            // What the name is bound to in the module the path is resolved to is what it refers
            // to, rather than something else it could
            items.extend(binding.filter(|_| other).into_iter().chain(others));

            if found {
                break;
            }
        }
    }

    items
}

/// The other modules that a segment of a path before the last one could name, besides `resolved`,
/// the one it's resolved to (see [`alternative_items`]). A name that glob imports bring in different
/// modules with could also name each of them (see [`Database::ambiguous_alternatives`](rue_hir::Database::ambiguous_alternatives)). Each is
/// returned with each import it's found through, if any.
pub(crate) fn alternative_modules(
    ctx: &Compiler,
    primary: Option<Place>,
    previous: &[SymbolId],
    name: &str,
    resolved: Option<(SymbolId, Option<ImportId>)>,
) -> IndexSet<(SymbolId, Option<ImportId>)> {
    let mut modules = alternative_items(ctx, primary, previous, &[Namespace::Module], name, false)
        .into_iter()
        .filter_map(|(item, via)| Some((item.symbol()?, via)))
        .collect::<IndexSet<_>>();

    let named = modules.iter().copied().chain(resolved).collect::<Vec<_>>();

    for (module, via) in named {
        for alternative in ctx.ambiguous_alternatives(via, Declaration::Symbol(module)) {
            if let Declaration::Symbol(alternative) = alternative {
                modules.insert((alternative, None));
            }
        }
    }

    // The path itself uses the module it's resolved to through the import it's resolved through,
    // but another glob import could bring in the same module, which it uses too
    modules.retain(|&(module, via)| {
        resolved != Some((module, via))
            && !ctx.is_error_item(Item::Module(module))
            && matches!(ctx.symbol(module), Symbol::Module(_))
    });

    modules
}

/// The other modules that a segment of a path could name (see [`alternative_modules`]), without
/// the imports they're found through or the module it's resolved to, which is where the rest of
/// the path is resolved already.
pub(crate) fn modules_of(
    modules: IndexSet<(SymbolId, Option<ImportId>)>,
    resolved: Option<(SymbolId, Option<ImportId>)>,
) -> Vec<SymbolId> {
    modules
        .into_iter()
        .map(|(module, _)| module)
        .filter(|&module| resolved.is_none_or(|(resolved, _)| module != resolved))
        .collect::<IndexSet<_>>()
        .into_iter()
        .collect()
}
