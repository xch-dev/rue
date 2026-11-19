//! What a path could refer to other than what it's compiled as referring to, when glob imports
//! bring in different modules with a name in it. Which of them the name is bound to depends on the
//! order the imports are resolved in, and so on module names, so a use of the path counts as a use
//! of what it could refer to in each of them, and so does an import through it. That way, which of
//! their members are reported as unused doesn't depend on module names either.

use indexmap::IndexSet;
use rue_hir::{Declaration, ImportId, Item, Items, Namespace, Scope, ScopeId, Symbol, SymbolId};

use crate::Compiler;

/// An item, with the import it's found through, if any.
pub(crate) type Found = (Item, Option<ImportId>);

/// Where a segment of a path is looked up, for finding what else it could refer to.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Place {
    /// In a scope, and then each enclosing scope.
    Chain(ScopeId),
    /// In a module's scope.
    Scope(ScopeId),
}

/// Whether a name in a namespace is hidden in a scope from what the glob imports into it could
/// bring in besides what they do (see
/// [`Database::import_alternatives`](rue_hir::Database::import_alternatives)), whichever way the
/// names in their paths are bound. A declaration that binds it hides it, and so does a named
/// import, unless what any named import of the name brings in is uncertain (see
/// [`Database::add_uncertain_import`](rue_hir::Database::add_uncertain_import)), since in
/// another order it might bring in something else, or nothing. What a glob import binds doesn't
/// hide it, since what another one could bring in instead would collide with it, and neither does
/// a failed import's error item, which only fills a name that nothing else brings in. Only what a
/// glob import could bring in can be hidden, since anything else that binds a named import's name
/// would be a duplicate of it.
pub(crate) fn hides_alternatives(
    ctx: &Compiler,
    scope: &Scope,
    namespace: Namespace,
    name: &str,
) -> bool {
    let Some((item, via)) = scope.item_with_import(namespace, name) else {
        return false;
    };

    if ctx.is_error_item(item) {
        return false;
    }

    let Some(via) = via else {
        return true;
    };

    !ctx.import(via).is_glob()
        && !scope.imports().any(|import| {
            matches!(&ctx.import(import).items, Items::Named(item) if item.text() == name)
                && ctx.is_uncertain_import(import)
        })
}

/// The imports into a scope, or only the exported ones, to collect what they could bring in
/// besides what they do (see
/// [`Database::import_alternatives`](rue_hir::Database::import_alternatives)).
fn alternative_imports<'a>(
    ctx: &'a Compiler,
    scope: &'a Scope,
    exported_only: bool,
) -> impl Iterator<Item = ImportId> + 'a {
    scope
        .imports()
        .filter(move |&import| ctx.import(import).exported || !exported_only)
}

/// What an import into a scope could bring in besides what it does (see
/// [`Database::import_alternatives`](rue_hir::Database::import_alternatives)) that nothing in the
/// scope hides (see [`hides_alternatives`]), with the namespace and name of each.
pub(crate) fn unhidden_alternatives(
    ctx: &Compiler,
    scope: ScopeId,
    import: ImportId,
) -> impl Iterator<Item = (Namespace, &str, Item)> + '_ {
    let glob = ctx.import(import).is_glob();

    ctx.all_import_alternatives(import)
        .filter(move |&(namespace, name, _)| {
            !glob || !hides_alternatives(ctx, ctx.scope(scope), namespace, name)
        })
}

/// What the imports into a scope (or only the exported ones) could bring in besides what they do
/// (see [`Database::import_alternatives`](rue_hir::Database::import_alternatives)), that nothing in
/// the scope hides, which is what it could pass on.
pub(crate) fn scope_alternatives(
    ctx: &Compiler,
    scope: ScopeId,
    exported_only: bool,
) -> impl Iterator<Item = (Namespace, &str, Item)> + '_ {
    alternative_imports(ctx, ctx.scope(scope), exported_only)
        .flat_map(move |import| unhidden_alternatives(ctx, scope, import))
}

/// What a name is bound to in one namespace where a segment of a path is looked up, if anything
/// other than a failed import's error item, along with what else the imports there could bring in
/// with the name, if the modules their paths name were bound differently, unless it's hidden (see
/// [`hides_alternatives`]). Those are returned with the import. With `exported_only`, only what the place
/// exports counts, as when another module imports it.
fn lookup(
    ctx: &Compiler,
    place: Place,
    namespace: Namespace,
    name: &str,
    exported_only: bool,
) -> (Option<Found>, Vec<Found>) {
    let mut others = Vec::new();

    let mut visit = |scope: &Scope| {
        // A private item hides what glob imports could bring in too, even where only what the
        // scope exports counts. Whether the name is hidden is only worked out once a glob import
        // could bring in something with it.
        let mut hidden = None;

        for import in alternative_imports(ctx, scope, exported_only) {
            let mut items = ctx.import_alternatives(import, namespace, name).peekable();

            if items.peek().is_none()
                || (ctx.import(import).is_glob()
                    && *hidden
                        .get_or_insert_with(|| hides_alternatives(ctx, scope, namespace, name)))
            {
                continue;
            }

            others.extend(items.map(|item| (item, Some(import))));
        }

        scope
            .item_with_import(namespace, name)
            .filter(|&(item, _)| !exported_only || scope.is_item_exported(item))
    };

    let binding = match place {
        Place::Chain(scope) => ctx.lookup_in(scope, |_, scope| visit(scope)),
        Place::Scope(scope) => visit(ctx.scope(scope)),
    };

    let binding = binding.filter(|&(item, _)| !ctx.is_error_item(item));

    (binding, others)
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
/// the one it's resolved to (see [`alternative_items`]). A name that glob imports bring in
/// different modules with could also name each of them (see
/// [`Database::ambiguous_alternatives`](rue_hir::Database::ambiguous_alternatives)). Each is
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
    // but another glob import could bring in the same module, which it uses too, so that's kept
    // (and left out of where the rest of the path is looked up by `modules_of`)
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
