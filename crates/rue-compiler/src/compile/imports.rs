use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    mem,
};

use indexmap::IndexMap;
use rowan::TextRange;
use rue_ast::{AstImportItem, AstImportPathSegment};
use rue_diagnostic::{DiagnosticKind, Name};
use rue_hir::{
    Declaration, Import, ImportId, ImportUser, Item, Items, Namespace, ScopeId, Symbol, SymbolId,
};

use crate::{Compiler, SyntaxItemKind};

pub fn declare_import_item(ctx: &mut Compiler, import: &AstImportItem) {
    let Some(path) = import.path() else {
        return;
    };

    let base_scope = ctx.last_scope_id();
    let module_stack = ctx.parent_module_stack();

    let imports = construct_imports(
        ctx,
        base_scope,
        module_stack,
        Vec::new(),
        &path.segments().collect::<Vec<_>>(),
        import.export().is_some(),
    );

    for import in imports {
        ctx.last_scope_mut().add_import(import);
        ctx.add_relevant_import(import);
    }
}

fn construct_imports(
    ctx: &mut Compiler,
    mut base_scope: ScopeId,
    mut module_stack: Vec<SymbolId>,
    mut path: Vec<Name>,
    segments: &[AstImportPathSegment],
    exported: bool,
) -> Vec<ImportId> {
    let mut has_non_super = false;
    let mut has_super = false;

    if let Some(segment) = segments.first()
        && let Some(separator) = segment.separator()
    {
        ctx.diagnostic(&separator, DiagnosticKind::PathSeparatorInFirstSegment);
    }

    for segment in segments.iter().take(segments.len() - 1) {
        if let Some(name) = segment.name() {
            if name.text() == "super" {
                if has_non_super {
                    ctx.diagnostic(&name, DiagnosticKind::SuperAfterNamedImport);
                } else if let Some(module) = module_stack.pop() {
                    base_scope = ctx.module(module).scope;

                    ctx.add_syntax(SyntaxItemKind::SymbolReference(module), name.text_range());
                } else {
                    ctx.diagnostic(&name, DiagnosticKind::UnresolvedSuper);
                }

                has_super = true;
            } else {
                path.push(ctx.local_name(&name));
                has_non_super = true;
            }
        }
    }

    let Some(last) = segments.last() else {
        return vec![];
    };

    if let Some(name) = last.name()
        && name.text() == "super"
    {
        // There's nothing to import, so the import isn't also reported as unresolved
        ctx.diagnostic(&name, DiagnosticKind::SuperAtEnd);
        return vec![];
    }

    let source = ctx.source().clone();

    if let Some(name) = last.name() {
        let name = ctx.local_name(&name);

        vec![ctx.alloc_import(Import {
            base_scope,
            source,
            path,
            items: Items::Named(name),
            exported,
            declarations: Vec::new(),
            has_super,
        })]
    } else if let Some(star) = last.star() {
        let star = ctx.local_name(&star);

        vec![ctx.alloc_import(Import {
            base_scope,
            source,
            path,
            items: Items::All(star),
            exported,
            declarations: Vec::new(),
            has_super,
        })]
    } else if let items = last.items().collect::<Vec<_>>()
        && !items.is_empty()
    {
        let mut imports = Vec::new();

        for item in items {
            imports.extend(construct_imports(
                ctx,
                base_scope,
                module_stack.clone(),
                path.clone(),
                &item.segments().collect::<Vec<_>>(),
                exported,
            ));
        }

        imports
    } else {
        vec![]
    }
}

#[derive(Debug, Default)]
pub struct ImportCache {
    /// The modules that paths resolve to from each scope. A path whose first segment could be
    /// brought into the scope by the import itself is cached for that import only, since the
    /// other imports in the scope wait for it to be resolved and then find what it brought in.
    scopes: HashMap<(ScopeId, Option<ImportId>, Vec<String>), CachedPath>,
    unused_imports: IndexMap<ImportId, IndexMap<String, Name>>,
    glob_import_counts: IndexMap<ImportId, (Name, usize)>,
    /// The errors in imports, keyed by where they are, so that each is reported once however
    /// many times the import is resolved.
    errors: IndexMap<(ImportId, ImportError), (Name, DiagnosticKind)>,
    /// The lookups that were answered without waiting for a module to settle, since nothing else
    /// could be resolved. They're checked once every import is resolved.
    forced: IndexMap<(ImportId, LookupKey), ForcedLookup>,

    /// The names in imports that refer to a declaration through another import, which are
    /// ambiguous if that import is an ambiguous glob import. They're checked once every glob
    /// import is resolved.
    imported_names: IndexMap<(ImportId, Declaration), (Name, ImportId)>,
}

/// An error in an import, which is reported once all imports are resolved.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ImportError {
    /// The segment of the path at this index names a private module.
    PrivateModule(usize),
    PrivateSymbol,
    PrivateType,
    /// The name is already declared in the scope as a different symbol.
    DuplicateSymbol,
    /// The name is already declared in the scope as a different type.
    DuplicateType,
}

impl ImportError {
    /// The error for a private item in a namespace, along with its diagnostic. Values and
    /// modules are both reported as symbols, so a name that's both is reported once.
    fn private(namespace: Namespace, name: &str, module: &str) -> (Self, DiagnosticKind) {
        let (name, module) = (name.to_string(), module.to_string());

        match namespace {
            Namespace::Value | Namespace::Module => (
                Self::PrivateSymbol,
                DiagnosticKind::PrivateSymbol(name, module),
            ),
            Namespace::Type => (Self::PrivateType, DiagnosticKind::PrivateType(name, module)),
        }
    }

    /// The error for a name that's already declared in a namespace, along with its diagnostic.
    fn duplicate(namespace: Namespace, name: &str) -> (Self, DiagnosticKind) {
        let name = name.to_string();

        match namespace {
            Namespace::Value | Namespace::Module => {
                (Self::DuplicateSymbol, DiagnosticKind::DuplicateSymbol(name))
            }
            Namespace::Type => (Self::DuplicateType, DiagnosticKind::DuplicateType(name)),
        }
    }
}

/// The module that a path resolves to.
#[derive(Debug, Clone)]
struct CachedPath {
    scope: ScopeId,
    symbol: SymbolId,
    /// The declarations referenced through other imports while resolving the path, with the
    /// index of the segment that names each, so that they're recorded for every import that uses
    /// it.
    references: Vec<(usize, ImportId, Declaration)>,
    /// The indices of the segments of the path that name a private module.
    private_segments: Vec<usize>,
}

fn flatten_imports(
    ctx: &mut Compiler,
    top_level_modules: Vec<SymbolId>,
) -> Vec<(ScopeId, ImportId)> {
    let mut imports = Vec::new();
    let mut stack = vec![(None, top_level_modules)];

    while let Some((scope, modules)) = stack.pop() {
        if let Some(scope) = scope {
            for import in ctx.scope(scope).imports() {
                imports.push((scope, import));
            }
        }

        for module in modules {
            let Symbol::Module(module) = ctx.symbol(module).clone() else {
                unreachable!();
            };

            stack.push((Some(module.scope), module.declarations.modules.clone()));
        }
    }

    imports
}

pub fn resolve_imports(
    ctx: &mut Compiler,
    all_modules: Vec<SymbolId>,
    cache: &mut ImportCache,
    diagnostics: bool,
) {
    let mut imports = flatten_imports(ctx, all_modules);
    imports.sort_by(|(_, left), (_, right)| compare_imports(ctx.import(*left), ctx.import(*right)));
    let mut resolution = Resolution {
        diagnostics,
        named_imports: HashMap::new(),
        glob_imports: HashMap::new(),
        bases: HashMap::new(),
        bound: HashSet::new(),
        waiting: Vec::new(),
        private_names: Vec::new(),
    };
    let mut missing_imports = IndexMap::new();

    for &(scope, import) in &imports {
        match &ctx.import(import).items {
            Items::Named(name) => resolution
                .named_imports
                .entry(scope)
                .or_default()
                .entry(name.text().to_string())
                .or_default()
                .push(import),
            Items::All(_) => resolution
                .glob_imports
                .entry(scope)
                .or_default()
                .push(import),
        }
    }

    loop {
        let mut updated = false;
        resolution.waiting.clear();
        resolution.private_names.clear();

        for &(import_scope, import) in &imports {
            updated |= resolve_import(
                ctx,
                import_scope,
                import,
                cache,
                &mut resolution,
                &mut missing_imports,
            );
        }

        if updated {
            continue;
        }

        // Nothing else can be resolved, so the lookups that are waiting for a module to settle
        // are answered without waiting. They're all answered from the state they were waiting in,
        // so that none of them depends on what another one brings in (or on the order that
        // they're in), and they're checked once every import is resolved.
        if resolution.waiting.is_empty() {
            break;
        }

        for lookup in stuck_lookups(mem::take(&mut resolution.waiting)) {
            cache.forced.entry(lookup.key).or_insert(lookup.forced);
        }
    }

    // The uses of a name that's reported as private at its import aren't reported again. This is
    // only done once every import is resolved, since a public item with the name could be found
    // in a later pass, and the name isn't blocked if something else brought it in.
    for (scope, name, namespaces) in resolution.private_names.drain(..) {
        let scope = ctx.scope_mut(scope);

        for namespace in namespaces {
            if scope.item(namespace, &name).is_none() {
                scope.block(namespace, name.clone());
            }
        }
    }

    if diagnostics {
        for missing_imports in missing_imports.into_values() {
            for name in missing_imports.into_values() {
                ctx.diagnostic_name(
                    &name,
                    DiagnosticKind::UnresolvedImport(name.text().to_string()),
                );
            }
        }

        for (name, kind) in cache.errors.values() {
            ctx.diagnostic_name(name, kind.clone());
        }

        check_forced_lookups(ctx, cache);

        for (&(_, declaration), (name, via)) in &cache.imported_names {
            if ctx.is_ambiguous_import(Some(*via), declaration) {
                ctx.diagnostic_name(
                    name,
                    DiagnosticKind::AmbiguousImport(name.text().to_string()),
                );
            }
        }

        for unused_imports in cache.unused_imports.values() {
            for name in unused_imports.values() {
                ctx.lint_sites_mut().empty_imports.push(name.clone());
            }
        }

        for (name, count) in cache.glob_import_counts.values() {
            if *count == 0 {
                ctx.lint_sites_mut().empty_imports.push(name.clone());
            }
        }
    }
}

fn compare_imports(left: &Import, right: &Import) -> Ordering {
    import_priority(&left.items)
        .cmp(&import_priority(&right.items))
        .then_with(|| {
            left.path
                .iter()
                .map(Name::text)
                .cmp(right.path.iter().map(Name::text))
        })
        .then_with(|| import_item_name(&left.items).cmp(import_item_name(&right.items)))
        .then_with(|| left.exported.cmp(&right.exported))
}

fn import_priority(items: &Items) -> u8 {
    match items {
        Items::Named(_) => 0,
        Items::All(_) => 1,
    }
}

fn import_item_name(items: &Items) -> &str {
    match items {
        Items::Named(name) | Items::All(name) => name.text(),
    }
}

/// One of the lookups an import makes that can look past a module's own items: to a file's sibling
/// modules, or the prelude.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum LookupKey {
    /// The first segment of the import's path.
    Path,
    /// The imported name, in a namespace.
    Item(Namespace),
    /// A file's sibling module that a glob import of the file (through `super`) brings in.
    Sibling(Namespace, String),
}

/// A lookup that's waiting for a module to settle, with what it'd find if it didn't wait, and
/// the imports it waits for.
#[derive(Debug)]
struct WaitingLookup {
    key: (ImportId, LookupKey),
    forced: ForcedLookup,
    blockers: Vec<ImportId>,
}

/// A lookup that was answered without waiting for the module it looked past to settle.
#[derive(Debug, Clone)]
struct ForcedLookup {
    /// The module whose own items were looked past.
    scope: ScopeId,
    namespace: Namespace,
    /// The name, where it's written in the import.
    name: Name,
    answer: Option<(Item, Option<ImportId>)>,
}

/// Reports each lookup that was answered without waiting, if the module it looked past ended up
/// with a different item with the name (other than one the import itself brought in). The name
/// would refer to that item everywhere else in the module, so the import is ambiguous.
fn check_forced_lookups(ctx: &mut Compiler, cache: &ImportCache) {
    let mut reported = HashSet::new();

    for (&(import, _), forced) in &cache.forced {
        let Some((answer, _)) = forced.answer else {
            continue;
        };

        let found = ctx
            .scope(forced.scope)
            .item_with_import(forced.namespace, forced.name.text())
            .filter(|&(_, via)| via != Some(import));

        if let Some((item, _)) = found
            && item != answer
            && reported.insert((import, forced.name.text().to_string()))
        {
            ctx.diagnostic_name(
                &forced.name,
                DiagnosticKind::AmbiguousOuterImport(forced.name.text().to_string()),
            );
        }
    }
}

/// The import that a path is cached for, if its first segment could be brought into the scope by
/// the import itself (see [`ImportCache::scopes`]).
fn path_owner(
    resolution: &Resolution,
    import: &Import,
    import_scope: ScopeId,
    import_id: ImportId,
) -> Option<ImportId> {
    let first = import.path.first()?;

    (import.base_scope == import_scope
        && resolution.may_bring_in_itself(import_scope, first.text(), import_id))
    .then_some(import_id)
}

/// The state of one call to [`resolve_imports`].
struct Resolution {
    diagnostics: bool,
    /// The named imports of each name into each scope.
    named_imports: HashMap<ScopeId, HashMap<String, Vec<ImportId>>>,
    /// The glob imports into each scope.
    glob_imports: HashMap<ScopeId, Vec<ImportId>>,
    /// The module each import whose path has been resolved imports from, and whether all of its
    /// items can be imported (when it's named by `super`, which includes a file's sibling
    /// modules) rather than only its exported ones.
    bases: HashMap<ImportId, (ScopeId, bool)>,
    /// The named imports that have brought in what they import.
    bound: HashSet<ImportId>,
    /// The lookups that are waiting for a module to settle in this pass.
    waiting: Vec<WaitingLookup>,
    /// The names that named imports found only private items for, with the scope they're
    /// imported into and the namespaces the private items are in.
    private_names: Vec<(ScopeId, String, Vec<Namespace>)>,
}

impl Resolution {
    fn named_imports_of(&self, scope: ScopeId, name: &str) -> Option<&Vec<ImportId>> {
        self.named_imports.get(&scope)?.get(name)
    }

    /// Whether the import is one of those that could bring the name into the scope: its named
    /// imports of the name, or if it has none, its glob imports.
    fn may_bring_in_itself(&self, scope: ScopeId, name: &str, importer: ImportId) -> bool {
        self.named_imports_of(scope, name)
            .or_else(|| self.glob_imports.get(&scope))
            .is_some_and(|imports| imports.contains(&importer))
    }
}

/// What a lookup waits for: the imports that could still bring the name in, because their paths
/// aren't resolved yet.
#[derive(Debug, Default)]
struct Blockers {
    imports: Vec<ImportId>,
    visited: HashSet<(ScopeId, bool)>,
}

/// Whether a module that doesn't have an item with a name in a namespace could still bring one
/// in, through one of its imports other than `importer`. A named import could until it has
/// brought in what it imports, unless the module it imports from can't have the item. A glob
/// import could until its path is resolved, and then if the module it imports from has the item
/// or could still bring it in. With `exported`, only the items that the module would export count,
/// as for a glob import of it. A cycle of modules that nothing else brings the item into can't
/// bring it in. Every import that could is recorded in `blockers`.
#[allow(clippy::too_many_arguments)]
fn may_gain(
    ctx: &Compiler,
    resolution: &Resolution,
    scope: ScopeId,
    namespace: Namespace,
    name: &str,
    importer: Option<ImportId>,
    exported: bool,
    blockers: &mut Blockers,
) -> bool {
    if !blockers.visited.insert((scope, exported)) {
        return false;
    }

    let target = ctx.scope(scope);

    if let Some(item) = target.item(namespace, name)
        && (!exported || target.is_item_exported(item))
    {
        return true;
    }

    let relevant =
        |import: ImportId| Some(import) != importer && (!exported || ctx.import(import).exported);

    let mut gains = false;

    for &import in resolution
        .named_imports_of(scope, name)
        .into_iter()
        .flatten()
    {
        if relevant(import) {
            gains |= may_bind(ctx, resolution, import, namespace, name, blockers);
        }
    }

    for &import in resolution.glob_imports.get(&scope).into_iter().flatten() {
        if relevant(import) {
            gains |= may_import(ctx, resolution, import, namespace, name, blockers);
        }
    }

    gains
}

/// Whether a named import could still bring in an item with its name in a namespace.
fn may_bind(
    ctx: &Compiler,
    resolution: &Resolution,
    import: ImportId,
    namespace: Namespace,
    name: &str,
    blockers: &mut Blockers,
) -> bool {
    !resolution.bound.contains(&import)
        && may_import(ctx, resolution, import, namespace, name, blockers)
}

/// Whether the module that an import imports from has, or could still bring in, an item with a
/// name in a namespace that the import would bring in. It could if the import's path isn't
/// resolved yet.
fn may_import(
    ctx: &Compiler,
    resolution: &Resolution,
    import: ImportId,
    namespace: Namespace,
    name: &str,
    blockers: &mut Blockers,
) -> bool {
    let Some(&(source, members)) = resolution.bases.get(&import) else {
        blockers.imports.push(import);
        return true;
    };

    may_gain(
        ctx, resolution, source, namespace, name, None, !members, blockers,
    ) || (members && ctx.sibling_item(source, namespace, name).is_some())
}

/// The waiting lookups that are answered without waiting once nothing else can be resolved: those
/// of the imports that only wait for each other (or for nothing that's still waiting). Imports
/// that wait for those are answered later, once they're resolved, so an inline module's lookup
/// through `super` waits for the cycle of imports in the module it names.
fn stuck_lookups(waiting: Vec<WaitingLookup>) -> Vec<WaitingLookup> {
    let waiting_imports = waiting
        .iter()
        .map(|lookup| lookup.key.0)
        .collect::<HashSet<_>>();

    let mut edges: HashMap<ImportId, HashSet<ImportId>> = HashMap::new();

    for lookup in &waiting {
        let blocked_by = edges.entry(lookup.key.0).or_default();

        for &blocker in &lookup.blockers {
            if blocker != lookup.key.0 && waiting_imports.contains(&blocker) {
                blocked_by.insert(blocker);
            }
        }
    }

    let reachable = |from: ImportId| {
        let mut seen = HashSet::new();
        let mut stack = vec![from];

        while let Some(import) = stack.pop() {
            for &next in edges.get(&import).into_iter().flatten() {
                if seen.insert(next) {
                    stack.push(next);
                }
            }
        }

        seen
    };

    // An import is stuck if every waiting import it waits for (directly or not) waits for it too
    let stuck = waiting_imports
        .iter()
        .copied()
        .filter(|&import| {
            reachable(import)
                .into_iter()
                .all(|other| reachable(other).contains(&import))
        })
        .collect::<HashSet<_>>();

    waiting
        .into_iter()
        .filter(|lookup| stuck.contains(&lookup.key.0))
        .collect()
}

/// Where an import looks a name up.
#[derive(Debug, Clone, Copy)]
enum Base {
    /// In the scope the import is in, and then the ones it's in (a file's sibling modules and
    /// the prelude), like a use of the name there.
    Local(ScopeId),
    /// In the module that `super` names, and then a file's sibling modules. It contains the
    /// import, so its private items can be used.
    Member(ScopeId),
}

/// What an import's lookup of a name found.
enum Lookup {
    /// The item, and the import that brought it into the scope it was found in, if any.
    Found(Item, Option<ImportId>),
    /// The module could still bring the name in, so it isn't known yet.
    Undetermined,
    NotFound,
}

impl Lookup {
    fn from_answer(answer: Option<(Item, Option<ImportId>)>) -> Self {
        match answer {
            Some((item, via)) => Self::Found(item, via),
            None => Self::NotFound,
        }
    }

    /// The item, if it's known. If it isn't known yet, `undetermined` is set.
    fn found(self, undetermined: &mut bool) -> Option<(Item, Option<ImportId>)> {
        match self {
            Self::Found(item, via) => Some((item, via)),
            Self::Undetermined => {
                *undetermined = true;
                None
            }
            Self::NotFound => None,
        }
    }
}

/// Looks up a name for an import, the way that a use of it would find it once every import is
/// resolved. The module's own items shadow what's outside of it, so when it doesn't have the name
/// yet but could still bring it in, the lookup waits. Once nothing else can be resolved, it's
/// answered with what's outside (see [`check_forced_lookups`]).
#[allow(clippy::too_many_arguments)]
fn lookup(
    ctx: &Compiler,
    forced: &IndexMap<(ImportId, LookupKey), ForcedLookup>,
    resolution: &mut Resolution,
    import_id: ImportId,
    key: LookupKey,
    at: &Name,
    base: Base,
    namespace: Namespace,
) -> Lookup {
    let name = at.text();
    let scope = match base {
        Base::Local(scope) | Base::Member(scope) => scope,
    };

    if let Some((item, via)) = ctx.scope(scope).item_with_import(namespace, name) {
        return Lookup::Found(item, via);
    }

    if let Some(forced) = forced.get(&(import_id, key.clone())) {
        return Lookup::from_answer(forced.answer);
    }

    let outside = match base {
        Base::Local(scope) => ctx
            .scope(scope)
            .parent()
            .and_then(|parent| ctx.resolve_item_in(parent, namespace, name)),
        Base::Member(scope) => ctx.sibling_item(scope, namespace, name),
    };

    let mut blockers = Blockers::default();

    if !resolution.diagnostics
        && may_gain(
            ctx,
            resolution,
            scope,
            namespace,
            name,
            Some(import_id),
            false,
            &mut blockers,
        )
    {
        resolution.waiting.push(WaitingLookup {
            key: (import_id, key),
            forced: ForcedLookup {
                scope,
                namespace,
                name: at.clone(),
                answer: outside,
            },
            blockers: blockers.imports,
        });
        return Lookup::Undetermined;
    }

    Lookup::from_answer(outside)
}

fn resolve_import(
    ctx: &mut Compiler,
    import_scope: ScopeId,
    import_id: ImportId,
    cache: &mut ImportCache,
    resolution: &mut Resolution,
    missing_imports: &mut IndexMap<ImportId, IndexMap<String, Name>>,
) -> bool {
    let diagnostics = resolution.diagnostics;
    let import = ctx.import(import_id).clone();
    let has_super = import.has_super;
    let source = import.source.clone();

    let mut base = None;
    let mut path_so_far = Vec::new();
    let mut path_references = Vec::new();
    let mut private_segments = Vec::new();
    let owner = path_owner(resolution, &import, import_scope, import_id);

    // A path that starts with `super` is resolved from another module's scope, so it isn't
    // cached under the names in it, which other imports in the scope would share
    for i in (1..=import.path.len()).rev().filter(|_| !has_super) {
        let subpath = import
            .path
            .iter()
            .take(i)
            .map(|t| t.text().to_string())
            .collect::<Vec<_>>();

        if let Some(cached) = cache.scopes.get(&(import_scope, owner, subpath.clone())) {
            base = Some(cached.scope);
            path_so_far = subpath;
            path_references.clone_from(&cached.references);
            private_segments.clone_from(&cached.private_segments);

            for &(_, import, declaration) in &cached.references {
                ctx.add_import_reference(import, declaration, ImportUser::Import(import_id));
            }

            for name in import.path.iter().take(path_so_far.len()) {
                if diagnostics && let Some(srcloc) = name.srcloc() {
                    ctx.add_syntax_for_source(
                        SyntaxItemKind::SymbolReference(cached.symbol),
                        TextRange::new(
                            srcloc.span.start.try_into().unwrap(),
                            srcloc.span.end.try_into().unwrap(),
                        ),
                        source.kind.clone(),
                    );
                }
            }

            break;
        }
    }

    for (index, name) in import.path.iter().enumerate().skip(path_so_far.len()) {
        // A private module is reported, but still resolved, like it is in other paths
        let module = if let Some(base) = base {
            let base = ctx.scope(base);
            base.module_with_import(name.text())
                .map(|(m, via)| (m, via, base.is_module_exported(m)))
        } else {
            let base = if import.base_scope == import_scope {
                Base::Local(import_scope)
            } else {
                Base::Member(import.base_scope)
            };

            match lookup(
                ctx,
                &cache.forced,
                resolution,
                import_id,
                LookupKey::Path,
                name,
                base,
                Namespace::Module,
            ) {
                Lookup::Found(module, via) => module.symbol().map(|module| (module, via, true)),
                Lookup::Undetermined => return false,
                Lookup::NotFound => None,
            }
        };

        let Some((symbol, via, exported)) = module else {
            // A name that a failed import blocked where it's looked up has already been reported
            let blocked = Namespace::ALL.into_iter().any(|namespace| {
                if let Some(base) = base {
                    ctx.scope(base).is_blocked(namespace, name.text())
                } else if import.base_scope == import_scope {
                    ctx.is_blocked_in(import.base_scope, namespace, name.text())
                } else {
                    ctx.scope(import.base_scope)
                        .is_blocked(namespace, name.text())
                }
            });

            if diagnostics && !blocked {
                // Only a module can have a path after it, so a value or type is reported as such
                let other = [Namespace::Value, Namespace::Type]
                    .into_iter()
                    .any(|namespace| {
                        if let Some(base) = base {
                            ctx.scope(base).item(namespace, name.text()).is_some()
                        } else if import.base_scope == import_scope {
                            ctx.resolve_item_in(import.base_scope, namespace, name.text())
                                .is_some()
                        } else {
                            ctx.module_member(import.base_scope, namespace, name.text())
                                .is_some()
                        }
                    });

                ctx.diagnostic_name(
                    name,
                    if other {
                        DiagnosticKind::SubpathNotSupported(name.text().to_string())
                    } else {
                        DiagnosticKind::UndeclaredSymbol(name.text().to_string())
                    },
                );
            }
            return false;
        };

        if let Some(via) = via {
            ctx.add_import_reference(
                via,
                Declaration::Symbol(symbol),
                ImportUser::Import(import_id),
            );
            path_references.push((index, via, Declaration::Symbol(symbol)));
        }

        if diagnostics && let Some(srcloc) = name.srcloc() {
            ctx.add_syntax_for_source(
                SyntaxItemKind::SymbolReference(symbol),
                TextRange::new(
                    srcloc.span.start.try_into().unwrap(),
                    srcloc.span.end.try_into().unwrap(),
                ),
                source.kind.clone(),
            );
        }

        let Symbol::Module(module) = ctx.symbol(symbol) else {
            unreachable!("the module namespace only has modules");
        };

        base = Some(module.scope);
        path_so_far.push(name.text().to_string());

        if !exported {
            private_segments.push(index);
        }

        if !has_super {
            cache.scopes.insert(
                (import_scope, owner, path_so_far.clone()),
                CachedPath {
                    scope: module.scope,
                    symbol,
                    references: path_references.clone(),
                    private_segments: private_segments.clone(),
                },
            );
        }
    }

    // The module the import's items are imported from, which other lookups can wait for. A name
    // that's looked up from the import's own scope doesn't come from a module.
    if let Some(base) = base {
        resolution.bases.insert(import_id, (base, false));
    } else if import.base_scope != import_scope {
        resolution
            .bases
            .insert(import_id, (import.base_scope, true));
    }

    // Errors are only recorded once every declaration is known
    if diagnostics {
        for &(index, via, declaration) in &path_references {
            cache
                .imported_names
                .entry((import_id, declaration))
                .or_insert_with(|| (import.path[index].clone(), via));
        }

        for &index in &private_segments {
            cache
                .errors
                .entry((import_id, ImportError::PrivateModule(index)))
                .or_insert_with(|| {
                    let name = &import.path[index];

                    (
                        name.clone(),
                        DiagnosticKind::PrivateSymbol(
                            name.text().to_string(),
                            import.path[index - 1].text().to_string(),
                        ),
                    )
                });
        }
    }

    let mut updated = false;

    match import.items {
        Items::All(star) => {
            let count = &mut cache
                .glob_import_counts
                .entry(import_id)
                .or_insert_with(|| (star.clone(), 0))
                .1;

            // Glob imports bring in every exported item of a module. A glob import of a parent
            // module (through `super`) brings in its private items too.
            let source_scope = base.unwrap_or(import.base_scope);

            for namespace in Namespace::ALL {
                let items = if base.is_some() {
                    ctx.scope(source_scope).exported_items(namespace)
                } else {
                    // A file's sibling modules are brought in too, unless the file has an item
                    // with the name, or could still bring one in through its imports
                    let mut items = ctx.scope(source_scope).items(namespace);

                    for (name, _) in ctx.sibling_items(source_scope, namespace) {
                        if ctx.scope(source_scope).item(namespace, &name).is_some() {
                            continue;
                        }

                        let at = Name::new(name.as_str(), star.srcloc().cloned());

                        if let Lookup::Found(item, _) = lookup(
                            ctx,
                            &cache.forced,
                            resolution,
                            import_id,
                            LookupKey::Sibling(namespace, name.clone()),
                            &at,
                            Base::Member(source_scope),
                            namespace,
                        ) {
                            items.push((name, item));
                        }
                    }

                    items
                };

                for (name, item) in items {
                    // A named import of the name shadows the glob import in the namespaces that
                    // it brings an item into, or could still
                    if resolution
                        .named_imports_of(import_scope, &name)
                        .into_iter()
                        .flatten()
                        .any(|&named| {
                            ctx.scope(import_scope).item_import(item) == Some(named)
                                || may_bind(
                                    ctx,
                                    resolution,
                                    named,
                                    namespace,
                                    &name,
                                    &mut Blockers::default(),
                                )
                        })
                    {
                        continue;
                    }

                    // The import the item was brought into the module by, if any
                    let via = ctx.scope(source_scope).item_import(item);

                    // A name that's ambiguous where it's imported from is ambiguous here too. The
                    // imports are resolved again until nothing changes, so this reaches the end
                    // of any chain of glob imports.
                    if ctx.is_ambiguous_import(via, item.into())
                        && ctx.scope(import_scope).item_import(item) == Some(import_id)
                    {
                        updated |= ctx.add_ambiguous_import(import_id, item.into());
                    }

                    let target = ctx.scope_mut(import_scope);
                    let existing = target.item(namespace, &name);

                    if existing.is_none() {
                        target.insert_item(name.clone(), item, import.exported);
                        target.add_item_import(item, import_id);
                        ctx.import_mut(import_id)
                            .declarations
                            .push((name, item.into()));
                        updated = true;
                        *count += 1;
                    } else if !target.is_item_exported(item)
                        && import.exported
                        && existing == Some(item)
                    {
                        target.export_item(item);
                        ctx.import_mut(import_id)
                            .declarations
                            .push((name, item.into()));
                        updated = true;
                        *count += 1;
                    } else if let Some(existing) = existing
                        && existing != item
                        && let Some(existing_import) = target.item_import(existing)
                        && matches!(ctx.import(existing_import).items, Items::All(_))
                    {
                        updated |= ctx.add_ambiguous_import(existing_import, existing.into());
                    }
                }
            }
        }
        Items::Named(item) => {
            let missing_imports = missing_imports.entry(import_id).or_insert_with(|| {
                [item.clone()]
                    .into_iter()
                    .map(|item| (item.text().to_string(), item))
                    .collect()
            });

            let unused_imports = cache.unused_imports.entry(import_id).or_insert_with(|| {
                [item.clone()]
                    .into_iter()
                    .map(|item| (item.text().to_string(), item))
                    .collect()
            });

            let name = item.text();

            // Each item is resolved along with the import it was brought into its scope by, if
            // any, in every namespace that has the name
            let found: Vec<(Item, Option<ImportId>)> = if let Some(base) = base {
                let base = ctx.scope(base);
                let items = Namespace::ALL
                    .into_iter()
                    .filter_map(|namespace| {
                        let (found, via) = base.item_with_import(namespace, name)?;
                        Some((found, via, base.is_item_exported(found)))
                    })
                    .collect::<Vec<_>>();

                // If another namespace has a public item with the name, only it's imported and
                // nothing is reported. Private items are reported, but never imported: an import
                // can't be undone, and a public item with the name may only be brought into the
                // module by an import that's resolved in a later pass, so a private item imported
                // now would stay imported.
                if items.iter().any(|&(_, _, public)| public) {
                    items
                        .into_iter()
                        .filter(|&(_, _, public)| public)
                        .map(|(found, via, _)| (found, via))
                        .collect()
                } else {
                    // The name is reported as private rather than unresolved
                    if !items.is_empty() {
                        missing_imports.shift_remove(name);
                        resolution.private_names.push((
                            import_scope,
                            name.to_string(),
                            items.iter().map(|&(found, ..)| found.namespace()).collect(),
                        ));
                    }

                    if diagnostics {
                        let module = import.path.last().unwrap().text();

                        for &(found, ..) in &items {
                            let (error, kind) =
                                ImportError::private(found.namespace(), name, module);

                            cache
                                .errors
                                .entry((import_id, error))
                                .or_insert_with(|| (item.clone(), kind));
                        }
                    }

                    Vec::new()
                }
            } else if import.base_scope == import_scope {
                // A namespace that isn't known yet is looked up again in a later pass
                let mut undetermined = false;

                let found = Namespace::ALL
                    .into_iter()
                    .filter_map(|namespace| {
                        lookup(
                            ctx,
                            &cache.forced,
                            resolution,
                            import_id,
                            LookupKey::Item(namespace),
                            &item,
                            Base::Local(import_scope),
                            namespace,
                        )
                        .found(&mut undetermined)
                    })
                    .collect::<Vec<_>>();

                if !undetermined && !found.is_empty() {
                    resolution.bound.insert(import_id);
                }

                found
            } else {
                let mut undetermined = false;

                let found = Namespace::ALL
                    .into_iter()
                    .filter_map(|namespace| {
                        lookup(
                            ctx,
                            &cache.forced,
                            resolution,
                            import_id,
                            LookupKey::Item(namespace),
                            &item,
                            Base::Member(import.base_scope),
                            namespace,
                        )
                        .found(&mut undetermined)
                    })
                    .collect::<Vec<_>>();

                if !undetermined && !found.is_empty() {
                    resolution.bound.insert(import_id);
                }

                // A name that a failed import blocked in that module has already been reported
                let blocked = Namespace::ALL
                    .into_iter()
                    .filter(|&namespace| ctx.scope(import.base_scope).is_blocked(namespace, name))
                    .collect::<Vec<_>>();

                if found.is_empty() && !blocked.is_empty() {
                    missing_imports.shift_remove(name);
                    resolution
                        .private_names
                        .push((import_scope, name.to_string(), blocked));
                }

                found
            };

            // This is recorded in every pass, since an import of a name with no path finds its own
            // binding once it has been resolved. The import that it's found through is used by
            // this one, like one in its path.
            for &(found, via) in &found {
                if let Some(via) = via
                    && via != import_id
                {
                    let declaration = found.into();
                    ctx.add_import_reference(via, declaration, ImportUser::Import(import_id));
                    cache
                        .imported_names
                        .entry((import_id, declaration))
                        .or_insert_with(|| (item.clone(), via));
                }
            }

            for &(found, _) in &found {
                let namespace = found.namespace();
                if diagnostics && let Some(srcloc) = item.srcloc() {
                    ctx.add_syntax_for_source(
                        match found {
                            Item::Value(symbol) | Item::Module(symbol) => {
                                SyntaxItemKind::SymbolReference(symbol)
                            }
                            Item::Type(ty) => SyntaxItemKind::TypeReference(ty),
                        },
                        TextRange::new(
                            srcloc.span.start.try_into().unwrap(),
                            srcloc.span.end.try_into().unwrap(),
                        ),
                        source.kind.clone(),
                    );
                }

                let target = ctx.scope_mut(import_scope);
                let existing = target.item(namespace, name);

                if existing.is_none() {
                    target.insert_item(name.to_string(), found, import.exported);
                    target.add_item_import(found, import_id);
                    ctx.import_mut(import_id)
                        .declarations
                        .push((name.to_string(), found.into()));
                    updated = true;
                    unused_imports.shift_remove(name);
                } else if !target.is_item_exported(found)
                    && import.exported
                    && existing == Some(found)
                {
                    target.export_item(found);
                    ctx.import_mut(import_id)
                        .declarations
                        .push((name.to_string(), found.into()));
                    updated = true;
                    unused_imports.shift_remove(name);
                } else if diagnostics && existing != Some(found) {
                    let (error, kind) = ImportError::duplicate(namespace, name);

                    cache
                        .errors
                        .entry((import_id, error))
                        .or_insert_with(|| (item.clone(), kind));
                    unused_imports.shift_remove(name);
                }
            }

            if !found.is_empty() {
                missing_imports.shift_remove(name);
            } else if diagnostics {
                unused_imports.shift_remove(name);
            }
        }
    }

    updated
}
