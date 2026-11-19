use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    mem,
};

use indexmap::{IndexMap, IndexSet};
use rowan::TextRange;
use rue_ast::{AstImportItem, AstImportPathSegment, AstNode};
use rue_diagnostic::{DiagnosticKind, Name, Source};
use rue_hir::{
    Declaration, Import, ImportId, ImportUser, Item, Items, Namespace, Scope, ScopeId, Symbol,
    SymbolId,
};
use rue_parser::SyntaxKind;

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
        false,
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
    mut from_parent: bool,
    mut path: Vec<Name>,
    segments: &[AstImportPathSegment],
    exported: bool,
) -> Vec<ImportId> {
    let mut has_non_super = false;

    // Only the first segment of the whole path can be `project`, not the first one in braces
    let starts_path = segments
        .first()
        .and_then(|segment| segment.syntax().parent()?.parent())
        .is_some_and(|parent| parent.kind() == SyntaxKind::ImportItem);

    if let Some(segment) = segments.first()
        && let Some(separator) = segment.separator()
    {
        ctx.diagnostic(&separator, DiagnosticKind::PathSeparatorInFirstSegment);
    }

    for (index, segment) in segments.iter().take(segments.len() - 1).enumerate() {
        if let Some(name) = segment.name() {
            if name.text() == "project" {
                if starts_path && index == 0 {
                    let root = ctx.project_root();
                    base_scope = ctx.module(root).scope;
                    from_parent = true;

                    // The root has no parent for a `super` after it to name
                    module_stack.clear();

                    ctx.add_syntax(SyntaxItemKind::SymbolReference(root), name.text_range());
                } else {
                    ctx.diagnostic(&name, DiagnosticKind::ProjectNotAtStart);
                }
            } else if name.text() == "super" {
                if has_non_super {
                    ctx.diagnostic(&name, DiagnosticKind::SuperAfterNamedImport);
                } else if let Some(module) = module_stack.pop() {
                    base_scope = ctx.module(module).scope;
                    from_parent = true;

                    ctx.add_syntax(SyntaxItemKind::SymbolReference(module), name.text_range());
                } else {
                    ctx.diagnostic(&name, DiagnosticKind::UnresolvedSuper);
                }
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

    if let Some(name) = last.name()
        && name.text() == "project"
    {
        // Like `super`, a path that names the root module has nothing to import
        let kind = if starts_path && segments.len() == 1 {
            DiagnosticKind::ProjectAtEnd
        } else {
            DiagnosticKind::ProjectNotAtStart
        };
        ctx.diagnostic(&name, kind);
        return vec![];
    }

    let source = ctx.source().clone();

    if let Some(name) = last.name() {
        let name = ctx.local_name(&name);

        vec![ctx.alloc_import(Import {
            base_scope,
            from_parent,
            source,
            path,
            items: Items::Named(name),
            exported,
            declarations: Vec::new(),
        })]
    } else if let Some(star) = last.star() {
        let star = ctx.local_name(&star);

        vec![ctx.alloc_import(Import {
            base_scope,
            from_parent,
            source,
            path,
            items: Items::All(star),
            exported,
            declarations: Vec::new(),
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
                from_parent,
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
    /// What each import's path resolves to, once it's resolved.
    paths: HashMap<ImportId, CachedPath>,
    unused_imports: IndexMap<ImportId, IndexMap<String, Name>>,
    glob_import_counts: IndexMap<ImportId, (Name, usize)>,
    /// The errors in imports, keyed by where they are, so that each is reported once however
    /// many times the import is resolved.
    errors: IndexMap<(ImportId, ImportError), (Name, DiagnosticKind)>,
    /// What each exported named import with no path re-exports: the scope it imports into, and
    /// each item it found, with whether it was found in the module it looks in (its own, or the
    /// one that `super` or `project` names) rather than outside of it, where items are public (a
    /// file's sibling modules and the prelude). See [`report_private_reexports`].
    reexports: IndexMap<ImportId, (ScopeId, IndexMap<Item, bool>)>,
    /// The lookups that were answered without waiting for a module to settle, since nothing else
    /// could be resolved. They're checked once every import is resolved.
    forced: IndexMap<(ImportId, LookupKey), ForcedLookup>,

    /// The names in imports that refer to a declaration through another import, which are
    /// ambiguous if that import is an ambiguous glob import. They're checked once every glob
    /// import is resolved.
    imported_names: IndexMap<(ImportId, Declaration), (Name, ImportId)>,

    /// The imports that failed at a name that's blocked where they looked it up (a segment of the
    /// path, or the name imported by an import without a path), with the origins of the block (see
    /// [`Scope::block`]). Whether their errors are reported depends on whether those imports'
    /// failures are (see [`failure_is_reported`]).
    blocked_imports: HashMap<ImportId, HashSet<ImportId>>,
}

/// An error in an import, which is reported once all imports are resolved.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ImportError {
    /// The segment of the path at this index names a private module.
    PrivateModule(usize),
    /// The segment of the path at this index doesn't name a module.
    UnresolvedPath(usize),
    PrivateSymbol,
    PrivateType,
    /// An exported import re-exports a private item.
    PrivateReexport,
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

/// What an import's path resolves to.
#[derive(Debug)]
struct CachedPath {
    /// The scope of the module that the path names.
    scope: ScopeId,
    /// The module that each segment names.
    symbols: Vec<SymbolId>,
    /// The declarations referenced through other imports while resolving the path, with the
    /// index of the segment that names each, so that they're recorded each time it's used.
    references: Vec<(usize, ImportId, Declaration)>,
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
        failed_paths: HashSet::new(),
        waiting: Vec::new(),
        failed_names: Vec::new(),
    };
    let mut missing_imports = IndexMap::new();
    let import_scopes = imports
        .iter()
        .map(|&(scope, _)| scope)
        .collect::<IndexSet<_>>();

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
        let bases = resolution.bases.len();
        let failed_paths = resolution.failed_paths.len();
        resolution.waiting.clear();
        resolution.failed_names.clear();

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

        // A lookup made before an import's path was resolved (or failed) in this pass waited for
        // it, so the lookups are only forced after a pass in which every resolved path was known
        // throughout. Lookups don't wait when diagnostics are reported, so this doesn't apply then.
        updated |= !diagnostics
            && (resolution.bases.len() != bases || resolution.failed_paths.len() != failed_paths);

        if updated {
            continue;
        }

        // Nothing else can be resolved, so the lookups of the imports that only wait for each
        // other (see `stuck_lookups`) are answered without waiting. They're all answered from the
        // state they were waiting in, so that none of them depends on what another one brings in
        // (or on the order that they're in), and they're checked once every import is resolved.
        // The other waiting lookups are retried once those are resolved.
        if resolution.waiting.is_empty() {
            // A reported re-export exports what it found, which changes what other imports find,
            // so the imports are resolved again
            if diagnostics && report_private_reexports(ctx, cache) {
                continue;
            }

            // The uses of a name whose named import failed aren't reported again. This is only
            // done once every import is resolved, since an item with the name could be found in a
            // later pass, and the name isn't blocked if something else brought it in. Blocking a
            // name can fail imports of it in other modules, such as through `super`, so the
            // imports are resolved again until nothing new is blocked.
            let mut blocked = false;

            // A name that's still missing fails its import too
            let missing = imports.iter().flat_map(|&(scope, import)| {
                missing_imports
                    .get(&import)
                    .into_iter()
                    .flat_map(IndexMap::keys)
                    .flat_map(move |name| FailedName::all(scope, name, import))
            });

            for failed in resolution.failed_names.drain(..).chain(missing) {
                blocked |= block(ctx, failed);
            }

            if blocked {
                continue;
            }

            // A block made by an import that no longer fails, such as one that resolves once a
            // reported re-export exports what it found (see `report_private_reexports`), doesn't
            // hide its name any more, so the imports are resolved again without it. Names are only
            // blocked in the scopes that imports are in.
            if diagnostics {
                let failing = failing_imports(&missing_imports, cache);
                let mut unblocked = false;

                for &scope in &import_scopes {
                    unblocked |= ctx
                        .scope_mut(scope)
                        .retain_block_origins(|origin| failing.contains(&origin));
                }

                if unblocked {
                    continue;
                }
            }

            break;
        }

        let waiting = mem::take(&mut resolution.waiting);

        for lookup in stuck_lookups(ctx, &resolution, waiting) {
            cache.forced.entry(lookup.key).or_insert(lookup.forced);
        }
    }

    if diagnostics {
        // An import that only failed in an earlier pass doesn't explain the failures at the blocks
        // it made then
        let failing = failing_imports(&missing_imports, cache);

        for (import, missing_imports) in missing_imports {
            if !failure_is_reported(cache, &failing, import) {
                continue;
            }

            for name in missing_imports.into_values() {
                ctx.diagnostic_name(
                    &name,
                    DiagnosticKind::UnresolvedImport(name.text().to_string()),
                );
            }
        }

        for (&(import, error), (name, kind)) in &cache.errors {
            if matches!(error, ImportError::UnresolvedPath(_))
                && !failure_is_reported(cache, &failing, import)
            {
                continue;
            }

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

/// Reports each exported import that only found items that are private where it found them, since
/// a re-export can't make a private item public, as in Rust. Each item an import finds is only
/// exported if it's public there, so a name that's public in only one namespace is re-exported in
/// only that one. A module can export an item through an import that's resolved later, so this is
/// done once every import is resolved. A reported import exports what it found anyway, so that the
/// uses and re-exports of the name aren't reported too, and the imports are resolved again if any
/// were reported.
fn report_private_reexports(ctx: &mut Compiler, cache: &mut ImportCache) -> bool {
    let private = cache
        .reexports
        .iter()
        .filter(|&(&import_id, (_, found))| {
            let module = ctx.scope(ctx.import(import_id).base_scope);

            !cache
                .errors
                .contains_key(&(import_id, ImportError::PrivateReexport))
                && !found
                    .iter()
                    .any(|(&item, &in_module)| !in_module || module.is_item_exported(item))
        })
        .map(|(&import_id, _)| import_id)
        .collect::<Vec<_>>();

    // An import through `super` (or `project`, below the root) waits for the private re-exports into
    // the module that it names, and is only reported if it still finds something private once they
    // are, so a chain of re-exports is reported once, at its start. That module encloses the
    // import's own, so some import doesn't wait.
    let waits = |import_id: ImportId| {
        let base = ctx.import(import_id).base_scope;

        base != cache.reexports[&import_id].0
            && private.iter().any(|other| cache.reexports[other].0 == base)
    };

    let reported = private
        .iter()
        .copied()
        .filter(|&import_id| !waits(import_id))
        .collect::<Vec<_>>();

    for &import_id in &reported {
        let Items::Named(name) = &ctx.import(import_id).items else {
            unreachable!("only named imports are recorded");
        };
        let name = name.clone();
        let (import_scope, found) = &cache.reexports[&import_id];

        for &item in found.keys() {
            let target = ctx.scope_mut(*import_scope);

            if target.item(item.namespace(), name.text()) == Some(item) {
                target.export_item(item);
            }
        }

        let kind = DiagnosticKind::PrivateReexport(name.text().to_string());
        cache
            .errors
            .insert((import_id, ImportError::PrivateReexport), (name, kind));
    }

    !reported.is_empty()
}

/// A name that a named import failed to bring into a scope, or that an import through `super`
/// passes on, which is blocked there (see [`Scope::block`]) once the imports are resolved, so that
/// its uses aren't reported again.
#[derive(Debug)]
struct FailedName {
    scope: ScopeId,
    namespace: Namespace,
    name: String,
    /// The import that failed, or for a name that an import through `super` passes on, an origin
    /// of the block it passes on.
    origin: ImportId,
}

impl FailedName {
    /// The name failed by an import in every namespace.
    fn all(scope: ScopeId, name: &str, origin: ImportId) -> impl Iterator<Item = Self> {
        Namespace::ALL.into_iter().map(move |namespace| Self {
            scope,
            namespace,
            name: name.to_string(),
            origin,
        })
    }

    /// A name blocked with some origins in another scope, which an import through `super` passes
    /// on to its own scope in a namespace.
    fn passed_on(
        scope: ScopeId,
        namespace: Namespace,
        name: &str,
        origins: &[ImportId],
    ) -> impl Iterator<Item = Self> {
        origins.iter().map(move |&origin| Self {
            scope,
            namespace,
            name: name.to_string(),
            origin,
        })
    }
}

/// Blocks a failed name, unless the scope has an item with it. Returns whether the block or its
/// origin is new.
fn block(ctx: &mut Compiler, failed: FailedName) -> bool {
    let scope = ctx.scope_mut(failed.scope);

    scope.item(failed.namespace, &failed.name).is_none()
        && scope.block(failed.namespace, failed.name, failed.origin)
}

/// The imports that fail, as far as the imports are resolved: those with a name that's missing, a
/// path that doesn't resolve, or only private items with the name.
fn failing_imports(
    missing_imports: &IndexMap<ImportId, IndexMap<String, Name>>,
    cache: &ImportCache,
) -> HashSet<ImportId> {
    missing_imports
        .iter()
        .filter(|(_, names)| !names.is_empty())
        .map(|(&import, _)| import)
        .chain(cache.errors.keys().filter_map(|&(import, error)| {
            matches!(
                error,
                ImportError::UnresolvedPath(_)
                    | ImportError::PrivateSymbol
                    | ImportError::PrivateType
            )
            .then_some(import)
        }))
        .collect()
}

/// Whether an import's failure is reported. An import that failed at a blocked name (see
/// `blocked_imports`) failed because the imports the block comes from did, and so on. Following
/// those reaches imports that didn't fail because of a block, or groups of imports that failed
/// because of each other's blocks, whose failures are reported, since nothing else explains them.
/// Any other import's failure has already been reported through those. So an import's failure is
/// reported if every import that it reaches this way reaches it back, which doesn't depend on the
/// order the imports are resolved in.
fn failure_is_reported(cache: &ImportCache, failing: &HashSet<ImportId>, import: ImportId) -> bool {
    let reached = reached_through_blocks(cache, failing, import);

    reached.iter().all(|&other| {
        other == import || reached_through_blocks(cache, failing, other).contains(&import)
    })
}

/// The imports reached by following the origins of the blocks that an import failed at, including
/// itself. Only the origins that are still `failing` are followed.
fn reached_through_blocks(
    cache: &ImportCache,
    failing: &HashSet<ImportId>,
    import: ImportId,
) -> HashSet<ImportId> {
    let mut reached = HashSet::from([import]);
    let mut stack = vec![import];

    while let Some(current) = stack.pop() {
        for &origin in cache.blocked_imports.get(&current).into_iter().flatten() {
            if failing.contains(&origin) && reached.insert(origin) {
                stack.push(origin);
            }
        }
    }

    reached
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

/// One of the lookups an import makes that can wait for a module to settle, and that (except for
/// a segment after the first) look past the module's own items when it doesn't have the name: to a
/// file's sibling modules, or the prelude.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum LookupKey {
    /// The first segment of the import's path.
    Path,
    /// The imported name, in a namespace.
    Item(Namespace),
    /// A file's sibling module that a glob import of the file (through `super`) brings in.
    Sibling(Namespace, String),
    /// A segment of the import's path after the first, which is looked up in the module the
    /// previous one names. It doesn't look past the module, but it waits for it to settle.
    Segment(usize),
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
    /// The module that the lookup waited for, whose own items it looked past.
    scope: ScopeId,
    namespace: Namespace,
    /// The name, where it's written in the import.
    name: Name,
    answer: Option<(Item, Option<ImportId>)>,
}

/// Reports each lookup that was answered without waiting, if the module it looked past ended up
/// with a different item with the name (other than one the import itself brought in), or with an
/// ambiguous one. The name would refer to that item everywhere else in the module, so the import
/// is ambiguous.
fn check_forced_lookups(ctx: &mut Compiler, cache: &ImportCache) {
    let mut reported = HashSet::new();

    for (&(import, _), forced) in &cache.forced {
        let Some((answer, _)) = forced.answer else {
            continue;
        };

        // A different item contradicts the answer, unless the import itself brings it in under the
        // name (whichever glob import recorded it). An ambiguous one does even if it's the same.
        let ambiguous = ctx
            .scope(forced.scope)
            .item_with_import(forced.namespace, forced.name.text())
            .is_some_and(|(item, via)| {
                // A binding that a named import (other than this one) made takes precedence over
                // what a glob import brings in, so it isn't this import's, whatever it brings in
                let own = via.is_some_and(|via| {
                    via == import || matches!(ctx.import(via).items, Items::All(_))
                }) && brings_in(ctx, cache, import, forced, item);

                ctx.is_ambiguous_import(via, item.into()) || (item != answer && !own)
            });

        if ambiguous && reported.insert((import, forced.name.text().to_string())) {
            ctx.diagnostic_name(
                &forced.name,
                DiagnosticKind::AmbiguousOuterImport(forced.name.text().to_string()),
            );
        }
    }
}

/// Whether an import brings in an item with the name that a forced lookup of it looked up, into
/// the module whose items it looked past. An import with a path brings in what the module it names
/// exports, and one without a path brings in what it finds itself.
fn brings_in(
    ctx: &Compiler,
    cache: &ImportCache,
    import_id: ImportId,
    forced: &ForcedLookup,
    item: Item,
) -> bool {
    let import = ctx.import(import_id);
    let name = forced.name.text();

    // An import through `super` brings items into its own module, not the one it looked past
    if !ctx
        .scope(forced.scope)
        .imports()
        .any(|other| other == import_id)
    {
        return false;
    }

    if let Items::Named(named) = &import.items
        && named.text() != name
    {
        return false;
    }

    if let Some(path) = cache.paths.get(&import_id) {
        let source = ctx.scope(path.scope);

        source
            .item(forced.namespace, name)
            .is_some_and(|found| found == item && source.is_item_exported(found))
    } else {
        import.path.is_empty() && forced.answer.is_some_and(|(answer, _)| answer == item)
    }
}

/// Records that an import failed at a name that's blocked where it looked it up, in any namespace
/// (see [`Scope::block`]), along with the origins of the block.
fn record_blocked_import(
    ctx: &Compiler,
    blocked_imports: &mut HashMap<ImportId, HashSet<ImportId>>,
    import: ImportId,
    base: Base,
    name: &str,
) {
    for namespace in Namespace::ALL {
        let origins = base.block_origins(ctx, namespace, name);

        if !origins.is_empty() {
            blocked_imports.entry(import).or_default().extend(origins);
        }
    }
}

/// Where an import's name, or the first segment of its path, is looked up.
fn import_base(import: &Import, import_scope: ScopeId) -> Base {
    if import.from_parent {
        Base::Member(import.base_scope)
    } else {
        Base::Local(import_scope)
    }
}

/// Records a reference to a declaration from a name in an import, for the language server.
fn add_reference(ctx: &mut Compiler, source: &Source, name: &Name, declaration: Declaration) {
    if let Some(srcloc) = name.srcloc() {
        ctx.add_syntax_for_source(
            SyntaxItemKind::reference(declaration),
            TextRange::new(
                srcloc.span.start.try_into().unwrap(),
                srcloc.span.end.try_into().unwrap(),
            ),
            source.kind.clone(),
        );
    }
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
    /// The imports whose paths don't resolve, which block the names they import.
    failed_paths: HashSet<ImportId>,
    /// The lookups that are waiting for a module to settle in this pass.
    waiting: Vec<WaitingLookup>,
    /// The names that named imports with a path failed to bring in in this pass, because they
    /// found only private items or their path didn't resolve, and the names that imports through
    /// `super` pass on from the module. Names that are still missing once the imports are
    /// resolved are blocked too.
    failed_names: Vec<FailedName>,
}

impl Resolution {
    fn named_imports_of(&self, scope: ScopeId, name: &str) -> Option<&Vec<ImportId>> {
        self.named_imports.get(&scope)?.get(name)
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

/// Whether a module blocks a name in a namespace (see [`Scope::block`]), or will once its imports
/// are resolved, because a named import of the name into it only found a private item or its path
/// doesn't resolve. Lookups past the module's own items don't find the name, like they don't once
/// it's blocked.
fn blocks(
    ctx: &Compiler,
    resolution: &Resolution,
    scope: ScopeId,
    namespace: Namespace,
    name: &str,
) -> bool {
    ctx.scope(scope).is_blocked(namespace, name)
        || resolution
            .named_imports_of(scope, name)
            .into_iter()
            .flatten()
            .any(|import| {
                // An import whose path doesn't resolve blocks its name in every namespace
                if resolution.failed_paths.contains(import) {
                    return true;
                }

                // An import through `super` can import private items, so it never fails this way
                let Some(&(source, false)) = resolution.bases.get(import) else {
                    return false;
                };

                // A public item in another namespace is imported instead
                matches!(
                    named_items(ctx.scope(source), name),
                    NamedItems::Private(items) if items.iter().any(|item| item.namespace() == namespace)
                )
            })
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
/// of the imports that only wait for each other (or for nothing that's still waiting), which are
/// the strongly connected components of the imports that wait for each other that don't wait for
/// any other one. Imports that wait for those are answered later, once they're resolved, so an
/// inline module's lookup through `super` waits for the cycle of imports in the module it names.
///
/// A glob import's lookup of a name isn't answered while a named import of the name into the
/// module it looks past is in the same component. A named import's binding takes precedence over
/// what's outside of the module, so the lookup waits for it to be resolved instead of finding
/// what's outside and being contradicted by it. The named import's own lookups are answered, so
/// the component still makes progress.
fn stuck_lookups(
    ctx: &Compiler,
    resolution: &Resolution,
    waiting: Vec<WaitingLookup>,
) -> Vec<WaitingLookup> {
    let mut edges: IndexMap<ImportId, IndexSet<ImportId>> = IndexMap::new();

    for lookup in &waiting {
        edges
            .entry(lookup.key.0)
            .or_default()
            .extend(lookup.blockers.iter().copied());
    }

    let components = rue_types::strongly_connected_components(&edges);

    let component_of = components
        .iter()
        .enumerate()
        .flat_map(|(index, component)| component.iter().map(move |&import| (import, index)))
        .collect::<HashMap<_, _>>();

    let stuck = components
        .iter()
        .enumerate()
        .filter(|&(index, component)| {
            // A blocker that isn't waiting itself is resolved, so it doesn't count
            component.iter().all(|import| {
                edges[import].iter().all(|blocker| {
                    component_of
                        .get(blocker)
                        .is_none_or(|&other| other == index)
                })
            })
        })
        .map(|(index, _)| index)
        .collect::<HashSet<_>>();

    waiting
        .into_iter()
        .filter(|lookup| {
            let import = lookup.key.0;
            let component = component_of[&import];

            if !stuck.contains(&component) {
                return false;
            }

            let shadowed = matches!(ctx.import(import).items, Items::All(_))
                && resolution
                    .named_imports_of(lookup.forced.scope, lookup.forced.name.text())
                    .into_iter()
                    .flatten()
                    .any(|named| component_of.get(named) == Some(&component));

            !shadowed
        })
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
    /// In the module that the previous segment of a path names, and nowhere else.
    Inner(ScopeId),
}

impl Base {
    /// The scope whose own items the name is looked up in first.
    fn scope(self) -> ScopeId {
        match self {
            Self::Local(scope) | Self::Member(scope) | Self::Inner(scope) => scope,
        }
    }

    /// Looks a name up past the scope's own items: in the scopes that enclose it, or a file's
    /// sibling modules, and nowhere for a segment after the first.
    fn outside(
        self,
        ctx: &Compiler,
        namespace: Namespace,
        name: &str,
    ) -> Option<(Item, Option<ImportId>)> {
        match self {
            Self::Local(scope) => ctx
                .scope(scope)
                .parent()
                .and_then(|parent| ctx.resolve_item_in(parent, namespace, name)),
            Self::Member(scope) => ctx.sibling_item(scope, namespace, name),
            Self::Inner(_) => None,
        }
    }

    /// Finds a name in the scope's own items, and then outside of them unless the scope blocks
    /// it (see [`Scope::block`]), without waiting for anything.
    fn find(
        self,
        ctx: &Compiler,
        namespace: Namespace,
        name: &str,
    ) -> Option<(Item, Option<ImportId>)> {
        let scope = ctx.scope(self.scope());

        scope.item_with_import(namespace, name).or_else(|| {
            if scope.is_blocked(namespace, name) {
                None
            } else {
                self.outside(ctx, namespace, name)
            }
        })
    }

    /// The origins of the block that looking a name up stops at (see [`Scope::block`]), which are
    /// empty unless it stops at one.
    fn block_origins<'a>(
        self,
        ctx: &'a Compiler,
        namespace: Namespace,
        name: &str,
    ) -> &'a [ImportId] {
        match self {
            Self::Local(scope) => ctx.block_origins_in(scope, namespace, name),
            Self::Member(scope) | Self::Inner(scope) => {
                ctx.scope(scope).block_origins(namespace, name)
            }
        }
    }
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
/// answered with what's outside (see [`check_forced_lookups`]), which for a segment of a path
/// after the first is nothing, since it's only looked up in the module the previous one names.
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
    let scope = base.scope();

    // A forced answer is used even once the module has an item with the name, which might have
    // been brought in by another forced lookup, so that they're all answered from the same state
    let forced = forced.get(&(import_id, key.clone()));

    if let Some(ForcedLookup {
        answer: Some((item, via)),
        ..
    }) = forced
    {
        return Lookup::Found(*item, *via);
    }

    if let Some((item, via)) = ctx.scope(scope).item_with_import(namespace, name) {
        return Lookup::Found(item, via);
    }

    // A segment after the first isn't looked up outside of the module, so it isn't blocked there
    let outside = match base {
        Base::Inner(_) => None,
        _ if blocks(ctx, resolution, scope, namespace, name) => None,
        _ => base.outside(ctx, namespace, name),
    };

    if forced.is_some() {
        return Lookup::from_answer(outside);
    }

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
    let import = ctx.import(import_id).clone();

    if !resolve_path(ctx, import_scope, import_id, &import, cache, resolution) {
        return false;
    }

    let base = cache.paths.get(&import_id).map(|path| path.scope);

    // The module the import's items are imported from, which other lookups can wait for. A name
    // that's looked up from the import's own scope doesn't come from a module.
    if let Some(base) = base {
        resolution.bases.insert(import_id, (base, false));
    } else if import.from_parent {
        resolution
            .bases
            .insert(import_id, (import.base_scope, true));
    }

    // Errors are only recorded once every declaration is known
    if resolution.diagnostics {
        check_path(ctx, import_id, &import, cache);
    }

    match &import.items {
        Items::All(star) => resolve_glob_import(
            ctx,
            import_scope,
            import_id,
            &import,
            star,
            base,
            cache,
            resolution,
        ),
        Items::Named(item) => resolve_named_import(
            ctx,
            import_scope,
            import_id,
            &import,
            item,
            base,
            cache,
            resolution,
            missing_imports,
        ),
    }
}

/// Resolves an import's path, if it has one, and caches what it resolves to. Returns whether it's
/// resolved, or has no path.
fn resolve_path(
    ctx: &mut Compiler,
    import_scope: ScopeId,
    import_id: ImportId,
    import: &Import,
    cache: &mut ImportCache,
    resolution: &mut Resolution,
) -> bool {
    let diagnostics = resolution.diagnostics;

    // The path doesn't change once it's resolved, and looking it up again could find what the
    // import itself brought in, so it's only resolved once
    if let Some(cached) = cache.paths.get(&import_id) {
        for &(_, via, declaration) in &cached.references {
            ctx.add_import_reference(via, declaration, ImportUser::Import(import_id));
        }

        if diagnostics {
            for (name, &symbol) in import.path.iter().zip(&cached.symbols) {
                add_reference(ctx, &import.source, name, Declaration::Symbol(symbol));
            }
        }

        return true;
    }

    if import.path.is_empty() {
        return true;
    }

    let mut base = None;
    let mut symbols = Vec::new();
    let mut references = Vec::new();

    for (index, name) in import.path.iter().enumerate() {
        // A private module is reported, but still resolved, like it is in other paths. A segment
        // after the first waits for the module the previous one names, so what waits for this
        // import waits for that module too.
        let (key, segment_base) = match base {
            Some(scope) => (LookupKey::Segment(index), Base::Inner(scope)),
            None => (LookupKey::Path, import_base(import, import_scope)),
        };

        let module = match lookup(
            ctx,
            &cache.forced,
            resolution,
            import_id,
            key,
            name,
            segment_base,
            Namespace::Module,
        ) {
            Lookup::Found(module, via) => module.symbol().map(|module| (module, via)),
            Lookup::Undetermined => return false,
            Lookup::NotFound => None,
        };

        let Some((symbol, via)) = module else {
            resolution.failed_paths.insert(import_id);

            // The import failed, so the name it brings in is blocked, and its uses aren't
            // reported again
            if let Items::Named(item) = &import.items {
                resolution.failed_names.extend(FailedName::all(
                    import_scope,
                    item.text(),
                    import_id,
                ));
            }

            // A name that a failed import blocked where it's looked up has already been reported
            // there, unless that import only failed because of this one. The name may only be
            // blocked in a later pass, once the imports are resolved, so the error is recorded
            // and only reported then.
            record_blocked_import(
                ctx,
                &mut cache.blocked_imports,
                import_id,
                segment_base,
                name.text(),
            );

            if diagnostics {
                // Only a module can have a path after it, so a value or type is reported as such
                let other = [Namespace::Value, Namespace::Type]
                    .into_iter()
                    .any(|namespace| segment_base.find(ctx, namespace, name.text()).is_some());

                let kind = if other {
                    DiagnosticKind::SubpathNotSupported(name.text().to_string())
                } else {
                    DiagnosticKind::UndeclaredSymbol(name.text().to_string())
                };

                // The imports are resolved again once a reported re-export exports what it found
                // (see `report_private_reexports`), which can bring in what the path names, so
                // only how it fails in the last pass is reported
                forget_unresolved_path(cache, import_id, index);

                cache.errors.insert(
                    (import_id, ImportError::UnresolvedPath(index)),
                    (name.clone(), kind),
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
            references.push((index, via, Declaration::Symbol(symbol)));
        }

        if diagnostics {
            add_reference(ctx, &import.source, name, Declaration::Symbol(symbol));
        }

        base = Some(ctx.module(symbol).scope);
        symbols.push(symbol);
    }

    // It failed before a reported re-export brought in a module that it names (see
    // `report_private_reexports`)
    forget_unresolved_path(cache, import_id, import.path.len());

    cache.paths.insert(
        import_id,
        CachedPath {
            scope: base.expect("a resolved path ends in a module"),
            symbols,
            references,
        },
    );

    true
}

/// Forgets the errors recorded for the segments of an import's path before an index, which it
/// failed at in an earlier pass.
fn forget_unresolved_path(cache: &mut ImportCache, import_id: ImportId, before: usize) {
    for index in 0..before {
        cache
            .errors
            .shift_remove(&(import_id, ImportError::UnresolvedPath(index)));
    }
}

/// Records the errors in an import's resolved path, and the names in it that refer to a
/// declaration through another import.
fn check_path(ctx: &Compiler, import_id: ImportId, import: &Import, cache: &mut ImportCache) {
    let Some(path) = cache.paths.get(&import_id) else {
        return;
    };

    for &(index, via, declaration) in &path.references {
        cache
            .imported_names
            .entry((import_id, declaration))
            .or_insert_with(|| (import.path[index].clone(), via));
    }

    // A segment after the first names a module in the one before it, which might be private. This
    // is checked each time, since a reported re-export of a private module exports it (see
    // `report_private_reexports`).
    let symbols = &path.symbols;

    for index in 1..symbols.len() {
        let parent = ctx.module(symbols[index - 1]).scope;

        if ctx.scope(parent).is_module_exported(symbols[index]) {
            cache
                .errors
                .shift_remove(&(import_id, ImportError::PrivateModule(index)));
            continue;
        }

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

/// Brings in what a glob import imports from `base`, the module its path names, or if it has no
/// path, its base scope. Returns whether anything changed.
#[allow(clippy::too_many_arguments)]
fn resolve_glob_import(
    ctx: &mut Compiler,
    import_scope: ScopeId,
    import_id: ImportId,
    import: &Import,
    star: &Name,
    base: Option<ScopeId>,
    cache: &mut ImportCache,
    resolution: &mut Resolution,
) -> bool {
    let mut updated = false;

    let count = &mut cache
        .glob_import_counts
        .entry(import_id)
        .or_insert_with(|| (star.clone(), 0))
        .1;

    // Glob imports bring in every exported item of a module. A glob import of a parent module
    // (through `super` or `project`) brings in its private items too, but doesn't export them.
    let source_scope = base.unwrap_or(import.base_scope);

    for namespace in Namespace::ALL {
        let items = if base.is_some() {
            ctx.scope(source_scope).exported_items(namespace)
        } else {
            // A file's sibling modules are brought in too, unless the file has an item with the
            // name, or could still bring one in through its imports
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

            // A name that a failed import blocked in the module has already been reported, so
            // it's blocked here too, unless something else brings it in
            for (name, origins) in ctx.scope(source_scope).blocked(namespace) {
                resolution.failed_names.extend(FailedName::passed_on(
                    import_scope,
                    namespace,
                    name,
                    origins,
                ));
            }

            items
        };

        for (name, item) in items {
            // A named import of the name shadows the glob import in the namespaces that it
            // brings an item into, or could still
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

            let module = ctx.scope(source_scope);

            // The import the item was brought into the module by, if any
            let via = module.item_import(item);

            // Only an item that's public where it's imported from is exported. A file's sibling
            // modules aren't items of the file, and they're public.
            let exported = import.exported
                && (module.item(namespace, &name) != Some(item) || module.is_item_exported(item));

            let existing = ctx.scope(import_scope).item(namespace, &name);

            // The glob import that the binding of the name was recorded on, if any
            let recorded = existing
                .and_then(|existing| ctx.scope(import_scope).item_import(existing))
                .filter(|&recorded| matches!(ctx.import(recorded).items, Items::All(_)));

            // A name that's ambiguous where it's imported from is ambiguous here too. The imports
            // are resolved again until nothing changes, so this reaches the end of any chain of
            // glob imports. When several glob imports bring in the same item, it's ambiguous if
            // any of them does, whichever one recorded it, so the ambiguity is recorded on that
            // one. What else the name could refer to is recorded the same way, even if a named
            // import passed it on, which isn't reported again.
            if existing == Some(item)
                && let Some(recorded) = recorded
            {
                if ctx.is_ambiguous_import(via, item.into()) {
                    updated |= ctx.add_ambiguous_import(recorded, item.into());
                }

                updated |= ctx.add_ambiguous_alternatives(recorded, item.into(), via, item.into());
            }

            // The glob import that bound the name, if another one did
            if let Some(existing) = existing
                && let Some(bound_by) = recorded.filter(|&bound_by| bound_by != import_id)
            {
                // This import brings in the name too, whether it's the same item or a different
                // one, so it's used wherever the binding is. Otherwise, which of them counts as
                // used would depend on which one bound the name, and so on the order they're
                // resolved in.
                if ctx.add_shared_glob_binding(bound_by, existing.into(), import_id, item.into()) {
                    ctx.import_mut(import_id)
                        .declarations
                        .push((name, item.into()));
                    *count += 1;
                }

                // The binding (or its ambiguity) is exported if any of the glob imports exports
                // the name, whichever one bound it, so that it's reported where the name is used
                if exported && !ctx.scope(import_scope).is_item_exported(existing) {
                    ctx.scope_mut(import_scope).export_item(existing);
                    updated = true;
                }

                // The name could refer to the item this import brings in, or to what that could
                // refer to if it's ambiguous where it's imported from
                if existing != item {
                    updated |= ctx.add_ambiguous_import(bound_by, existing.into());

                    updated |=
                        ctx.add_ambiguous_alternatives(bound_by, existing.into(), via, item.into());
                }
            } else if bind(ctx, import_scope, import_id, &name, item, exported) {
                updated = true;
                *count += 1;
            }
        }
    }

    updated
}

/// Brings in the items with the name that a named import imports, from `base`, the module its path
/// names, or if it has no path, from where it looks the name up. Returns whether anything changed.
#[allow(clippy::too_many_arguments)]
fn resolve_named_import(
    ctx: &mut Compiler,
    import_scope: ScopeId,
    import_id: ImportId,
    import: &Import,
    item: &Name,
    base: Option<ScopeId>,
    cache: &mut ImportCache,
    resolution: &mut Resolution,
    missing_imports: &mut IndexMap<ImportId, IndexMap<String, Name>>,
) -> bool {
    let diagnostics = resolution.diagnostics;
    let lookup_base = import_base(import, import_scope);
    let mut updated = false;

    let missing_imports = missing_imports
        .entry(import_id)
        .or_insert_with(|| single_name(item));

    let unused_imports = cache
        .unused_imports
        .entry(import_id)
        .or_insert_with(|| single_name(item));

    let name = item.text();

    // Each item is resolved along with the import it was brought into its scope by, if any, in
    // every namespace that has the name
    let found = if let Some(base) = base {
        match named_items(ctx.scope(base), name) {
            NamedItems::Public(found) => {
                // It may have found only private items in an earlier pass, before a reported
                // re-export exported one (see `report_private_reexports`)
                for error in [ImportError::PrivateSymbol, ImportError::PrivateType] {
                    cache.errors.shift_remove(&(import_id, error));
                }

                found
            }
            NamedItems::Private(items) => {
                // The name is reported as private rather than unresolved
                if !items.is_empty() {
                    missing_imports.shift_remove(name);
                    for &found in &items {
                        resolution.failed_names.push(FailedName {
                            scope: import_scope,
                            namespace: found.namespace(),
                            name: name.to_string(),
                            origin: import_id,
                        });
                    }
                }

                if diagnostics {
                    let module = import.path.last().unwrap().text();

                    for &found in &items {
                        let (error, kind) = ImportError::private(found.namespace(), name, module);

                        cache
                            .errors
                            .entry((import_id, error))
                            .or_insert_with(|| (item.clone(), kind));
                    }
                }

                Vec::new()
            }
        }
    } else {
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
                    item,
                    lookup_base,
                    namespace,
                )
                .found(&mut undetermined)
            })
            .collect::<Vec<_>>();

        if !undetermined && !found.is_empty() {
            resolution.bound.insert(import_id);
        }

        // A name that a failed import blocked in the module that `super` names is blocked here
        // too, in the namespaces that the import doesn't find it in
        if let Base::Member(scope) = lookup_base {
            for namespace in Namespace::ALL {
                if found.iter().any(|(item, _)| item.namespace() == namespace) {
                    continue;
                }

                let origins = ctx.scope(scope).block_origins(namespace, name);
                resolution.failed_names.extend(FailedName::passed_on(
                    import_scope,
                    namespace,
                    name,
                    origins,
                ));
            }
        }

        found
    };

    // A name that a failed import blocked where an import without a path looks it up has already
    // been reported there, unless that import only failed because of this one. The name is still
    // missing, so it's blocked here too.
    if base.is_none() && found.is_empty() && missing_imports.contains_key(name) {
        record_blocked_import(
            ctx,
            &mut cache.blocked_imports,
            import_id,
            lookup_base,
            name,
        );
    }

    // This is recorded in every pass, since an import of a name with no path finds its own binding
    // once it has been resolved. The import that it's found through is used by this one, like one
    // in its path.
    for &(found, via) in &found {
        if let Some(via) = via
            && via != import_id
        {
            let declaration = found.into();
            ctx.add_import_reference(via, declaration, ImportUser::Import(import_id));

            // What else the name could refer to is passed on, so that it's used and exported
            // wherever this import's binding is
            updated |=
                ctx.add_ambiguous_alternatives(import_id, declaration, Some(via), declaration);
            cache
                .imported_names
                .entry((import_id, declaration))
                .or_insert_with(|| (item.clone(), via));
        }
    }

    for &(found, _) in &found {
        let namespace = found.namespace();

        // An import with no path can find a private item of its own module, or of the one that
        // `super` or `project` names, which it only exports if it's public there
        let mut exported = import.exported;

        if import.exported && base.is_none() {
            let module = ctx.scope(import.base_scope);
            let in_module = module.item(namespace, name) == Some(found);

            exported = !in_module || module.is_item_exported(found);

            // A name that's already bound to a different item is reported as a duplicate instead
            if ctx
                .scope(import_scope)
                .item(namespace, name)
                .is_none_or(|existing| existing == found)
            {
                // It's reported if nothing it found is public, rather than as unused
                if !exported {
                    unused_imports.shift_remove(name);
                }

                cache
                    .reexports
                    .entry(import_id)
                    .or_insert_with(|| (import_scope, IndexMap::new()))
                    .1
                    .entry(found)
                    .or_insert(in_module);
            }
        }

        if diagnostics {
            add_reference(ctx, &import.source, item, found.into());
        }

        let existing = ctx.scope(import_scope).item(namespace, name);

        if bind(ctx, import_scope, import_id, name, found, exported) {
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

    updated
}

/// The names that a named import imports, keyed by their text, for tracking which are missing or
/// unused.
fn single_name(name: &Name) -> IndexMap<String, Name> {
    IndexMap::from([(name.text().to_string(), name.clone())])
}

/// The items with a name that a named import with a path finds in the module the path names, in
/// every namespace.
enum NamedItems {
    /// At least one of them is public there, and these are the public ones, along with the import
    /// each was brought into the module by, if any. Only they're imported, and nothing is
    /// reported.
    Public(Vec<(Item, Option<ImportId>)>),
    /// None of them is public there, and these are the private ones, if any. They're reported,
    /// but never imported: an import can't be undone, and a public item with the name may only be
    /// brought into the module by an import that's resolved in a later pass, so a private item
    /// imported now would stay imported. The import fails in their namespaces.
    Private(Vec<Item>),
}

fn named_items(module: &Scope, name: &str) -> NamedItems {
    let items = Namespace::ALL
        .into_iter()
        .filter_map(|namespace| module.item_with_import(namespace, name))
        .collect::<Vec<_>>();

    if items.iter().any(|&(item, _)| module.is_item_exported(item)) {
        NamedItems::Public(
            items
                .into_iter()
                .filter(|&(item, _)| module.is_item_exported(item))
                .collect(),
        )
    } else {
        NamedItems::Private(items.into_iter().map(|(item, _)| item).collect())
    }
}

/// Binds a name in a scope to an item that an import brings in, or if it's already bound to the
/// item, exports the binding when the import exports it and it isn't yet. Either way, the
/// declaration is recorded on the import. Returns whether it did either.
fn bind(
    ctx: &mut Compiler,
    scope: ScopeId,
    import_id: ImportId,
    name: &str,
    item: Item,
    exported: bool,
) -> bool {
    let target = ctx.scope_mut(scope);

    match target.item(item.namespace(), name) {
        None => {
            target.insert_item(name.to_string(), item, exported);
            target.add_item_import(item, import_id);
        }
        Some(existing) if existing == item && exported && !target.is_item_exported(item) => {
            target.export_item(item);
        }
        Some(_) => return false,
    }

    ctx.import_mut(import_id)
        .declarations
        .push((name.to_string(), item.into()));
    true
}
