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
use rue_types::Type;

use crate::{
    Compiler, Found, Place, SyntaxItemKind, alternative_items, alternative_modules, modules_of,
    scope_alternatives, unhidden_alternatives,
};

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
    /// The items that each named import of a name that another named import into the same scope
    /// imports too finds, with the import that each is found through, if any (see
    /// [`resolve_duplicate_imports`]).
    named_finds: HashMap<ImportId, IndexSet<Found>>,
    glob_import_counts: IndexMap<ImportId, (ScopeId, Name, usize)>,
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

    /// The imports that failed once nothing else could be resolved, and how (see
    /// [`bind_failed_imports`]). A failure is final, even once the import finds the error items
    /// of the imports it failed because of.
    failed: IndexMap<ImportId, Failure>,
    /// The imports that each error item stands for in each scope it's bound in: the failed imports
    /// of the name into the scope, what it stands for where it was imported from, and what the
    /// error items that met it in the name there stand for (see [`add_error_origins`]). Only
    /// failed imports are reported (see [`reported_failures`]). It's kept for each scope since an
    /// error item can be imported into many, and what explains it in one doesn't in another.
    error_origins: HashMap<(ScopeId, Item), IndexSet<ImportId>>,
    /// The imports whose error items each import found where it failed, or resolved its path or
    /// name through, which explain its failure (see [`reported_failures`]).
    error_links: HashMap<ImportId, IndexSet<ImportId>>,

    /// Whether the pass limit has been reported (see [`report_pass_limit`]).
    pass_limit_reported: bool,
}

impl ImportCache {
    /// Whether an exported named import with no path is a private re-export that hasn't been
    /// reported yet: it only found items that are private where it found them.
    fn is_private_reexport(&self, ctx: &Compiler, import_id: ImportId) -> bool {
        let Some((_, found)) = self.reexports.get(&import_id) else {
            return false;
        };
        let module = ctx.scope(ctx.import(import_id).base_scope);

        !self
            .errors
            .contains_key(&(import_id, ImportError::PrivateReexport))
            && !found
                .iter()
                .any(|(&item, &in_module)| !in_module || module.is_item_exported(item))
    }

    /// The private re-exports that would export an item that a module has with a name but doesn't
    /// export, once they're reported, since a reported re-export exports what it found (see
    /// [`report_private_reexports`]). They're those into the module the item was found in, and, if
    /// a glob import brought it in, those that would export it in a module that an exported glob
    /// import of `super` or `project` brings it in from, which then exports it too, as any glob
    /// import that brings in a name exports it if it's exported.
    fn exporters(
        &self,
        ctx: &Compiler,
        scope: ScopeId,
        name: &str,
        item: Item,
    ) -> IndexSet<ImportId> {
        let mut exporters = IndexSet::new();
        let mut visited = HashSet::new();
        let mut stack = vec![scope];

        while let Some(scope) = stack.pop() {
            let module = ctx.scope(scope);

            if !visited.insert(scope) || module.is_item_exported(item) {
                continue;
            }

            exporters.extend(
                self.reexports
                    .iter()
                    .filter(|&(&import_id, (into, found))| {
                        *into == scope
                            && found.contains_key(&item)
                            && self.is_private_reexport(ctx, import_id)
                    })
                    .map(|(&import_id, _)| import_id),
            );

            let by_glob = module
                .item_import(item)
                .is_some_and(|via| matches!(ctx.import(via).items, Items::All(_)));

            if !by_glob {
                continue;
            }

            for glob in module.imports() {
                let glob = ctx.import(glob);

                if matches!(glob.items, Items::All(_))
                    && glob.exported
                    && glob.from_parent
                    && glob.path.is_empty()
                    && ctx.scope(glob.base_scope).item(item.namespace(), name) == Some(item)
                {
                    stack.push(glob.base_scope);
                }
            }
        }

        exporters
    }
}

/// How an import failed: the errors it's reported with, and the namespaces that a named import
/// binds its name to error items in, in the scope it's in.
#[derive(Debug, Clone)]
struct Failure {
    scope: ScopeId,
    namespaces: Vec<Namespace>,
    errors: Vec<(Name, DiagnosticKind)>,
}

impl Failure {
    /// A named import whose name isn't found, in any namespace.
    fn missing(scope: ScopeId, name: &Name) -> Self {
        Self {
            scope,
            namespaces: Namespace::ALL.to_vec(),
            errors: vec![(
                name.clone(),
                DiagnosticKind::UnresolvedImport(name.text().to_string()),
            )],
        }
    }

    /// A named import with a path that only finds private items with its name in the module the
    /// path names, if any, and fails in their namespaces. Values and modules are both reported as
    /// symbols, so a name that's both is reported once.
    fn private(scope: ScopeId, name: &Name, module: &str, items: &[Item]) -> Self {
        if items.is_empty() {
            return Self::missing(scope, name);
        }

        let mut errors = Vec::new();
        let (mut symbol, mut ty) = (false, false);

        for item in items {
            let (text, module) = (name.text().to_string(), module.to_string());

            let kind = match item.namespace() {
                Namespace::Value | Namespace::Module if !mem::replace(&mut symbol, true) => {
                    DiagnosticKind::PrivateSymbol(text, module)
                }
                Namespace::Type if !mem::replace(&mut ty, true) => {
                    DiagnosticKind::PrivateType(text, module)
                }
                _ => continue,
            };

            errors.push((name.clone(), kind));
        }

        Self {
            scope,
            namespaces: items.iter().map(|item| item.namespace()).collect(),
            errors,
        }
    }
}

/// An error in an import, which is reported once all imports are resolved.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ImportError {
    /// The segment of the path at this index names a private module.
    PrivateModule(usize),
    /// An exported import re-exports a private item.
    PrivateReexport,
    /// The name is already declared in the scope as a different symbol.
    DuplicateSymbol,
    /// The name is already declared in the scope as a different type.
    DuplicateType,
}

impl ImportError {
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
        failures: IndexMap::new(),
        settled: false,
    };
    let mut passes = 0;

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
        let bases = resolution.bases.len();
        let failed_paths = resolution.failed_paths.len();

        let Some(mut updated) = resolve_pass(ctx, &imports, cache, &mut resolution, &mut passes)
        else {
            return;
        };

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
            // so the imports are resolved again. Nothing has failed for good yet, so an import
            // that only failed because of the private item doesn't.
            if report_private_reexports(ctx, cache) {
                continue;
            }

            // The imports that still fail have failed for good, and their names are bound to
            // error items, which other imports can find (such as through `super`), so the imports
            // are resolved again until nothing new is bound
            if bind_failed_imports(ctx, cache, &mut resolution) {
                continue;
            }

            break;
        }

        let waiting = mem::take(&mut resolution.waiting);

        for lookup in stuck_lookups(ctx, &resolution, waiting) {
            cache.forced.entry(lookup.key).or_insert(lookup.forced);
        }
    }

    // What names and glob imports could refer to besides what they're bound to only matters
    // once something is ambiguous, and a glob import only passes on what it could bring in under
    // names that nothing hides where it's passed through. Names are still being bound in the
    // passes above, so what paths and glob imports could refer to is only worked out once the
    // bindings are settled. Only the alternatives change in these passes, and they only grow, so
    // they settle.
    if ctx.has_alternatives() {
        resolution.settled = true;

        loop {
            let Some(updated) = resolve_pass(ctx, &imports, cache, &mut resolution, &mut passes)
            else {
                return;
            };

            if !updated {
                break;
            }
        }
    }

    if diagnostics {
        let reported = reported_failures(cache);

        for (import, failure) in &cache.failed {
            if reported.contains(import) {
                for (name, kind) in &failure.errors {
                    ctx.diagnostic_name(name, kind.clone());
                }
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

        resolve_duplicate_imports(ctx, cache, &resolution);

        for unused_imports in cache.unused_imports.values() {
            for name in unused_imports.values() {
                ctx.lint_sites_mut().empty_imports.push(name.clone());
            }
        }

        // Only a name that's ambiguous somewhere could make a glob import bring in more
        for (&import, &(scope, ref name, count)) in &cache.glob_import_counts {
            // One that could bring in something if its path were bound differently is reported
            // as unused instead, unless any of that is used
            if ctx.has_alternatives() && unhidden_alternatives(ctx, scope, import).next().is_some()
            {
                ctx.add_glob_that_may_bring_in_more(import);
            } else if count == 0 {
                ctx.lint_sites_mut().empty_imports.push(name.clone());
            }
        }
    }
}

/// Resolves each import once, as one pass. Returns whether anything changed, or nothing once the
/// pass limit is reached, which is reported.
fn resolve_pass(
    ctx: &mut Compiler,
    imports: &[(ScopeId, ImportId)],
    cache: &mut ImportCache,
    resolution: &mut Resolution,
    passes: &mut usize,
) -> Option<bool> {
    let limit = pass_limit(imports.len());

    if *passes == limit {
        report_pass_limit(ctx, imports, cache, limit);
        return None;
    }

    *passes += 1;
    resolution.waiting.clear();
    resolution.failures.clear();

    let mut updated = false;

    for &(import_scope, import) in imports {
        updated |= resolve_import(ctx, import_scope, import, cache, resolution);
    }

    Some(updated)
}

#[cfg(test)]
thread_local! {
    /// A limit that replaces [`pass_limit`]'s in tests on this thread, so that they can reach it.
    pub(crate) static PASS_LIMIT: std::cell::Cell<Option<usize>> = const {
        std::cell::Cell::new(None)
    };
}

/// The most passes that [`resolve_imports`] makes before it gives up. A pass is made again when the
/// last one changed something, or when one of the steps once nothing changes (reporting private
/// re-exports, binding failed imports' names to error items and forcing stuck lookups) did. A
/// change moves on through at least one import per pass, so one that a chain of imports passes
/// along (such as a glob import of a module that glob-imports the next one) takes about a pass per
/// import, and the settle steps start new changes only a few times. Nothing proves that they can't
/// start more, so no tight bound is practical. Instead, this leaves a wide margin above what's been
/// measured: in the tests, in generated projects with long chains and cycles of glob, named and
/// private re-exports and nested glob imports through `super`, and in random generated projects,
/// it never took more than two passes per import plus two.
fn pass_limit(imports: usize) -> usize {
    #[cfg(test)]
    if let Some(limit) = PASS_LIMIT.get() {
        return limit;
    }

    64 + 16 * imports
}

/// Reports that the imports didn't settle within the pass limit (see [`pass_limit`]), at the first
/// import, and once for both calls to [`resolve_imports`]. It's an error rather than a panic, even
/// in debug builds, so that the language server keeps running, and the error fails any test that
/// reaches it anyway.
fn report_pass_limit(
    ctx: &mut Compiler,
    imports: &[(ScopeId, ImportId)],
    cache: &mut ImportCache,
    limit: usize,
) {
    if mem::replace(&mut cache.pass_limit_reported, true) {
        return;
    }

    let Some(&(_, import)) = imports.first() else {
        return;
    };

    let (Items::Named(name) | Items::All(name)) = ctx.import(import).items.clone();
    ctx.diagnostic_name(&name, DiagnosticKind::ImportPassLimit(limit));
}

/// Reports each exported import that only found items that are private where it found them, since
/// a re-export can't make a private item public, as in Rust. Each item an import finds is only
/// exported if it's public there, so a name that's public in only one namespace is re-exported in
/// only that one. A module can export an item through an import that's resolved later, so this is
/// done once every import is resolved. A reported import exports what it found anyway, so that the
/// uses and re-exports of the name aren't reported too, and the imports are resolved again if any
/// were reported. A failed import could bind its name to an error item instead, but a module that
/// re-exports its own item (`const X: Int = 1; export X;`) already has the item with the name.
fn report_private_reexports(ctx: &mut Compiler, cache: &mut ImportCache) -> bool {
    let private = cache
        .reexports
        .keys()
        .copied()
        .filter(|&import_id| cache.is_private_reexport(ctx, import_id))
        .collect::<Vec<_>>();

    // An import waits for the other private re-exports that would export something it found once
    // they're reported (see [`ImportCache::exporters`]), and is only reported if it still finds
    // nothing public once they are, so a chain of re-exports is reported once, at its start. That
    // includes a chain through an exported glob import of `super` or `project`, and a module's own
    // re-export of a name that one brings in. Imports that only wait for each other, such as two
    // re-exports of a name in one module, are all reported.
    let edges = private
        .iter()
        .map(|&import_id| {
            let import = ctx.import(import_id);
            let Items::Named(name) = &import.items else {
                unreachable!("only named imports are recorded");
            };
            let mut waits_for = IndexSet::new();

            for &item in cache.reexports[&import_id].1.keys() {
                waits_for.extend(cache.exporters(ctx, import.base_scope, name.text(), item));
            }

            (import_id, waits_for)
        })
        .collect::<IndexMap<_, _>>();

    let (_, component_of, closed) = closed_components(&edges);

    // They're reported in the order they were recorded in
    let reported = private
        .into_iter()
        .filter(|import_id| closed.contains(&component_of[import_id]))
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

/// Makes the failures in the last pass final, and binds the names of the failed named imports to
/// error items (see [`Compiler::is_error_item`]) in the namespaces that they fail in, wherever
/// nothing else brought them in. This is only done once nothing else can be resolved, since an
/// item with the name could still be found before then. Returns whether anything was bound.
fn bind_failed_imports(
    ctx: &mut Compiler,
    cache: &mut ImportCache,
    resolution: &mut Resolution,
) -> bool {
    for (import, failure) in resolution.failures.drain(..) {
        cache.failed.entry(import).or_insert(failure);
    }

    let mut bound = false;

    for (&import_id, failure) in &cache.failed {
        let import = ctx.import(import_id);

        let Items::Named(name) = &import.items else {
            continue;
        };

        let (name, exported) = (name.text().to_string(), import.exported);

        bound |= bind_error_items(
            ctx,
            &mut cache.error_origins,
            (failure.scope, import_id),
            &name,
            &failure.namespaces,
            exported,
        );
    }

    bound
}

/// Binds a name that an import brings into a scope to new error items in some namespaces, where
/// nothing else brought it in. Where another import's error item already has the name (such as
/// `import nope::x;` along with `import x;`), this import explains what finds it too, and it's
/// exported if either import is. Returns whether anything was bound or exported.
fn bind_error_items(
    ctx: &mut Compiler,
    error_origins: &mut HashMap<(ScopeId, Item), IndexSet<ImportId>>,
    (scope, import_id): (ScopeId, ImportId),
    name: &str,
    namespaces: &[Namespace],
    exported: bool,
) -> bool {
    let mut bound = false;

    for &namespace in namespaces {
        let item = match ctx.scope(scope).item(namespace, name) {
            None => {
                ctx.note_placeholder();
                let item = match namespace {
                    Namespace::Value => Item::Value(ctx.alloc_symbol(Symbol::Unresolved)),
                    Namespace::Type => Item::Type(ctx.alloc_type(Type::Unresolved)),
                    Namespace::Module => Item::Module(ctx.alloc_symbol(Symbol::Unresolved)),
                };

                ctx.add_error_item(item);
                item
            }
            Some(existing) if ctx.is_error_item(existing) => existing,
            Some(_) => continue,
        };

        error_origins
            .entry((scope, item))
            .or_default()
            .insert(import_id);
        bound |= bind(ctx, scope, import_id, name, item, exported);
    }

    bound
}

/// Adds what an error item stands for in the scope it's imported from to what an error item in
/// the name it's imported into stands for, which is the same item, or one that another import
/// would have filled the name with too, so that either explains what finds the name. Returns
/// whether anything was added.
fn add_error_origins(
    error_origins: &mut HashMap<(ScopeId, Item), IndexSet<ImportId>>,
    into: (ScopeId, Item),
    from: (ScopeId, Item),
) -> bool {
    if into == from {
        return false;
    }

    let from = error_origins.get(&from).cloned().unwrap_or_default();
    let into = error_origins.entry(into).or_default();
    let count = into.len();
    into.extend(from);
    into.len() != count
}

/// Records that an import found an error item in a scope, which the imports it stands for there
/// explain.
fn link_error(cache: &mut ImportCache, import_id: ImportId, scope: ScopeId, item: Item) {
    if let Some(origins) = cache.error_origins.get(&(scope, item)) {
        cache
            .error_links
            .entry(import_id)
            .or_default()
            .extend(origins.iter().copied());
    }
}

/// Records the failed imports whose error items an import finds in the scope where it looks a
/// name up, in any namespace, since they explain why the import failed there or resolved to an
/// error. Error items are only ever in a module's own scope, not outside of it.
fn link_errors(
    ctx: &Compiler,
    cache: &mut ImportCache,
    import_id: ImportId,
    base: Base,
    name: &str,
) {
    let scope = base.scope();

    for namespace in Namespace::ALL {
        if let Some(item) = ctx.scope(scope).item(namespace, name) {
            link_error(cache, import_id, scope, item);
        }
    }
}

/// The failed imports whose failures are reported. An import that found error items where it
/// failed (or that it resolved through instead) failed because the imports they come from did, and
/// so on (see `error_links`). Following those reaches imports that failed on their own, or groups
/// of imports that failed because of each other, such as `import a::b; import b::a;`, whose
/// failures are reported, since nothing else explains them. Every other failure is explained by
/// one of those. So the failures reported are those in the strongly connected components that
/// don't link to any other, which doesn't depend on the order the imports are resolved in.
fn reported_failures(cache: &ImportCache) -> HashSet<ImportId> {
    // Error items only stand for failed imports, so the links only lead to them
    let links = cache
        .failed
        .keys()
        .map(|&import| {
            let links = cache.error_links.get(&import).cloned().unwrap_or_default();
            (import, links)
        })
        .collect::<IndexMap<_, _>>();

    let (components, _, closed) = closed_components(&links);

    closed
        .into_iter()
        .flat_map(|index| components[index].iter().copied())
        .collect()
}

/// The strongly connected components of a graph of imports, the component that each import is
/// in, and the indices of the closed components: those with no edge to an import in another
/// component. An edge to an import that isn't in the graph doesn't count.
fn closed_components(
    edges: &IndexMap<ImportId, IndexSet<ImportId>>,
) -> (Vec<Vec<ImportId>>, HashMap<ImportId, usize>, HashSet<usize>) {
    let components = rue_types::strongly_connected_components(edges);

    let component_of = components
        .iter()
        .enumerate()
        .flat_map(|(index, component)| component.iter().map(move |&import| (import, index)))
        .collect::<HashMap<_, _>>();

    let closed = components
        .iter()
        .enumerate()
        .filter(|&(index, component)| {
            component.iter().all(|import| {
                edges[import]
                    .iter()
                    .all(|other| component_of.get(other).is_none_or(|&other| other == index))
            })
        })
        .map(|(index, _)| index)
        .collect();

    (components, component_of, closed)
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
                // A failed import's error item doesn't contradict anything. It only fills a name
                // once nothing else can be resolved, so this is only a guard.
                if ctx.is_error_item(item) {
                    return false;
                }

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
    /// The imports whose paths don't resolve, which bind their names to error items once nothing
    /// else can be resolved (see [`fails_to_import`]).
    failed_paths: HashSet<ImportId>,
    /// The lookups that are waiting for a module to settle in this pass.
    waiting: Vec<WaitingLookup>,
    /// Whether the bindings are settled, so that what names could refer to besides what they're
    /// bound to through ambiguous paths is worked out (see [`resolve_imports`]).
    settled: bool,
    /// The imports that failed in this pass, and how. They've failed for good once nothing else can
    /// be resolved (see [`bind_failed_imports`]).
    failures: IndexMap<ImportId, Failure>,
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
/// as for a glob import of it. With `cache`, an item that isn't exported yet counts if a private
/// re-export that's still to be reported would export it (see [`ImportCache::exporters`]). A cycle
/// of modules that nothing else brings the item into can't bring it in. Every import that could is
/// recorded in `blockers`.
#[allow(clippy::too_many_arguments)]
fn may_gain(
    ctx: &Compiler,
    resolution: &Resolution,
    cache: Option<&ImportCache>,
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
        && (!exported
            || target.is_item_exported(item)
            || cache.is_some_and(|cache| !cache.exporters(ctx, scope, name, item).is_empty()))
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
            gains |= may_bind(ctx, resolution, cache, import, namespace, name, blockers);
        }
    }

    for &import in resolution.glob_imports.get(&scope).into_iter().flatten() {
        if relevant(import) {
            gains |= may_import(ctx, resolution, cache, import, namespace, name, blockers);
        }
    }

    gains
}

/// Whether a named import could still bring in an item with its name in a namespace.
fn may_bind(
    ctx: &Compiler,
    resolution: &Resolution,
    cache: Option<&ImportCache>,
    import: ImportId,
    namespace: Namespace,
    name: &str,
    blockers: &mut Blockers,
) -> bool {
    !resolution.bound.contains(&import)
        && may_import(ctx, resolution, cache, import, namespace, name, blockers)
}

/// Whether a module will bind a name in a namespace to an error item once nothing else can be
/// resolved (see [`bind_failed_imports`]), because a named import of the name into it only found a
/// private item or its path doesn't resolve. Lookups past the module's own items don't find the
/// name, like they don't once it's bound.
fn fails_to_import(
    ctx: &Compiler,
    resolution: &Resolution,
    scope: ScopeId,
    namespace: Namespace,
    name: &str,
) -> bool {
    resolution
        .named_imports_of(scope, name)
        .into_iter()
        .flatten()
        .any(|import| {
            // An import whose path doesn't resolve fails in every namespace
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
    cache: Option<&ImportCache>,
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
        ctx, resolution, cache, source, namespace, name, None, !members, blockers,
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

    // A blocker that isn't waiting itself is resolved, so it doesn't count
    let (_, component_of, stuck) = closed_components(&edges);

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

    // A segment after the first isn't looked up outside of the module, so a failed import of it
    // doesn't matter there
    let outside = match base {
        Base::Inner(_) => None,
        _ if fails_to_import(ctx, resolution, scope, namespace, name) => None,
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
            None,
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
) -> bool {
    let import = ctx.import(import_id).clone();

    if !resolve_path(ctx, import_scope, import_id, &import, cache, resolution) {
        return false;
    }

    let base = cache.paths.get(&import_id).map(|path| path.scope);
    let alternatives = if resolution.settled {
        path_alternatives(ctx, import_scope, import_id, &import, cache)
    } else {
        Vec::new()
    };

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
            &alternatives,
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
            &alternatives,
            cache,
            resolution,
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

        let found = match lookup(
            ctx,
            &cache.forced,
            resolution,
            import_id,
            key,
            name,
            segment_base,
            Namespace::Module,
        ) {
            Lookup::Found(module, via) => Some((module, via)),
            Lookup::Undetermined => return false,
            Lookup::NotFound => None,
        };

        // The failed imports whose error items are found explain this import's failure, or why
        // it resolves to an error
        if found.is_none_or(|(module, _)| ctx.is_error_item(module)) {
            link_errors(ctx, cache, import_id, segment_base, name.text());
        }

        let Some((module, via)) = found else {
            resolution.failed_paths.insert(import_id);

            // Only a module can have a path after it, so a value or type is reported as such. Like
            // the module, it isn't looked for past a name that fails to be imported.
            let other = [Namespace::Value, Namespace::Type]
                .into_iter()
                .any(|namespace| {
                    let scope = segment_base.scope();

                    let found = ctx.scope(scope).item(namespace, name.text()).or_else(|| {
                        if fails_to_import(ctx, resolution, scope, namespace, name.text()) {
                            return None;
                        }

                        segment_base
                            .outside(ctx, namespace, name.text())
                            .map(|(item, _)| item)
                    });

                    found.is_some_and(|item| !ctx.is_error_item(item))
                });

            let kind = if other {
                DiagnosticKind::SubpathNotSupported(name.text().to_string())
            } else {
                DiagnosticKind::UndeclaredSymbol(name.text().to_string())
            };

            resolution.failures.insert(
                import_id,
                Failure {
                    scope: import_scope,
                    namespaces: Namespace::ALL.to_vec(),
                    errors: vec![(name.clone(), kind)],
                },
            );
            return false;
        };

        // It resolves to an error without being reported. It failed in the pass before the error
        // item was bound, since nothing had the name, so a named import's own name is bound to
        // error items already.
        if ctx.is_error_item(module) {
            return false;
        }

        let symbol = module.symbol().expect("a module is a symbol");

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

/// The other modules that an import's resolved path could name, if the names in it were bound to
/// the other modules that glob imports bring in with them (see [`alternative_modules`]). What the
/// import brings in could be what they have, so it's used and exported along with what it does
/// (see [`add_path_alternatives`] and [`add_glob_alternatives`]). The imports they're found
/// through are used by this one, like the ones in its path.
fn path_alternatives(
    ctx: &mut Compiler,
    import_scope: ScopeId,
    import_id: ImportId,
    import: &Import,
    cache: &ImportCache,
) -> Vec<SymbolId> {
    let Some(path) = cache.paths.get(&import_id) else {
        return Vec::new();
    };

    let mut alternatives = Vec::new();

    for (index, (name, &symbol)) in import.path.iter().zip(&path.symbols).enumerate() {
        let via = path
            .references
            .iter()
            .find(|&&(segment, _, _)| segment == index)
            .map(|&(_, via, _)| via);

        // Where the name is looked up in the module the path names up to it
        let place = if index == 0 {
            match import_base(import, import_scope) {
                Base::Local(scope) => Place::Chain(scope),
                Base::Member(scope) | Base::Inner(scope) => Place::Scope(scope),
            }
        } else {
            Place::Scope(ctx.module(path.symbols[index - 1]).scope)
        };

        let modules = alternative_modules(
            ctx,
            Some(place),
            &alternatives,
            name.text(),
            Some((symbol, via)),
        );

        for &(module, via) in &modules {
            if let Some(via) = via {
                let user = ImportUser::Import(import_id);
                ctx.add_import_reference(via, Declaration::Symbol(module), user);
            }
        }

        alternatives = modules_of(modules, Some((symbol, via)));
    }

    alternatives
}

/// Records that a name that an import binds, through the binding recorded on `recorded`, could
/// also refer to what each of the other modules its path could name exports with the name (see
/// [`path_alternatives`]), and to what that could refer to. Returns whether any of them weren't
/// already recorded.
fn add_path_alternatives(
    ctx: &mut Compiler,
    alternatives: &[SymbolId],
    recorded: ImportId,
    binding: Item,
    name: &str,
) -> bool {
    if alternatives.is_empty() {
        return false;
    }

    let namespaces = [binding.namespace()];
    let mut updated = false;

    for (item, via) in alternative_items(ctx, None, alternatives, &namespaces, name, true) {
        updated |= ctx.add_ambiguous_alternatives(recorded, binding.into(), via, item.into());
    }

    updated
}

/// Records what a glob import could bring in besides what it does (see
/// [`Database::import_alternatives`](rue_hir::Database::import_alternatives)): what each of the other
/// modules its path could name exports, what the glob imports that they, or the module it imports
/// from, export could bring in, and once any of that is recorded, what it does bring in, which a
/// failed import's error item could hide one way but not another. Returns whether any of it
/// wasn't already recorded.
fn add_glob_alternatives(
    ctx: &mut Compiler,
    import_id: ImportId,
    import: &Import,
    base: Option<ScopeId>,
    alternatives: &[SymbolId],
) -> bool {
    let mut found = Vec::new();

    for &module in alternatives {
        let scope = ctx.module(module).scope;

        for namespace in Namespace::ALL {
            for (name, item) in ctx.scope(scope).exported_items(namespace) {
                if !ctx.is_error_item(item) {
                    found.push((namespace, name, item));
                }
            }
        }

        found.extend(scope_alternatives(ctx, scope, true));
    }

    // A glob import of a parent module brings in its private items too
    let source = base.unwrap_or(import.base_scope);
    found.extend(scope_alternatives(ctx, source, base.is_some()));

    // What it brings in from the module it imports from could be hidden by a failed import's
    // error item one way but not another, so that's included too, if anything else could be
    // different
    if !found.is_empty() || !alternatives.is_empty() {
        for namespace in Namespace::ALL {
            let items = if base.is_some() {
                ctx.scope(source).exported_items(namespace)
            } else {
                ctx.scope(source).items(namespace)
            };

            for (name, item) in items {
                if !ctx.is_error_item(item) {
                    found.push((namespace, name, item));
                }
            }
        }
    }

    let mut updated = false;

    for (namespace, name, item) in found {
        updated |= ctx.add_import_alternative(import_id, namespace, name, item);
    }

    updated
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

    // A segment after the first names a module in the one before it, which might be private
    let symbols = &path.symbols;

    for index in 1..symbols.len() {
        let parent = ctx.module(symbols[index - 1]).scope;

        if ctx.scope(parent).is_module_exported(symbols[index]) {
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
    alternatives: &[SymbolId],
    cache: &mut ImportCache,
    resolution: &mut Resolution,
) -> bool {
    let mut updated =
        resolution.settled && add_glob_alternatives(ctx, import_id, import, base, alternatives);

    let mut count = cache
        .glob_import_counts
        .get(&import_id)
        .map_or(0, |&(_, _, count)| count);

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

            items
        };

        for (name, item) in items {
            let module = ctx.scope(source_scope);

            // The import the item was brought into the module by, if any
            let via = module.item_import(item);

            // Only an item that's public where it's imported from is exported. A file's sibling
            // modules aren't items of the file, and they're public.
            let exported = import.exported
                && (module.item(namespace, &name) != Some(item) || module.is_item_exported(item));

            // A failed import's error item only fills a name that nothing else brought in. Error
            // items are only bound once nothing else can be resolved, so nothing else brings in
            // the name after one does, and the name is never ambiguous. Where this import already
            // brought it in, or another glob import's error item fills the name, which this one
            // would have filled too, what the item stands for here explains what finds the name
            // too. A named import's doesn't: it shadows this import, so it fails on its own.
            if let Some(existing) = ctx.scope(import_scope).item(namespace, &name)
                && (ctx.is_error_item(item) || ctx.is_error_item(existing))
            {
                let by_glob = ctx
                    .scope(import_scope)
                    .item_import(existing)
                    .is_some_and(|via| ctx.import(via).is_glob());

                if ctx.is_error_item(item) && ctx.is_error_item(existing) && by_glob {
                    updated |= add_error_origins(
                        &mut cache.error_origins,
                        (import_scope, existing),
                        (source_scope, item),
                    );

                    // Like a name that several glob imports bring in (see below), it's exported if
                    // any of them exports it, and each of them brings it in, whichever bound it
                    updated |= bind(ctx, import_scope, import_id, &name, existing, exported);
                    count = count.max(1);
                }

                continue;
            }

            // A named import of the name shadows the glob import in the namespaces that it
            // brings an item into, or could still, including once a private re-export that's
            // still to be reported exports what it imports
            if resolution
                .named_imports_of(import_scope, &name)
                .into_iter()
                .flatten()
                .any(|&named| {
                    ctx.scope(import_scope).item_import(item) == Some(named)
                        || may_bind(
                            ctx,
                            resolution,
                            Some(cache),
                            named,
                            namespace,
                            &name,
                            &mut Blockers::default(),
                        )
                })
            {
                // A named import that brings in something with the name here might not if its
                // path were bound differently, and then this one would bring the item in
                if resolution.settled
                    && !ctx.is_error_item(item)
                    && resolution
                        .named_imports_of(import_scope, &name)
                        .into_iter()
                        .flatten()
                        .any(|&named| ctx.is_partial_import(named, namespace))
                {
                    updated |= ctx.add_import_alternative(import_id, namespace, name, item);
                }

                continue;
            }

            let existing = ctx.scope(import_scope).item(namespace, &name);

            // The glob import that the binding of the name was recorded on, if any
            let recorded = existing
                .and_then(|existing| ctx.scope(import_scope).item_import(existing))
                .filter(|&recorded| ctx.import(recorded).is_glob());

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
                if ctx.add_shared_binding(bound_by, existing.into(), import_id, item.into()) {
                    ctx.import_mut(import_id)
                        .declarations
                        .push((name.clone(), item.into()));
                    count += 1;
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
                count += 1;
            }

            // The name could also refer to what the other modules that the path could name have
            // with it, which is recorded on the glob import that bound it, whichever one did
            if !alternatives.is_empty()
                && let Some((binding, Some(recorded))) =
                    ctx.scope(import_scope).item_with_import(namespace, &name)
                && ctx.import(recorded).is_glob()
            {
                updated |= add_path_alternatives(ctx, alternatives, recorded, binding, &name);
            }
        }
    }

    cache
        .glob_import_counts
        .entry(import_id)
        .or_insert_with(|| (import_scope, star.clone(), 0))
        .2 = count;

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
    alternatives: &[SymbolId],
    cache: &mut ImportCache,
    resolution: &mut Resolution,
) -> bool {
    let diagnostics = resolution.diagnostics;
    let lookup_base = import_base(import, import_scope);
    let mut updated = false;

    let name = item.text();

    // Each item is resolved along with the import it was brought into its scope by, if any, in
    // every namespace that has the name
    let found = if let Some(base) = base {
        match named_items(ctx.scope(base), name) {
            NamedItems::Public(found) => {
                // An exported error item comes from a failed import in the module, which explains
                // why this one brings in an error
                for &(found, _) in &found {
                    link_error(cache, import_id, base, found);
                }

                found
            }
            NamedItems::Private(items) => {
                // A name with only private items is reported as private rather than unresolved
                let module = import.path.last().unwrap().text();
                let failure = Failure::private(import_scope, item, module, &items);
                resolution.failures.insert(import_id, failure);
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

        // The failed imports whose error items are found explain this import's failure, or why
        // it brings in an error
        if found.is_empty() || found.iter().any(|&(item, _)| ctx.is_error_item(item)) {
            link_errors(ctx, cache, import_id, lookup_base, name);
        }

        if found.is_empty() && !undetermined {
            resolution
                .failures
                .insert(import_id, Failure::missing(import_scope, item));
        }

        found
    };

    let unused_imports = cache
        .unused_imports
        .entry(import_id)
        .or_insert_with(|| single_name(item));

    // This is recorded in every pass, since an import of a name with no path finds its own binding
    // once it has been resolved. The import that it's found through is used by this one, like one
    // in its path.
    for &(found, via) in &found {
        if let Some(via) = via
            && via != import_id
            && !ctx.is_error_item(found)
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

    // Another named import of the name could bring in the same items
    if resolution
        .named_imports_of(import_scope, name)
        .is_some_and(|imports| imports.len() > 1)
    {
        cache.named_finds.entry(import_id).or_default().extend(
            found
                .iter()
                .copied()
                .filter(|&(item, _)| !ctx.is_error_item(item)),
        );
    }

    for &(found, _) in &found {
        let namespace = found.namespace();
        let error = ctx.is_error_item(found);
        let existing = ctx.scope(import_scope).item(namespace, name);

        // A failed import's error item only fills a name that nothing else brought in, so it's
        // never a duplicate. What it stands for where it's found explains what finds the name
        // here too, whether it fills it or another failed import's error item does.
        if error {
            if existing.is_none_or(|existing| existing == found)
                && bind(ctx, import_scope, import_id, name, found, import.exported)
            {
                updated = true;
                unused_imports.shift_remove(name);
            }

            if let Some(target) = ctx
                .scope(import_scope)
                .item(namespace, name)
                .filter(|&target| ctx.is_error_item(target))
            {
                let source = base.unwrap_or(lookup_base.scope());

                updated |= add_error_origins(
                    &mut cache.error_origins,
                    (import_scope, target),
                    (source, found),
                );
            }

            continue;
        }

        // An import with no path can find a private item of its own module, or of the one that
        // `super` or `project` names, which it only exports if it's public there
        let mut exported = import.exported;

        if import.exported && base.is_none() {
            let module = ctx.scope(import.base_scope);
            let in_module = module.item(namespace, name) == Some(found);

            exported = !in_module || module.is_item_exported(found);

            // A name that's already bound to a different item is reported as a duplicate instead
            if existing.is_none_or(|existing| existing == found) {
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

        // The name could also refer to what the other modules that the path could name have
        // with it, which is recorded on the import that bound it, if another one brought in the
        // same item first
        if !alternatives.is_empty()
            && let Some((binding, Some(recorded))) =
                ctx.scope(import_scope).item_with_import(namespace, name)
            && binding == found
        {
            updated |= add_path_alternatives(ctx, alternatives, recorded, found, name);
        }
    }

    // In each namespace that it brings nothing in, it could bring in what the other modules its
    // path could name export with the name, if the path were bound to one of them. In each that it
    // does, it might bring in nothing if one of them doesn't export anything with the name.
    if !alternatives.is_empty() {
        for namespace in Namespace::ALL {
            if found
                .iter()
                .any(|&(found, _)| found.namespace() == namespace && !ctx.is_error_item(found))
            {
                let partial = alternatives.iter().any(|&module| {
                    let scope = ctx.scope(ctx.module(module).scope);

                    scope
                        .item_with_import(namespace, name)
                        .is_none_or(|(item, _)| {
                            !scope.is_item_exported(item) || ctx.is_error_item(item)
                        })
                });

                if partial {
                    updated |= ctx.add_partial_import(import_id, namespace);
                }

                continue;
            }

            let namespaces = [namespace];

            for (item, _) in alternative_items(ctx, None, alternatives, &namespaces, name, true) {
                updated |= ctx.add_import_alternative(import_id, namespace, name.to_string(), item);
            }
        }
    }

    // An import that fails, or only brings in error items, is reported (or explained) as failed
    // rather than as unused
    if diagnostics && found.iter().all(|&(found, _)| ctx.is_error_item(found)) {
        unused_imports.shift_remove(name);
    }

    updated
}

/// Picks which of the named imports into a scope that bring in the same item with the same name
/// is used. Whichever is resolved first binds the name, which depends on module names, so the
/// exported one, or else the first one in the source, gets the uses of the binding instead, and
/// the others don't bring it in, so they're reported as unused. An import that finds the item
/// through another of them (such as `import X;` after `import a::X;`) needs that one, so it isn't
/// one of them.
fn resolve_duplicate_imports(ctx: &mut Compiler, cache: &mut ImportCache, resolution: &Resolution) {
    // The names that more than one named import imports into a scope, in a fixed order
    let mut groups = resolution
        .named_imports
        .iter()
        .flat_map(|(&scope, names)| {
            names
                .iter()
                .filter(|(_, imports)| imports.len() > 1)
                .map(move |(name, imports)| (scope, name, imports))
        })
        .collect::<Vec<_>>();

    groups.sort_by_key(|&(_, _, imports)| imports[0].index());

    for (scope, name, imports) in groups {
        let finds = imports
            .iter()
            .flat_map(|&import| {
                cache
                    .named_finds
                    .get(&import)
                    .into_iter()
                    .flatten()
                    .map(move |&(item, via)| (import, item, via))
            })
            .collect::<IndexSet<_>>();

        let importers = finds
            .iter()
            .map(|&(import, _, _)| import)
            .collect::<IndexSet<_>>();

        let items = finds
            .iter()
            .map(|&(_, item, _)| item)
            .collect::<IndexSet<_>>();

        for item in items {
            let mut candidates = finds
                .iter()
                .filter(|&&(_, found, via)| {
                    found == item && via.is_none_or(|via| !importers.contains(&via))
                })
                .map(|&(import, _, _)| import)
                .collect::<IndexSet<_>>();

            if candidates.len() < 2 {
                continue;
            }

            let Some(bound_by) = ctx
                .scope(scope)
                .item_import(item)
                .filter(|bound_by| candidates.contains(bound_by))
            else {
                continue;
            };

            candidates.sort_by_key(|&import| (!ctx.import(import).exported, import.index()));
            let used = candidates[0];
            let declaration = Declaration::from(item);

            if used != bound_by {
                ctx.add_shared_binding(bound_by, declaration, used, declaration);

                let entry = (name.clone(), declaration);

                if !ctx.import(used).declarations.contains(&entry) {
                    ctx.import_mut(used).declarations.push(entry);
                }

                if let Some(unused) = cache.unused_imports.get_mut(&used) {
                    unused.shift_remove(name);
                }
            }

            for &import in candidates.iter().skip(1) {
                ctx.import_mut(import)
                    .declarations
                    .retain(|&(_, found)| found != declaration);

                // It's reported as bringing in nothing, unless it brings in something else with
                // the name (in another namespace)
                if ctx.import(import).declarations.is_empty()
                    && let Items::Named(item) = &ctx.import(import).items
                {
                    cache
                        .unused_imports
                        .entry(import)
                        .or_default()
                        .insert(name.clone(), item.clone());
                }
            }
        }
    }
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
/// declaration is recorded on the import, unless it's an error item (see
/// [`Compiler::is_error_item`]), which isn't anything that could be used. Returns whether it did
/// either.
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

    // An error item isn't anything that could be used
    if !ctx.is_error_item(item) {
        ctx.import_mut(import_id)
            .declarations
            .push((name.to_string(), item.into()));
    }

    true
}
