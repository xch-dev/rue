use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
};

use indexmap::IndexMap;
use rowan::TextRange;
use rue_ast::{AstImportItem, AstImportPathSegment};
use rue_diagnostic::{DiagnosticKind, Name};
use rue_hir::{
    Declaration, Import, ImportId, ImportUser, Item, Items, Namespace, Scope, ScopeId, Symbol,
    SymbolId,
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
    /// The imports whose lookups through enclosing scopes stopped waiting for nearer scopes,
    /// which are checked once every import is resolved.
    forced: HashSet<ImportId>,
    /// The names in each scope that more than one import (or a declaration and an import) bring
    /// the same module in for, with the imports other than the one recorded as bringing it in.
    shared_modules: HashMap<(ScopeId, String), HashSet<ImportId>>,
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

/// The imports in each module, along with how deeply each module's scope is nested in the files'
/// modules (a file's is 0, and an inline module's is one more than the module it's in).
fn flatten_imports(
    ctx: &mut Compiler,
    top_level_modules: Vec<SymbolId>,
) -> (Vec<(ScopeId, ImportId)>, HashMap<ScopeId, usize>) {
    let mut imports = Vec::new();
    let mut depths = HashMap::new();
    let mut stack = vec![(None, 0, top_level_modules)];

    while let Some((scope, depth, modules)) = stack.pop() {
        if let Some(scope) = scope {
            depths.insert(scope, depth);

            for import in ctx.scope(scope).imports() {
                imports.push((scope, import));
            }
        }

        let depth = if scope.is_some() { depth + 1 } else { 0 };

        for module in modules {
            let Symbol::Module(module) = ctx.symbol(module).clone() else {
                unreachable!();
            };

            stack.push((
                Some(module.scope),
                depth,
                module.declarations.modules.clone(),
            ));
        }
    }

    (imports, depths)
}

pub fn resolve_imports(
    ctx: &mut Compiler,
    all_modules: Vec<SymbolId>,
    cache: &mut ImportCache,
    diagnostics: bool,
) {
    let (mut imports, module_depths) = flatten_imports(ctx, all_modules);
    imports.sort_by(|(_, left), (_, right)| compare_imports(ctx.import(*left), ctx.import(*right)));
    let mut resolution = Resolution {
        diagnostics,
        named_imports: HashMap::new(),
        glob_imports: HashMap::new(),
        module_depths,
        deferred: Vec::new(),
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
        resolution.deferred.clear();
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

        // Nothing else can be resolved, so the lookups that are waiting for a module to bring in a
        // name are made without waiting. The outermost modules' go first, since the imports that
        // a lookup waits for are in its own module or one that it's in (through `super`). A
        // forced lookup can still be wrong if the modules wait on each other, which is checked
        // once they're all resolved.
        let Some(depth) = resolution.deferred.iter().map(|&(depth, _)| depth).min() else {
            break;
        };

        cache.forced.extend(
            resolution
                .deferred
                .iter()
                .filter(|&&(d, _)| d == depth)
                .map(|&(_, import)| import),
        );
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

        let import_scopes = imports
            .iter()
            .map(|&(scope, import)| (import, scope))
            .collect::<HashMap<_, _>>();

        for &(_, import) in &imports {
            if cache.forced.contains(&import) {
                check_forced_import(ctx, cache, &resolution, &import_scopes, import);
            }
        }

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

/// Reports an import whose lookup through enclosing scopes was forced, if one of the scopes it
/// skipped has since brought in a different item with the name. A use of the name in the
/// importing scope would find that item instead, so the import is ambiguous.
fn check_forced_import(
    ctx: &mut Compiler,
    cache: &ImportCache,
    resolution: &Resolution,
    import_scopes: &HashMap<ImportId, ScopeId>,
    import_id: ImportId,
) {
    let import_scope = import_scopes[&import_id];
    let import = ctx.import(import_id).clone();

    // A lookup through `super` can be forced too, but its path isn't cached, so it isn't checked
    if import.base_scope != import_scope {
        return;
    }

    let ambiguous = if let Some(first) = import.path.first() {
        // The first segment of the path, which is cached once it's resolved
        let owner = path_owner(resolution, &import, import_scope, import_id);
        let Some(cached) = cache
            .scopes
            .get(&(import_scope, owner, vec![first.text().to_string()]))
        else {
            return;
        };

        // A glob import can bring in an item with the same name as the module it imports from,
        // which doesn't change what its own path refers to, unless another import brings in the
        // same item (other than by importing it back from this scope)
        let skip_own = cache
            .shared_modules
            .get(&(import_scope, first.text().to_string()))
            .is_none_or(|others| {
                others.iter().all(|&other| {
                    imported_back(ctx, cache, resolution, import_scopes, other, import_id)
                })
            });

        (lookup_past(
            ctx,
            import_scope,
            skip_own.then_some(import_id),
            first.text(),
            Scope::module_with_import,
        ) != Some(cached.symbol))
        .then_some(first)
    } else if let Items::Named(name) = &import.items {
        // Each namespace the import bound the name in is checked
        Namespace::ALL
            .into_iter()
            .any(|namespace| {
                ctx.scope(import_scope)
                    .item_with_import(namespace, name.text())
                    .filter(|&(_, via)| via == Some(import_id))
                    .is_some_and(|(bound, _)| {
                        lookup_past(
                            ctx,
                            import_scope,
                            Some(import_id),
                            name.text(),
                            |scope, name| scope.item_with_import(namespace, name),
                        ) != Some(bound)
                    })
            })
            .then_some(name)
    } else {
        None
    };

    if let Some(name) = ambiguous {
        ctx.diagnostic_name(
            name,
            DiagnosticKind::AmbiguousOuterImport(name.text().to_string()),
        );
    }
}

/// Whether an import brings in the module that `origin` brought into its scope only by importing
/// it back from there, through a chain of imports with paths. If the chain can't be followed, it's
/// treated as bringing the symbol in on its own.
fn imported_back(
    ctx: &Compiler,
    cache: &ImportCache,
    resolution: &Resolution,
    import_scopes: &HashMap<ImportId, ScopeId>,
    mut import_id: ImportId,
    origin: ImportId,
) -> bool {
    let origin_scope = import_scopes[&origin];
    let Some(first) = ctx.import(origin).path.first() else {
        return false;
    };
    let Some(module) = ctx.scope(origin_scope).module(first.text()) else {
        return false;
    };
    let mut visited = HashSet::new();

    while visited.insert(import_id) {
        let import = ctx.import(import_id);
        let scope = import_scopes[&import_id];
        let path = import
            .path
            .iter()
            .map(|name| name.text().to_string())
            .collect::<Vec<_>>();
        let owner = path_owner(resolution, import, scope, import_id);

        // The module the import brought the symbol in from
        let Some(cached) = cache.scopes.get(&(scope, owner, path)) else {
            return false;
        };
        let Some(via) = ctx.scope(cached.scope).item_import(Item::Module(module)) else {
            return false;
        };

        if cached.scope == origin_scope && via == origin {
            return true;
        }

        import_id = via;
    }

    false
}

/// What a use of a name in the importing scope finds, apart from what an import brought in.
fn lookup_past<T>(
    ctx: &Compiler,
    import_scope: ScopeId,
    import_id: Option<ImportId>,
    name: &str,
    find: impl Fn(&Scope, &str) -> Option<(T, Option<ImportId>)>,
) -> Option<T> {
    ctx.lookup_in(import_scope, |_, scope| {
        find(scope, name)
            .filter(|&(_, via)| import_id.is_none() || via != import_id)
            .map(|(item, _)| item)
    })
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
    /// How deeply each module's scope is nested (see [`flatten_imports`]).
    module_depths: HashMap<ScopeId, usize>,
    /// The imports with a lookup that's waiting in this pass, and the depth of their modules.
    deferred: Vec<(usize, ImportId)>,
    /// The names that named imports found only private items for, with the scope they're
    /// imported into and the namespaces the private items are in.
    private_names: Vec<(ScopeId, String, Vec<Namespace>)>,
}

impl Resolution {
    fn named_imports_of(&self, scope: ScopeId, name: &str) -> Option<&Vec<ImportId>> {
        self.named_imports.get(&scope)?.get(name)
    }

    /// The imports that could bring a name into a scope: its named imports of the name, or if it
    /// has none, its glob imports, since they don't bring in a name that has a named import.
    fn imports_of(&self, scope: ScopeId, name: &str) -> &[ImportId] {
        self.named_imports_of(scope, name)
            .or_else(|| self.glob_imports.get(&scope))
            .map_or(&[], Vec::as_slice)
    }

    /// Whether a scope that doesn't have the name yet could still bring it in through one of its
    /// imports, other than the import that's looking it up.
    fn may_bring_in(&self, scope: ScopeId, name: &str, importer: ImportId) -> bool {
        self.imports_of(scope, name)
            .iter()
            .any(|&import| import != importer)
    }

    /// Whether the import is one of those that could bring the name into the scope.
    fn may_bring_in_itself(&self, scope: ScopeId, name: &str, importer: ImportId) -> bool {
        self.imports_of(scope, name).contains(&importer)
    }

    fn defer(&mut self, scope: ScopeId, import: ImportId) {
        let depth = self.module_depths.get(&scope).copied().unwrap_or_default();
        self.deferred.push((depth, import));
    }
}

/// What a name refers to, when it's looked up from a scope.
enum Lookup<T> {
    /// The item, and the import that brought it into the scope it was found in, if any.
    Found(T, Option<ImportId>),
    /// A scope nearer than the item could still bring the name in, so it isn't known yet.
    Undetermined,
    NotFound,
}

impl<T> Lookup<T> {
    /// The item, if it's known. If it isn't known yet, `undetermined` is set.
    fn found(self, undetermined: &mut bool) -> Option<(T, Option<ImportId>)> {
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

/// Looks up a name from a scope and then each enclosing scope, the way that a use of the name in
/// that scope would be resolved once every import is. Imports are resolved in passes, so a scope
/// that could still bring the name in through an unresolved import has to be waited for, or a
/// nearer item would be missed. Once nothing else can be resolved, the lookup is forced and those
/// scopes are skipped.
fn lookup_in_scopes<T>(
    ctx: &Compiler,
    resolution: &Resolution,
    force: bool,
    scope: ScopeId,
    name: &str,
    importer: ImportId,
    find: impl Fn(&Scope, &str) -> Option<(T, Option<ImportId>)>,
) -> Lookup<T> {
    ctx.lookup_in(scope, |id, scope| {
        if let Some((item, via)) = find(scope, name) {
            Some(Lookup::Found(item, via))
        } else if !force && resolution.may_bring_in(id, name, importer) {
            Some(Lookup::Undetermined)
        } else {
            None
        }
    })
    .unwrap_or(Lookup::NotFound)
}

/// Looks up a name in the module that `super` names. It contains the import, so its private items
/// can be used, and a file's sibling modules can be too. The module's own items shadow its
/// siblings, so a sibling is only found once the module can't bring in the name through one of
/// its imports.
fn lookup_member(
    ctx: &Compiler,
    resolution: &Resolution,
    force: bool,
    scope: ScopeId,
    namespace: Namespace,
    name: &str,
    importer: ImportId,
) -> Lookup<Item> {
    if let Some((item, via)) = ctx.scope(scope).item_with_import(namespace, name) {
        return Lookup::Found(item, via);
    }

    if !force && resolution.may_bring_in(scope, name, importer) {
        return Lookup::Undetermined;
    }

    match ctx.sibling_item(scope, namespace, name) {
        Some((item, via)) => Lookup::Found(item, via),
        None => Lookup::NotFound,
    }
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
    let force = diagnostics || cache.forced.contains(&import_id);
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
        } else if import.base_scope == import_scope {
            match lookup_in_scopes(
                ctx,
                resolution,
                force,
                import_scope,
                name.text(),
                import_id,
                Scope::module_with_import,
            ) {
                Lookup::Found(module, via) => Some((module, via, true)),
                Lookup::Undetermined => {
                    resolution.defer(import_scope, import_id);
                    return false;
                }
                Lookup::NotFound => None,
            }
        } else {
            match lookup_member(
                ctx,
                resolution,
                force,
                import.base_scope,
                Namespace::Module,
                name.text(),
                import_id,
            ) {
                Lookup::Found(module, via) => module.symbol().map(|module| (module, via, true)),
                Lookup::Undetermined => {
                    resolution.defer(import_scope, import_id);
                    return false;
                }
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

                    for (name, item) in ctx.sibling_items(source_scope, namespace) {
                        if ctx.scope(source_scope).item(namespace, &name).is_some() {
                            continue;
                        }

                        if !force && resolution.may_bring_in(source_scope, &name, import_id) {
                            resolution.defer(import_scope, import_id);
                            continue;
                        }

                        items.push((name, item));
                    }

                    items
                };

                for (name, item) in items {
                    if resolution.named_imports_of(import_scope, &name).is_some() {
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

                    if namespace == Namespace::Module
                        && existing == Some(item)
                        && target.item_import(item) != Some(import_id)
                    {
                        cache
                            .shared_modules
                            .entry((import_scope, name.clone()))
                            .or_default()
                            .insert(import_id);
                    }

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
                        let (found, via) = lookup_in_scopes(
                            ctx,
                            resolution,
                            force,
                            import_scope,
                            name,
                            import_id,
                            |scope, name| scope.item_with_import(namespace, name),
                        )
                        .found(&mut undetermined)?;
                        Some((found, via))
                    })
                    .collect();

                if undetermined {
                    resolution.defer(import_scope, import_id);
                }

                found
            } else {
                let mut undetermined = false;

                let found = Namespace::ALL
                    .into_iter()
                    .filter_map(|namespace| {
                        lookup_member(
                            ctx,
                            resolution,
                            force,
                            import.base_scope,
                            namespace,
                            name,
                            import_id,
                        )
                        .found(&mut undetermined)
                    })
                    .collect::<Vec<_>>();

                if undetermined {
                    resolution.defer(import_scope, import_id);
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

            // This is recorded in every pass, since an import of a name from an enclosing scope
            // finds its own binding once it has been resolved. The import that it's found through
            // is used by this one, like one in its path.
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

                if namespace == Namespace::Module
                    && existing == Some(found)
                    && target.item_import(found) != Some(import_id)
                {
                    cache
                        .shared_modules
                        .entry((import_scope, name.to_string()))
                        .or_default()
                        .insert(import_id);
                }

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
