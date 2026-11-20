//! Renaming a declaration and everything that refers to it.
//!
//! The places to change are the items of the compile that are about the same thing as the one at
//! the cursor, like finding references. A rename is refused rather than guessed at: the name has to
//! be declared in the project's files, the new name has to be a valid one, and it can't be a name
//! that's already used where it would change what any name refers to.

use std::collections::HashSet;
use std::ops::Range;
use std::path::{Component, Path, PathBuf};

use indexmap::IndexMap;
use rowan::{TextRange, TextSize};
use rue_ast::{
    AstEnumItem, AstFunctionItem, AstGenericParameters, AstNode, AstStructFieldBinding,
    AstStructInitializerField,
};
use rue_compiler::{Compiler, File, FileTree};
use rue_diagnostic::SourceKind;
use rue_hir::{Declaration, Item, Namespace, Scope, ScopeId, Symbol, SymbolId};
use rue_lexer::{Lexer, TokenKind, string_value};
use rue_parser::{SyntaxKind, SyntaxToken};
use rue_types::Type;
use tower_lsp_server::ls_types::TextEdit;

use super::{Cache, Role, SyntaxItemKind, Target};
use crate::paths::file_name;
use crate::position::source_range;

/// A name that can be renamed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Renamable {
    pub range: tower_lsp_server::ls_types::Range,
    pub name: String,
}

/// The changes that rename something.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Rename {
    /// The edits to each file, in the order they appear in it.
    pub edits: IndexMap<SourceKind, Vec<TextEdit>>,
    /// The file or directory to rename, if a module that's one of them was renamed.
    pub moved: Option<Moved>,
}

/// A file or directory that's a module, which is renamed along with the module.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Moved {
    pub from: PathBuf,
    pub to: PathBuf,
}

/// A place that names what's being renamed.
struct Site<'a> {
    file: &'a File,
    role: Role,
    token: SyntaxToken,
}

/// What's being renamed, and every place that names it.
struct Plan<'a> {
    target: Target<'a>,
    name: String,
    span: TextRange,
    sites: Vec<Site<'a>>,
    module: Option<ModulePath>,
}

/// A module that's a file or a directory.
struct ModulePath {
    /// The names of the directories it's in, from the one the project is compiled from, and of
    /// the file or directory that it is.
    entries: Vec<String>,
    /// The path of the file that was compiled as the module, if it's a file.
    file: Option<PathBuf>,
    /// The module of the directory it's in, unless it's the directory or file that the project
    /// is compiled from.
    parent: Option<SymbolId>,
}

impl Cache<Compiler> {
    /// The name at the index, if it can be renamed. There's an error if it names something that
    /// can't be, and nothing if it isn't a name.
    pub fn prepare_rename(&self, index: usize) -> Result<Option<Renamable>, String> {
        Ok(self.plan(index)?.map(|plan| Renamable {
            range: source_range(
                &self.source,
                plan.span.start().into(),
                plan.span.end().into(),
            ),
            name: plan.name,
        }))
    }

    /// The changes that rename what's at the index.
    pub fn rename(&self, index: usize, new_name: &str) -> Result<Option<Rename>, String> {
        let Some(plan) = self.plan(index)? else {
            return Ok(None);
        };

        check_name(new_name)?;

        if new_name == plan.name {
            return Ok(Some(Rename::default()));
        }

        self.check_conflicts(&plan, new_name)?;

        let mut replacements = IndexMap::<&SourceKind, (&File, Vec<(TextRange, String)>)>::new();

        for site in &plan.sites {
            let replacement = match (shorthand_field(&site.token), plan.target) {
                // The binding that the shorthand declares or uses keeps its name
                (true, Target::Field(..)) => format!("{new_name}: {}", plan.name),
                (true, Target::Symbol(_) | Target::Type(_)) => {
                    format!("{}: {new_name}", plan.name)
                }
                (false, _) => new_name.to_string(),
            };

            replacements
                .entry(&site.file.source.kind)
                .or_insert_with(|| (site.file, Vec::new()))
                .1
                .push((site.token.text_range(), replacement));
        }

        let mut edits = IndexMap::new();

        for (kind, (file, mut replacements)) in replacements {
            // The same place can be recorded more than once, such as when code is compiled for
            // each place it's used
            replacements.sort_by_key(|(span, _)| (span.start(), span.end()));
            replacements.dedup();

            edits.insert(
                kind.clone(),
                replacements
                    .into_iter()
                    .map(|(span, replacement)| {
                        TextEdit::new(
                            source_range(&file.source, span.start().into(), span.end().into()),
                            replacement,
                        )
                    })
                    .collect(),
            );
        }

        let moved = match &plan.module {
            Some(module) => Some(self.moved(module, &plan.name, new_name)?),
            None => None,
        };

        Ok(Some(Rename { edits, moved }))
    }

    /// The file or directory to rename along with the module that it is.
    fn moved(&self, module: &ModulePath, name: &str, new_name: &str) -> Result<Moved, String> {
        let from = &self.entry_path(module, name)?;

        // A symbolic link elsewhere in the project that leads to it, or to something in it,
        // would be left leading nowhere
        if let Some(root) = &self.root {
            let mut files = Vec::new();
            file_entries(&self.tree, &mut PathBuf::new(), &mut files);

            for (entries, file) in files {
                if let SourceKind::File(path) = &file.source.kind
                    && Path::new(path).starts_with(from)
                    && !root.join(&entries).starts_with(from)
                {
                    return Err(format!(
                        "`{name}` can't be renamed, since the symbolic link at `{}` leads to it",
                        entries.display()
                    ));
                }
            }
        }

        let directory = module.file.is_none();

        let to = from.with_file_name(match from.extension() {
            Some(extension) if !directory => {
                format!("{new_name}.{}", extension.to_string_lossy())
            }
            _ => new_name.to_string(),
        });

        // A name that only differs in case is the same file on some file systems
        if to.exists() && to.canonicalize().ok() != from.canonicalize().ok() {
            return Err(format!("`{}` already exists", file_name(&to)));
        }

        // An external function's file is found from the directory of the file it's declared in,
        // one part of its path at a time. A path that stays in the directory that's renamed, or
        // only leaves it, still leads to its file
        if directory {
            for file in self.tree.all_files() {
                let SourceKind::File(path) = &file.source.kind else {
                    continue;
                };

                let Some(directory) = Path::new(path).parent() else {
                    continue;
                };

                for external in file
                    .document
                    .syntax()
                    .descendants()
                    .filter_map(AstFunctionItem::cast)
                    .filter_map(|function| function.source_path())
                {
                    // The compiler follows the path on the file system, where a part can be a
                    // symbolic link or another spelling of a name, so each step is followed
                    // there too. The path breaks if a step goes into the directory
                    let mut current = directory.to_path_buf();

                    let enters =
                        Path::new(&string_value(external.text()))
                            .components()
                            .any(|component| {
                                let inside = current.starts_with(from);

                                match component {
                                    Component::ParentDir => {
                                        current.pop();
                                    }
                                    Component::Normal(part) => current.push(part),
                                    Component::Prefix(_)
                                    | Component::RootDir
                                    | Component::CurDir => {}
                                }

                                if let Ok(followed) = current.canonicalize() {
                                    current = followed;
                                }

                                !inside && current.starts_with(from)
                            });

                    if enters {
                        return Err(format!(
                            "`{name}` can't be renamed, since the path of the external function \
                            at {} leads into it",
                            self.place(file, external.text_range().start())
                        ));
                    }
                }
            }
        }

        Ok(Moved {
            from: from.clone(),
            to,
        })
    }

    /// Where the file or directory that a module is is on disk, which is found from the
    /// directory that the project is compiled from, one entry at a time. The paths of the
    /// compiled files are where their symbolic links lead, which isn't where the project has
    /// them, so an entry that's a link can't be renamed, and neither can anything in it.
    fn entry_path(&self, module: &ModulePath, name: &str) -> Result<PathBuf, String> {
        let missing = || format!("`{name}` isn't where it was when the project was compiled");

        let mut path = self.root.clone().ok_or_else(missing)?;

        for entry in &module.entries {
            path.push(entry);

            if path.is_symlink() {
                return Err(format!(
                    "`{name}` is reached through a symbolic link, so it can't be renamed"
                ));
            }
        }

        let found = match &module.file {
            Some(file) => path.canonicalize().is_ok_and(|path| path == *file),
            None => path.is_dir(),
        };

        if found { Ok(path) } else { Err(missing()) }
    }

    /// A place in a file as it's shown in a message: in the directory that the project is
    /// compiled from, or if it isn't compiled from one, next to the file that the query is about.
    fn place(&self, file: &File, offset: TextSize) -> String {
        let directory = match (&self.root, &self.source.kind) {
            (Some(root), _) => root.as_path(),
            (None, SourceKind::File(path)) => Path::new(path).parent().unwrap_or(Path::new("")),
            (None, SourceKind::Std(_)) => Path::new(""),
        };

        format!(
            "{}:{}",
            file.source.kind.display(directory),
            file.source.line_col(offset.into())
        )
    }

    fn plan(&self, index: usize) -> Result<Option<Plan<'_>>, String> {
        let Some((target, span)) = self
            .items_at(index)
            .into_iter()
            .find_map(|item| Some((self.classify(&item.kind)?.0, item.span)))
        else {
            return Ok(None);
        };

        let Some(name) = self.source.text.get(Range::<usize>::from(span)) else {
            return Ok(None);
        };

        // `super` and `project` refer to a module without naming it
        if !is_identifier(name) {
            return Err(format!("`{name}` is a keyword, so it can't be renamed"));
        }

        let mut sites = Vec::new();
        let mut declared = false;

        for item in self.syntax_map.items() {
            let Some((_, role)) = self
                .classify(&item.kind)
                .filter(|(other, _)| *other == target)
            else {
                continue;
            };

            let Some(file) = self.tree.find(&item.source_kind) else {
                return Err(format!(
                    "`{name}` is part of the standard library, so it can't be renamed"
                ));
            };

            // A file that's a module is renamed itself, rather than a name in it
            if role == Role::FileModule {
                continue;
            }

            // Only the places that are written with the name are changed, which leaves out a
            // `super` or `project` that refers to a module
            let Some(token) = name_token(file, item.span, name) else {
                continue;
            };

            declared |= role == Role::Declaration;

            sites.push(Site { file, role, token });
        }

        // A file that's in the project more than once, through a symbolic link, is compiled as
        // a module for each, and only one of them has the name that's renamed
        let files = self.tree.all_files();

        if sites.iter().any(|site| {
            files
                .iter()
                .filter(|file| file.source.kind == site.file.source.kind)
                .count()
                > 1
        }) {
            return Err(format!(
                "`{name}` is used in a file that's in the project more than once, through a \
                symbolic link, so it can't be renamed"
            ));
        }

        let module = match target {
            Target::Symbol(symbol) if matches!(self.ctx.symbol(symbol), Symbol::Module(_)) => {
                module_path(&self.tree, symbol, None, &mut Vec::new())
            }
            _ => None,
        };

        // The project is compiled from its directory or file, and built from the `main` file in
        // its directory
        if let Some(module) = &module
            && (module.parent.is_none()
                || (module.parent == Some(self.tree.module())
                    && module.file.is_some()
                    && name == "main"))
        {
            return Err(format!(
                "`{name}` is the project's entrypoint, so it can't be renamed"
            ));
        }

        if !declared && module.is_none() {
            return Err(format!(
                "`{name}` isn't declared in the project, so it can't be renamed"
            ));
        }

        Ok(Some(Plan {
            target,
            name: name.to_string(),
            span,
            sites,
            module,
        }))
    }

    /// Checks that renaming wouldn't declare a name twice, or change what a name refers to.
    fn check_conflicts(&self, plan: &Plan<'_>, new_name: &str) -> Result<(), String> {
        let old_name = plan.name.as_str();

        if let Target::Field(owner, _) = plan.target {
            if let Type::Struct(ty) = self.ctx.ty(owner)
                && ty.fields.contains(new_name)
            {
                return Err(format!("There's already a field named `{new_name}`"));
            }

            // Fields aren't looked up in scopes
            return Ok(());
        }

        // The other enum variants are declared next to the name, in a scope of their own that's
        // only found from here. The other generic parameters are found by the scopes below too,
        // and are only checked here to say what's in the way
        for site in plan
            .sites
            .iter()
            .filter(|site| site.role == Role::Declaration)
        {
            let Some(parent) = site.token.parent() else {
                continue;
            };

            if AstGenericParameters::cast(parent.clone())
                .is_some_and(|generics| generics.names().any(|name| name.text() == new_name))
            {
                return Err(format!(
                    "There's already a generic parameter named `{new_name}`"
                ));
            }

            if parent.kind() == SyntaxKind::EnumVariant
                && parent
                    .parent()
                    .and_then(AstEnumItem::cast)
                    .is_some_and(|item| {
                        item.variants()
                            .filter_map(|variant| variant.name())
                            .any(|name| name.text() == new_name)
                    })
            {
                return Err(format!("There's already a variant named `{new_name}`"));
            }
        }

        let namespaces = self.namespaces(plan.target);

        let has_old = |scope: &Scope| {
            namespaces.iter().any(|&namespace| {
                scope
                    .item(namespace, old_name)
                    .is_some_and(|item| self.item_target(item) == plan.target)
            })
        };

        let has_new = |scope: &Scope, namespaces: &[Namespace]| {
            namespaces
                .iter()
                .any(|&namespace| scope.item(namespace, new_name).is_some())
        };

        // The scopes that the name is declared in or imported into can't have the new name too
        let mut scopes = self
            .syntax_map
            .items()
            .filter_map(|item| match item.kind {
                SyntaxItemKind::Scope(scope) => Some(scope),
                _ => None,
            })
            .collect::<HashSet<ScopeId>>();

        // A directory has no file that its scope would be recorded for
        if let Some(parent) = plan.module.as_ref().and_then(|module| module.parent) {
            scopes.insert(self.ctx.module(parent).scope);
        }

        if scopes.into_iter().any(|scope| {
            let scope = self.ctx.scope(scope);
            has_old(scope) && has_new(scope, &namespaces)
        }) {
            return Err(format!(
                "`{new_name}` is already used in a scope that has `{old_name}`"
            ));
        }

        let conflict = |file: &File, span: TextRange| {
            format!(
                "Renaming `{old_name}` to `{new_name}` would change what `{new_name}` refers to \
                at {}",
                self.place(file, span.start())
            )
        };

        // A reference that's looked up by its name alone finds the innermost declaration with
        // that name, which has to stay the one that's renamed
        for site in &plan.sites {
            if site.role != Role::Reference || !is_unqualified(&site.token) {
                continue;
            }

            let scopes = self
                .scopes_in(
                    &site.file.source.kind,
                    site.token.text_range().start().into(),
                )
                .0;

            let declared = scopes
                .iter()
                .position(|&scope| has_old(self.ctx.scope(scope)))
                .map_or(scopes.len(), |depth| depth + 1);

            if scopes[..declared]
                .iter()
                .any(|&scope| has_new(self.ctx.scope(scope), &namespaces))
            {
                return Err(conflict(site.file, site.token.text_range()));
            }
        }

        // And a reference to something else with the new name has to keep finding it, rather than
        // what's renamed
        for item in self.syntax_map.items() {
            let Some((other, Role::Reference)) = self.classify(&item.kind) else {
                continue;
            };

            if other == plan.target {
                continue;
            }

            let shared = self
                .namespaces(other)
                .into_iter()
                .filter(|namespace| namespaces.contains(namespace))
                .collect::<Vec<_>>();

            if shared.is_empty() {
                continue;
            }

            let Some(file) = self.tree.find(&item.source_kind) else {
                continue;
            };

            if !name_token(file, item.span, new_name).is_some_and(|token| is_unqualified(&token)) {
                continue;
            }

            let scopes = self
                .scopes_in(&item.source_kind, item.span.start().into())
                .0;

            let Some(renamed) = scopes
                .iter()
                .position(|&scope| has_old(self.ctx.scope(scope)))
            else {
                continue;
            };

            let found = scopes
                .iter()
                .position(|&scope| has_new(self.ctx.scope(scope), &shared));

            if found.is_none_or(|found| renamed <= found) {
                return Err(conflict(file, item.span));
            }
        }

        Ok(())
    }

    /// The namespaces that a name for the target is looked up in. An enum is also the namespace of
    /// its variants, and a variant without fields is also a constant.
    fn namespaces(&self, target: Target<'_>) -> Vec<Namespace> {
        match target {
            Target::Symbol(symbol) => match self.ctx.symbol(symbol) {
                Symbol::Module(_) => vec![Namespace::Module],
                _ => vec![Namespace::Value],
            },
            Target::Type(ty) => {
                let mut namespaces = vec![Namespace::Type];

                if self.ctx.enum_variants(ty).is_some() {
                    namespaces.push(Namespace::Module);
                }

                if self.ctx.is_unit_variant(ty) {
                    namespaces.push(Namespace::Value);
                }

                namespaces
            }
            Target::Field(..) => Vec::new(),
        }
    }

    /// What a reference to an item in a scope is about.
    fn item_target(&self, item: Item) -> Target<'static> {
        match self.ctx.reference_target(Declaration::from(item)) {
            Declaration::Symbol(symbol) => Target::Symbol(symbol),
            Declaration::Type(ty) => Target::Type(ty),
        }
    }
}

/// The token of a file that's the name at a span. The text is compared first, since it rules out
/// nearly every span without looking for its token.
fn name_token(file: &File, span: TextRange, name: &str) -> Option<SyntaxToken> {
    if file.source.text.get(Range::<usize>::from(span)) != Some(name) {
        return None;
    }

    file.document
        .syntax()
        .token_at_offset(span.start())
        .right_biased()
        .filter(|token| token.text_range() == span)
}

/// Checks that a name can be given to a declaration.
fn check_name(name: &str) -> Result<(), String> {
    if name == "_" || !is_identifier(name) {
        return Err(format!("`{name}` isn't a valid name"));
    }

    Ok(())
}

/// Whether the text is one identifier, rather than a keyword or anything else.
pub(super) fn is_identifier(text: &str) -> bool {
    let mut tokens = Lexer::new(text);

    matches!(tokens.next(), Some(token) if token.kind == TokenKind::Ident && token.span.len() == text.len())
}

/// Whether the name is a field in a struct initializer or a struct binding that's written without
/// a value or a binding of its own, such as `x` in `Point { x }` or `let { x } = point;`. It's
/// then both the field and the binding with the same name.
fn shorthand_field(token: &SyntaxToken) -> bool {
    let Some(parent) = token.parent() else {
        return false;
    };

    if let Some(field) = AstStructInitializerField::cast(parent.clone()) {
        return field.expr().is_none();
    }

    AstStructFieldBinding::cast(parent)
        .is_some_and(|field| field.binding().is_none() && field.spread().is_none())
}

/// Whether the name is looked up in the scopes around it, rather than in the module that the path
/// before it names.
fn is_unqualified(token: &SyntaxToken) -> bool {
    let Some(parent) = token.parent() else {
        return false;
    };

    let first = parent
        .prev_sibling()
        .is_none_or(|sibling| sibling.kind() != parent.kind());

    match parent.kind() {
        SyntaxKind::PathSegment => first,
        // The paths in the braces of an import continue the path before them
        SyntaxKind::ImportPathSegment => {
            first
                && parent
                    .parent()
                    .and_then(|path| path.parent())
                    .is_some_and(|item| item.kind() == SyntaxKind::ImportItem)
        }
        SyntaxKind::StructInitializerField => true,
        _ => false,
    }
}

/// A module that's a file or a directory of the project, given the directory that the tree is in
/// and the entries that lead to that directory.
fn module_path(
    tree: &FileTree,
    module: SymbolId,
    parent: Option<&FileTree>,
    entries: &mut Vec<String>,
) -> Option<ModulePath> {
    if tree.module() == module {
        return Some(ModulePath {
            entries: entries.clone(),
            file: match tree {
                FileTree::File(file) => match &file.source.kind {
                    SourceKind::File(path) => Some(PathBuf::from(path)),
                    SourceKind::Std(_) => return None,
                },
                FileTree::Directory(_) => None,
            },
            parent: parent.map(FileTree::module),
        });
    }

    let FileTree::Directory(directory) = tree else {
        return None;
    };

    directory.children.iter().find_map(|child| {
        entries.push(match child {
            FileTree::File(file) => format!("{}.rue", file.name),
            FileTree::Directory(directory) => directory.name.clone(),
        });

        let found = module_path(child, module, Some(tree), entries);
        entries.pop();
        found
    })
}

/// Adds each file of the tree, with the entries that lead to it from the directory that the
/// tree is in.
fn file_entries<'a>(
    tree: &'a FileTree,
    entries: &mut PathBuf,
    files: &mut Vec<(PathBuf, &'a File)>,
) {
    match tree {
        FileTree::File(file) => files.push((entries.clone(), file)),
        FileTree::Directory(directory) => {
            for child in &directory.children {
                entries.push(match child {
                    FileTree::File(file) => format!("{}.rue", file.name),
                    FileTree::Directory(directory) => directory.name.clone(),
                });

                file_entries(child, entries, files);
                entries.pop();
            }
        }
    }
}
