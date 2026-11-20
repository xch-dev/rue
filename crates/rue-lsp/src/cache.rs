use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use indexmap::IndexMap;
use rowan::{TextRange, TextSize, TokenAtOffset};
use rue_ast::AstNode;
use rue_compiler::{
    Compiler, CompletionContext, FileTree, SyntaxField, SyntaxItem, SyntaxItemKind, SyntaxMap,
};
use rue_diagnostic::{Source, SourceKind};
use rue_hir::{Declaration, FunctionKind, Item, Namespace, ScopeId, Symbol, SymbolId};
use rue_lexer::{Lexer, TokenKind};
use rue_parser::{SyntaxKind, SyntaxToken, T};
use rue_types::{DISCRIMINANT_FIELD, Type, TypeId, Union};
use tower_lsp_server::ls_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, Location, Position,
};

mod rename;
mod signature;

pub use rename::Rename;
pub use signature::Signatures;

use crate::paths::path_to_uri;
use crate::position::{position_to_offset, source_range};

#[derive(Debug, Clone)]
pub struct Scopes(Vec<ScopeId>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HoverInfo {
    Symbol(SymbolHoverInfo),
    Module(ModuleHoverInfo),
    Type(TypeHoverInfo),
    Struct(StructHoverInfo),
    Enum(EnumHoverInfo),
    Variant(VariantHoverInfo),
    Field(FieldHoverInfo),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolHoverInfo {
    pub name: String,
    pub kind: SymbolHoverKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolHoverKind {
    Binding {
        type_name: String,
    },
    Parameter {
        type_name: String,
    },
    Constant {
        type_name: String,
        inline: bool,
    },
    Function {
        signatures: Vec<String>,
        inline: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleHoverInfo {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeHoverInfo {
    pub name: String,
    pub inner_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructHoverInfo {
    pub name: String,
    pub fields: Vec<FieldHoverInfo>,
    pub nil_terminated: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumHoverInfo {
    pub name: String,
    /// Each variant, as it would be declared in the enum.
    pub variants: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantHoverInfo {
    /// The variant's path, such as `Json::Bool`.
    pub name: String,
    /// The variant as it would be declared in its enum, without its name.
    pub declaration: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldHoverInfo {
    pub name: String,
    pub type_name: String,
}

/// Each field as it's declared, with a spread before the last one if the struct isn't nil
/// terminated.
pub fn field_declarations(fields: &[FieldHoverInfo], nil_terminated: bool) -> Vec<String> {
    fields
        .iter()
        .enumerate()
        .map(|(i, field)| {
            let spread = if !nil_terminated && i == fields.len() - 1 {
                "..."
            } else {
                ""
            };
            format!("{spread}{}: {}", field.name, field.type_name)
        })
        .collect()
}

/// Something that can be declared and referenced, which is what navigation queries are about.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Target<'a> {
    Symbol(SymbolId),
    Type(TypeId),
    /// A field, by the type that contains it and its name.
    Field(TypeId, &'a str),
}

/// Whether an item declares or refers to its target.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Role {
    Declaration,
    /// A file that declares a module, whose span is the whole file.
    FileModule,
    Reference,
}

/// A project's compile, which answers queries about one of its files at a time. Queries can
/// allocate types, so they need the compiler mutably, and they run on the thread that owns it.
#[derive(Debug)]
pub struct Cache<T> {
    ctx: T,
    tree: FileTree,
    source: Source,
    syntax_map: SyntaxMap,
    /// The struct that declares the fields of each type that a field was used through, such as
    /// `Wrap` for a `Wrap<Int>`, so that every use of a field is about the same declaration.
    field_owners: HashMap<TypeId, TypeId>,
    /// The directory that the project is compiled from, if it's compiled from one, without any
    /// symbolic links in its path.
    root: Option<PathBuf>,
}

impl Cache<Compiler> {
    pub fn new(mut ctx: Compiler, tree: FileTree, source: Source, entrypoint: &Path) -> Self {
        let syntax_map = ctx.syntax_map().clone();

        let mut field_owners = HashMap::new();

        for item in syntax_map.items() {
            let (SyntaxItemKind::FieldDeclaration(field)
            | SyntaxItemKind::FieldReference(field)
            | SyntaxItemKind::FieldInitializer(field)) = &item.kind
            else {
                continue;
            };

            // A container that isn't a struct, such as a pair, is its own owner, which is also
            // remembered so that it's only worked out once
            if let Entry::Vacant(entry) = field_owners.entry(field.container) {
                entry.insert(field_owner(&mut ctx, field.container).unwrap_or(field.container));
            }
        }

        Self {
            ctx,
            tree,
            source,
            syntax_map,
            field_owners,
            root: entrypoint.canonicalize().ok().filter(|root| root.is_dir()),
        }
    }

    /// What an item is about and its role, for the items that navigation queries can go to.
    fn classify<'a>(&self, kind: &'a SyntaxItemKind) -> Option<(Target<'a>, Role)> {
        let field = |field: &'a SyntaxField| {
            let owner = self.field_owners.get(&field.container);
            Target::Field(*owner.unwrap_or(&field.container), &field.name)
        };

        match kind {
            SyntaxItemKind::SymbolDeclaration(symbol) => {
                Some((Target::Symbol(*symbol), Role::Declaration))
            }
            SyntaxItemKind::FileModule(symbol) => Some((Target::Symbol(*symbol), Role::FileModule)),
            SyntaxItemKind::SymbolReference(symbol) => {
                Some((Target::Symbol(*symbol), Role::Reference))
            }
            SyntaxItemKind::TypeDeclaration(ty) => Some((Target::Type(*ty), Role::Declaration)),
            SyntaxItemKind::TypeReference(ty) => Some((Target::Type(*ty), Role::Reference)),
            SyntaxItemKind::FieldDeclaration(declared) => {
                Some((field(declared), Role::Declaration))
            }
            SyntaxItemKind::FieldReference(used) | SyntaxItemKind::FieldInitializer(used) => {
                Some((field(used), Role::Reference))
            }
            SyntaxItemKind::Call(_)
            | SyntaxItemKind::Scope(_)
            | SyntaxItemKind::CompletionContext(_) => None,
        }
    }

    /// Sets the file that queries are about.
    pub fn set_source(&mut self, source: Source) {
        self.source = source;
    }

    pub fn position(&self, position: Position) -> usize {
        position_to_offset(
            self.source.line_index_with_carriage_returns(),
            &self.source.text,
            position,
        )
    }

    /// The scopes that are visible at the index, innermost first, so that a name that's shadowed
    /// resolves to the innermost declaration: the innermost scope that contains it, and the scopes
    /// it's in. An inline module doesn't see the scopes of the module it's in, though its text is
    /// inside of them.
    pub fn scopes(&self, index: usize) -> Scopes {
        self.scopes_in(&self.source.kind, index)
    }

    /// The scopes that are visible at the index of a file, like [`Self::scopes`].
    fn scopes_in(&self, kind: &SourceKind, index: usize) -> Scopes {
        // Every scope that contains the index is nested in or equal to the others, so the narrower
        // one is the inner one. Of scopes with the same span, the first one exited (which is added
        // to the syntax map first) is the innermost.
        let innermost = self
            .syntax_map
            .items()
            .filter(|item| item.source_kind == *kind && contains(item.span, index))
            .filter_map(|item| match item.kind {
                SyntaxItemKind::Scope(scope) => Some((item.span.len(), scope)),
                _ => None,
            })
            .min_by_key(|&(len, _)| len)
            .map(|(_, scope)| scope);

        let mut visible = Vec::new();
        let mut current = innermost;

        while let Some(scope) = current {
            visible.push(scope);
            current = self.ctx.scope(scope).parent();
        }

        Scopes(visible)
    }

    fn completion_context(&self, index: usize) -> (CompletionContext, CompletionContext) {
        let mut first = CompletionContext::Document;
        let mut second = CompletionContext::Document;
        let mut has_first = false;

        for item in self
            .syntax_map
            .items()
            .filter(|item| item.source_kind == self.source.kind)
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
        {
            let SyntaxItemKind::CompletionContext(context) = &item.kind else {
                continue;
            };

            if contains(item.span, index) {
                if has_first {
                    second = context.clone();
                    break;
                }

                first = context.clone();
                has_first = true;
            }
        }

        // When a type or expression is missing entirely, there's no node to provide a completion
        // context, so it's worked out from the token before the cursor.
        match self.expected_context(index) {
            Some(CompletionContext::Type) if !matches!(first, CompletionContext::Type) => {
                (CompletionContext::Type, first)
            }
            Some(CompletionContext::Expression)
                if matches!(
                    first,
                    CompletionContext::Document
                        | CompletionContext::Item
                        | CompletionContext::Statement
                ) =>
            {
                (CompletionContext::Expression, first)
            }
            _ => (first, second),
        }
    }

    /// The tokens that touch the offset, of which there are two if it's between them.
    fn tokens_at(&self, offset: TextSize) -> TokenAtOffset<SyntaxToken> {
        self.tree
            .find(&self.source.kind)
            .map_or(TokenAtOffset::None, |file| {
                file.document.syntax().token_at_offset(offset)
            })
    }

    /// The context that the token before the cursor must be followed by, if it's a type or an
    /// expression.
    fn expected_context(&self, index: usize) -> Option<CompletionContext> {
        let offset = TextSize::from(index as u32);

        let mut token = match self.tokens_at(offset) {
            TokenAtOffset::None => return None,
            TokenAtOffset::Single(token) | TokenAtOffset::Between(token, _) => token,
        };

        while token.kind().is_trivia() || token.text_range().end() > offset {
            token = token.prev_token()?;
        }

        let parent = token.parent().map(|parent| parent.kind());

        match token.kind() {
            T![->] | T![as] | T![is] => Some(CompletionContext::Type),
            T![:]
                if matches!(
                    parent,
                    Some(
                        SyntaxKind::FunctionParameter
                            | SyntaxKind::LambdaParameter
                            | SyntaxKind::LambdaExpr
                            | SyntaxKind::ConstantItem
                            | SyntaxKind::LetStmt
                            | SyntaxKind::StructField
                    )
                ) =>
            {
                Some(CompletionContext::Type)
            }
            T![=] if parent == Some(SyntaxKind::TypeAliasItem) => Some(CompletionContext::Type),
            T![=] if matches!(parent, Some(SyntaxKind::LetStmt | SyntaxKind::ConstantItem)) => {
                Some(CompletionContext::Expression)
            }
            // A match arm starts with its pattern, which is a type, and ends with its value
            T!['{'] | T![,] if parent == Some(SyntaxKind::MatchExpr) => {
                Some(CompletionContext::Type)
            }
            T![=>] if parent == Some(SyntaxKind::MatchArm) => Some(CompletionContext::Expression),
            T![return] | T![assert] | T![raise] | T![if] | T![match] => {
                Some(CompletionContext::Expression)
            }
            // The message of an `assert` comes after its condition
            T![,] if parent == Some(SyntaxKind::AssertStmt) => Some(CompletionContext::Expression),
            _ => None,
        }
    }

    /// Whether the index is directly after the `..` of a slice (`value[start..end]`).
    fn after_range(&self, index: usize) -> bool {
        let offset = TextSize::from(index as u32);

        self.tokens_at(offset)
            .any(|token| token.kind() == T![..] && token.text_range().end() == offset)
    }

    /// Whether the index is inside a string or a comment, where names aren't completed. The end
    /// of a line comment, or of a string or block comment that isn't terminated, is still inside
    /// of it.
    fn in_string_or_comment(&self, index: usize) -> bool {
        let offset = TextSize::from(index as u32);

        self.tokens_at(offset).any(|token| {
            if !matches!(
                token.kind(),
                SyntaxKind::String | SyntaxKind::LineComment | SyntaxKind::BlockComment
            ) {
                return false;
            }

            // The parser doesn't keep whether a token is terminated, so it's lexed again. A line
            // comment includes the line break, so the start of the next line isn't inside of it.
            let terminated = match Lexer::new(token.text()).next().map(|token| token.kind) {
                Some(
                    TokenKind::String { is_terminated } | TokenKind::BlockComment { is_terminated },
                ) => is_terminated,
                Some(TokenKind::LineComment) => token.text().ends_with('\n'),
                _ => false,
            };

            let range = token.text_range();

            range.start() < offset && (offset < range.end() || !terminated)
        })
    }

    fn partial_identifier(&self, index: usize) -> Option<String> {
        let mut source = self.source.text[..index].to_string();
        let mut ident = String::new();

        while let Some(c) = source.pop() {
            if matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_') {
                ident.insert(0, c);
            } else {
                break;
            }
        }

        if ident.chars().next()?.is_ascii_digit() {
            return None;
        }

        Some(ident)
    }

    pub fn completions(&mut self, scopes: &Scopes, index: usize) -> Vec<CompletionItem> {
        if self.in_string_or_comment(index) {
            return vec![];
        }

        let (context, subcontext) = self.completion_context(index);

        let before = self.source.text.get(..index).unwrap_or_default();

        if before.ends_with(':')
            && (!before.ends_with("::")
                || !matches!(context, CompletionContext::ModuleExports { .. }))
        {
            return vec![];
        }

        // A `.` is followed by a field, unless it's the end of the `..` of a slice, which is
        // followed by an expression
        if before.ends_with('.')
            && !matches!(context, CompletionContext::StructFields { .. })
            && !self.after_range(index)
        {
            return vec![];
        }

        let partial = self.partial_identifier(index).unwrap_or_default();

        let mut scored_items = Vec::new();
        // Values and modules are both symbols, but are in different namespaces, so a value and
        // a module can have the same name
        let mut symbol_names = HashSet::new();
        let mut type_names = HashSet::new();

        if let CompletionContext::StructFields {
            ty,
            specified_fields,
        } = context.clone()
        {
            let semantic = rue_types::unwrap_semantic(self.ctx.types_mut(), ty, true);

            let mut fields = IndexMap::new();

            match self.ctx.ty(semantic) {
                Type::Struct(_) => {
                    for field in self
                        .struct_fields(scopes, semantic)
                        .into_iter()
                        .flat_map(|(fields, _)| fields)
                    {
                        fields.insert(field.name, field.type_name);
                    }
                }
                Type::Atom(_) => {
                    let ty = self.ctx.builtins().types.int;
                    let type_name = self.type_name(scopes, ty);
                    fields.insert("length".to_string(), type_name);
                }
                Type::Pair(_) | Type::Union(_) => {
                    let pairs = rue_types::extract_pairs(self.ctx.types_mut(), ty, true);

                    if !pairs.is_empty() {
                        let first = if pairs.len() == 1 {
                            pairs[0].first
                        } else {
                            self.ctx.alloc_type(Type::Union(Union::new(
                                pairs.iter().map(|pair| pair.first).collect(),
                            )))
                        };

                        let rest = if pairs.len() == 1 {
                            pairs[0].rest
                        } else {
                            self.ctx.alloc_type(Type::Union(Union::new(
                                pairs.iter().map(|pair| pair.rest).collect(),
                            )))
                        };

                        let first = self.type_name(scopes, first);
                        let rest = self.type_name(scopes, rest);

                        fields.insert("first".to_string(), first);
                        fields.insert("rest".to_string(), rest);
                    }
                }
                _ => {}
            }

            for (field, field_type) in fields {
                if specified_fields
                    .as_ref()
                    .is_some_and(|fields| fields.contains(&field))
                {
                    continue;
                }

                if let Some(score) = fuzzy_match(&partial, &field) {
                    scored_items.push((
                        score,
                        create_completion_item(
                            field,
                            Some(CompletionItemKind::FIELD),
                            Some(field_type),
                        ),
                    ));
                }
            }
        }

        if let CompletionContext::ModuleExports {
            module,
            allow_super,
            members,
        } = context.clone()
        {
            if allow_super && let Some(score) = fuzzy_match(&partial, "super") {
                scored_items.push((
                    score,
                    create_completion_item(
                        "super".to_string(),
                        Some(CompletionItemKind::MODULE),
                        None,
                    ),
                ));
            }

            let scope = self.ctx.module(module).scope;

            // A module that `super` or `project` names contains the code, so its private items and
            // a file's sibling modules can be used too.
            // A failed import's name isn't anything that could be used
            let items = |namespace| {
                let mut items = if members {
                    self.ctx.module_members(scope, namespace)
                } else {
                    self.ctx.scope(scope).exported_items(namespace)
                };

                items.retain(|&(_, item)| !self.ctx.is_error_item(item));
                items
            };

            let symbols = [Namespace::Value, Namespace::Module]
                .into_iter()
                .flat_map(items)
                .filter_map(|(name, item)| symbol_item(name, item))
                .collect::<Vec<_>>();

            let types = items(Namespace::Type)
                .into_iter()
                .filter_map(|(name, item)| Some((name, item.ty()?)))
                .collect::<Vec<_>>();

            for (name, symbol, is_module) in symbols {
                if !symbol_names.insert((name.clone(), is_module)) {
                    continue;
                }

                if let Some(score) = fuzzy_match(&partial, &name)
                    && self.completes_symbol(&subcontext, symbol, is_module)
                {
                    // A module has no type to show
                    let detail = (!is_module).then(|| {
                        let ty = self.ctx.symbol_type_in(scope, symbol);
                        self.symbol_detail(scopes, symbol, ty)
                    });
                    let kind = self.symbol_kind(symbol);
                    scored_items.push((score, create_completion_item(name, kind, detail)));
                }
            }

            for (name, ty) in types {
                if !type_names.insert(name.clone()) {
                    continue;
                }

                if !self.completes_type(&subcontext, ty, &name, &symbol_names) {
                    continue;
                }

                if let Some(score) = fuzzy_match(&partial, &name) {
                    let type_name = self.inner_type(ty).map(|ty| self.type_name(scopes, ty));
                    let kind = self.type_kind(ty);

                    scored_items.push((score, create_completion_item(name, Some(kind), type_name)));
                }
            }
        }

        if matches!(
            context,
            CompletionContext::Expression | CompletionContext::Type
        ) {
            for keyword in ["super", "project"] {
                if let Some(score) = fuzzy_match(&partial, keyword) {
                    scored_items.push((
                        score,
                        create_completion_item(
                            keyword.to_string(),
                            Some(CompletionItemKind::MODULE),
                            None,
                        ),
                    ));
                }
            }
        }

        for scope in &scopes.0 {
            let scope = self.ctx.scope(*scope);

            // A failed import's name isn't anything that could be used
            let symbols = [Namespace::Value, Namespace::Module]
                .into_iter()
                .flat_map(|namespace| scope.items(namespace))
                .filter(|&(_, item)| !self.ctx.is_error_item(item))
                .filter_map(|(name, item)| symbol_item(name, item))
                .collect::<Vec<_>>();

            let types = scope
                .type_names()
                .filter_map(|name| Some((name.to_string(), scope.ty(name)?)))
                .filter(|&(_, ty)| !self.ctx.is_error_item(Item::Type(ty)))
                .collect::<Vec<_>>();

            for (name, symbol, is_module) in symbols {
                if !symbol_names.insert((name.clone(), is_module)) {
                    continue;
                }

                if let Some(score) = fuzzy_match(&partial, &name)
                    && self.completes_symbol(&context, symbol, is_module)
                {
                    // The type is looked up from the cursor's scopes, like on hover, so that it's
                    // narrowed by the conditions the cursor is inside of
                    let detail = (!is_module).then(|| {
                        let ty = self.symbol_type(scopes, symbol);
                        self.symbol_detail(scopes, symbol, ty)
                    });
                    let kind = self.symbol_kind(symbol);
                    scored_items.push((score, create_completion_item(name, kind, detail)));
                }
            }

            for (name, ty) in types {
                if !type_names.insert(name.clone()) {
                    continue;
                }

                if !self.completes_type(&context, ty, &name, &symbol_names) {
                    continue;
                }

                if let Some(score) = fuzzy_match(&partial, &name) {
                    let type_name = self.inner_type(ty).map(|ty| self.type_name(scopes, ty));
                    let kind = self.type_kind(ty);

                    scored_items.push((score, create_completion_item(name, Some(kind), type_name)));
                }
            }
        }

        // Sort by score in descending order
        scored_items.sort_by(|(a, _), (b, _)| b.cmp(a));

        // Return just the completion items
        scored_items.into_iter().map(|(_, item)| item).collect()
    }

    pub fn hover(&mut self, scopes: &Scopes, index: usize) -> Vec<HoverInfo> {
        let mut infos = Vec::new();

        let items = self
            .items_at(index)
            .into_iter()
            .map(|item| item.kind.clone())
            .collect::<Vec<_>>();

        for kind in items {
            match kind {
                SyntaxItemKind::SymbolDeclaration(symbol)
                | SyntaxItemKind::SymbolReference(symbol) => {
                    let Some(name) = self.symbol_name(scopes, symbol) else {
                        continue;
                    };

                    if let Symbol::Module(_) = self.ctx.symbol(symbol) {
                        infos.push(HoverInfo::Module(ModuleHoverInfo { name }));
                    } else {
                        let kind = self.symbol_hover_kind(scopes, symbol);
                        infos.push(HoverInfo::Symbol(SymbolHoverInfo { name, kind }));
                    }
                }
                SyntaxItemKind::TypeDeclaration(ty) | SyntaxItemKind::TypeReference(ty) => {
                    if self.ctx.enum_variants(ty).is_some() {
                        let info = self.enum_hover(scopes, ty);
                        infos.push(HoverInfo::Enum(info));
                        continue;
                    }

                    if let Some(info) = self.variant_hover(scopes, ty) {
                        infos.push(HoverInfo::Variant(info));
                        continue;
                    }

                    let fields = self.struct_fields(scopes, ty);

                    if let Some((fields, nil_terminated)) = fields {
                        infos.push(HoverInfo::Struct(StructHoverInfo {
                            name: self.declared_type_name(scopes, ty),
                            fields,
                            nil_terminated,
                        }));
                    } else {
                        infos.push(HoverInfo::Type(TypeHoverInfo {
                            name: self.declared_type_name(scopes, ty),
                            inner_name: self.inner_type(ty).map(|ty| self.type_name(scopes, ty)),
                        }));
                    }
                }
                SyntaxItemKind::FieldDeclaration(field)
                | SyntaxItemKind::FieldReference(field)
                | SyntaxItemKind::FieldInitializer(field) => {
                    infos.push(HoverInfo::Field(FieldHoverInfo {
                        name: field.name,
                        type_name: self.type_name(scopes, field.ty),
                    }));
                }
                SyntaxItemKind::Call(_)
                | SyntaxItemKind::Scope(_)
                | SyntaxItemKind::CompletionContext(_)
                | SyntaxItemKind::FileModule(_) => {}
            }
        }

        // The same item can be added more than once, such as when code is compiled for each place
        // it's used, so it's only shown once
        let mut unique = Vec::new();

        for info in infos {
            if !unique.contains(&info) {
                unique.push(info);
            }
        }

        unique
    }

    /// Going to the definition of a reference lists its declarations, and going to the definition
    /// of a declaration lists its references, so that either can be reached from the other.
    pub fn definitions(&self, index: usize) -> Vec<Location> {
        let Some((target, role)) = self.target_at(index) else {
            return Vec::new();
        };

        let roles: &[Role] = match role {
            Role::Reference => &[Role::Declaration, Role::FileModule],
            Role::Declaration | Role::FileModule => &[Role::Reference],
        };

        self.locations(target, roles)
    }

    pub fn references(&self, index: usize, include_declaration: bool) -> Vec<Location> {
        let Some((target, _)) = self.target_at(index) else {
            return Vec::new();
        };

        // A file module's declaration is the whole file, which isn't a useful reference
        let roles: &[Role] = if include_declaration {
            &[Role::Reference, Role::Declaration]
        } else {
            &[Role::Reference]
        };

        self.locations(target, roles)
    }

    /// The items at the index, in the order they were added. Several items can share a span, such
    /// as the binding and the field in the struct initializer `Point { x }`.
    fn items_at(&self, index: usize) -> Vec<&SyntaxItem> {
        let items = self
            .syntax_map
            .items()
            .filter(|item| {
                item.source_kind == self.source.kind
                    && contains(item.span, index)
                    && match item.kind {
                        SyntaxItemKind::SymbolDeclaration(_)
                        | SyntaxItemKind::SymbolReference(_)
                        | SyntaxItemKind::TypeDeclaration(_)
                        | SyntaxItemKind::TypeReference(_)
                        | SyntaxItemKind::FieldDeclaration(_)
                        | SyntaxItemKind::FieldReference(_)
                        | SyntaxItemKind::FieldInitializer(_) => true,
                        SyntaxItemKind::FileModule(_)
                        | SyntaxItemKind::Call(_)
                        | SyntaxItemKind::Scope(_)
                        | SyntaxItemKind::CompletionContext(_) => false,
                    }
            })
            .collect::<Vec<_>>();

        let Some(span) = best_span(items.iter().map(|item| item.span), index) else {
            return Vec::new();
        };

        items.into_iter().filter(|item| item.span == span).collect()
    }

    /// What the item at the index refers to, and whether it's a declaration or a reference.
    fn target_at(&self, index: usize) -> Option<(Target<'_>, Role)> {
        self.items_at(index)
            .into_iter()
            .find_map(|item| self.classify(&item.kind))
    }

    /// The locations of the items with the target and one of the roles, sorted by file and
    /// position and without duplicates. Items in the standard library are left out, since it has
    /// no file the editor can open.
    fn locations(&self, target: Target<'_>, roles: &[Role]) -> Vec<Location> {
        let mut locations = self
            .syntax_map
            .items()
            .filter(|item| {
                self.classify(&item.kind)
                    .is_some_and(|(other, role)| other == target && roles.contains(&role))
            })
            .filter_map(|item| {
                let SourceKind::File(path) = &item.source_kind else {
                    return None;
                };

                let source = &self.tree.find(&item.source_kind)?.source;
                let range = source_range(source, item.span.start().into(), item.span.end().into());

                Some(Location::new(path_to_uri(Path::new(path))?, range))
            })
            .collect::<Vec<_>>();

        locations.sort_by(|a, b| {
            (a.uri.as_str(), a.range.start, a.range.end).cmp(&(
                b.uri.as_str(),
                b.range.start,
                b.range.end,
            ))
        });
        locations.dedup();

        locations
    }

    fn type_name(&mut self, scopes: &Scopes, ty: TypeId) -> String {
        for &scope in &scopes.0 {
            if let Some(name) = self.ctx.scope(scope).type_name(ty) {
                return name.to_string();
            }
        }

        rue_types::stringify(self.ctx.types_mut(), ty)
    }

    /// The name of a type like [`Self::type_name`], but if it isn't named by anything in scope,
    /// only its first `parts` parts are written, and the rest are shown as `...`. A type that
    /// was inferred can be far too large to write in full.
    fn bounded_type_name(&mut self, scopes: &Scopes, ty: TypeId, parts: usize) -> String {
        for &scope in &scopes.0 {
            if let Some(name) = self.ctx.scope(scope).type_name(ty) {
                return name.to_string();
            }
        }

        rue_types::stringify_bounded(self.ctx.types_mut(), ty, parts)
    }

    /// The name of a type along with its generic parameters, e.g. `List<T>`.
    fn declared_type_name(&mut self, scopes: &Scopes, ty: TypeId) -> String {
        let name = self.type_name(scopes, ty);

        let generics = match self.ctx.ty(ty) {
            Type::Alias(alias) => alias.generics.clone(),
            Type::Struct(ty) => ty.generics.clone(),
            _ => Vec::new(),
        };

        if generics.is_empty() || name.contains('<') {
            return name;
        }

        let generics = generics
            .into_iter()
            .map(|generic| self.type_name(scopes, generic))
            .collect::<Vec<_>>();

        format!("{name}<{}>", generics.join(", "))
    }

    /// The detail of a completion, which for functions is their signature as shown on hover.
    fn symbol_detail(&mut self, scopes: &Scopes, symbol: SymbolId, ty: TypeId) -> String {
        match self.ctx.symbol(symbol).clone() {
            Symbol::Builtin(builtin) => format!("fn{}", builtin.signatures()[0]),
            Symbol::Function(function) => self
                .function_signature(scopes, &function.vars, ty)
                .map_or_else(
                    || self.type_name(scopes, ty),
                    |signature| format!("fn{signature}"),
                ),
            _ => self.type_name(scopes, ty),
        }
    }

    fn symbol_hover_kind(&mut self, scopes: &Scopes, symbol: SymbolId) -> SymbolHoverKind {
        match self.ctx.symbol(symbol).clone() {
            Symbol::Builtin(builtin) => SymbolHoverKind::Function {
                signatures: builtin
                    .signatures()
                    .iter()
                    .map(ToString::to_string)
                    .collect(),
                inline: false,
            },
            Symbol::Function(function) => {
                let ty = self.symbol_type(scopes, symbol);

                if let Some(signature) = self.function_signature(scopes, &function.vars, ty) {
                    SymbolHoverKind::Function {
                        signatures: vec![signature],
                        inline: function.kind == FunctionKind::Inline,
                    }
                } else {
                    SymbolHoverKind::Binding {
                        type_name: self.type_name(scopes, ty),
                    }
                }
            }
            Symbol::Constant(constant) => SymbolHoverKind::Constant {
                type_name: self.type_name(scopes, self.symbol_type(scopes, symbol)),
                inline: constant.inline,
            },
            Symbol::Parameter(_) => SymbolHoverKind::Parameter {
                type_name: self.type_name(scopes, self.symbol_type(scopes, symbol)),
            },
            Symbol::Binding(_) | Symbol::Module(_) | Symbol::Unresolved => {
                SymbolHoverKind::Binding {
                    type_name: self.type_name(scopes, self.symbol_type(scopes, symbol)),
                }
            }
        }
    }

    /// Renders a function's signature, without the `fn` keyword or name, e.g. `<T>(value: T) -> T`.
    fn function_signature(
        &mut self,
        scopes: &Scopes,
        generics: &[TypeId],
        mut ty: TypeId,
    ) -> Option<String> {
        while let Type::Ref(inner) = self.ctx.ty(ty) {
            ty = *inner;
        }

        let Type::Function(function) = self.ctx.ty(ty).clone() else {
            return None;
        };

        let generics = if generics.is_empty() {
            String::new()
        } else {
            let names = generics
                .iter()
                .map(|&generic| self.type_name(scopes, generic))
                .collect::<Vec<_>>();

            format!("<{}>", names.join(", "))
        };

        let len = function.params.len();

        let params = function
            .params
            .iter()
            .enumerate()
            .map(|(i, param)| {
                let spread = if !function.nil_terminated && i == len - 1 {
                    "..."
                } else {
                    ""
                };

                format!(
                    "{spread}{}: {}",
                    param.name,
                    self.type_name(scopes, param.ty)
                )
            })
            .collect::<Vec<_>>()
            .join(", ");

        let ret = self.type_name(scopes, function.ret);

        Some(format!("{generics}({params}) -> {ret}"))
    }

    fn symbol_name(&self, scopes: &Scopes, symbol: SymbolId) -> Option<String> {
        for &scope in &scopes.0 {
            let scope = self.ctx.scope(scope);

            if let Some(name) = scope
                .symbol_name(symbol)
                .or_else(|| scope.module_name(symbol))
            {
                return Some(name.to_string());
            }
        }

        self.ctx
            .symbol(symbol)
            .name()
            .map(|token| token.text().to_string())
    }

    fn symbol_type(&self, scopes: &Scopes, symbol: SymbolId) -> TypeId {
        for &scope in &scopes.0 {
            if let Some(ty) = self.ctx.scope(scope).symbol_override_type(symbol) {
                return ty;
            }
        }

        self.ctx.symbol_type(symbol)
    }

    fn inner_type(&self, ty: TypeId) -> Option<TypeId> {
        match self.ctx.ty(ty) {
            Type::Alias(ty) => Some(ty.inner),
            _ => None,
        }
    }

    /// Whether a value or module is completed in the context, which is a module in a type (for
    /// the path to a type in it), or anything in an expression. An enum's namespace isn't
    /// completed in a type, since the enum itself is.
    fn completes_symbol(
        &self,
        context: &CompletionContext,
        symbol: SymbolId,
        is_module: bool,
    ) -> bool {
        match context {
            CompletionContext::Expression => true,
            CompletionContext::Type => is_module && !self.is_enum_namespace(symbol),
            CompletionContext::Document
            | CompletionContext::Item
            | CompletionContext::Statement
            | CompletionContext::StructFields { .. }
            | CompletionContext::ModuleExports { .. } => false,
        }
    }

    /// Whether a type is completed in the context, which is any type in a type, or a struct in an
    /// expression (for a struct initializer). A variant without fields isn't completed in an
    /// expression if its constant, which has the same name, already is.
    fn completes_type(
        &mut self,
        context: &CompletionContext,
        ty: TypeId,
        name: &str,
        symbol_names: &HashSet<(String, bool)>,
    ) -> bool {
        match context {
            CompletionContext::Type => true,
            CompletionContext::Expression => {
                let semantic = rue_types::unwrap_semantic(self.ctx.types_mut(), ty, true);

                matches!(self.ctx.ty(semantic), Type::Struct(_))
                    && !(self.ctx.enum_of(Declaration::Type(ty)).is_some()
                        && symbol_names.contains(&(name.to_string(), false)))
            }
            CompletionContext::Document
            | CompletionContext::Item
            | CompletionContext::Statement
            | CompletionContext::StructFields { .. }
            | CompletionContext::ModuleExports { .. } => false,
        }
    }

    fn is_enum_namespace(&self, symbol: SymbolId) -> bool {
        matches!(self.ctx.symbol(symbol), Symbol::Module(_))
            && self.ctx.enum_of(Declaration::Symbol(symbol)).is_some()
    }

    fn symbol_kind(&self, symbol: SymbolId) -> Option<CompletionItemKind> {
        if self.is_enum_namespace(symbol) {
            Some(CompletionItemKind::ENUM)
        } else if self.ctx.enum_of(Declaration::Symbol(symbol)).is_some() {
            Some(CompletionItemKind::ENUM_MEMBER)
        } else {
            base_symbol_kind(self.ctx.symbol(symbol))
        }
    }

    fn type_kind(&self, ty: TypeId) -> CompletionItemKind {
        if self.ctx.enum_variants(ty).is_some() {
            CompletionItemKind::ENUM
        } else if self.ctx.enum_of(Declaration::Type(ty)).is_some() {
            CompletionItemKind::ENUM_MEMBER
        } else {
            CompletionItemKind::STRUCT
        }
    }

    fn enum_hover(&mut self, scopes: &Scopes, ty: TypeId) -> EnumHoverInfo {
        let name = self.declared_type_name(scopes, ty);

        let variants = self
            .ctx
            .enum_variants(ty)
            .unwrap_or_default()
            .to_vec()
            .into_iter()
            .filter_map(|variant| {
                let Type::Struct(ty) = self.ctx.ty(variant) else {
                    return None;
                };

                // The variant is named by its path, which ends with its own name
                let name = ty.name.as_ref()?.text();
                let name = name.rsplit("::").next().unwrap_or(name).to_string();

                Some(format!(
                    "{name}{}",
                    self.variant_declaration(scopes, variant, variant)
                ))
            })
            .collect();

        EnumHoverInfo { name, variants }
    }

    fn variant_hover(&mut self, scopes: &Scopes, ty: TypeId) -> Option<VariantHoverInfo> {
        let semantic = rue_types::unwrap_semantic(self.ctx.types_mut(), ty, true);

        let Type::Struct(variant) = self.ctx.ty(semantic) else {
            return None;
        };

        let variant = variant.semantic;

        self.ctx.enum_of(Declaration::Type(variant))?;

        Some(VariantHoverInfo {
            name: self.declared_type_name(scopes, ty),
            declaration: self.variant_declaration(scopes, ty, variant),
        })
    }

    /// A variant as it's declared in its enum, after its name, such as ` = 1 { value: Bool }`.
    /// The type is the variant, or an instance of it.
    fn variant_declaration(&mut self, scopes: &Scopes, ty: TypeId, variant: TypeId) -> String {
        let discriminant = if let Some(value) = self.ctx.default_field(variant, DISCRIMINANT_FIELD)
        {
            format!(
                " = {}",
                rue_types::stringify(self.ctx.types_mut(), value.ty)
            )
        } else {
            String::new()
        };

        let Some((fields, nil_terminated)) = self
            .struct_fields(scopes, ty)
            .filter(|(fields, _)| !fields.is_empty())
        else {
            // A variant without fields that's declared with braces is written with them
            return if self.ctx.is_unit_variant(variant) {
                discriminant
            } else {
                format!("{discriminant} {{}}")
            };
        };

        let fields = field_declarations(&fields, nil_terminated).join(", ");

        format!("{discriminant} {{ {fields} }}")
    }

    fn struct_fields(
        &mut self,
        scopes: &Scopes,
        ty: TypeId,
    ) -> Option<(Vec<FieldHoverInfo>, bool)> {
        let (fields, nil_terminated) = self.struct_field_types(ty)?;

        let fields = fields
            .into_iter()
            .map(|(name, ty)| FieldHoverInfo {
                name,
                type_name: self.type_name(scopes, ty),
            })
            .collect();

        Some((fields, nil_terminated))
    }

    /// The names and types of a struct's fields, and whether it's nil terminated.
    fn struct_field_types(&mut self, ty: TypeId) -> Option<(Vec<(String, TypeId)>, bool)> {
        let semantic = rue_types::unwrap_semantic(self.ctx.types_mut(), ty, true);

        let mut fields = Vec::new();

        let Type::Struct(ty) = self.ctx.ty(semantic).clone() else {
            return None;
        };

        let mut inner = ty.inner;

        for (i, name) in ty.fields.iter().enumerate() {
            let next = if i == ty.fields.len() - 1 && !ty.nil_terminated {
                inner
            } else {
                let pairs = rue_types::extract_pairs(self.ctx.types_mut(), inner, false);

                let (firsts, rests) = if pairs.is_empty() {
                    let never = self.ctx.builtins().types.never;
                    (never, never)
                } else if pairs.len() == 1 {
                    (pairs[0].first, pairs[0].rest)
                } else {
                    let firsts = self.ctx.alloc_type(Type::Union(Union::new(
                        pairs.iter().map(|pair| pair.first).collect(),
                    )));
                    let rests = self.ctx.alloc_type(Type::Union(Union::new(
                        pairs.iter().map(|pair| pair.rest).collect(),
                    )));
                    (firsts, rests)
                };

                inner = rests;

                firsts
            };

            // The discriminant of an enum variant isn't a field that can be named
            if name == DISCRIMINANT_FIELD {
                continue;
            }

            fields.push((name.clone(), next));
        }

        Some((fields, ty.nil_terminated))
    }
}

/// The struct that declares the fields of a type that a field is used through: the struct itself
/// for an instance of it, or for a union of instances of it (such as narrowed copies).
fn field_owner(ctx: &mut Compiler, container: TypeId) -> Option<TypeId> {
    let semantic = rue_types::unwrap_semantic(ctx.types_mut(), container, true);

    match ctx.ty(semantic) {
        Type::Struct(ty) => Some(ty.semantic),
        Type::Union(_) => rue_types::extract_structs(ctx.types_mut(), semantic)?
            .first()
            .map(|ty| ty.semantic),
        _ => None,
    }
}

/// The span that a query at the index is about, out of the spans that contain it. Names are always
/// separated by at least one character, so today every span that contains the index is the same
/// one. The choice is still made deterministic in case that changes: the narrowest span wins, and
/// since the end of a span counts as being inside it so that the cursor can be right after a name,
/// a span the cursor is strictly inside of wins over one that ends at the cursor. Ties go to the
/// first span.
fn best_span(spans: impl Iterator<Item = TextRange>, index: usize) -> Option<TextRange> {
    spans.min_by_key(|span| (usize::from(span.end()) == index, span.len()))
}

fn contains(range: TextRange, index: usize) -> bool {
    range.contains_inclusive(TextSize::from(index as u32))
}

fn fuzzy_match(query: &str, candidate: &str) -> Option<u32> {
    if query.is_empty() {
        return Some(0);
    }

    let query = query.to_lowercase();
    let candidate = candidate.to_lowercase();

    // Exact prefix match gets highest score
    if candidate.starts_with(&query) {
        return Some(1000);
    }

    // Positions are counted in characters, since lowercasing can change the length of the text in
    // bytes, and identifiers that aren't ASCII would otherwise be sliced inside a character
    let candidate = candidate.chars().collect::<Vec<_>>();

    // Check if all query chars appear in order in the candidate
    let mut last_matched_pos = 0;
    let mut consecutive_matches = 0;
    let mut total_matches = 0;

    for query_char in query.chars() {
        // Look for the next query char in remaining candidate chars
        let i = candidate[last_matched_pos..]
            .iter()
            .position(|&candidate_char| candidate_char == query_char)?;

        // Check if this match is consecutive with the last match
        if i == 0 {
            consecutive_matches += 1;
        } else {
            consecutive_matches = 1;
        }

        total_matches += 1;
        last_matched_pos += i + 1;
    }

    // Calculate score based on:
    // - Number of consecutive matches (higher is better)
    // - Total matches relative to candidate length (higher ratio is better)
    // - Position of first match (earlier is better)
    let consecutive_score = u32::try_from(consecutive_matches).unwrap_or(0) * 10;
    let match_ratio =
        f64::from(total_matches) / f64::from(u32::try_from(candidate.len()).unwrap_or(1)) * 100.0;
    let match_ratio_score = u32::try_from(match_ratio.round() as i64).unwrap_or(0);
    let position_score = 100u32.saturating_sub(u32::try_from(last_matched_pos).unwrap_or(100));

    Some(consecutive_score + match_ratio_score + position_score)
}

fn create_completion_item(
    name: String,
    kind: Option<CompletionItemKind>,
    ty: Option<String>,
) -> CompletionItem {
    CompletionItem {
        label: name,
        kind,
        label_details: Some(CompletionItemLabelDetails {
            detail: ty.map(|ty| format!(" {ty}")),
            description: None,
        }),
        ..Default::default()
    }
}

fn base_symbol_kind(symbol: &Symbol) -> Option<CompletionItemKind> {
    match symbol {
        Symbol::Binding(_) => Some(CompletionItemKind::VARIABLE),
        Symbol::Constant(_) => Some(CompletionItemKind::CONSTANT),
        Symbol::Function(_) => Some(CompletionItemKind::FUNCTION),
        Symbol::Parameter(_) => Some(CompletionItemKind::VARIABLE),
        Symbol::Module(_) => Some(CompletionItemKind::MODULE),
        Symbol::Builtin(_) => Some(CompletionItemKind::FUNCTION),
        Symbol::Unresolved => None,
    }
}

/// A value or module completion, with whether it's a module. Values and modules are both
/// symbols, but are in different namespaces, so a value and a module can have the same name.
fn symbol_item(name: String, item: Item) -> Option<(String, SymbolId, bool)> {
    match item {
        Item::Value(symbol) => Some((name, symbol, false)),
        Item::Module(module) => Some((name, module, true)),
        Item::Type(_) => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestProject;

    #[test]
    fn test_fuzzy_match() {
        assert_eq!(fuzzy_match("", "anything"), Some(0));
        assert_eq!(fuzzy_match("val", "value"), Some(1000));
        assert_eq!(fuzzy_match("VAL", "value"), Some(1000));
        assert_eq!(fuzzy_match("xyz", "value"), None);
        assert_eq!(fuzzy_match("lav", "value"), None);

        // Consecutive matches near the start of a short candidate score higher.
        let close = fuzzy_match("ab", "xab").unwrap();
        let far = fuzzy_match("ab", "x_long_name_a_b").unwrap();
        assert!(close > far);
        assert!(close < 1000);
    }

    fn span(start: u32, end: u32) -> TextRange {
        TextRange::new(start.into(), end.into())
    }

    #[test]
    fn test_best_span() {
        assert_eq!(best_span(std::iter::empty(), 0), None);

        // The narrowest span wins, whichever order they're in
        let outer = span(0, 10);
        let inner = span(2, 5);
        assert_eq!(best_span([outer, inner].into_iter(), 3), Some(inner));
        assert_eq!(best_span([inner, outer].into_iter(), 3), Some(inner));

        // A span that starts at the cursor wins over one that ends there, even if it's wider
        let before = span(0, 3);
        let after = span(3, 10);
        assert_eq!(best_span([before, after].into_iter(), 3), Some(after));
        assert_eq!(best_span([after, before].into_iter(), 3), Some(after));

        // A span that ends at the cursor is used if it's the only one
        assert_eq!(best_span([before].into_iter(), 3), Some(before));

        // Ties go to the first span
        let other = span(1, 4);
        assert_eq!(best_span([before, other].into_iter(), 2), Some(before));
    }

    #[test]
    fn test_duplicate_items() {
        // Each item is only shown once, even if it's in the syntax map more than once
        let project = TestProject::new(&[(
            "main.rue",
            "struct Point {\n    x: Int,\n}\n\nfn main(point: Point) -> Int {\n    point.$0x + point.x\n}\n",
        )]);

        let (uri, position) = project.cursor();

        // The compile is only reachable from the worker thread, so the queries run there and the
        // results are checked here
        let (before, duplicated, widened) = project
            .backend()
            .worker
            .run(None, move |state| {
                let cache = state.cache(&uri, None).unwrap();
                let index = cache.position(position);

                let query = |cache: &mut Cache<Compiler>| {
                    let scopes = cache.scopes(index);
                    (
                        cache.hover(&scopes, index),
                        cache.definitions(index),
                        cache.references(index, true),
                    )
                };

                let before = query(cache);

                for item in cache.syntax_map.items().cloned().collect::<Vec<_>>() {
                    cache.syntax_map.add_item(item);
                }

                let duplicated = query(cache);

                // An item with a wider span around the cursor is ignored in favour of the field
                let mut wider = cache
                    .syntax_map
                    .items()
                    .find(|item| matches!(item.kind, SyntaxItemKind::TypeDeclaration(_)))
                    .unwrap()
                    .clone();
                wider.span = TextRange::new(0.into(), u32::try_from(index + 5).unwrap().into());
                cache.syntax_map.add_item(wider);

                (before, duplicated, query(cache))
            })
            .blocking_recv()
            .unwrap()
            .unwrap();

        assert_eq!(before.0.len(), 1);
        assert_eq!(before.1.len(), 1);
        assert_eq!(before.2.len(), 3);
        assert_eq!(duplicated, before);
        assert_eq!(widened, before);
    }

    #[test]
    fn test_rename_with_duplicate_items() {
        // Each place is only edited once, even if it's in the syntax map more than once
        let project = TestProject::new(&[(
            "main.rue",
            "struct Point {\n    x: Int,\n}\n\nfn main(point: Point) -> Int {\n    point.$0x + point.x\n}\n",
        )]);

        let (uri, position) = project.cursor();

        let (before, duplicated) = project
            .backend()
            .worker
            .run(None, move |state| {
                let cache = state.cache(&uri, None).unwrap();
                let index = cache.position(position);

                let before = cache.rename(index, "left");

                for item in cache.syntax_map.items().cloned().collect::<Vec<_>>() {
                    cache.syntax_map.add_item(item);
                }

                (before, cache.rename(index, "left"))
            })
            .blocking_recv()
            .unwrap()
            .unwrap();

        let edits = before.clone().unwrap().unwrap().edits;
        assert_eq!(edits.values().map(Vec::len).sum::<usize>(), 3);
        assert_eq!(duplicated, before);
    }

    #[test]
    fn test_fuzzy_match_non_ascii() {
        // Matches after a multi-byte character used to slice the candidate inside a character.
        assert!(fuzzy_match("\u{e9}b", "x\u{e9}b").is_some());
        assert!(fuzzy_match("\u{1F600}b", "a\u{1F600}xb").is_some());
        assert_eq!(fuzzy_match("\u{e9}", "\u{c9}t\u{e9}"), Some(1000));
        assert_eq!(fuzzy_match("b\u{e9}", "abc"), None);

        // `İ` lowercases to two characters, which changes the length of the candidate.
        assert!(fuzzy_match("ix", "\u{130}ax").is_some());

        // The score counts characters rather than bytes, so it's the same as for ASCII.
        assert_eq!(fuzzy_match("ab", "x\u{e9}ab"), fuzzy_match("ab", "xeab"));
    }
}
