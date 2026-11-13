use std::collections::HashSet;

use indexmap::IndexMap;
use rowan::{TextRange, TextSize, TokenAtOffset};
use rue_ast::AstNode;
use rue_compiler::{Compiler, CompletionContext, FileTree, SyntaxItem, SyntaxItemKind, SyntaxMap};
use rue_diagnostic::{Source, SourceKind};
use rue_hir::{FunctionKind, ScopeId, Symbol, SymbolId};
use rue_lexer::{Lexer, TokenKind};
use rue_parser::{SyntaxKind, SyntaxToken, T};
use rue_types::{Type, TypeId, Union};
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, Location, Position, Url,
};

use crate::position::{offsets_to_range, position_to_offset};

#[derive(Debug, Clone)]
pub struct Scopes(Vec<ScopeId>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HoverInfo {
    Symbol(SymbolHoverInfo),
    Module(ModuleHoverInfo),
    Type(TypeHoverInfo),
    Struct(StructHoverInfo),
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
pub struct FieldHoverInfo {
    pub name: String,
    pub type_name: String,
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

/// What an item is about and its role, for the items that navigation queries can go to.
fn classify(kind: &SyntaxItemKind) -> Option<(Target<'_>, Role)> {
    match kind {
        SyntaxItemKind::SymbolDeclaration(symbol) => {
            Some((Target::Symbol(*symbol), Role::Declaration))
        }
        SyntaxItemKind::FileModule(symbol) => Some((Target::Symbol(*symbol), Role::FileModule)),
        SyntaxItemKind::SymbolReference(symbol) => Some((Target::Symbol(*symbol), Role::Reference)),
        SyntaxItemKind::TypeDeclaration(ty) => Some((Target::Type(*ty), Role::Declaration)),
        SyntaxItemKind::TypeReference(ty) => Some((Target::Type(*ty), Role::Reference)),
        SyntaxItemKind::FieldDeclaration(field) => Some((
            Target::Field(field.container, &field.name),
            Role::Declaration,
        )),
        SyntaxItemKind::FieldReference(field) => {
            Some((Target::Field(field.container, &field.name), Role::Reference))
        }
        SyntaxItemKind::FieldInitializer(_)
        | SyntaxItemKind::Scope(_)
        | SyntaxItemKind::CompletionContext(_) => None,
    }
}

/// A project's compile, which answers queries about one of its files at a time. Queries can
/// allocate types, so they need the compiler mutably, and they run on the thread that owns it.
#[derive(Debug)]
pub struct Cache<T> {
    ctx: T,
    tree: FileTree,
    source: Source,
    syntax_map: SyntaxMap,
}

impl Cache<Compiler> {
    pub fn new(ctx: Compiler, tree: FileTree, source: Source) -> Self {
        let syntax_map = ctx.syntax_map().clone();

        Self {
            ctx,
            tree,
            source,
            syntax_map,
        }
    }

    /// Sets the file that queries are about.
    pub fn set_source(&mut self, source: Source) {
        self.source = source;
    }

    pub fn position(&self, position: Position) -> usize {
        position_to_offset(&self.source.text, position)
    }

    /// The scopes that contain the index, innermost first, so that a name that's shadowed
    /// resolves to the innermost declaration.
    pub fn scopes(&self, index: usize) -> Scopes {
        let mut scopes = Vec::new();

        for item in self.syntax_map.items() {
            if item.source_kind != self.source.kind {
                continue;
            }

            let SyntaxItemKind::Scope(scope) = item.kind else {
                continue;
            };

            if contains(item.span, index) {
                scopes.push((item.span, scope));
            }
        }

        // Scopes are added to the syntax map when they're exited, which is innermost first within
        // a function body, but the file's scopes are added before any of the bodies are compiled.
        // Every scope that contains the index is nested in or equal to the others, so the narrower
        // one is the inner one. The sort is stable, so scopes with the same span keep the order in
        // which they were exited.
        scopes.sort_by_key(|(span, _)| span.len());

        Scopes(scopes.into_iter().map(|(_, scope)| scope).collect())
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
            T![return] | T![assert] | T![raise] | T![if] => Some(CompletionContext::Expression),
            _ => None,
        }
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

        if before.ends_with('.') && !matches!(context, CompletionContext::StructFields { .. }) {
            return vec![];
        }

        let partial = self.partial_identifier(index).unwrap_or_default();

        let mut scored_items = Vec::new();
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

            for (name, symbol) in self
                .ctx
                .scope(scope)
                .exported_symbols()
                .map(|(name, symbol)| (name.to_string(), symbol))
                .collect::<Vec<_>>()
            {
                if !symbol_names.insert(name.clone()) {
                    continue;
                }

                if let Some(score) = fuzzy_match(&partial, &name)
                    && (matches!(subcontext, CompletionContext::Expression)
                        || (matches!(self.ctx.symbol(symbol), Symbol::Module(_))
                            && matches!(subcontext, CompletionContext::Type)))
                {
                    let ty = self.ctx.symbol_type_in(scope, symbol);
                    let detail = self.symbol_detail(scopes, symbol, ty);
                    let kind = symbol_kind(self.ctx.symbol(symbol));
                    scored_items.push((score, create_completion_item(name, kind, Some(detail))));
                }
            }

            for (name, ty) in self
                .ctx
                .scope(scope)
                .exported_types()
                .map(|(name, ty)| (name.to_string(), ty))
                .collect::<Vec<_>>()
            {
                if !type_names.insert(name.clone()) {
                    continue;
                }

                match subcontext {
                    CompletionContext::Document
                    | CompletionContext::Item
                    | CompletionContext::Statement
                    | CompletionContext::StructFields { .. }
                    | CompletionContext::ModuleExports { .. } => {
                        continue;
                    }
                    CompletionContext::Type => {}
                    CompletionContext::Expression => {
                        let semantic = rue_types::unwrap_semantic(self.ctx.types_mut(), ty, true);

                        match self.ctx.ty(semantic) {
                            Type::Struct(_) => {}
                            _ => continue,
                        }
                    }
                }

                if let Some(score) = fuzzy_match(&partial, &name) {
                    let type_name = self.inner_type(ty).map(|ty| self.type_name(scopes, ty));

                    scored_items.push((
                        score,
                        create_completion_item(name, Some(CompletionItemKind::STRUCT), type_name),
                    ));
                }
            }
        }

        if let Some(score) = fuzzy_match(&partial, "super")
            && matches!(
                context,
                CompletionContext::Expression | CompletionContext::Type
            )
        {
            scored_items.push((
                score,
                create_completion_item("super".to_string(), Some(CompletionItemKind::MODULE), None),
            ));
        }

        for scope in &scopes.0 {
            let scope = self.ctx.scope(*scope);

            let symbols = scope
                .symbol_names()
                .filter_map(|name| Some((name.to_string(), scope.symbol(name)?)))
                .collect::<Vec<_>>();

            let types = scope
                .type_names()
                .filter_map(|name| Some((name.to_string(), scope.ty(name)?)))
                .collect::<Vec<_>>();

            for (name, symbol) in symbols {
                if !symbol_names.insert(name.clone()) {
                    continue;
                }

                if let Some(score) = fuzzy_match(&partial, &name)
                    && (matches!(context, CompletionContext::Expression)
                        || (matches!(self.ctx.symbol(symbol), Symbol::Module(_))
                            && matches!(context, CompletionContext::Type)))
                {
                    // The type is looked up from the cursor's scopes, like on hover, so that it's
                    // narrowed by the conditions the cursor is inside of
                    let ty = self.symbol_type(scopes, symbol);
                    let detail = self.symbol_detail(scopes, symbol, ty);
                    let kind = symbol_kind(self.ctx.symbol(symbol));
                    scored_items.push((score, create_completion_item(name, kind, Some(detail))));
                }
            }

            for (name, ty) in types {
                if !type_names.insert(name.clone()) {
                    continue;
                }

                match context {
                    CompletionContext::Document
                    | CompletionContext::Item
                    | CompletionContext::Statement
                    | CompletionContext::StructFields { .. }
                    | CompletionContext::ModuleExports { .. } => {
                        continue;
                    }
                    CompletionContext::Type => {}
                    CompletionContext::Expression => {
                        let semantic = rue_types::unwrap_semantic(self.ctx.types_mut(), ty, true);

                        match self.ctx.ty(semantic) {
                            Type::Struct(_) => {}
                            _ => continue,
                        }
                    }
                }

                if let Some(score) = fuzzy_match(&partial, &name) {
                    let type_name = self.inner_type(ty).map(|ty| self.type_name(scopes, ty));

                    scored_items.push((
                        score,
                        create_completion_item(name, Some(CompletionItemKind::STRUCT), type_name),
                    ));
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
                SyntaxItemKind::Scope(_)
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
            .find_map(|item| classify(&item.kind))
    }

    /// The locations of the items with the target and one of the roles, sorted by file and
    /// position and without duplicates. Items in the standard library are left out, since it has
    /// no file the editor can open.
    fn locations(&self, target: Target<'_>, roles: &[Role]) -> Vec<Location> {
        let mut locations = self
            .syntax_map
            .items()
            .filter(|item| {
                classify(&item.kind)
                    .is_some_and(|(other, role)| other == target && roles.contains(&role))
            })
            .filter_map(|item| {
                let SourceKind::File(path) = &item.source_kind else {
                    return None;
                };

                let text = &self.tree.find(&item.source_kind)?.source.text;
                let range =
                    offsets_to_range(text, item.span.start().into(), item.span.end().into());

                Some(Location::new(Url::from_file_path(path).ok()?, range))
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
            if let Some(name) = self.ctx.scope(scope).symbol_name(symbol) {
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

    fn struct_fields(
        &mut self,
        scopes: &Scopes,
        ty: TypeId,
    ) -> Option<(Vec<FieldHoverInfo>, bool)> {
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

            fields.push(FieldHoverInfo {
                name: name.clone(),
                type_name: self.type_name(scopes, next),
            });
        }

        Some((fields, ty.nil_terminated))
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

fn symbol_kind(symbol: &Symbol) -> Option<CompletionItemKind> {
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
