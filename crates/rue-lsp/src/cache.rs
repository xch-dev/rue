use std::{collections::HashSet, sync::Arc};

use indexmap::IndexMap;
use rowan::{TextRange, TextSize};
use rue_compiler::{Compiler, CompletionContext, SyntaxItemKind, SyntaxMap};
use rue_diagnostic::{LineCol, Source, SourceKind};
use rue_hir::{ScopeId, Symbol, SymbolId};
use rue_types::{Type, TypeId, Union};
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, Location, Position, Range, Url,
};

#[derive(Debug, Clone)]
pub struct Scopes(Vec<ScopeId>);

#[derive(Debug, Clone)]
pub enum HoverInfo {
    Symbol(SymbolHoverInfo),
    Module(ModuleHoverInfo),
    Type(TypeHoverInfo),
    Struct(StructHoverInfo),
    Field(FieldHoverInfo),
}

#[derive(Debug, Clone)]
pub struct SymbolHoverInfo {
    pub name: String,
    pub type_name: String,
}

#[derive(Debug, Clone)]
pub struct ModuleHoverInfo {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct TypeHoverInfo {
    pub name: String,
    pub inner_name: Option<String>,
}

#[derive(Debug, Clone)]
pub struct StructHoverInfo {
    pub name: String,
    pub fields: Vec<FieldHoverInfo>,
}

#[derive(Debug, Clone)]
pub struct FieldHoverInfo {
    pub name: String,
    pub type_name: String,
}

#[derive(Debug, Clone)]
pub struct Cache<T> {
    ctx: T,
    source: Source,
    syntax_map: SyntaxMap,
}

impl Cache<Arc<Compiler>> {
    pub fn new(ctx: Arc<Compiler>, source: Source) -> Self {
        let syntax_map = ctx.syntax_map().clone();

        Self {
            ctx,
            source,
            syntax_map,
        }
    }

    pub fn to_cloned(&self) -> Cache<Compiler> {
        Cache {
            ctx: self.ctx.as_ref().clone(),
            source: self.source.clone(),
            syntax_map: self.syntax_map.clone(),
        }
    }
}

impl Cache<Compiler> {
    pub fn position(&self, position: Position) -> usize {
        LineCol {
            line: position.line as usize,
            col: position.character as usize,
        }
        .index(&self.source.text)
    }

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
                scopes.push(scope);
            }
        }

        Scopes(scopes)
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

        (first, second)
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
        if self.source.text.get(index - 1..index) == Some(":")
            && self.source.text.get(index - 2..index) != Some("::")
        {
            return vec![];
        }

        let (context, subcontext) = self.completion_context(index);
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
                    for field in self.struct_fields(scopes, semantic).unwrap() {
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

        if let CompletionContext::ModuleExports { module } = context.clone() {
            let scope = self.ctx.module(module).scope;

            for (name, symbol) in self
                .ctx
                .scope(scope)
                .exported_symbols()
                .map(|(name, symbol)| (name.to_string(), symbol))
                .collect::<Vec<_>>()
            {
                if !symbol_names.insert(name.to_string()) {
                    continue;
                }

                if let Some(score) = fuzzy_match(&partial, &name)
                    && (matches!(subcontext, CompletionContext::Expression)
                        || (matches!(self.ctx.symbol(symbol), Symbol::Module(_))
                            && matches!(subcontext, CompletionContext::Type)))
                {
                    let ty = self.ctx.symbol_type_in(scope, symbol);
                    let type_name = self.type_name(scopes, ty);
                    let kind = symbol_kind(self.ctx.symbol(symbol));
                    scored_items.push((score, create_completion_item(name, kind, Some(type_name))));
                }
            }

            for (name, ty) in self
                .ctx
                .scope(scope)
                .exported_types()
                .map(|(name, ty)| (name.to_string(), ty))
                .collect::<Vec<_>>()
            {
                if !type_names.insert(name.to_string()) {
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

        for scope in &scopes.0 {
            let symbols = self
                .ctx
                .scope(*scope)
                .symbol_names()
                .map(ToString::to_string)
                .collect::<Vec<_>>();

            let types = self
                .ctx
                .scope(*scope)
                .type_names()
                .map(ToString::to_string)
                .collect::<Vec<_>>();

            for name in symbols {
                if !symbol_names.insert(name.clone()) {
                    continue;
                }

                if let Some(score) = fuzzy_match(&partial, &name) {
                    let symbol = self.ctx.scope(*scope).symbol(&name).unwrap();

                    if matches!(context, CompletionContext::Expression)
                        || (matches!(self.ctx.symbol(symbol), Symbol::Module(_))
                            && matches!(context, CompletionContext::Type))
                    {
                        let ty = self.ctx.symbol_type_in(*scope, symbol);
                        let type_name = self.type_name(scopes, ty);
                        let kind = symbol_kind(self.ctx.symbol(symbol));
                        scored_items
                            .push((score, create_completion_item(name, kind, Some(type_name))));
                    }
                }
            }

            for name in types {
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
                        let ty = self.ctx.scope(*scope).ty(&name).unwrap();

                        let semantic = rue_types::unwrap_semantic(self.ctx.types_mut(), ty, true);

                        match self.ctx.ty(semantic) {
                            Type::Struct(_) => {}
                            _ => continue,
                        }
                    }
                }

                if let Some(score) = fuzzy_match(&partial, &name) {
                    let ty = self.ctx.scope(*scope).ty(&name).unwrap();
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

        for item in self
            .syntax_map
            .items()
            .filter(|item| item.source_kind == self.source.kind)
            .map(Clone::clone)
            .collect::<Vec<_>>()
        {
            if !contains(item.span, index) {
                continue;
            }

            match item.kind.clone() {
                SyntaxItemKind::SymbolDeclaration(symbol)
                | SyntaxItemKind::SymbolReference(symbol) => {
                    let Some(name) = self.symbol_name(scopes, symbol) else {
                        continue;
                    };

                    if let Symbol::Module(_) = self.ctx.symbol(symbol) {
                        infos.push(HoverInfo::Module(ModuleHoverInfo { name }));
                    } else {
                        infos.push(HoverInfo::Symbol(SymbolHoverInfo {
                            name,
                            type_name: self.type_name(scopes, self.symbol_type(scopes, symbol)),
                        }));
                    }
                }
                SyntaxItemKind::TypeDeclaration(ty) | SyntaxItemKind::TypeReference(ty) => {
                    let fields = self.struct_fields(scopes, ty);

                    if let Some(fields) = fields {
                        infos.push(HoverInfo::Struct(StructHoverInfo {
                            name: self.type_name(scopes, ty),
                            fields,
                        }));
                    } else {
                        infos.push(HoverInfo::Type(TypeHoverInfo {
                            name: self.type_name(scopes, ty),
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

        infos
    }

    pub fn definitions(&self, index: usize) -> Vec<Location> {
        self.definitions_impl(index)
            .into_iter()
            .filter(|(_, kind)| matches!(kind, SourceKind::File(_)))
            .map(|(span, kind)| {
                let SourceKind::File(path) = kind else {
                    unreachable!();
                };

                let start = LineCol::new(&self.source.text, span.start().into());
                let end = LineCol::new(&self.source.text, span.end().into());

                let range = Range::new(
                    Position::new(start.line as u32, start.col as u32),
                    Position::new(end.line as u32, end.col as u32),
                );

                Location::new(Url::from_file_path(path).unwrap(), range)
            })
            .collect()
    }

    pub fn references(&self, index: usize) -> Vec<Location> {
        self.references_impl(index)
            .into_iter()
            .filter(|(_, kind)| matches!(kind, SourceKind::File(_)))
            .map(|(span, kind)| {
                let SourceKind::File(path) = kind else {
                    unreachable!();
                };

                let start = LineCol::new(&self.source.text, span.start().into());
                let end = LineCol::new(&self.source.text, span.end().into());

                let range = Range::new(
                    Position::new(start.line as u32, start.col as u32),
                    Position::new(end.line as u32, end.col as u32),
                );

                Location::new(Url::from_file_path(path).unwrap(), range)
            })
            .collect()
    }

    fn definitions_impl(&self, index: usize) -> Vec<(TextRange, SourceKind)> {
        for item in self
            .syntax_map
            .items()
            .filter(|item| item.source_kind == self.source.kind && contains(item.span, index))
        {
            match item.kind.clone() {
                SyntaxItemKind::SymbolDeclaration(symbol) => {
                    return self
                        .syntax_map
                        .items()
                        .filter_map(|item| {
                            let SyntaxItemKind::SymbolReference(declaration) = item.kind else {
                                return None;
                            };

                            if declaration == symbol {
                                Some((item.span, item.source_kind.clone()))
                            } else {
                                None
                            }
                        })
                        .collect();
                }
                SyntaxItemKind::SymbolReference(symbol) => {
                    return self
                        .syntax_map
                        .items()
                        .filter_map(|item| {
                            let (SyntaxItemKind::SymbolDeclaration(declaration)
                            | SyntaxItemKind::FileModule(declaration)) = item.kind
                            else {
                                return None;
                            };

                            if declaration == symbol {
                                Some((item.span, item.source_kind.clone()))
                            } else {
                                None
                            }
                        })
                        .collect();
                }
                SyntaxItemKind::TypeDeclaration(ty) => {
                    return self
                        .syntax_map
                        .items()
                        .filter_map(|item| {
                            let SyntaxItemKind::TypeReference(declaration) = item.kind else {
                                return None;
                            };

                            if declaration == ty {
                                Some((item.span, item.source_kind.clone()))
                            } else {
                                None
                            }
                        })
                        .collect();
                }
                SyntaxItemKind::TypeReference(ty) => {
                    return self
                        .syntax_map
                        .items()
                        .filter_map(|item| {
                            let SyntaxItemKind::TypeDeclaration(declaration) = item.kind else {
                                return None;
                            };

                            if declaration == ty {
                                Some((item.span, item.source_kind.clone()))
                            } else {
                                None
                            }
                        })
                        .collect();
                }
                SyntaxItemKind::FieldDeclaration(field) => {
                    return self
                        .syntax_map
                        .items()
                        .filter_map(|item| {
                            let SyntaxItemKind::FieldReference(declaration) = &item.kind else {
                                return None;
                            };

                            if declaration.container == field.container
                                && declaration.name == field.name
                            {
                                Some((item.span, item.source_kind.clone()))
                            } else {
                                None
                            }
                        })
                        .collect();
                }
                SyntaxItemKind::FieldReference(field) => {
                    return self
                        .syntax_map
                        .items()
                        .filter_map(|item| {
                            let SyntaxItemKind::FieldDeclaration(declaration) = &item.kind else {
                                return None;
                            };

                            if declaration.container == field.container
                                && declaration.name == field.name
                            {
                                Some((item.span, item.source_kind.clone()))
                            } else {
                                None
                            }
                        })
                        .collect();
                }
                SyntaxItemKind::FieldInitializer(_)
                | SyntaxItemKind::Scope(_)
                | SyntaxItemKind::CompletionContext(_)
                | SyntaxItemKind::FileModule(_) => {}
            }
        }

        Vec::new()
    }

    fn references_impl(&self, index: usize) -> Vec<(TextRange, SourceKind)> {
        for item in self
            .syntax_map
            .items()
            .filter(|item| item.source_kind == self.source.kind && contains(item.span, index))
        {
            match item.kind.clone() {
                SyntaxItemKind::SymbolDeclaration(symbol)
                | SyntaxItemKind::SymbolReference(symbol) => {
                    return self
                        .syntax_map
                        .items()
                        .filter_map(|item| {
                            let SyntaxItemKind::SymbolReference(declaration) = item.kind else {
                                return None;
                            };

                            if declaration == symbol {
                                Some((item.span, item.source_kind.clone()))
                            } else {
                                None
                            }
                        })
                        .collect();
                }
                SyntaxItemKind::TypeDeclaration(ty) | SyntaxItemKind::TypeReference(ty) => {
                    return self
                        .syntax_map
                        .items()
                        .filter_map(|item| {
                            let SyntaxItemKind::TypeReference(declaration) = item.kind else {
                                return None;
                            };

                            if declaration == ty {
                                Some((item.span, item.source_kind.clone()))
                            } else {
                                None
                            }
                        })
                        .collect();
                }
                SyntaxItemKind::FieldDeclaration(field) | SyntaxItemKind::FieldReference(field) => {
                    return self
                        .syntax_map
                        .items()
                        .filter_map(|item| {
                            let SyntaxItemKind::FieldReference(declaration) = &item.kind else {
                                return None;
                            };

                            if declaration.container == field.container
                                && declaration.name == field.name
                            {
                                Some((item.span, item.source_kind.clone()))
                            } else {
                                None
                            }
                        })
                        .collect();
                }
                SyntaxItemKind::FieldInitializer(_)
                | SyntaxItemKind::Scope(_)
                | SyntaxItemKind::CompletionContext(_)
                | SyntaxItemKind::FileModule(_) => {}
            }
        }

        Vec::new()
    }

    fn type_name(&mut self, scopes: &Scopes, ty: TypeId) -> String {
        for &scope in &scopes.0 {
            let scope = self.ctx.scope(scope).clone();

            if let Some(name) = scope.type_name(ty) {
                return name.to_string();
            }
        }

        match self.ctx.ty(ty) {
            Type::Struct(ty) => ty.name.as_ref().map(|token| token.text().to_string()),
            Type::Alias(ty) => ty.name.as_ref().map(|token| token.text().to_string()),
            _ => None,
        }
        .unwrap_or_else(|| rue_types::stringify(self.ctx.types_mut(), ty))
    }

    fn symbol_name(&self, scopes: &Scopes, symbol: SymbolId) -> Option<String> {
        for &scope in &scopes.0 {
            let scope = self.ctx.scope(scope).clone();

            if let Some(name) = scope.symbol_name(symbol) {
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
            let scope = self.ctx.scope(scope).clone();

            if let Some(ty) = scope.symbol_override_type(symbol) {
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

    fn struct_fields(&mut self, scopes: &Scopes, ty: TypeId) -> Option<Vec<FieldHoverInfo>> {
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
                name: name.to_string(),
                type_name: self.type_name(scopes, next),
            });
        }

        Some(fields)
    }
}

fn contains(range: TextRange, index: usize) -> bool {
    TextRange::new(range.start(), range.end().checked_add(1.into()).unwrap())
        .contains(TextSize::from(index as u32))
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

    // Check if all query chars appear in order in the candidate
    let mut last_matched_pos = 0;
    let mut consecutive_matches = 0;
    let mut total_matches = 0;
    // Check if all query chars appear in order in the candidate
    for query_char in query.chars() {
        let mut found = false;

        // Look for the next query char in remaining candidate chars
        for (i, candidate_char) in candidate[last_matched_pos..].char_indices() {
            if query_char == candidate_char {
                // Found a match
                found = true;

                // Check if this match is consecutive with the last match
                if i == 0 {
                    consecutive_matches += 1;
                } else {
                    consecutive_matches = 1;
                }

                total_matches += 1;
                last_matched_pos += i + 1;
                break;
            }
        }

        if !found {
            return None;
        }
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
