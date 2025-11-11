use std::collections::HashSet;

use indexmap::IndexSet;
use rowan::{TextRange, TextSize};
use rue_compiler::{Compiler, CompletionContext, SyntaxItemKind, SyntaxMap};
use rue_diagnostic::{LineCol, Source};
use rue_hir::{ScopeId, Symbol, SymbolId};
use rue_types::{Type, TypeId};
use tower_lsp::lsp_types::{CompletionItem, Position, Range};

#[derive(Debug, Clone)]
pub struct Scopes(Vec<ScopeId>);

#[derive(Debug, Clone)]
pub enum HoverInfo {
    Symbol(SymbolHoverInfo),
    Module(ModuleHoverInfo),
    Type(TypeHoverInfo),
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
pub struct FieldHoverInfo {
    pub name: String,
    pub type_name: String,
}

#[derive(Debug, Clone)]
pub struct Cache {
    ctx: Compiler,
    source: Source,
    syntax_map: SyntaxMap,
}

impl Cache {
    pub fn new(ctx: Compiler, source: Source) -> Self {
        let syntax_map = ctx.syntax_map(&source.kind).cloned().unwrap_or_default();

        Self {
            ctx,
            source,
            syntax_map,
        }
    }

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
            let SyntaxItemKind::Scope(scope) = item.kind else {
                continue;
            };

            if contains(item.span, index) {
                scopes.push(scope);
            }
        }

        Scopes(scopes)
    }

    fn completion_context(&self, index: usize) -> CompletionContext {
        for item in self
            .syntax_map
            .items()
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
        {
            let SyntaxItemKind::CompletionContext(context) = &item.kind else {
                continue;
            };

            if contains(item.span, index) {
                return context.clone();
            }
        }

        CompletionContext::Document
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
        let context = self.completion_context(index);
        let partial = self.partial_identifier(index).unwrap_or_default();

        let mut scored_items = Vec::new();
        let mut symbol_names = HashSet::new();
        let mut types_names = HashSet::new();

        if let CompletionContext::StructFields {
            ty,
            specified_fields,
        } = context.clone()
        {
            let semantic = rue_types::unwrap_semantic(self.ctx.types_mut(), ty, true);

            let mut fields = IndexSet::new();

            match self.ctx.ty(semantic) {
                Type::Struct(ty) => {
                    fields.extend(ty.fields.clone());
                }
                Type::Atom(_) => {
                    fields.insert("length".to_string());
                }
                Type::Pair(_) | Type::Union(_) => {
                    let pairs = rue_types::extract_pairs(self.ctx.types_mut(), ty, true);

                    if !pairs.is_empty() {
                        fields.insert("first".to_string());
                        fields.insert("rest".to_string());
                    }
                }
                _ => {}
            }

            for field in fields {
                if specified_fields
                    .as_ref()
                    .is_some_and(|fields| fields.contains(&field))
                {
                    continue;
                }

                if let Some(score) = fuzzy_match(&partial, &field) {
                    scored_items.push((
                        score,
                        CompletionItem::new_simple(field, "Field".to_string()),
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

            if matches!(context, CompletionContext::Expression) {
                for name in symbols {
                    if !symbol_names.insert(name.clone()) {
                        continue;
                    }

                    if let Some(score) = fuzzy_match(&partial, &name) {
                        scored_items.push((
                            score,
                            CompletionItem::new_simple(name, "Symbol".to_string()),
                        ));
                    }
                }
            }

            for name in types {
                if !types_names.insert(name.clone()) {
                    continue;
                }

                match context {
                    CompletionContext::Document
                    | CompletionContext::Item
                    | CompletionContext::Statement
                    | CompletionContext::StructFields { .. } => {
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
                    scored_items
                        .push((score, CompletionItem::new_simple(name, "Type".to_string())));
                }
            }
        }

        // Sort by score in descending order
        scored_items.sort_by(|(a, _), (b, _)| b.cmp(a));

        // Return just the completion items
        scored_items.into_iter().map(|(_, item)| item).collect()
    }

    pub fn hover(&self, scopes: &Scopes, index: usize) -> Vec<HoverInfo> {
        let mut infos = Vec::new();

        for item in self.syntax_map.items() {
            if !contains(item.span, index) {
                continue;
            }

            match item.kind.clone() {
                SyntaxItemKind::SymbolDeclaration(symbol) => {
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
                SyntaxItemKind::SymbolReference(symbol) => {
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
                SyntaxItemKind::TypeDeclaration(ty) => {
                    infos.push(HoverInfo::Type(TypeHoverInfo {
                        name: self.type_name(scopes, ty),
                        inner_name: self.inner_type(ty).map(|ty| self.type_name(scopes, ty)),
                    }));
                }
                SyntaxItemKind::TypeReference(ty) => {
                    infos.push(HoverInfo::Type(TypeHoverInfo {
                        name: self.type_name(scopes, ty),
                        inner_name: self.inner_type(ty).map(|ty| self.type_name(scopes, ty)),
                    }));
                }
                SyntaxItemKind::FieldDeclaration(field) => {
                    infos.push(HoverInfo::Field(FieldHoverInfo {
                        name: field.name,
                        type_name: self.type_name(scopes, field.ty),
                    }));
                }
                SyntaxItemKind::FieldReference(field) => {
                    infos.push(HoverInfo::Field(FieldHoverInfo {
                        name: field.name,
                        type_name: self.type_name(scopes, field.ty),
                    }));
                }
                SyntaxItemKind::FieldInitializer(field) => {
                    infos.push(HoverInfo::Field(FieldHoverInfo {
                        name: field.name,
                        type_name: self.type_name(scopes, field.ty),
                    }));
                }
                SyntaxItemKind::Scope(_) | SyntaxItemKind::CompletionContext(_) => {}
            }
        }

        infos
    }

    pub fn definitions(&self, index: usize) -> Vec<Range> {
        self.definitions_impl(index)
            .into_iter()
            .map(|span| {
                let start = LineCol::new(&self.source.text, span.start().into());
                let end = LineCol::new(&self.source.text, span.end().into());

                Range::new(
                    Position::new(start.line as u32, start.col as u32),
                    Position::new(end.line as u32, end.col as u32),
                )
            })
            .collect()
    }

    pub fn references(&self, index: usize) -> Vec<Range> {
        self.references_impl(index)
            .into_iter()
            .map(|span| {
                let start = LineCol::new(&self.source.text, span.start().into());
                let end = LineCol::new(&self.source.text, span.end().into());

                Range::new(
                    Position::new(start.line as u32, start.col as u32),
                    Position::new(end.line as u32, end.col as u32),
                )
            })
            .collect()
    }

    fn definitions_impl(&self, index: usize) -> Vec<TextRange> {
        for item in self.syntax_map.items() {
            if !contains(item.span, index) {
                continue;
            }

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
                                Some(item.span)
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
                            let SyntaxItemKind::SymbolDeclaration(declaration) = item.kind else {
                                return None;
                            };

                            if declaration == symbol {
                                Some(item.span)
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
                                Some(item.span)
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
                                Some(item.span)
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
                                Some(item.span)
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
                                Some(item.span)
                            } else {
                                None
                            }
                        })
                        .collect();
                }
                SyntaxItemKind::FieldInitializer(_)
                | SyntaxItemKind::Scope(_)
                | SyntaxItemKind::CompletionContext(_) => {}
            }
        }

        Vec::new()
    }

    fn references_impl(&self, index: usize) -> Vec<TextRange> {
        for item in self.syntax_map.items() {
            if !contains(item.span, index) {
                continue;
            }

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
                                Some(item.span)
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
                                Some(item.span)
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
                                Some(item.span)
                            } else {
                                None
                            }
                        })
                        .collect();
                }
                SyntaxItemKind::FieldInitializer(_)
                | SyntaxItemKind::Scope(_)
                | SyntaxItemKind::CompletionContext(_) => {}
            }
        }

        Vec::new()
    }

    fn type_name(&self, scopes: &Scopes, ty: TypeId) -> String {
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
        .unwrap_or_else(|| rue_types::stringify_without_substitution(self.ctx.types(), ty))
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
