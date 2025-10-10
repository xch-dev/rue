use std::collections::HashSet;

use rowan::{TextRange, TextSize};
use rue_compiler::{Compilation, CompletionContext, SyntaxItemKind};
use rue_diagnostic::LineCol;
use rue_hir::{ScopeId, SymbolId};
use rue_types::{Type, TypeId};
use tower_lsp::lsp_types::{CompletionItem, Position, Range};

#[derive(Debug, Clone)]
pub struct Scopes(Vec<ScopeId>);

#[derive(Debug, Clone)]
pub enum HoverInfo {
    Symbol(SymbolHoverInfo),
    Type(TypeHoverInfo),
    Field(FieldHoverInfo),
}

#[derive(Debug, Clone)]
pub struct SymbolHoverInfo {
    pub name: String,
    pub type_name: String,
    pub kind: NameKind,
}

#[derive(Debug, Clone)]
pub struct TypeHoverInfo {
    pub name: String,
    pub inner_name: Option<String>,
    pub kind: NameKind,
}

#[derive(Debug, Clone)]
pub struct FieldHoverInfo {
    pub name: String,
    pub type_name: String,
    pub kind: NameKind,
}

#[derive(Debug, Clone)]
pub enum NameKind {
    Declaration,
    Reference,
    Initializer,
}

#[derive(Debug, Clone)]
pub struct Cache {
    compilation: Compilation,
}

impl Cache {
    pub fn new(compilation: Compilation) -> Self {
        Self { compilation }
    }

    pub fn position(&self, position: Position) -> usize {
        LineCol {
            line: position.line as usize,
            col: position.character as usize,
        }
        .index(&self.compilation.source.text)
    }

    pub fn scopes(&self, index: usize) -> Scopes {
        let mut scopes = Vec::new();

        for item in self.compilation.syntax_map.items() {
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
            .compilation
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
        let mut source = self.compilation.source.text[..index].to_string();
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
            && let semantic =
                rue_types::unwrap_semantic(self.compilation.compiler.types_mut(), ty, true)
            && let Type::Struct(ty) = self.compilation.compiler.ty(semantic).clone()
        {
            for field in ty.fields {
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
                .compilation
                .compiler
                .scope(*scope)
                .symbol_names()
                .map(ToString::to_string)
                .collect::<Vec<_>>();

            let types = self
                .compilation
                .compiler
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
                    CompletionContext::Document | CompletionContext::StructFields { .. } => {
                        continue;
                    }
                    CompletionContext::Type => {}
                    CompletionContext::Expression => {
                        let ty = self.compilation.compiler.scope(*scope).ty(&name).unwrap();

                        let semantic = rue_types::unwrap_semantic(
                            self.compilation.compiler.types_mut(),
                            ty,
                            true,
                        );

                        match self.compilation.compiler.ty(semantic) {
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

    pub fn hover(&self, scopes: &Scopes, index: usize) -> Option<HoverInfo> {
        for item in self.compilation.syntax_map.items() {
            if !contains(item.span, index) {
                continue;
            }

            match item.kind.clone() {
                SyntaxItemKind::SymbolDeclaration(symbol) => {
                    return Some(HoverInfo::Symbol(SymbolHoverInfo {
                        name: self.symbol_name(scopes, symbol)?,
                        type_name: self.type_name(scopes, self.symbol_type(scopes, symbol)),
                        kind: NameKind::Declaration,
                    }));
                }
                SyntaxItemKind::SymbolReference(symbol) => {
                    return Some(HoverInfo::Symbol(SymbolHoverInfo {
                        name: self.symbol_name(scopes, symbol)?,
                        type_name: self.type_name(scopes, self.symbol_type(scopes, symbol)),
                        kind: NameKind::Reference,
                    }));
                }
                SyntaxItemKind::TypeDeclaration(ty) => {
                    return Some(HoverInfo::Type(TypeHoverInfo {
                        name: self.type_name(scopes, ty),
                        inner_name: self.inner_type(ty).map(|ty| self.type_name(scopes, ty)),
                        kind: NameKind::Declaration,
                    }));
                }
                SyntaxItemKind::TypeReference(ty) => {
                    return Some(HoverInfo::Type(TypeHoverInfo {
                        name: self.type_name(scopes, ty),
                        inner_name: self.inner_type(ty).map(|ty| self.type_name(scopes, ty)),
                        kind: NameKind::Reference,
                    }));
                }
                SyntaxItemKind::FieldDeclaration(field) => {
                    return Some(HoverInfo::Field(FieldHoverInfo {
                        name: field.name,
                        type_name: self.type_name(scopes, field.ty),
                        kind: NameKind::Declaration,
                    }));
                }
                SyntaxItemKind::FieldReference(field) => {
                    return Some(HoverInfo::Field(FieldHoverInfo {
                        name: field.name,
                        type_name: self.type_name(scopes, field.ty),
                        kind: NameKind::Reference,
                    }));
                }
                SyntaxItemKind::FieldInitializer(field) => {
                    return Some(HoverInfo::Field(FieldHoverInfo {
                        name: field.name,
                        type_name: self.type_name(scopes, field.ty),
                        kind: NameKind::Initializer,
                    }));
                }
                SyntaxItemKind::Scope(_) | SyntaxItemKind::CompletionContext(_) => {}
            }
        }

        None
    }

    pub fn definitions(&self, index: usize) -> Vec<Range> {
        self.definitions_impl(index)
            .into_iter()
            .map(|span| {
                let start = LineCol::new(&self.compilation.source.text, span.start().into());
                let end = LineCol::new(&self.compilation.source.text, span.end().into());

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
                let start = LineCol::new(&self.compilation.source.text, span.start().into());
                let end = LineCol::new(&self.compilation.source.text, span.end().into());

                Range::new(
                    Position::new(start.line as u32, start.col as u32),
                    Position::new(end.line as u32, end.col as u32),
                )
            })
            .collect()
    }

    fn definitions_impl(&self, index: usize) -> Vec<TextRange> {
        for item in self.compilation.syntax_map.items() {
            if !contains(item.span, index) {
                continue;
            }

            match item.kind.clone() {
                SyntaxItemKind::SymbolDeclaration(symbol) => {
                    return self
                        .compilation
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
                        .compilation
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
                        .compilation
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
                        .compilation
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
                        .compilation
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
                        .compilation
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
        for item in self.compilation.syntax_map.items() {
            if !contains(item.span, index) {
                continue;
            }

            match item.kind.clone() {
                SyntaxItemKind::SymbolDeclaration(symbol)
                | SyntaxItemKind::SymbolReference(symbol) => {
                    return self
                        .compilation
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
                        .compilation
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
                        .compilation
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
            let scope = self.compilation.compiler.scope(scope);

            if let Some(name) = scope.type_name(ty) {
                return name.to_string();
            }
        }

        match self.compilation.compiler.ty(ty) {
            Type::Struct(ty) => ty.name.as_ref().map(|token| token.text().to_string()),
            Type::Alias(ty) => ty.name.as_ref().map(|token| token.text().to_string()),
            _ => None,
        }
        .unwrap_or_else(|| {
            rue_types::stringify_without_substitution(self.compilation.compiler.types(), ty)
        })
    }

    fn symbol_name(&self, scopes: &Scopes, symbol: SymbolId) -> Option<String> {
        for &scope in &scopes.0 {
            let scope = self.compilation.compiler.scope(scope);

            if let Some(name) = scope.symbol_name(symbol) {
                return Some(name.to_string());
            }
        }

        self.compilation
            .compiler
            .symbol(symbol)
            .name()
            .map(|token| token.text().to_string())
    }

    fn symbol_type(&self, scopes: &Scopes, symbol: SymbolId) -> TypeId {
        for &scope in &scopes.0 {
            let scope = self.compilation.compiler.scope(scope);

            if let Some(ty) = scope.symbol_override_type(symbol) {
                return ty;
            }
        }

        self.compilation.compiler.symbol_type(symbol)
    }

    fn inner_type(&self, ty: TypeId) -> Option<TypeId> {
        match self.compilation.compiler.ty(ty) {
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
