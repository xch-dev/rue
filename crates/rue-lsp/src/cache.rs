use rowan::TextSize;
use rue_compiler::{Compilation, SyntaxItemKind};
use rue_diagnostic::LineCol;
use rue_hir::{ScopeId, SymbolId};
use rue_types::{Type, TypeId};
use tower_lsp::lsp_types::Position;

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
    pub container_name: String,
    pub type_name: String,
    pub kind: NameKind,
}

#[derive(Debug, Clone)]
pub enum NameKind {
    Declaration,
    Reference,
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

            if item.span.contains(TextSize::from(index as u32)) {
                scopes.push(scope);
            }
        }

        Scopes(scopes)
    }

    pub fn hover(&self, scopes: &Scopes, index: usize) -> Option<HoverInfo> {
        for item in self.compilation.syntax_map.items() {
            if !item.span.contains(TextSize::from(index as u32)) {
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
                        container_name: self.type_name(scopes, field.container),
                        type_name: self.type_name(scopes, field.ty),
                        kind: NameKind::Declaration,
                    }));
                }
                SyntaxItemKind::FieldReference(field) => {
                    return Some(HoverInfo::Field(FieldHoverInfo {
                        name: field.name,
                        container_name: self.type_name(scopes, field.container),
                        type_name: self.type_name(scopes, field.ty),
                        kind: NameKind::Reference,
                    }));
                }
                SyntaxItemKind::Scope(_) => {}
            }
        }

        None
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
