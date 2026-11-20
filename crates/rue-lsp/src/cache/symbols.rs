//! The outline of a file: its functions, constants, types and modules, with the fields of structs
//! and the variants of enums inside of them. It's read from the syntax tree of the last compile,
//! so that a file that has errors still has an outline of what parsed.

use rue_ast::{
    AstConstantItem, AstEnumItem, AstEnumVariant, AstFunctionItem, AstItem, AstModuleItem, AstNode,
    AstStructField, AstStructItem, AstSymbolItem, AstType, AstTypeAliasItem, AstTypeItem,
};
use rue_compiler::{Compiler, File};
use rue_diagnostic::SourceKind;
use rue_parser::{SyntaxNode, SyntaxToken};
use tower_lsp_server::ls_types::{DocumentSymbol, Range, SymbolKind};

use super::{Cache, fuzzy_match};
use crate::position::source_range;

/// How many characters of a symbol's detail are shown, so that a long signature stays on a line.
const DETAIL_CHARS: usize = 80;

/// A symbol of a project, for searching all of them by name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectSymbol {
    pub name: String,
    pub kind: SymbolKind,
    /// The name of the symbol that it's inside of, such as a field's struct.
    pub container: Option<String>,
    pub file: SourceKind,
    pub range: Range,
    /// How well the name matches what was searched for.
    pub score: u32,
}

impl Cache<Compiler> {
    /// The outline of the file that queries are about.
    pub fn document_symbols(&self) -> Vec<DocumentSymbol> {
        self.tree
            .find(&self.source.kind)
            .map(file_symbols)
            .unwrap_or_default()
    }

    /// The symbols of every file of the project whose names match the query, which is all of
    /// them if it's empty.
    pub fn project_symbols(&self, query: &str) -> Vec<ProjectSymbol> {
        let mut symbols = Vec::new();

        for file in self.tree.all_files() {
            matching_symbols(file, &file_symbols(file), None, query, &mut symbols);
        }

        symbols
    }
}

/// Adds the symbols that match the query, with the symbols inside of them.
fn matching_symbols(
    file: &File,
    symbols: &[DocumentSymbol],
    container: Option<&str>,
    query: &str,
    matching: &mut Vec<ProjectSymbol>,
) {
    for symbol in symbols {
        if let Some(score) = fuzzy_match(query, &symbol.name) {
            matching.push(ProjectSymbol {
                name: symbol.name.clone(),
                kind: symbol.kind,
                container: container.map(ToString::to_string),
                file: file.source.kind.clone(),
                range: symbol.selection_range,
                score,
            });
        }

        matching_symbols(
            file,
            symbol.children.as_deref().unwrap_or_default(),
            Some(&symbol.name),
            query,
            matching,
        );
    }
}

fn file_symbols(file: &File) -> Vec<DocumentSymbol> {
    item_symbols(file, file.document.items())
}

fn item_symbols(file: &File, items: impl Iterator<Item = AstItem>) -> Vec<DocumentSymbol> {
    items.filter_map(|item| item_symbol(file, &item)).collect()
}

/// The symbol for an item, unless it has no name (which is a syntax error) or it's an import.
fn item_symbol(file: &File, item: &AstItem) -> Option<DocumentSymbol> {
    match item {
        AstItem::SymbolItem(AstSymbolItem::ModuleItem(module)) => module_symbol(file, module),
        AstItem::SymbolItem(AstSymbolItem::FunctionItem(function)) => {
            function_symbol(file, function)
        }
        AstItem::SymbolItem(AstSymbolItem::ConstantItem(constant)) => {
            constant_symbol(file, constant)
        }
        AstItem::TypeItem(AstTypeItem::TypeAliasItem(alias)) => alias_symbol(file, alias),
        AstItem::TypeItem(AstTypeItem::StructItem(item)) => struct_symbol(file, item),
        AstItem::TypeItem(AstTypeItem::EnumItem(item)) => enum_symbol(file, item),
        AstItem::ImportItem(_) => None,
    }
}

fn module_symbol(file: &File, module: &AstModuleItem) -> Option<DocumentSymbol> {
    Some(symbol(
        file,
        module.syntax(),
        &module.name()?,
        SymbolKind::MODULE,
        None,
        item_symbols(file, module.items()),
    ))
}

fn function_symbol(file: &File, function: &AstFunctionItem) -> Option<DocumentSymbol> {
    let name = function.name()?;

    // A function is shown with its signature, as it's written
    let generics = function
        .generic_parameters()
        .map(|generics| generics.syntax().text().to_string())
        .unwrap_or_default();

    let parameters = function
        .parameters()
        .map(|parameter| one_line(&parameter.syntax().text().to_string()))
        .collect::<Vec<_>>()
        .join(", ");

    let return_type = function
        .return_type()
        .map(|ty| format!(" -> {}", ty.syntax().text()))
        .unwrap_or_default();

    let signature = detail(&format!("{generics}({parameters}){return_type}"));

    // A test is told apart from the functions it tests
    let signature = if function.test().is_some() {
        format!("test {signature}")
    } else {
        signature
    };

    Some(symbol(
        file,
        function.syntax(),
        &name,
        SymbolKind::FUNCTION,
        Some(signature),
        Vec::new(),
    ))
}

fn constant_symbol(file: &File, constant: &AstConstantItem) -> Option<DocumentSymbol> {
    Some(symbol(
        file,
        constant.syntax(),
        &constant.name()?,
        SymbolKind::CONSTANT,
        type_detail(constant.ty()),
        Vec::new(),
    ))
}

fn alias_symbol(file: &File, alias: &AstTypeAliasItem) -> Option<DocumentSymbol> {
    // There's no kind for a type alias, and this is the one that other servers use for it
    Some(symbol(
        file,
        alias.syntax(),
        &alias.name()?,
        SymbolKind::TYPE_PARAMETER,
        type_detail(alias.ty()),
        Vec::new(),
    ))
}

fn struct_symbol(file: &File, item: &AstStructItem) -> Option<DocumentSymbol> {
    Some(symbol(
        file,
        item.syntax(),
        &item.name()?,
        SymbolKind::STRUCT,
        None,
        field_symbols(file, item.fields()),
    ))
}

fn enum_symbol(file: &File, item: &AstEnumItem) -> Option<DocumentSymbol> {
    Some(symbol(
        file,
        item.syntax(),
        &item.name()?,
        SymbolKind::ENUM,
        None,
        item.variants()
            .filter_map(|variant| variant_symbol(file, &variant))
            .collect(),
    ))
}

fn variant_symbol(file: &File, variant: &AstEnumVariant) -> Option<DocumentSymbol> {
    Some(symbol(
        file,
        variant.syntax(),
        &variant.name()?,
        SymbolKind::ENUM_MEMBER,
        None,
        field_symbols(file, variant.struct_fields()),
    ))
}

fn field_symbols(file: &File, fields: impl Iterator<Item = AstStructField>) -> Vec<DocumentSymbol> {
    fields
        .filter_map(|field| {
            Some(symbol(
                file,
                field.syntax(),
                &field.name()?,
                SymbolKind::FIELD,
                type_detail(field.ty()),
                Vec::new(),
            ))
        })
        .collect()
}

/// A symbol for a node, which is selected by its name.
fn symbol(
    file: &File,
    node: &SyntaxNode,
    name: &SyntaxToken,
    kind: SymbolKind,
    detail: Option<String>,
    children: Vec<DocumentSymbol>,
) -> DocumentSymbol {
    let range = |range: rowan::TextRange| {
        source_range(&file.source, range.start().into(), range.end().into())
    };

    #[allow(deprecated)]
    DocumentSymbol {
        name: name.text().to_string(),
        detail,
        kind,
        tags: None,
        deprecated: None,
        range: range(node.text_range()),
        selection_range: range(name.text_range()),
        children: (!children.is_empty()).then_some(children),
    }
}

fn type_detail(ty: Option<AstType>) -> Option<String> {
    ty.map(|ty| detail(&ty.syntax().text().to_string()))
}

/// Source text as it's shown next to a symbol's name: on one line, and not too much of it.
fn detail(text: &str) -> String {
    let text = one_line(text);

    if text.chars().count() <= DETAIL_CHARS {
        return text;
    }

    let kept = text.chars().take(DETAIL_CHARS - 3).collect::<String>();

    format!("{}...", kept.trim_end())
}

/// Source text without its line breaks and indentation.
fn one_line(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}
