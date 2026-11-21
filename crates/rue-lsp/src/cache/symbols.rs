//! The outline of a file: its functions, constants, types and modules, with the fields of structs
//! and the variants of enums inside of them. It's read from the syntax tree of the last compile,
//! so that a file that has errors still has an outline of what parsed.

use rowan::{NodeOrToken, TextRange};
use rue_ast::{
    AstConstantItem, AstEnumItem, AstEnumVariant, AstFunctionItem, AstItem, AstModuleItem, AstNode,
    AstStructField, AstStructItem, AstSymbolItem, AstType, AstTypeAliasItem, AstTypeItem,
};
use rue_compiler::{Compiler, File};
use rue_diagnostic::SourceKind;
use rue_parser::{SyntaxKind, SyntaxNode, SyntaxToken};
use rue_types::ELIDED;
use tower_lsp_server::ls_types::{DocumentSymbol, Range, SymbolKind};

use super::{Cache, fuzzy_match};
use crate::position::source_range;

/// How many characters of a symbol's detail are shown, so that a long signature stays on a line.
const DETAIL_CHARS: usize = 80;

/// A symbol of a file, for searching all of them by name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FoundSymbol {
    pub name: String,
    pub kind: SymbolKind,
    /// The name of the symbol that it's inside of, such as a field's struct.
    pub container: Option<String>,
    /// Where its name is.
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

    /// The symbols whose names match the query (which is all of them if it's empty), for each
    /// of the project's files that are searched.
    pub fn find_symbols(
        &self,
        query: &str,
        searched: impl Fn(&SourceKind) -> bool,
    ) -> Vec<(&SourceKind, Vec<FoundSymbol>)> {
        self.tree
            .all_files()
            .into_iter()
            .filter(|file| searched(&file.source.kind))
            .map(|file| {
                let mut symbols = Vec::new();
                matching_symbols(&file_symbols(file), None, query, &mut symbols);
                (&file.source.kind, symbols)
            })
            .collect()
    }
}

/// Adds the symbols that match the query, with the symbols inside of them.
fn matching_symbols(
    symbols: &[DocumentSymbol],
    container: Option<&str>,
    query: &str,
    matching: &mut Vec<FoundSymbol>,
) {
    for symbol in symbols {
        if let Some(score) = fuzzy_match(query, &symbol.name) {
            matching.push(FoundSymbol {
                name: symbol.name.clone(),
                kind: symbol.kind,
                container: container.map(ToString::to_string),
                range: symbol.selection_range,
                score,
            });
        }

        matching_symbols(
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

    // A function is shown with its signature, as it's written, and a test is told apart from the
    // functions it tests
    let test = if function.test().is_some() {
        "test "
    } else {
        ""
    };

    let generics = function
        .generic_parameters()
        .map(|generics| code(generics.syntax()))
        .unwrap_or_default();

    let parameters = function
        .parameters()
        .map(|parameter| code(parameter.syntax()))
        .collect::<Vec<_>>()
        .join(", ");

    let return_type = function
        .return_type()
        .map(|ty| format!(" -> {}", code(ty.syntax())))
        .unwrap_or_default();

    let signature = detail(&format!("{test}{generics}({parameters}){return_type}"));

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
    let range =
        |range: TextRange| source_range(&file.source, range.start().into(), range.end().into());

    // A node can end with the whitespace and comments after its last token, which aren't part of
    // the item
    let end = std::iter::successors(node.last_token(), SyntaxToken::prev_token)
        .find(|token| !token.kind().is_trivia())
        .map_or(node.text_range().end(), |token| token.text_range().end());

    let item = TextRange::new(node.text_range().start(), end);

    #[allow(deprecated)]
    DocumentSymbol {
        name: name.text().to_string(),
        detail,
        kind,
        tags: None,
        deprecated: None,
        range: range(item),
        selection_range: range(name.text_range()),
        children: (!children.is_empty()).then_some(children),
    }
}

fn type_detail(ty: Option<AstType>) -> Option<String> {
    ty.map(|ty| detail(&code(ty.syntax())))
}

/// The text of a node without its comments, on one line.
fn code(node: &SyntaxNode) -> String {
    let mut text = String::new();

    for token in node
        .descendants_with_tokens()
        .filter_map(NodeOrToken::into_token)
    {
        if !matches!(
            token.kind(),
            SyntaxKind::LineComment | SyntaxKind::BlockComment
        ) {
            text.push_str(token.text());
        }
    }

    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

/// Text as it's shown next to a symbol's name, which is cut short if it's long.
fn detail(text: &str) -> String {
    if text.chars().count() <= DETAIL_CHARS {
        return text.to_string();
    }

    let kept = text
        .chars()
        .take(DETAIL_CHARS - ELIDED.len())
        .collect::<String>();

    format!("{}{ELIDED}", kept.trim_end())
}
