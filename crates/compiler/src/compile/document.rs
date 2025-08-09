use rue_ast::{AstDocument, AstItem};

use crate::{
    Context, ScopeId, SymbolId, TypeId, compile_symbol_item, compile_type_item,
    declare_symbol_item, declare_type_item,
};

#[derive(Debug, Default)]
pub struct DocumentDeclarations {
    types: Vec<TypeId>,
    symbols: Vec<SymbolId>,
}

pub fn declare_document(
    ctx: &mut Context,
    scope: ScopeId,
    document: &AstDocument,
) -> DocumentDeclarations {
    let mut declarations = DocumentDeclarations::default();

    ctx.push_scope(scope);

    for item in document.items() {
        match item {
            AstItem::TypeItem(item) => declarations.types.push(declare_type_item(ctx, &item)),
            AstItem::SymbolItem(_) => {}
        }
    }

    for item in document.items() {
        match item {
            AstItem::TypeItem(_) => {}
            AstItem::SymbolItem(item) => declarations.symbols.push(declare_symbol_item(ctx, &item)),
        }
    }

    ctx.pop_scope();

    declarations
}

pub fn compile_document(
    ctx: &mut Context,
    scope: ScopeId,
    document: &AstDocument,
    declarations: DocumentDeclarations,
) {
    ctx.push_scope(scope);

    let mut index = 0;

    for item in document.items() {
        match item {
            AstItem::TypeItem(item) => {
                compile_type_item(ctx, &item, declarations.types[index]);
                index += 1;
            }
            AstItem::SymbolItem(_) => {}
        }
    }

    let mut index = 0;

    for item in document.items() {
        match item {
            AstItem::TypeItem(_) => {}
            AstItem::SymbolItem(item) => {
                compile_symbol_item(ctx, &item, declarations.symbols[index]);
                index += 1;
            }
        }
    }

    ctx.pop_scope();
}
