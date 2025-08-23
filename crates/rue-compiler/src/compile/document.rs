use rue_ast::{AstDocument, AstItem};
use rue_hir::{ScopeId, SymbolId};
use rue_types::TypeId;

use crate::{
    Compiler, compile_symbol_item, compile_type_item, declare_symbol_item, declare_type_item,
};

#[derive(Debug, Default)]
pub struct DocumentDeclarations {
    types: Vec<(TypeId, ScopeId)>,
    symbols: Vec<SymbolId>,
}

pub fn declare_document(
    ctx: &mut Compiler,
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
    ctx: &mut Compiler,
    scope: ScopeId,
    document: &AstDocument,
    declarations: DocumentDeclarations,
) {
    ctx.push_scope(scope);

    let mut index = 0;

    for item in document.items() {
        match item {
            AstItem::TypeItem(item) => {
                let (ty, scope) = declarations.types[index];
                compile_type_item(ctx, &item, ty, scope);
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
