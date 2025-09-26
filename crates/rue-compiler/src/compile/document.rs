use rue_ast::{AstDocument, AstItem};
use rue_hir::{ModuleDeclarations, ScopeId};

use crate::{
    Compiler, compile_symbol_item, compile_type_item, declare_symbol_item, declare_type_item,
};

pub fn declare_document(
    ctx: &mut Compiler,
    scope: ScopeId,
    document: &AstDocument,
) -> ModuleDeclarations {
    declare_items(ctx, scope, document.items())
}

pub fn declare_items(
    ctx: &mut Compiler,
    scope: ScopeId,
    items: impl Iterator<Item = AstItem>,
) -> ModuleDeclarations {
    let items = &items.collect::<Vec<_>>();

    let mut declarations = ModuleDeclarations::default();

    ctx.push_scope(scope);

    for item in items {
        match item {
            AstItem::TypeItem(item) => declarations.types.push(declare_type_item(ctx, item)),
            AstItem::SymbolItem(_) => {}
        }
    }

    for item in items {
        match item {
            AstItem::TypeItem(_) => {}
            AstItem::SymbolItem(item) => declarations.symbols.push(declare_symbol_item(ctx, item)),
        }
    }

    ctx.pop_scope();

    declarations
}

pub fn compile_document(
    ctx: &mut Compiler,
    scope: ScopeId,
    document: &AstDocument,
    declarations: ModuleDeclarations,
) {
    compile_items(ctx, scope, document.items(), declarations);
}

#[allow(clippy::needless_pass_by_value)]
pub fn compile_items(
    ctx: &mut Compiler,
    scope: ScopeId,
    items: impl Iterator<Item = AstItem>,
    declarations: ModuleDeclarations,
) {
    let items = &items.collect::<Vec<_>>();

    ctx.push_scope(scope);

    let mut index = 0;

    for item in items {
        match item {
            AstItem::TypeItem(item) => {
                let (ty, scope) = declarations.types[index];
                compile_type_item(ctx, item, ty, scope);
                index += 1;
            }
            AstItem::SymbolItem(_) => {}
        }
    }

    let mut index = 0;

    for item in items {
        match item {
            AstItem::TypeItem(_) => {}
            AstItem::SymbolItem(item) => {
                compile_symbol_item(ctx, item, declarations.symbols[index]);
                index += 1;
            }
        }
    }

    ctx.pop_scope();
}
