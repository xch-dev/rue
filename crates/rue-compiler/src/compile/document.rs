use rue_ast::{AstItem, AstSymbolItem};
use rue_hir::ModuleDeclarations;

use crate::{
    Compiler, compile_module_symbols, compile_module_types, compile_symbol_item, compile_type_item,
    declare_module_symbols, declare_module_types, declare_symbol_item, declare_type_item,
};

pub fn declare_type_items(
    ctx: &mut Compiler,
    items: impl Iterator<Item = AstItem>,
    declarations: &mut ModuleDeclarations,
) {
    for item in items {
        match item {
            AstItem::TypeItem(item) => declarations.types.push(declare_type_item(ctx, &item)),
            AstItem::SymbolItem(item) => {
                if let AstSymbolItem::ModuleItem(item) = item {
                    declarations.symbols.push(declare_module_types(ctx, &item));
                }
            }
            AstItem::ImportItem(_) => {}
        }
    }
}

pub fn declare_symbol_items(
    ctx: &mut Compiler,
    items: impl Iterator<Item = AstItem>,
    declarations: &mut ModuleDeclarations,
) {
    let mut index = 0;

    for item in items {
        match item {
            AstItem::TypeItem(_) => {}
            AstItem::SymbolItem(item) => {
                if let AstSymbolItem::ModuleItem(item) = item {
                    declare_module_symbols(ctx, &item, declarations.symbols[index]);
                    index += 1;
                } else {
                    declarations.symbols.push(declare_symbol_item(ctx, &item));
                }
            }
            AstItem::ImportItem(_) => {}
        }
    }
}

pub fn compile_type_items(
    ctx: &mut Compiler,
    items: impl Iterator<Item = AstItem>,
    declarations: &ModuleDeclarations,
) {
    let mut index = 0;
    let mut module_index = 0;

    for item in items {
        match item {
            AstItem::TypeItem(item) => {
                let (ty, scope) = declarations.types[index];
                compile_type_item(ctx, &item, ty, scope);
                index += 1;
            }
            AstItem::SymbolItem(item) => {
                if let AstSymbolItem::ModuleItem(item) = item {
                    compile_module_types(ctx, &item, declarations.symbols[module_index]);
                    module_index += 1;
                }
            }
            AstItem::ImportItem(_) => {}
        }
    }
}

pub fn compile_symbol_items(
    ctx: &mut Compiler,
    items: impl Iterator<Item = AstItem>,
    declarations: &ModuleDeclarations,
) {
    let mut index = 0;

    for item in items {
        match item {
            AstItem::TypeItem(_) => {}
            AstItem::SymbolItem(item) => {
                if let AstSymbolItem::ModuleItem(item) = item {
                    compile_module_symbols(ctx, &item, declarations.symbols[index]);
                } else {
                    let symbol = declarations.symbols[index];
                    compile_symbol_item(ctx, &item, symbol);
                }
                index += 1;
            }
            AstItem::ImportItem(_) => {}
        }
    }
}
