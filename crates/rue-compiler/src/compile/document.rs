use rowan::TextSize;
use rue_ast::{AstItem, AstSymbolItem};
use rue_hir::{ModuleDeclarations, Symbol};

use crate::{
    Compiler, compile_module_symbols, compile_module_types, compile_symbol_item, compile_type_item,
    declare_import_item, declare_module_imports, declare_module_symbols, declare_module_types,
    declare_symbol_item, declare_type_item, resolve_imports,
};

pub fn declare_import_items(
    ctx: &mut Compiler,
    items: impl Iterator<Item = AstItem>,
    declarations: &mut ModuleDeclarations,
) {
    for item in items {
        match item {
            AstItem::ImportItem(item) => declare_import_item(ctx, &item),
            AstItem::SymbolItem(AstSymbolItem::ModuleItem(item)) => {
                declarations
                    .modules
                    .push(declare_module_imports(ctx, &item));
            }
            _ => {}
        }
    }
}

pub fn declare_type_items(
    ctx: &mut Compiler,
    items: impl Iterator<Item = AstItem>,
    declarations: &mut ModuleDeclarations,
) {
    let mut module = 0;

    for item in items {
        match item {
            AstItem::TypeItem(item) => declarations.types.push(declare_type_item(ctx, &item)),
            AstItem::SymbolItem(item) => {
                if let AstSymbolItem::ModuleItem(item) = item {
                    declare_module_types(ctx, &item, declarations.modules[module]);
                    module += 1;
                }
            }
            AstItem::ImportItem(_) => {}
        }
    }
}

pub fn resolve_type_imports(ctx: &mut Compiler, declarations: &ModuleDeclarations) {
    resolve_imports(ctx, false);

    for &symbol in &declarations.modules {
        let Symbol::Module(module) = ctx.symbol(symbol).clone() else {
            continue;
        };
        ctx.push_scope(module.scope, TextSize::from(0));
        resolve_type_imports(ctx, &module.declarations);
        ctx.pop_scope(TextSize::from(0));
    }
}

pub fn declare_symbol_items(
    ctx: &mut Compiler,
    items: impl Iterator<Item = AstItem>,
    declarations: &mut ModuleDeclarations,
) {
    let mut module = 0;

    for item in items {
        match item {
            AstItem::TypeItem(_) => {}
            AstItem::SymbolItem(item) => {
                if let AstSymbolItem::ModuleItem(item) = item {
                    declare_module_symbols(ctx, &item, declarations.modules[module]);
                    module += 1;
                } else {
                    declarations.symbols.push(declare_symbol_item(ctx, &item));
                }
            }
            AstItem::ImportItem(_) => {}
        }
    }
}

pub fn resolve_symbol_imports(ctx: &mut Compiler, declarations: &ModuleDeclarations) {
    resolve_imports(ctx, true);

    for &symbol in &declarations.modules {
        let Symbol::Module(module) = ctx.symbol(symbol).clone() else {
            continue;
        };
        ctx.push_scope(module.scope, TextSize::from(0));
        resolve_symbol_imports(ctx, &module.declarations);
        ctx.pop_scope(TextSize::from(0));
    }
}

pub fn compile_type_items(
    ctx: &mut Compiler,
    items: impl Iterator<Item = AstItem>,
    declarations: &ModuleDeclarations,
) {
    let mut index = 0;
    let mut module = 0;

    for item in items {
        match item {
            AstItem::TypeItem(item) => {
                let (ty, scope) = declarations.types[index];
                compile_type_item(ctx, &item, ty, scope);
                index += 1;
            }
            AstItem::SymbolItem(item) => {
                if let AstSymbolItem::ModuleItem(item) = item {
                    compile_module_types(ctx, &item, declarations.modules[module]);
                    module += 1;
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
    let mut module = 0;

    for item in items {
        match item {
            AstItem::TypeItem(_) => {}
            AstItem::SymbolItem(item) => {
                if let AstSymbolItem::ModuleItem(item) = item {
                    compile_module_symbols(ctx, &item, declarations.modules[module]);
                    module += 1;
                } else {
                    let symbol = declarations.symbols[index];
                    compile_symbol_item(ctx, &item, symbol);
                    index += 1;
                }
            }
            AstItem::ImportItem(_) => {}
        }
    }
}
