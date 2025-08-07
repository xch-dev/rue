mod function;
mod type_alias;

pub use function::*;
pub use type_alias::*;

use crate::{AstSymbolItem, AstTypeItem, Context, SymbolId, TypeId};

pub fn declare_type_item(ctx: &mut Context, item: &AstTypeItem) -> TypeId {
    match item {
        AstTypeItem::TypeAliasItem(type_alias) => declare_type_alias(ctx, type_alias),
    }
}

pub fn declare_symbol_item(ctx: &mut Context, item: &AstSymbolItem) -> SymbolId {
    match item {
        AstSymbolItem::FunctionItem(function) => declare_function(ctx, function),
    }
}

pub fn compile_type_item(ctx: &mut Context, item: &AstTypeItem, ty: TypeId) {
    match item {
        AstTypeItem::TypeAliasItem(type_alias) => compile_type_alias(ctx, type_alias, ty),
    }
}

pub fn compile_symbol_item(ctx: &mut Context, item: &AstSymbolItem, symbol: SymbolId) {
    match item {
        AstSymbolItem::FunctionItem(function) => compile_function(ctx, function, symbol),
    }
}
