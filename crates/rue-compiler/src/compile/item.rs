mod function;
mod struct_item;
mod type_alias;

pub use function::*;
pub use struct_item::*;
pub use type_alias::*;

use rue_hir::{ScopeId, SymbolId};
use rue_types::TypeId;

use crate::Compiler;

use rue_ast::{AstSymbolItem, AstTypeItem};

pub fn declare_type_item(ctx: &mut Compiler, item: &AstTypeItem) -> (TypeId, ScopeId) {
    match item {
        AstTypeItem::TypeAliasItem(type_alias) => declare_type_alias(ctx, type_alias),
        AstTypeItem::StructItem(struct_item) => declare_struct_item(ctx, struct_item),
    }
}

pub fn declare_symbol_item(ctx: &mut Compiler, item: &AstSymbolItem) -> SymbolId {
    match item {
        AstSymbolItem::FunctionItem(function) => declare_function(ctx, function),
    }
}

pub fn compile_type_item(ctx: &mut Compiler, item: &AstTypeItem, ty: TypeId, scope: ScopeId) {
    match item {
        AstTypeItem::TypeAliasItem(type_alias) => compile_type_alias(ctx, type_alias, ty, scope),
        AstTypeItem::StructItem(struct_item) => compile_struct_item(ctx, struct_item, ty, scope),
    }
}

pub fn compile_symbol_item(ctx: &mut Compiler, item: &AstSymbolItem, symbol: SymbolId) {
    match item {
        AstSymbolItem::FunctionItem(function) => compile_function(ctx, function, symbol),
    }
}
