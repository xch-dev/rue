mod constant;
mod function;
mod module;
mod struct_item;
mod type_alias;

pub use constant::*;
pub use function::*;
pub use module::*;
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
        AstSymbolItem::ModuleItem(_) => unreachable!(), // This is handled externally
        AstSymbolItem::FunctionItem(function) => declare_function(ctx, function),
        AstSymbolItem::ConstantItem(constant) => declare_constant(ctx, constant),
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
        AstSymbolItem::ModuleItem(_) => unreachable!(), // This is handled externally
        AstSymbolItem::FunctionItem(function) => compile_function(ctx, function, symbol),
        AstSymbolItem::ConstantItem(constant) => compile_constant(ctx, constant, symbol),
    }
}
