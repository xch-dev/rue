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

use rue_ast::AstNode;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{ScopeId, SymbolId};
use rue_types::{Alias, Struct, Type, TypeId};

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

    check_contractive(ctx, item, ty);
}

/// Rejects a type item that can reach itself without passing through a pair, such as
/// `type A = B; type B = A;`. Without this, the type comparison logic would treat the cycle as
/// assignable to anything (or overflow the stack), so the cycle is broken after reporting it.
///
/// Every type item is checked as soon as it's compiled, and types that haven't been compiled yet
/// are unresolved, so the item that closes a cycle is always the one that gets reported. This
/// keeps non-contractive types from ever reaching code that uses previously compiled types.
fn check_contractive(ctx: &mut Compiler, item: &AstTypeItem, ty: TypeId) {
    if !rue_types::is_non_contractive(ctx.types_mut(), ty) {
        return;
    }

    let name = match item {
        AstTypeItem::TypeAliasItem(type_alias) => type_alias.name(),
        AstTypeItem::StructItem(struct_item) => struct_item.name(),
    };

    let text = name
        .as_ref()
        .map_or_else(|| ctx.type_name(ty), |name| name.text().to_string());

    if let Some(name) = name {
        ctx.diagnostic(&name, DiagnosticKind::NonContractiveType(text));
    } else {
        ctx.diagnostic(item.syntax(), DiagnosticKind::NonContractiveType(text));
    }

    let (Type::Alias(Alias { inner, .. }) | Type::Struct(Struct { inner, .. })) = ctx.ty(ty) else {
        unreachable!()
    };

    let inner = *inner;
    let unresolved = ctx.builtins().unresolved.ty;

    *ctx.ty_mut(inner) = Type::Ref(unresolved);
}

pub fn compile_symbol_item(ctx: &mut Compiler, item: &AstSymbolItem, symbol: SymbolId) {
    match item {
        AstSymbolItem::ModuleItem(_) => unreachable!(), // This is handled externally
        AstSymbolItem::FunctionItem(function) => compile_function(ctx, function, symbol),
        AstSymbolItem::ConstantItem(constant) => compile_constant(ctx, constant, symbol),
    }
}
