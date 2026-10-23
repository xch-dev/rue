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

    if !check_regular(ctx, item, ty) {
        return;
    }

    check_contractive(ctx, item, ty);
}

pub fn compile_type_item_defaults(
    ctx: &mut Compiler,
    item: &AstTypeItem,
    ty: TypeId,
    scope: ScopeId,
) {
    if let AstTypeItem::StructItem(struct_item) = item {
        compile_struct_defaults(ctx, struct_item, ty, scope);
    }
}

/// Rejects a type item that refers to itself with generic arguments built from its own generic
/// parameters, such as `type L<T> = nil | (T, L<(T, T)>);`. Each level of such a type has new
/// generic arguments, so it can't be substituted into a finite type. The cycle is broken after
/// reporting it, and this must happen before anything substitutes the type (including the
/// contractivity check). Returns `false` if the type was rejected.
fn check_regular(ctx: &mut Compiler, item: &AstTypeItem, ty: TypeId) -> bool {
    if !rue_types::is_non_regular(ctx.types(), ty) {
        return true;
    }

    report_and_break_cycle(ctx, item, ty, DiagnosticKind::NonRegularType);

    false
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

    report_and_break_cycle(ctx, item, ty, DiagnosticKind::NonContractiveType);
}

fn report_and_break_cycle(
    ctx: &mut Compiler,
    item: &AstTypeItem,
    ty: TypeId,
    kind: fn(String) -> DiagnosticKind,
) {
    let name = match item {
        AstTypeItem::TypeAliasItem(type_alias) => type_alias.name(),
        AstTypeItem::StructItem(struct_item) => struct_item.name(),
    };

    let text = name
        .as_ref()
        .map_or_else(|| ctx.type_name(ty), |name| name.text().to_string());

    if let Some(name) = name {
        ctx.diagnostic(&name, kind(text));
    } else {
        ctx.diagnostic(item.syntax(), kind(text));
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
