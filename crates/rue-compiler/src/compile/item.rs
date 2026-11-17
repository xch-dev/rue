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
use rue_hir::{ScopeId, Symbol, SymbolId};
use rue_parser::SyntaxToken;
use rue_types::{Alias, Struct, Type, TypeId};

use crate::Compiler;

use rue_ast::{AstSymbolItem, AstTypeItem};

pub fn declare_type_item(ctx: &mut Compiler, item: &AstTypeItem) -> (TypeId, ScopeId) {
    match item {
        AstTypeItem::TypeAliasItem(type_alias) => declare_type_alias(ctx, type_alias),
        AstTypeItem::StructItem(struct_item) => declare_struct_item(ctx, struct_item),
    }
}

/// Adds a function or constant's name to the scope before any imports are resolved, so that an
/// import can never take a name that's declared locally, whatever order they're resolved in. The
/// symbol is filled in by [`declare_symbol_item`] once the types its signature can refer to have
/// been imported.
pub fn reserve_symbol_item(ctx: &mut Compiler, item: &AstSymbolItem) -> SymbolId {
    let symbol = ctx.alloc_symbol(Symbol::Unresolved);

    let (name, exported) = match item {
        AstSymbolItem::ModuleItem(_) => unreachable!(), // This is handled externally
        AstSymbolItem::FunctionItem(function) => (function.name(), function.export().is_some()),
        AstSymbolItem::ConstantItem(constant) => (constant.name(), constant.export().is_some()),
    };

    if let Some(name) = name {
        insert_local_symbol(ctx, &name, symbol, exported);
    }

    symbol
}

/// Adds a function or constant that's declared in the current scope, unless the name is already
/// taken by another value, in which case it's reported as a duplicate.
pub fn insert_local_symbol(
    ctx: &mut Compiler,
    name: &SyntaxToken,
    symbol: SymbolId,
    exported: bool,
) {
    if ctx.last_scope().symbol(name.text()).is_some() {
        ctx.diagnostic(
            name,
            DiagnosticKind::DuplicateSymbol(name.text().to_string()),
        );
    } else {
        ctx.last_scope_mut()
            .insert_symbol(name.text().to_string(), symbol, exported);
    }
}

/// Adds a module that's declared in the current scope, unless the name is already taken by
/// another module, in which case it's reported as a duplicate. Modules have their own namespace,
/// so a value or type can have the same name.
pub fn insert_local_module(
    ctx: &mut Compiler,
    name: &SyntaxToken,
    module: SymbolId,
    exported: bool,
) {
    if ctx.last_scope().module(name.text()).is_some() {
        ctx.diagnostic(
            name,
            DiagnosticKind::DuplicateSymbol(name.text().to_string()),
        );
    } else {
        ctx.last_scope_mut()
            .insert_module(name.text().to_string(), module, exported);
    }
}

pub fn declare_symbol_item(ctx: &mut Compiler, item: &AstSymbolItem, symbol: SymbolId) {
    match item {
        AstSymbolItem::ModuleItem(_) => unreachable!(), // This is handled externally
        AstSymbolItem::FunctionItem(function) => declare_function(ctx, function, symbol),
        AstSymbolItem::ConstantItem(constant) => declare_constant(ctx, constant, symbol),
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

    if check_contractive(ctx, item, ty) {
        ctx.push_unchecked_type_item(ty);
    }
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
/// Returns `false` if the type was rejected.
///
/// Every type item is checked as soon as it's compiled, and types that haven't been compiled yet
/// are unresolved, so the item that closes a cycle is reported if the cycle passes through it (or
/// through an application of it). A cycle that only passes through an earlier item, such as
/// `type T0 = Id<T0>;` followed by `type Id<T> = T;`, is caught by
/// [`recheck_contractive_types`] once every type item has been compiled.
fn check_contractive(ctx: &mut Compiler, item: &AstTypeItem, ty: TypeId) -> bool {
    if !rue_types::is_non_contractive(ctx.types_mut(), ty) {
        return true;
    }

    report_and_break_cycle(ctx, item, ty, DiagnosticKind::NonContractiveType);

    false
}

/// Checks the contractiveness of every type item compiled since the last call again, now that
/// the types they refer to have been compiled, and reports (and breaks the cycle of) each one
/// that can reach itself without passing through a pair. This must run before anything compares
/// the types, which is after every type item has been compiled but before struct defaults.
pub fn recheck_contractive_types(ctx: &mut Compiler) {
    // The items are checked together first, since checking each one separately would substitute
    // the types they share again for every item. That over-approximates, since an application of
    // an item may only be part of another item's cycle (such as `Id<Forward>` in
    // `type Forward = Id<Forward>;`), so each candidate is confirmed separately.
    let items = ctx.take_unchecked_type_items();
    let mut candidates = rue_types::non_contractive_types(ctx.types_mut(), &items);
    let mut index = 0;

    while let Some(&ty) = candidates.get(index) {
        index += 1;

        if !rue_types::is_non_contractive(ctx.types_mut(), ty) {
            continue;
        }

        let (Type::Alias(Alias { name, .. }) | Type::Struct(Struct { name, .. })) = ctx.ty(ty)
        else {
            unreachable!()
        };

        // An item without a name can't be referred to, so it can't be part of a cycle. Its cycle
        // would still be broken, without a diagnostic.
        if let Some(name) = name.clone() {
            let text = name.text().to_string();
            ctx.diagnostic_name(&name, DiagnosticKind::NonContractiveType(text));
        }

        break_cycle(ctx, ty);

        // Breaking the cycle may have broken those of the remaining candidates as well, in which
        // case they're only reported once (for example, `type A = Id<B>; type B = Id<A>;` followed
        // by `type Id<T> = T;`). Checking them together again avoids confirming each of them
        // separately when many items are part of the cycle that was broken.
        candidates = rue_types::non_contractive_types(ctx.types_mut(), &candidates[index..]);
        index = 0;
    }
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

    break_cycle(ctx, ty);
}

/// Replaces the inner type of the type item with an unresolved type, so that nothing can reach a
/// cycle through it after it has been reported.
fn break_cycle(ctx: &mut Compiler, ty: TypeId) {
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
