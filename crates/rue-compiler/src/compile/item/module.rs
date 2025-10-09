use rue_ast::AstModuleItem;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{ModuleDeclarations, ModuleSymbol, Scope, Symbol, SymbolId};

use crate::{
    Compiler, compile_symbol_items, compile_type_items, declare_symbol_items, declare_type_items,
};

pub fn declare_module_types(ctx: &mut Compiler, module: &AstModuleItem) -> SymbolId {
    let scope = ctx.alloc_scope(Scope::new());

    let mut declarations = ModuleDeclarations::default();

    ctx.push_scope(scope);
    declare_type_items(ctx, module.items(), &mut declarations);
    ctx.pop_scope();

    let symbol = ctx.alloc_symbol(Symbol::Module(ModuleSymbol {
        name: module.name(),
        scope,
        declarations,
    }));

    if let Some(name) = module.name() {
        if ctx.last_scope().symbol(name.text()).is_some() {
            ctx.diagnostic(
                &name,
                DiagnosticKind::DuplicateSymbol(name.text().to_string()),
            );
        }

        ctx.last_scope_mut().insert_symbol(
            name.text().to_string(),
            symbol,
            module.export().is_some(),
        );
    }

    symbol
}

pub fn declare_module_symbols(ctx: &mut Compiler, module: &AstModuleItem, symbol: SymbolId) {
    let (scope, mut declarations) = if let Symbol::Module(ModuleSymbol {
        scope,
        declarations,
        ..
    }) = ctx.symbol(symbol)
    {
        (*scope, declarations.clone())
    } else {
        unreachable!();
    };

    ctx.push_scope(scope);
    declare_symbol_items(ctx, module.items(), &mut declarations);
    ctx.pop_scope();

    let Symbol::Module(ModuleSymbol {
        declarations: updated,
        ..
    }) = ctx.symbol_mut(symbol)
    else {
        unreachable!();
    };

    *updated = declarations;
}

pub fn compile_module_types(ctx: &mut Compiler, module: &AstModuleItem, symbol: SymbolId) {
    let (scope, declarations) = if let Symbol::Module(ModuleSymbol {
        scope,
        declarations,
        ..
    }) = ctx.symbol(symbol)
    {
        (*scope, declarations.clone())
    } else {
        unreachable!();
    };

    ctx.push_scope(scope);
    compile_type_items(ctx, module.items(), &declarations);
    ctx.pop_scope();
}

pub fn compile_module_symbols(ctx: &mut Compiler, module: &AstModuleItem, symbol: SymbolId) {
    let (scope, declarations) = if let Symbol::Module(ModuleSymbol {
        scope,
        declarations,
        ..
    }) = ctx.symbol(symbol)
    {
        (*scope, declarations.clone())
    } else {
        unreachable!();
    };

    ctx.push_scope(scope);
    compile_symbol_items(ctx, module.items(), &declarations);
    ctx.pop_scope();
}
