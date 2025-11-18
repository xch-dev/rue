use rue_ast::{AstModuleItem, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Declaration, ModuleDeclarations, ModuleSymbol, Symbol, SymbolId};

use crate::{
    Compiler, CompletionContext, SyntaxItemKind, compile_symbol_items, compile_type_items,
    declare_module_items, declare_symbol_items, declare_type_items,
};

pub fn declare_module(ctx: &mut Compiler, module: &AstModuleItem) -> SymbolId {
    ctx.add_syntax(
        SyntaxItemKind::CompletionContext(CompletionContext::Item),
        module.syntax().text_range(),
    );

    let scope = ctx.alloc_child_scope();

    let name = module.name().map(|name| ctx.local_name(&name));

    let symbol = ctx.alloc_symbol(Symbol::Module(ModuleSymbol {
        name,
        scope,
        declarations: ModuleDeclarations::default(),
    }));

    let mut declarations = ModuleDeclarations::default();

    let range = module.syntax().text_range();
    ctx.push_scope(scope, range.start());
    // ctx.push_module(symbol);
    declare_module_items(ctx, module.items(), &mut declarations);
    // ctx.pop_module();
    ctx.pop_scope(range.end());

    ctx.module_mut(symbol).declarations = declarations;

    if let Some(name) = module.name() {
        if ctx.last_scope().symbol(name.text()).is_some() {
            ctx.diagnostic(
                &name,
                DiagnosticKind::DuplicateSymbol(name.text().to_string()),
            );
        } else {
            ctx.last_scope_mut().insert_symbol(
                name.text().to_string(),
                symbol,
                module.export().is_some(),
            );
        }

        ctx.declaration_span(Declaration::Symbol(symbol), name.text_range());
    }

    symbol
}

pub fn declare_module_types(ctx: &mut Compiler, module: &AstModuleItem, symbol: SymbolId) {
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

    let range = module.syntax().text_range();
    ctx.push_scope(scope, range.start());
    // ctx.push_module(symbol);
    declare_type_items(ctx, module.items(), &mut declarations);
    // ctx.pop_module();
    ctx.pop_scope(range.end());

    let Symbol::Module(ModuleSymbol {
        declarations: updated,
        ..
    }) = ctx.symbol_mut(symbol)
    else {
        unreachable!();
    };

    *updated = declarations;
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

    let range = module.syntax().text_range();
    ctx.push_scope(scope, range.start());
    // ctx.push_module(symbol);
    declare_symbol_items(ctx, module.items(), &mut declarations);
    // ctx.pop_module();
    ctx.pop_scope(range.end());

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

    let range = module.syntax().text_range();
    ctx.push_scope(scope, range.start());
    // ctx.push_module(symbol);
    compile_type_items(ctx, module.items(), &declarations);
    // ctx.pop_module();
    ctx.pop_scope(range.end());
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

    let range = module.syntax().text_range();
    ctx.push_scope(scope, range.start());
    // ctx.push_module(symbol);
    compile_symbol_items(ctx, module.items(), &declarations);
    // ctx.pop_module();
    ctx.pop_scope(range.end());
}
