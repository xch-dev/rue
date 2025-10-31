use rue_ast::{AstModuleItem, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Declaration, ModuleDeclarations, ModuleSymbol, Symbol, SymbolId};

use crate::{
    Compiler, CompletionContext, SyntaxItem, SyntaxItemKind, compile_symbol_items,
    compile_type_items, declare_import_items, declare_symbol_items, declare_type_items,
};

pub fn declare_module_imports(ctx: &mut Compiler, module: &AstModuleItem) -> SymbolId {
    ctx.syntax_map_mut().add_item(SyntaxItem::new(
        SyntaxItemKind::CompletionContext(CompletionContext::Item),
        module.syntax().text_range(),
    ));

    let scope = ctx.alloc_child_scope();

    let mut declarations = ModuleDeclarations::default();

    let range = module.syntax().text_range();
    ctx.push_scope(scope, range.start());
    declare_import_items(ctx, module.items(), &mut declarations);
    ctx.pop_scope(range.end());

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
    declare_type_items(ctx, module.items(), &mut declarations);
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
    declare_symbol_items(ctx, module.items(), &mut declarations);
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
    compile_type_items(ctx, module.items(), &declarations);
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
    compile_symbol_items(ctx, module.items(), &declarations);
    ctx.pop_scope(range.end());
}
