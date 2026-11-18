use rue_ast::{AstModuleItem, AstNode};
use rue_hir::{Declaration, ModuleDeclarations, ModuleSymbol, Scope, Symbol, SymbolId};

use crate::{
    Compiler, CompletionContext, SyntaxItemKind, compile_default_items, compile_symbol_items,
    compile_type_items, declare_module_items, declare_symbol_items, declare_type_items,
    insert_local_module,
};

pub fn declare_module(ctx: &mut Compiler, module: &AstModuleItem) -> SymbolId {
    ctx.add_syntax(
        SyntaxItemKind::CompletionContext(CompletionContext::Item),
        module.syntax().text_range(),
    );

    let outer = ctx.inline_module_parent_scope();
    let scope = ctx.alloc_scope(Scope::new(Some(outer)));
    let parent = ctx.current_module();

    let name = module.name().map(|name| ctx.local_name(&name));

    let symbol = ctx.alloc_symbol(Symbol::Module(ModuleSymbol {
        name,
        scope,
        parent: Some(parent),
        declarations: ModuleDeclarations::default(),
    }));

    in_module(ctx, module, symbol, |ctx, declarations| {
        declare_module_items(ctx, module.items(), declarations);
    });

    if ctx.source().kind.check_unused() {
        let srcloc = ctx.srcloc(module.syntax());
        ctx.lint_sites_mut().modules.push((symbol, srcloc));
    }

    if let Some(name) = module.name() {
        insert_local_module(ctx, &name, symbol, module.export().is_some());
        ctx.declaration_span(Declaration::Symbol(symbol), name.text_range());
    }

    symbol
}

pub fn declare_module_types(ctx: &mut Compiler, module: &AstModuleItem, symbol: SymbolId) {
    in_module(ctx, module, symbol, |ctx, declarations| {
        declare_type_items(ctx, module.items(), declarations);
    });
}

pub fn declare_module_symbols(ctx: &mut Compiler, module: &AstModuleItem, symbol: SymbolId) {
    in_module(ctx, module, symbol, |ctx, declarations| {
        declare_symbol_items(ctx, module.items(), declarations);
    });
}

pub fn compile_module_types(ctx: &mut Compiler, module: &AstModuleItem, symbol: SymbolId) {
    in_module(ctx, module, symbol, |ctx, declarations| {
        compile_type_items(ctx, module.items(), declarations);
    });
}

pub fn compile_module_defaults(ctx: &mut Compiler, module: &AstModuleItem, symbol: SymbolId) {
    in_module(ctx, module, symbol, |ctx, declarations| {
        compile_default_items(ctx, module.items(), declarations);
    });
}

pub fn compile_module_symbols(ctx: &mut Compiler, module: &AstModuleItem, symbol: SymbolId) {
    in_module(ctx, module, symbol, |ctx, declarations| {
        compile_symbol_items(ctx, module.items(), declarations);
    });
}

/// Runs a pass over the items of an inline module, in its scope and with its declarations, which
/// the pass can add to.
fn in_module(
    ctx: &mut Compiler,
    module: &AstModuleItem,
    symbol: SymbolId,
    pass: impl FnOnce(&mut Compiler, &mut ModuleDeclarations),
) {
    let scope = ctx.module(symbol).scope;
    let mut declarations = ctx.module(symbol).declarations.clone();

    let range = module.syntax().text_range();
    ctx.push_scope(scope, range.start());
    ctx.push_inline_module(symbol);
    pass(ctx, &mut declarations);
    ctx.pop_inline_module();
    ctx.pop_scope(range.end());

    ctx.module_mut(symbol).declarations = declarations;
}
