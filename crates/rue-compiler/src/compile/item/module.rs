use rue_ast::AstModuleItem;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{ModuleSymbol, Scope, Symbol, SymbolId};

use crate::{Compiler, compile_items, declare_items};

pub fn declare_module(ctx: &mut Compiler, module: &AstModuleItem) -> SymbolId {
    let scope = ctx.alloc_scope(Scope::new());
    let declarations = declare_items(ctx, scope, module.items());

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

pub fn compile_module(ctx: &mut Compiler, module: &AstModuleItem, symbol: SymbolId) {
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

    compile_items(ctx, scope, module.items(), declarations);
}
