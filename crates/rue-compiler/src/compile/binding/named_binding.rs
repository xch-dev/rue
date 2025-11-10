use rue_ast::AstNamedBinding;
use rue_hir::{Declaration, Symbol, SymbolId};
use rue_parser::SyntaxToken;

use crate::Compiler;

pub fn create_named_binding(ctx: &mut Compiler, symbol: SymbolId, binding: &AstNamedBinding) {
    let Some(name) = binding.name() else {
        return;
    };

    create_binding_for_identifier(ctx, symbol, &name);
}

pub fn create_binding_for_identifier(ctx: &mut Compiler, symbol: SymbolId, name: &SyntaxToken) {
    ctx.last_scope_mut()
        .insert_symbol(name.text().to_string(), symbol, false);

    ctx.declaration_span(Declaration::Symbol(symbol), name.text_range());

    let name = ctx.local_name(name);

    match ctx.symbol_mut(symbol) {
        Symbol::Binding(binding) => binding.name = Some(name),
        Symbol::Parameter(parameter) => parameter.name = Some(name),
        _ => {}
    }
}
