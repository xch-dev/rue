use rue_ast::AstNamedBinding;
use rue_diagnostic::DiagnosticKind;
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
    // Each pattern (a `let` binding, or a function's or lambda's parameter list) gets its own
    // scope, so a name that's already in it was bound earlier in the same pattern. A bare `_`
    // discards the value, so it can be repeated.
    if name.text() != "_" && ctx.last_scope().symbol(name.text()).is_some() {
        ctx.diagnostic(
            name,
            DiagnosticKind::DuplicateSymbol(name.text().to_string()),
        );

        // The duplicate can't be referenced, which is already reported, so it isn't unused too
        ctx.reference(Declaration::Symbol(symbol), None);
    } else {
        ctx.last_scope_mut()
            .insert_symbol(name.text().to_string(), symbol, false);
    }

    ctx.declaration_span(Declaration::Symbol(symbol), name.text_range());

    let name = ctx.local_name(name);

    match ctx.symbol_mut(symbol) {
        Symbol::Binding(binding) => binding.name = Some(name),
        Symbol::Parameter(parameter) => parameter.name = Some(name),
        _ => {}
    }
}
