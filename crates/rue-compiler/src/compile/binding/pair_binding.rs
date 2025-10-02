use rue_ast::{AstNode, AstPairBinding};
use rue_hir::{BindingSymbol, Declaration, Hir, Symbol, SymbolId, SymbolPath, Value};

use crate::{Compiler, compile_pair_fields, create_binding};

pub fn create_pair_binding(ctx: &mut Compiler, symbol: SymbolId, binding: &AstPairBinding) {
    let ty = ctx.symbol_type(symbol);

    let reference = Value::new(ctx.alloc_hir(Hir::Reference(symbol)), ty)
        .with_reference(SymbolPath::new(symbol, vec![]));

    let (first_value, rest_value) = compile_pair_fields(ctx, binding.syntax(), &reference);

    if let Some(first) = binding.first() {
        let first_symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
            name: None,
            value: first_value,
            inline: true,
        }));
        ctx.push_declaration(Declaration::Symbol(first_symbol));
        ctx.reference(Declaration::Symbol(symbol));
        create_binding(ctx, first_symbol, &first);
        ctx.pop_declaration();
    }

    if let Some(rest) = binding.rest() {
        let rest_symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
            name: None,
            value: rest_value,
            inline: true,
        }));
        ctx.push_declaration(Declaration::Symbol(rest_symbol));
        ctx.reference(Declaration::Symbol(symbol));
        create_binding(ctx, rest_symbol, &rest);
        ctx.pop_declaration();
    }
}
