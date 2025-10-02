use log::debug;
use rue_ast::{AstListBinding, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{BindingSymbol, Declaration, Hir, Symbol, SymbolId, SymbolPath, Value};

use crate::{Compiler, compile_pair_fields, create_binding};

pub fn create_list_binding(ctx: &mut Compiler, symbol: SymbolId, list_binding: &AstListBinding) {
    let ty = ctx.symbol_type(symbol);

    let mut symbol = symbol;
    let mut reference = Value::new(ctx.alloc_hir(Hir::Reference(symbol)), ty)
        .with_reference(SymbolPath::new(symbol, vec![]));
    let mut needs_popping = 0;

    let len = list_binding.items().count();

    for (i, item) in list_binding.items().enumerate() {
        let mut is_cons = true;

        if let Some(spread) = item.spread() {
            if i == len - 1 {
                is_cons = false;
            } else {
                ctx.diagnostic(&spread, DiagnosticKind::InvalidSpread);
            }
        }

        let Some(binding) = item.binding() else {
            debug!("Unresolved list item binding");
            continue;
        };

        if !is_cons {
            create_binding(ctx, symbol, &binding);
            continue;
        }

        let (first_value, rest_value) = compile_pair_fields(ctx, binding.syntax(), &reference);

        let first_symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
            name: None,
            value: first_value,
            inline: true,
        }));
        ctx.push_declaration(Declaration::Symbol(first_symbol));
        ctx.reference(Declaration::Symbol(symbol));
        create_binding(ctx, first_symbol, &binding);
        ctx.pop_declaration();

        let rest_symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
            name: None,
            value: rest_value.clone(),
            inline: true,
        }));

        needs_popping += 1;
        ctx.push_declaration(Declaration::Symbol(rest_symbol));
        ctx.reference(Declaration::Symbol(symbol));

        symbol = rest_symbol;
        reference = rest_value;
    }

    for _ in 0..needs_popping {
        ctx.pop_declaration();
    }
}
