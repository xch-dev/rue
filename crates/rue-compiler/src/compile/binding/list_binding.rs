use log::debug;
use rue_ast::{AstListBinding, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{
    BindingSymbol, Declaration, Hir, Symbol, SymbolId, SymbolPath, TypePath, UnaryOp, Value,
};
use rue_types::{Type, Union};

use crate::{Compiler, create_binding};

pub fn create_list_binding(ctx: &mut Compiler, symbol: SymbolId, list_binding: &AstListBinding) {
    let mut symbol = symbol;
    let mut ty = ctx.symbol_type(symbol);
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

        let pairs = rue_types::extract_pairs(ctx.types_mut(), ty, true);

        if pairs.is_empty() {
            let name = ctx.type_name(ty);
            ctx.diagnostic(
                binding.syntax(),
                DiagnosticKind::CannotDestructurePair(name),
            );
            break;
        }

        let (first, rest) = if pairs.len() == 1 {
            (pairs[0].first, pairs[0].rest)
        } else {
            let first = ctx.alloc_type(Type::Union(Union::new(
                pairs.iter().map(|pair| pair.first).collect(),
            )));
            let rest = ctx.alloc_type(Type::Union(Union::new(
                pairs.iter().map(|pair| pair.rest).collect(),
            )));
            (first, rest)
        };

        let reference = ctx.alloc_hir(Hir::Reference(symbol));

        let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::First, reference));
        let first_symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
            name: None,
            value: Value::new(hir, first)
                .with_reference(SymbolPath::new(symbol, vec![TypePath::First])),
            inline: true,
        }));
        ctx.push_declaration(Declaration::Symbol(first_symbol));
        ctx.reference(Declaration::Symbol(symbol));
        create_binding(ctx, first_symbol, &binding);
        ctx.pop_declaration();

        let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::Rest, reference));
        let rest_symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
            name: None,
            value: Value::new(hir, rest)
                .with_reference(SymbolPath::new(symbol, vec![TypePath::Rest])),
            inline: true,
        }));

        needs_popping += 1;
        ctx.push_declaration(Declaration::Symbol(rest_symbol));
        ctx.reference(Declaration::Symbol(symbol));

        symbol = rest_symbol;
        ty = rest;
    }

    for _ in 0..needs_popping {
        ctx.pop_declaration();
    }
}
