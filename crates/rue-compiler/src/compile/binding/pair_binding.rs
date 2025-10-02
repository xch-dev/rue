use rue_ast::{AstNode, AstPairBinding};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{
    BindingSymbol, Declaration, Hir, Symbol, SymbolId, SymbolPath, TypePath, UnaryOp, Value,
};
use rue_types::{Type, Union};

use crate::{Compiler, create_binding};

pub fn create_pair_binding(ctx: &mut Compiler, symbol: SymbolId, binding: &AstPairBinding) {
    let ty = ctx.symbol_type(symbol);
    let pairs = rue_types::extract_pairs(ctx.types_mut(), ty, true);

    if pairs.is_empty() {
        let name = ctx.type_name(ty);
        ctx.diagnostic(
            binding.syntax(),
            DiagnosticKind::CannotDestructurePair(name),
        );
        return;
    }

    let (first_ty, rest_ty) = if pairs.len() == 1 {
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

    if let Some(first) = binding.first() {
        let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::First, reference));
        let first_symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
            name: None,
            value: Value::new(hir, first_ty)
                .with_reference(SymbolPath::new(symbol, vec![TypePath::First])),
            inline: true,
        }));
        ctx.push_declaration(Declaration::Symbol(first_symbol));
        ctx.reference(Declaration::Symbol(symbol));
        create_binding(ctx, first_symbol, &first);
        ctx.pop_declaration();
    }

    if let Some(rest) = binding.rest() {
        let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::Rest, reference));
        let rest_symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
            name: None,
            value: Value::new(hir, rest_ty)
                .with_reference(SymbolPath::new(symbol, vec![TypePath::Rest])),
            inline: true,
        }));
        ctx.push_declaration(Declaration::Symbol(rest_symbol));
        ctx.reference(Declaration::Symbol(symbol));
        create_binding(ctx, rest_symbol, &rest);
        ctx.pop_declaration();
    }
}
