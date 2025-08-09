use crate::{Context, Hir, HirId, Mir, MirId, Symbol, SymbolId};

pub fn lower_hir(ctx: &mut Context, hir: HirId) -> MirId {
    match ctx.hir(hir).clone() {
        Hir::Unresolved => ctx.builtins().unresolved_mir,
        Hir::Atom(atom) => ctx.alloc_mir(Mir::Atom(atom)),
        Hir::Reference(symbol) => lower_reference(ctx, symbol),
        Hir::Unary(op, hir) => {
            let mir = lower_hir(ctx, hir);
            ctx.alloc_mir(Mir::Unary(op, mir))
        }
        Hir::Binary(op, left, right) => {
            let left = lower_hir(ctx, left);
            let right = lower_hir(ctx, right);
            ctx.alloc_mir(Mir::Binary(op, left, right))
        }
        Hir::Block(block) => {
            assert!(block.statements.is_empty());
            let hir = block.body.unwrap().hir;
            lower_hir(ctx, hir)
        }
    }
}

pub fn lower_reference(ctx: &mut Context, symbol: SymbolId) -> MirId {
    match ctx.symbol(symbol).clone() {
        Symbol::Function(function) => lower_hir(ctx, function.body),
        Symbol::Parameter(parameter) => {
            todo!();
        }
        _ => todo!(),
    }
}
