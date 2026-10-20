use log::debug;
use rue_ast::{AstConstExpr, AstNode};
use rue_diagnostic::SrcLoc;
use rue_hir::{ConstExpr, Hir};
use rue_types::TypeId;

use crate::{BlockKind, Compiler, compile_block};

pub fn compile_const_expr(
    ctx: &mut Compiler,
    expr: &AstConstExpr,
    expected_type: Option<TypeId>,
) -> rue_hir::Value {
    let value = if let Some(block) = expr.block() {
        compile_block(ctx, &block, BlockKind::Expr, expected_type, true)
    } else {
        debug!("Unresolved const expression block");
        ctx.builtins().unresolved.clone()
    };

    let loc = SrcLoc::new(ctx.source().clone(), expr.syntax().text_range().into());
    let hir = ctx.alloc_hir(Hir::Const(ConstExpr {
        value: value.hir,
        loc,
    }));

    value.with_hir(hir)
}
