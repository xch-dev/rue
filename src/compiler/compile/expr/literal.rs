use crate::{AstLiteralExpr, Context, HirId, SyntaxKind, T};

pub fn compile_literal_expr(ctx: &mut Context, expr: &AstLiteralExpr) -> HirId {
    let Some(value) = expr.value() else {
        return ctx.builtins().unresolved_hir;
    };

    match value.kind() {
        SyntaxKind::String => todo!(),
        SyntaxKind::Hex => todo!(),
        SyntaxKind::Integer => todo!(),
        SyntaxKind::Ident => todo!(),
        T![nil] => todo!(),
        T![true] => todo!(),
        T![false] => todo!(),
        _ => unreachable!(),
    }
}
