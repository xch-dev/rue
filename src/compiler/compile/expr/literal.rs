use crate::{AstLiteralExpr, Atom, Context, Hir, SyntaxKind, T, Value};

pub fn compile_literal_expr(ctx: &mut Context, expr: &AstLiteralExpr) -> Value {
    let Some(value) = expr.value() else {
        return ctx.builtins().unresolved.clone();
    };

    match value.kind() {
        SyntaxKind::String => {
            let mut text = value.text();

            if let Some(stripped) = text.strip_prefix('"') {
                text = stripped;
            }

            if let Some(stripped) = text.strip_suffix('"') {
                text = stripped;
            }

            Value::new(
                ctx.alloc_hir(Hir::Atom(Atom::String(text.to_string()))),
                ctx.builtins().bytes,
            )
        }
        SyntaxKind::Hex => todo!(),
        SyntaxKind::Integer => todo!(),
        SyntaxKind::Ident => todo!(),
        T![nil] => todo!(),
        T![true] => todo!(),
        T![false] => todo!(),
        _ => unreachable!(),
    }
}
