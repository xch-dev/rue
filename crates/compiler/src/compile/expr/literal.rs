use rue_ast::AstLiteralExpr;
use rue_hir::{Hir, Value};
use rue_parser::{SyntaxKind, T};

use crate::Compiler;

pub fn compile_literal_expr(ctx: &mut Compiler, expr: &AstLiteralExpr) -> Value {
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
                ctx.alloc_hir(Hir::Atom(text.as_bytes().to_vec())),
                ctx.builtins().bytes,
            )
        }
        SyntaxKind::Hex => todo!(),
        SyntaxKind::Integer => todo!(),
        SyntaxKind::Ident => ctx.builtins().unresolved.clone(),
        T![nil] => todo!(),
        T![true] => todo!(),
        T![false] => todo!(),
        _ => unreachable!(),
    }
}
