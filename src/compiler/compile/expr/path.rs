use crate::{AstPathExpr, Context, DiagnosticKind, Hir, Value, compile_generic_arguments};

pub fn compile_path_expr(ctx: &mut Context, path: &AstPathExpr) -> Value {
    let mut value = None;

    for segment in path.segments() {
        let Some(name) = segment.name() else {
            return ctx.builtins().unresolved.clone();
        };

        if value.is_some() {
            todo!();
        }

        if segment.separator().is_some() && value.is_some() {
            todo!();
        }

        let Some(resolved) = ctx.resolve_symbol(name.text()) else {
            ctx.diagnostic(
                &name,
                DiagnosticKind::UndeclaredSymbol(name.text().to_string()),
            );
            return ctx.builtins().unresolved.clone();
        };

        let args = if let Some(generic_arguments) = segment.generic_arguments() {
            compile_generic_arguments(ctx, &generic_arguments)
        } else {
            vec![]
        };

        assert!(args.is_empty());

        value = Some(Value::new(
            ctx.alloc_hir(Hir::Reference(resolved)),
            ctx.builtins().unresolved.ty,
        ));
    }

    value.unwrap_or(ctx.builtins().unresolved.clone())
}
