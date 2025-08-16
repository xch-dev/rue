use rue_ast::AstPathExpr;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Hir, Symbol, Value};

use crate::{Compiler, compile_generic_arguments};

pub fn compile_path_expr(ctx: &mut Compiler, path: &AstPathExpr) -> Value {
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

        let ty = match ctx.symbol(resolved) {
            Symbol::Binding(binding) => binding.ty,
            Symbol::Function(_) => todo!(),
            Symbol::Parameter(parameter) => parameter.ty,
        };

        value = Some(Value::unmapped(ctx.alloc_hir(Hir::Reference(resolved)), ty));
    }

    value.unwrap_or(ctx.builtins().unresolved.clone())
}
