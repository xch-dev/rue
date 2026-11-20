use log::debug;
use rowan::TextRange;
use rue_ast::{AstFieldAccessExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::Value;

use crate::{
    Compiler, CompletionContext, Field, FieldResult, SyntaxField, SyntaxItemKind, compile_expr,
    compile_field, field_container,
};

pub fn compile_field_access_expr(ctx: &mut Compiler, access: &AstFieldAccessExpr) -> Value {
    let expr = if let Some(expr) = access.expr() {
        compile_expr(ctx, &expr, None)
    } else {
        debug!("Unresolved field access expr");
        ctx.unresolved()
    };

    let Some(dot) = access.dot() else {
        debug!("Unresolved field access dot");
        return ctx.unresolved();
    };

    ctx.add_syntax(
        SyntaxItemKind::CompletionContext(CompletionContext::StructFields {
            ty: expr.ty,
            specified_fields: None,
        }),
        TextRange::new(dot.text_range().end(), access.syntax().text_range().end()),
    );

    let Some(name) = access.field() else {
        debug!("Unresolved field access name");
        return ctx.unresolved();
    };

    match compile_field(ctx, expr.clone(), &Field::Named(name.text())) {
        FieldResult::Value(value) => {
            let container = field_container(ctx, expr.ty);
            ctx.add_syntax(
                SyntaxItemKind::FieldReference(SyntaxField {
                    name: name.text().to_string(),
                    container,
                    ty: value.ty,
                }),
                name.text_range(),
            );
            value
        }
        FieldResult::Unknown => {
            debug!("Unresolved field access due to unknown field");
            let type_name = ctx.type_name(expr.ty);
            ctx.diagnostic(
                &name,
                DiagnosticKind::UnknownField(name.text().to_string(), type_name),
            );
            ctx.unresolved()
        }
        FieldResult::Error => {
            debug!("Unresolved field access due to missing field in underlying struct type");
            let type_name = ctx.type_name(expr.ty);
            ctx.diagnostic(
                &name,
                DiagnosticKind::MissingField(name.text().to_string(), type_name),
            );
            ctx.unresolved()
        }
    }
}
