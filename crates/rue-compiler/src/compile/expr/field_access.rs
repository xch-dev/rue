use log::debug;
use rowan::TextRange;
use rue_ast::{AstFieldAccessExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::Value;

use crate::{
    Compiler, CompletionContext, Field, FieldResult, SyntaxField, SyntaxItem, SyntaxItemKind,
    compile_expr, compile_field,
};

pub fn compile_field_access_expr(ctx: &mut Compiler, access: &AstFieldAccessExpr) -> Value {
    let mut start = access.syntax().text_range().start();

    let expr = if let Some(expr) = access.expr() {
        start = expr.syntax().text_range().end();
        compile_expr(ctx, &expr, None)
    } else {
        debug!("Unresolved field access expr");
        ctx.builtins().unresolved.clone()
    };

    ctx.syntax_map_mut().add_item(SyntaxItem::new(
        SyntaxItemKind::CompletionContext(CompletionContext::StructFields {
            ty: expr.ty,
            specified_fields: None,
        }),
        TextRange::new(start, access.syntax().text_range().end()),
    ));

    let Some(name) = access.field() else {
        debug!("Unresolved field access name");
        return ctx.builtins().unresolved.clone();
    };

    match compile_field(ctx, expr.clone(), &Field::Named(name.text())) {
        FieldResult::Value(value) => {
            ctx.syntax_map_mut().add_item(SyntaxItem::new(
                SyntaxItemKind::FieldReference(SyntaxField {
                    name: name.text().to_string(),
                    container: expr.ty,
                    ty: value.ty,
                }),
                name.text_range(),
            ));
            value
        }
        FieldResult::Unknown => {
            debug!("Unresolved field access due to unknown field");
            let type_name = ctx.type_name(expr.ty);
            ctx.diagnostic(
                &name,
                DiagnosticKind::UnknownField(name.text().to_string(), type_name),
            );
            ctx.builtins().unresolved.clone()
        }
        FieldResult::Error => {
            debug!("Unresolved field access due to missing field in underlying struct type");
            let type_name = ctx.type_name(expr.ty);
            ctx.diagnostic(
                &name,
                DiagnosticKind::MissingField(name.text().to_string(), type_name),
            );
            ctx.builtins().unresolved.clone()
        }
    }
}
