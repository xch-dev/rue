use log::debug;
use rue_ast::AstFieldAccessExpr;
use rue_diagnostic::DiagnosticKind;
use rue_hir::Value;

use crate::{
    Compiler, Field, FieldResult, SyntaxField, SyntaxItem, SyntaxItemKind, compile_expr,
    compile_field,
};

pub fn compile_field_access_expr(ctx: &mut Compiler, access: &AstFieldAccessExpr) -> Value {
    let expr = if let Some(expr) = access.expr() {
        compile_expr(ctx, &expr, None)
    } else {
        debug!("Unresolved field access expr");
        ctx.builtins().unresolved.clone()
    };

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
