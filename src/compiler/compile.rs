use crate::{AstDocument, AstFunction, AstItem, Context, ScopeId};

pub fn compile_document(ctx: &mut Context, scope: ScopeId, document: &AstDocument) {
    ctx.push_scope(scope);

    for item in document.items() {
        compile_item(ctx, &item);
    }

    ctx.pop_scope();
}

fn compile_item(ctx: &mut Context, item: &AstItem) {
    match item {
        AstItem::Function(function) => compile_function(ctx, function),
    }
}

fn compile_function(ctx: &mut Context, function: &AstFunction) {}
