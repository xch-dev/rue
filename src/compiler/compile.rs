use crate::{AstDocument, AstFunction, AstItem, Context};

pub fn compile_document(ctx: &mut Context, document: &AstDocument) {
    ctx.push_scope();

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
