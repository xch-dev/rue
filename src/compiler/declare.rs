use crate::{AstDocument, AstFunction, AstItem, Context, ErrorKind, FunctionSymbol, Symbol};

pub fn declare_document(ctx: &mut Context, document: &AstDocument) {
    for item in document.items() {
        declare_item(ctx, &item);
    }
}

fn declare_item(ctx: &mut Context, item: &AstItem) {
    match item {
        AstItem::Function(function) => declare_function(ctx, function),
    }
}

fn declare_function(ctx: &mut Context, function: &AstFunction) {
    let Some(name) = function.name() else {
        return;
    };

    if ctx.scope().symbol(name.text()).is_some() {
        ctx.error(&name, ErrorKind::DuplicateSymbol(name.text().to_string()));
        return;
    }

    let symbol = ctx.alloc_symbol(Symbol::Function(FunctionSymbol { name: name.clone() }));

    ctx.scope_mut()
        .insert_symbol(name.text().to_string(), symbol);
}
