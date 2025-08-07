use crate::{
    AstDocument, AstFunction, AstItem, Context, ErrorKind, FunctionSymbol, ScopeId, Symbol,
};

pub fn declare_document(ctx: &mut Context, scope: ScopeId, document: &AstDocument) {
    ctx.push_scope(scope);

    for item in document.items() {
        declare_item(ctx, &item);
    }

    ctx.pop_scope();
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

    if ctx.last_scope().symbol(name.text()).is_some() {
        ctx.error(&name, ErrorKind::DuplicateSymbol(name.text().to_string()));
        return;
    }

    let symbol = ctx.alloc_symbol(Symbol::Function(FunctionSymbol { name: name.clone() }));

    ctx.last_scope_mut()
        .insert_symbol(name.text().to_string(), symbol);
}
