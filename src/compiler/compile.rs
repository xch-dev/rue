use crate::{
    AstDocument, AstFunctionItem, AstItem, AstSymbolItem, AstTypeAliasItem, AstTypeItem, Context,
    ErrorKind, FunctionSymbol, ScopeId, Symbol, SymbolId, Type, TypeId, UnresolvedType,
};

#[derive(Debug, Default)]
pub struct DocumentDeclarations {
    types: Vec<TypeId>,
    symbols: Vec<SymbolId>,
}

pub fn declare_document(
    ctx: &mut Context,
    scope: ScopeId,
    document: &AstDocument,
) -> DocumentDeclarations {
    let mut declarations = DocumentDeclarations::default();

    ctx.push_scope(scope);

    for item in document.items() {
        match item {
            AstItem::TypeItem(item) => declarations.types.push(declare_type_item(ctx, &item)),
            AstItem::SymbolItem(_) => {}
        }
    }

    for item in document.items() {
        match item {
            AstItem::TypeItem(_) => {}
            AstItem::SymbolItem(item) => declarations.symbols.push(declare_symbol_item(ctx, &item)),
        }
    }

    ctx.pop_scope();

    declarations
}

fn declare_type_item(ctx: &mut Context, item: &AstTypeItem) -> TypeId {
    match item {
        AstTypeItem::TypeAliasItem(type_alias) => declare_type_alias(ctx, type_alias),
    }
}

fn declare_symbol_item(ctx: &mut Context, item: &AstSymbolItem) -> SymbolId {
    match item {
        AstSymbolItem::FunctionItem(function) => declare_function(ctx, function),
    }
}

fn declare_function(ctx: &mut Context, function: &AstFunctionItem) -> SymbolId {
    let symbol = ctx.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: function.name(),
    }));

    if let Some(name) = function.name() {
        if ctx.last_scope().symbol(name.text()).is_some() {
            ctx.error(&name, ErrorKind::DuplicateSymbol(name.text().to_string()));
        }

        ctx.last_scope_mut()
            .insert_symbol(name.text().to_string(), symbol);
    }

    symbol
}

fn declare_type_alias(ctx: &mut Context, type_alias: &AstTypeAliasItem) -> TypeId {
    let ty = ctx.alloc_type(Type::Unresolved(UnresolvedType {
        name: type_alias.name(),
    }));

    if let Some(name) = type_alias.name() {
        if ctx.last_scope().ty(name.text()).is_some() {
            ctx.error(&name, ErrorKind::DuplicateType(name.text().to_string()));
        }

        ctx.last_scope_mut()
            .insert_type(name.text().to_string(), ty);
    }

    ty
}

pub fn compile_document(
    ctx: &mut Context,
    scope: ScopeId,
    document: &AstDocument,
    declarations: DocumentDeclarations,
) {
    ctx.push_scope(scope);

    for (index, item) in document.items().enumerate() {
        match item {
            AstItem::TypeItem(item) => compile_type_item(ctx, &item, declarations.types[index]),
            AstItem::SymbolItem(_) => {}
        }
    }

    for (index, item) in document.items().enumerate() {
        match item {
            AstItem::TypeItem(_) => {}
            AstItem::SymbolItem(item) => {
                compile_symbol_item(ctx, &item, declarations.symbols[index])
            }
        }
    }

    ctx.pop_scope();
}

fn compile_type_item(ctx: &mut Context, item: &AstTypeItem, ty: TypeId) {
    match item {
        AstTypeItem::TypeAliasItem(type_alias) => compile_type_alias(ctx, type_alias, ty),
    }
}

fn compile_symbol_item(ctx: &mut Context, item: &AstSymbolItem, symbol: SymbolId) {
    match item {
        AstSymbolItem::FunctionItem(function) => compile_function(ctx, function, symbol),
    }
}

fn compile_function(ctx: &mut Context, function: &AstFunctionItem, symbol: SymbolId) {}

fn compile_type_alias(ctx: &mut Context, type_alias: &AstTypeAliasItem, ty: TypeId) {}
