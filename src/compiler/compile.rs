use crate::{
    AstDocument, AstFunctionItem, AstItem, AstLiteralType, AstSymbolItem, AstType,
    AstTypeAliasItem, AstTypeItem, BindingSymbol, Context, ErrorKind, FunctionSymbol, Scope,
    ScopeId, Symbol, SymbolId, Type, TypeId, UnresolvedType,
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
    let scope = ctx.alloc_scope(Scope::new());

    let return_type = if let Some(return_type) = function.return_type() {
        compile_type(ctx, &return_type)
    } else {
        ctx.builtins().unresolved
    };

    let mut parameters = Vec::new();

    for parameter in function.parameters() {
        let ty = if let Some(ty) = parameter.ty() {
            compile_type(ctx, &ty)
        } else {
            ctx.builtins().unresolved
        };

        let symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
            name: parameter.name(),
            ty,
        }));

        if let Some(name) = parameter.name() {
            if ctx.scope(scope).symbol(name.text()).is_some() {
                ctx.error(&name, ErrorKind::DuplicateSymbol(name.text().to_string()));
            }

            ctx.scope_mut(scope)
                .insert_symbol(name.text().to_string(), symbol);
        }

        parameters.push(symbol);
    }

    let symbol = ctx.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: function.name(),
        scope,
        parameters,
        return_type,
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

fn compile_type(ctx: &mut Context, ty: &AstType) -> TypeId {
    match ty {
        AstType::LiteralType(literal) => compile_literal_type(ctx, literal),
    }
}

fn compile_literal_type(ctx: &mut Context, literal: &AstLiteralType) -> TypeId {
    let Some(value) = literal.value() else {
        return ctx.builtins().unresolved;
    };
    let Some(ty) = ctx.resolve_type(value.text()) else {
        ctx.error(&value, ErrorKind::UndeclaredType(value.text().to_string()));
        return ctx.builtins().unresolved;
    };
    ty
}
