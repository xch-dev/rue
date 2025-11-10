use log::debug;
use rue_ast::{AstNode, AstTypeAliasItem};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Declaration, ScopeId};
use rue_types::{Alias, Type, TypeId};

use crate::{
    Compiler, CompletionContext, SyntaxItem, SyntaxItemKind, compile_generic_parameters,
    compile_type,
};

pub fn declare_type_alias(ctx: &mut Compiler, type_alias: &AstTypeAliasItem) -> (TypeId, ScopeId) {
    ctx.syntax_map_mut().add_item(SyntaxItem::new(
        SyntaxItemKind::CompletionContext(CompletionContext::Item),
        type_alias.syntax().text_range(),
    ));

    let ty = ctx.alloc_type(Type::Unresolved);

    ctx.push_declaration(Declaration::Type(ty));

    let scope = ctx.alloc_child_scope();

    let generics = if let Some(generic_parameters) = type_alias.generic_parameters() {
        compile_generic_parameters(ctx, scope, &generic_parameters)
    } else {
        vec![]
    };

    let inner = ctx.alloc_type(Type::Unresolved);

    let name = type_alias.name().map(|name| ctx.local_name(&name));

    *ctx.ty_mut(ty) = Type::Alias(Alias {
        name,
        generics,
        inner,
    });

    if let Some(name) = type_alias.name() {
        if ctx.last_scope().ty(name.text()).is_some() {
            ctx.diagnostic(
                &name,
                DiagnosticKind::DuplicateType(name.text().to_string()),
            );
        }

        ctx.last_scope_mut().insert_type(
            name.text().to_string(),
            ty,
            type_alias.export().is_some(),
        );

        ctx.declaration_span(Declaration::Type(ty), name.text_range());
    }

    ctx.pop_declaration();

    (ty, scope)
}

pub fn compile_type_alias(
    ctx: &mut Compiler,
    type_alias: &AstTypeAliasItem,
    ty: TypeId,
    scope: ScopeId,
) {
    ctx.push_declaration(Declaration::Type(ty));

    let range = type_alias.syntax().text_range();
    ctx.push_scope(scope, range.start());

    let resolved_inner = if let Some(inner) = type_alias.ty() {
        compile_type(ctx, &inner)
    } else {
        debug!("Unresolved type alias type");
        ctx.builtins().unresolved.ty
    };

    ctx.pop_scope(range.end());

    let Type::Alias(Alias { inner, .. }) = ctx.ty(ty) else {
        unreachable!()
    };

    let inner = *inner;

    *ctx.ty_mut(inner) = Type::Ref(resolved_inner);

    ctx.pop_declaration();
}
