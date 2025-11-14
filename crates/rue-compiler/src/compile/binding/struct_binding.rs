use std::collections::HashSet;

use log::debug;
use rue_ast::{AstNode, AstStructBinding};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{BindingSymbol, Declaration, Hir, Symbol, SymbolId, SymbolPath, Value};

use crate::{
    Compiler, CompletionContext, Field, FieldResult, SyntaxField, SyntaxItem, SyntaxItemKind,
    compile_field, create_binding, create_binding_for_identifier,
};

pub fn create_struct_binding(
    ctx: &mut Compiler,
    symbol: SymbolId,
    struct_binding: &AstStructBinding,
) {
    let ty = ctx.symbol_type(symbol);
    let reference = Value::new(ctx.alloc_hir(Hir::Reference(symbol)), ty)
        .with_reference(SymbolPath::new(symbol, vec![]));

    let mut specified_fields = HashSet::new();

    for field in struct_binding.fields() {
        let Some(name) = field.name() else {
            continue;
        };

        specified_fields.insert(name.text().to_string());

        let value = match compile_field(ctx, reference.clone(), &Field::Named(name.text())) {
            FieldResult::Value(value) => value,
            FieldResult::Unknown => {
                debug!("Unresolved field access due to unknown field");
                let type_name = ctx.type_name(reference.ty);
                ctx.diagnostic(
                    &name,
                    DiagnosticKind::UnknownField(name.text().to_string(), type_name),
                );
                ctx.builtins().unresolved.clone()
            }
            FieldResult::Error => {
                debug!("Unresolved field access due to missing field in underlying struct type");
                let type_name = ctx.type_name(reference.ty);
                ctx.diagnostic(
                    &name,
                    DiagnosticKind::MissingField(name.text().to_string(), type_name),
                );
                ctx.builtins().unresolved.clone()
            }
        };

        let field_type = value.ty;

        let binding_symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
            name: None,
            value,
            inline: true,
        }));

        ctx.push_declaration(Declaration::Symbol(binding_symbol));

        ctx.reference(Declaration::Symbol(symbol), None);

        if let Some(binding) = field.binding() {
            create_binding(ctx, binding_symbol, &binding);
        } else {
            create_binding_for_identifier(ctx, binding_symbol, &name);
        }

        ctx.pop_declaration();

        ctx.syntax_map_mut().add_item(SyntaxItem::new(
            SyntaxItemKind::FieldReference(SyntaxField {
                name: name.text().to_string(),
                container: ty,
                ty: field_type,
            }),
            name.text_range(),
        ));
    }

    ctx.syntax_map_mut().add_item(SyntaxItem::new(
        SyntaxItemKind::CompletionContext(CompletionContext::StructFields {
            ty,
            specified_fields: Some(specified_fields),
        }),
        struct_binding.syntax().text_range(),
    ));
}
