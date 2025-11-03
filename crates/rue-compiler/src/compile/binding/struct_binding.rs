use log::debug;
use rue_ast::AstStructBinding;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{BindingSymbol, Declaration, Hir, Symbol, SymbolId, SymbolPath, Value};

use crate::{
    Compiler, Field, FieldResult, compile_field, create_binding, create_binding_for_identifier,
};

pub fn create_struct_binding(
    ctx: &mut Compiler,
    symbol: SymbolId,
    struct_binding: &AstStructBinding,
) {
    let ty = ctx.symbol_type(symbol);
    let reference = Value::new(ctx.alloc_hir(Hir::Reference(symbol)), ty)
        .with_reference(SymbolPath::new(symbol, vec![]));

    for field in struct_binding.fields() {
        let Some(name) = field.name() else {
            continue;
        };

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
            create_binding_for_identifier(ctx, binding_symbol, name);
        }

        ctx.pop_declaration();
    }
}
