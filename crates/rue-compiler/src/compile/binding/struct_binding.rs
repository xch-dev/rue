use rue_ast::AstStructBinding;
use rue_hir::{BindingSymbol, Declaration, Hir, Symbol, SymbolId, SymbolPath, Value};

use crate::{Compiler, compile_named_field, create_binding, create_binding_for_identifier};

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

        let value = compile_named_field(ctx, &reference, &name);

        let binding_symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
            name: None,
            value,
            inline: true,
        }));
        ctx.push_declaration(Declaration::Symbol(binding_symbol));
        ctx.reference(Declaration::Symbol(symbol));

        if let Some(binding) = field.binding() {
            create_binding(ctx, binding_symbol, &binding);
        } else {
            create_binding_for_identifier(ctx, binding_symbol, name);
        }

        ctx.pop_declaration();
    }
}
