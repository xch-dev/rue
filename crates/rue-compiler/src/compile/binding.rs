mod list_binding;
mod named_binding;
mod pair_binding;
mod struct_binding;

pub use list_binding::*;
pub use named_binding::*;
pub use pair_binding::*;
pub use struct_binding::*;

use rue_ast::AstBinding;
use rue_hir::SymbolId;

use crate::Compiler;

pub fn create_binding(ctx: &mut Compiler, symbol: SymbolId, binding: &AstBinding) {
    match binding {
        AstBinding::NamedBinding(binding) => create_named_binding(ctx, symbol, binding),
        AstBinding::PairBinding(binding) => create_pair_binding(ctx, symbol, binding),
        AstBinding::ListBinding(binding) => create_list_binding(ctx, symbol, binding),
        AstBinding::StructBinding(binding) => create_struct_binding(ctx, symbol, binding),
    }
}
