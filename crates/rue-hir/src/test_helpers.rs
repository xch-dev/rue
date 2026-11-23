//! What the unit tests in this crate share to build symbols and statements.

use rue_diagnostic::{Source, SourceKind, SrcLoc};
use rue_types::BuiltinTypes;

use crate::{BindingSymbol, Database, HirId, Symbol, SymbolId, Value};

/// A location that statements can point to, since tests don't have a source.
pub fn srcloc() -> SrcLoc {
    SrcLoc::new(Source::new("".into(), SourceKind::Std(String::new())), 0..0)
}

/// A binding of the value, typed as an `Int`.
pub fn binding(db: &mut Database, value: HirId, inline: bool) -> SymbolId {
    let ty = BuiltinTypes::new(db.types_mut()).int;
    db.alloc_symbol(Symbol::Binding(BindingSymbol {
        name: None,
        value: Value::new(value, ty),
        inline,
    }))
}
