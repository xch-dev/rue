use std::collections::HashMap;

use rue_diagnostic::{Name, SrcLoc};
use rue_hir::{Declaration, SymbolId};

/// Facts recorded by the compiler while compiling, for lints that need something that isn't kept
/// in the HIR. The compiler records every site; the lints decide what to report.
///
/// Most lints don't need sites, since they run as [`HirLintPass`](crate::HirLintPass)es over the
/// HIR, which records the type and location of every expression.
#[derive(Debug, Default, Clone)]
pub struct LintSites {
    /// The items of imports that resolved, but didn't bring anything into scope (for example,
    /// because everything they name is already declared there). For a glob import, this is the
    /// `*`.
    pub empty_imports: Vec<Name>,
    /// The fields of structs that are read, by a field access or by destructuring. A struct
    /// binding's fields are only in the HIR through the bindings they create, which aren't
    /// visited unless they're referenced, so the reads are recorded as they're compiled.
    pub field_reads: Vec<FieldRead>,
    /// Where each declaration is referenced through a module (such as `module::name`), rather
    /// than from the scope it's written in, which only resolves if it's exported.
    pub qualified_references: HashMap<Declaration, Vec<SrcLoc>>,
    /// The `mod` items declared in the documents being linted, along with the location of the
    /// whole item. Modules aren't declarations, so they can't be found from the HIR.
    pub modules: Vec<(SymbolId, SrcLoc)>,
}

/// A read of a struct's field. The struct is identified by its name, which every copy of it (such
/// as for generic arguments or narrowing) shares.
#[derive(Debug, Clone)]
pub struct FieldRead {
    pub struct_name: Name,
    pub field: String,
}
