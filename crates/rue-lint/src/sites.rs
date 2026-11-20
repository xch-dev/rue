use std::collections::HashMap;

use rue_diagnostic::{Name, SrcLoc};
use rue_hir::{Declaration, HirId, SymbolId};

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
    /// The names in inline modules that resolve outside of the file (to the prelude or a
    /// builtin), while a module that encloses them has an item with the same name in the same
    /// namespace. An inline module doesn't see the items of the modules it's in, so the name means
    /// the prelude's item rather than the enclosing one.
    pub enclosed_prelude_references: Vec<EnclosedPreludeReference>,
    /// The arms of `match` expressions that are never taken, since the arms before them handle
    /// every value they match. They're left out of the HIR, so they're recorded as they're
    /// compiled.
    pub unreachable_match_arms: Vec<UnreachableMatchArm>,
    /// The arms of `match` expressions that also handle members they can't tell apart from their
    /// own type.
    pub indistinguishable_match_arms: Vec<IndistinguishableMatchArm>,
    /// The arms of each `inline match` that are evaluated, by the node of the `match`. Its arms
    /// are generated as an `inline if` chain, which has no location of its own.
    pub inline_match_arms: HashMap<HirId, Vec<HirId>>,
    /// The type guards and `match` arms that leave too many combinations of a struct's fields to
    /// narrow it by, so its fields are merged instead. The types are subtracted while compiling,
    /// and not again by the lints.
    pub imprecise_narrowings: Vec<ImpreciseNarrowing>,
    /// The values spread in struct initializers that nothing is read from, since every field is
    /// either written or a constant. The value isn't in the HIR unless it's evaluated for its
    /// effect.
    pub unused_struct_spreads: Vec<UnusedStructSpread>,
}

/// A value spread in a struct initializer that nothing is read from (see
/// [`LintSites::unused_struct_spreads`]).
#[derive(Debug, Clone)]
pub struct UnusedStructSpread {
    /// The spread, with its `...`.
    pub srcloc: SrcLoc,
    /// The name of the struct being initialized.
    pub struct_name: String,
    /// Whether some fields aren't written, which are all constants (such as the opcode of a
    /// condition), rather than every field being written.
    pub constants: bool,
    /// Whether the initializer is still complete without the spread, since every field that
    /// isn't written has a default value.
    pub removable: bool,
    /// Whether the value is still evaluated, since it isn't a path (such as a function call), so
    /// removing it could change what the program does.
    pub evaluated: bool,
}

/// A type guard or `match` arm after which a struct is wider than it could be (see
/// [`LintSites::imprecise_narrowings`]).
#[derive(Debug, Clone)]
pub struct ImpreciseNarrowing {
    /// The guard's type, or the arm's pattern.
    pub srcloc: SrcLoc,
    /// The name of the struct that's merged.
    pub struct_name: String,
}

/// A read of a struct's field. The struct is identified by its name, which every copy of it (such
/// as for generic arguments or narrowing) shares.
#[derive(Debug, Clone)]
pub struct FieldRead {
    pub struct_name: Name,
    pub field: String,
}

/// An arm of a `match` expression that's never taken (see [`LintSites::unreachable_match_arms`]).
#[derive(Debug, Clone)]
pub struct UnreachableMatchArm {
    /// The arm's pattern.
    pub pattern: SrcLoc,
    /// Why the arm is never taken.
    pub reason: UnreachableReason,
}

/// Why a match arm is never taken.
#[derive(Debug, Clone)]
pub enum UnreachableReason {
    /// The arms before it handle every value that the pattern matches between them.
    Handled,
    /// An earlier arm handles every value that's left.
    CatchAll(SrcLoc),
    /// An earlier arm also handles the pattern's values, since it can't tell them apart from its
    /// own type at runtime (see [`IndistinguishableMatchArm`]).
    Indistinguishable {
        /// The earlier arm's pattern.
        arm: SrcLoc,
        /// The members that the earlier arm handles, as a list for a diagnostic.
        members: String,
    },
}

/// An arm of a `match` expression whose check also lets through members of the value's type that
/// aren't values of its own type, since they're the same at runtime, which no later arm names (see
/// [`LintSites::indistinguishable_match_arms`]). An arm that does name them is unreachable instead.
#[derive(Debug, Clone)]
pub struct IndistinguishableMatchArm {
    /// The arm's pattern.
    pub pattern: SrcLoc,
    /// The pattern's type, for a diagnostic.
    pub ty: String,
    /// The members that the arm also handles, as a list for a diagnostic.
    pub members: String,
}

/// A name in an inline module that resolves to the prelude or a builtin, while an enclosing module
/// has an item with the same name in the same namespace (see
/// [`LintSites::enclosed_prelude_references`]).
#[derive(Debug, Clone)]
pub struct EnclosedPreludeReference {
    /// The name as it's written in the inline module.
    pub name: Name,
    /// Whether the name resolves to a builtin, rather than an item of the prelude.
    pub builtin: bool,
    /// How many `super`s name the enclosing module with the item, which is the nearest one.
    pub depth: usize,
    /// Where the enclosing module declares the item, or the import that brings it into the module.
    pub enclosing: Option<SrcLoc>,
    /// Whether the enclosing module brings the item in with an import, rather than declaring it.
    pub imported: bool,
    /// Whether the name is a struct field shorthand (such as `Point { x }`), which is both the
    /// field's name and the value.
    pub field_shorthand: bool,
}
