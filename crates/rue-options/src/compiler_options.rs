use rue_diagnostic::LintLevels;

#[derive(Debug, Clone, Copy)]
#[allow(clippy::struct_excessive_bools)]
pub struct CompilerOptions {
    /// Whether to compile the standard library.
    pub std: bool,

    /// Whether symbols which are only referenced once (including any parameters they have)
    /// should be inlined automatically (even if they are not marked as `inline`).
    pub auto_inline: bool,

    /// Whether to fully optimize the generated LIR, or preserve the original structure.
    pub optimize_lir: bool,

    /// Whether to include debug symbols in the generated code.
    pub debug_symbols: bool,

    /// Whether codegen collapses static pair trees into a single quoted value.
    pub optimize_static_pairs: bool,

    /// The maximum CLVM cost permitted while evaluating a const expression.
    pub const_eval_max_cost: u64,

    /// The level each lint is reported at.
    pub lints: LintLevels,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            std: true,
            auto_inline: true,
            optimize_lir: true,
            debug_symbols: false,
            optimize_static_pairs: true,
            const_eval_max_cost: 11_000_000_000,
            lints: LintLevels::default(),
        }
    }
}

impl CompilerOptions {
    pub fn debug() -> Self {
        Self {
            std: true,
            auto_inline: false,
            optimize_lir: false,
            debug_symbols: true,
            optimize_static_pairs: false,
            const_eval_max_cost: 11_000_000_000,
            lints: LintLevels::default(),
        }
    }
}
