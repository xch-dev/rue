#[derive(Debug, Clone, Copy)]
pub struct CompilerOptions {
    /// Whether symbols which are only referenced once (including any parameters they have)
    /// should be inlined automatically (even if they are not marked as `inline`).
    pub auto_inline: bool,

    /// Whether to fully optimize the generated LIR, or preserve the original structure.
    pub optimize_lir: bool,

    /// Whether to include debug symbols in the generated code.
    pub debug_symbols: bool,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            auto_inline: true,
            optimize_lir: true,
            debug_symbols: false,
        }
    }
}

impl CompilerOptions {
    pub fn debug() -> Self {
        Self {
            auto_inline: false,
            optimize_lir: false,
            debug_symbols: true,
        }
    }
}
