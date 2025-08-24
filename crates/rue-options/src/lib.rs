#[derive(Debug, Clone)]
pub struct CompilerOptions {
    /// Whether symbols which are only referenced once (including any parameters they have)
    /// should be inlined automatically (even if they are not marked as `inline`).
    pub auto_inline: bool,

    /// Whether to fully optimize the generated LIR, or preserve the original structure.
    pub optimize_lir: bool,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            auto_inline: true,
            optimize_lir: true,
        }
    }
}

impl CompilerOptions {
    pub fn debug() -> Self {
        Self {
            auto_inline: false,
            optimize_lir: false,
        }
    }
}
