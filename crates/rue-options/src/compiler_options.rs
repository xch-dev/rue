use rue_diagnostic::LintLevels;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

    /// Whether programs target CLVM with the `LIMITS` flag, which Chia mainnet enforces in both
    /// the mempool and blocks until hard fork 2. Off by default.
    ///
    /// `LIMITS` makes some operators fail when an operand is too long, counting any leading zero
    /// bytes:
    ///
    /// - `*` fails if an operand is longer than 256 bytes, or if the running product is longer
    ///   than 1024 bytes after any operand after the first.
    /// - `/`, `%` and `divmod` fail if the dividend is longer than 256 bytes, or the divisor is
    ///   longer than 1024 bytes.
    /// - `modpow` fails if the base, exponent or modulus is longer than 256 bytes.
    /// - `g1_multiply` and `g2_multiply` fail if the scalar is longer than 1024 bytes.
    ///
    /// When it's on, the compiler guarantees that a release build fails under `LIMITS` exactly when
    /// the debug build does: the failure analysis treats `*`, `/`, `%` and `divmod` as able to fail
    /// unless every operand is a constant short enough to be allowed (and the divisor isn't zero),
    /// so their checks aren't moved, removed or skipped, and no optimization removes or adds a
    /// failure caused by an operand's length. The test runner, `rue test` and `rue debug` then run
    /// programs with `LIMITS`, so tests fail where mainnet would.
    ///
    /// When it's off, the compiler targets CLVM without `LIMITS`, as mainnet will be after hard
    /// fork 2. Operands of any length are treated as allowed, so `*` can't fail, and `/`, `%` and
    /// `divmod` only fail for a zero divisor. A program compiled this way can pass its tests and
    /// succeed in debug builds, yet fail on mainnet today when an operand exceeds a limit, and the
    /// optimizer may remove such a failure, so a release build can succeed where the same program
    /// fails on mainnet. For example, `x % 2 == 1` is compiled to `logand`, which doesn't limit
    /// `x`, and products of constants are folded whatever their length.
    ///
    /// Const expressions are evaluated at compile time without `LIMITS` either way, since they're
    /// never run on chain, so a `const` can use operands of any length.
    ///
    /// This only concerns `LIMITS`. The other ways the flags programs are run with differ from
    /// mainnet's are described by `rue_lir::clvm_flags`.
    pub operand_limits: bool,

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
            operand_limits: false,
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
            operand_limits: false,
            lints: LintLevels::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_options_optimize_and_use_the_standard_library() {
        assert_eq!(
            CompilerOptions::default(),
            CompilerOptions {
                std: true,
                auto_inline: true,
                optimize_lir: true,
                debug_symbols: false,
                optimize_static_pairs: true,
                const_eval_max_cost: 11_000_000_000,
                operand_limits: false,
                lints: LintLevels::default(),
            }
        );
    }

    #[test]
    fn debug_options_only_differ_in_optimizations_and_debug_symbols() {
        assert_eq!(
            CompilerOptions::debug(),
            CompilerOptions {
                auto_inline: false,
                optimize_lir: false,
                debug_symbols: true,
                optimize_static_pairs: false,
                ..CompilerOptions::default()
            }
        );
    }
}
