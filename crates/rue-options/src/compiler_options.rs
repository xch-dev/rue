use rue_diagnostic::LintLevels;
use thiserror::Error;

/// Both `operand_limits` and `hard_fork_2` are set, although hard fork 2 removes `LIMITS`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Error)]
#[error(
    "`operand_limits` has no effect with `hard_fork_2`, since hard fork 2 removes `LIMITS`, so only one of them can be set"
)]
pub struct TargetConflict;

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
    /// the mempool and blocks until hard fork 2. Off by default. It has no effect with
    /// [`hard_fork_2`](Self::hard_fork_2), which removes `LIMITS`, and setting both is an error in
    /// the CLI and the test runner (see [`check_target`](Self::check_target)).
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
    /// failure caused by an operand's length. The test runner, `rue test`, and `rue debug` run in
    /// the project then run programs with `LIMITS`, so they fail where mainnet would. `rue debug`
    /// can also be passed `--operand-limits` to run a program with `LIMITS` outside of a project.
    ///
    /// When it's off, the compiler targets CLVM without `LIMITS`, which mainnet only does after
    /// hard fork 2. Operands of any length are treated as allowed, so `*` can't fail, and `/`, `%` and
    /// `divmod` only fail for a zero divisor. A program compiled this way can pass its tests and
    /// succeed in debug builds, yet fail on mainnet today when an operand exceeds a limit. The
    /// optimizer can also change whether it fails under `LIMITS`, in either direction, so a release
    /// build can succeed where the debug build fails, or fail where it succeeds:
    ///
    /// - It removes such failures, such as by compiling `x % 2 == 1` to `logand`, which doesn't
    ///   limit `x`, or by dropping or moving a product or quotient that it treats as unable to fail.
    /// - It adds them, by folding constants into operands longer than the limits, even when each
    ///   constant in the source is within them. For example, `x * C * C` with a 150 byte constant
    ///   `C` is compiled to `x` times a single 299 byte constant. It also flattens products and
    ///   moves their constants to the end, which changes the running products that `LIMITS`
    ///   checks: `a * 0 * b * c * d * e` is compiled to `(* a b c d e 0)`, whose running product
    ///   can grow past 1024 bytes before it reaches the zero.
    ///
    /// For a solution whose values match their declared types, the only failures it removes are
    /// the length checks of `LIMITS` itself, never an `assert` or `raise`, and what the release
    /// build returns is the result the source computes without `LIMITS`. So leaving this off never
    /// makes a release build accept such a spend that the source rejects. (A value that doesn't
    /// match its type, such as a pair passed for an `Int`, can make a release build succeed where
    /// the debug build fails with or without this option, since the compiler trusts declared types
    /// and doesn't check them at runtime.) The risk is the other way: a spend can fail on mainnet
    /// although its tests pass, and if the long operand is fixed by the puzzle, such as a constant
    /// or a curried value, that path can't be spent at all.
    ///
    /// So a program compiled with this off is safe to run on mainnet today as long as no operand
    /// or product of these operators can come near the limits, counting the values the program
    /// computes, the constants the optimizer folds together, and the products of operands it
    /// multiplies together. Typical puzzles are far from them: amounts are at most 8 bytes and
    /// hashes are 32. Turn it on when operands could be that long, or to make release and debug
    /// builds agree exactly under `LIMITS`.
    ///
    /// Const expressions are evaluated at compile time without `LIMITS` either way, since they're
    /// never run on chain, so a `const` can use operands of any length.
    ///
    /// This only concerns `LIMITS`. The flags programs are run with are described by
    /// `rue_lir::ClvmTarget::flags`.
    pub operand_limits: bool,

    /// Whether programs target Chia mainnet as it will be after hard fork 2, rather than as it is
    /// today. Off by default.
    ///
    /// When it's off:
    ///
    /// - `modpow` is an error, since mainnet disables it until hard fork 2 (`DISABLE_OP`).
    /// - `keccak256` is an error, since mainnet doesn't know the operator outside of the softfork
    ///   guard until hard fork 2. It can't be used inside the guard instead, since the guard always
    ///   returns nil, so a hash can't be returned from it.
    /// - Programs are run, and the optimizer compares costs, with the cost model from before hard
    ///   fork 2.
    /// - `secp256k1_verify` and `secp256r1_verify` are compiled to the four byte opcodes that
    ///   encode their cost.
    ///
    /// When it's on, `modpow` and `keccak256` are allowed, programs are run with the new cost
    /// model and with relaxed BLS point checks, and the secp operators are compiled to the one byte
    /// opcodes 64 and 65. Hard fork 2 also removes `LIMITS`, so
    /// [`operand_limits`](Self::operand_limits) has no effect.
    ///
    /// This changes the compiled program, and so its puzzle hash, for any program that uses the
    /// secp operators, and it can change what the optimizer emits, since it folds constants by
    /// comparing costs in the target's cost model. A program compiled with this on can fail on
    /// mainnet until hard fork 2 activates, and costs reported before then don't match mainnet's.
    ///
    /// The test runner, `rue test`, and `rue debug` run inside the project, run programs with the
    /// flags for the target (see `rue_lir::ClvmTarget::flags`).
    pub hard_fork_2: bool,

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
            hard_fork_2: false,
            lints: LintLevels::default(),
        }
    }
}

impl CompilerOptions {
    /// Whether operands are limited by their length, which they are with
    /// [`operand_limits`](Self::operand_limits) until hard fork 2.
    pub fn enforces_operand_limits(&self) -> bool {
        self.operand_limits && !self.hard_fork_2
    }

    /// Checks that the options that choose the target don't conflict. `operand_limits` has no
    /// effect with `hard_fork_2`, so setting both is a mistake.
    pub fn check_target(&self) -> Result<(), TargetConflict> {
        if self.operand_limits && self.hard_fork_2 {
            return Err(TargetConflict);
        }

        Ok(())
    }

    pub fn debug() -> Self {
        Self {
            std: true,
            auto_inline: false,
            optimize_lir: false,
            debug_symbols: true,
            optimize_static_pairs: false,
            const_eval_max_cost: 11_000_000_000,
            operand_limits: false,
            hard_fork_2: false,
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
                hard_fork_2: false,
                lints: LintLevels::default(),
            }
        );
    }

    #[test]
    fn operand_limits_only_apply_before_hard_fork_2() {
        for (operand_limits, hard_fork_2, enforced) in [
            (false, false, false),
            (true, false, true),
            (false, true, false),
            (true, true, false),
        ] {
            let options = CompilerOptions {
                operand_limits,
                hard_fork_2,
                ..CompilerOptions::default()
            };
            assert_eq!(options.enforces_operand_limits(), enforced);
            assert_eq!(
                options.check_target().is_err(),
                operand_limits && hard_fork_2
            );
        }
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
