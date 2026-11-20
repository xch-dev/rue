use std::cell::RefCell;

use clvm_traits::{FromClvm, destructure_tuple, match_tuple};
use clvmr::{
    Allocator, ChiaDialect, ClvmFlags, MEMPOOL_MODE, NodePtr,
    dialect::{Dialect, OperatorSet},
    error::EvalErr,
    reduction::Reduction,
};
use colored::Colorize;
use rue_clvm::disassemble_value;

use crate::ClvmOp;

/// What a program is compiled for, which decides the flags it's run with (see
/// [`ClvmTarget::flags`]) and what the optimizer may assume. It's set from the compiler options
/// of the same names (see `CompilerOptions` in `rue-options`).
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClvmTarget {
    /// Whether the program is run with `LIMITS`, which rejects some long operands, so that the
    /// optimizer doesn't remove or add failures caused by the length of an operand. Without it,
    /// the optimizer assumes operands of any length are allowed. It has no effect with
    /// [`hard_fork_2`](Self::hard_fork_2), which removes `LIMITS`.
    pub operand_limits: bool,

    /// Whether the program is run as it will be after hard fork 2: with the new cost model,
    /// `keccak256` outside of the softfork guard, the secp operators as opcodes 64 and 65, and
    /// relaxed BLS point checks, and without `DISABLE_OP` or `LIMITS`.
    pub hard_fork_2: bool,
}

#[cfg(test)]
impl ClvmTarget {
    /// Mainnet today, without `operand_limits`, which is the default.
    pub(crate) const TODAY: Self = Self {
        operand_limits: false,
        hard_fork_2: false,
    };

    /// Mainnet today, with `operand_limits`.
    pub(crate) const TODAY_WITH_LIMITS: Self = Self {
        operand_limits: true,
        hard_fork_2: false,
    };

    /// Mainnet after hard fork 2.
    pub(crate) const HARD_FORK_2: Self = Self {
        operand_limits: false,
        hard_fork_2: true,
    };

    /// Every target the optimizer must preserve the behavior under.
    pub(crate) const ALL: [Self; 3] = [Self::TODAY, Self::TODAY_WITH_LIMITS, Self::HARD_FORK_2];
}

impl ClvmTarget {
    /// Whether operands are limited by their length, which they are with `operand_limits` until
    /// hard fork 2.
    pub const fn limits_operands(self) -> bool {
        self.operand_limits && !self.hard_fork_2
    }

    /// The flags programs are run with. These are mempool mode, which is stricter than blocks, as
    /// it is on mainnet, with the changes below. Costs use the cost model of the target.
    ///
    /// Before hard fork 2:
    ///
    /// - With `operand_limits`, they include `LIMITS`, which mainnet enforces in both the mempool
    ///   and blocks. It rejects some long operands of `*`, `/`, `%`, `divmod`, `modpow`,
    ///   `g1_multiply` and `g2_multiply` (see [`MAX_MUL_OPERAND_BYTES`] and the constants after
    ///   it), so these operators can fail for operands that aren't known at compile time.
    /// - `DISABLE_OP`, which mainnet also enforces, is only included with `operand_limits`. It
    ///   makes `modpow` unimplemented, which the compiler enforces by rejecting `modpow`, and it
    ///   rejects a dividend of `/`, `%` or `divmod` longer than 2048 bytes. That's a length limit
    ///   like the ones `LIMITS` adds (and less strict than them), so it's left out without
    ///   `operand_limits`, which doesn't model operand lengths.
    /// - `keccak256` is unknown outside of the softfork guard, as it is on mainnet, and the
    ///   compiler rejects it.
    ///
    /// After hard fork 2, they use the new cost model, enable `keccak256` outside of the guard,
    /// the secp operators as opcodes 64 and 65, and relaxed BLS point checks, as chia does once
    /// hard fork 2 activates. They don't include `LIMITS`, which chia stops setting then, or
    /// `DISABLE_OP`, which chia's mempool flags still include but which has no effect with the new
    /// cost model.
    pub const fn flags(self) -> ClvmFlags {
        if self.hard_fork_2 {
            return MEMPOOL_MODE
                .difference(ClvmFlags::DISABLE_OP)
                .union(ClvmFlags::NEW_COST_MODEL)
                .union(ClvmFlags::ENABLE_KECCAK_OPS_OUTSIDE_GUARD)
                .union(ClvmFlags::ENABLE_SECP_OPS)
                .union(ClvmFlags::RELAXED_BLS);
        }

        if self.operand_limits {
            MEMPOOL_MODE.union(ClvmFlags::LIMITS)
        } else {
            MEMPOOL_MODE.difference(ClvmFlags::DISABLE_OP)
        }
    }
}

/// The longest atom (in bytes, including any leading zeros) that `LIMITS` allows as an operand of
/// `*`. The running product is limited by [`MAX_PRODUCT_BYTES`].
pub const MAX_MUL_OPERAND_BYTES: usize = 256;

/// The largest magnitude (in bytes) that `LIMITS` allows the running product of `*` to reach after
/// each operand after the first.
pub const MAX_PRODUCT_BYTES: usize = 1024;

/// The longest atom (in bytes, including any leading zeros) that `LIMITS` allows as the dividend of
/// `/`, `%` and `divmod`.
pub const MAX_DIVIDEND_BYTES: usize = 256;

/// The longest atom (in bytes, including any leading zeros) that `LIMITS` allows as the divisor of
/// `/`, `%` and `divmod`.
pub const MAX_DIVISOR_BYTES: usize = 1024;

/// The cost, under the cost model from before hard fork 2, that `keccak256_verify` declares for the
/// softfork guard it checks the hash in, apart from [`KECCAK256_COST_PER_BYTE`] for each byte of
/// the value. It's the cost of entering the guard (140), plus the cost of running the check in it,
/// `(a (i (= (keccak256 2) 5) (q) (q 8)) 1)`, for an empty value and a matching 32 byte hash. The
/// guard fails unless its cost is exactly the cost it declares.
pub const KECCAK256_VERIFY_COST: u64 = 1206;

/// The cost of `keccak256` for each byte of its argument, under the cost model from before hard
/// fork 2.
pub const KECCAK256_COST_PER_BYTE: u64 = 2;

/// The cost charged for each byte of a serialized program. This is `cost_per_byte` in Chia's
/// mainnet consensus constants, which aren't available from `clvmr` itself.
pub const COST_PER_BYTE: u64 = 12_000;

#[derive(Debug, Clone)]
pub struct PrintMessage {
    pub srcloc: String,
    pub message: String,
}

#[derive(Debug, Clone)]
pub struct DebugDialect {
    flags: ClvmFlags,
    stdout: bool,
    log: RefCell<Vec<PrintMessage>>,
}

impl DebugDialect {
    pub fn new(flags: ClvmFlags, stdout: bool) -> Self {
        Self {
            flags,
            stdout,
            log: RefCell::new(Vec::new()),
        }
    }

    pub fn log(&self) -> Vec<PrintMessage> {
        self.log.borrow().clone()
    }

    fn inner(&self) -> ChiaDialect {
        ChiaDialect::new(self.flags)
    }
}

impl Dialect for DebugDialect {
    fn quote_kw(&self) -> u32 {
        self.inner().quote_kw()
    }

    fn apply_kw(&self) -> u32 {
        self.inner().apply_kw()
    }

    fn softfork_kw(&self) -> u32 {
        self.inner().softfork_kw()
    }

    fn softfork_extension(&self, ext: u32) -> OperatorSet {
        self.inner().softfork_extension(ext)
    }

    fn flags(&self) -> ClvmFlags {
        self.flags
    }

    fn gc_candidate(&self, allocator: &Allocator, op: NodePtr) -> bool {
        self.inner().gc_candidate(allocator, op)
    }

    fn op(
        &self,
        allocator: &mut Allocator,
        op: NodePtr,
        args: NodePtr,
        max_cost: u64,
        extensions: OperatorSet,
    ) -> Result<Reduction, EvalErr> {
        if allocator.atom(op).as_ref() == ClvmOp::DebugPrint.to_atom() {
            let Some(destructure_tuple!(srcloc, value, _)) =
                <match_tuple!(String, NodePtr, NodePtr)>::from_clvm(allocator, args).ok()
            else {
                return Err(EvalErr::InvalidNilTerminator(args));
            };

            let message = disassemble_value(allocator, value);

            if self.stdout {
                eprintln!("{}: {}", srcloc.cyan().bold(), message);
            }

            self.log.borrow_mut().push(PrintMessage { srcloc, message });

            return Ok(Reduction(0, NodePtr::NIL));
        }

        self.inner().op(allocator, op, args, max_cost, extensions)
    }

    fn allow_unknown_ops(&self) -> bool {
        self.inner().allow_unknown_ops()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TODAY: ClvmTarget = ClvmTarget::TODAY;
    const TODAY_WITH_LIMITS: ClvmTarget = ClvmTarget::TODAY_WITH_LIMITS;
    const HARD_FORK_2: ClvmTarget = ClvmTarget::HARD_FORK_2;

    /// Whether each flag that hard fork 2 changes is set, in the order of the assertions below.
    fn hard_fork_flags(target: ClvmTarget) -> [bool; 6] {
        let flags = target.flags();

        [
            flags.contains(ClvmFlags::NEW_COST_MODEL),
            flags.contains(ClvmFlags::ENABLE_KECCAK_OPS_OUTSIDE_GUARD),
            flags.contains(ClvmFlags::ENABLE_SECP_OPS),
            flags.contains(ClvmFlags::RELAXED_BLS),
            flags.contains(ClvmFlags::DISABLE_OP),
            flags.contains(ClvmFlags::LIMITS),
        ]
    }

    #[test]
    fn flags_match_the_target() {
        // Before hard fork 2, only `operand_limits` adds `LIMITS`, and `DISABLE_OP` along with it
        assert_eq!(hard_fork_flags(TODAY), [false; 6]);
        assert_eq!(
            hard_fork_flags(TODAY_WITH_LIMITS),
            [false, false, false, false, true, true]
        );

        // Hard fork 2 enables the new operators and cost model, and removes both limits, even with
        // `operand_limits`
        for operand_limits in [false, true] {
            let target = ClvmTarget {
                operand_limits,
                ..HARD_FORK_2
            };
            assert_eq!(
                hard_fork_flags(target),
                [true, true, true, true, false, false]
            );
            assert!(!target.limits_operands());
        }

        assert!(!TODAY.limits_operands());
        assert!(TODAY_WITH_LIMITS.limits_operands());

        // Every target is mempool mode otherwise
        for target in [TODAY, TODAY_WITH_LIMITS, HARD_FORK_2] {
            assert!(
                target
                    .flags()
                    .contains(MEMPOOL_MODE.difference(ClvmFlags::DISABLE_OP))
            );
        }
    }

    #[test]
    fn secp_opcodes_match_the_target() {
        assert_eq!(
            ClvmOp::Secp256K1Verify.to_target_atom(TODAY),
            [0x13, 0xd6, 0x1f, 0x00]
        );
        assert_eq!(ClvmOp::Secp256K1Verify.to_target_atom(HARD_FORK_2), [64]);
        assert_eq!(
            ClvmOp::Secp256R1Verify.to_target_atom(TODAY),
            [0x1c, 0x3a, 0x8f, 0x00]
        );
        assert_eq!(ClvmOp::Secp256R1Verify.to_target_atom(HARD_FORK_2), [65]);
        assert_eq!(
            ClvmOp::Sha256.to_target_atom(HARD_FORK_2),
            ClvmOp::Sha256.to_atom()
        );
    }
}
