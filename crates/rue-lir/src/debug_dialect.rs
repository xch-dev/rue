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

/// The flags used to run Rue programs, given whether the program is compiled with
/// `operand_limits` (see `CompilerOptions::operand_limits` in `rue-options`). These don't exactly
/// match any flags Chia uses. They're mempool mode, but with modpow and keccak256 enabled outside
/// of the softfork guard as they are after hard fork 2 (`DISABLE_OP` turns modpow off until then).
/// Costs use the cost model from before hard fork 2.
///
/// With `operand_limits`, they also include `LIMITS`, which mainnet enforces in both the mempool
/// and blocks until hard fork 2. It rejects some large operands of `*`, `/`, `%`, `divmod`,
/// `modpow`, `g1_multiply` and `g2_multiply` (see [`MAX_MUL_OPERAND_BYTES`] and the constants
/// after it), so these operators can fail for operands that aren't known at compile time. Without
/// it, operands aren't limited by their length, as they won't be after hard fork 2 (which also
/// removes the 2048 byte limit that `DISABLE_OP` puts on the dividend of `/`, `%` and `divmod`).
pub const fn clvm_flags(operand_limits: bool) -> ClvmFlags {
    let flags = MEMPOOL_MODE
        .union(ClvmFlags::ENABLE_KECCAK_OPS_OUTSIDE_GUARD)
        .difference(ClvmFlags::DISABLE_OP);

    if operand_limits {
        flags.union(ClvmFlags::LIMITS)
    } else {
        flags
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
