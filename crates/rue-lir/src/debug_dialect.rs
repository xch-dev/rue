use std::{cell::RefCell, fmt, rc::Rc};

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

/// The flags used to run Rue programs. These match mempool mode, with keccak256 enabled, and
/// with modpow left enabled (`DISABLE_OP` turns it off in newer versions of mempool mode).
pub const CLVM_FLAGS: ClvmFlags = MEMPOOL_MODE
    .union(ClvmFlags::ENABLE_KECCAK_OPS_OUTSIDE_GUARD)
    .difference(ClvmFlags::DISABLE_OP);

/// The cost charged for each byte of a serialized program. This is `cost_per_byte` in Chia's
/// mainnet consensus constants, which aren't available from `clvmr` itself.
pub const COST_PER_BYTE: u64 = 12_000;

#[derive(Debug, Clone)]
pub struct PrintMessage {
    pub srcloc: String,
    pub message: String,
}

/// Displays the value printed by the `debug` statement at a location, or returns `None` to
/// display it without a type.
pub type DisplayPrint = dyn Fn(&str, &Allocator, NodePtr) -> Option<String>;

#[derive(Clone)]
pub struct DebugDialect {
    flags: ClvmFlags,
    stdout: bool,
    log: RefCell<Vec<PrintMessage>>,
    display: Option<Rc<DisplayPrint>>,
}

impl fmt::Debug for DebugDialect {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DebugDialect")
            .field("flags", &self.flags)
            .field("stdout", &self.stdout)
            .field("log", &self.log)
            .field("display", &self.display.is_some())
            .finish()
    }
}

impl DebugDialect {
    pub fn new(flags: ClvmFlags, stdout: bool) -> Self {
        Self {
            flags,
            stdout,
            log: RefCell::new(Vec::new()),
            display: None,
        }
    }

    /// Displays printed values with `display` (for example, using their types), falling back to
    /// disassembling them where it returns `None`.
    #[must_use]
    pub fn with_display(
        mut self,
        display: impl Fn(&str, &Allocator, NodePtr) -> Option<String> + 'static,
    ) -> Self {
        self.display = Some(Rc::new(display));
        self
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

            let message = self
                .display
                .as_ref()
                .and_then(|display| display(&srcloc, allocator, value))
                .unwrap_or_else(|| disassemble_value(allocator, value));

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
