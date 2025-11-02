use std::cell::RefCell;

use chialisp::classic::clvm_tools::binutils::disassemble;
use clvm_traits::{FromClvm, destructure_tuple, match_tuple};
use clvmr::{
    Allocator, ChiaDialect, NodePtr,
    dialect::{Dialect, OperatorSet},
    error::EvalErr,
    reduction::Reduction,
};
use colored::Colorize;

use crate::ClvmOp;

#[derive(Debug, Clone)]
pub struct PrintMessage {
    pub srcloc: String,
    pub message: String,
}

#[derive(Debug, Clone)]
pub struct DebugDialect {
    flags: u32,
    stdout: bool,
    log: RefCell<Vec<PrintMessage>>,
}

impl DebugDialect {
    pub fn new(flags: u32, stdout: bool) -> Self {
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

            let message = disassemble(allocator, value, None);

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
