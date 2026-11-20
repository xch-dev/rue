//! Compares ways of lowering the same code by the size of the program they compile to.
//!
//! Each byte of a program costs far more on chain than anything it does when it's run, and how
//! large a program ends up depends on what the optimizer makes of it, such as which paths fit in
//! one byte and which values fold together. So where there's a choice between two ways of
//! lowering something that behave the same, each is lowered, optimized and generated, and the
//! smaller one is kept, rather than estimating which one it will be.

use rue_lir::{ClvmTarget, CodegenOptions, LirId, compiled_size};

use super::{Lowerer, LoweringDebugInfo};

/// One way of lowering something, which is kept if it's the smallest (see [`Lowerer::trial`]).
#[derive(Debug)]
pub(super) struct Trial {
    pub lir: LirId,
    /// How many bytes the LIR compiles to on its own.
    pub size: usize,
    /// What was recorded for a debug sidecar once it was lowered, if one was requested.
    debug: Option<LoweringDebugInfo>,
}

impl Lowerer<'_, '_, '_> {
    /// Lowers something one way, and measures it. What it recorded for a debug sidecar is set
    /// aside, so that another way of lowering it starts from the same state. The one that's kept
    /// is passed to [`Self::keep`].
    pub(super) fn trial(&mut self, f: impl FnOnce(&mut Self) -> LirId) -> Trial {
        let before = self.debug.clone();
        let lir = f(self);
        let size = self.compiled_size(lir);
        let debug = std::mem::replace(&mut self.debug, before);
        Trial { lir, size, debug }
    }

    /// Keeps the result of a trial, along with what it recorded for a debug sidecar.
    pub(super) fn keep(&mut self, trial: Trial) -> LirId {
        self.debug = trial.debug;
        trial.lir
    }

    /// How many bytes the LIR compiles to on its own, with the options the program is compiled
    /// with. Code that can't be generated is as large as it gets, so it's never chosen over code
    /// that can.
    fn compiled_size(&mut self, lir: LirId) -> usize {
        let options = CodegenOptions {
            optimize_static_pairs: self.options.optimize_static_pairs,
            target: ClvmTarget {
                operand_limits: self.options.operand_limits,
                hard_fork_2: self.options.hard_fork_2,
            },
        };

        compiled_size(self.arena, lir, self.options.optimize_lir, options).unwrap_or(usize::MAX)
    }
}
