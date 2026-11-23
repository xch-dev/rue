//! Compares ways of lowering the same code by the size of the program they compile to.
//!
//! Each byte of a program costs far more on chain than anything it does when it's run, and how
//! large a program ends up depends on what the optimizer makes of it, such as which paths fit in
//! one byte and which values fold together. So where there's a choice between two ways of
//! lowering something that behave the same, each is lowered, optimized and generated, and the
//! smaller one is kept, rather than estimating which one it will be.

use rue_lir::{ClvmTarget, CodegenOptions, LirId, compiled_size, is_pure, optimize};

use rue_options::CompilerOptions;

use crate::{Environment, HirId};

use super::{Lowerer, LoweringDebugInfo, TooDeep};

/// One way of lowering something, which is kept if it's the smallest (see [`Lowerer::trial`]).
#[derive(Debug)]
pub(super) struct Trial {
    pub lir: LirId,
    /// How many bytes the LIR compiles to on its own.
    pub size: usize,
    /// What was recorded for a debug sidecar once it was lowered, if one was requested.
    debug: Option<LoweringDebugInfo>,
    /// Whether lowering it went too deep, which it didn't before.
    too_deep: Option<TooDeep>,
}

impl Trial {
    /// Whether lowering it went too deep, which it didn't before.
    pub fn is_too_deep(&self) -> bool {
        self.too_deep.is_some()
    }
}

impl Lowerer<'_, '_, '_> {
    /// Lowers something one way, and measures it. What it recorded for a debug sidecar is set
    /// aside, so that another way of lowering it starts from the same state. The one that's kept
    /// is passed to [`Self::keep`].
    ///
    /// One way of lowering something can nest deeper than another, so a trial that goes too deep
    /// is as large as it gets, and lowering only went too deep if the trial that's kept did.
    pub(super) fn trial(&mut self, f: impl FnOnce(&mut Self) -> LirId) -> Trial {
        let debug_before = self.debug.clone();
        let was_too_deep = self.too_deep.is_some();

        let lir = f(self);

        let too_deep = if was_too_deep {
            None
        } else {
            self.too_deep.take()
        };

        let size = if too_deep.is_some() {
            usize::MAX
        } else {
            self.compiled_size(lir)
        };

        let debug = std::mem::replace(&mut self.debug, debug_before);

        Trial {
            lir,
            size,
            debug,
            too_deep,
        }
    }

    /// Keeps the result of a trial, along with what it recorded for a debug sidecar.
    pub(super) fn keep(&mut self, trial: Trial) -> LirId {
        self.debug = trial.debug;

        if self.too_deep.is_none() {
            self.too_deep = trial.too_deep;
        }

        trial.lir
    }

    /// Whether the value is compiled to a constant, which is the same wherever it's evaluated,
    /// and can't fail. That's found by lowering and optimizing it, rather than guessing what the
    /// optimizer folds, and what's lowered is discarded.
    ///
    /// The value is lowered with nothing in the environment. It isn't a constant if lowering it
    /// reads anything from there, such as a parameter, a value that's bound, or a function that
    /// isn't inlined, which ends the lowering.
    pub(super) fn folds_to_constant(&mut self, value: HirId) -> bool {
        debug_assert!(self.missing_paths.is_none());

        if !self.options.optimize_lir {
            return false;
        }

        // What's recorded while lowering it is discarded, as for a trial that isn't kept
        let debug = self.debug.clone();
        let was_too_deep = self.too_deep.is_some();

        self.missing_paths = Some(false);
        let lir = self.lower_hir(&Environment::Nil, value);
        let missing = self.missing_paths.take() == Some(true);

        self.debug = debug;
        let too_deep = !was_too_deep && self.too_deep.take().is_some();

        if missing || too_deep {
            return false;
        }

        // A value that's pure can't fail, and is the same wherever it's evaluated
        let lir = optimize(self.arena, lir, self.target());
        is_pure(self.arena, lir)
    }

    /// What the program is compiled for.
    pub(super) fn target(&self) -> ClvmTarget {
        Self::target_of(&self.options)
    }

    /// What a program compiled with the options is compiled for.
    pub(super) fn target_of(options: &CompilerOptions) -> ClvmTarget {
        ClvmTarget {
            operand_limits: options.operand_limits,
            hard_fork_2: options.hard_fork_2,
        }
    }

    /// How many bytes the LIR compiles to on its own, with the options the program is compiled
    /// with. Code that can't be generated is as large as it gets, so it's never chosen over code
    /// that can.
    fn compiled_size(&mut self, lir: LirId) -> usize {
        let options = CodegenOptions {
            optimize_static_pairs: self.options.optimize_static_pairs,
            target: self.target(),
        };

        compiled_size(self.arena, lir, self.options.optimize_lir, options).unwrap_or(usize::MAX)
    }
}
