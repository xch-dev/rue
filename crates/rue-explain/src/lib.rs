//! Decompiles a compiled CLVM program into Rue-like pseudocode, using its debug metadata sidecar
//! (see `rue_compiler::debug_metadata`) to name functions and the values of their environments,
//! and to locate each line in the source. It works from the program and the sidecar alone, so it
//! can explain a program that's only known by its serialization, such as one from the chain.
//!
//! A function whose calls pass constants for the parts of its environment that the metadata
//! doesn't name (such as the functions and constants it captures) has a version for each set of
//! constants, which uses them, and the version that knows none of them is what its code is when
//! it's used as a value. A large constant that's used more than once is named and shown once.
//!
//! What's shown is meant to mean exactly what the CLVM does, which [`Explanation::check`]
//! checks by running the decompiled form: code whose structure isn't recognized is shown as CLVM
//! (`clvm!(...)`) and evaluated as it is. Two things are left out on purpose:
//!
//! - Reading from the environment is shown where the value is used, while in CLVM it's read where
//!   it's passed. A read through an atom fails, which only happens if the environment doesn't have
//!   the shape the code expects, so the decompiled form can fail later (or not at all) for an
//!   environment like that.
//! - Computed values are bound by `let`s before the statement they're in. Everything but the
//!   branches of an `if` is evaluated strictly, and the `let`s are in the order `clvmr` evaluates
//!   operands in (from the last to the first), but a `let` can come before an operand that's
//!   evaluated earlier, which only changes which of several failures is reported.

mod build;
mod eval;
mod ir;
mod print;
mod program;
mod stack;

use std::collections::HashMap;

use crate::ir::{Function, FunctionId};
use crate::program::{NodeId, Program};

/// A program that's been decompiled.
#[derive(Debug)]
pub struct Explanation {
    program: Program,
    functions: Vec<Function>,
    /// The function that's the whole program.
    entry: FunctionId,
    /// The function each body of a function is (including copies of it).
    function_of: HashMap<NodeId, FunctionId>,
    /// Whether each node of the program is part of what's shown.
    covered: Vec<bool>,
}

#[derive(Debug, thiserror::Error)]
pub enum ExplainError {
    #[error("the program isn't valid CLVM: {0}")]
    Program(String),
    #[error("the debug metadata isn't valid for the program: {0}")]
    Metadata(String),
}

impl Explanation {
    /// Decompiles the serialized program, with its debug metadata sidecar if there is one (which
    /// must be for exactly this program).
    pub fn new(program: &[u8], sidecar: Option<&[u8]>) -> Result<Self, ExplainError> {
        let program = Program::new(program, sidecar)?;
        let built = build::build(&program);

        Ok(Self {
            program,
            functions: built.functions,
            entry: built.entry,
            function_of: built.function_of,
            covered: built.covered,
        })
    }

    /// The pseudocode, with a source location on each line that has one.
    pub fn render(&self) -> String {
        print::render(self)
    }

    /// How many nodes of the program aren't part of what's shown, which should be none.
    pub fn uncovered(&self) -> usize {
        self.covered.iter().filter(|covered| !**covered).count()
    }
}
