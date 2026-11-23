//! Decompiles a compiled CLVM program into Rue-like pseudocode, using its debug metadata sidecar
//! (see `rue_compiler::debug_metadata`) to name functions and the values of their environments,
//! and to locate each line in the source. It works from the program and the sidecar alone, so it
//! can explain a program that's only known by its serialization, such as one from the chain.
//!
//! What's shown is meant to mean exactly what the CLVM does, which [`Explanation::evaluate`]
//! checks by running the decompiled form: code whose structure isn't recognized is shown as CLVM
//! (`clvm!(...)`) and evaluated as it is. Two things are left out on purpose: reading from the
//! environment is shown where the value is used (in CLVM, a read through an atom fails, which
//! only happens if the environment doesn't have the shape the code expects), and the order of
//! strict evaluation, which only changes which of several failures is reported.

mod build;
mod eval;
mod ir;
mod print;
mod program;

use std::collections::HashMap;

pub use build::FunctionKind;
pub use eval::EvalFailure;
pub use ir::*;
pub use program::{Layout, NamedSlot, Node, NodeId, NodeKind, Path, Program};

/// A program that's been decompiled.
#[derive(Debug)]
pub struct Explanation {
    pub program: Program,
    pub functions: Vec<Function>,
    pub kinds: Vec<FunctionKind>,
    /// The function that's the whole program.
    pub entry: FunctionId,
    /// The function each body of a function is (including copies of it).
    pub function_of: HashMap<NodeId, FunctionId>,
    /// Whether each node of the program is part of what's shown.
    pub covered: Vec<bool>,
    /// Whether the hidden values of environments had to be given up on.
    pub gave_up: bool,
}

#[derive(Debug, thiserror::Error)]
pub enum ExplainError {
    #[error("the program isn't valid CLVM: {0}")]
    Program(String),
    #[error("the debug metadata isn't valid for the program: {0}")]
    Metadata(String),
}

impl Explanation {
    /// Decompiles the serialized program, with its debug metadata sidecar if there is one.
    pub fn new(program: &[u8], sidecar: Option<&[u8]>) -> Result<Self, ExplainError> {
        Ok(Self::from_program(Program::new(program, sidecar)?))
    }

    pub fn from_program(program: Program) -> Self {
        let built = build::build(&program);

        Self {
            program,
            functions: built.functions,
            kinds: built.kinds,
            entry: built.entry,
            function_of: built.function_of,
            covered: built.covered,
            gave_up: built.gave_up,
        }
    }

    /// The pseudocode, with a source location on each line that has one.
    pub fn render(&self) -> String {
        print::render(self)
    }

    /// The nodes of the program that aren't part of what's shown, which should be none.
    pub fn uncovered(&self) -> Vec<NodeId> {
        self.covered
            .iter()
            .enumerate()
            .filter(|(_, covered)| !**covered)
            .map(|(node, _)| node)
            .collect()
    }
}
