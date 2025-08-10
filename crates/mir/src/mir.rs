use id_arena::Id;

use crate::{BinaryOp, UnaryOp};

pub type MirId = Id<Mir>;

#[derive(Debug, Clone)]
pub enum Mir {
    /// A literal atom value (strings, integers, hex, booleans, nil, etc).
    Atom(Vec<u8>),

    /// Creates a new environment. All bindings, captures, and parameters that
    /// are used in the body must be declared up front, even if the enclosing
    /// environment contained them.
    Function {
        body: MirId,
        captures: usize,
        parameters: usize,
    },

    /// Attaches a list of bindings to the current environment, and automatically
    /// adjusts paths into the environment based on the number of bindings.
    Bind {
        bindings: Vec<MirId>,
        body: MirId,
    },

    /// References a binding in the current environment. This starts from the
    /// beginning of the environment and ends at the start of the capture list.
    Binding(usize),

    /// References a capture in the current environment. This starts from the end
    /// of the binding list and ends at the start of the parameter list.
    Capture(usize),

    /// References a parameter in the current environment. This concludes the
    /// environment.
    Parameter(usize),

    /// Curries the captures into a function's environment, so that they don't
    /// need to be specified by the caller. This will make the function callable
    /// with a signature that matches its parameters rather than needing additional
    /// context.
    Closure {
        body: MirId,
        captures: Vec<MirId>,
    },

    /// Curries definitions into a function.
    Curry {
        body: MirId,
        args: Vec<MirId>,
    },

    Unary(UnaryOp, MirId),
    Binary(BinaryOp, MirId, MirId),
}
