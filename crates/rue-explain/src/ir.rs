//! What a program is decompiled into: functions with bodies of statements and expressions, which
//! mean exactly what the CLVM they come from does (see [`crate::eval`]).

use crate::program::{Layout, NodeId, Path};

/// The index of a variable bound by a `let`, which is unique within its function.
pub type VarId = usize;

/// The index of a function in [`Explanation::functions`](crate::Explanation::functions).
pub type FunctionId = usize;

/// What a reference reads from.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Root {
    /// The environment the function (or the whole program) is run with.
    Env,
    Var(VarId),
}

/// A value, along with as much of its structure as is known: the environments that are built for
/// functions and inline bodies, and the data the program builds.
#[derive(Debug, Clone)]
pub enum Value {
    /// A value in the program, which is the node itself (whose value is quoted, or nil).
    Const(NodeId),
    /// The part of the environment or a variable at the path.
    Ref(Root, Path),
    /// A pair that's built from its parts, which are evaluated first then rest.
    Cons(Box<Value>, Box<Value>),
    /// A value that's computed.
    Expr(Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    /// The node of code the expression comes from, which locates it in the source.
    pub node: NodeId,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Value(Value),
    /// A CLVM operator (other than `q` and `a`), which is applied to its operands after they're
    /// evaluated in order. The operator is the node of its atom.
    Op {
        op: NodeId,
        args: Vec<Expr>,
    },
    /// Evaluates the condition, then only one branch: the first if the condition isn't nil.
    If {
        condition: Box<Expr>,
        then: Block,
        otherwise: Block,
    },
    /// Runs the body of a function with the environment.
    Call {
        function: FunctionId,
        env: Value,
    },
    /// Runs a program that's only known at runtime with the environment.
    Run {
        program: Box<Expr>,
        env: Value,
    },
    /// CLVM code that isn't decompiled, which is evaluated as it is with the environment.
    Raw {
        code: NodeId,
        env: Value,
    },
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        var: VarId,
        value: Expr,
    },
    /// Fails (with `raise` of the arguments) unless the condition isn't nil.
    Assert {
        condition: Expr,
        raise: Vec<Expr>,
        node: NodeId,
    },
}

/// A value of a function's environment that every call passes the same constant for.
#[derive(Debug, Clone)]
pub struct KnownSlot {
    pub path: Path,
    pub value: NodeId,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    /// The node of the body.
    pub node: NodeId,
    /// The layout of the environment, if the debug metadata describes it.
    pub layout: Option<Layout>,
    /// The hidden parts of the environment that are known, which the body uses the values of.
    pub known: Vec<KnownSlot>,
    /// The hidden parts of the environment that the body reads, which calls show.
    pub reads_hidden: Vec<bool>,
    pub body: Block,
    /// The names of the variables.
    pub vars: Vec<String>,
}
