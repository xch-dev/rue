//! What a program is decompiled into: functions with bodies of statements and expressions, which
//! mean exactly what the CLVM they come from does (see [`crate::eval`]).
//!
//! Operands are kept in the order `clvmr` evaluates them in, which is from the last to the first,
//! and so are the `let`s that bind them.

use crate::program::{Layout, NodeId, Path, Program};
use crate::stack::{Deep, grow_stack};

/// The index of a variable bound by a `let`, which is unique within its function.
pub(crate) type VarId = usize;

/// The index of a function in an explanation.
pub(crate) type FunctionId = usize;

/// What a reference reads from.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Root {
    /// The environment the function (or the whole program) is run with.
    Env,
    Var(VarId),
}

/// A value, along with as much of its structure as is known: the environments that are built for
/// functions and inline bodies, and the data the program builds.
#[derive(Debug, Clone)]
pub(crate) enum Value {
    /// A value in the program, which is the node itself (whose value is quoted, or nil).
    Const(NodeId),
    /// The part of the environment or a variable at the path.
    Ref(Root, Path),
    /// A pair that's built from its parts, which are evaluated rest first.
    Cons(Deep<Value>, Deep<Value>),
    /// A value that's computed.
    Expr(Deep<Expr>),
}

impl Value {
    pub(crate) fn cons(first: Value, rest: Value) -> Self {
        Self::Cons(Deep::new(first), Deep::new(rest))
    }

    /// The part of the value at the path, if it's known: `None` if the path goes into a computed
    /// part, or through an atom (which fails at runtime).
    pub(crate) fn at(&self, program: &Program, path: &[bool]) -> Option<Value> {
        let mut value = self;

        for (index, &step) in path.iter().enumerate() {
            match value {
                Value::Cons(first, rest) => value = if step { rest } else { first },
                Value::Const(node) => {
                    return program.descend(*node, &path[index..]).map(Value::Const);
                }
                Value::Ref(root, prefix) => {
                    let mut full = prefix.clone();
                    full.extend_from_slice(&path[index..]);
                    return Some(Value::Ref(*root, full));
                }
                Value::Expr(_) => return None,
            }
        }

        Some(value.clone())
    }

    /// Whether any part of the value is computed.
    pub(crate) fn has_computed(&self) -> bool {
        match self {
            Value::Expr(_) => true,
            Value::Cons(first, rest) => grow_stack(|| first.has_computed() || rest.has_computed()),
            Value::Const(_) | Value::Ref(..) => false,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Expr {
    pub kind: ExprKind,
    /// The node of code the expression comes from, which locates it in the source.
    pub node: NodeId,
}

#[derive(Debug, Clone)]
pub(crate) enum ExprKind {
    Value(Value),
    /// A CLVM operator (other than `q` and `a`), which is applied to its operands after they're
    /// evaluated. The operator is the node of its atom.
    Op {
        op: NodeId,
        args: Deep<Vec<Expr>>,
    },
    /// Evaluates the condition, then only one branch: the first if the condition isn't nil.
    If {
        condition: Deep<Expr>,
        then: Block,
        otherwise: Block,
    },
    /// Runs the body of a function with the environment.
    Call {
        function: FunctionId,
        env: Value,
    },
    /// Runs a program that's only known at runtime with the environment, which is evaluated
    /// first.
    Run {
        program: Deep<Expr>,
        env: Value,
    },
    /// CLVM code that isn't decompiled, which is evaluated as it is with the environment.
    Raw {
        code: NodeId,
        env: Value,
    },
}

#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub statements: Vec<Statement>,
    pub value: Deep<Expr>,
}

#[derive(Debug, Clone)]
pub(crate) enum Statement {
    Let {
        var: VarId,
        value: Expr,
    },
    /// Fails (with `raise` of the arguments) unless the condition isn't nil.
    Assert {
        condition: Expr,
        raise: Deep<Vec<Expr>>,
        node: NodeId,
    },
}

/// A hidden part of a function's environment that every call passes the same constant for.
#[derive(Debug, Clone)]
pub(crate) struct KnownSlot {
    pub path: Path,
    pub value: NodeId,
}

/// The value at a path of a function's environment, which is the constant every call passes if
/// it's in a hidden part that's known.
pub(crate) fn known_env(program: &Program, known: &[KnownSlot], path: Path) -> Value {
    for slot in known {
        if path.starts_with(&slot.path)
            && let Some(part) = program.descend(slot.value, &path[slot.path.len()..])
        {
            return Value::Const(part);
        }
    }

    Value::Ref(Root::Env, path)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FunctionKind {
    /// A function that the debug metadata describes.
    Described,
    /// A program that's run from a constant, which the debug metadata doesn't describe.
    Anonymous,
    /// The whole program, if it isn't the body of a function.
    Program,
}

#[derive(Debug, Clone)]
pub(crate) struct Function {
    pub name: String,
    pub kind: FunctionKind,
    /// The node of the body.
    pub node: NodeId,
    /// The layout of the environment, if the debug metadata describes it.
    pub layout: Option<Layout>,
    /// The hidden parts of the environment that are known, which the body uses the values of.
    pub known: Vec<KnownSlot>,
    /// Which hidden parts of the environment the body reads, which calls show.
    pub reads_hidden: Vec<bool>,
    pub body: Block,
    /// The names of the variables.
    pub vars: Vec<String>,
}
