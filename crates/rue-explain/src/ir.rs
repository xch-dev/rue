//! What a program is decompiled into: functions with bodies of statements and expressions, which
//! mean exactly what the CLVM they come from does (see [`crate::eval`]).
//!
//! Operands are kept in the order they're written in, and evaluated (and their `let`s bound) in
//! the order `clvmr` evaluates them in, which is from the last to the first.

use std::borrow::Cow;

use crate::program::{Layout, NodeId, Path, Program};
use crate::stack::Deep;

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
    /// Nil, as an atom of code that's all zero bytes (other than nil itself) evaluates to.
    Nil,
    /// The part of the environment or a variable at the path.
    Ref(Root, Path),
    /// A pair that's built from its parts, which are evaluated rest first.
    Cons(Deep<Value>, Deep<Value>, Shape),
    /// A value that's computed.
    Expr(Deep<Expr>),
}

/// How long a path can be before a value that's read from it is bound to a variable (like a
/// computed value) rather than read again at each use, which keeps it from being shown many times.
pub(crate) const MAX_PATH: usize = 64;

/// What's known about a pair's parts without looking at them.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Shape {
    /// Whether any part is computed (or read from a long path).
    pub computed: bool,
    /// How many values the pair is made of, counting a part each time it's used (saturating).
    pub size: usize,
}

impl Value {
    pub(crate) fn cons(first: Value, rest: Value) -> Self {
        let shape = Shape {
            computed: first.has_computed() || rest.has_computed(),
            size: first.size().saturating_add(rest.size()).saturating_add(1),
        };
        Self::Cons(Deep::new(first), Deep::new(rest), shape)
    }

    /// How many values the value is made of, as it's shown.
    pub(crate) fn size(&self) -> usize {
        match self {
            Value::Cons(_, _, shape) => shape.size,
            _ => 1,
        }
    }

    /// The part of the value at the path, if it's known: `None` if the path goes into a computed
    /// part, or through an atom (which fails at runtime).
    pub(crate) fn at(&self, program: &Program, path: &[bool]) -> Option<Cow<'_, Value>> {
        let mut value = self;

        for (index, &step) in path.iter().enumerate() {
            match value {
                Value::Cons(first, rest, _) => value = if step { rest } else { first },
                Value::Const(node) => {
                    return program
                        .descend(*node, &path[index..])
                        .map(|node| Cow::Owned(Value::Const(node)));
                }
                Value::Ref(root, prefix) => {
                    let mut full = prefix.clone();
                    full.extend_from_slice(&path[index..]);
                    return Some(Cow::Owned(Value::Ref(*root, full)));
                }
                Value::Nil | Value::Expr(_) => return None,
            }
        }

        Some(Cow::Borrowed(value))
    }

    /// Like [`at`](Self::at), with the constant for a hidden part of the environment that's
    /// known.
    pub(crate) fn resolve(
        &self,
        program: &Program,
        known: &[KnownSlot],
        path: &[bool],
    ) -> Option<Cow<'_, Value>> {
        let value = self.at(program, path)?;
        Some(match value.as_ref() {
            Value::Ref(Root::Env, path) => Cow::Owned(known_env(program, known, path.clone())),
            _ => value,
        })
    }

    /// Whether any part of the value is computed, or read from a path longer than [`MAX_PATH`],
    /// which are bound to variables before the value is used.
    pub(crate) fn has_computed(&self) -> bool {
        match self {
            Value::Expr(_) => true,
            Value::Cons(_, _, shape) => shape.computed,
            Value::Ref(_, path) => path.len() > MAX_PATH,
            Value::Const(_) | Value::Nil => false,
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
        value: Deep<Expr>,
    },
    /// Fails (with `raise` of the arguments) unless the condition isn't nil.
    Assert {
        condition: Deep<Expr>,
        raise: Deep<Vec<Expr>>,
        node: NodeId,
    },
}

/// A hidden part of a function's environment that every call to a version of it passes the same
/// constant for.
#[derive(Debug, Clone)]
pub(crate) struct KnownSlot {
    pub path: Path,
    pub value: NodeId,
    /// Whether the body uses a part of the environment that has the value in it as data (other
    /// than by reading the value itself), so that it can be run with another environment.
    pub exposed: bool,
}

/// The value at a path of a function's environment, which is the constant every call passes if
/// it's in a hidden part that's known.
fn known_env(program: &Program, known: &[KnownSlot], path: Path) -> Value {
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
    /// The hidden parts of the environment that are known, which the body uses the values of
    /// (and every call to this version passes).
    pub known: Vec<KnownSlot>,
    /// The version of the function that knows nothing about its environment, which is what its
    /// code is wherever it's used other than in a call.
    pub generic: FunctionId,
    /// Where the function is first called from, if it's a version, which orders the versions the
    /// way they're read.
    pub call: Option<NodeId>,
    /// Whether the body reads each hidden part of the environment, which calls only show if it
    /// does (or if what they pass is computed).
    pub reads_hidden: Vec<bool>,
    pub body: Block,
    /// The names of the variables.
    pub vars: Vec<String>,
}
