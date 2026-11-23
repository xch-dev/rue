//! Runs a decompiled program, to check that it means what the CLVM does. Operators are applied by
//! `clvmr` itself (to their values, quoted), and code that isn't decompiled is run by it as it
//! is, so what this checks is the structure: environments, names, calls, branches and constants.
//!
//! Programs that are built at runtime (such as closures) are run by a small interpreter that
//! runs the decompiled body of a function whenever one of them runs a function's code, so those
//! bodies are checked too.

use std::collections::HashMap;

use clvmr::{
    Allocator, NodePtr, SExp, cost::Cost, dialect::Dialect, error::EvalErr,
    run_program::run_program,
};
use rue_clvm::operator_name;

use crate::Explanation;
use crate::ir::{Block, Expr, ExprKind, FunctionId, Root, Statement, Value};
use crate::program::{NodeId, NodeKind, atom_path};

/// What a step of evaluation costs, on top of the operators, so that a program that doesn't
/// terminate runs out.
const STEP_COST: Cost = 10;

#[derive(Debug, thiserror::Error)]
pub enum EvalFailure {
    /// The program fails, as it does in CLVM.
    #[error("{0}")]
    Clvm(EvalErr),
    /// The program ran out of the cost it was given, which isn't the same as CLVM's cost.
    #[error("ran out of cost")]
    OutOfCost,
}

impl From<EvalErr> for EvalFailure {
    fn from(error: EvalErr) -> Self {
        match error {
            EvalErr::CostExceeded => Self::OutOfCost,
            error => Self::Clvm(error),
        }
    }
}

type Result<T> = std::result::Result<T, EvalFailure>;

impl Explanation {
    /// Runs the decompiled program with the solution, with operators applied by the dialect.
    pub fn evaluate<D: Dialect>(
        &self,
        allocator: &mut Allocator,
        dialect: &D,
        solution: NodePtr,
        max_cost: Cost,
    ) -> Result<NodePtr> {
        let mut evaluator = Evaluator {
            explanation: self,
            allocator,
            dialect,
            budget: max_cost,
            materialized: HashMap::new(),
            programs: HashMap::new(),
        };

        // Running one of these values as a program runs the function's decompiled body
        for (&node, &id) in &self.function_of {
            let ptr = evaluator.materialize(node)?;
            evaluator.programs.insert(ptr, id);
        }

        evaluator.function(self.entry, solution)
    }
}

struct Frame {
    env: NodePtr,
    vars: Vec<Option<NodePtr>>,
}

struct Evaluator<'a, D> {
    explanation: &'a Explanation,
    allocator: &'a mut Allocator,
    dialect: &'a D,
    budget: Cost,
    materialized: HashMap<NodeId, NodePtr>,
    programs: HashMap<NodePtr, FunctionId>,
}

impl<D: Dialect> Evaluator<'_, D> {
    fn charge(&mut self, cost: Cost) -> Result<()> {
        self.budget = self
            .budget
            .checked_sub(cost)
            .ok_or(EvalFailure::OutOfCost)?;
        Ok(())
    }

    /// The node of the program as a value, which is the same value each time.
    fn materialize(&mut self, root: NodeId) -> Result<NodePtr> {
        let program = &self.explanation.program;
        let mut stack = vec![(root, false)];

        while let Some((node, children_done)) = stack.pop() {
            if self.materialized.contains_key(&node) {
                continue;
            }

            let ptr = match program.kind(node) {
                NodeKind::Atom(atom) => self.allocator.new_atom(atom)?,
                &NodeKind::Pair(first, rest) => {
                    if !children_done {
                        stack.push((node, true));
                        stack.push((rest, false));
                        stack.push((first, false));
                        continue;
                    }
                    self.allocator
                        .new_pair(self.materialized[&first], self.materialized[&rest])?
                }
            };

            self.materialized.insert(node, ptr);
        }

        Ok(self.materialized[&root])
    }

    fn function(&mut self, id: FunctionId, env: NodePtr) -> Result<NodePtr> {
        self.charge(STEP_COST)?;
        let function = &self.explanation.functions[id];
        let mut frame = Frame {
            env,
            vars: vec![None; function.vars.len()],
        };
        self.block(&mut frame, &function.body)
    }

    fn block(&mut self, frame: &mut Frame, block: &Block) -> Result<NodePtr> {
        for statement in &block.statements {
            match statement {
                Statement::Let { var, value } => {
                    frame.vars[*var] = Some(self.expr(frame, value)?);
                }
                Statement::Assert {
                    condition, raise, ..
                } => {
                    let condition = self.expr(frame, condition)?;
                    if self.is_nil(condition) {
                        let args = raise
                            .iter()
                            .map(|arg| self.expr(frame, arg))
                            .collect::<Result<Vec<_>>>()?;
                        let op = self.allocator.new_atom(&[8])?;
                        self.op(op, &args)?;
                        unreachable!("raise fails");
                    }
                }
            }
        }

        self.expr(frame, &block.value)
    }

    fn expr(&mut self, frame: &mut Frame, expr: &Expr) -> Result<NodePtr> {
        stacker::maybe_grow(256 * 1024, 4 * 1024 * 1024, || self.expr_inner(frame, expr))
    }

    fn expr_inner(&mut self, frame: &mut Frame, expr: &Expr) -> Result<NodePtr> {
        self.charge(STEP_COST)?;

        match &expr.kind {
            ExprKind::Value(value) => self.value(frame, value),
            ExprKind::Op { op, args } => {
                let args = args
                    .iter()
                    .map(|arg| self.expr(frame, arg))
                    .collect::<Result<Vec<_>>>()?;
                let op = self.materialize(*op)?;
                self.op(op, &args)
            }
            ExprKind::If {
                condition,
                then,
                otherwise,
            } => {
                let condition = self.expr(frame, condition)?;
                if self.is_nil(condition) {
                    self.block(frame, otherwise)
                } else {
                    self.block(frame, then)
                }
            }
            ExprKind::Call { function, env } => {
                let env = self.value(frame, env)?;
                self.function(*function, env)
            }
            ExprKind::Run { program, env } => {
                let program = self.expr(frame, program)?;
                let env = self.value(frame, env)?;
                self.run(program, env)
            }
            ExprKind::Raw { code, env } => {
                let env = self.value(frame, env)?;
                let code = self.materialize(*code)?;
                self.interpret(code, env)
            }
        }
    }

    fn value(&mut self, frame: &mut Frame, value: &Value) -> Result<NodePtr> {
        match value {
            Value::Const(node) => self.materialize(*node),
            Value::Ref(root, path) => {
                let mut ptr = match root {
                    Root::Env => frame.env,
                    Root::Var(var) => {
                        frame.vars[*var].expect("a variable is bound before it's used")
                    }
                };
                for &step in path {
                    ptr = match self.allocator.sexp(ptr) {
                        SExp::Pair(first, rest) => {
                            if step {
                                rest
                            } else {
                                first
                            }
                        }
                        SExp::Atom => return Err(EvalFailure::Clvm(EvalErr::PathIntoAtom)),
                    };
                }
                Ok(ptr)
            }
            Value::Cons(first, rest) => {
                let first = self.value(frame, first)?;
                let rest = self.value(frame, rest)?;
                Ok(self.allocator.new_pair(first, rest)?)
            }
            Value::Expr(expr) => self.expr(frame, expr),
        }
    }

    fn is_nil(&self, value: NodePtr) -> bool {
        matches!(self.allocator.sexp(value), SExp::Atom) && self.allocator.atom_len(value) == 0
    }

    /// Applies the operator to the values, by running it in CLVM with each of them quoted.
    fn op(&mut self, op: NodePtr, args: &[NodePtr]) -> Result<NodePtr> {
        let quote = self.allocator.new_atom(&[1])?;
        let mut list = NodePtr::NIL;
        for &arg in args.iter().rev() {
            let quoted = self.allocator.new_pair(quote, arg)?;
            list = self.allocator.new_pair(quoted, list)?;
        }
        let program = self.allocator.new_pair(op, list)?;
        self.clvm(program, NodePtr::NIL)
    }

    fn clvm(&mut self, program: NodePtr, env: NodePtr) -> Result<NodePtr> {
        // clvmr treats a limit of 0 as no limit
        if self.budget == 0 {
            return Err(EvalFailure::OutOfCost);
        }
        let reduction = run_program(self.allocator, self.dialect, program, env, self.budget)?;
        self.charge(reduction.0)?;
        Ok(reduction.1)
    }

    /// Runs a value as a program, which is a function's decompiled body if it's the code of one.
    fn run(&mut self, program: NodePtr, env: NodePtr) -> Result<NodePtr> {
        if let Some(&id) = self.programs.get(&program) {
            return self.function(id, env);
        }
        self.interpret(program, env)
    }

    /// Runs code like CLVM does, except that the programs it runs (with `a`) are run by
    /// [`run`](Self::run). Anything unusual is left to `clvmr`.
    fn interpret(&mut self, code: NodePtr, env: NodePtr) -> Result<NodePtr> {
        stacker::maybe_grow(256 * 1024, 4 * 1024 * 1024, || {
            self.interpret_inner(code, env)
        })
    }

    fn interpret_inner(&mut self, code: NodePtr, env: NodePtr) -> Result<NodePtr> {
        self.charge(STEP_COST)?;

        let SExp::Pair(op, args) = self.allocator.sexp(code) else {
            let atom = self.allocator.atom(code).as_ref().to_vec();
            let Some(path) = atom_path(&atom) else {
                return Ok(NodePtr::NIL);
            };
            let mut ptr = env;
            for step in path {
                ptr = match self.allocator.sexp(ptr) {
                    SExp::Pair(first, rest) => {
                        if step {
                            rest
                        } else {
                            first
                        }
                    }
                    SExp::Atom => return Err(EvalFailure::Clvm(EvalErr::PathIntoAtom)),
                };
            }
            return Ok(ptr);
        };

        let op_atom = match self.allocator.sexp(op) {
            SExp::Atom => self.allocator.atom(op).as_ref().to_vec(),
            SExp::Pair(..) => return self.clvm(code, env),
        };

        if operator_name(&op_atom).is_none() {
            return self.clvm(code, env);
        }

        if op_atom == [1] {
            return Ok(args);
        }

        let mut operands = Vec::new();
        let mut rest = args;
        loop {
            match self.allocator.sexp(rest) {
                SExp::Pair(first, next) => {
                    operands.push(first);
                    rest = next;
                }
                SExp::Atom => {
                    if self.allocator.atom_len(rest) != 0 {
                        return self.clvm(code, env);
                    }
                    break;
                }
            }
        }

        if op_atom == [2] && operands.len() != 2 {
            return self.clvm(code, env);
        }

        let values = operands
            .iter()
            .map(|&operand| self.interpret(operand, env))
            .collect::<Result<Vec<_>>>()?;

        if op_atom == [2] {
            return self.run(values[0], values[1]);
        }

        self.op(op, &values)
    }
}
