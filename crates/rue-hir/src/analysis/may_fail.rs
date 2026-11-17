//! Finds whether evaluating a value may fail.

use std::{collections::HashSet, rc::Rc};

use indexmap::{IndexMap, IndexSet};

use num_bigint::BigInt;
use rue_lir::{ClvmOp, atom_bigint};

use crate::{BinaryOp, Database, FunctionKind, Hir, HirId, Statement, Symbol, SymbolId, UnaryOp};

use super::AnalysisContext;

/// The largest shift that CLVM allows, in either direction.
const MAX_SHIFT: u32 = 65535;

/// Whether evaluating the HIR may fail for well-typed values, such as a solution that matches the
/// parameter types of `main`.
///
/// This is conservative, so it's `true` for anything that might fail: `raise` and `assert`,
/// operations that fail for some well-typed operands (such as division by zero, `substr` out of
/// range, or an invalid BLS point), and calls to functions whose bodies may fail, or that can't be
/// resolved. Reading a value doesn't fail, including a field of a parameter, since the solution
/// is assumed to match the types.
///
/// A substituted value (see [`AnalysisContext::substituted_value`]) is checked where it's
/// referenced, since that's where it's evaluated.
pub(crate) fn may_fail(cx: &impl AnalysisContext, hir: HirId) -> bool {
    Visitor::new(cx).visit(hir, false)
}

/// Whether calling a function may fail, and the parameters whose function values it calls, which
/// are checked where it's called, since that's where their values are known.
#[derive(Debug, Clone, Default)]
pub(crate) struct FunctionFailure {
    fails: bool,
    /// The parameters whose values are called. These are the function's own parameters, or those
    /// of an enclosing function, whose values a closure captures.
    calls: Rc<[SymbolId]>,
}

impl FunctionFailure {
    fn fails() -> Self {
        Self {
            fails: true,
            calls: Rc::new([]),
        }
    }
}

/// The function value that's called, as far as it's known.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Callee {
    Function(SymbolId),
    /// The value of a parameter, which is only known where the function it belongs to is called.
    Parameter(SymbolId),
    /// A value that's only known at runtime, such as a field or the result of a call.
    Unknown,
}

/// The function value that's called. This follows bindings, so that calling a lambda that's bound
/// to a name is treated like calling the lambda itself.
pub(crate) fn resolve_callee(db: &Database, mut callee: HirId) -> Callee {
    let mut seen = HashSet::new();

    loop {
        match db.hir(db.without_casts(callee)) {
            Hir::Lambda(lambda) => return Callee::Function(*lambda),
            Hir::Reference(symbol) => match db.symbol(*symbol) {
                Symbol::Function(_) => return Callee::Function(*symbol),
                Symbol::Parameter(_) => return Callee::Parameter(*symbol),
                Symbol::Binding(binding) if seen.insert(*symbol) => {
                    callee = binding.value.hir;
                }
                Symbol::Binding(_)
                | Symbol::Constant(_)
                | Symbol::Unresolved
                | Symbol::Module(_)
                | Symbol::Builtin(_) => return Callee::Unknown,
            },
            _ => return Callee::Unknown,
        }
    }
}

/// The function that's called, if it's known (see [`resolve_callee`]).
pub(crate) fn resolve_function(db: &Database, callee: HirId) -> Option<SymbolId> {
    match resolve_callee(db, callee) {
        Callee::Function(function) => Some(function),
        Callee::Parameter(_) | Callee::Unknown => None,
    }
}

struct Visitor<'a, C> {
    cx: &'a C,
    /// The substituted values being checked, so that a value that references itself isn't
    /// checked forever.
    visiting: HashSet<SymbolId>,
    /// The functions that are assumed not to fail while their bodies are checked, in the order
    /// they were reached. Each is mapped to the earliest of these that it depends on: itself while
    /// its body is being checked, then the earliest one its body called again, if that one's
    /// result still isn't known. Entries are only ever removed from the end, so an entry never
    /// outlives one that it depends on.
    assumed: IndexMap<SymbolId, usize>,
    /// The earliest assumed function that the body being checked depends on.
    lowest_assumption: usize,
    /// The parameters whose function values are called by each function body being checked.
    calls: Vec<IndexSet<SymbolId>>,
}

impl<'a, C: AnalysisContext> Visitor<'a, C> {
    fn new(cx: &'a C) -> Self {
        Self {
            cx,
            visiting: HashSet::new(),
            assumed: IndexMap::new(),
            lowest_assumption: usize::MAX,
            calls: Vec::new(),
        }
    }

    fn db(&self) -> &'a Database {
        self.cx.db()
    }

    fn any(&mut self, hirs: impl IntoIterator<Item = HirId>, in_body: bool) -> bool {
        hirs.into_iter().any(|hir| self.visit(hir, in_body))
    }

    /// Checks the HIR. Inside of a function body (`in_body`), the parameters and captures are
    /// read from the environment, so they can't fail. Their arguments are checked where the
    /// function is called or created instead.
    fn visit(&mut self, hir: HirId, in_body: bool) -> bool {
        // This follows chains of values and calls, which can be as deep as the program
        rue_types::grow_stack(|| self.visit_inner(hir, in_body))
    }

    fn visit_inner(&mut self, hir: HirId, in_body: bool) -> bool {
        let db = self.db();
        let node = db.hir(hir);

        match node {
            Hir::Unresolved
            | Hir::Nil
            | Hir::String(_)
            | Hir::Int(_)
            | Hir::Bytes(_)
            | Hir::Bool(_)
            | Hir::InfinityG1
            | Hir::InfinityG2
            // Constant expressions are evaluated at compile time
            | Hir::Const(_) => false,
            Hir::Lambda(lambda) => !in_body && self.cx.captures_may_fail(*lambda),
            Hir::Reference(symbol) => self.visit_reference(*symbol, in_body),
            Hir::Block(block) => {
                self.visit_statements(&block.statements, in_body)
                    || self.any(block.reachable_body(), in_body)
            }
            // The value is only evaluated if the check it's lowered to uses it
            Hir::Guard(guard) => guard.value_uses() > 0 && self.visit(guard.value, in_body),
            Hir::FunctionCall(call) => {
                self.visit_call(call.function, &call.args, in_body)
                    || self.any(call.args.iter().copied(), in_body)
            }
            Hir::Unary(op, _) => {
                matches!(op, UnaryOp::G1Negate | UnaryOp::G2Negate)
                    || self.any(node.children(), in_body)
            }
            Hir::Binary(op, _, right) => {
                binary_may_fail(db, *op, *right) || self.any(node.children(), in_body)
            }
            Hir::ClvmOp(op, _) => clvm_op_may_fail(*op) || self.any(node.children(), in_body),
            Hir::CoinId(..)
            | Hir::Substr(..)
            | Hir::Modpow(..)
            | Hir::G1Map(..)
            | Hir::G2Map(..)
            | Hir::BlsPairingIdentity(_)
            | Hir::BlsVerify(..)
            | Hir::Secp256K1Verify(..)
            | Hir::Secp256R1Verify(..) => true,
            Hir::If(..)
            | Hir::List(_)
            | Hir::Struct(_)
            | Hir::FieldAccess(_)
            | Hir::Cast(_)
            | Hir::Pair(..) => self.any(node.children(), in_body),
        }
    }

    fn visit_statements(&mut self, statements: &[Statement], in_body: bool) -> bool {
        let db = self.db();

        for statement in crate::reachable_statements(statements) {
            let fails = match *statement {
                Statement::Raise(..) | Statement::Assert(..) => true,
                // Other bindings are evaluated at their statement, or where they're referenced
                // if they can't fail, or for their effect if they're never referenced
                Statement::Let(symbol, _) => match db.symbol(symbol) {
                    Symbol::Binding(binding) if !binding.inline && self.cx.is_evaluated(symbol) => {
                        self.visit(binding.value.hir, in_body)
                    }
                    _ => false,
                },
                Statement::Expr(ref statement) => self.visit(statement.hir, in_body),
                Statement::If(ref statement) => {
                    self.any([statement.condition, statement.then], in_body)
                }
                Statement::Return(hir, _) => self.visit(hir, in_body),
                // Debug statements are only lowered with debug symbols, which are only used for
                // finding errors, so whether they fail doesn't matter
                Statement::Debug(..) => false,
            };

            if fails {
                return true;
            }
        }

        false
    }

    fn visit_reference(&mut self, symbol: SymbolId, in_body: bool) -> bool {
        match self.db().symbol(symbol) {
            Symbol::Function(_) => !in_body && self.cx.captures_may_fail(symbol),
            Symbol::Parameter(_) if in_body => false,
            Symbol::Parameter(_) | Symbol::Binding(_) | Symbol::Constant(_) => {
                let Some(value) = self.cx.substituted_value(symbol) else {
                    return false;
                };

                if !self.visiting.insert(symbol) {
                    return false;
                }

                let fails = self.visit(value, in_body);
                self.visiting.remove(&symbol);
                fails
            }
            Symbol::Unresolved | Symbol::Module(_) | Symbol::Builtin(_) => false,
        }
    }

    /// Whether the call may fail, other than in evaluating its arguments.
    fn visit_call(&mut self, callee: HirId, args: &[HirId], in_body: bool) -> bool {
        match resolve_callee(self.db(), callee) {
            Callee::Function(function) => {
                (!in_body && self.cx.captures_may_fail(function))
                    || self.call_may_fail(function, args, in_body)
            }
            Callee::Parameter(parameter) => self.parameter_call_may_fail(parameter, in_body),
            // The function is only known at runtime, so it could do anything
            Callee::Unknown => true,
        }
    }

    /// Whether calling the function with the arguments may fail, including the calls it makes to
    /// the function values it's passed.
    fn call_may_fail(&mut self, function: SymbolId, args: &[HirId], in_body: bool) -> bool {
        let failure = self.function_may_fail(function);

        if failure.fails {
            return true;
        }

        let Symbol::Function(symbol) = self.db().symbol(function) else {
            return true;
        };

        failure.calls.iter().any(|&parameter| {
            if let Some(index) = symbol
                .fixed_parameters()
                .iter()
                .position(|&fixed| fixed == parameter)
            {
                return args
                    .get(index)
                    .is_none_or(|&arg| self.value_call_may_fail(arg, in_body));
            }

            // The function values in a rest parameter aren't followed. Any other parameter
            // belongs to an enclosing function, whose closure captured it.
            symbol.parameters.contains(&parameter)
                || self.parameter_call_may_fail(parameter, in_body)
        })
    }

    /// Whether calling the function value with arguments that aren't known may fail.
    fn value_call_may_fail(&mut self, value: HirId, in_body: bool) -> bool {
        match resolve_callee(self.db(), value) {
            Callee::Function(function) => {
                let failure = self.function_may_fail(function);

                if failure.fails {
                    return true;
                }

                let Symbol::Function(symbol) = self.db().symbol(function) else {
                    return true;
                };

                // The values of its own parameters aren't known, but those it captured are
                failure.calls.iter().any(|&parameter| {
                    symbol.parameters.contains(&parameter)
                        || self.parameter_call_may_fail(parameter, in_body)
                })
            }
            Callee::Parameter(parameter) => self.parameter_call_may_fail(parameter, in_body),
            Callee::Unknown => true,
        }
    }

    /// Whether calling the value of the parameter may fail. Inside of a function body, the call
    /// is recorded, and checked where the function is called, since that's where the value of the
    /// parameter is known. Otherwise, the value isn't known, so it could do anything.
    fn parameter_call_may_fail(&mut self, parameter: SymbolId, in_body: bool) -> bool {
        let Some(calls) = self.calls.last_mut().filter(|_| in_body) else {
            return true;
        };

        calls.insert(parameter);
        false
    }

    /// Whether calling the function may fail, other than in calling the function values it's
    /// passed (see [`FunctionFailure`]). A recursive call is assumed not to fail, since the body
    /// fails anyway if anything else in it does. Functions that call each other are resolved
    /// together, like the strongly connected components of the call graph, so that each
    /// function's body is only checked once.
    fn function_may_fail(&mut self, function: SymbolId) -> FunctionFailure {
        if let Some(failure) = self.cx.function_failures().borrow().get(&function) {
            return failure.clone();
        }

        if let Some(&assumption) = self.assumed.get(&function) {
            self.lowest_assumption = self.lowest_assumption.min(assumption);
            return FunctionFailure::default();
        }

        let Symbol::Function(symbol) = self.db().symbol(function) else {
            return FunctionFailure::fails();
        };

        // The body of an external function is compiled CLVM, which could do anything
        if symbol.kind == FunctionKind::External {
            self.cache(function, FunctionFailure::fails());
            return FunctionFailure::fails();
        }

        let body = symbol.body;
        let index = self.assumed.len();
        self.assumed.insert(function, index);
        let lowest_assumption = std::mem::replace(&mut self.lowest_assumption, usize::MAX);

        self.calls.push(IndexSet::new());
        let mut fails = self.visit(body, true);
        let calls = self.calls.pop().unwrap_or_default();

        // A function that only calls itself, such as one that maps a function over a list
        if !fails
            && !calls.is_empty()
            && self.lowest_assumption == index
            && self.assumed.len() == index + 1
        {
            let failure = self.self_recursive_failure(function, body, index, calls);
            self.lowest_assumption = lowest_assumption;
            return failure;
        }

        // Otherwise, the function values that a recursive call calls aren't known until the
        // functions in the cycle are resolved, so a function in a cycle that calls any is treated
        // as failing
        if !calls.is_empty() && self.lowest_assumption != usize::MAX {
            fails = true;
        }

        let failure = if fails {
            FunctionFailure::fails()
        } else {
            FunctionFailure {
                fails: false,
                calls: calls.into_iter().collect(),
            }
        };

        if fails {
            // Failing doesn't depend on any assumption. The functions that were assumed not to
            // fail since this one was reached may have been wrong, so they're checked again
            // if they're reached again.
            self.assumed.truncate(index);
            self.cache(function, failure.clone());
            self.lowest_assumption = lowest_assumption;
        } else if self.lowest_assumption >= index {
            // Every assumption made while checking the body was about this function or the ones
            // reached after it, and none of them fail. Only a function that didn't depend on any
            // assumption calls the function values it's passed, which is then the only one here.
            let mut failures = self.cx.function_failures().borrow_mut();

            for (assumed, _) in self.assumed.drain(index..) {
                let failure = if assumed == function {
                    failure.clone()
                } else {
                    FunctionFailure::default()
                };

                failures.insert(assumed, failure);
            }

            drop(failures);

            self.lowest_assumption = lowest_assumption;
        } else {
            // This depends on a function that was reached earlier, whose result isn't known yet,
            // so it stays assumed not to fail until then
            self.assumed.insert(function, self.lowest_assumption);
            self.lowest_assumption = self.lowest_assumption.min(lowest_assumption);
        }

        failure
    }

    /// Checks a function that calls itself, and calls the function values it's passed, again with
    /// the values that it calls known, so that the values its recursive calls pass are checked
    /// too. Each check can only find more of them, so this stops once it doesn't.
    fn self_recursive_failure(
        &mut self,
        function: SymbolId,
        body: HirId,
        index: usize,
        mut calls: IndexSet<SymbolId>,
    ) -> FunctionFailure {
        self.assumed.truncate(index);

        let failure = loop {
            let failure = FunctionFailure {
                fails: false,
                calls: calls.iter().copied().collect(),
            };

            self.cache(function, failure.clone());
            self.lowest_assumption = usize::MAX;

            let cached: HashSet<SymbolId> = self
                .cx
                .function_failures()
                .borrow()
                .keys()
                .copied()
                .collect();

            self.calls.push(IndexSet::new());
            let fails = self.visit(body, true);
            let more = self.calls.pop().unwrap_or_default();

            let known = calls.len();
            calls.extend(more);

            // Reaching a function whose result isn't known yet would make this depend on it, so
            // it's treated as failing instead
            let failed = fails || self.lowest_assumption != usize::MAX;

            if !failed && calls.len() == known {
                break failure;
            }

            // What was found during this check depended on calls that weren't all known yet, or
            // on this function not failing
            self.cx
                .function_failures()
                .borrow_mut()
                .retain(|symbol, _| cached.contains(symbol));

            if failed {
                self.assumed.truncate(index);
                break FunctionFailure::fails();
            }
        };

        self.cache(function, failure.clone());
        failure
    }

    fn cache(&self, function: SymbolId, failure: FunctionFailure) {
        self.cx
            .function_failures()
            .borrow_mut()
            .insert(function, failure);
    }
}

/// Whether the operation may fail for well-typed operands, given the right operand. Division
/// and shifts only fail for some right operands, so they can't fail if it's a suitable constant.
fn binary_may_fail(db: &Database, op: BinaryOp, right: HirId) -> bool {
    match op {
        BinaryOp::Div | BinaryOp::Mod | BinaryOp::Divmod => {
            constant_int(db, right).is_none_or(|divisor| divisor == BigInt::ZERO)
        }
        BinaryOp::LeftArithmeticShift
        | BinaryOp::RightArithmeticShift
        | BinaryOp::RightLogicalShift => {
            constant_int(db, right).is_none_or(|shift| shift.magnitude() > &MAX_SHIFT.into())
        }
        BinaryOp::G1Add
        | BinaryOp::G1Subtract
        | BinaryOp::G1Multiply
        | BinaryOp::G2Add
        | BinaryOp::G2Subtract
        | BinaryOp::G2Multiply => true,
        BinaryOp::Add
        | BinaryOp::Sub
        | BinaryOp::Mul
        | BinaryOp::Concat
        | BinaryOp::Gt
        | BinaryOp::Lt
        | BinaryOp::Gte
        | BinaryOp::Lte
        | BinaryOp::GtBytes
        | BinaryOp::LtBytes
        | BinaryOp::GteBytes
        | BinaryOp::LteBytes
        | BinaryOp::Eq
        | BinaryOp::Ne
        | BinaryOp::And
        | BinaryOp::Or
        | BinaryOp::All
        | BinaryOp::Any
        | BinaryOp::BitwiseAnd
        | BinaryOp::BitwiseOr
        | BinaryOp::BitwiseXor => false,
    }
}

/// Whether the builtin operation may fail for well-typed arguments. These are only created for
/// builtins that take a list of arguments.
fn clvm_op_may_fail(op: ClvmOp) -> bool {
    !matches!(
        op,
        ClvmOp::Sha256
            | ClvmOp::Keccak256
            | ClvmOp::Concat
            | ClvmOp::Add
            | ClvmOp::Sub
            | ClvmOp::Mul
            | ClvmOp::Any
            | ClvmOp::All
    )
}

/// The value of the integer, if it's known at compile time.
fn constant_int(db: &Database, hir: HirId) -> Option<BigInt> {
    match db.hir(db.without_casts(hir)) {
        Hir::Int(value) => Some(value.clone()),
        Hir::Bytes(bytes) => Some(atom_bigint(bytes)),
        Hir::Nil => Some(BigInt::ZERO),
        Hir::Const(expr) => expr.result.and_then(|result| constant_int(db, result)),
        _ => None,
    }
}
