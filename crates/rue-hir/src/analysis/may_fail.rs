//! Finds whether evaluating a value may fail.

use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use indexmap::{IndexMap, IndexSet};

use num_bigint::BigInt;
use rue_lir::{ClvmOp, atom_bigint};

use crate::{BinaryOp, Database, FunctionKind, Hir, HirId, Statement, Symbol, SymbolId, UnaryOp};

use super::AnalysisContext;

/// The largest shift that CLVM allows, in either direction.
const MAX_SHIFT: u32 = 65535;

/// Whether evaluating the HIR may fail for values that have the shapes their types say, which the
/// compiler trusts for the solution, the results of calls it can't see into, and `unchecked_cast`
/// (see the [module docs](super) for exactly what's trusted).
///
/// This is conservative, so it's `true` for anything that might fail: `raise` and `assert`,
/// operations that fail for some well-typed operands (such as division by zero, `substr` out of
/// range, or an invalid BLS point), and calls to functions whose bodies may fail, or that can't be
/// resolved. Reading a value doesn't fail, including a field of a parameter or of a value
/// returned by a call that can't be resolved, since its shape is trusted to match its type.
///
/// A substituted value (see [`AnalysisContext::substituted_value`]) is checked where it's
/// referenced, since that's where it's evaluated.
pub(crate) fn may_fail(cx: &impl AnalysisContext, hir: HirId) -> bool {
    Visitor::new(cx).visit(hir, false)
}

/// Whether evaluating the HIR, which is in a function body, may fail other than by calling the
/// function values of parameters. If it can't, these are the parameters whose values it calls,
/// which are only known where the function they belong to is called.
pub(crate) fn body_calls(cx: &impl AnalysisContext, hir: HirId) -> Option<IndexSet<SymbolId>> {
    let (fails, calls) = Visitor::new(cx).visit_body(hir);
    (!fails).then_some(calls)
}

/// Whether calling the function value with arguments that aren't known may fail, outside of a
/// function body.
pub(crate) fn value_call_may_fail(cx: &impl AnalysisContext, hir: HirId) -> bool {
    Visitor::new(cx).value_call_may_fail(hir, false)
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
                Symbol::Constant(constant) if seen.insert(*symbol) => {
                    callee = constant.value.hir;
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

/// The result of checking a substituted value, which is reused wherever it's referenced again.
#[derive(Debug, Clone)]
struct Substituted {
    fails: bool,
    /// The parameters whose values it calls, which are recorded again where it's reused.
    calls: Rc<[SymbolId]>,
    /// The lowest index in [`Visitor::working`] whose summary it read, or `usize::MAX`.
    low: usize,
}

struct Visitor<'a, C> {
    cx: &'a C,
    /// The substituted values being checked, so that a value that references itself isn't
    /// checked forever.
    visiting: HashSet<SymbolId>,
    /// The functions whose summaries are still being found, with their summaries so far. This is
    /// the stack of Tarjan's algorithm over the call graph: a function stays here until the
    /// strongly connected component it's in is finished, and the summaries of that component are
    /// found together, starting from not failing and growing until none of them changes. Only
    /// then are they cached.
    working: IndexMap<SymbolId, FunctionFailure>,
    /// The lowest index in `working` whose summary the value being checked read, or `usize::MAX`
    /// if it only read finished summaries.
    low: usize,
    /// Whether a summary in `working` grew since the current pass over its component started.
    changed: bool,
    /// The parameters whose function values are called by each function body being checked.
    calls: Vec<IndexSet<SymbolId>>,
    /// The substituted values that have been checked, by whether they're in a function body, so
    /// that a value referenced many times (such as through a chain of inline bindings) is only
    /// checked once.
    substituted: HashMap<(SymbolId, bool), Substituted>,
}

impl<'a, C: AnalysisContext> Visitor<'a, C> {
    fn new(cx: &'a C) -> Self {
        Self {
            cx,
            visiting: HashSet::new(),
            working: IndexMap::new(),
            low: usize::MAX,
            changed: false,
            calls: Vec::new(),
            substituted: HashMap::new(),
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
                    || self.visit_arguments(call.function, &call.args, in_body)
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

                let key = (symbol, in_body);

                if let Some(entry) = self.substituted.get(&key).cloned() {
                    self.low = self.low.min(entry.low);

                    if let Some(calls) = self.calls.last_mut().filter(|_| in_body) {
                        calls.extend(entry.calls.iter().copied());
                    }

                    return entry.fails;
                }

                if let Some(&fails) = self.cx.substitution_failures().borrow().get(&key) {
                    return fails;
                }

                // A value that references itself is a recursion error, which is reported
                // elsewhere
                if !self.visiting.insert(symbol) {
                    return false;
                }

                let outer_low = std::mem::replace(&mut self.low, usize::MAX);
                let (fails, calls) = if in_body {
                    self.visit_body(value)
                } else {
                    (self.visit(value, false), IndexSet::new())
                };
                self.visiting.remove(&symbol);

                // Summaries only grow, so a value that fails with them still will
                let low = if fails { usize::MAX } else { self.low };
                self.low = outer_low.min(low);

                if let Some(outer) = self.calls.last_mut().filter(|_| in_body) {
                    outer.extend(calls.iter().copied());
                }

                if low == usize::MAX
                    && calls.is_empty()
                    && self.cx.shares_substitution(symbol, fails)
                {
                    self.cx
                        .substitution_failures()
                        .borrow_mut()
                        .insert(key, fails);
                }

                self.substituted.insert(
                    key,
                    Substituted {
                        fails,
                        calls: calls.into_iter().collect(),
                        low,
                    },
                );

                fails
            }
            Symbol::Unresolved | Symbol::Module(_) | Symbol::Builtin(_) => false,
        }
    }

    /// Whether the call may fail, other than in evaluating its arguments.
    fn visit_call(&mut self, callee: HirId, args: &[HirId], in_body: bool) -> bool {
        let Callee::Function(function) = resolve_callee(self.db(), callee) else {
            return self.value_call_may_fail(callee, in_body);
        };

        (!in_body && self.cx.captures_may_fail(function))
            || self.call_may_fail(function, args, in_body)
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
            Callee::Function(function) => self.call_may_fail(function, &[], in_body),
            Callee::Parameter(parameter) => self.parameter_call_may_fail(parameter, in_body),
            // The function is only known at runtime, so it could do anything
            Callee::Unknown => true,
        }
    }

    /// Whether evaluating the arguments of the call may fail. The arguments of an `inline fn` are
    /// only evaluated where its parameters are referenced, so one whose parameter never is isn't.
    fn visit_arguments(&mut self, callee: HirId, args: &[HirId], in_body: bool) -> bool {
        let db = self.db();

        if let Hir::Reference(function) = db.hir(db.without_casts(callee))
            && let Symbol::Function(symbol) = db.symbol(*function)
            && symbol.kind == FunctionKind::Inline
        {
            let fixed = symbol.fixed_parameters();
            let rest = symbol.parameters.get(fixed.len()).copied();

            return args.iter().enumerate().any(|(index, &arg)| {
                fixed
                    .get(index)
                    .copied()
                    .or(rest)
                    .is_none_or(|parameter| self.cx.is_parameter_used(*function, parameter))
                    && self.visit(arg, in_body)
            });
        }

        self.any(args.iter().copied(), in_body)
    }

    /// Whether calling the value of the parameter may fail. Inside of a function body, the call
    /// is recorded, and checked where the function is called, since that's where the value of the
    /// parameter is known. Otherwise, the value isn't known, so it could do anything.
    fn parameter_call_may_fail(&mut self, parameter: SymbolId, in_body: bool) -> bool {
        // The argument of an inlined function is substituted for its parameter, so it's called
        if !in_body && let Some(arg) = self.cx.substituted_value(parameter) {
            return self.value_call_may_fail(arg, in_body);
        }

        let Some(calls) = self.calls.last_mut().filter(|_| in_body) else {
            return true;
        };

        calls.insert(parameter);
        false
    }

    /// Whether calling the function may fail, other than in calling the function values it's
    /// passed (see [`FunctionFailure`]). A call to a function whose summary is still being found
    /// uses the summary so far, which starts from not failing, since the body fails anyway if
    /// anything else in it does.
    fn function_may_fail(&mut self, function: SymbolId) -> FunctionFailure {
        if let Some(failure) = self.cx.function_failures().borrow().get(&function).cloned() {
            return failure;
        }

        if let Some(index) = self.working.get_index_of(&function) {
            self.low = self.low.min(index);
            return self.working[index].clone();
        }

        let Symbol::Function(symbol) = self.db().symbol(function) else {
            return FunctionFailure::fails();
        };

        // The body of an external function is compiled CLVM, which could do anything
        if symbol.kind == FunctionKind::External {
            self.cache(function, FunctionFailure::fails());
            return FunctionFailure::fails();
        }

        let index = self.working.len();
        self.working.insert(function, FunctionFailure::default());
        let outer_low = std::mem::replace(&mut self.low, usize::MAX);
        let outer_changed = std::mem::take(&mut self.changed);

        self.summarize(index);
        let mut low = self.low;
        let mut grew = false;

        // This is the first function reached in its component, so the component is finished
        // once a pass over it doesn't change any summary. A pass can reach new functions, which
        // join the component if they call back into it.
        if low >= index {
            while std::mem::take(&mut self.changed) {
                grew = true;
                self.forget_unfinished();

                let mut member = index;

                while member < self.working.len() {
                    self.low = usize::MAX;
                    self.summarize(member);
                    low = low.min(self.low);
                    member += 1;
                }
            }
        }

        let failure = self.working[index].clone();

        if low >= index {
            for (finished, failure) in self.working.drain(index..).collect::<Vec<_>>() {
                self.cache(finished, failure);
            }

            self.forget_unfinished();
            self.low = outer_low;
            self.changed = outer_changed;
        } else {
            // It calls a function reached earlier whose summary isn't finished, so it's found
            // along with that one. If a pass above found that late, the summaries it grew are
            // checked again with that one's component.
            self.low = outer_low.min(low);
            self.changed |= outer_changed || grew;
        }

        failure
    }

    /// Forgets the substituted values that read a summary that wasn't finished. One that read an
    /// earlier function's summary may also have read a later one that has grown since, and `low`
    /// only records the earliest, so all of them are forgotten.
    fn forget_unfinished(&mut self) {
        self.substituted.retain(|_, entry| entry.low == usize::MAX);
    }

    /// Checks the body of the function at the index in `working` again, with the summaries found
    /// so far, and grows its summary if needed.
    fn summarize(&mut self, index: usize) {
        let (&function, old) = self.working.get_index(index).expect("working function");
        let old = old.clone();

        let Symbol::Function(symbol) = self.db().symbol(function) else {
            return;
        };

        let (fails, calls) = self.visit_body(symbol.body);

        let failure = if fails || old.fails {
            FunctionFailure::fails()
        } else {
            let mut all: IndexSet<SymbolId> = old.calls.iter().copied().collect();
            all.extend(calls);
            FunctionFailure {
                fails: false,
                calls: all.into_iter().collect(),
            }
        };

        if failure.fails != old.fails || failure.calls.len() != old.calls.len() {
            self.working[index] = failure;
            self.changed = true;
        }
    }

    /// Checks a function body, and finds the parameters whose values it calls.
    fn visit_body(&mut self, body: HirId) -> (bool, IndexSet<SymbolId>) {
        self.calls.push(IndexSet::new());
        let fails = self.visit(body, true);
        let calls = self.calls.pop().unwrap_or_default();
        (fails, calls)
    }

    fn cache(&mut self, function: SymbolId, failure: FunctionFailure) {
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
