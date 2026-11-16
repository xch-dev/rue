//! Finds whether evaluating a value may fail, so that inlining never drops it or moves it
//! somewhere that's only evaluated some of the time (see [`super::evaluation`]).

use std::collections::{HashMap, HashSet};

use indexmap::IndexMap;

use num_bigint::BigInt;
use rue_lir::{ClvmOp, atom_bigint};

use crate::{BinaryOp, Database, FunctionKind, Hir, HirId, Statement, Symbol, SymbolId, UnaryOp};

use super::Lowerer;

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
/// A substituted value (see [`Lowerer::substituted_value`]) is checked where it's referenced,
/// since that's where it's evaluated.
pub(super) fn may_fail(lowerer: &Lowerer<'_, '_, '_>, hir: HirId) -> bool {
    Visitor::new(lowerer).visit(hir, false)
}

/// The function that's called, if it's known. This follows bindings, so that calling a lambda
/// that's bound to a name is treated like calling the lambda itself.
pub(super) fn resolve_function(db: &Database, mut callee: HirId) -> Option<SymbolId> {
    let mut seen = HashSet::new();

    loop {
        match db.hir(db.without_casts(callee)) {
            Hir::Lambda(lambda) => return Some(*lambda),
            Hir::Reference(symbol) => match db.symbol(*symbol) {
                Symbol::Function(_) => return Some(*symbol),
                Symbol::Binding(binding) if seen.insert(*symbol) => {
                    callee = binding.value.hir;
                }
                Symbol::Binding(_)
                | Symbol::Constant(_)
                | Symbol::Parameter(_)
                | Symbol::Unresolved
                | Symbol::Module(_)
                | Symbol::Builtin(_) => return None,
            },
            _ => return None,
        }
    }
}

impl Lowerer<'_, '_, '_> {
    /// The value that's evaluated where the symbol is referenced, if it's substituted there rather
    /// than read from the environment. This is the value of an inline binding or constant, or of
    /// one that's inlined automatically, or the argument for a parameter of an inlined function.
    fn substituted_value(&self, symbol: SymbolId) -> Option<HirId> {
        if let Some(hir) = self.inline_argument(symbol) {
            return Some(hir);
        }

        let (value, inline) = match self.db.symbol(symbol) {
            Symbol::Binding(binding) => (binding.value.hir, binding.inline),
            Symbol::Constant(constant) => (constant.value.hir, constant.inline),
            Symbol::Function(_)
            | Symbol::Parameter(_)
            | Symbol::Unresolved
            | Symbol::Module(_)
            | Symbol::Builtin(_) => return None,
        };

        // A binding or constant whose single reference hasn't been decided yet might be
        // substituted, so its value is checked where it's referenced
        let substituted = inline
            || (self.options.auto_inline
                && self.graph.references(symbol) <= 1
                && self
                    .substitutions
                    .borrow()
                    .get(&symbol)
                    .copied()
                    .unwrap_or(true));

        substituted.then_some(value)
    }

    /// Whether creating a closure for the function, or calling it directly, evaluates an argument
    /// of an inlined function that may fail, since the arguments it captures are evaluated then.
    fn captures_may_fail(&self, function: SymbolId) -> bool {
        if self.inline_symbols.iter().all(HashMap::is_empty) {
            return false;
        }

        self.dependencies(function, true).iter().any(|&capture| {
            self.inline_argument(capture)
                .is_some_and(|hir| !self.is_static_hir(hir) && may_fail(self, hir))
        })
    }
}

struct Visitor<'a, 'd, 'b, 'g> {
    lowerer: &'a Lowerer<'d, 'b, 'g>,
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
}

impl<'a, 'd, 'b, 'g> Visitor<'a, 'd, 'b, 'g> {
    fn new(lowerer: &'a Lowerer<'d, 'b, 'g>) -> Self {
        Self {
            lowerer,
            visiting: HashSet::new(),
            assumed: IndexMap::new(),
            lowest_assumption: usize::MAX,
        }
    }

    fn db(&self) -> &'a Database {
        self.lowerer.db
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
            Hir::Lambda(lambda) => !in_body && self.lowerer.captures_may_fail(*lambda),
            Hir::Reference(symbol) => self.visit_reference(*symbol, in_body),
            Hir::Block(block) => {
                self.visit_statements(&block.statements, in_body)
                    || self.any(block.reachable_body(), in_body)
            }
            // The value is only evaluated if the check it's lowered to uses it
            Hir::Guard(guard) => guard.value_uses() > 0 && self.visit(guard.value, in_body),
            Hir::FunctionCall(call) => {
                self.visit_callee(call.function, in_body)
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
                // if they can't fail, unless they're never referenced
                Statement::Let(symbol, _) => match db.symbol(symbol) {
                    Symbol::Binding(binding)
                        if !binding.inline && self.lowerer.graph.references(symbol) > 0 =>
                    {
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
            Symbol::Function(_) => !in_body && self.lowerer.captures_may_fail(symbol),
            Symbol::Parameter(_) if in_body => false,
            Symbol::Parameter(_) | Symbol::Binding(_) | Symbol::Constant(_) => {
                let Some(value) = self.lowerer.substituted_value(symbol) else {
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

    /// Whether calling the function may fail, other than in evaluating its arguments.
    fn visit_callee(&mut self, callee: HirId, in_body: bool) -> bool {
        let Some(function) = resolve_function(self.db(), callee) else {
            // The function is only known at runtime, so it could do anything
            return true;
        };

        (!in_body && self.lowerer.captures_may_fail(function)) || self.function_may_fail(function)
    }

    /// Whether the body of the function may fail. A recursive call is assumed not to fail, since
    /// the body fails anyway if anything else in it does. Functions that call each other are
    /// resolved together, like the strongly connected components of the call graph, so that each
    /// function's body is only checked once.
    fn function_may_fail(&mut self, function: SymbolId) -> bool {
        if let Some(&fails) = self.lowerer.function_failures.borrow().get(&function) {
            return fails;
        }

        if let Some(&assumption) = self.assumed.get(&function) {
            self.lowest_assumption = self.lowest_assumption.min(assumption);
            return false;
        }

        let Symbol::Function(symbol) = self.db().symbol(function) else {
            return true;
        };

        // The body of an external function is compiled CLVM, which could do anything
        if symbol.kind == FunctionKind::External {
            self.cache(function, true);
            return true;
        }

        let body = symbol.body;
        let index = self.assumed.len();
        self.assumed.insert(function, index);
        let lowest_assumption = std::mem::replace(&mut self.lowest_assumption, usize::MAX);

        let fails = self.visit(body, true);

        if fails {
            // Failing doesn't depend on any assumption. The functions that were assumed not to
            // fail since this one was reached may have been wrong, so they're checked again
            // if they're reached again.
            self.assumed.truncate(index);
            self.cache(function, true);
            self.lowest_assumption = lowest_assumption;
        } else if self.lowest_assumption >= index {
            // Every assumption made while checking the body was about this function or the ones
            // reached after it, and none of them fail
            let mut failures = self.lowerer.function_failures.borrow_mut();

            for (function, _) in self.assumed.drain(index..) {
                failures.insert(function, false);
            }

            drop(failures);

            self.lowest_assumption = lowest_assumption;
        } else {
            // This depends on a function that was reached earlier, whose result isn't known yet,
            // so it stays assumed not to fail until then
            self.assumed.insert(function, self.lowest_assumption);
            self.lowest_assumption = self.lowest_assumption.min(lowest_assumption);
        }

        fails
    }

    fn cache(&self, function: SymbolId, fails: bool) {
        self.lowerer
            .function_failures
            .borrow_mut()
            .insert(function, fails);
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
