//! Decides when the right operand of `&&` or `||` is evaluated whether or not the left operand
//! decides the result.
//!
//! Branching on the left operand takes about 14 more bytes than evaluating both operands with
//! `all` or `any`, and each byte costs far more than running a small operand does. So the right
//! operand is evaluated either way when nothing can tell the difference, other than by its cost:
//!
//! - It doesn't rely on the left operand. The left operand of `x is Int && x > 5` narrows the
//!   type that the right one is checked with, so the right one fails on its own for an `x` that
//!   isn't an atom. The compiler records the expressions whose left operand narrows nothing but
//!   values that were already atoms (see
//!   [`Database::has_independent_operands`](crate::Database)). The checks that a type guard is
//!   lowered to are never recorded, since each relies on the ones before it.
//! - It can't fail (see [`may_fail`]), so evaluating it doesn't add a failure.
//! - It only reads what the left operand already read (see [`Lowerer::reads_are_evaluated`]).
//!   The failure analysis assumes that a value has the shape its type says, which is enough to
//!   evaluate a read later than it's written, or not at all: the program can only fail less. But
//!   evaluating it earlier makes the program fail where it didn't before if the value isn't
//!   there, and solutions do leave out what a puzzle doesn't read. A `Solution` struct that's
//!   only read in the right operand of `||` can be passed as nil whenever the left operand is
//!   true, as the Chialisp puzzle it was ported from allows.
//! - It's cheap to evaluate (see [`MAX_EAGER_OPERAND_SIZE`]).
//!
//! The optimizer then turns the operands into the arguments of an `all` or `any` wherever the
//! result is the same (see `opt_if` in `rue_lir`).

use rue_types::{Check, CheckPlan};

use crate::{
    BinaryOp, FieldAccess, FieldKind, Hir, HirId, Symbol, SymbolId, TypePath, UnaryOp,
    analysis::may_fail, check_value_uses,
};

use super::Lowerer;

/// How many nodes of HIR the right operand of `&&` or `||` can have and still be evaluated when
/// the left operand already decides the result. It can't contain a call, so each node is at most
/// one operator, which costs a few hundred to run on atoms the size of a hash. That keeps the
/// operand well below the cost of the bytes that branching on the left operand takes, which is
/// about 14 bytes at 12,000 each.
const MAX_EAGER_OPERAND_SIZE: usize = 24;

impl Lowerer<'_, '_, '_> {
    /// Whether the right operand of the `&&` or `||` expression is evaluated whether or not the
    /// left operand decides the result (see the [module docs](self)). This is only done when the
    /// LIR is optimized, so that debug builds evaluate what's written.
    pub(super) fn is_eager_operand(&self, hir: HirId, left: HirId, right: HirId) -> bool {
        let mut budget = MAX_EAGER_OPERAND_SIZE;

        self.options.optimize_lir
            && self.db.has_independent_operands(hir)
            // A left operand that's known decides the result while it's compiled, which only
            // leaves the right operand out if it isn't evaluated either way
            && !self.is_static_hir(left)
            && self.is_cheap(right, &mut budget)
            && !may_fail(self, right)
            && self.reads_are_evaluated(left, right)
    }

    /// Whether everything the right operand reads is there whenever the left operand was
    /// evaluated, without trusting the shape of anything that comes from outside of the program.
    ///
    /// A binding, or a parameter of a function that's only ever called directly, is always in
    /// the environment, since the program put it there. A parameter of `main` comes from the
    /// solution, and one of a function that's used as a value from whoever calls it, so it's
    /// only there for sure if the left operand read it, or a parameter after it, which can only
    /// be reached past the ones before. A field is only there for sure if the left operand read
    /// it, or a field inside of it.
    fn reads_are_evaluated(&self, left: HirId, right: HirId) -> bool {
        let mut evaluated = Vec::new();
        self.evaluated_reads(left, &mut evaluated);

        let mut reads = Vec::new();

        self.reads(right, &mut reads)
            && reads.iter().all(|(symbol, steps)| {
                (steps.is_empty() && !self.comes_from_outside(*symbol))
                    || evaluated.iter().any(|(read, read_steps)| {
                        (read == symbol && read_steps.starts_with(steps))
                            || (steps.is_empty() && self.is_parameter_before(*symbol, *read))
                    })
            })
    }

    /// Whether the symbol is a parameter whose argument isn't put in the environment by the
    /// program itself: one of `main`, or of a function that's used as a value.
    fn comes_from_outside(&self, symbol: SymbolId) -> bool {
        matches!(self.db.symbol(symbol), Symbol::Parameter(_))
            && self
                .graph
                .owner(symbol)
                .is_none_or(|function| function == self.main || self.graph.is_closure(function))
    }

    /// Whether both symbols are parameters of the same function, and the first one comes before
    /// the other. The parameters of `main` and of functions that are used as values are a list,
    /// so the later one can only be read if the earlier one is there.
    fn is_parameter_before(&self, symbol: SymbolId, other: SymbolId) -> bool {
        let Some(function) = self.graph.owner(symbol) else {
            return false;
        };

        if self.graph.owner(other) != Some(function) {
            return false;
        }

        let Symbol::Function(function) = self.db.symbol(function) else {
            return false;
        };

        let position = |symbol| function.parameters.iter().position(|&p| p == symbol);

        matches!((position(symbol), position(other)), (Some(a), Some(b)) if a < b)
    }

    /// Collects what the value reads from the environment whenever it's evaluated, as the symbol
    /// and the path to the field of it that's read. This leaves out anything that's only
    /// evaluated some of the time, and anything it can't follow, so it can miss reads.
    fn evaluated_reads(&self, hir: HirId, reads: &mut Vec<(SymbolId, Vec<TypePath>)>) {
        // This follows the values that are substituted, which can go as deep as a chain of them
        rue_types::grow_stack(|| self.evaluated_reads_inner(hir, reads));
    }

    fn evaluated_reads_inner(&self, hir: HirId, reads: &mut Vec<(SymbolId, Vec<TypePath>)>) {
        if let Some(read) = self.path_read(hir) {
            reads.push(read);
            return;
        }

        let node = self.db.hir(hir);

        match node {
            // Only the left operand and the condition are always evaluated
            Hir::Binary(BinaryOp::And | BinaryOp::Or, left, _) => {
                self.evaluated_reads(*left, reads);
            }
            Hir::If(condition, _, _, false) => self.evaluated_reads(*condition, reads),
            Hir::Reference(symbol) => {
                if let Some(value) = self.substituted_value(*symbol) {
                    self.evaluated_reads(value, reads);
                }
            }
            Hir::Const(expr) => {
                if let Some(result) = expr.result {
                    self.evaluated_reads(result, reads);
                }
            }
            // A check that doesn't use the value doesn't read it
            Hir::Guard(guard) => {
                if guard.value_uses() > 0 {
                    self.evaluated_reads(guard.value, reads);
                }
            }
            Hir::Cast(_)
            | Hir::Unary(..)
            | Hir::Binary(..)
            | Hir::ClvmOp(..)
            | Hir::FieldAccess(_)
            | Hir::List(_)
            | Hir::Struct(_)
            | Hir::Pair(..)
            | Hir::If(_, _, _, true)
            | Hir::CoinId(..)
            | Hir::Substr(..)
            | Hir::Modpow(..)
            | Hir::G1Map(..)
            | Hir::G2Map(..)
            | Hir::BlsPairingIdentity(_)
            | Hir::BlsVerify(..)
            | Hir::Secp256K1Verify(..)
            | Hir::Secp256R1Verify(..)
            | Hir::Keccak256Verify(..) => {
                for child in node.children() {
                    self.evaluated_reads(child, reads);
                }
            }
            // The arguments of a call that's inlined are only evaluated where they're used, and
            // a block can return early
            Hir::Unresolved
            | Hir::Nil
            | Hir::String(_)
            | Hir::Int(_)
            | Hir::Bytes(_)
            | Hir::Bool(_)
            | Hir::InfinityG1
            | Hir::InfinityG2
            | Hir::Block(_)
            | Hir::Lambda(_)
            | Hir::Program(_)
            | Hir::FunctionCall(_) => {}
        }
    }

    /// Collects everything the value can read from the environment, whether or not it's always
    /// evaluated, and returns whether all of it could be followed. The value is cheap (see
    /// [`Self::is_cheap`]), so there are no calls, lambdas or statements in it, but anything that
    /// can't be followed is refused here too.
    fn reads(&self, hir: HirId, reads: &mut Vec<(SymbolId, Vec<TypePath>)>) -> bool {
        // This follows the values that are substituted, which can go as deep as a chain of them
        rue_types::grow_stack(|| self.reads_inner(hir, reads))
    }

    fn reads_inner(&self, hir: HirId, reads: &mut Vec<(SymbolId, Vec<TypePath>)>) -> bool {
        if let Some(read) = self.path_read(hir) {
            reads.push(read);
            return true;
        }

        let node = self.db.hir(hir);

        match node {
            Hir::Nil
            | Hir::String(_)
            | Hir::Int(_)
            | Hir::Bytes(_)
            | Hir::Bool(_)
            | Hir::InfinityG1
            | Hir::InfinityG2
            | Hir::Program(_) => true,
            Hir::Reference(symbol) => self
                .substituted_value(*symbol)
                .is_some_and(|value| self.reads(value, reads)),
            Hir::Const(expr) => expr.result.is_some_and(|result| self.reads(result, reads)),
            // A check of the parts of a pair reads them without checking that the value is a
            // pair first, when its type already says so, so it reads more than the value. A
            // check that doesn't use the value doesn't read it.
            Hir::Guard(guard) => {
                let plan = guard.runtime_check();

                check_value_uses(&plan) == 0
                    || (!checks_fields(&plan) && self.reads(guard.value, reads))
            }
            // A field of a value that isn't read from the environment, such as of a struct
            // that's built in place, isn't followed
            Hir::FieldAccess(FieldAccess {
                kind: FieldKind::Path(_),
                ..
            })
            | Hir::Unary(UnaryOp::First | UnaryOp::Rest, _) => false,
            Hir::Block(block) if !block.statements.is_empty() => false,
            Hir::Block(_)
            | Hir::Cast(_)
            | Hir::Unary(..)
            | Hir::Binary(..)
            | Hir::ClvmOp(..)
            | Hir::FieldAccess(_)
            | Hir::List(_)
            | Hir::Struct(_)
            | Hir::Pair(..)
            | Hir::If(..) => node.children().all(|child| self.reads(child, reads)),
            Hir::Unresolved
            | Hir::Lambda(_)
            | Hir::FunctionCall(_)
            | Hir::CoinId(..)
            | Hir::Substr(..)
            | Hir::Modpow(..)
            | Hir::G1Map(..)
            | Hir::G2Map(..)
            | Hir::BlsPairingIdentity(_)
            | Hir::BlsVerify(..)
            | Hir::Secp256K1Verify(..)
            | Hir::Secp256R1Verify(..)
            | Hir::Keccak256Verify(..) => false,
        }
    }

    /// Whether the value has no more nodes than the budget, which is reduced by them, and none
    /// of them is a call or anything else that can run code the compiler can't bound. This
    /// follows the values that are substituted, which are evaluated where they're referenced.
    /// The budget bounds how far and how deep it goes.
    fn is_cheap(&self, hir: HirId, budget: &mut usize) -> bool {
        let Some(rest) = budget.checked_sub(1) else {
            return false;
        };

        *budget = rest;

        let node = self.db.hir(hir);

        match node {
            Hir::Nil
            | Hir::String(_)
            | Hir::Int(_)
            | Hir::Bytes(_)
            | Hir::Bool(_)
            | Hir::InfinityG1
            | Hir::InfinityG2 => true,
            // A program is as large as its function
            Hir::Program(_) => false,
            Hir::Const(expr) => expr
                .result
                .is_some_and(|result| self.is_cheap(result, budget)),
            Hir::Reference(symbol) => self
                .substituted_value(*symbol)
                .is_none_or(|value| self.is_cheap(value, budget)),
            // The check uses the value as many times as it has parts to check, and it's lowered
            // to operators on it
            // A check with helpers is a program of its own, which isn't cheap to repeat
            Hir::Guard(guard) if !guard.runtime_check().helpers.is_empty() => false,
            Hir::Guard(guard) => {
                let uses = guard.value_uses().max(1);

                (0..uses).all(|_| self.is_cheap(guard.value, budget))
                    && budget
                        .checked_sub(uses)
                        .map(|rest| *budget = rest)
                        .is_some()
            }
            // A block that's only a value
            Hir::Block(block) if block.statements.is_empty() => {
                block.body.is_some_and(|body| self.is_cheap(body, budget))
            }
            // This costs as much to run as a hundred bytes do
            Hir::Unary(UnaryOp::PubkeyForExp, _) => false,
            Hir::Cast(_)
            | Hir::Unary(..)
            | Hir::Binary(..)
            | Hir::ClvmOp(..)
            | Hir::FieldAccess(_)
            | Hir::List(_)
            | Hir::Struct(_)
            | Hir::Pair(..)
            | Hir::If(..) => node.children().all(|child| self.is_cheap(child, budget)),
            // A call can run for as long as it likes, a lambda builds a closure, and the rest
            // may fail, so they're never evaluated early anyway
            Hir::Unresolved
            | Hir::Block(_)
            | Hir::Lambda(_)
            | Hir::FunctionCall(_)
            | Hir::CoinId(..)
            | Hir::Substr(..)
            | Hir::Modpow(..)
            | Hir::G1Map(..)
            | Hir::G2Map(..)
            | Hir::BlsPairingIdentity(_)
            | Hir::BlsVerify(..)
            | Hir::Secp256K1Verify(..)
            | Hir::Secp256R1Verify(..)
            | Hir::Keccak256Verify(..) => false,
        }
    }
}

/// Whether the check looks at the parts of a pair. Parts are only shared between the checks of
/// pairs, so a check with helpers always does.
fn checks_fields(plan: &CheckPlan) -> bool {
    check_tree_fields(&plan.check)
}

fn check_tree_fields(check: &Check) -> bool {
    match check {
        Check::Pair(..) | Check::Helper(_) => true,
        Check::And(checks) | Check::Or(checks) => checks.iter().any(check_tree_fields),
        Check::None
        | Check::Impossible
        | Check::IsAtom { .. }
        | Check::IsPair { .. }
        | Check::Atom(_) => false,
        Check::Shared(_) => unreachable!("a planned check has no shared parts"),
    }
}
