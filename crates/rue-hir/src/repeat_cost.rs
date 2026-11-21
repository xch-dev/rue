//! Decides whether a value is cheap enough to evaluate again each time something reads it, rather
//! than binding it and reading it from the environment.
//!
//! A value that's only referenced once is normally evaluated where it's referenced. If that's in
//! a lambda that's called for each item of a list, the value is evaluated for each item too (see
//! [`DependencyGraph::is_repeated`]). That's what makes a loop quadratic when the value goes
//! through the list itself, as in `let k = total(xs); map(xs, fn(x) => x + k)`. But binding a
//! value isn't free: it's put in the environment and captured by the lambda, which takes a dozen
//! bytes or more, and each byte of a program costs as much as running a dozen operators. So a
//! value is only bound if evaluating it can take long: it can go through a list of any length,
//! or it uses an operator that costs about as much as the bytes would.
//!
//! This only looks at how the value is written. A value that's larger than this allows can still
//! be compiled to a constant, which the lowerer finds out (see `Lowerer::decide_repeats`).

use std::collections::HashSet;

use crate::{
    BinaryOp, Database, DependencyGraph, FieldKind, FunctionKind, Hir, HirId, Statement, Symbol,
    SymbolId, UnaryOp,
};

/// How many operators and calls a value can take to evaluate and still be evaluated again for
/// each read, counting the bodies of the functions it calls. Each one costs up to about a thousand
/// when it's run, which is less than one byte of the program does, and binding the value takes at
/// least a dozen bytes. So a value of this size has to be read a few dozen times before binding it
/// is cheaper, and it never costs more than a few times what calling the lambda that reads it
/// does.
const MAX_REPEATED_VALUE_SIZE: usize = 8;

/// Whether evaluating the value takes no more than a few cheap operators (see
/// [`MAX_REPEATED_VALUE_SIZE`]), whatever it's given. It doesn't if it calls a function that
/// calls itself, runs a loop, calls a function value that isn't known, checks a type, or uses an
/// operator that's expensive on its own, such as a signature check.
///
/// The values that are substituted into it count too: an `inline` binding or constant, and one
/// that's only referenced once, which is evaluated wherever this value is. A value that's
/// referenced more than once is bound, so reading it is free, like reading a parameter.
pub(crate) fn is_cheap_to_repeat(db: &Database, graph: &DependencyGraph, value: HirId) -> bool {
    let mut cost = RepeatCost {
        db,
        graph,
        budget: MAX_REPEATED_VALUE_SIZE,
        locals: HashSet::new(),
        following: HashSet::new(),
    };

    cost.fits(value)
}

struct RepeatCost<'a> {
    db: &'a Database,
    graph: &'a DependencyGraph,
    /// How many more operators and calls the value can have.
    budget: usize,
    /// The bindings of the let statements inside the value, whose values are counted there.
    locals: HashSet<SymbolId>,
    /// The functions, bindings and constants that are being counted, so that a function that
    /// calls itself isn't followed forever.
    following: HashSet<SymbolId>,
}

impl RepeatCost<'_> {
    fn take(&mut self) -> bool {
        match self.budget.checked_sub(1) {
            Some(rest) => {
                self.budget = rest;
                true
            }
            None => false,
        }
    }

    fn fits(&mut self, hir: HirId) -> bool {
        // A value can be nested as deeply as the parser allows inside each function it calls
        rue_types::grow_stack(|| self.fits_inner(hir))
    }

    fn all(&mut self, hirs: impl IntoIterator<Item = HirId>) -> bool {
        hirs.into_iter().all(|hir| self.fits(hir))
    }

    fn fits_inner(&mut self, hir: HirId) -> bool {
        match self.db.hir(hir) {
            Hir::Unresolved => false,
            Hir::Nil
            | Hir::String(_)
            | Hir::Int(_)
            | Hir::Bytes(_)
            | Hir::Bool(_)
            | Hir::InfinityG1
            | Hir::InfinityG2 => true,
            Hir::Pair(first, rest) => self.all([*first, *rest]),
            Hir::List(list) => self.all(list.items.iter().copied()),
            Hir::Struct(initializer) => {
                self.all(initializer.fields.iter().map(|field| field.value))
            }
            Hir::FieldAccess(access) => {
                let counted = match access.kind {
                    FieldKind::Path(_) => true,
                    FieldKind::Length => self.take(),
                };

                counted && self.fits(access.value)
            }
            Hir::Cast(value) | Hir::Unary(UnaryOp::Plus, value) => self.fits(*value),
            // A check can go through a whole list
            Hir::Guard(_) => false,
            Hir::Const(expr) => expr.result.is_some_and(|result| self.fits(result)),
            Hir::Reference(symbol) => self.reference(*symbol),
            Hir::Block(block) => {
                block
                    .reachable_statements()
                    .iter()
                    .all(|statement| self.statement(statement))
                    && block.reachable_body().is_none_or(|body| self.fits(body))
            }
            // Building a closure doesn't evaluate its body
            Hir::Lambda(_) => self.take(),
            Hir::If(condition, then, else_, _) => {
                self.take() && self.all([*condition, *then, *else_])
            }
            Hir::FunctionCall(call) => {
                self.take()
                    && self.all(call.args.iter().copied())
                    && self
                        .callee(call.function)
                        .is_some_and(|function| self.body(function))
            }
            Hir::Unary(op, value) => {
                let cheap = match op {
                    UnaryOp::Listp
                    | UnaryOp::First
                    | UnaryOp::Rest
                    | UnaryOp::Strlen
                    | UnaryOp::Not
                    | UnaryOp::Plus
                    | UnaryOp::Neg
                    | UnaryOp::BitwiseNot
                    | UnaryOp::Sha256
                    | UnaryOp::Sha256Inline
                    | UnaryOp::Keccak256
                    | UnaryOp::Keccak256Inline => true,
                    UnaryOp::G1Negate | UnaryOp::G2Negate | UnaryOp::PubkeyForExp => false,
                };

                cheap && self.take() && self.fits(*value)
            }
            Hir::Binary(op, left, right) => {
                let cheap = match op {
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod
                    | BinaryOp::Divmod
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
                    | BinaryOp::BitwiseXor
                    | BinaryOp::LeftArithmeticShift
                    | BinaryOp::RightArithmeticShift
                    | BinaryOp::RightLogicalShift => true,
                    BinaryOp::G1Add
                    | BinaryOp::G1Subtract
                    | BinaryOp::G1Multiply
                    | BinaryOp::G2Add
                    | BinaryOp::G2Subtract
                    | BinaryOp::G2Multiply => false,
                };

                cheap && self.take() && self.all([*left, *right])
            }
            Hir::CoinId(parent, puzzle, amount) => {
                self.take() && self.all([*parent, *puzzle, *amount])
            }
            Hir::Substr(value, start, end) => {
                self.take() && self.all([Some(*value), Some(*start), *end].into_iter().flatten())
            }
            // These cost more to run once than the bytes that binding a value takes
            Hir::G1Map(..)
            | Hir::G2Map(..)
            | Hir::Modpow(..)
            | Hir::BlsPairingIdentity(_)
            | Hir::BlsVerify(..)
            | Hir::Secp256K1Verify(..)
            | Hir::Secp256R1Verify(..)
            | Hir::Keccak256Verify(..)
            | Hir::ClvmOp(..) => false,
        }
    }

    fn statement(&mut self, statement: &Statement) -> bool {
        if let Statement::Let(symbol, _) = statement {
            let Symbol::Binding(binding) = self.db.symbol(*symbol) else {
                return false;
            };

            // An `inline` binding is evaluated where it's referenced, and counted there
            if binding.inline {
                return true;
            }

            self.locals.insert(*symbol);
            return self.fits(binding.value.hir);
        }

        self.all(statement.lowered_hirs(false))
    }

    /// A reference to a value that's bound is free. One that's substituted is its value.
    fn reference(&mut self, symbol: SymbolId) -> bool {
        if self.locals.contains(&symbol) {
            return true;
        }

        let (value, inline) = match self.db.symbol(symbol) {
            Symbol::Binding(binding) => (binding.value.hir, binding.inline),
            Symbol::Constant(constant) => (constant.value.hir, constant.inline),
            // A function that's used as a value is a closure, which is built from its captures
            Symbol::Parameter(_) | Symbol::Function(_) | Symbol::Builtin(_) | Symbol::Module(_) => {
                return true;
            }
            Symbol::Unresolved => return false,
        };

        if !inline && self.graph.references(symbol) > 1 {
            return true;
        }

        self.follow(symbol, value)
    }

    /// The function that's called, if it's known: one that's called by name, or a lambda,
    /// through any bindings that it's the value of.
    fn callee(&self, mut hir: HirId) -> Option<SymbolId> {
        let mut seen = HashSet::new();

        loop {
            match self.db.hir(self.db.without_casts(hir)) {
                Hir::Lambda(lambda) => return Some(*lambda),
                Hir::Reference(symbol) if seen.insert(*symbol) => match self.db.symbol(*symbol) {
                    Symbol::Function(_) => return Some(*symbol),
                    Symbol::Binding(binding) => hir = binding.value.hir,
                    Symbol::Constant(constant) => hir = constant.value.hir,
                    Symbol::Parameter(_)
                    | Symbol::Builtin(_)
                    | Symbol::Module(_)
                    | Symbol::Unresolved => return None,
                },
                _ => return None,
            }
        }
    }

    /// Counts the body of a function that's called. Its parameters are bound to the arguments,
    /// which are counted at the call. A loop goes through a list, and the body of an `extern fn`
    /// isn't known.
    fn body(&mut self, function: SymbolId) -> bool {
        let Symbol::Function(symbol) = self.db.symbol(function) else {
            return false;
        };

        match symbol.kind {
            FunctionKind::Loop | FunctionKind::External => false,
            FunctionKind::BinaryTree | FunctionKind::Sequential | FunctionKind::Inline => {
                self.follow(function, symbol.body)
            }
        }
    }

    /// Counts the value of a binding or constant, or the body of a function, unless it's
    /// already being counted, which is the case for a function that calls itself.
    fn follow(&mut self, symbol: SymbolId, hir: HirId) -> bool {
        if !self.following.insert(symbol) {
            return false;
        }

        let fits = self.fits(hir);
        self.following.remove(&symbol);
        fits
    }
}
