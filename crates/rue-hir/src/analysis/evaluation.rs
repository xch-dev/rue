//! Finds whether evaluating some HIR always evaluates a reference to a symbol.

use std::collections::{HashMap, HashSet};

use crate::{BinaryOp, Database, FunctionKind, Hir, HirId, Statement, Symbol, SymbolId};

use super::{AnalysisContext, resolve_function};

/// Finds whether evaluating some HIR always evaluates a reference to the target symbol, unless it
/// fails first.
///
/// It only follows what's evaluated unconditionally: a branch only counts if both branches
/// evaluate the target, and the body of a lambda doesn't count, since it might not be called.
///
/// A binding, constant or argument whose value may fail is always evaluated, either where it's
/// defined or at its reference. This assumes that the target may fail, so any value that
/// references it may fail as well, which means that the values of other bindings and the
/// arguments of calls are evaluated if they're reached.
pub(crate) struct Evaluation<'a, C> {
    cx: &'a C,
    target: SymbolId,
    /// The arguments of the calls to `inline fn`s being followed, which are evaluated where the
    /// parameters are referenced.
    arguments: Vec<HashMap<SymbolId, HirId>>,
    /// The symbols whose values or bodies have been followed, so that each is only followed once.
    visited: HashSet<SymbolId>,
    /// The symbols in `visited`, in the order they were followed, so that the ones followed
    /// inside a call to an `inline fn` can be followed again for the next call.
    followed: Vec<SymbolId>,
}

impl<'a, C: AnalysisContext> Evaluation<'a, C> {
    pub(crate) fn new(cx: &'a C, target: SymbolId) -> Self {
        Self {
            cx,
            target,
            arguments: Vec::new(),
            visited: HashSet::new(),
            followed: Vec::new(),
        }
    }

    /// Whether the symbol's value or body hasn't been followed yet, in which case it's followed.
    fn follow(&mut self, symbol: SymbolId) -> bool {
        let first = self.visited.insert(symbol);

        if first {
            self.followed.push(symbol);
        }

        first
    }

    fn db(&self) -> &'a Database {
        self.cx.db()
    }

    fn any(&mut self, hirs: impl IntoIterator<Item = HirId>) -> bool {
        hirs.into_iter().any(|hir| self.hir(hir))
    }

    pub(crate) fn hir(&mut self, hir: HirId) -> bool {
        // This follows chains of values and calls, which can be as deep as the program
        rue_types::grow_stack(|| self.hir_inner(hir))
    }

    fn hir_inner(&mut self, hir: HirId) -> bool {
        let node = self.db().hir(hir);

        match node {
            Hir::Unresolved
            | Hir::Nil
            | Hir::String(_)
            | Hir::Int(_)
            | Hir::Bytes(_)
            | Hir::Bool(_)
            | Hir::InfinityG1
            | Hir::InfinityG2
            | Hir::Const(_) => false,
            Hir::Reference(symbol) => self.reference(*symbol),
            Hir::Lambda(lambda) => self.captures_target(*lambda),
            Hir::Block(block) => self.block(&block.statements, block.body),
            Hir::If(condition, then, else_, inline) => {
                if *inline {
                    self.any(node.children())
                } else {
                    self.hir(*condition) || (self.hir(*then) && self.hir(*else_))
                }
            }
            // The right side of `&&` and `||` is only evaluated some of the time
            Hir::Binary(BinaryOp::And | BinaryOp::Or, left, _) => self.hir(*left),
            Hir::FunctionCall(call) => self.call(call.function, &call.args),
            Hir::Guard(guard) => guard.value_uses() > 0 && self.hir(guard.value),
            Hir::Cast(_)
            | Hir::Unary(..)
            | Hir::ClvmOp(..)
            | Hir::FieldAccess(_)
            | Hir::List(_)
            | Hir::Struct(_)
            | Hir::Pair(..)
            | Hir::Binary(..)
            | Hir::CoinId(..)
            | Hir::Modpow(..)
            | Hir::Secp256K1Verify(..)
            | Hir::Secp256R1Verify(..)
            | Hir::Substr(..)
            | Hir::G1Map(..)
            | Hir::G2Map(..)
            | Hir::BlsPairingIdentity(_)
            | Hir::BlsVerify(..) => self.any(node.children()),
        }
    }

    pub(crate) fn block(&mut self, statements: &[Statement], body: Option<HirId>) -> bool {
        for (index, statement) in statements.iter().enumerate() {
            match *statement {
                Statement::Let(symbol, _) => {
                    let Symbol::Binding(binding) = self.db().symbol(symbol) else {
                        continue;
                    };

                    // An inline binding is evaluated where it's referenced instead
                    let evaluated = !binding.inline
                        && self
                            .cx
                            .let_evaluates(symbol, &statements[index + 1..], body);

                    if evaluated && self.hir(binding.value.hir) {
                        return true;
                    }
                }
                Statement::Expr(ref statement) => {
                    if self.hir(statement.hir) {
                        return true;
                    }
                }
                Statement::Assert(condition, _) => {
                    if self.hir(condition) {
                        return true;
                    }
                }
                // The program fails, so it doesn't matter what's evaluated after
                Statement::Raise(..) => return true,
                Statement::Return(hir, _) => return self.hir(hir),
                // The rest of the block is only evaluated if the condition is false, and the
                // early return only if it's true
                Statement::If(ref statement) => {
                    if self.hir(statement.condition) {
                        return true;
                    }

                    return self.hir(statement.then) && self.block(&statements[index + 1..], body);
                }
                // Debug statements are only lowered with debug symbols
                Statement::Debug(..) => {}
            }
        }

        body.is_some_and(|body| self.hir(body))
    }

    fn reference(&mut self, symbol: SymbolId) -> bool {
        if symbol == self.target {
            return true;
        }

        if let Some(hir) = self
            .arguments
            .iter()
            .rev()
            .find_map(|arguments| arguments.get(&symbol).copied())
        {
            return self.hir(hir);
        }

        match self.db().symbol(symbol) {
            // A binding or constant that references the target may fail, so its value is
            // evaluated either where it's defined (which dominates this reference) or here
            Symbol::Binding(binding) => self.follow(symbol) && self.hir(binding.value.hir),
            Symbol::Constant(constant) => self.follow(symbol) && self.hir(constant.value.hir),
            Symbol::Function(_) => self.captures_target(symbol),
            Symbol::Parameter(_) | Symbol::Unresolved | Symbol::Module(_) | Symbol::Builtin(_) => {
                false
            }
        }
    }

    /// Whether creating a closure for the function evaluates the target. That's the case for an
    /// argument of an inlined function that the closure captures, since it's captured by value.
    fn captures_target(&self, function: SymbolId) -> bool {
        matches!(self.db().symbol(self.target), Symbol::Parameter(_))
            && self.cx.captures(function, self.target)
    }

    fn call(&mut self, callee: HirId, args: &[HirId]) -> bool {
        let db = self.db();

        // The arguments of an `inline fn` are evaluated where its parameters are referenced. A
        // rest parameter isn't followed, which only means that an argument passed to it isn't
        // counted as evaluated.
        if let Hir::Reference(function) = db.hir(db.without_casts(callee))
            && let Symbol::Function(symbol) = db.symbol(*function)
            && symbol.kind == FunctionKind::Inline
        {
            let function = *function;
            let arguments = symbol
                .fixed_parameters()
                .iter()
                .copied()
                .zip(args.iter().copied())
                .collect();

            // What's followed inside the body depends on the arguments, so it's followed again
            // for each call
            let followed = self.followed.len();
            self.arguments.push(arguments);
            let evaluated = self.follow(function) && self.hir(symbol.body);
            self.arguments.pop();

            for symbol in self.followed.drain(followed..) {
                self.visited.remove(&symbol);
            }

            return evaluated || self.captures_target(function);
        }

        // Otherwise, an argument that references the target may fail, so it's evaluated at the
        // call, and the body is evaluated whenever the function is called, if it's known
        self.hir(callee)
            || self.any(args.iter().copied())
            || resolve_function(db, callee).is_some_and(|function| {
                let Symbol::Function(symbol) = db.symbol(function) else {
                    return false;
                };

                self.follow(function) && self.hir(symbol.body)
            })
    }
}
