//! Decides when a value that may fail can be inlined, so that inlining never drops a check, or
//! moves it somewhere that's only evaluated some of the time.
//!
//! A binding, constant or argument that's referenced once is normally evaluated where it's
//! referenced. That's only done for a value that may fail (see [`may_fail`]) if the reference is
//! always evaluated whenever the value would have been, which is at the let statement for a
//! binding, at the start of the program for a constant, and at the call for an argument.
//! Otherwise, it's bound like it would be if it were referenced more than once, or evaluated for
//! its effect if it's never referenced.

use std::collections::{HashMap, HashSet};

use crate::{BinaryOp, Database, FunctionKind, Hir, HirId, Statement, Symbol, SymbolId};

use super::{
    Lowerer, fixed_parameters,
    may_fail::{may_fail, resolve_function},
};

impl Lowerer<'_, '_, '_> {
    /// Whether the binding or constant, which is referenced at most once and isn't `inline`, can
    /// be evaluated where it's referenced rather than bound. This is cached, since it's needed
    /// each time the symbol is grouped, captured or referenced.
    pub(super) fn can_substitute(&self, symbol: SymbolId) -> bool {
        if self.graph.references(symbol) == 0 {
            return true;
        }

        if let Some(&substitute) = self.substitutions.borrow().get(&symbol) {
            return substitute;
        }

        let substitute = self.decide_substitution(symbol);
        self.substitutions.borrow_mut().insert(symbol, substitute);
        substitute
    }

    /// Forgets whether the binding can be substituted, since its let statement is being lowered
    /// again, which might be in a call to an `inline fn` with different arguments than before.
    /// It's decided again when the statement's bindings are grouped.
    pub(super) fn forget_substitution(&self, symbol: SymbolId) {
        self.substitutions.borrow_mut().remove(&symbol);
    }

    fn decide_substitution(&self, symbol: SymbolId) -> bool {
        match self.db.symbol(symbol) {
            Symbol::Binding(binding) => {
                !may_fail(self, binding.value.hir) || self.binding_always_evaluated(symbol)
            }
            Symbol::Constant(constant) => {
                !may_fail(self, constant.value.hir) || self.constant_always_evaluated(symbol)
            }
            Symbol::Function(_)
            | Symbol::Parameter(_)
            | Symbol::Unresolved
            | Symbol::Module(_)
            | Symbol::Builtin(_) => true,
        }
    }

    /// Whether the rest of the block after the binding's let statement always evaluates its
    /// single reference, or fails.
    fn binding_always_evaluated(&self, symbol: SymbolId) -> bool {
        let Some((block, index)) = self.graph.let_statement(symbol) else {
            return false;
        };

        let Hir::Block(block) = self.db.hir(block) else {
            return false;
        };

        // The walk stops at a `return` or `raise`, so it doesn't reach what isn't lowered
        Evaluation::new(self, symbol).block(&block.statements[index + 1..], block.body)
    }

    /// Whether the constant's single reference is always evaluated when the program is run, or
    /// fails. Constants that are referenced more than once are evaluated at the start of the
    /// program, so a reference in one of their values counts too.
    fn constant_always_evaluated(&self, symbol: SymbolId) -> bool {
        let roots = self
            .constant_roots
            .get_or_init(|| self.find_constant_roots());
        let mut evaluation = Evaluation::new(self, symbol);
        roots.iter().any(|&root| evaluation.hir(root))
    }

    /// What's always evaluated when the program is run: the body of `main`, and the values of the
    /// constants that are referenced more than once, since they're bound before it.
    fn find_constant_roots(&self) -> Vec<HirId> {
        let Symbol::Function(main) = self.db.symbol(self.main) else {
            return Vec::new();
        };

        if main.kind == FunctionKind::External {
            return Vec::new();
        }

        let mut roots = vec![main.body];

        for &dependency in self.dependencies(self.main, false).iter() {
            if let Symbol::Constant(constant) = self.db.symbol(dependency)
                && !constant.inline
                && self.graph.references(dependency) > 1
            {
                roots.push(constant.value.hir);
            }
        }

        roots
    }

    /// Whether the body of a function that's being inlined always evaluates the parameter's
    /// single reference, or fails, so the argument can be substituted for it.
    pub(super) fn parameter_always_evaluated(&self, body: HirId, parameter: SymbolId) -> bool {
        self.graph.references(parameter) == 1 && Evaluation::new(self, parameter).hir(body)
    }
}

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
struct Evaluation<'a, 'd, 'b, 'g> {
    lowerer: &'a Lowerer<'d, 'b, 'g>,
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

impl<'a, 'd, 'b, 'g> Evaluation<'a, 'd, 'b, 'g> {
    fn new(lowerer: &'a Lowerer<'d, 'b, 'g>, target: SymbolId) -> Self {
        Self {
            lowerer,
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
        self.lowerer.db
    }

    fn any(&mut self, hirs: impl IntoIterator<Item = HirId>) -> bool {
        hirs.into_iter().any(|hir| self.hir(hir))
    }

    fn hir(&mut self, hir: HirId) -> bool {
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

    fn block(&mut self, statements: &[Statement], body: Option<HirId>) -> bool {
        for (index, statement) in statements.iter().enumerate() {
            match *statement {
                Statement::Let(symbol, _) => {
                    let Symbol::Binding(binding) = self.db().symbol(symbol) else {
                        continue;
                    };

                    // An inline binding is evaluated where it's referenced instead, and an
                    // unreferenced binding isn't evaluated at all
                    let evaluated = !binding.inline && self.lowerer.graph.references(symbol) > 0;

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
            && self
                .lowerer
                .dependencies(function, true)
                .contains(&self.target)
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
            let arguments = fixed_parameters(symbol)
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
