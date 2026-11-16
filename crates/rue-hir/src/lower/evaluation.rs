//! Decides when a value that may fail can be inlined, so that inlining never drops a check, or
//! moves it somewhere that's only evaluated some of the time.
//!
//! A binding, constant or argument that's referenced once is normally evaluated where it's
//! referenced. That's only done for a value that may fail (see [`may_fail`]) if the reference is
//! always evaluated whenever the value would have been, which is at the let statement for a
//! binding, at the start of the program for a constant, and at the call for an argument.
//! Otherwise, it's bound like it would be if it were referenced more than once, or evaluated for
//! its effect if it's never referenced.

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

use crate::{
    BinaryOp, Database, FailureContext, FunctionKind, Hir, HirId, Statement, Symbol, SymbolId,
    may_fail, reachable_body, reachable_statements,
};

use super::Lowerer;

impl FailureContext for Lowerer<'_, '_, '_> {
    fn db(&self) -> &Database {
        self.db
    }

    fn references(&self, symbol: SymbolId) -> usize {
        self.graph.references(symbol)
    }

    fn substituted_value(&self, symbol: SymbolId) -> Option<HirId> {
        if let Some(hir) = self.inline_argument(symbol) {
            return Some(hir);
        }

        let (value, inline) = match self.db.symbol(symbol) {
            Symbol::Binding(binding) => (binding.value.hir, binding.inline),
            Symbol::Constant(constant) => (constant.value.hir, constant.inline),
            _ => return None,
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

    fn captures_may_fail(&self, function: SymbolId) -> bool {
        if self.inline_symbols.iter().all(HashMap::is_empty) {
            return false;
        }

        self.dependencies(function, true).iter().any(|&capture| {
            self.inline_argument(capture)
                .is_some_and(|hir| !self.is_static_hir(hir) && may_fail(self, hir))
        })
    }

    fn function_failures(&self) -> &RefCell<HashMap<SymbolId, bool>> {
        &self.function_failures
    }
}

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

    /// Decides whether the binding can be substituted again, since its let statement is being
    /// lowered, which might be in a different inline function call than before.
    pub(super) fn redecide_substitution(&self, symbol: SymbolId) {
        if self.options.auto_inline && self.graph.references(symbol) == 1 {
            self.substitutions.borrow_mut().remove(&symbol);
            self.can_substitute(symbol);
        }
    }

    fn decide_substitution(&self, symbol: SymbolId) -> bool {
        match self.db.symbol(symbol) {
            Symbol::Binding(binding) => {
                !may_fail(self, binding.value.hir) || self.binding_always_evaluated(symbol)
            }
            Symbol::Constant(constant) => {
                !may_fail(self, constant.value.hir) || self.constant_always_evaluated(symbol)
            }
            _ => true,
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

        let statements = reachable_statements(&block.statements);
        let rest = statements.get(index + 1..).unwrap_or_default();
        let body = reachable_body(&block.statements, block.body);

        Evaluation::new(self, symbol).block(rest, body)
    }

    /// Whether the constant's single reference is always evaluated when the program is run, or
    /// fails. Constants that are referenced more than once are evaluated at the start of the
    /// program, so a reference in one of their values counts too.
    fn constant_always_evaluated(&self, symbol: SymbolId) -> bool {
        let Symbol::Function(main) = self.db.symbol(self.main) else {
            return false;
        };

        if main.kind == FunctionKind::External {
            return false;
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

        let mut evaluation = Evaluation::new(self, symbol);
        roots.into_iter().any(|root| evaluation.hir(root))
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
}

impl<'a, 'd, 'b, 'g> Evaluation<'a, 'd, 'b, 'g> {
    fn new(lowerer: &'a Lowerer<'d, 'b, 'g>, target: SymbolId) -> Self {
        Self {
            lowerer,
            target,
            arguments: Vec::new(),
            visited: HashSet::new(),
        }
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
        match self.db().hir(hir).clone() {
            Hir::Unresolved
            | Hir::Nil
            | Hir::String(_)
            | Hir::Int(_)
            | Hir::Bytes(_)
            | Hir::Bool(_)
            | Hir::InfinityG1
            | Hir::InfinityG2
            | Hir::Const(_) => false,
            Hir::Reference(symbol) => self.reference(symbol),
            Hir::Lambda(lambda) => self.captures_target(lambda),
            Hir::Block(block) => self.block(&block.statements, block.body),
            Hir::If(condition, then, else_, inline) => {
                if inline {
                    self.any([condition, then, else_])
                } else {
                    self.hir(condition) || (self.hir(then) && self.hir(else_))
                }
            }
            // The right side of `&&` and `||` is only evaluated some of the time
            Hir::Binary(BinaryOp::And | BinaryOp::Or, left, _) => self.hir(left),
            Hir::FunctionCall(call) => self.call(call.function, &call.args),
            Hir::Guard(guard) => guard.value_uses() > 0 && self.hir(guard.value),
            Hir::Cast(hir) | Hir::Unary(_, hir) | Hir::ClvmOp(_, hir) => self.hir(hir),
            Hir::FieldAccess(access) => self.hir(access.value),
            Hir::List(list) => self.any(list.items),
            Hir::Struct(initializer) => {
                self.any(initializer.fields.into_iter().map(|field| field.value))
            }
            Hir::Pair(a, b) | Hir::Binary(_, a, b) => self.any([a, b]),
            Hir::CoinId(a, b, c)
            | Hir::Modpow(a, b, c)
            | Hir::Secp256K1Verify(a, b, c)
            | Hir::Secp256R1Verify(a, b, c) => self.any([a, b, c]),
            Hir::Substr(hir, start, end) => self.any([hir, start]) || self.any(end),
            Hir::G1Map(data, dst) | Hir::G2Map(data, dst) => self.hir(data) || self.any(dst),
            Hir::BlsPairingIdentity(args) => self.any(args),
            Hir::BlsVerify(sig, args) => self.hir(sig) || self.any(args),
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
            Symbol::Binding(binding) => self.visited.insert(symbol) && self.hir(binding.value.hir),
            Symbol::Constant(constant) => {
                self.visited.insert(symbol) && self.hir(constant.value.hir)
            }
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

        let function = match db.hir(db.without_casts(callee)) {
            Hir::Reference(symbol) if matches!(db.symbol(*symbol), Symbol::Function(_)) => {
                Some(*symbol)
            }
            Hir::Lambda(lambda) => Some(*lambda),
            _ => None,
        };

        let Some(function) = function else {
            return self.hir(callee) || self.any(args.iter().copied());
        };

        let Symbol::Function(symbol) = db.symbol(function) else {
            unreachable!();
        };

        // The arguments of an `inline fn` are evaluated where its parameters are referenced
        if symbol.kind == FunctionKind::Inline {
            let parameters = if symbol.nil_terminated {
                symbol.parameters.len()
            } else {
                symbol.parameters.len().saturating_sub(1)
            };

            let arguments = symbol
                .parameters
                .iter()
                .take(parameters)
                .copied()
                .zip(args.iter().copied())
                .collect();

            self.arguments.push(arguments);
            let evaluated = self.visited.insert(function) && self.hir(symbol.body);
            self.visited.remove(&function);
            self.arguments.pop();
            return evaluated || self.captures_target(function);
        }

        // Otherwise, an argument that references the target may fail, so it's evaluated at the
        // call, and the body is evaluated whenever the function is called
        self.hir(callee)
            || self.any(args.iter().copied())
            || (self.visited.insert(function) && self.hir(symbol.body))
    }
}
