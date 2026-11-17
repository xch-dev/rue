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
    /// The symbols whose values or bodies have been followed outside of any scope, so that each
    /// is only followed once.
    followed: HashSet<SymbolId>,
    /// The calls to `inline fn`s being followed, and the walks of their arguments, innermost
    /// last. What's followed inside a scope is forgotten when it ends, since it depends on the
    /// arguments, and is out of reach while an argument of an outer call is walked.
    scopes: Vec<Scope>,
}

/// A call to an `inline fn` being followed, whose arguments are evaluated where the parameters
/// are referenced, or the walk of an argument, which has none.
#[derive(Default)]
struct Scope {
    arguments: HashMap<SymbolId, HirId>,
    /// The symbols whose values or bodies have been followed inside of the scope.
    followed: HashSet<SymbolId>,
}

impl<'a, C: AnalysisContext> Evaluation<'a, C> {
    pub(crate) fn new(cx: &'a C, target: SymbolId) -> Self {
        Self {
            cx,
            target,
            followed: HashSet::new(),
            scopes: Vec::new(),
        }
    }

    /// Whether the symbol's value or body hasn't been followed in a scope that's in reach, in
    /// which case it's followed in the innermost one.
    fn follow(&mut self, symbol: SymbolId) -> bool {
        if self.followed.contains(&symbol)
            || self
                .scopes
                .iter()
                .any(|scope| scope.followed.contains(&symbol))
        {
            return false;
        }

        match self.scopes.last_mut() {
            Some(scope) => scope.followed.insert(symbol),
            None => self.followed.insert(symbol),
        }
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

        if let Some((frame, hir)) = self.argument(symbol, self.scopes.len()) {
            // The argument is written outside of the call it's passed to, so the parameters it
            // references are those of the calls around that one, and what was followed inside
            // that call (such as the `inline fn` itself) can be followed again in it
            let inner = self.scopes.split_off(frame);
            self.scopes.push(Scope::default());
            let evaluated = self.hir(hir);
            self.scopes.pop();
            self.scopes.extend(inner);
            return evaluated;
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

    /// The innermost of the first `frames` frames of arguments that has one for the parameter,
    /// and that argument.
    fn argument(&self, parameter: SymbolId, frames: usize) -> Option<(usize, HirId)> {
        self.scopes[..frames]
            .iter()
            .enumerate()
            .rev()
            .find_map(|(index, scope)| Some((index, *scope.arguments.get(&parameter)?)))
    }

    /// Whether the HIR could reference the target at all, following the values and bodies it
    /// references, with the first `frames` frames of arguments in scope.
    fn mentions(&self, hir: HirId, frames: usize, visited: &mut HashSet<SymbolId>) -> bool {
        rue_types::grow_stack(|| {
            let node = self.db().hir(hir);

            match node {
                Hir::Reference(symbol) | Hir::Lambda(symbol) => {
                    self.mentions_symbol(*symbol, frames, visited)
                }
                Hir::Block(block) => {
                    block.statements.iter().any(|statement| match *statement {
                        Statement::Let(symbol, _) => self.mentions_symbol(symbol, frames, visited),
                        _ => statement
                            .hirs()
                            .any(|hir| self.mentions(hir, frames, visited)),
                    }) || block
                        .body
                        .is_some_and(|body| self.mentions(body, frames, visited))
                }
                _ => node
                    .children()
                    .any(|child| self.mentions(child, frames, visited)),
            }
        })
    }

    fn mentions_symbol(
        &self,
        symbol: SymbolId,
        frames: usize,
        visited: &mut HashSet<SymbolId>,
    ) -> bool {
        if symbol == self.target {
            return true;
        }

        if let Some((frame, hir)) = self.argument(symbol, frames) {
            return self.mentions(hir, frame, visited);
        }

        if !visited.insert(symbol) {
            return false;
        }

        match self.db().symbol(symbol) {
            Symbol::Binding(binding) => self.mentions(binding.value.hir, frames, visited),
            Symbol::Constant(constant) => self.mentions(constant.value.hir, frames, visited),
            Symbol::Function(function) => self.mentions(function.body, frames, visited),
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

            // Unless the target is a constant, which any function can reference, an `inline fn`
            // can only reach it through what it's passed. Otherwise the body could still fail
            // anyway, but treating it as not evaluating the target is only conservative, and it
            // saves walking each call again for every target.
            if !matches!(db.symbol(self.target), Symbol::Constant(_)) {
                let mut visited = HashSet::new();
                let frames = self.scopes.len();

                if !args
                    .iter()
                    .chain([&callee])
                    .any(|&hir| self.mentions(hir, frames, &mut visited))
                {
                    return false;
                }
            }

            let arguments = symbol
                .fixed_parameters()
                .iter()
                .copied()
                .zip(args.iter().copied())
                .collect();

            // What's followed inside the body depends on the arguments, so it's followed again
            // for each call
            self.scopes.push(Scope {
                arguments,
                followed: HashSet::new(),
            });
            let evaluated = self.follow(function) && self.hir(symbol.body);
            self.scopes.pop();

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
