use std::collections::HashSet;

use rue_diagnostic::Lint;
use rue_hir::{Block, Declaration, Hir, HirId, Statement, Symbol, SymbolId};

use crate::{LintContext, LintInput};

/// A lint pass that runs over the HIR of the documents being linted. The walk visits every
/// function, constant and struct field default declared in them (including lambdas), and calls
/// the pass for each HIR node and statement once.
///
/// The type and location of the expression that a node was compiled from are available from
/// [`Database::hir_type`](rue_hir::Database::hir_type) and
/// [`Database::hir_srcloc`](rue_hir::Database::hir_srcloc). Nodes that the compiler generated in
/// other ways don't have either, so passes should skip them.
pub trait HirLintPass: Default {
    /// The lints the pass can report, so that the walk can be skipped if they're all allowed.
    const LINTS: &'static [Lint];

    /// Called for each function and lambda, before its body.
    fn check_function(&mut self, _cx: &mut LintContext<'_>, _symbol: SymbolId, _body: HirId) {}

    /// Called for each HIR node, before the nodes inside of it.
    fn check_hir(&mut self, _cx: &mut LintContext<'_>, _hir: HirId, _node: &Hir) {}

    /// Called for each statement of a block, before the nodes inside of it.
    fn check_statement(&mut self, _cx: &mut LintContext<'_>, _block: &Block, _index: usize) {}
}

/// Runs a HIR lint pass, in the form of a pass in [`PASSES`](crate::lints::PASSES).
pub(crate) fn run_hir_pass<P: HirLintPass>(cx: &mut LintContext<'_>, _input: &LintInput<'_>) {
    if !P::LINTS.iter().any(|&lint| cx.is_enabled(lint)) {
        return;
    }

    let mut walker = Walker {
        pass: P::default(),
        visited: HashSet::new(),
    };

    for root in roots(cx) {
        match root {
            Root::Function(symbol) => walker.visit_function(cx, symbol),
            Root::Hir(hir) => walker.visit_hir(cx, hir),
        }
    }
}

enum Root {
    Function(SymbolId),
    Hir(HirId),
}

/// Every function, constant and struct field default declared in the documents being linted.
/// Lambdas aren't declarations, so they're visited from the HIR they're declared in.
fn roots(cx: &LintContext<'_>) -> Vec<Root> {
    let db = cx.db();
    let mut roots = Vec::new();

    for declaration in db.relevant_declarations() {
        match declaration {
            Declaration::Symbol(symbol) => match db.symbol(symbol) {
                Symbol::Function(_) => roots.push(Root::Function(symbol)),
                Symbol::Constant(constant) => roots.push(Root::Hir(constant.value.hir)),
                // Bindings are visited from their `let` statement, and the rest don't have HIR
                _ => {}
            },
            Declaration::Type(ty) => {
                roots.extend(db.default_fields(ty).map(|(_, value)| Root::Hir(value.hir)));
            }
        }
    }

    roots
}

struct Walker<P> {
    pass: P,
    visited: HashSet<HirId>,
}

impl<P: HirLintPass> Walker<P> {
    fn visit_function(&mut self, cx: &mut LintContext<'_>, symbol: SymbolId) {
        let Symbol::Function(function) = cx.db().symbol(symbol) else {
            return;
        };

        let body = function.body;

        // The body is only visited once, so the function is too
        if self.visited.contains(&body) {
            return;
        }

        self.pass.check_function(cx, symbol, body);
        self.visit_hir(cx, body);
    }

    fn visit_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId) {
        // A few nodes that the compiler generates are shared, such as the `nil` of a `return`
        // without a value, so each node is only visited once
        if !self.visited.insert(hir) {
            return;
        }

        let node = cx.db().hir(hir).clone();

        self.pass.check_hir(cx, hir, &node);

        match node {
            Hir::Unresolved
            | Hir::Nil
            | Hir::String(_)
            | Hir::Int(_)
            | Hir::Bytes(_)
            | Hir::Bool(_)
            | Hir::Reference(_)
            | Hir::InfinityG1
            | Hir::InfinityG2 => {}
            // The result of a const expression is generated, so only the expression is visited
            Hir::Const(expr) => self.visit_hir(cx, expr.value),
            Hir::List(list) => self.visit_all(cx, list.items),
            // Default values are visited from the struct declaration instead
            Hir::Struct(initializer) => self.visit_all(
                cx,
                initializer
                    .fields
                    .into_iter()
                    .filter(|field| !field.is_default)
                    .map(|field| field.value),
            ),
            Hir::FieldAccess(access) => self.visit_hir(cx, access.value),
            Hir::Cast(hir) => self.visit_hir(cx, hir),
            Hir::Guard(guard) => self.visit_hir(cx, guard.value),
            Hir::Block(block) => self.visit_block(cx, &block),
            Hir::Lambda(symbol) => self.visit_function(cx, symbol),
            Hir::Pair(first, rest) => self.visit_all(cx, [first, rest]),
            Hir::If(condition, then, else_, _) => self.visit_all(cx, [condition, then, else_]),
            Hir::FunctionCall(call) => {
                self.visit_hir(cx, call.function);
                self.visit_all(cx, call.args);
            }
            Hir::Unary(_, hir) | Hir::ClvmOp(_, hir) => self.visit_hir(cx, hir),
            Hir::Binary(_, left, right) => self.visit_all(cx, [left, right]),
            Hir::CoinId(a, b, c)
            | Hir::Modpow(a, b, c)
            | Hir::Secp256K1Verify(a, b, c)
            | Hir::Secp256R1Verify(a, b, c) => self.visit_all(cx, [a, b, c]),
            Hir::Substr(hir, start, end) => {
                self.visit_all(cx, [hir, start]);
                self.visit_all(cx, end);
            }
            Hir::G1Map(data, dst) | Hir::G2Map(data, dst) => {
                self.visit_hir(cx, data);
                self.visit_all(cx, dst);
            }
            Hir::BlsPairingIdentity(args) => self.visit_all(cx, args),
            Hir::BlsVerify(sig, args) => {
                self.visit_hir(cx, sig);
                self.visit_all(cx, args);
            }
        }
    }

    fn visit_all(&mut self, cx: &mut LintContext<'_>, hirs: impl IntoIterator<Item = HirId>) {
        for hir in hirs {
            self.visit_hir(cx, hir);
        }
    }

    fn visit_block(&mut self, cx: &mut LintContext<'_>, block: &Block) {
        for (index, statement) in block.statements.iter().enumerate() {
            self.pass.check_statement(cx, block, index);

            match statement {
                Statement::Let(symbol) => {
                    if let Symbol::Binding(binding) = cx.db().symbol(*symbol) {
                        let value = binding.value.hir;
                        self.visit_hir(cx, value);
                    }
                }
                Statement::Expr(statement) => self.visit_hir(cx, statement.hir),
                Statement::If(statement) => {
                    self.visit_all(cx, [statement.condition, statement.then]);
                }
                Statement::Return(hir, _)
                | Statement::Assert(hir, _)
                | Statement::Debug(hir, _) => {
                    self.visit_hir(cx, *hir);
                }
                Statement::Raise(hir, _) => self.visit_all(cx, *hir),
            }
        }

        self.visit_all(cx, block.body);
    }
}
