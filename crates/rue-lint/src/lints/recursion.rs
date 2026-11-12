use std::collections::HashMap;

use rue_diagnostic::{Lint, Note};
use rue_hir::{BinaryOp, Database, FunctionKind, Hir, HirId, Statement, Symbol, SymbolId};

use crate::{HirLintPass, LintContext};

/// A function that calls itself on every path never returns, since each call makes another one.
#[derive(Debug, Default)]
pub(crate) struct UnconditionalRecursion;

impl HirLintPass for UnconditionalRecursion {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::UnconditionalRecursion]
    }

    fn check_function(&mut self, cx: &mut LintContext<'_>, symbol: SymbolId, body: HirId) {
        let db = cx.db();

        // Inline functions can't be recursive, which is already an error
        let Symbol::Function(function) = db.symbol(symbol) else {
            return;
        };

        if function.kind == FunctionKind::Inline {
            return;
        }

        let Some(srcloc) = function
            .name
            .as_ref()
            .and_then(|name| name.srcloc())
            .cloned()
        else {
            return;
        };

        let mut search = Search {
            db,
            function: symbol,
            bindings: HashMap::new(),
        };

        let Some(call) = search
            .expr(body)
            .and_then(|call| db.hir_srcloc(call).cloned())
        else {
            return;
        };

        let name = function
            .name
            .as_ref()
            .map_or("", |name| name.text())
            .to_string();

        if let Some(diagnostic) = cx.report(
            Lint::UnconditionalRecursion,
            srcloc,
            format!("Function `{name}` calls itself on every path, so it never returns"),
        ) {
            diagnostic
                .notes
                .push(Note::at(call, "It calls itself here"));
        }
    }
}

/// Finds a call to the function that's evaluated on every path through an expression, before it
/// finishes evaluating. A path that raises finishes without calling it.
struct Search<'a> {
    db: &'a Database,
    function: SymbolId,
    /// The result of searching each binding's value, so that it's only searched once.
    bindings: HashMap<SymbolId, Option<HirId>>,
}

impl Search<'_> {
    fn expr(&mut self, hir: HirId) -> Option<HirId> {
        match self.db.hir(hir).clone() {
            Hir::Unresolved
            | Hir::Nil
            | Hir::String(_)
            | Hir::Int(_)
            | Hir::Bytes(_)
            | Hir::Bool(_)
            | Hir::InfinityG1
            | Hir::InfinityG2
            // A lambda's body isn't evaluated until it's called, and a constant expression is
            // evaluated at compile time
            | Hir::Lambda(_)
            | Hir::Const(_) => None,
            // A binding's value is evaluated if it's referenced
            Hir::Reference(symbol) => {
                if let Some(&call) = self.bindings.get(&symbol) {
                    return call;
                }

                // Bindings can't reference themselves, but this stops the search from repeating
                self.bindings.insert(symbol, None);

                let call = match self.db.symbol(symbol) {
                    Symbol::Binding(binding) => self.expr(binding.value.hir),
                    _ => None,
                };

                self.bindings.insert(symbol, call);
                call
            }
            Hir::FunctionCall(call) => {
                if matches!(
                    self.db.hir(self.db.without_casts(call.function)),
                    Hir::Reference(symbol) if *symbol == self.function
                ) {
                    return Some(hir);
                }

                self.any([call.function].into_iter().chain(call.args))
            }
            Hir::If(condition, then, else_, inline) => self.expr(condition).or_else(|| {
                if inline {
                    // Both branches of an `inline if` are evaluated
                    self.any([then, else_])
                } else {
                    let then = self.expr(then)?;
                    self.expr(else_).map(|_| then)
                }
            }),
            // The right side of `&&` and `||` is only evaluated in some cases
            Hir::Binary(BinaryOp::And | BinaryOp::Or, left, _) => self.expr(left),
            Hir::Binary(_, left, right) | Hir::Pair(left, right) => self.any([left, right]),
            Hir::List(list) => self.any(list.items),
            Hir::Struct(initializer) => self.any(
                initializer
                    .fields
                    .into_iter()
                    .filter(|field| !field.is_default)
                    .map(|field| field.value),
            ),
            Hir::FieldAccess(access) => self.expr(access.value),
            Hir::Guard(guard) => self.expr(guard.value),
            Hir::Cast(hir) | Hir::Unary(_, hir) | Hir::ClvmOp(_, hir) => self.expr(hir),
            Hir::CoinId(a, b, c)
            | Hir::Modpow(a, b, c)
            | Hir::Secp256K1Verify(a, b, c)
            | Hir::Secp256R1Verify(a, b, c) => self.any([a, b, c]),
            Hir::Substr(a, b, c) => self.any([a, b].into_iter().chain(c)),
            Hir::G1Map(a, b) | Hir::G2Map(a, b) => self.any(std::iter::once(a).chain(b)),
            Hir::BlsPairingIdentity(args) => self.any(args),
            Hir::BlsVerify(a, args) => self.any(std::iter::once(a).chain(args)),
            Hir::Block(block) => self.block(&block.statements, block.body),
        }
    }

    fn any(&mut self, hirs: impl IntoIterator<Item = HirId>) -> Option<HirId> {
        hirs.into_iter().find_map(|hir| self.expr(hir))
    }

    fn block(&mut self, statements: &[Statement], body: Option<HirId>) -> Option<HirId> {
        for (index, statement) in statements.iter().enumerate() {
            match statement {
                // A binding is only evaluated if it's referenced, which is searched from there
                Statement::Let(..) => {}
                Statement::Expr(statement) => {
                    if let Some(call) = self.expr(statement.hir) {
                        return Some(call);
                    }
                }
                Statement::Debug(hir, _) => {
                    if let Some(call) = self.expr(*hir) {
                        return Some(call);
                    }
                }
                Statement::If(statement) => {
                    if let Some(call) = self.expr(statement.condition) {
                        return Some(call);
                    }

                    // The `then` block exits, so both it and the rest of the block must call it
                    let then = self.expr(statement.then)?;
                    return self.block(&statements[index + 1..], body).map(|_| then);
                }
                Statement::Return(hir, _) => return self.expr(*hir),
                Statement::Raise(hir, _) => return hir.and_then(|hir| self.expr(hir)),
                // If the assertion fails, it raises before anything after it
                Statement::Assert(hir, _) => return self.expr(*hir),
            }
        }

        body.and_then(|body| self.expr(body))
    }
}
