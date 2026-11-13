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
        let node = self.db.hir(hir).clone();

        match node {
            // A lambda's body isn't evaluated until it's called, and a constant expression is
            // evaluated at compile time
            Hir::Lambda(_) | Hir::Const(_) => None,
            // An inline binding's value is evaluated where it's referenced. Other bindings are
            // evaluated at their let statement, which is searched first, so this finds the same call
            Hir::Reference(symbol) => self.binding(symbol),
            Hir::FunctionCall(ref call)
                if matches!(
                    self.db.hir(self.db.without_casts(call.function)),
                    Hir::Reference(symbol) if *symbol == self.function
                ) =>
            {
                Some(hir)
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
            // Default values are generated, and evaluated wherever the struct is initialized
            Hir::Struct(initializer) => self.any(initializer.written_values()),
            Hir::Block(block) => self.block(&block.statements, block.body),
            // Everything else evaluates all of the nodes inside of it
            _ => self.any(node.children()),
        }
    }

    fn binding(&mut self, symbol: SymbolId) -> Option<HirId> {
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

    fn any(&mut self, hirs: impl IntoIterator<Item = HirId>) -> Option<HirId> {
        hirs.into_iter().find_map(|hir| self.expr(hir))
    }

    fn block(&mut self, statements: &[Statement], body: Option<HirId>) -> Option<HirId> {
        for (index, statement) in statements.iter().enumerate() {
            match statement {
                // A binding that isn't inline is evaluated at its let statement
                Statement::Let(symbol, _) => {
                    if matches!(self.db.symbol(*symbol), Symbol::Binding(binding) if !binding.inline)
                        && let Some(call) = self.binding(*symbol)
                    {
                        return Some(call);
                    }
                }
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
