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
            loops: Vec::new(),
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
    /// The functions of the `for` expressions whose bodies are being searched (see
    /// [`FunctionKind::Loop`]), innermost last.
    loops: Vec<SymbolId>,
}

impl Search<'_> {
    fn expr(&mut self, hir: HirId) -> Option<HirId> {
        let node = self.db.hir(hir).clone();

        match node {
            // A lambda's body isn't evaluated until it's called, and a constant expression is
            // evaluated at compile time
            Hir::Lambda(_) | Hir::Const(_) => None,
            // A binding's value is evaluated if it's referenced
            Hir::Reference(symbol) => self.binding(symbol),
            Hir::FunctionCall(ref call)
                if matches!(
                    self.db.hir(self.db.without_casts(call.function)),
                    Hir::Reference(symbol) if *symbol == self.function
                ) =>
            {
                Some(hir)
            }
            Hir::FunctionCall(ref call) if self.loop_function(call.function).is_some() => self
                .any(call.args.iter().copied())
                .or_else(|| self.loop_call(hir, call.function)),
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
            Hir::Block(block) => {
                // A binding can only reference the ones declared before it, so searching them in
                // order means that each one is searched before anything that references it. This
                // keeps the search from recursing through a long chain of bindings that each
                // reference the previous one.
                for statement in &block.statements {
                    if let Statement::Let(symbol, _) = statement {
                        self.binding(*symbol);
                    }
                }

                self.block(&block.statements, block.body)
            }
            // Everything else evaluates all of the nodes inside of it
            _ => self.any(node.children()),
        }
    }

    /// The function of a `for` expression that the node refers to, and its body.
    fn loop_function(&self, callee: HirId) -> Option<(SymbolId, HirId)> {
        let Hir::Reference(symbol) = self.db.hir(callee) else {
            return None;
        };

        match self.db.symbol(*symbol) {
            Symbol::Function(function) if function.kind == FunctionKind::Loop => {
                Some((*symbol, function.body))
            }
            _ => None,
        }
    }

    /// Searches what the function of a `for` expression evaluates when it's called. It calls
    /// itself with the rest of its list until there's nothing left of it, so a call to the
    /// function in what it does at the end of its list is on every path, if every item gets to
    /// the loop's call of itself. That's where the items after a `for` that's spread in a list
    /// are, and the value of a fold.
    fn loop_call(&mut self, call: HirId, callee: HirId) -> Option<HirId> {
        let (function, body) = self.loop_function(callee)?;

        // The loop's call of itself gets to the end of its list, which is being searched
        if self.loops.contains(&function) {
            return Some(call);
        }

        let Hir::If(condition, step, end, _) = *self.db.hir(body) else {
            return None;
        };

        self.loops.push(function);
        let found = self.expr(condition).or_else(|| {
            self.expr(step)?;
            self.expr(end)
        });
        self.loops.pop();

        found
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
                // A branch might not call it and still return, raise or fall through, so the rest
                // of the block isn't counted
                Statement::Control(statement) => return self.expr(statement.hir),
                Statement::Return(hir, _) => return self.expr(*hir),
                Statement::Raise(hir, _) => return hir.and_then(|hir| self.expr(hir)),
                // If the assertion fails, it raises before anything after it, and its message is
                // only evaluated then
                Statement::Assert(hir, _, _) => return self.expr(*hir),
            }
        }

        body.and_then(|body| self.expr(body))
    }
}
