use std::collections::HashMap;

use rue_diagnostic::{Lint, Note, SrcLoc};
use rue_hir::{
    Block, Database, Declaration, FailureAnalysis, FunctionCall, FunctionKind, Hir, HirId,
    Statement, Symbol, SymbolId,
};

use crate::{HirLintPass, LintContext, LintInput};

/// An `inline` value is evaluated where it's referenced, rather than where it's defined, so a
/// value that can fail (such as one that calls a function with an `assert`) is skipped wherever
/// it isn't referenced. Without `inline`, it would be evaluated where it's defined, so the check
/// would always run.
///
/// The arguments of an `inline fn` are substituted for its parameters in the same way, so an
/// argument that can fail is skipped if the function doesn't always evaluate its parameter.
#[derive(Debug)]
pub(crate) struct InlineValueMayFail<'a> {
    failures: &'a FailureAnalysis,
    /// Whether the body of the `inline fn` that each parameter belongs to always evaluates it.
    evaluated_parameters: HashMap<SymbolId, bool>,
}

impl<'a> InlineValueMayFail<'a> {
    pub(crate) fn new(input: &'a LintInput<'a>) -> Self {
        Self {
            failures: input.failures(),
            evaluated_parameters: HashMap::new(),
        }
    }
}

impl HirLintPass for InlineValueMayFail<'_> {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::InlineValueMayFail]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, _hir: HirId, node: &Hir) {
        if let Hir::FunctionCall(call) = node {
            self.check_call(cx, call);
        }
    }

    fn check_statement(&mut self, cx: &mut LintContext<'_>, block: &Block, index: usize) {
        let Statement::Let(symbol, ref srcloc) = block.statements[index] else {
            return;
        };

        let Symbol::Binding(binding) = cx.db().symbol(symbol) else {
            return;
        };

        if !binding.inline || !self.failures.may_fail(cx.db(), binding.value.hir) {
            return;
        }

        // The result of a `for` expression's fold for the rest of the list is only evaluated
        // where the body refers to it, which is what a fold is, rather than an `inline let`
        if let Hir::FunctionCall(call) = cx.db().hir(binding.value.hir)
            && let Hir::Reference(function) = cx.db().hir(call.function)
            && matches!(
                cx.db().symbol(*function),
                Symbol::Function(function) if function.kind == FunctionKind::Loop
            )
        {
            return;
        }

        if self.failures.always_evaluated_in_block(
            cx.db(),
            symbol,
            &block.statements[index + 1..],
            block.body,
        ) {
            return;
        }

        report(
            cx,
            srcloc.clone(),
            "This `inline let` can fail, but it's only evaluated where it's referenced, which doesn't always happen, so its check can be skipped",
            "Remove `inline` to always evaluate it here",
        );
    }

    // There's no single place that a constant is evaluated, so any reference to an inline
    // constant might be somewhere that isn't always evaluated
    fn finish(&mut self, cx: &mut LintContext<'_>) {
        let db = cx.db();
        let mut reports = Vec::new();

        for declaration in db.relevant_declarations() {
            let Declaration::Symbol(symbol) = declaration else {
                continue;
            };

            let Symbol::Constant(constant) = db.symbol(symbol) else {
                continue;
            };

            if !constant.inline || !self.failures.may_fail(db, constant.value.hir) {
                continue;
            }

            if let Some(srcloc) = constant.name.as_ref().and_then(|name| name.srcloc()) {
                reports.push(srcloc.clone());
            }
        }

        for srcloc in reports {
            report(
                cx,
                srcloc,
                "This `inline const` can fail, but it's only evaluated where it's referenced, so its check is skipped wherever it isn't",
                "Remove `inline` to evaluate it whenever the program references it",
            );
        }
    }
}

impl InlineValueMayFail<'_> {
    fn check_call(&mut self, cx: &mut LintContext<'_>, call: &FunctionCall) {
        let db = cx.db();

        let Hir::Reference(function) = db.hir(db.without_casts(call.function)) else {
            return;
        };

        let function = *function;

        let Symbol::Function(symbol) = db.symbol(function) else {
            return;
        };

        if symbol.kind != FunctionKind::Inline {
            return;
        }

        let fixed = symbol.fixed_parameters();

        // The arguments after the fixed parameters are passed to the rest parameter as a list
        let rest = symbol.parameters.get(fixed.len()).copied();

        let mut skipped = Vec::new();

        for (index, &arg) in call.args.iter().enumerate() {
            let Some(parameter) = fixed.get(index).copied().or(rest) else {
                continue;
            };

            // An inline binding or constant is reported where it's defined instead
            if is_inline_value(db, arg)
                || !self.failures.may_fail(db, arg)
                || self.always_evaluated(db, symbol.body, parameter)
            {
                continue;
            }

            if let Some(srcloc) = db.hir_srcloc(arg) {
                skipped.push((srcloc.clone(), parameter));
            }
        }

        for (srcloc, parameter) in skipped {
            let db = cx.db();
            let function_name = symbol_name(db, function);
            let parameter_name = symbol_name(db, parameter);

            report(
                cx,
                srcloc,
                &format!(
                    "This argument can fail, but `{function_name}` is an `inline fn` that doesn't always evaluate its parameter `{parameter_name}`, so its check can be skipped"
                ),
                "Bind the argument with `let` before the call to always evaluate it, or remove `inline` from the function",
            );
        }
    }

    fn always_evaluated(&mut self, db: &Database, body: HirId, parameter: SymbolId) -> bool {
        if let Some(&evaluated) = self.evaluated_parameters.get(&parameter) {
            return evaluated;
        }

        let evaluated = self.failures.always_evaluated(db, parameter, body);
        self.evaluated_parameters.insert(parameter, evaluated);
        evaluated
    }
}

/// Whether the HIR is a reference to an inline binding or constant.
fn is_inline_value(db: &Database, hir: HirId) -> bool {
    matches!(
        db.hir(db.without_casts(hir)),
        Hir::Reference(symbol) if db.symbol(*symbol).inline_value().is_some()
    )
}

fn symbol_name(db: &Database, symbol: SymbolId) -> String {
    db.symbol(symbol)
        .name()
        .map_or_else(String::new, |name| name.text().to_string())
}

fn report(cx: &mut LintContext<'_>, srcloc: SrcLoc, message: &str, note: &str) {
    if let Some(diagnostic) = cx.report(Lint::InlineValueMayFail, srcloc, message) {
        diagnostic.notes.push(Note::new(note));
    }
}
