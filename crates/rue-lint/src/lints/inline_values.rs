use std::collections::HashMap;

use rue_diagnostic::{Lint, Note, SrcLoc};
use rue_hir::{
    Block, Declaration, FailureAnalysis, FunctionCall, FunctionKind, Hir, HirId, Statement, Symbol,
    SymbolId,
};

use crate::{HirLintPass, LintContext};

/// An `inline` value is evaluated where it's referenced, rather than where it's defined, so a
/// value that can fail (such as one that calls a function with an `assert`) is skipped wherever
/// it isn't referenced. Without `inline`, it would be evaluated where it's defined, so the check
/// would always run.
///
/// The arguments of an `inline fn` are substituted for its parameters in the same way, so an
/// argument that can fail is skipped if the function doesn't always evaluate its parameter.
#[derive(Debug, Default)]
pub(crate) struct InlineValueMayFail {
    failures: FailureAnalysis,
    /// Whether the body of each `inline fn` always evaluates each of its parameters.
    evaluated_parameters: HashMap<(SymbolId, SymbolId), bool>,
}

impl HirLintPass for InlineValueMayFail {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::InlineValueMayFail]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        if let Hir::FunctionCall(call) = node {
            self.check_call(cx, call);
        }

        // The value of an inline constant is the root of the walk over its declaration
        if cx.parent().is_some() {
            return;
        }

        let Some(Declaration::Symbol(symbol)) = cx.enclosing_declaration() else {
            return;
        };

        let Symbol::Constant(constant) = cx.db().symbol(symbol) else {
            return;
        };

        if !constant.inline || constant.value.hir != hir {
            return;
        }

        let Some(srcloc) = constant
            .name
            .as_ref()
            .and_then(|name| name.srcloc())
            .cloned()
        else {
            return;
        };

        // There's no single place that a constant is evaluated, so any reference to it might be
        // somewhere that isn't always evaluated
        if self.failures.may_fail(cx.db(), hir) {
            report(
                cx,
                srcloc,
                "This `inline const` can fail, but it's only evaluated where it's referenced, so its check is skipped wherever it isn't",
                "Remove `inline` to evaluate it whenever the program references it",
            );
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
}

impl InlineValueMayFail {
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
        let rest = (!symbol.nil_terminated)
            .then(|| symbol.parameters.last().copied())
            .flatten();

        let mut skipped = Vec::new();

        for (index, &arg) in call.args.iter().enumerate() {
            // The arguments after the fixed parameters are passed to the rest parameter as a list
            let Some(parameter) = fixed.get(index).copied().or(rest) else {
                continue;
            };

            if !self.failures.may_fail(db, arg) || self.always_evaluated(cx, function, parameter) {
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

    fn always_evaluated(
        &mut self,
        cx: &LintContext<'_>,
        function: SymbolId,
        parameter: SymbolId,
    ) -> bool {
        if let Some(&evaluated) = self.evaluated_parameters.get(&(function, parameter)) {
            return evaluated;
        }

        let Symbol::Function(symbol) = cx.db().symbol(function) else {
            return true;
        };

        let evaluated = self
            .failures
            .always_evaluated(cx.db(), parameter, symbol.body);

        self.evaluated_parameters
            .insert((function, parameter), evaluated);

        evaluated
    }
}

fn symbol_name(db: &rue_hir::Database, symbol: SymbolId) -> String {
    let name = match db.symbol(symbol) {
        Symbol::Function(function) => function.name.as_ref(),
        Symbol::Parameter(parameter) => parameter.name.as_ref(),
        _ => None,
    };

    name.map_or_else(String::new, |name| name.text().to_string())
}

fn report(cx: &mut LintContext<'_>, srcloc: SrcLoc, message: &str, note: &str) {
    if let Some(diagnostic) = cx.report(Lint::InlineValueMayFail, srcloc, message) {
        diagnostic.notes.push(Note::new(note));
    }
}
