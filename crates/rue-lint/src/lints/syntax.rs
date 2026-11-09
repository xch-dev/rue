//! Lints that only depend on the syntax tree.

use rue_ast::{AstExpr, AstGenericArguments, AstGenericParameters, AstNode, AstPrefixExpr};
use rue_diagnostic::Lint;
use rue_parser::T;

use crate::{LintContext, LintInput, srcloc};

pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    if !cx.is_enabled(Lint::EmptyGenerics) && !cx.is_enabled(Lint::DoubleNegation) {
        return;
    }

    for &(source, document) in &input.documents {
        for node in document.syntax().descendants() {
            if let Some(parameters) = AstGenericParameters::cast(node.clone()) {
                if parameters.names().next().is_none() {
                    cx.report(
                        Lint::EmptyGenerics,
                        srcloc(source, node.text_range()),
                        "Unnecessary empty generic parameter list specified",
                    );
                }
            } else if let Some(arguments) = AstGenericArguments::cast(node.clone()) {
                if arguments.types().next().is_none() {
                    cx.report(
                        Lint::EmptyGenerics,
                        srcloc(source, node.text_range()),
                        "Unnecessary empty generic argument list specified",
                    );
                }
            } else if let Some(prefix) = AstPrefixExpr::cast(node.clone())
                && let Some(op) = prefix.op()
                && op.kind() == T![-]
                && let Some(AstExpr::PrefixExpr(inner)) = prefix.expr()
                && let Some(inner_op) = inner.op()
                && inner_op.kind() == T![-]
            {
                cx.report(
                    Lint::DoubleNegation,
                    srcloc(source, node.text_range()),
                    "Use of `--` in an expression will perform a double negation, but could be confused with the 'prefix decrement' operator in other languages",
                );
            }
        }
    }
}
