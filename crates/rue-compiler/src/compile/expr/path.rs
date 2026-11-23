use std::iter;

use log::debug;
use rue_ast::{AstNode, AstPathExpr};
use rue_diagnostic::{DiagnosticKind, Note};
use rue_hir::{Hir, Symbol, SymbolId, SymbolPath, Value};
use rue_parser::{SyntaxKind, SyntaxToken, T};

use crate::{Compiler, GetTextRange, PathKind, PathResult, compile_path};

pub fn compile_path_expr(ctx: &mut Compiler, path: &AstPathExpr) -> Value {
    compile_path_value(ctx, path, false)
}

/// Compiles a path expression. Builtins can only be referenced if `allow_builtin` is set, which
/// is only the case when they are being called directly.
pub fn compile_path_value(ctx: &mut Compiler, path: &AstPathExpr, allow_builtin: bool) -> Value {
    let count = ctx.diagnostic_count();

    let PathResult::Symbol(symbol, override_type, _) =
        compile_path(ctx, path.syntax(), path.segments(), PathKind::Symbol, true)
    else {
        note_struct_before_body(ctx, count, path);
        debug!("Unresolved path expr");
        return ctx.unresolved();
    };

    if !allow_builtin && !check_symbol_value(ctx, path.syntax(), symbol) {
        return ctx.unresolved();
    }

    let ty = ctx.symbol_type(symbol);

    let mut value =
        Value::new(ctx.alloc_hir(Hir::Reference(symbol)), ty).with_reference(SymbolPath {
            symbol,
            path: vec![],
        });

    if let Some(override_type) = override_type {
        value = value.with_type(override_type);
    }

    value
}

/// Reports an error if the symbol is a builtin, since builtins don't have a runtime value and
/// can only be called directly. Returns whether the symbol can be used as a value.
pub fn check_symbol_value(ctx: &mut Compiler, node: &impl GetTextRange, symbol: SymbolId) -> bool {
    if !matches!(ctx.symbol(symbol), Symbol::Builtin(_)) {
        return true;
    }

    let name = ctx.symbol_name(symbol);
    ctx.diagnostic(node, DiagnosticKind::CannotReferenceBuiltin(name));
    false
}

/// Explains an error about a type that's used as a value right before the body of an `if`, `match`
/// or `for`, such as `fold summary = Summary {}` followed by the body on the next line, which is
/// most likely a struct initializer whose braces were read as the body. A struct initializer whose
/// braces can't be a body is parsed as one, and reported as needing parentheses, by the parser.
fn note_struct_before_body(ctx: &mut Compiler, count: usize, path: &AstPathExpr) {
    let Some(brace) = iter::successors(
        path.syntax()
            .last_token()
            .and_then(|token| token.next_token()),
        SyntaxToken::next_token,
    )
    .find(|token| !token.kind().is_trivia()) else {
        return;
    };

    let is_body = brace.kind() == T!['{']
        && brace.parent().is_some_and(|parent| {
            matches!(parent.kind(), SyntaxKind::Block | SyntaxKind::MatchExpr)
        });

    if !is_body {
        return;
    }

    let srcloc = ctx.trimmed_srcloc(path.syntax());
    let name = srcloc.text();

    for diagnostic in ctx.diagnostics_since_mut(count) {
        if matches!(diagnostic.kind, DiagnosticKind::ExpectedSymbol(..)) {
            diagnostic.notes.push(Note::new(format!(
                "A `{{` after `{name}` starts the body here, so a struct initializer needs parentheses: `({name} {{ ... }})`"
            )));
        }
    }
}
