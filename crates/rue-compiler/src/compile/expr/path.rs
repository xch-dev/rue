use log::debug;
use rue_ast::{AstNode, AstPathExpr};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Hir, Symbol, SymbolId, SymbolPath, Value};

use crate::{Compiler, GetTextRange, PathKind, PathResult, compile_path};

pub fn compile_path_expr(ctx: &mut Compiler, path: &AstPathExpr) -> Value {
    compile_path_value(ctx, path, false)
}

/// Compiles a path expression. Builtins can only be referenced if `allow_builtin` is set, which
/// is only the case when they are being called directly.
pub fn compile_path_value(ctx: &mut Compiler, path: &AstPathExpr, allow_builtin: bool) -> Value {
    let PathResult::Symbol(symbol, override_type, _) =
        compile_path(ctx, path.syntax(), path.segments(), PathKind::Symbol, true)
    else {
        debug!("Unresolved path expr");
        return ctx.builtins().unresolved.clone();
    };

    if !allow_builtin && !check_symbol_value(ctx, path.syntax(), symbol) {
        return ctx.builtins().unresolved.clone();
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
