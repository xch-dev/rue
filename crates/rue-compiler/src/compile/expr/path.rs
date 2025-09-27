use log::debug;
use rue_ast::{AstNode, AstPathExpr};
use rue_hir::{Declaration, Hir, SymbolPath, Value};

use crate::{Compiler, PathKind, PathResult, compile_path};

pub fn compile_path_expr(ctx: &mut Compiler, path: &AstPathExpr) -> Value {
    let PathResult::Symbol(symbol) =
        compile_path(ctx, path.syntax(), path.segments(), PathKind::Symbol)
    else {
        debug!("Unresolved path expr");
        return ctx.builtins().unresolved.clone();
    };

    ctx.reference(Declaration::Symbol(symbol));

    let ty = ctx.symbol_type(symbol);

    Value::new(ctx.alloc_hir(Hir::Reference(symbol)), ty).with_reference(SymbolPath {
        symbol,
        path: vec![],
    })
}
