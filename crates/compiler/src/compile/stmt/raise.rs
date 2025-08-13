use rue_ast::AstRaiseStmt;
use rue_hir::Statement;

use crate::Compiler;

pub fn compile_raise_stmt(_ctx: &mut Compiler, _stmt: &AstRaiseStmt) -> Statement {
    todo!()
}
