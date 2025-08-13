use rue_ast::AstReturnStmt;
use rue_hir::Statement;

use crate::Compiler;

pub fn compile_return_stmt(_ctx: &mut Compiler, _stmt: &AstReturnStmt) -> Statement {
    todo!()
}
