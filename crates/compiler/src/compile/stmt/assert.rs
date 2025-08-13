use rue_ast::AstAssertStmt;
use rue_hir::Statement;

use crate::Compiler;

pub fn compile_assert_stmt(_ctx: &mut Compiler, _stmt: &AstAssertStmt) -> Statement {
    todo!()
}
