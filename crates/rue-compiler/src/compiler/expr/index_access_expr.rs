use rue_parser::{AstNode, IndexAccessExpr};

use crate::{
    compiler::Compiler,
    value::{Type, Value},
    ErrorKind,
};

impl Compiler<'_> {
    pub fn compile_index_access_expr(&mut self, index_access: &IndexAccessExpr) -> Value {
        let Some(value) = index_access
            .expr()
            .map(|expr| self.compile_expr(&expr, None))
        else {
            return self.unknown();
        };

        let Some(index_token) = index_access.index() else {
            return self.unknown();
        };

        let index = index_token
            .text()
            .replace('_', "")
            .parse()
            .expect("failed to parse integer literal");

        let Type::List(item_type) = self.db.ty(value.type_id).clone() else {
            self.db.error(
                ErrorKind::InvalidIndexAccess(self.type_name(value.type_id)),
                index_access.expr().unwrap().syntax().text_range(),
            );
            return self.unknown();
        };

        Value::new(self.compile_index(value.hir_id, index, false), item_type)
    }
}
