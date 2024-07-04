use rue_parser::{AstNode, ExistsExpr};

use crate::{
    compiler::Compiler,
    hir::{Hir, Op},
    value::{Guard, Mutation, Type, TypeOverride, Value},
    ErrorKind,
};

impl Compiler<'_> {
    pub fn compile_exists_expr(&mut self, exists: &ExistsExpr) -> Value {
        let Some(value) = exists.expr().map(|expr| self.compile_expr(&expr, None)) else {
            return self.unknown();
        };

        let Type::Optional(inner) = self.db.ty(value.type_id).clone() else {
            self.db.error(
                ErrorKind::InvalidExistanceCheck(self.type_name(value.type_id)),
                exists.syntax().text_range(),
            );
            return self.unknown();
        };

        let exists = self.db.alloc_hir(Hir::Op(Op::Listp, value.hir_id));
        let mut new_value = Value::new(exists, self.builtins.bool);

        if let Some(guard_path) = value.guard_path {
            let mut unwrap = TypeOverride::new(inner);
            unwrap.mutation = Mutation::UnwrapOptional;
            new_value.guards.insert(
                guard_path,
                Guard::new(unwrap, TypeOverride::new(value.type_id)),
            );
        }

        new_value
    }
}
