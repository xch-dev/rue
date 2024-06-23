use rue_parser::{AstNode, PairExpr};

use crate::{
    compiler::Compiler,
    hir::Hir,
    ty::{PairType, Type, Value},
    TypeId,
};

impl Compiler<'_> {
    pub fn compile_pair_expr(
        &mut self,
        pair_expr: &PairExpr,
        expected_type: Option<TypeId>,
    ) -> Value {
        let expected_first = expected_type.and_then(|ty| match self.db.ty(ty) {
            Type::Pair(pair) => Some(pair.first),
            _ => None,
        });

        let expected_rest = expected_type.and_then(|ty| match self.db.ty(ty) {
            Type::Pair(pair) => Some(pair.rest),
            _ => None,
        });

        let first = if let Some(first) = pair_expr.first() {
            let value = self.compile_expr(&first, expected_first);
            self.type_check(
                value.type_id,
                expected_first.unwrap_or(self.builtins.unknown),
                first.syntax().text_range(),
            );
            value
        } else {
            self.unknown()
        };

        let rest = if let Some(rest) = pair_expr.rest() {
            let value = self.compile_expr(&rest, expected_rest);
            self.type_check(
                value.type_id,
                expected_rest.unwrap_or(self.builtins.unknown),
                rest.syntax().text_range(),
            );
            value
        } else {
            self.unknown()
        };

        let hir_id = self.db.alloc_hir(Hir::Pair(first.hir_id, rest.hir_id));
        let type_id = self.db.alloc_type(Type::Pair(PairType {
            first: first.type_id,
            rest: rest.type_id,
        }));

        Value::new(hir_id, type_id)
    }
}
