use rue_parser::{AstNode, PairExpr};

use crate::{
    compiler::Compiler,
    hir::Hir,
    value::{PairType, Type, Value},
    TypeId,
};

impl Compiler<'_> {
    pub fn compile_pair_expr(&mut self, pair: &PairExpr, expected_type: Option<TypeId>) -> Value {
        // Extract the first and rest type out of the expected type.
        let first = expected_type.and_then(|type_id| self.db.first_type(type_id));
        let rest = expected_type.and_then(|type_id| self.db.rest_type(type_id));

        // Compile the first expression, if present.
        // It's a parser error if not, so it's fine to return unknown.
        let first = pair
            .first()
            .map(|expr| {
                let value = self.compile_expr(&expr, first);
                self.type_check(
                    value.type_id,
                    first.unwrap_or(self.builtins.unknown),
                    expr.syntax().text_range(),
                );
                value
            })
            .unwrap_or_else(|| self.unknown());

        // Compile the rest expression, if present.
        // It's a parser error if not, so it's fine to return unknown.
        let rest = pair
            .rest()
            .map(|expr| {
                let value = self.compile_expr(&expr, rest);
                self.type_check(
                    value.type_id,
                    rest.unwrap_or(self.builtins.unknown),
                    expr.syntax().text_range(),
                );
                value
            })
            .unwrap_or_else(|| self.unknown());

        let hir_id = self.db.alloc_hir(Hir::Pair(first.hir_id, rest.hir_id));
        let type_id = self.db.alloc_type(Type::Pair(PairType {
            first: first.type_id,
            rest: rest.type_id,
        }));

        // We throw away type guards by creating this new value.
        // They shouldn't be relevant since the type is not `Bool`.
        Value::new(hir_id, type_id)
    }
}
