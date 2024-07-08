use rue_parser::{AstNode, ListExpr};

use crate::{
    compiler::Compiler,
    hir::Hir,
    value::{PairType, Type, Value},
    ErrorKind,
};

impl Compiler<'_> {
    pub fn compile_list_expr(&mut self, list_expr: &ListExpr) -> Value {
        let mut items = Vec::new();
        let mut rest = false;

        let len = list_expr.items().len();

        for (i, item) in list_expr.items().into_iter().enumerate() {
            let value = item
                .expr()
                .map(|expr| self.compile_expr(&expr, None))
                .unwrap_or(self.unknown());

            if item.spread().is_some() {
                if i + 1 == len {
                    rest = true;
                } else {
                    self.db
                        .error(ErrorKind::InvalidSpreadItem, item.syntax().text_range());
                }
            }

            items.push(value);
        }

        let mut hir_id = self.builtins.nil_hir;
        let mut type_id = self.builtins.nil;

        for (i, value) in items.into_iter().rev().enumerate() {
            if i == 0 && rest {
                hir_id = value.hir_id;
                type_id = value.type_id;
            } else {
                hir_id = self.db.alloc_hir(Hir::Pair(value.hir_id, hir_id));
                type_id = self.db.alloc_type(Type::Pair(PairType {
                    first: value.type_id,
                    rest: type_id,
                }));
            }
        }

        Value::new(hir_id, type_id)
    }
}
