use rue_parser::{AstNode, ListExpr};

use crate::{
    compiler::Compiler,
    hir::Hir,
    ty::{Type, Value},
    ErrorKind, TypeId,
};

impl Compiler<'_> {
    pub fn compile_list_expr(
        &mut self,
        list_expr: &ListExpr,
        expected_expr_type: Option<TypeId>,
    ) -> Value {
        let mut items = Vec::new();
        let mut nil_terminated = true;

        let mut list_type = expected_expr_type;
        let mut item_type = expected_expr_type.and_then(|ty| match self.db.ty(ty) {
            Type::List(ty) => Some(*ty),
            _ => None,
        });

        let len = list_expr.items().len();

        for (i, item) in list_expr.items().into_iter().enumerate() {
            let expected_item_type = if item.spread().is_some() {
                list_type
            } else {
                item_type
            };

            let output = item
                .expr()
                .map(|expr| self.compile_expr(&expr, expected_item_type))
                .unwrap_or(self.unknown());

            if let Some(expected_item_type) = expected_item_type {
                self.type_check(
                    output.type_id,
                    expected_item_type,
                    item.syntax().text_range(),
                );
            }

            if i == 0 && item_type.is_none() {
                if item.spread().is_some() {
                    list_type = Some(output.type_id);
                    item_type = match self.db.ty(output.type_id) {
                        Type::List(ty) => Some(*ty),
                        _ => None,
                    };
                } else {
                    list_type = Some(self.db.alloc_type(Type::List(output.type_id)));
                    item_type = Some(output.type_id);
                }
            }

            if let Some(spread) = item.spread() {
                if i + 1 == len {
                    nil_terminated = false;
                } else {
                    self.db
                        .error(ErrorKind::NonFinalSpread, spread.text_range());
                }
            }

            items.push(output.hir_id);
        }

        let mut hir_id = self.builtins.nil_hir;

        for (i, item) in items.into_iter().rev().enumerate() {
            if i == 0 && !nil_terminated {
                hir_id = item;
            } else {
                hir_id = self.db.alloc_hir(Hir::Pair(item, hir_id));
            }
        }

        Value::new(
            hir_id,
            self.db
                .alloc_type(Type::List(item_type.unwrap_or(self.builtins.unknown))),
        )
    }
}
