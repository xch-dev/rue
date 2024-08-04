use rue_parser::ListExpr;
use rue_typing::construct_items;

use crate::{Compiler, ErrorKind, Hir, Value};

impl Compiler<'_> {
    pub fn compile_list_expr(&mut self, list_expr: &ListExpr) -> Value {
        let mut items = Vec::new();
        let mut types = Vec::new();
        let mut nil_terminated = true;

        let length = list_expr.items().len();

        for (i, item) in list_expr.items().into_iter().enumerate() {
            let output = item
                .expr()
                .map(|expr| self.compile_expr(&expr, None))
                .unwrap_or(self.unknown());

            if let Some(spread) = item.spread() {
                if i + 1 == length {
                    nil_terminated = false;
                } else {
                    self.db
                        .error(ErrorKind::InvalidSpreadItem, spread.text_range());
                }
            }

            items.push(output.hir_id);
            types.push(output.type_id);
        }

        let mut hir_id = self.builtins.nil;

        for (i, item) in items.into_iter().rev().enumerate() {
            if i == 0 && !nil_terminated {
                hir_id = item;
            } else {
                hir_id = self.db.alloc_hir(Hir::Pair(item, hir_id));
            }
        }

        let type_id = construct_items(self.ty, types.into_iter(), nil_terminated);

        Value::new(hir_id, type_id)
    }
}
