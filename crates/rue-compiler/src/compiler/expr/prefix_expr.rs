use rue_parser::{AstNode, PrefixExpr, PrefixOp};

use crate::{
    compiler::Compiler,
    hir::{BinOp, Hir},
    ty::Value,
};

impl Compiler<'_> {
    pub fn compile_prefix_expr(&mut self, prefix_expr: &PrefixExpr) -> Value {
        let Some(expr) = prefix_expr.expr() else {
            return self.unknown();
        };

        let expr = self.compile_expr(&expr, None);

        let Some(op) = prefix_expr.op() else {
            return self.unknown();
        };

        match op {
            PrefixOp::Not => {
                self.type_check(
                    expr.type_id,
                    self.builtins.bool,
                    prefix_expr.syntax().text_range(),
                );

                let mut value =
                    Value::new(self.db.alloc_hir(Hir::Not(expr.hir_id)), self.builtins.bool);

                for (symbol_id, guard) in expr.guards {
                    value.guards.insert(symbol_id, !guard);
                }

                value
            }
            PrefixOp::Neg => {
                self.type_check(
                    expr.type_id,
                    self.builtins.int,
                    prefix_expr.syntax().text_range(),
                );

                Value::new(
                    self.db.alloc_hir(Hir::BinaryOp {
                        op: BinOp::Subtract,
                        lhs: self.builtins.nil_hir,
                        rhs: expr.hir_id,
                    }),
                    self.builtins.int,
                )
            }
        }
    }
}
