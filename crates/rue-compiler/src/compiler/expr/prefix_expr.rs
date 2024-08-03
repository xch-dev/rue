use rue_parser::{AstNode, PrefixExpr, PrefixOp};

use crate::{
    compiler::Compiler,
    hir::{BinOp, Hir, Op},
    value::Value,
};

impl Compiler<'_> {
    pub fn compile_prefix_expr(&mut self, prefix_expr: &PrefixExpr) -> Value {
        // Determine the expected type based on the prefix operator.
        let expected_type = match prefix_expr.op() {
            Some(PrefixOp::BitwiseNot | PrefixOp::Positive | PrefixOp::Negative) => {
                Some(self.ty.std().int)
            }
            Some(PrefixOp::Not) => Some(self.ty.std().bool),
            None => None,
        };

        // Compile the expression.
        let expr = prefix_expr
            .expr()
            .map(|expr| self.compile_expr(&expr, None))
            .unwrap_or_else(|| self.unknown());

        // If the expression has no prefix operator, return the expression as is.
        // This is likely a parser error of some sort (in fact, it shouldn't happen).
        let Some(op) = prefix_expr.op() else {
            return expr;
        };

        // Check the type of the expression.
        self.type_check(
            expr.type_id,
            expected_type.unwrap_or(self.ty.std().unknown),
            prefix_expr
                .expr()
                .map_or(prefix_expr.syntax().text_range(), |ast| {
                    ast.syntax().text_range()
                }),
        );

        match op {
            PrefixOp::Not => {
                // Negate the expression and its type guards.
                let mut value = Value::new(
                    self.db.alloc_hir(Hir::Op(Op::Not, expr.hir_id)),
                    self.ty.std().bool,
                );

                for (symbol_id, guard) in expr.guards {
                    value.guards.insert(symbol_id, !guard);
                }

                value
            }
            PrefixOp::Negative => Value::new(
                // Subtract the expression from nil.
                self.db.alloc_hir(Hir::BinaryOp(
                    BinOp::Subtract,
                    self.builtins.nil,
                    expr.hir_id,
                )),
                self.ty.std().int,
            ),
            PrefixOp::Positive => {
                // Return the expression as is.
                expr
            }
            PrefixOp::BitwiseNot => {
                // Negate the expression and its type guards.
                Value::new(
                    self.db.alloc_hir(Hir::Op(Op::BitwiseNot, expr.hir_id)),
                    self.ty.std().int,
                )
            }
        }
    }
}
