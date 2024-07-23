use std::collections::HashMap;

use rue_parser::{AstNode, GuardExpr};
use rue_typing::{bigint_to_bytes, Check, Semantics, TypeId};

use crate::{
    compiler::Compiler,
    hir::{BinOp, Hir, Op},
    value::{Guard, TypeOverride, Value},
    ErrorKind, HirId,
};

impl Compiler<'_> {
    pub fn compile_guard_expr(
        &mut self,
        guard: &GuardExpr,
        expected_type: Option<TypeId>,
    ) -> Value {
        let Some(expr) = guard
            .expr()
            .map(|expr| self.compile_expr(&expr, expected_type))
        else {
            return self.unknown();
        };

        let rhs = guard
            .ty()
            .map_or(self.ty.std().unknown, |ty| self.compile_type(ty));

        let lhs = self.ty.substitute(
            expr.type_id,
            HashMap::new(),
            Semantics::StructuralOnly {
                callable: self.ty.std().any,
            },
        );

        let rhs = self.ty.substitute(
            rhs,
            HashMap::new(),
            Semantics::StructuralOnly {
                callable: self.ty.std().never,
            },
        );

        let Ok(check) = self.ty.check(lhs, rhs) else {
            self.db.error(
                ErrorKind::RecursiveTypeCheck(self.type_name(expr.type_id), self.type_name(rhs)),
                guard.syntax().text_range(),
            );
            return self.unknown();
        };

        let hir_id = self.check_hir(expr.hir_id, check);

        let mut value = Value::new(hir_id, self.ty.std().bool);

        if let Some(guard_path) = expr.guard_path {
            let difference = self.ty.difference(expr.type_id, rhs);

            value.guards.insert(
                guard_path,
                Guard::new(TypeOverride::new(rhs), TypeOverride::new(difference)),
            );
        }

        value
    }

    fn check_hir(&mut self, hir_id: HirId, check: Check) -> HirId {
        match check {
            Check::True => self.db.alloc_hir(Hir::Atom(vec![1])),
            Check::False => self.db.alloc_hir(Hir::Atom(Vec::new())),
            Check::IsAtom => {
                let listp = self.db.alloc_hir(Hir::Op(Op::Listp, hir_id));
                self.db.alloc_hir(Hir::Op(Op::Not, listp))
            }
            Check::IsPair => self.db.alloc_hir(Hir::Op(Op::Listp, hir_id)),
            Check::Value(value) => {
                let value = self.db.alloc_hir(Hir::Atom(bigint_to_bytes(value)));
                self.db
                    .alloc_hir(Hir::BinaryOp(BinOp::Equals, hir_id, value))
            }
            Check::Length(length) => {
                let strlen = self.db.alloc_hir(Hir::Op(Op::Strlen, hir_id));
                let length = self.db.alloc_hir(Hir::Atom(bigint_to_bytes(length.into())));
                self.db
                    .alloc_hir(Hir::BinaryOp(BinOp::Equals, strlen, length))
            }
            Check::If(cond, a, b) => {
                let cond = self.check_hir(hir_id, *cond);
                let a = self.check_hir(hir_id, *a);
                let b = self.check_hir(hir_id, *b);
                self.db.alloc_hir(Hir::If(cond, a, b))
            }
            Check::And(mut items) => {
                if items.is_empty() {
                    self.db.alloc_hir(Hir::Atom(vec![1]))
                } else if items.len() == 1 {
                    self.check_hir(hir_id, items.remove(0))
                } else {
                    let a = self.check_hir(hir_id, items.pop().unwrap());
                    let b = self.check_hir(hir_id, items.pop().unwrap());

                    let mut result = self.db.alloc_hir(Hir::BinaryOp(BinOp::LogicalAnd, b, a));

                    while let Some(item) = items.pop() {
                        let next = self.check_hir(hir_id, item);
                        result = self
                            .db
                            .alloc_hir(Hir::BinaryOp(BinOp::LogicalAnd, next, result));
                    }

                    result
                }
            }
            Check::Or(mut items) => {
                if items.is_empty() {
                    unreachable!()
                } else if items.len() == 1 {
                    self.check_hir(hir_id, items.remove(1))
                } else {
                    let a = self.check_hir(hir_id, items.pop().unwrap());
                    let b = self.check_hir(hir_id, items.pop().unwrap());

                    let mut result = self.db.alloc_hir(Hir::BinaryOp(BinOp::LogicalOr, b, a));

                    while let Some(item) = items.pop() {
                        let next = self.check_hir(hir_id, item);
                        result = self
                            .db
                            .alloc_hir(Hir::BinaryOp(BinOp::LogicalOr, next, result));
                    }

                    result
                }
            }
            Check::First(check) => {
                let first = self.db.alloc_hir(Hir::Op(Op::First, hir_id));
                self.check_hir(first, *check)
            }
            Check::Rest(check) => {
                let rest = self.db.alloc_hir(Hir::Op(Op::Rest, hir_id));
                self.check_hir(rest, *check)
            }
        }
    }
}
