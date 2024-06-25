use rowan::TextRange;
use rue_parser::{AstNode, GuardExpr};

use crate::{
    compiler::Compiler,
    hir::{BinOp, Hir},
    value::{Guard, PairType, Type, Value},
    Comparison, ErrorKind, HirId, TypeId, WarningKind,
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

        let ty = guard
            .ty()
            .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

        let Some((guard, hir_id)) =
            self.guard_into(expr.type_id, ty, expr.hir_id, guard.syntax().text_range())
        else {
            return Value::new(self.builtins.unknown_hir, ty);
        };

        let mut value = Value::new(hir_id, self.builtins.bool);

        if let Hir::Reference(symbol_id) = self.db.hir(expr.hir_id) {
            value.guards.insert(*symbol_id, guard);
        }

        value
    }

    fn guard_into(
        &mut self,
        from: TypeId,
        to: TypeId,
        hir_id: HirId,
        text_range: TextRange,
    ) -> Option<(Guard, HirId)> {
        if self.db.compare_type(from, to) <= Comparison::Assignable {
            self.db.warning(
                WarningKind::RedundantTypeCheck(self.type_name(from)),
                text_range,
            );
            return Some((Guard::new(to, self.builtins.bool), hir_id));
        }

        match (self.db.ty(from).clone(), self.db.ty(to).clone()) {
            (Type::Any, Type::Pair(PairType { first, rest })) => {
                if !self.db.compare_type(first, self.builtins.any).is_equal() {
                    self.db.error(ErrorKind::NonAnyPairTypeGuard, text_range);
                }

                if !self.db.compare_type(rest, self.builtins.any).is_equal() {
                    self.db.error(ErrorKind::NonAnyPairTypeGuard, text_range);
                }

                let hir_id = self.db.alloc_hir(Hir::IsCons(hir_id));
                Some((Guard::new(to, self.builtins.bytes), hir_id))
            }
            (Type::Any, Type::Bytes) => {
                let pair_type = self.db.alloc_type(Type::Pair(PairType {
                    first: self.builtins.any,
                    rest: self.builtins.any,
                }));
                let is_cons = self.db.alloc_hir(Hir::IsCons(hir_id));
                let hir_id = self.db.alloc_hir(Hir::Not(is_cons));
                Some((Guard::new(to, pair_type), hir_id))
            }
            (Type::List(inner), Type::Pair(PairType { first, rest })) => {
                if !self.db.compare_type(first, inner).is_equal() {
                    self.db.error(ErrorKind::NonListPairTypeGuard, text_range);
                }

                if !self.db.compare_type(rest, from).is_equal() {
                    self.db.error(ErrorKind::NonListPairTypeGuard, text_range);
                }

                let hir_id = self.db.alloc_hir(Hir::IsCons(hir_id));
                Some((Guard::new(to, self.builtins.nil), hir_id))
            }
            (Type::List(inner), Type::Nil) => {
                let pair_type = self.db.alloc_type(Type::Pair(PairType {
                    first: inner,
                    rest: from,
                }));
                let is_cons = self.db.alloc_hir(Hir::IsCons(hir_id));
                let hir_id = self.db.alloc_hir(Hir::Not(is_cons));
                Some((Guard::new(to, pair_type), hir_id))
            }
            (Type::Bytes, Type::Bytes32) => {
                let strlen = self.db.alloc_hir(Hir::Strlen(hir_id));
                let length = self.db.alloc_hir(Hir::Atom(vec![32]));
                let hir_id = self.db.alloc_hir(Hir::BinaryOp {
                    op: BinOp::Equals,
                    lhs: strlen,
                    rhs: length,
                });
                Some((Guard::new(to, from), hir_id))
            }
            (Type::Bytes, Type::PublicKey) => {
                let strlen = self.db.alloc_hir(Hir::Strlen(hir_id));
                let length = self.db.alloc_hir(Hir::Atom(vec![48]));
                let hir_id = self.db.alloc_hir(Hir::BinaryOp {
                    op: BinOp::Equals,
                    lhs: strlen,
                    rhs: length,
                });
                Some((Guard::new(to, from), hir_id))
            }
            _ => {
                self.db.error(
                    ErrorKind::UnsupportedTypeGuard {
                        from: self.type_name(from),
                        to: self.type_name(to),
                    },
                    text_range,
                );
                None
            }
        }
    }
}
