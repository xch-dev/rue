use rowan::TextRange;
use rue_parser::{AstNode, GuardExpr};

use crate::{
    compiler::Compiler,
    hir::{BinOp, Hir, Op},
    value::{Guard, PairType, Type, TypeOverride, Value},
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

        if let Some(guard_path) = expr.guard_path {
            value.guards.insert(guard_path, guard);
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
            return Some((
                Guard::new(TypeOverride::new(to), TypeOverride::new(self.builtins.bool)),
                hir_id,
            ));
        }

        match (self.db.ty(from).clone(), self.db.ty(to).clone()) {
            (Type::Any, Type::Pair(PairType { first, rest })) => {
                if !self.db.compare_type(first, self.builtins.any).is_equal() {
                    self.db.error(ErrorKind::NonAnyPairTypeGuard, text_range);
                }

                if !self.db.compare_type(rest, self.builtins.any).is_equal() {
                    self.db.error(ErrorKind::NonAnyPairTypeGuard, text_range);
                }

                let hir_id = self.db.alloc_hir(Hir::Op(Op::Listp, hir_id));
                Some((
                    Guard::new(
                        TypeOverride::new(to),
                        TypeOverride::new(self.builtins.bytes),
                    ),
                    hir_id,
                ))
            }
            (Type::Any, Type::Bytes) => {
                let pair_type = self.db.alloc_type(Type::Pair(PairType {
                    first: self.builtins.any,
                    rest: self.builtins.any,
                }));
                let is_cons = self.db.alloc_hir(Hir::Op(Op::Listp, hir_id));
                let hir_id = self.db.alloc_hir(Hir::Op(Op::Not, is_cons));
                Some((
                    Guard::new(TypeOverride::new(to), TypeOverride::new(pair_type)),
                    hir_id,
                ))
            }
            (Type::List(inner), Type::Pair(PairType { first, rest })) => {
                if !self.db.compare_type(first, inner).is_equal() {
                    self.db.error(ErrorKind::NonListPairTypeGuard, text_range);
                }

                if !self.db.compare_type(rest, from).is_equal() {
                    self.db.error(ErrorKind::NonListPairTypeGuard, text_range);
                }

                let hir_id = self.db.alloc_hir(Hir::Op(Op::Listp, hir_id));
                Some((
                    Guard::new(TypeOverride::new(to), TypeOverride::new(self.builtins.nil)),
                    hir_id,
                ))
            }
            (Type::List(inner), Type::Nil) => {
                let pair_type = self.db.alloc_type(Type::Pair(PairType {
                    first: inner,
                    rest: from,
                }));
                let is_cons = self.db.alloc_hir(Hir::Op(Op::Listp, hir_id));
                let hir_id = self.db.alloc_hir(Hir::Op(Op::Not, is_cons));
                Some((
                    Guard::new(TypeOverride::new(to), TypeOverride::new(pair_type)),
                    hir_id,
                ))
            }
            (Type::Bytes, Type::Bytes32) => {
                let strlen = self.db.alloc_hir(Hir::Op(Op::Strlen, hir_id));
                let length = self.db.alloc_hir(Hir::Atom(vec![32]));
                let hir_id = self
                    .db
                    .alloc_hir(Hir::BinaryOp(BinOp::Equals, strlen, length));
                Some((
                    Guard::new(TypeOverride::new(to), TypeOverride::new(from)),
                    hir_id,
                ))
            }
            (Type::Bytes, Type::PublicKey) => {
                let strlen = self.db.alloc_hir(Hir::Op(Op::Strlen, hir_id));
                let length = self.db.alloc_hir(Hir::Atom(vec![48]));
                let hir_id = self
                    .db
                    .alloc_hir(Hir::BinaryOp(BinOp::Equals, strlen, length));
                Some((
                    Guard::new(TypeOverride::new(to), TypeOverride::new(from)),
                    hir_id,
                ))
            }
            (Type::Enum(enum_type), Type::EnumVariant(variant_type)) => {
                if variant_type.enum_type != from {
                    self.db.error(
                        ErrorKind::UnsupportedTypeGuard(self.type_name(from), self.type_name(to)),
                        text_range,
                    );
                    return None;
                }

                let hir_id = if enum_type.has_fields {
                    let first = self.db.alloc_hir(Hir::Op(Op::First, hir_id));
                    self.db.alloc_hir(Hir::BinaryOp(
                        BinOp::Equals,
                        first,
                        variant_type.discriminant,
                    ))
                } else {
                    self.db.alloc_hir(Hir::BinaryOp(
                        BinOp::Equals,
                        hir_id,
                        variant_type.discriminant,
                    ))
                };

                Some((
                    Guard::new(TypeOverride::new(to), TypeOverride::new(from)),
                    hir_id,
                ))
            }
            (Type::Int, Type::EnumVariant(variant_type)) => {
                let Type::Enum(enum_type) = self.db.ty(variant_type.enum_type).clone() else {
                    self.db.error(
                        ErrorKind::UnsupportedTypeGuard(self.type_name(from), self.type_name(to)),
                        text_range,
                    );
                    return None;
                };

                if enum_type.has_fields {
                    self.db.error(
                        ErrorKind::UnsupportedTypeGuard(self.type_name(from), self.type_name(to)),
                        text_range,
                    );
                    return None;
                }

                let hir_id = self.db.alloc_hir(Hir::BinaryOp(
                    BinOp::Equals,
                    hir_id,
                    variant_type.discriminant,
                ));

                Some((
                    Guard::new(TypeOverride::new(to), TypeOverride::new(from)),
                    hir_id,
                ))
            }
            _ => {
                self.db.error(
                    ErrorKind::UnsupportedTypeGuard(self.type_name(from), self.type_name(to)),
                    text_range,
                );
                None
            }
        }
    }
}
