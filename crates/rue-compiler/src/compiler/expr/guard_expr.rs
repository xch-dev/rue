use std::collections::{HashMap, HashSet};

use num_bigint::BigInt;
use rowan::TextRange;
use rue_parser::{AstNode, GuardExpr};

use crate::{
    compiler::Compiler,
    hir::{BinOp, Hir, Op},
    value::{Guard, PairType, Rest, Type, TypeOverride, Value},
    ErrorKind, HirId, TypeId,
};

enum Check {
    None,
    IsAtom,
    IsPair,
    Length(BigInt),
    And(Vec<Check>),
    Or(Vec<Check>),
}

impl Compiler<'_> {
    pub fn compile_guard_expr(
        &mut self,
        guard_expr: &GuardExpr,
        expected_type: Option<TypeId>,
    ) -> Value {
        let Some(expr) = guard_expr
            .expr()
            .map(|expr| self.compile_expr(&expr, expected_type))
        else {
            return self.unknown();
        };

        let ty = guard_expr
            .ty()
            .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

        let old_type_id = self.db.substitute_type(expr.type_id, &HashMap::new());
        let new_type_id = self.db.substitute_type(ty, &HashMap::new());

        let Some(hir_id) = self.guard_into(
            old_type_id,
            new_type_id,
            expr.hir_id,
            guard_expr.syntax().text_range(),
            &mut HashSet::new(),
        ) else {
            return Value::new(self.builtins.unknown_hir, self.builtins.bool);
        };

        let mut value = Value::new(hir_id, self.builtins.bool);

        if let Some(guard_path) = expr.guard_path {
            value.guards.insert(
                guard_path,
                Guard::new(
                    TypeOverride::new(new_type_id),
                    TypeOverride::new(old_type_id),
                ),
            );
        }

        value
    }

    fn guard_into(
        &mut self,
        old_type_id: TypeId,
        new_type_id: TypeId,
        hir_id: HirId,
        text_range: TextRange,
        visited: &mut HashSet<(TypeId, TypeId)>,
    ) -> Option<HirId> {
        if !visited.insert((old_type_id, new_type_id)) {
            self.db.error(
                ErrorKind::RecursiveTypeGuard(
                    self.type_name(old_type_id),
                    self.type_name(new_type_id),
                ),
                text_range,
            );
            return None;
        }

        if old_type_id == new_type_id {
            visited.remove(&(old_type_id, new_type_id));
            return Some(hir_id);
        }

        let result = self.guard_into_match(old_type_id, new_type_id, hir_id, text_range, visited);

        visited.remove(&(old_type_id, new_type_id));

        if result.is_none() {
            self.db.error(
                ErrorKind::UnsupportedTypeGuard(
                    self.type_name(old_type_id),
                    self.type_name(new_type_id),
                ),
                text_range,
            );
        }

        result
    }

    #[allow(clippy::match_same_arms)]
    fn guard_into_match(
        &mut self,
        old_type_id: TypeId,
        new_type_id: TypeId,
        hir_id: HirId,
        text_range: TextRange,
        visited: &mut HashSet<(TypeId, TypeId)>,
    ) -> Option<HirId> {
        match (
            self.db.ty(old_type_id).clone(),
            self.db.ty(new_type_id).clone(),
        ) {
            (Type::Ref(..), _) | (_, Type::Ref(..)) => unreachable!(),
            (Type::Alias(..), _) | (_, Type::Alias(..)) => unreachable!(),
            (Type::Lazy(..), _) | (_, Type::Lazy(..)) => unreachable!(),
            (_, Type::Generic) => None,
            (Type::Function(..), _) | (_, Type::Function(..)) => None,
            (Type::Bytes32, Type::PublicKey) | (Type::PublicKey, Type::Bytes32) => None,
            (Type::Nil, Type::PublicKey) | (Type::PublicKey, Type::Nil) => None,
            (Type::Bytes32, Type::Nil) | (Type::Nil, Type::Bytes32) => None,
            (Type::Bytes32, Type::Bool) | (Type::Bool, Type::Bytes32) => None,
            (Type::Bool, Type::PublicKey) | (Type::PublicKey, Type::Bool) => None,
            (
                Type::Pair(..),
                Type::Nil | Type::Int | Type::Bool | Type::Bytes | Type::Bytes32 | Type::PublicKey,
            )
            | (
                Type::Nil | Type::Int | Type::Bool | Type::Bytes | Type::Bytes32 | Type::PublicKey,
                Type::Pair(..),
            ) => None,
            (_, Type::Any) => Some(self.builtins.one_hir),
            (Type::Nil, Type::Bool) => Some(self.builtins.one_hir),
            (Type::Unknown, _) | (_, Type::Unknown) => Some(self.builtins.one_hir),
            (Type::PublicKey, Type::PublicKey) => Some(self.builtins.one_hir),
            (Type::Bytes32, Type::Bytes32) => Some(self.builtins.one_hir),
            (Type::Nil, Type::Nil) => Some(self.builtins.one_hir),
            (Type::Bool, Type::Bool) => Some(self.builtins.one_hir),
            (
                Type::Nil | Type::Bool | Type::Int | Type::Bytes | Type::Bytes32 | Type::PublicKey,
                Type::Bytes | Type::Int,
            ) => Some(self.builtins.one_hir),
            (
                Type::Any | Type::Generic,
                Type::Bytes32 | Type::PublicKey | Type::Nil | Type::Bool,
            ) => {
                let is_atom = self.guard_into(
                    old_type_id,
                    self.builtins.bytes,
                    hir_id,
                    text_range,
                    visited,
                )?;
                let check_value = self.guard_into(
                    self.builtins.bytes,
                    new_type_id,
                    hir_id,
                    text_range,
                    visited,
                )?;
                let and = self
                    .db
                    .alloc_hir(Hir::BinaryOp(BinOp::LogicalAnd, is_atom, check_value));
                Some(and)
            }
            (Type::Any | Type::Generic, Type::Bytes | Type::Int) => {
                let is_pair = self.db.alloc_hir(Hir::Op(Op::Listp, hir_id));
                let is_atom = self.db.alloc_hir(Hir::Op(Op::Not, is_pair));
                Some(is_atom)
            }
            (Type::Any | Type::Generic, Type::Pair(pair)) => {
                let is_any_pair = self.db.alloc_hir(Hir::Op(Op::Listp, hir_id));

                let first_hir_id = self.db.alloc_hir(Hir::Op(Op::First, hir_id));
                let rest_hir_id = self.db.alloc_hir(Hir::Op(Op::Rest, hir_id));

                let first = self.guard_into(
                    self.builtins.any,
                    pair.first,
                    first_hir_id,
                    text_range,
                    visited,
                )?;
                let rest = self.guard_into(
                    self.builtins.any,
                    pair.rest,
                    rest_hir_id,
                    text_range,
                    visited,
                )?;

                let and = self
                    .db
                    .alloc_hir(Hir::BinaryOp(BinOp::LogicalAnd, first, rest));

                let is_pair = self
                    .db
                    .alloc_hir(Hir::BinaryOp(BinOp::LogicalAnd, is_any_pair, and));

                Some(is_pair)
            }
            (Type::Bytes | Type::Int, Type::Bytes32) => {
                let strlen = self.db.alloc_hir(Hir::Op(Op::Strlen, hir_id));
                let length = self.db.alloc_hir(Hir::Atom(vec![32]));
                let is_len = self
                    .db
                    .alloc_hir(Hir::BinaryOp(BinOp::Equals, strlen, length));
                Some(is_len)
            }
            (Type::Bytes | Type::Int, Type::PublicKey) => {
                let strlen = self.db.alloc_hir(Hir::Op(Op::Strlen, hir_id));
                let length = self.db.alloc_hir(Hir::Atom(vec![48]));
                let is_len = self
                    .db
                    .alloc_hir(Hir::BinaryOp(BinOp::Equals, strlen, length));
                Some(is_len)
            }
            (Type::Bytes | Type::Int, Type::Bool) => {
                let is_zero =
                    self.db
                        .alloc_hir(Hir::BinaryOp(BinOp::Equals, hir_id, self.builtins.nil_hir));
                let one = self.db.alloc_hir(Hir::Atom(vec![1]));
                let is_one = self.db.alloc_hir(Hir::BinaryOp(BinOp::Equals, hir_id, one));
                let is_bool = self
                    .db
                    .alloc_hir(Hir::BinaryOp(BinOp::LogicalOr, is_zero, is_one));
                Some(is_bool)
            }
            (Type::Bytes | Type::Int | Type::Bool, Type::Nil) => {
                let is_nil =
                    self.db
                        .alloc_hir(Hir::BinaryOp(BinOp::Equals, hir_id, self.builtins.nil_hir));
                Some(is_nil)
            }
            (Type::Pair(old), Type::Pair(new)) => {
                let first_hir_id = self.db.alloc_hir(Hir::Op(Op::First, hir_id));
                let rest_hir_id = self.db.alloc_hir(Hir::Op(Op::Rest, hir_id));

                let first =
                    self.guard_into(old.first, new.first, first_hir_id, text_range, visited)?;
                let rest = self.guard_into(old.rest, new.rest, rest_hir_id, text_range, visited)?;

                let and = self
                    .db
                    .alloc_hir(Hir::BinaryOp(BinOp::LogicalAnd, first, rest));

                Some(and)
            }
            (
                Type::Union(items),
                new @ (Type::Nil
                | Type::Bool
                | Type::Bytes
                | Type::Bytes32
                | Type::PublicKey
                | Type::Int),
            ) => {
                todo!()
            }
            (Type::Union(items), Type::Pair(..)) => {
                todo!()
            }
            // TODO: Implement these
            (Type::Optional(..), _) | (_, Type::Optional(..)) => None,
            (Type::Struct(..), _) | (_, Type::Struct(..)) => None,
            (Type::Enum(..), _) | (_, Type::Enum(..)) => None,
            (Type::EnumVariant(..), _) | (_, Type::EnumVariant(..)) => None,
            (Type::Union(..), _) | (_, Type::Union(..)) => None,
        }
    }

    #[allow(clippy::match_same_arms)]
    fn differentiate(&self, old_type_id: TypeId, new_type_id: TypeId) -> Option<Check> {
        match (
            self.db.ty(old_type_id).clone(),
            self.db.ty(new_type_id).clone(),
        ) {
            (Type::Ref(..), _) | (_, Type::Ref(..)) => unreachable!(),
            (Type::Alias(..), _) | (_, Type::Alias(..)) => unreachable!(),
            (Type::Lazy(..), _) | (_, Type::Lazy(..)) => unreachable!(),
            (Type::Union(..), _) | (_, Type::Union(..)) => unreachable!(),
            (_, Type::Generic) => None,
            (Type::Function(..), _) | (_, Type::Function(..)) => None,
            (Type::Bytes32, Type::PublicKey) | (Type::PublicKey, Type::Bytes32) => None,
            (Type::Nil, Type::PublicKey) | (Type::PublicKey, Type::Nil) => None,
            (Type::Bytes32, Type::Nil) | (Type::Nil, Type::Bytes32) => None,
            (Type::Bytes32, Type::Bool) | (Type::Bool, Type::Bytes32) => None,
            (Type::Bool, Type::PublicKey) | (Type::PublicKey, Type::Bool) => None,
            (
                Type::Pair(..),
                Type::Nil | Type::Int | Type::Bool | Type::Bytes | Type::Bytes32 | Type::PublicKey,
            )
            | (
                Type::Nil | Type::Int | Type::Bool | Type::Bytes | Type::Bytes32 | Type::PublicKey,
                Type::Pair(..),
            ) => None,
            (_, Type::Any) => Some(Check::None),
            (Type::Nil, Type::Bool) => Some(Check::None),
            (Type::Unknown, _) | (_, Type::Unknown) => Some(Check::None),
            (Type::PublicKey, Type::PublicKey) => Some(Check::None),
            (Type::Bytes32, Type::Bytes32) => Some(Check::None),
            (Type::Nil, Type::Nil) => Some(Check::None),
            (Type::Bool, Type::Bool) => Some(Check::None),
            (
                Type::Nil | Type::Bool | Type::Int | Type::Bytes | Type::Bytes32 | Type::PublicKey,
                Type::Bytes | Type::Int,
            ) => Some(Check::None),
            (Type::Any | Type::Generic, Type::Bytes32) => Some(Check::And(vec![
                Check::IsAtom,
                Check::Length(BigInt::from(32)),
            ])),
            (Type::Any | Type::Generic, Type::PublicKey) => Some(Check::And(vec![
                Check::IsAtom,
                Check::Length(BigInt::from(48)),
            ])),
            (Type::Any | Type::Generic, Type::Nil) => Some(Check::And(vec![
                Check::IsAtom,
                Check::Length(BigInt::from(0)),
            ])),
            (Type::Any | Type::Generic, Type::Bool) => Some(Check::And(vec![
                Check::IsAtom,
                Check::Or(vec![
                    Check::Length(BigInt::from(0)),
                    Check::Length(BigInt::from(1)),
                ]),
            ])),
            (Type::Any | Type::Generic, Type::Bytes | Type::Int) => Some(Check::IsAtom),
            (Type::Any | Type::Generic, Type::Pair(..)) => Some(Check::IsPair),
            (Type::Bytes | Type::Int, Type::Bytes32) => Some(Check::Length(BigInt::from(32))),
            (Type::Bytes | Type::Int, Type::PublicKey) => Some(Check::Length(BigInt::from(48))),
            (Type::Bytes | Type::Int, Type::Bool) => Some(Check::Or(vec![
                Check::Length(BigInt::from(0)),
                Check::Length(BigInt::from(1)),
            ])),
            (Type::Bytes | Type::Int | Type::Bool, Type::Nil) => {
                Some(Check::Length(BigInt::from(0)))
            }
            (Type::Pair(..), Type::Pair(..)) => Some(Check::None),

            // TODO: Add these.
            (Type::Optional(..), _) | (_, Type::Optional(..)) => None,
            (Type::Struct(..), _) | (_, Type::Struct(..)) => None,
            (Type::Enum(..), _) | (_, Type::Enum(..)) => None,
            (Type::EnumVariant(..), _) | (_, Type::EnumVariant(..)) => None,
        }
    }
}
