use rue_types::{AtomRestriction, Check};

use crate::{BinaryOp, Builtins, Database, Hir, HirId, UnaryOp};

pub fn generate_check_hir(
    db: &mut Database,
    builtins: &Builtins,
    check: Check,
    hir: HirId,
) -> HirId {
    match check {
        Check::None => builtins.true_value.hir,
        Check::Impossible => builtins.false_value.hir,
        Check::Pair(first, rest) => {
            let first_hir = db.alloc_hir(Hir::Unary(UnaryOp::First, hir));
            let first = (first.as_ref() != &Check::None)
                .then_some(generate_check_hir(db, builtins, *first, first_hir));

            let rest_hir = db.alloc_hir(Hir::Unary(UnaryOp::Rest, hir));
            let rest = (rest.as_ref() != &Check::None)
                .then_some(generate_check_hir(db, builtins, *rest, rest_hir));

            match (first, rest) {
                (Some(first), Some(rest)) => db.alloc_hir(Hir::Binary(BinaryOp::And, first, rest)),
                (Some(first), None) => first,
                (None, Some(rest)) => rest,
                (None, None) => builtins.true_value.hir,
            }
        }
        Check::IsAtom { can_be_truthy } => {
            let listp = db.alloc_hir(Hir::Unary(UnaryOp::Listp { can_be_truthy }, hir));
            db.alloc_hir(Hir::Unary(UnaryOp::Not, listp))
        }
        Check::IsPair { can_be_truthy } => {
            db.alloc_hir(Hir::Unary(UnaryOp::Listp { can_be_truthy }, hir))
        }
        Check::Atom(AtomRestriction::Length(length)) => {
            let strlen = db.alloc_hir(Hir::Unary(UnaryOp::Strlen, hir));
            let length = db.alloc_hir(Hir::Int(length.into()));
            db.alloc_hir(Hir::Binary(BinaryOp::Eq, strlen, length))
        }
        Check::Atom(AtomRestriction::Value(value)) => {
            let value = db.alloc_hir(Hir::Bytes(value.to_vec()));
            db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, value))
        }
        Check::And(checks) => {
            let mut checks = checks
                .into_iter()
                .map(|check| generate_check_hir(db, builtins, check, hir))
                .collect::<Vec<_>>();

            if checks.is_empty() {
                builtins.true_value.hir
            } else if checks.len() == 1 {
                checks[0]
            } else {
                let b = checks.pop().unwrap();
                let a = checks.pop().unwrap();

                let mut result = db.alloc_hir(Hir::Binary(BinaryOp::And, a, b));

                while let Some(check) = checks.pop() {
                    result = db.alloc_hir(Hir::Binary(BinaryOp::And, result, check));
                }

                result
            }
        }
        Check::Or(checks) => {
            let mut checks = checks
                .into_iter()
                .map(|check| generate_check_hir(db, builtins, check, hir))
                .collect::<Vec<_>>();

            if checks.is_empty() {
                builtins.false_value.hir
            } else if checks.len() == 1 {
                checks[0]
            } else {
                let b = checks.pop().unwrap();
                let a = checks.pop().unwrap();

                let mut result = db.alloc_hir(Hir::Binary(BinaryOp::Or, a, b));

                while let Some(check) = checks.pop() {
                    result = db.alloc_hir(Hir::Binary(BinaryOp::Or, result, check));
                }

                result
            }
        }
    }
}
