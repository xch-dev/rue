use crate::{BinaryOp, Builtins, Database, Hir, HirId, UnaryOp};

#[derive(Debug, Clone)]
pub enum Check {
    None,
    Impossible,
    Pair(Option<Box<Check>>, Option<Box<Check>>),
    IsAtom,
    IsPair,
    Length(usize),
    Value(Vec<u8>),
    Or(Vec<Check>),
}

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
            let first = first.map(|check| generate_check_hir(db, builtins, *check, first_hir));

            let rest_hir = db.alloc_hir(Hir::Unary(UnaryOp::Rest, hir));
            let rest = rest.map(|check| generate_check_hir(db, builtins, *check, rest_hir));

            match (first, rest) {
                (Some(first), Some(rest)) => db.alloc_hir(Hir::Binary(BinaryOp::All, first, rest)),
                (Some(first), None) => first,
                (None, Some(rest)) => rest,
                (None, None) => builtins.true_value.hir,
            }
        }
        Check::IsAtom => {
            let listp = db.alloc_hir(Hir::Unary(UnaryOp::Listp, hir));
            db.alloc_hir(Hir::Unary(UnaryOp::Not, listp))
        }
        Check::IsPair => db.alloc_hir(Hir::Unary(UnaryOp::Listp, hir)),
        Check::Length(length) => {
            let strlen = db.alloc_hir(Hir::Unary(UnaryOp::Strlen, hir));
            let length = db.alloc_hir(Hir::Int(length.into()));
            db.alloc_hir(Hir::Binary(BinaryOp::Eq, strlen, length))
        }
        Check::Value(value) => {
            let value = db.alloc_hir(Hir::Bytes(value));
            db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, value))
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

                // TODO: Is Any safe here, or should we use Or?
                let mut result = db.alloc_hir(Hir::Binary(BinaryOp::Any, a, b));

                while let Some(check) = checks.pop() {
                    result = db.alloc_hir(Hir::Binary(BinaryOp::Any, result, check));
                }

                result
            }
        }
    }
}
