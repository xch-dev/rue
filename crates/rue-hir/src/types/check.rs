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
    And(Vec<Check>),
    Or(Vec<Check>),
}

pub fn simplify_check(check: Check) -> Check {
    match check {
        Check::None => Check::None,
        Check::Impossible => Check::Impossible,
        Check::Pair(first, rest) => {
            let first = first.map(|check| simplify_check(*check));
            let rest = rest.map(|check| simplify_check(*check));
            Check::Pair(first.map(Box::new), rest.map(Box::new))
        }
        Check::IsAtom => Check::IsAtom,
        Check::IsPair => Check::IsPair,
        Check::Length(length) => Check::Length(length),
        Check::Value(value) => Check::Value(value),
        Check::And(checks) => {
            let mut flattened = Vec::new();

            for check in checks {
                match simplify_check(check) {
                    Check::None => {}
                    Check::Impossible => {
                        return Check::Impossible;
                    }
                    Check::And(inner) => {
                        flattened.extend(inner);
                    }
                    check => {
                        flattened.push(check);
                    }
                }
            }

            let mut listp = None;
            let mut length = None;
            let mut value = None;
            let mut result = Vec::new();

            for check in flattened {
                match check {
                    Check::None | Check::Impossible | Check::And(_) => unreachable!(),
                    Check::IsAtom => {
                        if listp == Some(true) {
                            return Check::Impossible;
                        }
                        listp = Some(false);
                    }
                    Check::IsPair => {
                        if listp == Some(false) {
                            return Check::Impossible;
                        }
                        listp = Some(true);
                    }
                    Check::Length(check) => {
                        if length.is_some_and(|length| length != check) {
                            return Check::Impossible;
                        }
                        length = Some(check);
                    }
                    Check::Value(check) => {
                        if value.is_some_and(|value| value != check) {
                            return Check::Impossible;
                        }
                        value = Some(check);
                    }
                    check @ (Check::Or(_) | Check::Pair(..)) => {
                        result.push(check);
                    }
                }
            }

            match (length, value) {
                (Some(length), Some(value)) => {
                    if length != value.len() {
                        return Check::Impossible;
                    }
                    result.insert(0, Check::Value(value));
                }
                (None, Some(value)) => {
                    result.insert(0, Check::Value(value));
                }
                (Some(length), None) => {
                    result.insert(0, Check::Length(length));
                }
                (None, None) => {}
            }

            match listp {
                Some(true) => result.insert(0, Check::IsPair),
                Some(false) => result.insert(0, Check::IsAtom),
                None => {}
            }

            if result.is_empty() {
                Check::None
            } else if result.len() == 1 {
                result[0].clone()
            } else {
                Check::And(result)
            }
        }
        Check::Or(checks) => Check::Or(checks.into_iter().map(simplify_check).collect()),
    }
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
