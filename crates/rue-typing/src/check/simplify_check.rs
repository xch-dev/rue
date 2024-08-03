use num_bigint::BigInt;

use super::{simplify_and_deep, simplify_or_deep, Check};

pub(crate) fn simplify_check(check: Check) -> Check {
    match check {
        Check::True => Check::True,
        Check::False => Check::False,
        Check::IsAtom => Check::IsAtom,
        Check::IsPair => Check::IsPair,
        Check::Value(value) => Check::Value(value),
        Check::Length(len) => {
            if len == 0 {
                Check::Value(BigInt::ZERO)
            } else {
                Check::Length(len)
            }
        }
        Check::And(items) => simplify_and_deep(items),
        Check::Or(items) => simplify_or_deep(items),
        Check::If(cond, then, else_) => {
            let cond = simplify_check(*cond);
            let then = simplify_check(*then);
            let else_ = simplify_check(*else_);
            Check::If(Box::new(cond), Box::new(then), Box::new(else_))
        }
        Check::First(first) => match simplify_check(*first) {
            Check::True => Check::True,
            Check::And(items) => Check::And(
                items
                    .into_iter()
                    .map(|item| Check::First(Box::new(item)))
                    .collect(),
            ),
            Check::Or(items) => Check::Or(
                items
                    .into_iter()
                    .map(|item| Check::First(Box::new(item)))
                    .collect(),
            ),
            first => Check::First(Box::new(first)),
        },
        Check::Rest(rest) => match simplify_check(*rest) {
            Check::True => Check::True,
            Check::And(items) => Check::And(
                items
                    .into_iter()
                    .map(|item| Check::Rest(Box::new(item)))
                    .collect(),
            ),
            Check::Or(items) => Check::Or(
                items
                    .into_iter()
                    .map(|item| Check::Rest(Box::new(item)))
                    .collect(),
            ),
            rest => Check::Rest(Box::new(rest)),
        },
    }
}
