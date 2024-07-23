use super::{simplify_and_deep, simplify_or_deep, Check};

pub(crate) fn simplify_check(check: Check) -> Check {
    match check {
        Check::None => Check::None,
        Check::IsAtom => Check::IsAtom,
        Check::IsPair => Check::IsPair,
        Check::IsBool => Check::IsBool,
        Check::IsNil => Check::IsNil,
        Check::Length(len) => {
            if len == 0 {
                Check::IsNil
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
        Check::First(first) => {
            let first = simplify_check(*first);

            if first == Check::None {
                Check::None
            } else {
                Check::First(Box::new(first))
            }
        }
        Check::Rest(rest) => {
            let rest = simplify_check(*rest);

            if rest == Check::None {
                Check::None
            } else {
                Check::Rest(Box::new(rest))
            }
        }
    }
}
