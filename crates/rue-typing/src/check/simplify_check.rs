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
        Check::Pair(first, rest) => {
            let first = simplify_check(*first);
            let rest = simplify_check(*rest);

            if first == Check::None && rest == Check::None {
                Check::None
            } else {
                Check::Pair(Box::new(first), Box::new(rest))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::simplify_and_shallow;

    use super::*;

    #[test]
    fn simplify_and_none() {
        assert_eq!(simplify_and_shallow(vec![Check::None]), Check::None);
    }

    #[test]
    fn simplify_none_and_none() {
        assert_eq!(
            simplify_and_shallow(vec![Check::None, Check::None]),
            Check::None
        );
    }

    #[test]
    fn simplify_check_and_none() {
        assert_eq!(
            simplify_and_shallow(vec![Check::None, Check::IsAtom]),
            Check::IsAtom
        );
        assert_eq!(
            simplify_and_shallow(vec![Check::IsAtom, Check::None]),
            Check::IsAtom
        );
    }

    #[test]
    fn simplify_and_one_check() {
        assert_eq!(simplify_and_shallow(vec![Check::IsAtom]), Check::IsAtom);
    }

    #[test]
    fn simplify_atom_and_atom() {
        assert_eq!(
            simplify_and_shallow(vec![Check::IsAtom, Check::IsAtom]),
            Check::IsAtom
        );
    }

    #[test]
    fn simplify_atom_and_pair() {
        assert_eq!(
            simplify_and_shallow(vec![Check::IsAtom, Check::IsPair]),
            Check::And(vec![Check::IsAtom, Check::IsPair])
        );
    }

    #[test]
    fn simplify_pair_and_pair() {
        assert_eq!(
            simplify_and_shallow(vec![Check::IsPair, Check::IsPair]),
            Check::IsPair
        );
    }

    #[test]
    fn simplify_pair_and_atom() {
        assert_eq!(
            simplify_and_shallow(vec![Check::IsPair, Check::IsAtom]),
            Check::And(vec![Check::IsPair, Check::IsAtom])
        );
    }
}
