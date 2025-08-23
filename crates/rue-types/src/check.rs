use crate::AtomRestriction;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Check {
    None,
    Impossible,
    IsAtom,
    IsPair,
    Pair(Box<Check>, Box<Check>),
    Atom(AtomRestriction),
    And(Vec<Check>),
    Or(Vec<Check>),
}

pub fn simplify_check(check: Check) -> Check {
    match check {
        Check::None => Check::None,
        Check::Impossible => Check::Impossible,
        Check::Pair(first, rest) => {
            let first = simplify_check(*first);
            let rest = simplify_check(*rest);
            Check::Pair(Box::new(first), Box::new(rest))
        }
        Check::IsAtom => Check::IsAtom,
        Check::IsPair => Check::IsPair,
        Check::Atom(restriction) => Check::Atom(restriction),
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
                    Check::Atom(AtomRestriction::Length(check)) => {
                        if length.is_some_and(|length| length != check) {
                            return Check::Impossible;
                        }
                        length = Some(check);
                    }
                    Check::Atom(AtomRestriction::Value(check)) => {
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
                    result.insert(0, Check::Atom(AtomRestriction::Value(value)));
                }
                (None, Some(value)) => {
                    result.insert(0, Check::Atom(AtomRestriction::Value(value)));
                }
                (Some(length), None) => {
                    result.insert(0, Check::Atom(AtomRestriction::Length(length)));
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
