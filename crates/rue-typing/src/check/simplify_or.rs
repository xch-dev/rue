use std::collections::VecDeque;

use super::{construct_and, simplify_check, Check};

enum ShapeCheck {
    None,
    Any,
    Or(Check),
}

pub(crate) fn simplify_or_deep(items: Vec<Check>) -> Check {
    let mut items = VecDeque::from(items);

    let iter = std::iter::from_fn(|| {
        while let Some(item) = items.pop_front() {
            match simplify_check(item) {
                Check::Or(children) => items.extend(children),
                item => return Some(item),
            }
        }
        None
    });

    simplify_or_shallow(iter)
}

pub(crate) fn simplify_or_shallow(items: impl IntoIterator<Item = Check>) -> Check {
    let mut result = Vec::new();

    let mut atom_checks: Vec<Check> = Vec::new();
    let mut pair_checks: Vec<Check> = Vec::new();

    let mut any_atom = false;
    let mut any_pair = false;

    for item in items {
        match item {
            Check::None => return Check::None,
            Check::IsAtom if any_atom => continue,
            Check::IsPair if any_pair => continue,
            Check::IsAtom => {
                any_atom = true;
                continue;
            }
            Check::IsPair => {
                any_pair = true;
                continue;
            }
            Check::And(children) => {
                let (shape, checks) = extract_shape_check(children);
                match shape {
                    Some(Check::IsAtom) => atom_checks.push(checks),
                    Some(Check::IsPair) => pair_checks.push(checks),
                    _ => result.push(checks),
                }
                continue;
            }
            item => result.push(item),
        }
    }

    let prefer_atom = atom_checks.len() > pair_checks.len();

    let atom_check = if any_atom {
        ShapeCheck::Any
    } else if atom_checks.is_empty() {
        ShapeCheck::None
    } else {
        ShapeCheck::Or(construct_or(atom_checks))
    };

    let pair_check = if any_pair {
        ShapeCheck::Any
    } else if pair_checks.is_empty() {
        ShapeCheck::None
    } else {
        ShapeCheck::Or(construct_or(pair_checks))
    };

    match (atom_check, pair_check) {
        (ShapeCheck::None, ShapeCheck::None) => {}
        (ShapeCheck::Any, ShapeCheck::Any) => {
            return Check::None;
        }
        (ShapeCheck::Any, ShapeCheck::None) => {
            result.push(Check::IsAtom);
        }
        (ShapeCheck::None, ShapeCheck::Any) => {
            result.push(Check::IsPair);
        }
        (ShapeCheck::Or(atom_check), ShapeCheck::None) => {
            result.push(construct_and(vec![Check::IsAtom, atom_check]));
        }
        (ShapeCheck::None, ShapeCheck::Or(pair_check)) => {
            result.push(construct_and(vec![Check::IsPair, pair_check]));
        }
        (ShapeCheck::Or(atom_check), ShapeCheck::Any) => {
            result.push(Check::IsPair);
            result.push(atom_check);
        }
        (ShapeCheck::Any, ShapeCheck::Or(pair_check)) => {
            result.push(Check::IsAtom);
            result.push(pair_check);
        }
        (ShapeCheck::Or(atom_check), ShapeCheck::Or(pair_check)) => {
            if prefer_atom {
                result.push(Check::If(
                    Box::new(Check::IsAtom),
                    Box::new(atom_check),
                    Box::new(pair_check),
                ));
            } else {
                result.push(Check::If(
                    Box::new(Check::IsPair),
                    Box::new(pair_check),
                    Box::new(atom_check),
                ));
            }
        }
    }

    construct_or(result)
}

fn extract_shape_check(items: Vec<Check>) -> (Option<Check>, Check) {
    let mut result = Vec::new();
    let mut shape = None;

    for item in items {
        match item {
            Check::IsAtom => shape = Some(Check::IsAtom),
            Check::IsPair => shape = Some(Check::IsPair),
            _ => result.push(item),
        }
    }

    (shape, construct_and(result))
}

fn construct_or(mut items: Vec<Check>) -> Check {
    if items.is_empty() {
        unreachable!()
    } else if items.len() == 1 {
        items.remove(0)
    } else {
        Check::Or(items)
    }
}

#[cfg(test)]
mod tests {
    use num_bigint::BigInt;

    use super::*;

    #[test]
    fn test_simplify_or_none() {
        assert_eq!(simplify_or_shallow([Check::None]), Check::None);
    }

    #[test]
    fn test_simplify_none_or_none() {
        assert_eq!(simplify_or_shallow([Check::None, Check::None]), Check::None);
    }

    #[test]
    fn test_simplify_check_or_none() {
        assert_eq!(
            simplify_or_shallow([Check::IsAtom, Check::None]),
            Check::None
        );
    }

    #[test]
    fn test_simplify_none_or_check() {
        assert_eq!(
            simplify_or_shallow([Check::None, Check::IsAtom]),
            Check::None
        );
    }

    #[test]
    fn test_simplify_or_one_check() {
        assert_eq!(simplify_or_shallow([Check::IsAtom]), Check::IsAtom);
    }

    #[test]
    fn test_simplify_or_two_checks() {
        assert_eq!(
            simplify_or_shallow([Check::IsPair, Check::Value(BigInt::ZERO)]),
            Check::Or(vec![Check::Value(BigInt::ZERO), Check::IsPair])
        );
    }

    #[test]
    fn test_simplify_atom_or_pair() {
        assert_eq!(
            simplify_or_shallow([Check::IsAtom, Check::IsPair]),
            Check::None
        );
    }

    #[test]
    fn test_simplify_pair_or_atom() {
        assert_eq!(
            simplify_or_shallow([Check::IsPair, Check::IsAtom]),
            Check::None
        );
    }

    #[test]
    fn test_simplify_atom_or_atom() {
        assert_eq!(
            simplify_or_shallow([Check::IsAtom, Check::IsAtom]),
            Check::IsAtom
        );
    }

    #[test]
    fn test_simplify_pair_or_pair() {
        assert_eq!(
            simplify_or_shallow([Check::IsPair, Check::IsPair]),
            Check::IsPair
        );
    }

    #[test]
    fn test_simplify_compound_atom_or_pair() {
        assert_eq!(
            simplify_or_shallow([
                Check::And(vec![Check::IsAtom, Check::Length(32)]),
                Check::IsPair
            ]),
            Check::Or(vec![Check::IsPair, Check::Length(32)])
        );
    }
}
