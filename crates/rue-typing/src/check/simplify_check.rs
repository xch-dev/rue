use std::collections::VecDeque;

use crate::Check;

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

fn simplify_or_deep(items: Vec<Check>) -> Check {
    let mut result = Vec::new();
    let mut atoms: Vec<Check> = Vec::new();
    let mut pairs: Vec<Check> = Vec::new();

    let mut items: VecDeque<_> = items.into();

    let mut is_atom = false;
    let mut is_pair = false;

    while !items.is_empty() {
        let item = simplify_check(items.pop_front().unwrap());

        let mut and = Vec::new();

        match item {
            Check::None => return Check::None,
            Check::And(children) => {
                match children
                    .iter()
                    .find(|child| matches!(child, Check::IsAtom | Check::IsPair))
                {
                    Some(Check::IsAtom) => {
                        let mut children: Vec<Check> = children
                            .into_iter()
                            .filter(|child| *child != Check::IsAtom)
                            .collect();

                        if children.is_empty() {
                            result.push(Check::IsAtom);
                        } else if children.len() == 1 {
                            atoms.push(children.remove(0));
                        } else {
                            atoms.push(Check::And(children));
                        }
                    }
                    Some(Check::IsPair) => {
                        let mut children: Vec<Check> = children
                            .into_iter()
                            .filter(|child| *child != Check::IsPair)
                            .collect();

                        if children.is_empty() {
                            result.push(Check::IsPair);
                        } else if children.len() == 1 {
                            pairs.push(children.remove(0));
                        } else {
                            pairs.push(Check::And(children));
                        }
                    }
                    _ => {
                        and.extend(children);
                    }
                }
            }
            Check::Or(children) => {
                items.extend(children);
            }
            item => {
                and.push(item);
            }
        }

        let mut new_and = Vec::new();

        for and_item in and {
            match and_item {
                Check::IsAtom => {
                    if is_atom {
                        continue;
                    } else if is_pair {
                        return Check::None;
                    }
                    new_and.push(Check::IsAtom);
                    is_atom = true;
                }
                Check::IsPair => {
                    if is_pair {
                        continue;
                    } else if is_atom {
                        return Check::None;
                    }
                    new_and.push(Check::IsPair);
                    is_pair = true;
                }
                item => {
                    new_and.push(item);
                }
            }
        }

        if new_and.is_empty() {
            continue;
        }

        if new_and.len() == 1 {
            result.push(new_and.remove(0));
        } else {
            result.push(Check::And(new_and));
        }
    }

    let prefer_atoms = atoms.len() > pairs.len();

    let atoms = if atoms.is_empty() {
        Check::None
    } else if atoms.len() == 1 {
        atoms.remove(0)
    } else {
        Check::Or(atoms)
    };

    let pairs = if pairs.is_empty() {
        Check::None
    } else if pairs.len() == 1 {
        pairs.remove(0)
    } else {
        Check::Or(pairs)
    };

    if atoms != Check::None && pairs != Check::None {
        if prefer_atoms {
            result.push(Check::If(
                Box::new(Check::IsAtom),
                Box::new(atoms),
                Box::new(pairs),
            ));
        } else {
            result.push(Check::If(
                Box::new(Check::IsPair),
                Box::new(pairs),
                Box::new(atoms),
            ));
        }
    } else if atoms == Check::None && pairs != Check::None {
        result.push(Check::And(vec![Check::IsPair, pairs]));
    } else if pairs == Check::None && atoms != Check::None {
        result.push(Check::And(vec![Check::IsAtom, atoms]));
    }

    if result.len() == 1 {
        result.remove(0)
    } else {
        Check::Or(result)
    }
}

fn simplify_and_deep(items: Vec<Check>) -> Check {
    let mut items = VecDeque::from(items);

    let iter = std::iter::from_fn(|| {
        while let Some(item) = items.pop_front() {
            match simplify_check(item) {
                Check::And(children) => items.extend(children),
                item => return Some(item),
            }
        }
        None
    });

    simplify_and_shallow(iter)
}

fn simplify_and_shallow(items: impl IntoIterator<Item = Check>) -> Check {
    let mut result = Vec::new();
    let mut is_atom = false;
    let mut is_pair = false;
    let mut is_bool = false;
    let mut is_nil = false;
    let mut length = false;

    for item in items {
        match item {
            Check::None => continue,
            Check::IsAtom if is_atom => continue,
            Check::IsAtom => is_atom = true,
            Check::IsPair if is_pair => continue,
            Check::IsPair => is_pair = true,
            Check::IsBool if is_bool => continue,
            Check::IsBool => is_bool = true,
            Check::IsNil if is_nil => continue,
            Check::IsNil => is_nil = true,
            Check::Length(..) if length => continue,
            Check::Length(..) => length = true,
            _ => {}
        }
        result.push(item);
    }

    construct_and(result)
}

fn construct_and(mut items: Vec<Check>) -> Check {
    if items.is_empty() {
        Check::None
    } else if items.len() == 1 {
        items.remove(0)
    } else {
        Check::And(items)
    }
}

#[cfg(test)]
mod tests {
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
