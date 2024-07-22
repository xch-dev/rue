use std::collections::VecDeque;

use super::{simplify_check, Check};

pub(crate) fn simplify_or_deep(items: Vec<Check>) -> Check {
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
