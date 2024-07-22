use std::collections::VecDeque;

use super::{simplify_check, Check};

pub(crate) fn simplify_and_deep(items: Vec<Check>) -> Check {
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

pub(crate) fn simplify_and_shallow(items: impl IntoIterator<Item = Check>) -> Check {
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
