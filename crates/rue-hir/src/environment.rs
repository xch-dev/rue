use std::{cmp::Reverse, mem};

use indexmap::IndexMap;
use num_bigint::BigUint;

use crate::SymbolId;

/// A sequential group is a chain of pairs, with one level for each symbol, so an environment can be
/// thousands of levels deep. Everything that walks it, including [`Clone`] and [`Drop`], is
/// iterative rather than recursing once per level.
#[derive(Debug, Default)]
#[must_use]
pub enum Environment {
    #[default]
    Nil,
    Leaf(SymbolId),
    Pair(Box<Environment>, Box<Environment>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PathError {
    SymbolNotFound,
}

impl Environment {
    pub fn sequential(symbols: &[SymbolId], nil_terminated: bool) -> Self {
        let mut result = Self::Nil;

        for &symbol in symbols.iter().rev() {
            if matches!(result, Self::Nil) && !nil_terminated {
                result = Self::Leaf(symbol);
                continue;
            }

            result = Self::Pair(Box::new(Self::Leaf(symbol)), Box::new(result));
        }

        result
    }

    pub fn tree(referenced_symbols: IndexMap<SymbolId, usize>) -> Self {
        if referenced_symbols.is_empty() {
            return Self::Nil;
        }

        if referenced_symbols.len() == 1 {
            return Self::Leaf(*referenced_symbols.keys().next().unwrap());
        }

        // Sort symbols by reference count in descending order
        let mut symbols: Vec<_> = referenced_symbols.into_iter().collect();
        symbols.sort_by_key(|b| Reverse(b.1));

        // Split symbols into two groups, trying to balance:
        // 1. Total reference counts between groups
        // 2. More frequently referenced symbols in the first group (shorter paths)
        let mut split_idx = symbols.len().div_ceil(2);
        let mut min_diff = i64::MAX;

        for i in 1..symbols.len() {
            let mut new_first_sum = 0usize;
            let mut new_rest_sum = 0usize;

            for (j, (_, count)) in symbols.iter().enumerate() {
                if j < i {
                    new_first_sum += *count;
                } else {
                    new_rest_sum += *count;
                }
            }

            #[allow(clippy::cast_possible_wrap)]
            let diff = (new_first_sum as i64 - new_rest_sum as i64).abs();
            if diff < min_diff {
                min_diff = diff;
                split_idx = i;
            }
        }

        // Create first and rest groups
        let first_group: IndexMap<_, _> = symbols
            .iter()
            .take(split_idx)
            .map(|(k, v)| (*k, *v))
            .collect();
        let rest_group: IndexMap<_, _> = symbols
            .iter()
            .skip(split_idx)
            .map(|(k, v)| (*k, *v))
            .collect();

        // Recursively build the tree
        Self::Pair(
            Box::new(Self::tree(first_group)),
            Box::new(Self::tree(rest_group)),
        )
    }

    pub fn path(&self, symbol_id: SymbolId) -> Result<BigUint, PathError> {
        let ops = self
            .get_pair_ops(symbol_id)
            .ok_or(PathError::SymbolNotFound)?;

        // The first operation is the lowest bit, and the path ends with a 1 above the last one
        let mut path = BigUint::default();
        path.set_bit(ops.len() as u64, true);

        for (i, op) in ops.iter().enumerate() {
            if matches!(op, PairOp::Rest) {
                path.set_bit(i as u64, true);
            }
        }

        Ok(path)
    }

    /// The operations that lead from the root to the symbol, searching the first of each pair before
    /// the rest.
    fn get_pair_ops(&self, symbol_id: SymbolId) -> Option<Vec<PairOp>> {
        let mut ops = Vec::new();

        // Each environment left to search, along with how many operations lead to its parent, and the
        // operation that leads from the parent to it
        let mut pending = vec![(self, 0, None)];

        while let Some((env, depth, op)) = pending.pop() {
            ops.truncate(depth);
            ops.extend(op);

            match env {
                Self::Nil => {}
                Self::Leaf(leaf) => {
                    if *leaf == symbol_id {
                        return Some(ops);
                    }
                }
                Self::Pair(first, rest) => {
                    pending.push((rest, ops.len(), Some(PairOp::Rest)));
                    pending.push((first, ops.len(), Some(PairOp::First)));
                }
            }
        }

        None
    }

    /// Moves the children of a pair that are pairs themselves into `pairs`, leaving [`Self::Nil`] in
    /// their place, so that dropping the pair doesn't recurse into them.
    fn take_child_pairs(&mut self, pairs: &mut Vec<Self>) {
        if let Self::Pair(first, rest) = self {
            for child in [first, rest] {
                if matches!(**child, Self::Pair(..)) {
                    pairs.push(mem::take(&mut **child));
                }
            }
        }
    }
}

impl Clone for Environment {
    fn clone(&self) -> Self {
        enum Step<'a> {
            Clone(&'a Environment),
            Pair,
        }

        let mut steps = vec![Step::Clone(self)];

        // The clones of the children of each pair are built before the pair itself
        let mut cloned = Vec::new();

        while let Some(step) = steps.pop() {
            match step {
                Step::Clone(Self::Nil) => cloned.push(Self::Nil),
                Step::Clone(Self::Leaf(symbol)) => cloned.push(Self::Leaf(*symbol)),
                Step::Clone(Self::Pair(first, rest)) => {
                    steps.push(Step::Pair);
                    steps.push(Step::Clone(rest));
                    steps.push(Step::Clone(first));
                }
                Step::Pair => {
                    let rest = cloned.pop().expect("rest is cloned");
                    let first = cloned.pop().expect("first is cloned");
                    cloned.push(Self::Pair(Box::new(first), Box::new(rest)));
                }
            }
        }

        cloned.pop().expect("environment is cloned")
    }
}

impl Drop for Environment {
    fn drop(&mut self) {
        let mut pairs = Vec::new();
        self.take_child_pairs(&mut pairs);

        // Each pair is dropped once its children have been taken, so dropping it doesn't recurse
        while let Some(mut pair) = pairs.pop() {
            pair.take_child_pairs(&mut pairs);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PairOp {
    First,
    Rest,
}

#[cfg(test)]
mod tests {
    use std::thread;

    use id_arena::Arena;

    use crate::Symbol;

    use super::*;

    fn symbols(n: usize) -> Vec<SymbolId> {
        let mut arena = Arena::<Symbol>::new();
        (0..n).map(|_| arena.alloc(Symbol::Unresolved)).collect()
    }

    fn pair(first: Environment, rest: Environment) -> Environment {
        Environment::Pair(Box::new(first), Box::new(rest))
    }

    fn path(env: &Environment, symbol: SymbolId) -> u64 {
        env.path(symbol).unwrap().try_into().unwrap()
    }

    #[test]
    fn test_paths() {
        let [a, b, c, d] = symbols(4)[..] else {
            unreachable!()
        };

        let list = Environment::sequential(&[a, b, c], true);
        assert_eq!(path(&list, a), 2);
        assert_eq!(path(&list, b), 5);
        assert_eq!(path(&list, c), 11);
        assert_eq!(list.path(d), Err(PathError::SymbolNotFound));

        let improper = Environment::sequential(&[a, b, c], false);
        assert_eq!(path(&improper, c), 7);

        let tree = pair(
            pair(Environment::Leaf(a), Environment::Leaf(b)),
            Environment::Leaf(c),
        );
        assert_eq!(path(&tree, a), 4);
        assert_eq!(path(&tree, b), 6);
        assert_eq!(path(&tree, c), 3);
        assert_eq!(path(&Environment::Leaf(a), a), 1);
        assert_eq!(Environment::Nil.path(a), Err(PathError::SymbolNotFound));

        // The first of a pair is searched before the rest
        let shadowed = pair(Environment::Leaf(a), Environment::Leaf(a));
        assert_eq!(path(&shadowed, a), 2);
    }

    /// Environments that are deep enough to overflow a 2 MiB stack in a debug build if finding a
    /// path, cloning or dropping recursed once per level, in either direction.
    #[test]
    fn test_deep_environments() {
        const DEPTH: usize = 100_000;

        thread::Builder::new()
            .stack_size(2 * 1024 * 1024)
            .spawn(|| {
                let symbols = symbols(DEPTH);
                let one = || BigUint::from(1u8);

                // The last symbol is the first of the innermost pair, after a rest for each pair
                // before it
                let right = Environment::sequential(&symbols, true);
                let expected = (one() << DEPTH) + (one() << (DEPTH - 1)) - 1u8;
                assert_eq!(right.path(symbols[DEPTH - 1]), Ok(expected.clone()));
                assert_eq!(right.clone().path(symbols[DEPTH - 1]), Ok(expected));

                // The first symbol is the rest of the innermost pair, after a first for each pair
                // before it
                let left = symbols.iter().fold(Environment::Nil, |env, &symbol| {
                    pair(env, Environment::Leaf(symbol))
                });
                let expected = (one() << DEPTH) + (one() << (DEPTH - 1));
                assert_eq!(left.path(symbols[0]), Ok(expected.clone()));
                assert_eq!(left.clone().path(symbols[0]), Ok(expected));
            })
            .unwrap()
            .join()
            .unwrap();
    }
}
