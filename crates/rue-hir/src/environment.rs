use std::{cmp::Reverse, mem};

use indexmap::IndexMap;
use num_bigint::BigUint;

use crate::SymbolId;

/// A sequential group is a chain of pairs, with one level for each symbol, and a tree is as deep as
/// it has unreferenced symbols, so an environment can be thousands of levels deep. Building it,
/// finding paths in it, [`Clone`] and [`Drop`] are iterative rather than recursing once per level.
/// [`Debug`] is derived, since it's only used for debugging.
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

    /// Builds a tree of the symbols, splitting each group so that the reference counts on either
    /// side are as even as possible, with the most referenced symbols first, so that they have
    /// shorter paths.
    ///
    /// Symbols that aren't referenced don't change the balance, so they're split off one at a time,
    /// and the tree is as deep as there are unreferenced symbols.
    pub fn tree(referenced_symbols: IndexMap<SymbolId, usize>) -> Self {
        // The sort is stable, so symbols that are referenced equally often stay in order
        let mut symbols: Vec<_> = referenced_symbols.into_iter().collect();
        symbols.sort_by_key(|&(_, count)| Reverse(count));

        if symbols.is_empty() {
            return Self::Nil;
        }

        // The total reference count of the symbols before each index
        let mut counts_before = Vec::with_capacity(symbols.len() + 1);
        counts_before.push(0);

        for (_, count) in &symbols {
            counts_before.push(counts_before.last().unwrap() + count);
        }

        let mut root = Self::Nil;

        // Each range of symbols left to build, and where to put its tree
        let mut pending = vec![(0, symbols.len(), &mut root)];

        while let Some((start, end, tree)) = pending.pop() {
            if end - start == 1 {
                *tree = Self::Leaf(symbols[start].0);
                continue;
            }

            let split = Self::split(&counts_before[start..=end]);

            *tree = Self::Pair(Box::default(), Box::default());

            if let Self::Pair(first, rest) = tree {
                pending.push((start, start + split, first));
                pending.push((start + split, end, rest));
            }
        }

        root
    }

    /// Finds how many of the symbols go in the first group, so that the reference counts of the two
    /// groups are as close as possible, picking the smallest such number when there's a tie. Each
    /// group has at least one symbol. This takes the running totals of the reference counts, from
    /// before the first symbol to after the last one.
    fn split(counts_before: &[usize]) -> usize {
        let start = counts_before[0];
        let total = counts_before[counts_before.len() - 1] - start;
        let difference = |split: usize| (2 * (counts_before[split] - start)).abs_diff(total);

        let mut best = 1;

        for split in 2..counts_before.len() - 1 {
            // Once the first group has at least half of the references, the difference only grows.
            // The symbols are sorted by how often they're referenced, so this stops before the
            // smaller group is passed, and right away for unreferenced symbols.
            if 2 * (counts_before[split - 1] - start) >= total {
                break;
            }

            if difference(split) < difference(best) {
                best = split;
            }
        }

        best
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
        let mut root = Self::Nil;

        // Each environment left to clone, and where to put its clone
        let mut pending = vec![(self, &mut root)];

        while let Some((env, clone)) = pending.pop() {
            *clone = match env {
                Self::Nil => Self::Nil,
                Self::Leaf(symbol) => Self::Leaf(*symbol),
                Self::Pair(..) => Self::Pair(Box::default(), Box::default()),
            };

            if let (Self::Pair(first, rest), Self::Pair(first_clone, rest_clone)) = (env, clone) {
                pending.push((first, first_clone));
                pending.push((rest, rest_clone));
            }
        }

        root
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

        for env in [&list, &improper, &tree, &shadowed, &Environment::Nil] {
            assert_eq!(tree_string(&env.clone()), tree_string(env));
        }
    }

    /// The tree that the recursive implementation built, which splits each group at the first index
    /// that balances the reference counts best.
    fn reference_tree(symbols: &[(SymbolId, usize)]) -> String {
        match symbols {
            [] => "Nil".to_string(),
            [(symbol, _)] => format!("{symbol:?}"),
            _ => {
                let total: usize = symbols.iter().map(|(_, count)| count).sum();
                let mut first = 0;
                let mut best = (usize::MAX, 0);

                for (i, (_, count)) in symbols.iter().enumerate().take(symbols.len() - 1) {
                    first += count;
                    let difference = (2 * first).abs_diff(total);

                    if difference < best.0 {
                        best = (difference, i + 1);
                    }
                }

                let (first, rest) = symbols.split_at(best.1);
                format!("({}, {})", reference_tree(first), reference_tree(rest))
            }
        }
    }

    fn tree_string(env: &Environment) -> String {
        match env {
            Environment::Nil => "Nil".to_string(),
            Environment::Leaf(symbol) => format!("{symbol:?}"),
            Environment::Pair(first, rest) => {
                format!("({}, {})", tree_string(first), tree_string(rest))
            }
        }
    }

    /// Trees are split the same way as they were by the recursive implementation, so that compiled
    /// programs don't change.
    #[test]
    fn test_tree_splits() {
        // Every combination of counts from 0 to 3 for up to 6 symbols
        let mut patterns: Vec<Vec<usize>> = Vec::new();

        for len in 0..=6 {
            for mut combination in 0..4usize.pow(len) {
                patterns.push(
                    (0..len)
                        .map(|_| {
                            let count = combination % 4;
                            combination /= 4;
                            count
                        })
                        .collect(),
                );
            }
        }

        patterns.push(vec![5, 3, 3, 2, 1, 1, 0, 0, 9, 1, 0, 0, 4]);
        patterns.push(vec![64, 32, 16, 8, 4, 2, 1, 1, 0, 0, 0]);

        // Long enough that an unstable sort would reorder symbols with the same count
        patterns.push((0..100).map(|i| (i * 7) % 3).collect());

        for counts in &patterns {
            let symbols: Vec<_> = symbols(counts.len())
                .into_iter()
                .zip(counts.iter().copied())
                .collect();

            let mut sorted = symbols.clone();
            sorted.sort_by_key(|&(_, count)| Reverse(count));

            let tree = Environment::tree(symbols.into_iter().collect());
            assert_eq!(tree_string(&tree), reference_tree(&sorted), "{counts:?}");
        }
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
