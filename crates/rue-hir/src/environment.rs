use std::cmp::Reverse;

use indexmap::IndexMap;
use num_bigint::BigUint;

use crate::SymbolId;

#[derive(Debug, Default, Clone)]
#[must_use]
pub enum Environment {
    #[default]
    Nil,
    Leaf(SymbolId),
    Pair(Box<Environment>, Box<Environment>),
    /// A subtree which is provided by the caller (such as parameters), rather than constructed
    /// by the compiler. Its shape isn't guaranteed at runtime, since values can come from the
    /// solution or a spread argument without being checked against their types.
    Unchecked(Box<Environment>),
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

    pub fn unchecked(env: Self) -> Self {
        match env {
            Self::Nil | Self::Unchecked(_) => env,
            _ => Self::Unchecked(Box::new(env)),
        }
    }

    /// Whether the value at the given path is guaranteed to be a pair at runtime,
    /// because the compiler constructed it rather than it being provided by the caller.
    pub fn is_pair(&self, path: &BigUint) -> bool {
        if *path == BigUint::ZERO {
            return false;
        }

        let mut env = self;

        for i in 0..path.bits() - 1 {
            let Self::Pair(first, rest) = env else {
                return false;
            };

            env = if path.bit(i) { rest } else { first };
        }

        matches!(env, Self::Pair(..))
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
        let mut path = BigUint::from(1u8);
        for op in ops.into_iter().rev() {
            path <<= 1;
            if matches!(op, PairOp::Rest) {
                path += 1u8;
            }
        }
        Ok(path)
    }

    fn get_pair_ops(&self, symbol_id: SymbolId) -> Option<Vec<PairOp>> {
        match self {
            Self::Nil => None,
            Self::Leaf(leaf) => {
                if *leaf == symbol_id {
                    Some(vec![])
                } else {
                    None
                }
            }
            Self::Pair(first, rest) => {
                if let Some(mut ops) = first.get_pair_ops(symbol_id) {
                    ops.insert(0, PairOp::First);
                    Some(ops)
                } else if let Some(mut ops) = rest.get_pair_ops(symbol_id) {
                    ops.insert(0, PairOp::Rest);
                    Some(ops)
                } else {
                    None
                }
            }
            Self::Unchecked(env) => env.get_pair_ops(symbol_id),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PairOp {
    First,
    Rest,
}
