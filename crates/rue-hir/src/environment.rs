use indexmap::IndexMap;

use crate::SymbolId;

#[derive(Debug, Default, Clone)]
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
    PathTooLarge,
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
        symbols.sort_by(|a, b| b.1.cmp(&a.1));

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

    pub fn path(&self, symbol_id: SymbolId) -> Result<u32, PathError> {
        let ops = self
            .get_pair_ops(symbol_id)
            .ok_or(PathError::SymbolNotFound)?;
        let mut path: u32 = 1;
        for op in ops.into_iter().rev() {
            match op {
                PairOp::First => {
                    path = path.checked_mul(2).ok_or(PathError::PathTooLarge)?;
                }
                PairOp::Rest => {
                    path = path
                        .checked_mul(2)
                        .ok_or(PathError::PathTooLarge)?
                        .checked_add(1)
                        .ok_or(PathError::PathTooLarge)?;
                }
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
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PairOp {
    First,
    Rest,
}
