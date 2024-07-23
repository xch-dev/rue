use std::collections::{HashMap, HashSet, VecDeque};

use num_bigint::BigInt;
use num_traits::One;

use crate::{Type, TypeId, TypeSystem};

use super::CheckError;

pub(crate) struct Attributes {
    pub atom_count: usize,
    pub bytes32_count: usize,
    pub public_key_count: usize,
    pub pairs: Vec<(TypeId, TypeId)>,
    pub values: HashMap<BigInt, usize>,
    pub length: usize,
}

impl Attributes {
    pub fn all_atoms(&self) -> bool {
        self.atom_count == self.length
    }

    pub fn all_bytes32(&self) -> bool {
        self.bytes32_count == self.length
    }

    pub fn all_public_key(&self) -> bool {
        self.public_key_count == self.length
    }

    pub fn all_pairs(&self) -> bool {
        self.pairs.len() == self.length
    }

    pub fn all_value(&self, value: &BigInt) -> bool {
        self.values.get(value).copied().unwrap_or(0) == self.length
    }

    pub fn atoms_are_bytes32(&self) -> bool {
        self.bytes32_count == self.atom_count
    }

    pub fn atoms_are_public_key(&self) -> bool {
        self.public_key_count == self.atom_count
    }

    pub fn atoms_are_value(&self, value: &BigInt) -> bool {
        self.values.get(value).copied().unwrap_or(0) == self.atom_count
    }
}

pub(crate) fn union_attributes(
    db: &TypeSystem,
    items: &[TypeId],
    is_lhs: bool,
    other_type_id: TypeId,
    visited: &mut HashSet<(TypeId, TypeId)>,
) -> Result<Attributes, CheckError> {
    let mut atom_count = 0;
    let mut bytes32_count = 0;
    let mut public_key_count = 0;
    let mut pairs = Vec::new();
    let mut values = HashMap::new();

    let mut items: VecDeque<_> = items.iter().copied().collect();
    let mut length = 0;

    while !items.is_empty() {
        let item = items.remove(0).unwrap();
        length += 1;

        let key = if is_lhs {
            (item, other_type_id)
        } else {
            (other_type_id, item)
        };

        if !visited.insert(key) {
            return Err(CheckError::Recursive(key.0, key.1));
        }

        match db.get(item) {
            Type::Ref(..) => unreachable!(),
            Type::Lazy(..) => unreachable!(),
            Type::Alias(..) => unreachable!(),
            Type::Struct(..) => unreachable!(),
            Type::Callable(..) => unreachable!(),
            Type::Enum(..) => unreachable!(),
            Type::Variant(..) => unreachable!(),
            Type::Generic => {
                length -= 1;
            }
            Type::Unknown => {}
            Type::Never => {
                length -= 1;
            }
            Type::Atom | Type::Bytes | Type::Int => {
                atom_count += 1;
            }
            Type::Bytes32 => {
                atom_count += 1;
                bytes32_count += 1;
            }
            Type::PublicKey => {
                atom_count += 1;
                public_key_count += 1;
            }
            Type::Nil => {
                atom_count += 1;
                *values.entry(BigInt::ZERO).or_insert(0) += 1;
            }
            Type::True => {
                atom_count += 1;
                *values.entry(BigInt::one()).or_insert(0) += 1;
            }
            Type::False => {
                atom_count += 1;
                *values.entry(BigInt::ZERO).or_insert(0) += 1;
            }
            Type::Value(value) => {
                atom_count += 1;
                *values.entry(value.clone()).or_insert(0) += 1;
            }
            Type::Pair(first, rest) => {
                pairs.push((*first, *rest));
            }
            Type::Union(child_items) => {
                items.extend(child_items);
            }
        }

        visited.remove(&key);
    }

    Ok(Attributes {
        atom_count,
        bytes32_count,
        public_key_count,
        pairs,
        values,
        length,
    })
}
