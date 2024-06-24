use std::ops::{BitAnd, BitAndAssign};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Comparison {
    /// The types are exactly the same once aliases are fully resolved.
    /// They can differ in syntax as long as they end up the same.
    Equal,
    /// The first type is a subset of the second type.
    /// This means that the first is assignable to the second.
    Assignable,
    /// Neither type can be directly assigned to the other,
    /// but the first is castable to the second.
    Castable,
    /// The first type is a superset of the second type.
    /// This means that the second is assignable to the first.
    Superset,
    /// Neither type can be assigned to the other.
    Unrelated,
}

impl Comparison {
    pub fn is_equal(self) -> bool {
        self == Self::Equal
    }

    pub fn is_assignable(self) -> bool {
        self <= Self::Assignable
    }

    pub fn is_castable(self) -> bool {
        self <= Self::Castable
    }
}

impl BitAnd for Comparison {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        self.max(rhs)
    }
}

impl BitAndAssign for Comparison {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}
