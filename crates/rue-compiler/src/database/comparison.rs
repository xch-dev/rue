use std::ops::{BitAnd, BitAndAssign};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Comparison {
    /// Neither type can be assigned to the other.
    Unrelated,
    /// Neither type can be directly assigned to the other,
    /// but the first is castable to the second.
    Castable,
    /// The types are exactly the same once aliases are fully resolved.
    /// They can differ in syntax as long as they end up the same.
    Equal,
    /// The first type is a subset of the second type.
    /// This means that the first is assignable to the second.
    Assignable,
    /// The first type is a superset of the second type.
    /// This means that the second is assignable to the first.
    Superset,
}

impl BitAnd for Comparison {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Equal, Self::Equal) => Self::Equal,
            (Self::Assignable, Self::Assignable) => Self::Assignable,
            (Self::Superset, Self::Superset) => Self::Superset,
            (Self::Castable, Self::Castable) => Self::Castable,
            (Self::Equal, Self::Superset) | (Self::Superset, Self::Equal) => Self::Superset,
            (Self::Equal, Self::Assignable) | (Self::Assignable, Self::Equal) => Self::Assignable,
            (Self::Castable, Self::Assignable | Self::Equal)
            | (Self::Assignable | Self::Equal, Self::Castable) => Self::Castable,
            (Self::Unrelated, _) | (_, Self::Unrelated) => Self::Unrelated,
            (Self::Superset, Self::Assignable | Self::Castable)
            | (Self::Assignable | Self::Castable, Self::Superset) => Self::Unrelated,
        }
    }
}

impl BitAndAssign for Comparison {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}
