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
