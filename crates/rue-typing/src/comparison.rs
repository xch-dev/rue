#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Comparison {
    Equal,
    Assignable,
    Castable,
    Superset,
    Incompatible,
}
