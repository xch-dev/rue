use crate::TypeId;

#[derive(Debug, Clone, Copy)]
pub struct StandardTypes {
    pub unknown: TypeId,
    pub never: TypeId,
    pub any: TypeId,
    pub atom: TypeId,
    pub bytes: TypeId,
    pub bytes32: TypeId,
    pub public_key: TypeId,
    pub int: TypeId,
    pub bool: TypeId,
    pub nil: TypeId,
}
