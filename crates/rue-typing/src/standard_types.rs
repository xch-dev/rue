use crate::TypeId;

#[derive(Debug, Clone, Copy)]
pub struct StandardTypes {
    pub unknown: TypeId,
    pub never: TypeId,
    pub any: TypeId,
    pub unmapped_list: TypeId,
    pub generic_list_item: TypeId,
    pub bytes: TypeId,
    pub bytes32: TypeId,
    pub public_key: TypeId,
    pub int: TypeId,
    pub bool: TypeId,
    pub true_bool: TypeId,
    pub false_bool: TypeId,
    pub nil: TypeId,
}
