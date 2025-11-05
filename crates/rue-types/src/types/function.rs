use indexmap::IndexMap;

use crate::TypeId;

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub params: IndexMap<String, TypeId>,
    pub nil_terminated: bool,
    pub ret: TypeId,
}
