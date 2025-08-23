use crate::TypeId;

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub params: Vec<TypeId>,
    pub ret: TypeId,
}
