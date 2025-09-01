use crate::TypeId;

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub params: Vec<TypeId>,
    pub nil_terminated: bool,
    pub ret: TypeId,
    pub inner: TypeId,
}
