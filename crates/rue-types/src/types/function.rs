use indexmap::IndexMap;

use crate::TypeId;

#[derive(Debug, Clone)]
pub struct FunctionType {
    /// The function's own generic type parameters, which are inferred at each call site.
    pub generics: Vec<TypeId>,
    pub params: IndexMap<String, TypeId>,
    pub nil_terminated: bool,
    pub ret: TypeId,
}
