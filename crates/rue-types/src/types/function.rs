use crate::TypeId;

#[derive(Debug, Clone)]
pub struct FunctionType {
    /// The function's own generic type parameters, which are inferred at each call site.
    pub generics: Vec<TypeId>,
    /// The parameters in order. Names are only used for display, and may be empty or repeated.
    pub params: Vec<FunctionParameter>,
    pub nil_terminated: bool,
    pub ret: TypeId,
}

#[derive(Debug, Clone)]
pub struct FunctionParameter {
    pub name: String,
    pub ty: TypeId,
}

impl FunctionParameter {
    pub fn new(name: impl Into<String>, ty: TypeId) -> Self {
        Self {
            name: name.into(),
            ty,
        }
    }
}
