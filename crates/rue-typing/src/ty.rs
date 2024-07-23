use crate::{Alias, Lazy, TypeId};

#[derive(Debug, Clone)]
pub enum Type {
    Unknown,
    Generic,
    Never,
    Atom,
    Bytes,
    Bytes32,
    PublicKey,
    Int,
    Bool,
    Nil,
    Pair(TypeId, TypeId),
    Union(Vec<TypeId>),
    Ref(TypeId),
    Lazy(Lazy),
    Alias(Alias),
}
