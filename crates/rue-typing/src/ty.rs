use num_bigint::BigInt;

use crate::{Alias, Callable, Enum, Lazy, Struct, TypeId, Variant};

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
    True,
    False,
    Nil,
    Value(BigInt),
    Pair(TypeId, TypeId),
    Union(Vec<TypeId>),
    Ref(TypeId),
    Lazy(Lazy),
    Alias(Alias),
    Struct(Struct),
    Callable(Callable),
    Enum(Enum),
    Variant(Variant),
}
