use crate::{Type, TypeId, TypeSystem};

#[derive(Debug, Clone, Copy)]
pub struct StandardTypes {
    pub unknown: TypeId,
    pub any: TypeId,
    pub bytes: TypeId,
    pub bytes32: TypeId,
    pub public_key: TypeId,
    pub int: TypeId,
    pub bool: TypeId,
    pub nil: TypeId,
}

impl StandardTypes {
    pub fn alloc(type_system: &mut TypeSystem) -> Self {
        let unknown = type_system.alloc(Type::Unknown);
        let bytes = type_system.alloc(Type::Bytes);
        let bytes32 = type_system.alloc(Type::Bytes32);
        let public_key = type_system.alloc(Type::PublicKey);
        let int = type_system.alloc(Type::Int);
        let bool = type_system.alloc(Type::Bool);
        let nil = type_system.alloc(Type::Nil);

        let any = type_system.alloc(Type::Unknown);
        let pair = type_system.alloc(Type::Pair(any, any));
        *type_system.get_mut(any) = Type::Union(vec![bytes, pair]);

        Self {
            unknown,
            any,
            bytes,
            bytes32,
            public_key,
            int,
            bool,
            nil,
        }
    }
}