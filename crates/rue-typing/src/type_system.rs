use std::collections::HashSet;

use id_arena::{Arena, Id};

use crate::{
    check_type, compare_type, difference_type, simplify_check, Check, CheckError, Comparison,
    StandardTypes, Type,
};

pub type TypeId = Id<Type>;

#[derive(Debug, Default, Clone)]
pub struct TypeSystem {
    arena: Arena<Type>,
}

impl TypeSystem {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc(&mut self, ty: Type) -> TypeId {
        self.arena.alloc(ty)
    }

    pub fn get(&self, id: TypeId) -> &Type {
        match &self.arena[id] {
            Type::Ref(id) => self.get(*id),
            ty => ty,
        }
    }

    pub fn get_mut(&mut self, id: TypeId) -> &mut Type {
        match &self.arena[id] {
            Type::Ref(id) => self.get_mut(*id),
            _ => &mut self.arena[id],
        }
    }

    pub fn compare(&self, lhs: TypeId, rhs: TypeId) -> Comparison {
        compare_type(self, lhs, rhs, &mut HashSet::new())
    }

    pub fn check(&self, lhs: TypeId, rhs: TypeId) -> Result<Check, CheckError> {
        check_type(self, lhs, rhs, &mut HashSet::new()).map(simplify_check)
    }

    pub fn difference(&mut self, std: &StandardTypes, lhs: TypeId, rhs: TypeId) -> TypeId {
        difference_type(self, std, lhs, rhs, &mut HashSet::new())
    }
}
