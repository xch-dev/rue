use std::ops::Not;

use id_arena::Arena;

use crate::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Shape {
    pub atom: bool,
    pub pair: bool,
}

impl Shape {
    pub const NONE: Self = Self {
        atom: false,
        pair: false,
    };

    pub const BOTH: Self = Self {
        atom: true,
        pair: true,
    };

    pub const ATOM: Self = Self {
        atom: true,
        pair: false,
    };

    pub const PAIR: Self = Self {
        atom: false,
        pair: true,
    };
}

impl Not for Shape {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self {
            atom: !self.atom,
            pair: !self.pair,
        }
    }
}

pub fn shape_of(arena: &Arena<Type>, ty: Type) -> Shape {
    match ty {
        Type::Unresolved | Type::Apply(_) | Type::Generic => unreachable!(),
        Type::Ref(id) => shape_of(arena, arena[id].clone()),
        Type::Alias(alias) => shape_of(arena, arena[alias.inner].clone()),
        Type::Struct(ty) => shape_of(arena, arena[ty.inner].clone()),
        Type::Atom(_) => Shape::ATOM,
        Type::Pair(_) => Shape::PAIR,
        Type::Function(_) => Shape::BOTH,
        Type::Union(ty) => {
            let mut shape = Shape::NONE;

            for &id in &ty.types {
                let inner = shape_of(arena, arena[id].clone());
                shape.atom |= inner.atom;
                shape.pair |= inner.pair;
            }

            shape
        }
    }
}
