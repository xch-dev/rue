use std::ops::Not;

use id_arena::Arena;

use crate::{Type, TypeId};

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

pub fn shape_of(arena: &Arena<Type>, id: TypeId) -> Shape {
    match arena[id].clone() {
        Type::Unresolved | Type::Apply(_) | Type::Generic => unreachable!(),
        Type::Alias(alias) => shape_of(arena, alias.inner),
        Type::Struct(ty) => shape_of(arena, ty.inner),
        Type::Never => Shape::NONE,
        Type::Any => Shape::BOTH,
        Type::List(_) => Shape::BOTH,
        Type::Atom(_) => Shape::ATOM,
        Type::Pair(_) => Shape::PAIR,
        Type::Function(_) => Shape::BOTH,
        Type::Union(ty) => {
            let mut shape = Shape::NONE;

            for &id in &ty.types {
                let inner = shape_of(arena, id);
                shape.atom |= inner.atom;
                shape.pair |= inner.pair;
            }

            shape
        }
    }
}
