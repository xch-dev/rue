//! The type builder that the unit tests share.
//!
//! Its helpers keep the names the unit tests have always used, which differ from those of the
//! integration tests' builder in `tests/common`: `generic` takes a name here (`unnamed_generic`
//! doesn't), `alias` is named and wraps its inner type in a `Ref` like the compiler does, and
//! comparing is `cmp`.

use std::collections::HashMap;

use id_arena::Arena;
use indexmap::IndexSet;
use rue_diagnostic::Name;

use crate::{
    Alias, Apply, Atom, AtomRestriction, AtomSemantic, BuiltinTypes, Comparison, FunctionParameter,
    FunctionType, Generic, Inference, Pair, Struct, Type, TypeId, Union, compare,
    compare_with_inference, stringify,
};

pub(crate) struct Db {
    pub(crate) arena: Arena<Type>,
    pub(crate) builtins: BuiltinTypes,
}

impl Db {
    pub(crate) fn new() -> Self {
        let mut arena = Arena::new();
        let builtins = BuiltinTypes::new(&mut arena);
        Self { arena, builtins }
    }

    pub(crate) fn alloc(&mut self, ty: Type) -> TypeId {
        self.arena.alloc(ty)
    }

    pub(crate) fn placeholder(&mut self) -> TypeId {
        self.alloc(Type::Unresolved)
    }

    pub(crate) fn set(&mut self, id: TypeId, ty: Type) {
        *self.arena.get_mut(id).unwrap() = ty;
    }

    pub(crate) fn pair(&mut self, first: TypeId, rest: TypeId) -> TypeId {
        pair(&mut self.arena, first, rest)
    }

    pub(crate) fn union(&mut self, types: &[TypeId]) -> TypeId {
        self.alloc(Type::Union(Union::new(types.to_vec())))
    }

    /// A generic without a name.
    pub(crate) fn unnamed_generic(&mut self) -> TypeId {
        self.alloc(Type::Generic(Generic { name: None }))
    }

    pub(crate) fn generic(&mut self, name: &str) -> TypeId {
        self.alloc(Type::Generic(Generic {
            name: Some(Name::new(name, None)),
        }))
    }

    pub(crate) fn function(&mut self, params: &[TypeId], ret: TypeId) -> TypeId {
        self.function_with(&[], params, true, ret)
    }

    pub(crate) fn function_with(
        &mut self,
        generics: &[TypeId],
        params: &[TypeId],
        nil_terminated: bool,
        ret: TypeId,
    ) -> TypeId {
        self.alloc(Type::Function(FunctionType {
            generics: generics.to_vec(),
            params: params
                .iter()
                .enumerate()
                .map(|(i, &ty)| FunctionParameter::new(format!("p{i}"), ty))
                .collect(),
            nil_terminated,
            ret,
        }))
    }

    /// Creates an alias whose inner type is a `Ref`, like the compiler does.
    pub(crate) fn alias(&mut self, name: &str, generics: &[TypeId], inner: TypeId) -> TypeId {
        let inner = self.alloc(Type::Ref(inner));
        self.alloc(Type::Alias(Alias {
            name: Some(Name::new(name, None)),
            inner,
            generics: generics.to_vec(),
        }))
    }

    /// Declares an alias whose inner type can be filled in later, to allow recursion.
    pub(crate) fn declare_alias(&mut self, name: &str, generics: &[TypeId]) -> TypeId {
        let inner = self.placeholder();
        self.alloc(Type::Alias(Alias {
            name: Some(Name::new(name, None)),
            inner,
            generics: generics.to_vec(),
        }))
    }

    /// Declares a struct whose inner type can be filled in later, to allow recursion.
    pub(crate) fn declare_struct(&mut self, name: &str, generics: &[TypeId]) -> TypeId {
        let id = self.placeholder();
        let inner = self.placeholder();
        self.set(
            id,
            Type::Struct(Struct {
                name: Some(Name::new(name, None)),
                semantic: id,
                inner,
                generics: generics.to_vec(),
                fields: IndexSet::new(),
                nil_terminated: true,
                narrowed: false,
            }),
        );
        id
    }

    pub(crate) fn define(&mut self, id: TypeId, resolved: TypeId) {
        let (Type::Alias(Alias { inner, .. }) | Type::Struct(Struct { inner, .. })) =
            self.arena[id]
        else {
            panic!("expected an alias or struct");
        };
        self.set(inner, Type::Ref(resolved));
    }

    pub(crate) fn strukt(&mut self, name: &str, inner: TypeId) -> TypeId {
        let id = self.declare_struct(name, &[]);
        self.define(id, inner);
        id
    }

    pub(crate) fn apply(&mut self, inner: TypeId, generics: &[(TypeId, TypeId)]) -> TypeId {
        self.alloc(Type::Apply(Apply::new(
            inner,
            generics.iter().copied().collect::<HashMap<_, _>>(),
        )))
    }

    pub(crate) fn list(&mut self, item: TypeId) -> TypeId {
        let (list, generic) = (self.builtins.list, self.builtins.list_generic);
        self.apply(list, &[(generic, item)])
    }

    pub(crate) fn cmp(&mut self, lhs: TypeId, rhs: TypeId) -> Comparison {
        compare(&mut self.arena, &self.builtins, lhs, rhs)
    }

    pub(crate) fn infer(&mut self, lhs: TypeId, rhs: TypeId, infer: &mut Inference) -> Comparison {
        compare_with_inference(&mut self.arena, &self.builtins, lhs, rhs, Some(infer))
    }

    pub(crate) fn name(&mut self, id: TypeId) -> String {
        stringify(&mut self.arena, id)
    }

    pub(crate) fn literal(&mut self, semantic: AtomSemantic, value: &[u8]) -> TypeId {
        literal(&mut self.arena, semantic, value)
    }
}

/// Allocates a pair, for the tests that build types in an arena of their own.
pub(crate) fn pair(arena: &mut Arena<Type>, first: TypeId, rest: TypeId) -> TypeId {
    arena.alloc(Type::Pair(Pair::new(first, rest)))
}

/// Allocates an atom with the given value, for the tests that build types in an arena of their
/// own.
pub(crate) fn literal(arena: &mut Arena<Type>, semantic: AtomSemantic, value: &[u8]) -> TypeId {
    arena.alloc(Type::Atom(Atom::new(
        semantic,
        Some(AtomRestriction::Value(value.to_vec().into())),
    )))
}
