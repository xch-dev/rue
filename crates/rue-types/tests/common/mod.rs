//! The type builder that the integration tests share. Each test only uses some of it.

#![allow(dead_code)]

use std::collections::HashMap;

use id_arena::Arena;
use indexmap::IndexSet;
use rue_diagnostic::Name;
use rue_types::{
    Alias, Apply, BuiltinTypes, Comparison, FunctionParameter, FunctionType, Generic, Pair, Struct,
    Type, TypeId, Union, compare, stringify,
};

pub struct Types {
    pub arena: Arena<Type>,
    pub builtins: BuiltinTypes,
}

impl Types {
    pub fn new() -> Self {
        let mut arena = Arena::new();
        let builtins = BuiltinTypes::new(&mut arena);
        Self { arena, builtins }
    }

    pub fn alloc(&mut self, ty: Type) -> TypeId {
        self.arena.alloc(ty)
    }

    pub fn pair(&mut self, first: TypeId, rest: TypeId) -> TypeId {
        self.alloc(Type::Pair(Pair::new(first, rest)))
    }

    pub fn union(&mut self, types: impl Into<Vec<TypeId>>) -> TypeId {
        self.alloc(Type::Union(Union::new(types.into())))
    }

    /// An alias without a name or generics.
    pub fn alias(&mut self, inner: TypeId) -> TypeId {
        self.alloc(Type::Alias(Alias {
            name: None,
            inner,
            generics: vec![],
        }))
    }

    /// Declares an alias whose inner type is filled in later by `define`, to allow recursion.
    pub fn declare_alias(&mut self, name: &str, generics: impl Into<Vec<TypeId>>) -> TypeId {
        let inner = self.alloc(Type::Unresolved);
        self.alloc(Type::Alias(Alias {
            name: Some(Name::new(name, None)),
            inner,
            generics: generics.into(),
        }))
    }

    pub fn define(&mut self, alias: TypeId, resolved: TypeId) {
        let Type::Alias(Alias { inner, .. }) = self.arena[alias] else {
            panic!("expected an alias");
        };
        self.arena[inner] = Type::Ref(resolved);
    }

    /// A struct without a name, generics or fields.
    pub fn structure(&mut self, inner: TypeId) -> TypeId {
        let id = self.alloc(Type::Unresolved);
        self.arena[id] = Type::Struct(Struct {
            name: None,
            semantic: id,
            inner,
            generics: vec![],
            fields: IndexSet::new(),
            nil_terminated: true,
            narrowed: false,
        });
        id
    }

    /// A generic without a name.
    pub fn generic(&mut self) -> TypeId {
        self.alloc(Type::Generic(Generic { name: None }))
    }

    pub fn named_generic(&mut self, name: &str) -> TypeId {
        self.alloc(Type::Generic(Generic {
            name: Some(Name::new(name, None)),
        }))
    }

    /// A function whose parameters are named `p0`, `p1` and so on, without a spread parameter.
    pub fn function(
        &mut self,
        generics: impl Into<Vec<TypeId>>,
        params: impl Into<Vec<TypeId>>,
        ret: TypeId,
    ) -> TypeId {
        self.alloc(Type::Function(FunctionType {
            generics: generics.into(),
            params: params
                .into()
                .into_iter()
                .enumerate()
                .map(|(i, ty)| FunctionParameter::new(format!("p{i}"), ty))
                .collect(),
            nil_terminated: true,
            ret,
        }))
    }

    pub fn apply(
        &mut self,
        inner: TypeId,
        generics: impl IntoIterator<Item = (TypeId, TypeId)>,
    ) -> TypeId {
        self.alloc(Type::Apply(Apply::new(
            inner,
            generics.into_iter().collect::<HashMap<_, _>>(),
        )))
    }

    pub fn compare(&mut self, lhs: TypeId, rhs: TypeId) -> Comparison {
        compare(&mut self.arena, &self.builtins, lhs, rhs)
    }

    pub fn name(&mut self, id: TypeId) -> String {
        stringify(&mut self.arena, id)
    }
}
