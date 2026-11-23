//! Tests for stringifying function and struct types, which should match the source syntax so that
//! types which differ are never displayed the same way.

mod common;

use common::Types;

use indexmap::IndexSet;
use rue_diagnostic::Name;
use rue_types::{FunctionParameter, FunctionType, Struct, Type, TypeId};

impl Types {
    fn named_function(
        &mut self,
        generics: &[TypeId],
        params: &[(&str, TypeId)],
        nil_terminated: bool,
        ret: TypeId,
    ) -> TypeId {
        self.arena.alloc(Type::Function(FunctionType {
            generics: generics.to_vec(),
            params: params
                .iter()
                .map(|&(name, ty)| FunctionParameter::new(name, ty))
                .collect(),
            nil_terminated,
            ret,
        }))
    }

    fn strukt(
        &mut self,
        name: &str,
        generics: &[TypeId],
        fields: &[&str],
        nil_terminated: bool,
        inner: TypeId,
    ) -> TypeId {
        let id = self.arena.alloc(Type::Unresolved);
        self.arena[id] = Type::Struct(Struct {
            name: Some(Name::new(name, None)),
            semantic: id,
            inner,
            generics: generics.to_vec(),
            fields: fields
                .iter()
                .map(ToString::to_string)
                .collect::<IndexSet<_>>(),
            nil_terminated,
            layout: rue_types::StructLayout::List,
            narrowed: false,
        });
        id
    }

    fn narrow(&mut self, id: TypeId, inner: TypeId) -> TypeId {
        let Type::Struct(ty) = self.arena[id].clone() else {
            panic!("expected a struct");
        };
        self.arena.alloc(Type::Struct(Struct {
            inner,
            narrowed: true,
            ..ty
        }))
    }
}

#[test]
fn test_function() {
    let mut db = Types::new();
    let int = db.builtins.int;
    let function = db.named_function(&[], &[("a", int), ("b", int)], true, int);
    assert_eq!(db.name(function), "fn(a: Int, b: Int) -> Int");

    let function = db.named_function(&[], &[], true, int);
    assert_eq!(db.name(function), "fn() -> Int");
}

#[test]
fn test_function_spread() {
    let mut db = Types::new();
    let int = db.builtins.int;
    let any = db.arena.alloc(Type::Any);

    let nil_terminated = db.named_function(&[], &[("a", int), ("b", any)], true, int);
    let spread = db.named_function(&[], &[("a", int), ("b", any)], false, int);
    assert_eq!(db.name(nil_terminated), "fn(a: Int, b: Any) -> Int");
    assert_eq!(db.name(spread), "fn(a: Int, ...b: Any) -> Int");

    let spread = db.named_function(&[], &[("value", any)], false, int);
    assert_eq!(db.name(spread), "fn(...value: Any) -> Int");
}

#[test]
fn test_function_generics() {
    let mut db = Types::new();
    let t = db.named_generic("T");
    let u = db.named_generic("U");

    let generic = db.named_function(&[u], &[("y", u)], true, t);
    let non_generic = db.named_function(&[], &[("y", u)], true, t);
    assert_eq!(db.name(generic), "fn<U>(y: U) -> T");
    assert_eq!(db.name(non_generic), "fn(y: U) -> T");

    let generic = db.named_function(&[t, u], &[("a", t), ("b", u)], false, u);
    assert_eq!(db.name(generic), "fn<T, U>(a: T, ...b: U) -> U");
}

#[test]
fn test_function_anonymous_generics() {
    let mut db = Types::new();
    let a = db.generic();
    let b = db.generic();

    let function = db.named_function(&[a, b], &[("a", a)], true, b);
    let name = db.name(function);
    let a = format!("{{generic {}}}", a.index());
    let b = format!("{{generic {}}}", b.index());
    assert_ne!(a, b);
    assert_eq!(name, format!("fn<{a}, {b}>(a: {a}) -> {b}"));
}

#[test]
fn test_narrowed_struct() {
    let mut db = Types::new();
    let int = db.builtins.int;
    let bytes = db.builtins.bytes;
    let nil = db.builtins.nil;

    let value = db.union([int, bytes]);
    let inner = db.pair(value, nil);
    let declared = db.strukt("Box", &[value], &["value"], true, inner);
    assert_eq!(db.name(declared), "Box<Int | Bytes>");

    // The generic arguments no longer describe the narrowed fields
    let inner = db.pair(int, nil);
    let narrowed = db.narrow(declared, inner);
    assert_eq!(db.name(narrowed), "Box { value: Int }");

    // Copies of the same struct which look the same are only displayed once
    let copy = db.narrow(declared, inner);
    let union = db.union([declared, narrowed, copy]);
    assert_eq!(db.name(union), "Box<Int | Bytes> | Box { value: Int }");

    // But different structs with the same name are all displayed
    let inner = db.pair(value, nil);
    let other = db.strukt("Box", &[value], &["value"], true, inner);
    let union = db.union([declared, other]);
    assert_eq!(db.name(union), "Box<Int | Bytes> | Box<Int | Bytes>");

    // If the structure of the fields is unknown, the declared type is displayed
    let any = db.arena.alloc(Type::Any);
    let unknown = db.narrow(declared, any);
    assert_eq!(db.name(unknown), "Box<Int | Bytes>");
}

#[test]
fn test_narrowed_struct_fields() {
    let mut db = Types::new();
    let int = db.builtins.int;
    let nil = db.builtins.nil;

    let rest = db.union([int, nil]);
    let inner = db.pair(rest, rest);
    let declared = db.strukt("Spread", &[], &["first", "rest"], false, inner);
    assert_eq!(db.name(declared), "Spread");

    let inner = db.pair(int, nil);
    let narrowed = db.narrow(declared, inner);
    assert_eq!(db.name(narrowed), "Spread { first: Int, ...rest: nil }");

    // Correlated fields are displayed as separate variants
    let other = db.pair(nil, int);
    let inner = db.union([inner, other]);
    let narrowed = db.narrow(declared, inner);
    assert_eq!(
        db.name(narrowed),
        "Spread { first: Int, ...rest: nil } | Spread { first: nil, ...rest: Int }"
    );

    // The preceding fields are kept when the remaining fields are correlated
    let rest = db.union([inner, other]);
    let inner = db.pair(int, rest);
    let declared = db.strukt("Three", &[], &["a", "b", "c"], false, inner);
    let first = db.pair(int, nil);
    let second = db.pair(nil, int);
    let rest = db.union([first, second]);
    let inner = db.pair(int, rest);
    let narrowed = db.narrow(declared, inner);
    assert_eq!(
        db.name(narrowed),
        "Three { a: Int, b: Int, ...c: nil } | Three { a: Int, b: nil, ...c: Int }"
    );
}
