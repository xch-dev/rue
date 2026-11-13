//! Tests for intersection types, which are produced by narrowing a generic type with a type guard
//! (such as `T & Atom` for a value of type `T` that has been checked to be an atom).

use std::collections::HashMap;

use id_arena::Arena;
use rue_diagnostic::Name;
use rue_types::{
    Alias, BuiltinTypes, CheckError, Comparison, FunctionType, Generic, Inference, Intersection,
    Pair, Type, TypeId, Union, check, compare, compare_with_inference, intersect, stringify,
    substitute_with_mappings, subtract,
};

struct Types {
    arena: Arena<Type>,
    builtins: BuiltinTypes,
}

impl Types {
    fn new() -> Self {
        let mut arena = Arena::new();
        let builtins = BuiltinTypes::new(&mut arena);
        Self { arena, builtins }
    }

    fn alloc(&mut self, ty: Type) -> TypeId {
        self.arena.alloc(ty)
    }

    fn generic(&mut self, name: &str) -> TypeId {
        self.alloc(Type::Generic(Generic {
            name: Some(Name::new(name, None)),
        }))
    }

    fn pair(&mut self, first: TypeId, rest: TypeId) -> TypeId {
        self.alloc(Type::Pair(Pair::new(first, rest)))
    }

    fn union(&mut self, types: Vec<TypeId>) -> TypeId {
        self.alloc(Type::Union(Union::new(types)))
    }

    fn any_pair(&mut self) -> TypeId {
        let any = self.builtins.permissive_any;
        self.pair(any, any)
    }

    fn intersect(&mut self, lhs: TypeId, rhs: TypeId) -> TypeId {
        intersect(&mut self.arena, &self.builtins, lhs, rhs)
    }

    fn subtract(&mut self, lhs: TypeId, rhs: TypeId) -> TypeId {
        subtract(&mut self.arena, &self.builtins, lhs, rhs)
    }

    fn compare(&mut self, lhs: TypeId, rhs: TypeId) -> Comparison {
        compare(&mut self.arena, &self.builtins, lhs, rhs)
    }

    fn name(&mut self, id: TypeId) -> String {
        stringify(&mut self.arena, id)
    }
}

#[test]
fn test_compare_from_intersection() {
    let mut t = Types::new();
    let b = t.builtins;
    let generic = t.generic("T");
    let other = t.generic("U");
    let t_atom = t.intersect(generic, b.atom);

    // It's a value of each of its members
    assert_eq!(t.compare(t_atom, generic), Comparison::Assign);
    assert_eq!(t.compare(t_atom, b.atom), Comparison::Assign);

    // And of anything that one of them is assignable to
    let optional = t.union(vec![generic, b.nil]);
    assert_eq!(t.compare(t_atom, optional), Comparison::Assign);
    let atom_or_nil = t.union(vec![b.atom, b.nil]);
    assert_eq!(t.compare(t_atom, atom_or_nil), Comparison::Assign);
    assert_eq!(t.compare(t_atom, b.recursive_any), Comparison::Assign);

    // But not otherwise
    assert_eq!(t.compare(t_atom, b.int), Comparison::Cast);
    assert_eq!(t.compare(t_atom, other), Comparison::Invalid);
    let pair = t.any_pair();
    assert_eq!(t.compare(t_atom, pair), Comparison::Invalid);
}

#[test]
fn test_compare_to_intersection() {
    let mut t = Types::new();
    let b = t.builtins;
    let generic = t.generic("T");
    let t_atom = t.intersect(generic, b.atom);
    let t_int = t.intersect(generic, b.int);

    // A value has to be in every member
    assert_eq!(t.compare(generic, t_atom), Comparison::Invalid);
    assert_eq!(t.compare(b.atom, t_atom), Comparison::Invalid);
    assert_eq!(t.compare(b.never, t_atom), Comparison::Assign);

    // Including another intersection
    let copy = t.alloc(Type::Intersection(Intersection::new(vec![generic, b.atom])));
    assert_eq!(t.compare(t_atom, copy), Comparison::Assign);
    assert_eq!(t.compare(t_int, t_atom), Comparison::Assign);
    assert_eq!(t.compare(t_atom, t_int), Comparison::Cast);
}

#[test]
fn test_infer_intersection() {
    let mut t = Types::new();
    let b = t.builtins;
    let generic = t.generic("T");
    let inferred = t.generic("U");
    let t_atom = t.intersect(generic, b.atom);

    // A narrowed generic is inferred as a whole, rather than as its members, including through an
    // alias of a union
    let optional = t.union(vec![inferred, b.nil]);
    let alias = t.alloc(Type::Alias(Alias {
        name: None,
        inner: optional,
        generics: vec![],
    }));

    for target in [inferred, optional, alias] {
        let mut infer = Inference::new([inferred]);
        let result =
            compare_with_inference(&mut t.arena, &t.builtins, t_atom, target, Some(&mut infer));
        assert_eq!(result, Comparison::Assign);

        let lower = infer.candidates(inferred).unwrap().lower.clone();
        assert_eq!(lower.len(), 1);
        assert_eq!(t.name(lower[0]), "T & Atom");
    }

    // But it's inferred from its structure if that's what matches, without the generic adding
    // `Any` as a candidate
    let int_pair = t.pair(b.int, b.int);
    let t_int_pair = t.intersect(generic, int_pair);
    let target = t.pair(inferred, inferred);
    let mut infer = Inference::new([inferred]);
    let result = compare_with_inference(
        &mut t.arena,
        &t.builtins,
        t_int_pair,
        target,
        Some(&mut infer),
    );
    assert_eq!(result, Comparison::Assign);

    let lower = infer.candidates(inferred).unwrap().lower.clone();
    let names: Vec<String> = lower.into_iter().map(|id| t.name(id)).collect();
    assert_eq!(names, ["Int", "Int"]);
}

#[test]
fn test_check_intersection() {
    let mut t = Types::new();
    let b = t.builtins;
    let generic = t.generic("T");
    let t_atom = t.intersect(generic, b.atom);
    let pair = t.any_pair();
    let t_pair = t.intersect(generic, pair);

    // It's checked the same as its structure
    for target in [b.bytes32, b.nil, b.int] {
        let expected = check(&mut t.arena, &t.builtins, b.atom, target);
        let actual = check(&mut t.arena, &t.builtins, t_atom, target);
        assert_eq!(actual, expected);
    }

    let int_pair = t.pair(b.int, b.int);
    let expected = check(&mut t.arena, &t.builtins, pair, int_pair);
    let actual = check(&mut t.arena, &t.builtins, t_pair, int_pair);
    assert_eq!(actual, expected);

    // A pair is never an atom
    let impossible = check(&mut t.arena, &t.builtins, t_pair, b.atom);
    assert_eq!(impossible, Ok(rue_types::Check::Impossible));

    // But it can't be checked against, since it's partly generic
    let result = check(&mut t.arena, &t.builtins, b.int, t_atom);
    assert_eq!(result, Err(CheckError::GenericType));
}

#[test]
fn test_guard_branches() {
    let mut t = Types::new();
    let b = t.builtins;
    let generic = t.generic("T");
    let pair = t.any_pair();

    // Both branches of a guard on a generic keep it
    let then_atom = t.intersect(generic, b.atom);
    assert_eq!(t.name(then_atom), "T & Atom");
    let else_atom = t.subtract(generic, b.atom);
    assert_eq!(t.name(else_atom), "T & (Any, Any)");

    let then_pair = t.intersect(generic, pair);
    assert_eq!(t.name(then_pair), "T & (Any, Any)");
    let else_pair = t.subtract(generic, pair);
    assert_eq!(t.name(else_pair), "T & Atom");

    // The compiler narrows the else branch by intersecting the original type with it
    let narrowed = t.intersect(generic, else_pair);
    assert_eq!(t.name(narrowed), "T & Atom");

    // Narrowing `T | nil` to `nil` doesn't keep `T`, since it's already a `nil`
    let optional = t.union(vec![generic, b.nil]);
    let then_nil = t.intersect(optional, b.nil);
    assert_eq!(t.name(then_nil), "nil");
    let else_nil = t.subtract(optional, b.nil);
    assert_eq!(t.name(else_nil), "T");
}

#[test]
fn test_substitute_intersection() {
    let mut t = Types::new();
    let b = t.builtins;
    let generic = t.generic("T");
    let t_atom = t.intersect(generic, b.atom);

    // Substituting the generic leaves the same values, even if the members aren't compared
    let ty = substitute_with_mappings(&mut t.arena, t_atom, &HashMap::from([(generic, b.int)]));
    assert_eq!(t.name(ty), "Int & Atom");
    assert_eq!(t.compare(ty, b.int), Comparison::Assign);
    assert_eq!(t.compare(b.int, ty), Comparison::Assign);

    // And it's simplified the next time it's intersected
    let ty = t.intersect(ty, b.atom);
    assert_eq!(t.name(ty), "Int");

    // An impossible or unconstrained member is simplified away
    let ty = substitute_with_mappings(&mut t.arena, t_atom, &HashMap::from([(generic, b.never)]));
    assert_eq!(t.name(ty), "Never");
    let any = b.permissive_any;
    let ty = substitute_with_mappings(&mut t.arena, t_atom, &HashMap::from([(generic, any)]));
    assert_eq!(t.name(ty), "Atom");

    // Nested intersections are flattened
    let other = t.generic("U");
    let u_atom = t.intersect(other, b.atom);
    let ty = substitute_with_mappings(&mut t.arena, t_atom, &HashMap::from([(generic, u_atom)]));
    assert_eq!(t.name(ty), "U & Atom");
}

#[test]
fn test_stringify_intersection() {
    let mut t = Types::new();
    let b = t.builtins;
    let generic = t.generic("T");

    // Members that contain a `|` or `->` are parenthesized
    let int_or_nil = t.union(vec![b.int, b.nil]);
    let ty = t.intersect(generic, int_or_nil);
    assert_eq!(t.name(ty), "T & (Int | nil)");

    let function = t.alloc(Type::Function(FunctionType {
        generics: vec![],
        params: vec![],
        nil_terminated: true,
        ret: b.int,
    }));
    let ty = t.alloc(Type::Intersection(Intersection::new(vec![
        generic, function,
    ])));
    assert_eq!(t.name(ty), "T & (fn() -> Int)");

    // But an intersection within a union isn't
    let t_atom = t.intersect(generic, b.atom);
    let pair = t.any_pair();
    let ty = t.union(vec![t_atom, pair]);
    assert_eq!(t.name(ty), "T & Atom | (Any, Any)");
}
