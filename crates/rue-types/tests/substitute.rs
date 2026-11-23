//! Tests for substituting recursive generic types whose recursive references change the generic
//! arguments, and for rejecting the ones that can't be substituted into a finite type.

mod common;

use common::Types;

use std::collections::HashMap;

use rue_types::{
    BuiltinTypes, Comparison, TypeId, is_non_contractive, is_non_regular, non_contractive_types,
    stringify, substitute_with_mappings,
};

impl Types {
    /// `type Alt<A, B> = nil | (A, Alt<B, A>);`
    fn alt(&mut self) -> (TypeId, TypeId, TypeId) {
        let a = self.named_generic("A");
        let b = self.named_generic("B");
        let alt = self.declare_alias("Alt", [a, b]);
        let rest = self.apply(alt, [(a, b), (b, a)]);
        let pair = self.pair(a, rest);
        let nil = self.builtins.nil;
        let inner = self.union([nil, pair]);
        self.define(alt, inner);
        (alt, a, b)
    }

    /// `type L<T> = nil | (T, L<(T, T)>);`
    fn non_regular(&mut self) -> TypeId {
        let t = self.named_generic("T");
        let l = self.declare_alias("L", [t]);
        let twice = self.pair(t, t);
        let rest = self.apply(l, [(t, twice)]);
        let pair = self.pair(t, rest);
        let nil = self.builtins.nil;
        let inner = self.union([nil, pair]);
        self.define(l, inner);
        l
    }

    /// Builds a nil-terminated list type from the given item types.
    fn list_of(&mut self, items: &[TypeId]) -> TypeId {
        items
            .iter()
            .rev()
            .fold(self.builtins.nil, |rest, &item| self.pair(item, rest))
    }
}

#[test]
fn alternating_arguments() {
    let mut db = Types::new();
    let (int, bytes32) = (db.builtins.int, db.builtins.bytes32);
    let (alt, a, b) = db.alt();
    let alt_int_bytes32 = db.apply(alt, [(a, int), (b, bytes32)]);

    // The second item must be a `Bytes32`, not an `Int`
    let wrong = db.list_of(&[int, int]);
    assert_eq!(db.compare(wrong, alt_int_bytes32), Comparison::Invalid);

    let right = db.list_of(&[int, bytes32]);
    assert_eq!(db.compare(right, alt_int_bytes32), Comparison::Assign);

    let longer = db.list_of(&[int, bytes32, int]);
    assert_eq!(db.compare(longer, alt_int_bytes32), Comparison::Assign);

    let wrong_third = db.list_of(&[int, bytes32, bytes32]);
    assert_eq!(db.compare(wrong_third, alt_int_bytes32), Comparison::Cast);

    // The rest of an `Alt<Int, Bytes32>` is an `Alt<Bytes32, Int>`
    let alt_bytes32_int = db.apply(alt, [(a, bytes32), (b, int)]);
    let pair = db.pair(int, alt_bytes32_int);
    let nil = db.builtins.nil;
    let unrolled = db.union([nil, pair]);
    assert_eq!(db.compare(unrolled, alt_int_bytes32), Comparison::Assign);
    assert_eq!(db.compare(alt_int_bytes32, unrolled), Comparison::Assign);
}

#[test]
fn mapping_refers_to_mapped_generic() {
    let mut db = Types::new();
    let int = db.builtins.int;
    let t = db.named_generic("T");
    let wrapped = db.pair(t, int);

    // Calling `fn wrap<T>(value: T) -> (T, Int)` with `(value, 1)` inside a generic function
    // maps `T` to `(T, Int)`, where the inner `T` is the caller's generic, not the callee's
    let mappings = HashMap::from_iter([(t, wrapped)]);
    let result = substitute_with_mappings(&mut db.arena, wrapped, &mappings);
    assert_eq!(stringify(&mut db.arena, result), "((T, Int), Int)");
}

#[test]
fn uniform_recursion_unchanged() {
    let mut db = Types::new();
    let (int, bytes32) = (db.builtins.int, db.builtins.bytes32);
    let (list, item) = (db.builtins.list, db.builtins.list_generic);
    let list_int = db.apply(list, [(item, int)]);

    let ints = db.list_of(&[int, int, int]);
    assert_eq!(db.compare(ints, list_int), Comparison::Assign);

    let mixed = db.list_of(&[int, bytes32]);
    assert_eq!(db.compare(mixed, list_int), Comparison::Cast);

    assert!(!is_non_regular(&db.arena, list));
    assert!(!is_non_contractive(&mut db.arena, list));
}

#[test]
fn permuted_arguments_are_regular() {
    let mut db = Types::new();
    let (alt, _, _) = db.alt();
    assert!(!is_non_regular(&db.arena, alt));
    assert!(!is_non_contractive(&mut db.arena, alt));
}

#[test]
fn changed_arguments_are_non_regular() {
    let mut db = Types::new();
    let l = db.non_regular();
    assert!(is_non_regular(&db.arena, l));
}

#[test]
fn closed_arguments_are_regular() {
    let mut db = Types::new();
    let (int, bytes32) = (db.builtins.int, db.builtins.bytes32);

    // type Fixed<T> = nil | (T, Fixed<Int>);
    let t = db.named_generic("T");
    let fixed = db.declare_alias("Fixed", [t]);
    let rest = db.apply(fixed, [(t, int)]);
    let pair = db.pair(t, rest);
    let nil = db.builtins.nil;
    let inner = db.union([nil, pair]);
    db.define(fixed, inner);

    assert!(!is_non_regular(&db.arena, fixed));
    assert!(!is_non_contractive(&mut db.arena, fixed));

    // Only the first item is a `Bytes32`, the rest are `Int`
    let fixed_bytes32 = db.apply(fixed, [(t, bytes32)]);
    let right = db.list_of(&[bytes32, int, int]);
    assert_eq!(db.compare(right, fixed_bytes32), Comparison::Assign);

    let wrong = db.list_of(&[int, int]);
    assert_eq!(db.compare(wrong, fixed_bytes32), Comparison::Invalid);
}

#[test]
fn closed_arguments_that_are_rebuilt_at_each_level() {
    let mut db = Types::new();
    let (int, bytes32) = (db.builtins.int, db.builtins.bytes32);
    let nil = db.builtins.nil;

    // type Two<T, U> = nil | ((T, U), Two<Int, (T, T)>);
    // The `(T, T)` is substituted into a new pair at each level, but is always `(Int, Int)`
    // after the first two levels.
    let t = db.named_generic("T");
    let u = db.named_generic("U");
    let two = db.declare_alias("Two", [t, u]);
    let twice = db.pair(t, t);
    let rest = db.apply(two, [(t, int), (u, twice)]);
    let item = db.pair(t, u);
    let pair = db.pair(item, rest);
    let inner = db.union([nil, pair]);
    db.define(two, inner);

    assert!(!is_non_regular(&db.arena, two));
    assert!(!is_non_contractive(&mut db.arena, two));

    let two_bytes32_nil = db.apply(two, [(t, bytes32), (u, nil)]);
    let first = db.pair(bytes32, nil);
    let bytes32_twice = db.pair(bytes32, bytes32);
    let second = db.pair(int, bytes32_twice);
    let int_twice = db.pair(int, int);
    let third = db.pair(int, int_twice);
    let right = db.list_of(&[first, second, third, third]);
    assert_eq!(db.compare(right, two_bytes32_nil), Comparison::Assign);

    let wrong = db.list_of(&[first, third]);
    assert_eq!(db.compare(wrong, two_bytes32_nil), Comparison::Invalid);
}

#[test]
fn recursive_closed_argument() {
    let mut db = Types::new();
    let (int, bytes32) = (db.builtins.int, db.builtins.bytes32);
    let nil = db.builtins.nil;

    // type Nest<T> = nil | (T, Nest<Nest<Int>>);
    let t = db.named_generic("T");
    let nest = db.declare_alias("Nest", [t]);
    let nest_int = db.apply(nest, [(t, int)]);
    let rest = db.apply(nest, [(t, nest_int)]);
    let pair = db.pair(t, rest);
    let inner = db.union([nil, pair]);
    db.define(nest, inner);

    assert!(!is_non_regular(&db.arena, nest));
    assert!(!is_non_contractive(&mut db.arena, nest));

    let nest_bytes32 = db.apply(nest, [(t, bytes32)]);
    let ints = db.list_of(&[int]);
    let right = db.list_of(&[bytes32, ints, nil]);
    assert_eq!(db.compare(right, nest_bytes32), Comparison::Assign);

    // The items after the first are `Nest<Int>`, whose items after the first are `Nest<Int>`
    let ints = db.list_of(&[int, int]);
    let wrong = db.list_of(&[bytes32, ints]);
    assert_eq!(db.compare(wrong, nest_bytes32), Comparison::Invalid);
}

#[test]
fn grown_argument_passed_to_another_parameter() {
    let mut db = Types::new();
    let nil = db.builtins.nil;

    // type L<T, U> = nil | (T, L<U, (T, T)>);
    let t = db.named_generic("T");
    let u = db.named_generic("U");
    let l = db.declare_alias("L", [t, u]);
    let twice = db.pair(t, t);
    let rest = db.apply(l, [(t, u), (u, twice)]);
    let pair = db.pair(t, rest);
    let inner = db.union([nil, pair]);
    db.define(l, inner);

    assert!(is_non_regular(&db.arena, l));
}

#[test]
fn mutual_recursion_with_closed_argument() {
    let mut db = Types::new();
    let (int, bytes32) = (db.builtins.int, db.builtins.bytes32);
    let nil = db.builtins.nil;

    // type A<T> = nil | (T, B<Int>); type B<U> = (U, A<(U, U)>);
    // The `(U, U)` only ever receives `Int`, so it's always `(Int, Int)`
    let t = db.named_generic("T");
    let u = db.named_generic("U");
    let a = db.declare_alias("A", [t]);
    let b = db.declare_alias("B", [u]);

    let b_int = db.apply(b, [(u, int)]);
    let pair = db.pair(t, b_int);
    let a_inner = db.union([nil, pair]);
    db.define(a, a_inner);

    let twice = db.pair(u, u);
    let a_twice = db.apply(a, [(t, twice)]);
    let b_inner = db.pair(u, a_twice);
    db.define(b, b_inner);

    assert!(!is_non_regular(&db.arena, a));
    assert!(!is_non_regular(&db.arena, b));

    let a_bytes32 = db.apply(a, [(t, bytes32)]);
    let int_twice = db.pair(int, int);
    let right = db.list_of(&[bytes32, int, int_twice, int]);
    assert_eq!(db.compare(right, a_bytes32), Comparison::Assign);

    let wrong = db.list_of(&[bytes32, int, int, int]);
    assert_eq!(db.compare(wrong, a_bytes32), Comparison::Invalid);
}

#[test]
fn mutual_recursion() {
    let mut db = Types::new();
    let nil = db.builtins.nil;

    // type Even<T> = nil | (T, Odd<T>); type Odd<U> = (U, Even<(U, U)>);
    let t = db.named_generic("T");
    let u = db.named_generic("U");
    let even = db.declare_alias("Even", [t]);
    let odd = db.declare_alias("Odd", [u]);

    let odd_t = db.apply(odd, [(u, t)]);
    let pair = db.pair(t, odd_t);
    let even_inner = db.union([nil, pair]);
    db.define(even, even_inner);

    // Even is regular until the cycle is closed
    assert!(!is_non_regular(&db.arena, even));

    let twice = db.pair(u, u);
    let even_twice = db.apply(even, [(t, twice)]);
    let odd_inner = db.pair(u, even_twice);
    db.define(odd, odd_inner);

    assert!(is_non_regular(&db.arena, odd));
    assert!(is_non_regular(&db.arena, even));
}

#[test]
fn reference_to_non_regular_type() {
    let mut db = Types::new();
    let l = db.non_regular();

    // type Wrapper<T> = (T, L<T>); only refers to `L`, so it isn't part of the cycle
    let t = db.named_generic("T");
    let wrapper = db.declare_alias("Wrapper", [t]);
    let l_t = db.apply(l, [(t, t)]);
    let inner = db.pair(t, l_t);
    db.define(wrapper, inner);

    assert!(!is_non_regular(&db.arena, wrapper));
}

#[test]
fn permuted_arguments_without_pair_are_non_contractive() {
    let mut db = Types::new();
    let nil = db.builtins.nil;

    // type P<A, B> = P<B, A> | nil;
    let a = db.named_generic("A");
    let b = db.named_generic("B");
    let p = db.declare_alias("P", [a, b]);
    let swapped = db.apply(p, [(a, b), (b, a)]);
    let inner = db.union([swapped, nil]);
    db.define(p, inner);

    assert!(!is_non_regular(&db.arena, p));
    assert!(is_non_contractive(&mut db.arena, p));
}

#[test]
fn application_of_alias_through_another_alias_is_non_contractive() {
    let mut db = Types::new();
    let BuiltinTypes { int, nil, .. } = db.builtins;

    // type T0 = T1<Int>; type T1<A> = A | (nil, A) | T0;
    let a = db.named_generic("A");
    let t0 = db.declare_alias("T0", []);
    let t1 = db.declare_alias("T1", [a]);
    let t1_int = db.apply(t1, [(a, int)]);
    db.define(t0, t1_int);
    let nil_a = db.pair(nil, a);
    let inner = db.union([a, nil_a, t0]);
    db.define(t1, inner);

    // `T1<A>` isn't part of the cycle, but `T1<Int>` is
    assert!(is_non_contractive(&mut db.arena, t0));
    assert!(is_non_contractive(&mut db.arena, t1));
}

#[test]
fn single_application_is_non_contractive() {
    let mut db = Types::new();
    let int = db.builtins.int;

    // type G<T> = H<T, Int>; type H<A, B> = G<B> | A;
    let g_param = db.named_generic("T");
    let h_first = db.named_generic("A");
    let h_second = db.named_generic("B");
    let g = db.declare_alias("G", [g_param]);
    let h = db.declare_alias("H", [h_first, h_second]);
    let h_applied = db.apply(h, [(h_first, g_param), (h_second, int)]);
    db.define(g, h_applied);
    let g_applied = db.apply(g, [(g_param, h_second)]);
    let inner = db.union([g_applied, h_first]);
    db.define(h, inner);

    // Neither alias is part of the cycle with its own generics, only `G<Int>` and `H<Int, Int>`
    assert!(is_non_contractive(&mut db.arena, g));
    assert!(is_non_contractive(&mut db.arena, h));

    // type U = G<Int>; only refers to the cycle
    let g_int = db.apply(g, [(g_param, int)]);
    let user = db.declare_alias("U", []);
    db.define(user, g_int);
    assert!(!is_non_contractive(&mut db.arena, user));
}

#[test]
fn reference_after_guarded_reference_is_non_contractive() {
    let mut db = Types::new();
    let BuiltinTypes { int, nil, .. } = db.builtins;

    // type T0 = Id<T1<Int>>; type T1<A> = (T0, nil) | T0 | A; type Id<T> = T;
    let a = db.named_generic("A");
    let t = db.named_generic("T");
    let t0 = db.declare_alias("T0", []);
    let t1 = db.declare_alias("T1", [a]);
    let id = db.declare_alias("Id", [t]);
    let t1_int = db.apply(t1, [(a, int)]);
    let id_t1 = db.apply(id, [(t, t1_int)]);
    db.define(t0, id_t1);
    let guarded = db.pair(t0, nil);
    let inner = db.union([guarded, t0, a]);
    db.define(t1, inner);
    db.define(id, t);

    // `T0` is substituted inside the pair first, so the cycle only passes through the copy that
    // the unguarded reference to it is substituted into
    assert!(is_non_contractive(&mut db.arena, t0));
    assert!(!is_non_contractive(&mut db.arena, id));

    // Checking them together also returns `Id`, since `Id<T1<Int>>` is part of the cycle
    assert_eq!(
        non_contractive_types(&mut db.arena, &[t0, id]),
        vec![t0, id]
    );
}

/// The types substituting makes, in the order they're made, when each of `count` generics is
/// mapped to a pair of its own: through the mappings substituting is given if `apply` is unset, or
/// through an application of an alias with those generics otherwise.
fn substituted_in_order(count: usize, apply: bool) -> Vec<String> {
    let mut db = Types::new();
    let BuiltinTypes { int, bytes, .. } = db.builtins;
    let generics: Vec<TypeId> = (0..count)
        .map(|i| db.named_generic(&format!("T{i}")))
        .collect();
    let list = db.list_of(&generics);
    let mappings: Vec<(TypeId, TypeId)> = generics
        .iter()
        .enumerate()
        .map(|(i, &generic)| {
            let item = if i % 2 == 0 { int } else { bytes };
            let inner = db.pair(item, int);
            (generic, db.pair(inner, bytes))
        })
        .collect();

    let (ty, mappings) = if apply {
        let alias = db.declare_alias("A", generics.as_slice());
        db.define(alias, list);
        (db.apply(alias, mappings.iter().copied()), HashMap::new())
    } else {
        (list, mappings.into_iter().collect())
    };

    let before = db.arena.len();
    substitute_with_mappings(&mut db.arena, ty, &mappings);
    let made: Vec<TypeId> = db.arena.iter().skip(before).map(|(id, _)| id).collect();
    made.into_iter()
        .map(|id| format!("{id:?} {}", stringify(&mut db.arena, id)))
        .collect()
}

/// Substituting makes its types in the same order every time, whatever order the `HashMap`s of
/// the mappings it's given and of an application's arguments iterate in, which `std`'s differs for
/// each map (and each process). Which types are made first decides the work that comparing them
/// does later, which decides whether a comparison near the step limit gives up.
#[test]
fn substituting_makes_types_in_the_same_order() {
    for count in [3, 6] {
        for apply in [false, true] {
            let first = substituted_in_order(count, apply);
            for _ in 0..10 {
                assert_eq!(substituted_in_order(count, apply), first, "{count} {apply}");
            }
        }
    }
}
