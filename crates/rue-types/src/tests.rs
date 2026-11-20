//! Tests for comparison, inference, and substitution across the different kinds of types.

use std::collections::HashMap;

use id_arena::Arena;
use indexmap::IndexSet;
use rue_diagnostic::Name;

use crate::{
    Alias, Apply, Atom, AtomRestriction, AtomSemantic, BranchId, BuiltinTypes, CandidateKind,
    Choices, Comparison, FunctionParameter, FunctionType, Generic, Inference, InferenceCandidates,
    MAX_SPLIT_PAIRS, Pair, Struct, Type, TypeId, Union, add_expected_bounds, check, compare,
    compare_with_inference, extract_atoms, extract_functions, extract_pairs, extract_structs,
    infer_from_expected, is_non_contractive, resolve_inference, resolve_inference_with_bounds,
    resolve_inference_with_fallback, stringify, substitute, substitute_with_mappings, subtract,
    unwrap_semantic,
};

struct Db {
    arena: Arena<Type>,
    builtins: BuiltinTypes,
}

impl Db {
    fn new() -> Self {
        let mut arena = Arena::new();
        let builtins = BuiltinTypes::new(&mut arena);
        Self { arena, builtins }
    }

    fn alloc(&mut self, ty: Type) -> TypeId {
        self.arena.alloc(ty)
    }

    fn placeholder(&mut self) -> TypeId {
        self.alloc(Type::Unresolved)
    }

    fn set(&mut self, id: TypeId, ty: Type) {
        *self.arena.get_mut(id).unwrap() = ty;
    }

    fn pair(&mut self, first: TypeId, rest: TypeId) -> TypeId {
        self.alloc(Type::Pair(Pair::new(first, rest)))
    }

    fn union(&mut self, types: &[TypeId]) -> TypeId {
        self.alloc(Type::Union(Union::new(types.to_vec())))
    }

    fn generic(&mut self, name: &str) -> TypeId {
        self.alloc(Type::Generic(Generic {
            name: Some(Name::new(name, None)),
        }))
    }

    fn function(&mut self, params: &[TypeId], ret: TypeId) -> TypeId {
        self.function_with(&[], params, true, ret)
    }

    fn function_with(
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
    fn alias(&mut self, name: &str, generics: &[TypeId], inner: TypeId) -> TypeId {
        let inner = self.alloc(Type::Ref(inner));
        self.alloc(Type::Alias(Alias {
            name: Some(Name::new(name, None)),
            inner,
            generics: generics.to_vec(),
        }))
    }

    /// Declares an alias whose inner type can be filled in later, to allow recursion.
    fn declare_alias(&mut self, name: &str, generics: &[TypeId]) -> TypeId {
        let inner = self.placeholder();
        self.alloc(Type::Alias(Alias {
            name: Some(Name::new(name, None)),
            inner,
            generics: generics.to_vec(),
        }))
    }

    /// Declares a struct whose inner type can be filled in later, to allow recursion.
    fn declare_struct(&mut self, name: &str, generics: &[TypeId]) -> TypeId {
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

    fn define(&mut self, id: TypeId, resolved: TypeId) {
        let (Type::Alias(Alias { inner, .. }) | Type::Struct(Struct { inner, .. })) =
            self.arena[id].clone()
        else {
            panic!("expected an alias or struct");
        };
        self.set(inner, Type::Ref(resolved));
    }

    fn strukt(&mut self, name: &str, inner: TypeId) -> TypeId {
        let id = self.declare_struct(name, &[]);
        self.define(id, inner);
        id
    }

    fn apply(&mut self, inner: TypeId, generics: &[(TypeId, TypeId)]) -> TypeId {
        self.alloc(Type::Apply(Apply::new(
            inner,
            generics.iter().copied().collect::<HashMap<_, _>>(),
        )))
    }

    fn list(&mut self, item: TypeId) -> TypeId {
        let (list, generic) = (self.builtins.list, self.builtins.list_generic);
        self.apply(list, &[(generic, item)])
    }

    fn cmp(&mut self, lhs: TypeId, rhs: TypeId) -> Comparison {
        compare(&mut self.arena, &self.builtins, lhs, rhs)
    }

    fn infer(&mut self, lhs: TypeId, rhs: TypeId, infer: &mut Inference) -> Comparison {
        compare_with_inference(&mut self.arena, &self.builtins, lhs, rhs, Some(infer))
    }

    fn name(&mut self, id: TypeId) -> String {
        stringify(&mut self.arena, id)
    }

    fn literal(&mut self, semantic: AtomSemantic, value: &[u8]) -> TypeId {
        self.alloc(Type::Atom(Atom::new(
            semantic,
            Some(AtomRestriction::Value(value.to_vec().into())),
        )))
    }
}

mod functions {
    use super::*;

    #[test]
    fn identical() {
        let mut db = Db::new();
        let (int, bytes32) = (db.builtins.int, db.builtins.bytes32);
        let a = db.function(&[int], bytes32);
        let b = db.function(&[int], bytes32);
        assert_eq!(db.cmp(a, b), Comparison::Assign);
    }

    #[test]
    fn params_are_contravariant() {
        let mut db = Db::new();
        let (bytes, bytes32) = (db.builtins.bytes, db.builtins.bytes32);
        let takes_bytes = db.function(&[bytes], bytes32);
        let takes_bytes32 = db.function(&[bytes32], bytes32);

        // A function that accepts any bytes can be used where only 32 bytes will be passed
        assert_eq!(db.cmp(takes_bytes, takes_bytes32), Comparison::Assign);

        // But not the other way around, since it could be called with arbitrary bytes
        assert_eq!(db.cmp(takes_bytes32, takes_bytes), Comparison::Invalid);
    }

    #[test]
    fn return_is_covariant() {
        let mut db = Db::new();
        let (bytes, bytes32, int) = (db.builtins.bytes, db.builtins.bytes32, db.builtins.int);
        let returns_bytes = db.function(&[int], bytes);
        let returns_bytes32 = db.function(&[int], bytes32);
        assert_eq!(db.cmp(returns_bytes32, returns_bytes), Comparison::Assign);
        assert_eq!(db.cmp(returns_bytes, returns_bytes32), Comparison::Invalid);
    }

    #[test]
    fn cast_params_and_return() {
        let mut db = Db::new();
        let (bytes, int) = (db.builtins.bytes, db.builtins.int);

        // Reinterpreting the arguments or return value with the same representation is a cast
        let takes_int = db.function(&[int], int);
        let takes_bytes = db.function(&[bytes], int);
        assert_eq!(db.cmp(takes_int, takes_bytes), Comparison::Cast);
        assert_eq!(db.cmp(takes_bytes, takes_int), Comparison::Cast);

        let returns_bytes = db.function(&[int], bytes);
        assert_eq!(db.cmp(takes_int, returns_bytes), Comparison::Cast);
    }

    #[test]
    fn cast_cannot_narrow_params() {
        let mut db = Db::new();
        let (bytes, bytes32) = (db.builtins.bytes, db.builtins.bytes32);
        let takes_bytes32 = db.function(&[bytes32], bytes32);
        let takes_bytes = db.function(&[bytes], bytes32);

        // This would let the function be called with a value that isn't 32 bytes long
        assert_eq!(db.cmp(takes_bytes32, takes_bytes), Comparison::Invalid);
    }

    #[test]
    fn arity() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let one = db.function(&[int], int);
        let two = db.function(&[int, int], int);
        let zero = db.function(&[], int);
        assert_eq!(db.cmp(one, two), Comparison::Invalid);
        assert_eq!(db.cmp(two, one), Comparison::Invalid);
        assert_eq!(db.cmp(zero, one), Comparison::Invalid);
        assert_eq!(db.cmp(zero, zero), Comparison::Assign);
    }

    #[test]
    fn spread() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let bytes32 = db.builtins.bytes32;
        let bytes = db.builtins.bytes;
        let list_bytes = db.list(bytes);
        let list_bytes32 = db.list(bytes32);
        let spread_bytes = db.function_with(&[], &[int, list_bytes], false, int);
        let spread_bytes32 = db.function_with(&[], &[int, list_bytes32], false, int);
        let fixed = db.function_with(&[], &[int, list_bytes], true, int);

        assert_eq!(db.cmp(spread_bytes, spread_bytes32), Comparison::Assign);
        assert_eq!(db.cmp(spread_bytes32, spread_bytes), Comparison::Invalid);
        assert_eq!(db.cmp(spread_bytes, fixed), Comparison::Invalid);
        assert_eq!(db.cmp(fixed, spread_bytes), Comparison::Invalid);
    }

    #[test]
    fn unions_of_functions() {
        let mut db = Db::new();
        let (bytes, bytes32, nil, int) = (
            db.builtins.bytes,
            db.builtins.bytes32,
            db.builtins.nil,
            db.builtins.int,
        );
        let takes_bytes = db.function(&[bytes], int);
        let takes_bytes32 = db.function(&[bytes32], int);
        let optional_bytes32 = db.union(&[nil, takes_bytes32]);
        let optional_bytes = db.union(&[nil, takes_bytes]);

        assert_eq!(db.cmp(takes_bytes, optional_bytes32), Comparison::Assign);
        assert_eq!(db.cmp(takes_bytes32, optional_bytes), Comparison::Invalid);
        assert_eq!(db.cmp(optional_bytes, optional_bytes32), Comparison::Assign);
        assert_eq!(
            db.cmp(optional_bytes32, optional_bytes),
            Comparison::Invalid
        );
        assert_eq!(db.cmp(optional_bytes32, takes_bytes32), Comparison::Invalid);

        let either = db.union(&[takes_bytes, takes_bytes32]);
        assert_eq!(db.cmp(either, takes_bytes32), Comparison::Assign);
        assert_eq!(db.cmp(either, takes_bytes), Comparison::Invalid);
    }

    #[test]
    fn functions_and_other_types() {
        let mut db = Db::new();
        let (int, atom, any, recursive_any) = (
            db.builtins.int,
            db.builtins.atom,
            db.builtins.permissive_any,
            db.builtins.recursive_any,
        );
        let function = db.function(&[int], int);
        let pair = db.pair(int, int);

        assert_eq!(db.cmp(function, any), Comparison::Assign);
        assert_eq!(db.cmp(function, recursive_any), Comparison::Assign);
        assert_eq!(db.cmp(any, function), Comparison::Invalid);
        assert_eq!(db.cmp(recursive_any, function), Comparison::Invalid);
        assert_eq!(db.cmp(function, atom), Comparison::Invalid);
        assert_eq!(db.cmp(atom, function), Comparison::Invalid);
        assert_eq!(db.cmp(function, pair), Comparison::Invalid);
        assert_eq!(db.cmp(pair, function), Comparison::Invalid);
    }

    #[test]
    fn generic_function_types_are_instantiated() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let t = db.generic("T");
        let u = db.generic("U");
        let identity_t = db.function_with(&[t], &[t], true, t);
        let identity_u = db.function_with(&[u], &[u], true, u);
        let concrete = db.function(&[int], int);

        // A generic function can be instantiated to a concrete or renamed function type, but a
        // concrete function doesn't work for every type
        assert_eq!(db.cmp(identity_t, identity_t), Comparison::Assign);
        assert_eq!(db.cmp(concrete, identity_t), Comparison::Invalid);
        assert_eq!(db.cmp(identity_t, concrete), Comparison::Assign);
        assert_eq!(db.cmp(identity_t, identity_u), Comparison::Assign);
    }

    #[test]
    fn free_generics_are_not_bound_by_function_types() {
        let mut db = Db::new();
        let u = db.generic("U");

        // `fn(p0: U) -> U` with a free `U` (such as in the body of `fn foo<U>`), and the type
        // of `foo` itself, which is generic over the same `U`
        let free = db.function(&[u], u);
        let identity = db.function_with(&[u], &[u], true, u);

        // The free `U` is a single type, but the generic function can be instantiated with it
        assert_eq!(db.cmp(free, identity), Comparison::Invalid);
        assert_eq!(db.cmp(identity, free), Comparison::Assign);
    }

    #[test]
    fn nested_function_generics_are_distinct() {
        let mut db = Db::new();
        let int = db.builtins.int;

        // `fn<U>(p0: U) -> (U, fn<U>(p0: U) -> (U, Int))`, where the inner `U` shadows the outer
        let u = db.generic("U");
        let inner = db.pair(u, int);
        let inner = db.function_with(&[u], &[u], true, inner);
        let outer = db.pair(u, inner);
        let expected = db.function_with(&[u], &[u], true, outer);

        // `fn<V>(p0: V) -> (V, fn(p0: V) -> (V, Int))` only takes the outer argument's type again
        let v = db.generic("V");
        let inner = db.pair(v, int);
        let inner = db.function(&[v], inner);
        let outer = db.pair(v, inner);
        let same = db.function_with(&[v], &[v], true, outer);
        assert_eq!(db.cmp(same, expected), Comparison::Invalid);

        // `fn<V>(p0: V) -> (V, fn<W>(p0: W) -> (W, Int))` takes any type
        let w = db.generic("W");
        let inner = db.pair(w, int);
        let inner = db.function_with(&[w], &[w], true, inner);
        let outer = db.pair(v, inner);
        let distinct = db.function_with(&[v], &[v], true, outer);
        assert_eq!(db.cmp(distinct, expected), Comparison::Assign);
    }

    /// A generic function compared inside of another can still be instantiated while the other's
    /// bounds are checked (#363): `fn<B>(f: fn() -> B) -> B` is assignable to
    /// `fn(f: fn<T>() -> (T, T)) -> (Int, Int)`, since `B ≥ (T', T')` and `B ≤ (Int, Int)` hold
    /// with `T'` as `Int`. But each such instance is instantiated once, along with its own bounds:
    /// with `fn<T>(x: T) -> (T, T)` passed `Int`, `T' ≥ Int`, so `(Int, Bytes32)` doesn't hold.
    #[test]
    fn inner_instances_are_instantiated_for_outer_bounds() {
        let mut db = Db::new();
        let BuiltinTypes {
            int, bytes32, nil, ..
        } = db.builtins;

        let compare = |db: &mut Db, param: Option<TypeId>, expected: TypeId| {
            let b = db.generic("B");
            let t = db.generic("T");
            let both = db.pair(t, t);
            let params: Vec<TypeId> = param.map(|_| t).into_iter().collect();
            let pairs = db.function_with(&[t], &params, true, both);
            let thunk_params: Vec<TypeId> = param.into_iter().collect();
            let thunk = db.function(&thunk_params, b);
            let call = db.function_with(&[b], &[thunk], true, b);
            let target = db.function(&[pairs], expected);
            db.cmp(call, target)
        };

        let ints = db.pair(int, int);
        let mixed = db.pair(int, bytes32);
        assert_eq!(compare(&mut db, None, ints), Comparison::Assign);
        assert_eq!(compare(&mut db, None, mixed), Comparison::Assign);
        assert_eq!(compare(&mut db, Some(int), ints), Comparison::Assign);
        assert_eq!(compare(&mut db, Some(int), mixed), Comparison::Invalid);
        assert_eq!(compare(&mut db, Some(int), nil), Comparison::Invalid);
    }

    /// An inner instance is instantiated once for all of the outer bounds: with
    /// `fn<B>(f: fn() -> B) -> (B, B)` and `fn<T>() -> fn(x: T) -> T`, `B` flows into both of the
    /// return type's functions, so one `T'` has to hold `Int` and be assignable to `Int | nil` for
    /// the first, and hold `nil` and be assignable to `nil | Bytes32` for the second, which no
    /// single type does, although each alone has one.
    #[test]
    fn inner_instances_are_instantiated_once() {
        let mut db = Db::new();
        let BuiltinTypes {
            int, nil, bytes32, ..
        } = db.builtins;

        let compare = |db: &mut Db, second_ret: TypeId| {
            let b = db.generic("B");
            let t = db.generic("T");
            let thunk = db.function(&[], b);
            let both = db.pair(b, b);
            let call = db.function_with(&[b], &[thunk], true, both);
            let identity = db.function(&[t], t);
            let inner = db.function_with(&[t], &[], true, identity);
            let int_or_nil = db.union(&[int, nil]);
            let first = db.function(&[int], int_or_nil);
            let second = db.function(&[nil], second_ret);
            let ret = db.pair(first, second);
            let target = db.function(&[inner], ret);
            db.cmp(call, target)
        };

        let nil_or_bytes32 = db.union(&[nil, bytes32]);
        let int_or_nil = db.union(&[int, nil]);
        assert_eq!(compare(&mut db, nil_or_bytes32), Comparison::Invalid);
        assert_eq!(compare(&mut db, int_or_nil), Comparison::Assign);
    }

    /// Reopened instances stay rigid when a union member is checked without inference.
    /// `fn<B>(f: fn(x: Int) -> B) -> B` against `fn(f: fn<T>(x: T) -> (H, Int)) -> R` holds with
    /// `R` a union of a member that fails and `(H', Int)`, in either order and with inference as
    /// well: checking the failing member without inference mustn't leave a bound on `T'`, next to
    /// `T' ≥ Int`. With `H = T` and `(nil, nil)`, it would leave `T' ≤ nil`. With
    /// `H = fn(g: fn(t: T, z: fn<Z>() -> Z) -> nil) -> nil` and
    /// `(fn(g: fn<D>(t: D, z: fn() -> Int) -> D) -> nil, nil)`, the failing member's own function
    /// comparison reopens `Z'` while `T'` is reopened, and it would leave `T' ≤ nil` through `D`.
    #[test]
    fn reopened_instances_stay_rigid_without_inference() {
        let mut db = Db::new();
        let BuiltinTypes { int, nil, .. } = db.builtins;

        let compare = |db: &mut Db, nested: bool, failing_first: bool| {
            let ret_b = db.generic("B");
            let item = db.generic("T");
            let unused = db.generic("X");
            let callback = db.function(&[int], ret_b);
            let call = db.function_with(&[ret_b], &[callback], true, ret_b);

            let (held, failing, holding) = if nested {
                let any = db.generic("Z");
                let given = db.generic("D");
                let any_z = db.function_with(&[any], &[], true, any);
                let takes_t = db.function(&[item, any_z], nil);
                let held = db.function(&[takes_t], nil);
                let give_int = db.function(&[], int);
                let returns_d = db.function_with(&[given], &[given, give_int], true, given);
                let failing = db.function(&[returns_d], nil);
                let takes_int = db.function(&[int, give_int], nil);
                let holding = db.function(&[takes_int], nil);
                (held, db.pair(failing, nil), db.pair(holding, int))
            } else {
                (item, db.pair(nil, nil), db.pair(int, int))
            };

            let tagged = db.pair(held, int);
            let tag = db.function_with(&[item], &[item], true, tagged);
            let members = if failing_first {
                [failing, holding]
            } else {
                [holding, failing]
            };
            let ret = db.union(&members);
            let target = db.function(&[tag], ret);
            let plain = db.cmp(call, target);
            let mut infer = Inference::new([unused]);
            (plain, db.infer(call, target, &mut infer))
        };

        let both = (Comparison::Assign, Comparison::Assign);

        for nested in [false, true] {
            assert_eq!(compare(&mut db, nested, true), both);
            assert_eq!(compare(&mut db, nested, false), both);
        }
    }

    /// The outer bounds can add lower bounds to an inner instance too, which are checked against
    /// its own upper bounds: `fn<T>(x: fn(y: T) -> nil) -> fn(x: T) -> nil` passed a
    /// `fn(y: Bytes32) -> nil` has `T' ≤ Bytes32`, so it isn't a `fn(x: Int) -> nil`.
    #[test]
    fn inner_instances_get_lower_bounds() {
        let mut db = Db::new();
        let BuiltinTypes {
            int, nil, bytes32, ..
        } = db.builtins;

        let compare = |db: &mut Db, param: TypeId| {
            let b = db.generic("B");
            let t = db.generic("T");
            let callback = db.function(&[bytes32], nil);
            let given = db.function(&[callback], b);
            let call = db.function_with(&[b], &[given], true, b);
            let takes = db.function(&[t], nil);
            let inner_param = db.function(&[t], nil);
            let inner = db.function_with(&[t], &[inner_param], true, takes);
            let ret = db.function(&[param], nil);
            let target = db.function(&[inner], ret);
            db.cmp(call, target)
        };

        assert_eq!(compare(&mut db, int), Comparison::Invalid);
        assert_eq!(compare(&mut db, bytes32), Comparison::Assign);
    }

    /// The instances nested further in can be instantiated too, as the inner ones' bounds are
    /// checked again. With `fn<B>(f: fn(g: Q) -> B) -> B` against `fn(f: P) -> (R, R)`, where
    /// `P = fn<T>(g: fn(x: Int) -> T) -> (T, T)` and `Q = fn<U>(x: X) -> (U, U)`, `T' ≥ (U', U')`
    /// and `T' ≤ R`. With `X` as `U`, so that `U' ≥ Int`, it holds for `R` as `(Int, Int)`, with
    /// `U'` as `Int`, and not for `(Int, Bytes32)`, which `U'` can't be both of. With `X` as `Int`,
    /// `U'` can be `Never`, which is both, as when `Q` is compared with `fn(x: Int) -> R` itself.
    #[test]
    fn deeper_instances_are_instantiated() {
        let mut db = Db::new();
        let BuiltinTypes { int, bytes32, .. } = db.builtins;

        let compare = |db: &mut Db, bounded: bool, result: TypeId| {
            let [outer, inner, deeper] = [db.generic("B"), db.generic("T"), db.generic("U")];
            let param = if bounded { deeper } else { int };
            let deepers = db.pair(deeper, deeper);
            let q = db.function_with(&[deeper], &[param], true, deepers);
            let given = db.function(&[q], outer);
            let call = db.function_with(&[outer], &[given], true, outer);
            let callback = db.function(&[int], inner);
            let inners = db.pair(inner, inner);
            let p = db.function_with(&[inner], &[callback], true, inners);
            let ret = db.pair(result, result);
            let target = db.function(&[p], ret);
            db.cmp(call, target)
        };

        let mixed = db.pair(int, bytes32);
        let ints = db.pair(int, int);
        assert_eq!(compare(&mut db, true, ints), Comparison::Assign);
        assert_eq!(compare(&mut db, true, mixed), Comparison::Invalid);
        assert_eq!(compare(&mut db, false, mixed), Comparison::Assign);
    }

    /// An instance made while another one's bounds are checked again is rigid while the next ones'
    /// are, since its own check has finished. `fn<B>(f: fn(g: Q, b: B) -> Int, w: B) -> Int`
    /// against `fn(f: P, w: W) -> Int`, where `W = fn<V>(v: V) -> V`,
    /// `Q = fn<U>(h: fn(v: U) -> (Int, Int), k: U) -> Int` and
    /// `P = fn<T>(g: fn(h: T, k: Int) -> Int, b: T) -> Int`, needs `Int ≤ U' ≤ V' ≤ (Int, Int)`
    /// for the instance `V'` that checking `T'`'s bounds again makes, which has no instantiation.
    #[test]
    fn instances_made_while_checking_again_stay_rigid() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let [outer, item, deeper, value] = [
            db.generic("B"),
            db.generic("T"),
            db.generic("U"),
            db.generic("V"),
        ];
        let identity = db.function_with(&[value], &[value], true, value);
        let ints = db.pair(int, int);
        let gives = db.function(&[deeper], ints);
        let gives_pair = db.function_with(&[deeper], &[gives, deeper], true, int);
        let takes = db.function(&[item, int], int);
        let takes_int = db.function_with(&[item], &[takes, item], true, int);
        let given = db.function(&[gives_pair, outer], int);
        let call = db.function_with(&[outer], &[given, outer], true, int);
        let target = db.function(&[takes_int, identity], int);
        assert_eq!(db.cmp(call, target), Comparison::Invalid);
    }

    /// Whether each instance can be instantiated again is found once, so that many instances that
    /// each refer to the next one twice don't take exponentially long:
    /// `A_k = fn<Z>(a: Z, b: Z, k: B_{k-1}<Z>) -> Int` and `B_k<X> = fn(a: X, b: X, k: A_{k-1}) -> Int`.
    #[test]
    fn deep_instances_are_checked_once() {
        type Build = Box<dyn Fn(&mut Db, TypeId) -> TypeId>;

        // `A_depth`, and what builds `B_depth<X>` from `X`
        fn nested(db: &mut Db, depth: usize) -> (TypeId, Build) {
            let int = db.builtins.int;
            let z = db.generic("Z");

            if depth == 0 {
                let a = db.function_with(&[z], &[z, z], true, int);
                let b: Build = Box::new(move |db, x| db.function(&[x, x], int));
                (a, b)
            } else {
                let (a_prev, b_prev) = nested(db, depth - 1);
                let bz = b_prev(db, z);
                let a = db.function_with(&[z], &[z, z, bz], true, int);
                let b: Build = Box::new(move |db, x| db.function(&[x, x, a_prev], int));
                (a, b)
            }
        }

        let mut db = Db::new();
        let int = db.builtins.int;
        let (a, b) = nested(&mut db, 32);
        let y = db.generic("Y");
        let by = b(&mut db, y);
        let top = db.function_with(&[y], &[by], true, int);
        let target = db.function(&[a], int);
        assert_eq!(db.cmp(top, target), Comparison::Assign);
    }

    /// An inner instance compared with a type that refers to it, while an enclosing comparison
    /// checks bounds that refer to it on both sides, isn't bounded by that type (an occurs check):
    /// `fn<B>(function: fn(x: B, y: Int) -> B) -> Int` against `fn(function: F) -> Int`, with
    /// `F = fn<T>(x: (T, X), y: T) -> (T, T)`, compares `(T', T') ≤ (T', X)`, which holds for `X`
    /// as `T` or `T | nil` without bounding `T'` by itself, so its `T' ≥ Int` is still enough.
    #[test]
    fn inner_instances_compared_with_themselves() {
        let mut db = Db::new();
        let BuiltinTypes { int, nil, .. } = db.builtins;

        let compare = |db: &mut Db, optional: bool| {
            let [outer, inner] = [db.generic("B"), db.generic("T")];
            let second = if optional {
                db.union(&[inner, nil])
            } else {
                inner
            };
            let param = db.pair(inner, second);
            let ret = db.pair(inner, inner);
            let function = db.function_with(&[inner], &[param, inner], true, ret);
            let callback = db.function(&[outer, int], outer);
            let apply = db.function_with(&[outer], &[callback], true, int);
            let target = db.function(&[function], int);
            db.cmp(apply, target)
        };

        assert_eq!(compare(&mut db, false), Comparison::Assign);
        assert_eq!(compare(&mut db, true), Comparison::Assign);
    }

    #[test]
    fn recursive_instantiations_are_checked() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let pair = db.pair(int, int);

        // type F = fn<U>(x: U) -> (U, F);
        let f = db.declare_alias("F", &[]);
        let u = db.generic("U");
        let ret = db.pair(u, f);
        let function = db.function_with(&[u], &[u], true, ret);
        db.define(f, function);

        // type R = (Int, G2); type G2 = fn(x: X) -> R; type G = fn(x: Int) -> R;
        for (param, expected) in [(int, Comparison::Assign), (pair, Comparison::Invalid)] {
            let r = db.declare_alias("R", &[]);
            let g2 = db.declare_alias("G2", &[]);
            let inner = db.pair(int, g2);
            db.define(r, inner);
            let function = db.function(&[param], r);
            db.define(g2, function);
            let g = db.function(&[int], r);

            // `U` is instantiated again for `G2`, where it must also be returned as an `Int`
            assert_eq!(db.cmp(f, g), expected);
        }
    }

    #[test]
    fn recursive_function_type() {
        let mut db = Db::new();
        let int = db.builtins.int;

        // type F = fn(f: F) -> Int;
        let f = db.declare_alias("F", &[]);
        let function = db.function(&[f], int);
        db.define(f, function);

        // type G = fn(g: G) -> Int;
        let g = db.declare_alias("G", &[]);
        let function = db.function(&[g], int);
        db.define(g, function);

        assert!(!is_non_contractive(&mut db.arena, f));
        assert_eq!(db.cmp(f, f), Comparison::Assign);
        assert_eq!(db.cmp(f, g), Comparison::Assign);
    }
}

mod structs {
    use super::*;

    #[test]
    fn nominal() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let shape = db.pair(int, nil);
        let a = db.strukt("A", shape);
        let b = db.strukt("B", shape);

        assert_eq!(db.cmp(a, a), Comparison::Assign);

        // Structs with the same shape are semantically different
        assert_eq!(db.cmp(a, b), Comparison::Cast);
        assert_eq!(db.cmp(b, a), Comparison::Cast);

        // Structs can be cast to and from their shape
        assert_eq!(db.cmp(a, shape), Comparison::Cast);
        assert_eq!(db.cmp(shape, a), Comparison::Cast);

        // But not to an incompatible shape
        let bytes32 = db.builtins.bytes32;
        let other = db.pair(bytes32, nil);
        assert_eq!(db.cmp(a, other), Comparison::Invalid);
        assert_eq!(db.cmp(a, int), Comparison::Invalid);
    }

    #[test]
    fn unions_of_structs() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let shape = db.pair(int, nil);
        let a = db.strukt("A", shape);
        let b = db.strukt("B", shape);
        let either = db.union(&[a, b]);
        let optional = db.union(&[a, nil]);

        assert_eq!(db.cmp(a, either), Comparison::Assign);
        assert_eq!(db.cmp(either, a), Comparison::Cast);
        assert_eq!(db.cmp(a, optional), Comparison::Assign);
        assert_eq!(db.cmp(nil, optional), Comparison::Assign);
        assert_eq!(db.cmp(optional, a), Comparison::Invalid);
    }

    #[test]
    fn generic() {
        let mut db = Db::new();
        let (int, nil, bytes, bytes32) = (
            db.builtins.int,
            db.builtins.nil,
            db.builtins.bytes,
            db.builtins.bytes32,
        );

        // struct Box<T> { value: T }
        let t = db.generic("T");
        let boxed = db.declare_struct("Box", &[t]);
        let inner = db.pair(t, nil);
        db.define(boxed, inner);

        let box_int = db.apply(boxed, &[(t, int)]);
        let box_int_2 = db.apply(boxed, &[(t, int)]);
        let box_bytes = db.apply(boxed, &[(t, bytes)]);
        let box_bytes32 = db.apply(boxed, &[(t, bytes32)]);

        assert_eq!(db.name(box_int), "Box<Int>");
        assert_eq!(db.cmp(box_int, box_int_2), Comparison::Assign);
        assert_eq!(db.cmp(box_bytes32, box_bytes), Comparison::Assign);
        assert_eq!(db.cmp(box_int, box_bytes), Comparison::Cast);
        assert_eq!(db.cmp(box_bytes, box_bytes32), Comparison::Invalid);
        assert_eq!(db.cmp(box_int, box_bytes32), Comparison::Invalid);
    }

    #[test]
    fn rest_field() {
        let mut db = Db::new();
        let (int, bytes, bytes32) = (db.builtins.int, db.builtins.bytes, db.builtins.bytes32);

        // struct R { x: Int, ...rest: Bytes32 }
        let shape = db.pair(int, bytes32);
        let r = db.strukt("R", shape);

        let pair = db.pair(int, bytes32);
        let wider = db.pair(int, bytes);
        assert_eq!(db.cmp(r, pair), Comparison::Cast);
        assert_eq!(db.cmp(pair, r), Comparison::Cast);
        assert_eq!(db.cmp(wider, r), Comparison::Invalid);
    }

    #[test]
    fn nested() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);

        // struct Inner { ...v: Int }
        // struct Outer { ...i: Inner }
        let inner = db.strukt("Inner", int);
        let outer = db.strukt("Outer", inner);

        // The same shape, but semantically different at either level
        let other_inner = db.strukt("OtherInner", int);
        let other_outer = db.strukt("OtherOuter", other_inner);
        let rewrapped = db.strukt("Rewrapped", inner);

        let t = db.generic("T");
        let boxed = db.declare_struct("Box", &[t]);
        let shape = db.pair(t, nil);
        db.define(boxed, shape);

        // Positions to compare the structs in, where the first three hold only the struct itself
        let positions = |db: &mut Db, ty: TypeId| {
            let alias = db.alias("A", &[], ty);
            let nested_alias = db.alias("B", &[], alias);
            let optional = db.union(&[ty, nil]);
            let optional_alias = db.union(&[alias, nil]);
            let alias_optional = db.alias("C", &[], optional);
            let pair = db.pair(optional, int);
            let applied = db.apply(boxed, &[(t, optional)]);
            [
                ty,
                alias,
                nested_alias,
                optional,
                optional_alias,
                alias_optional,
                pair,
                applied,
            ]
        };

        let lhs = positions(&mut db, outer);
        let same = positions(&mut db, outer);
        let different = [other_outer, rewrapped, inner].map(|ty| positions(&mut db, ty));

        for (i, &lhs) in lhs.iter().enumerate() {
            let targets = match i {
                0..3 => 0..6,
                3..6 => 3..6,
                _ => i..i + 1,
            };

            for j in targets {
                assert_eq!(db.cmp(lhs, same[j]), Comparison::Assign);

                for different in &different {
                    assert_eq!(db.cmp(lhs, different[j]), Comparison::Cast);
                }
            }
        }

        // An identical copy of an alias of nested structs
        let copy = db.alias("A", &[], outer);
        assert_eq!(db.cmp(same[1], copy), Comparison::Assign);
        assert_eq!(db.cmp(copy, same[1]), Comparison::Assign);

        // A union of the struct and an alias of it
        let union = db.union(&[outer, same[1]]);
        assert_eq!(db.cmp(union, outer), Comparison::Assign);

        for different in &different {
            assert_eq!(db.cmp(union, different[0]), Comparison::Cast);
        }
    }

    #[test]
    fn unions_of_instances() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);

        // struct Box<T> { ...value: T }
        let t = db.generic("T");
        let boxed = db.declare_struct("Box", &[t]);
        db.define(boxed, t);

        // Like pairs, structs aren't distributed over unions
        let optional = db.union(&[int, nil]);
        let box_optional = db.apply(boxed, &[(t, optional)]);
        let box_int = db.apply(boxed, &[(t, int)]);
        let box_nil = db.apply(boxed, &[(t, nil)]);
        let either = db.union(&[box_int, box_nil]);
        assert_eq!(db.cmp(box_optional, either), Comparison::Cast);
        assert_eq!(db.cmp(either, box_optional), Comparison::Assign);

        // But they can still be cast to a union of their shape
        let pair = db.pair(int, int);
        let shape = db.union(&[int, pair]);
        let w = db.strukt("W", shape);
        assert_eq!(db.cmp(w, shape), Comparison::Cast);
        assert_eq!(db.cmp(box_optional, optional), Comparison::Cast);

        // Instances of different structs with the same shape
        let a = db.strukt("A", int);
        let b = db.strukt("B", int);
        let box_a = db.apply(boxed, &[(t, a)]);
        let box_b = db.apply(boxed, &[(t, b)]);
        let optional_box_b = db.union(&[box_b, nil]);
        assert_eq!(db.cmp(box_a, optional_box_b), Comparison::Cast);
    }

    #[test]
    fn recursive_through_pair() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);

        // struct Node { value: Int, ...next: Node | nil }
        let node = db.declare_struct("Node", &[]);
        let next = db.union(&[node, nil]);
        let inner = db.pair(int, next);
        db.define(node, inner);

        // struct Other { value: Int, ...next: Other | nil }
        let other = db.declare_struct("Other", &[]);
        let next = db.union(&[other, nil]);
        let inner = db.pair(int, next);
        db.define(other, inner);

        assert!(!is_non_contractive(&mut db.arena, node));
        assert_eq!(db.cmp(node, node), Comparison::Assign);
        assert_eq!(db.cmp(node, other), Comparison::Cast);
        assert_eq!(db.cmp(node, int), Comparison::Invalid);
        assert_eq!(db.name(node), "Node");
    }

    #[test]
    fn extract_narrowed_variants() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);

        // struct A { x: Int | nil }, and a copy of it with `x` narrowed to `Int`
        let field = db.union(&[int, nil]);
        let shape = db.pair(field, nil);
        let a = db.strukt("A", shape);
        let Type::Struct(declared) = db.arena[a].clone() else {
            panic!("expected a struct");
        };
        let narrowed_shape = db.pair(int, nil);
        let narrowed = db.alloc(Type::Struct(Struct {
            inner: narrowed_shape,
            ..declared.clone()
        }));

        // Every instance of the same struct is extracted, even through aliases and nested unions
        let alias = db.alias("Alias", &[], narrowed);
        let nested = db.union(&[a, db.builtins.never]);
        let union = db.union(&[alias, nested]);
        let structs = extract_structs(&mut db.arena, union).unwrap();
        assert_eq!(structs.len(), 2);
        assert!(structs.iter().all(|ty| ty.semantic == declared.semantic));
        assert_eq!(db.name(structs[0].inner), "(Int, nil)");
        assert_eq!(db.name(structs[1].inner), "(Int | nil, nil)");

        // But not if any variant is a different struct, or not a struct at all
        let b = db.strukt("B", shape);
        let different = db.union(&[a, b]);
        assert!(extract_structs(&mut db.arena, different).is_none());

        let optional = db.union(&[a, nil]);
        assert!(extract_structs(&mut db.arena, optional).is_none());
    }
}

mod aliases {
    use super::*;

    #[test]
    fn transparent() {
        let mut db = Db::new();
        let (int, bytes) = (db.builtins.int, db.builtins.bytes);
        let alias = db.alias("Amount", &[], int);
        assert_eq!(db.cmp(alias, int), Comparison::Assign);
        assert_eq!(db.cmp(int, alias), Comparison::Assign);
        assert_eq!(db.cmp(alias, bytes), Comparison::Cast);
    }

    #[test]
    fn generic_apply() {
        let mut db = Db::new();
        let (int, bytes, bytes32) = (db.builtins.int, db.builtins.bytes, db.builtins.bytes32);

        // type Twice<T> = (T, T);
        let t = db.generic("T");
        let pair = db.pair(t, t);
        let twice = db.alias("Twice", &[t], pair);

        let twice_int = db.apply(twice, &[(t, int)]);
        let int_int = db.pair(int, int);
        let int_bytes32 = db.pair(int, bytes32);
        assert_eq!(db.name(twice_int), "Twice<Int>");
        assert_eq!(db.cmp(int_int, twice_int), Comparison::Assign);
        assert_eq!(db.cmp(twice_int, int_int), Comparison::Assign);
        assert_eq!(db.cmp(int_bytes32, twice_int), Comparison::Cast);

        let twice_bytes32 = db.apply(twice, &[(t, bytes32)]);
        let twice_bytes = db.apply(twice, &[(t, bytes)]);
        assert_eq!(db.cmp(twice_bytes32, twice_bytes), Comparison::Assign);
        assert_eq!(db.cmp(twice_bytes, twice_bytes32), Comparison::Invalid);
    }
}

mod unions_and_pairs {
    use super::*;

    #[test]
    fn unions() {
        let mut db = Db::new();
        let (int, nil, bytes, bytes32) = (
            db.builtins.int,
            db.builtins.nil,
            db.builtins.bytes,
            db.builtins.bytes32,
        );
        let optional = db.union(&[int, nil]);
        let mixed = db.union(&[int, bytes32]);

        assert_eq!(db.cmp(int, optional), Comparison::Assign);
        assert_eq!(db.cmp(nil, optional), Comparison::Assign);
        assert_eq!(db.cmp(optional, int), Comparison::Cast);
        assert_eq!(db.cmp(optional, bytes32), Comparison::Invalid);
        assert_eq!(db.cmp(mixed, bytes), Comparison::Cast);
        assert_eq!(db.cmp(mixed, bytes32), Comparison::Invalid);
        assert_eq!(db.cmp(mixed, mixed), Comparison::Assign);

        let empty = db.union(&[]);
        assert_eq!(db.cmp(empty, int), Comparison::Assign);
        assert_eq!(db.cmp(int, empty), Comparison::Invalid);
    }

    #[test]
    fn pairs() {
        let mut db = Db::new();
        let (int, bytes, bytes32) = (db.builtins.int, db.builtins.bytes, db.builtins.bytes32);
        let narrow = db.pair(int, bytes32);
        let wide = db.pair(int, bytes);
        let cast = db.pair(bytes, bytes);

        assert_eq!(db.cmp(narrow, wide), Comparison::Assign);
        assert_eq!(db.cmp(wide, narrow), Comparison::Invalid);
        assert_eq!(db.cmp(narrow, cast), Comparison::Cast);
        assert_eq!(db.cmp(narrow, int), Comparison::Invalid);
        assert_eq!(db.cmp(int, narrow), Comparison::Invalid);
    }

    #[test]
    fn special_types() {
        let mut db = Db::new();
        let BuiltinTypes {
            int,
            never,
            unresolved,
            permissive_any: any,
            recursive_any,
            ..
        } = db.builtins;
        let pair = db.pair(int, int);

        assert_eq!(db.cmp(never, int), Comparison::Assign);
        assert_eq!(db.cmp(never, pair), Comparison::Assign);
        assert_eq!(db.cmp(int, never), Comparison::Invalid);
        assert_eq!(db.cmp(int, any), Comparison::Assign);
        assert_eq!(db.cmp(pair, any), Comparison::Assign);
        assert_eq!(db.cmp(any, int), Comparison::Invalid);
        assert_eq!(db.cmp(any, recursive_any), Comparison::Assign);
        assert_eq!(db.cmp(recursive_any, any), Comparison::Assign);
        assert_eq!(db.cmp(pair, recursive_any), Comparison::Assign);
        assert_eq!(db.cmp(unresolved, int), Comparison::Assign);
        assert_eq!(db.cmp(int, unresolved), Comparison::Assign);
    }
}

mod recursive {
    use super::*;

    #[test]
    fn list() {
        let mut db = Db::new();
        let (int, nil, bytes, bytes32) = (
            db.builtins.int,
            db.builtins.nil,
            db.builtins.bytes,
            db.builtins.bytes32,
        );
        let list_int = db.list(int);
        let list_bytes = db.list(bytes);
        let list_bytes32 = db.list(bytes32);
        let one = db.pair(int, nil);
        let two = db.pair(int, one);
        let bad = db.pair(int, int);

        assert!(!is_non_contractive(&mut db.arena, list_int));
        assert_eq!(db.cmp(list_int, list_int), Comparison::Assign);
        assert_eq!(db.cmp(list_bytes32, list_bytes), Comparison::Assign);
        assert_eq!(db.cmp(list_int, list_bytes), Comparison::Cast);
        assert_eq!(db.cmp(list_bytes, list_bytes32), Comparison::Invalid);
        assert_eq!(db.cmp(nil, list_int), Comparison::Assign);
        assert_eq!(db.cmp(two, list_int), Comparison::Assign);
        assert_eq!(db.cmp(bad, list_int), Comparison::Invalid);
        assert_eq!(db.cmp(list_int, two), Comparison::Invalid);
        assert_eq!(db.name(list_int), "List<Int>");
    }

    #[test]
    fn alternating_list() {
        let mut db = Db::new();
        let BuiltinTypes {
            int,
            nil,
            bytes,
            bytes32,
            alternating_list,
            alternating_list_generic_a: a,
            alternating_list_generic_b: b,
            ..
        } = db.builtins;
        let list = db.apply(alternating_list, &[(a, int), (b, bytes32)]);
        let wider = db.apply(alternating_list, &[(a, int), (b, bytes)]);

        let tail = db.pair(bytes32, nil);
        let good = db.pair(int, tail);
        let tail = db.pair(int, nil);
        let bad = db.pair(int, tail);
        let tail = db.pair(int, good);
        let odd = db.pair(bytes32, tail);

        assert!(!is_non_contractive(&mut db.arena, list));
        assert_eq!(db.cmp(good, list), Comparison::Assign);
        assert_eq!(db.cmp(bad, list), Comparison::Invalid);
        assert_eq!(db.cmp(odd, list), Comparison::Invalid);
        assert_eq!(db.cmp(list, wider), Comparison::Assign);
        assert_eq!(db.cmp(wider, list), Comparison::Invalid);
    }

    #[test]
    fn recursive_any() {
        let mut db = Db::new();
        let recursive_any = db.builtins.recursive_any;
        assert!(!is_non_contractive(&mut db.arena, recursive_any));
    }

    /// `type A = B; type B = A;`
    fn alias_cycle(db: &mut Db) -> (TypeId, TypeId) {
        let a = db.declare_alias("A", &[]);
        let b = db.declare_alias("B", &[]);
        db.define(a, b);
        db.define(b, a);
        (a, b)
    }

    /// `type Loop = Loop | Int;`
    fn union_cycle(db: &mut Db) -> TypeId {
        let int = db.builtins.int;
        let looped = db.declare_alias("Loop", &[]);
        let union = db.union(&[looped, int]);
        db.define(looped, union);
        looped
    }

    /// `struct S { ...inner: S }`
    fn struct_cycle(db: &mut Db) -> TypeId {
        let s = db.declare_struct("S", &[]);
        db.define(s, s);
        s
    }

    /// `type Id<T> = T; type X = Id<X>;`
    fn generic_cycle(db: &mut Db) -> TypeId {
        let t = db.generic("T");
        let id = db.alias("Id", &[t], t);
        let x = db.declare_alias("X", &[]);
        let apply = db.apply(id, &[(t, x)]);
        db.define(x, apply);
        x
    }

    /// `type R<T> = R<T>;`
    fn generic_self_cycle(db: &mut Db) -> TypeId {
        let t = db.generic("T");
        let r = db.declare_alias("R", &[t]);
        let apply = db.apply(r, &[(t, t)]);
        db.define(r, apply);
        r
    }

    fn non_contractive_types(db: &mut Db) -> Vec<TypeId> {
        let (a, b) = alias_cycle(db);
        vec![
            a,
            b,
            union_cycle(db),
            struct_cycle(db),
            generic_cycle(db),
            generic_self_cycle(db),
        ]
    }

    #[test]
    fn detects_non_contractive_types() {
        let mut db = Db::new();

        for ty in non_contractive_types(&mut db) {
            assert!(is_non_contractive(&mut db.arena, ty));
        }

        // A type that merely refers to a non-contractive type isn't reported itself
        let (a, _) = alias_cycle(&mut db);
        let refers = db.alias("Refers", &[], a);
        assert!(!is_non_contractive(&mut db.arena, refers));

        // A struct containing itself through a pair is fine
        let nil = db.builtins.nil;
        let s = db.declare_struct("S", &[]);
        let inner = db.pair(s, nil);
        db.define(s, inner);
        assert!(!is_non_contractive(&mut db.arena, s));
    }

    #[test]
    fn non_contractive_types_are_not_universally_assignable() {
        let mut db = Db::new();
        let BuiltinTypes {
            int, bytes32, nil, ..
        } = db.builtins;
        let pair = db.pair(int, int);

        for ty in non_contractive_types(&mut db) {
            assert_ne!(db.cmp(pair, ty), Comparison::Assign);
            assert_ne!(db.cmp(ty, bytes32), Comparison::Assign);
            assert_ne!(db.cmp(ty, nil), Comparison::Assign);
        }

        let (a, b) = alias_cycle(&mut db);
        assert_eq!(db.cmp(pair, a), Comparison::Invalid);
        assert_eq!(db.cmp(a, bytes32), Comparison::Invalid);
        assert_eq!(db.cmp(a, b), Comparison::Invalid);

        let s = struct_cycle(&mut db);
        assert_eq!(db.cmp(s, bytes32), Comparison::Invalid);
        assert_eq!(db.cmp(pair, s), Comparison::Invalid);
    }

    #[test]
    fn unwrap_semantic_unwraps_the_only_member_of_a_union() {
        let mut db = Db::new();
        let BuiltinTypes { int, nil, .. } = db.builtins;

        // The types are substituted first, so they're compared by kind rather than by id
        let inner = db.union(&[int, nil]);
        let alias = db.alias("MaybeInt", &[], inner);
        let union = db.union(&[alias]);

        let unwrapped = unwrap_semantic(&mut db.arena, union, true);
        assert!(matches!(&db.arena[unwrapped], Type::Union(union) if union.types.len() == 2));

        let unwrapped = unwrap_semantic(&mut db.arena, union, false);
        assert!(matches!(db.arena[unwrapped], Type::Alias(_)));

        let reference = db.alloc(Type::Ref(int));
        let union = db.union(&[reference]);

        let unwrapped = unwrap_semantic(&mut db.arena, union, false);
        assert!(matches!(db.arena[unwrapped], Type::Atom(_)));
    }

    #[test]
    fn non_contractive_types_do_not_overflow() {
        let mut db = Db::new();
        let BuiltinTypes {
            int, bytes32, nil, ..
        } = db.builtins;

        for ty in non_contractive_types(&mut db) {
            let _ = db.name(ty);
            let _ = substitute(&mut db.arena, ty);
            let _ = unwrap_semantic(&mut db.arena, ty, false);
            let _ = unwrap_semantic(&mut db.arena, ty, true);
            let _ = extract_atoms(&mut db.arena, ty, false);
            let _ = extract_atoms(&mut db.arena, ty, true);
            let _ = extract_pairs(&mut db.arena, ty, false);
            let _ = extract_pairs(&mut db.arena, ty, true);
            let _ = extract_functions(&mut db.arena, ty, true);
            let _ = extract_structs(&mut db.arena, ty);
            let _ = check(&mut db.arena, &db.builtins, ty, bytes32);
            let _ = check(&mut db.arena, &db.builtins, int, ty);
            let _ = subtract(&mut db.arena, &db.builtins, ty, int);

            let pair = db.pair(ty, int);
            let int_pair = db.pair(int, int);
            let _ = check(&mut db.arena, &db.builtins, pair, int_pair);
            let _ = check(&mut db.arena, &db.builtins, int_pair, pair);
            let _ = subtract(&mut db.arena, &db.builtins, pair, int_pair);
            let _ = subtract(&mut db.arena, &db.builtins, int, ty);
            let either = db.union(&[int, nil]);
            let splittable = db.pair(either, either);
            let first = db.pair(int, ty);
            let rest = db.pair(ty, int);
            let _ = subtract(&mut db.arena, &db.builtins, splittable, first);
            let _ = subtract(&mut db.arena, &db.builtins, splittable, rest);
            let _ = db.cmp(ty, ty);
            let _ = db.cmp(int, ty);
            let union = db.union(&[ty, nil]);
            let _ = db.cmp(union, bytes32);

            let mut infer = Inference::new([]);
            let _ = db.infer(ty, int, &mut infer);
        }
    }

    /// These functions don't track cycles themselves, so they rely on non-contractive types being
    /// rejected (and their cycles broken) by the compiler before they're used.
    #[test]
    fn broken_cycles_are_safe() {
        let mut db = Db::new();
        let BuiltinTypes {
            int,
            bytes32,
            unresolved,
            ..
        } = db.builtins;

        let (a, b) = alias_cycle(&mut db);
        let looped = union_cycle(&mut db);
        let s = struct_cycle(&mut db);

        // This is what the compiler does after reporting the type
        db.define(b, unresolved);
        db.define(looped, unresolved);
        db.define(s, unresolved);

        for ty in [a, b, looped, s] {
            assert!(!is_non_contractive(&mut db.arena, ty));
            let _ = check(&mut db.arena, &db.builtins, ty, bytes32);
            let _ = check(&mut db.arena, &db.builtins, int, ty);
            let _ = subtract(&mut db.arena, &db.builtins, ty, int);

            let pair = db.pair(ty, int);
            let int_pair = db.pair(int, int);
            let _ = check(&mut db.arena, &db.builtins, pair, int_pair);
            let _ = check(&mut db.arena, &db.builtins, int_pair, pair);
            let _ = subtract(&mut db.arena, &db.builtins, pair, int_pair);
            let _ = extract_atoms(&mut db.arena, ty, false);
            let _ = extract_pairs(&mut db.arena, ty, false);
            let _ = extract_functions(&mut db.arena, ty, true);
            let _ = extract_structs(&mut db.arena, ty);
            let _ = unwrap_semantic(&mut db.arena, ty, true);
        }
    }
}

mod inference {
    use super::*;
    use crate::{CallSolver, pass_lower_bounds, pass_upper_bounds, upper_bounds};

    fn candidates(infer: &Inference, generic: TypeId) -> InferenceCandidates {
        infer.candidates(generic).unwrap().clone()
    }

    #[test]
    fn covariant() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        assert_eq!(db.infer(int, t, &mut infer), Comparison::Assign);
        assert_eq!(
            candidates(&infer, t),
            InferenceCandidates {
                lower: vec![int],
                upper: vec![],
                held: vec![],
            }
        );

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], int);
    }

    /// A lower candidate that's a union with the generic in it only bounds the generic by the
    /// other members, as passing `fn<V>(x: V) -> Int | V` for `fn(a: T) -> T` gives (#362, 13k4),
    /// and an upper one says nothing, as an untyped lambda parameter compiled as `Maybe<A>` gives
    /// against `fn(a: Maybe<A>) -> Int` (13k5).
    #[test]
    fn union_candidates_with_the_generic_in_them() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let t = db.generic("T");
        let union = db.union(&[int, t]);

        let mut infer = Inference::new([t]);
        infer.push(t, CandidateKind::Lower, union, Vec::new());
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(db.name(mappings[&t]), "Int");

        // Through another generic in the cycle too, as `T ≥ V ≥ T`
        let (t, v) = (db.generic("T"), db.generic("V"));
        let union = db.union(&[int, v]);
        let mut infer = Inference::new([t, v]);
        infer.push(t, CandidateKind::Lower, union, Vec::new());
        infer.push(v, CandidateKind::Lower, t, Vec::new());
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(db.name(mappings[&t]), "Int");
        assert_eq!(db.name(mappings[&v]), "Int");

        // `T ≤ T | nil` leaves `T` uninferred, rather than inferred as `T | nil`
        let t = db.generic("T");
        let nil = db.builtins.nil;
        let union = db.union(&[t, nil]);
        let mut infer = Inference::new([t]);
        infer.push(t, CandidateKind::Upper, union, Vec::new());
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert!(!mappings.contains_key(&t), "{}", db.name(mappings[&t]));

        // And with another candidate, that one decides it
        let mut infer = Inference::new([t]);
        infer.push(t, CandidateKind::Upper, union, Vec::new());
        infer.push(t, CandidateKind::Lower, int, Vec::new());
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(db.name(mappings[&t]), "Int");

        // A generic that's only bounded by the others in its cycle through such a union is
        // resolved through what they're resolved to, as `T ≥ V | T` with `V ≥ T` is through `V`'s
        // fallback
        let (t, v) = (db.generic("T"), db.generic("V"));
        let union = db.union(&[v, t]);
        let mut infer = Inference::new([t, v]);
        infer.push(t, CandidateKind::Lower, union, Vec::new());
        infer.push(v, CandidateKind::Lower, t, Vec::new());
        let fallback = HashMap::from([(v, int)]);
        let mappings =
            resolve_inference_with_fallback(&mut db.arena, &db.builtins, &infer, &fallback);
        assert_eq!(db.name(mappings[&v]), "Int");
        assert_eq!(
            mappings.get(&t).map(|&ty| db.name(ty)).as_deref(),
            Some("Int")
        );

        // A split union's members keep their order
        let t = db.generic("T");
        let bytes = db.builtins.bytes;
        let union = db.union(&[int, bytes, t]);
        let mut infer = Inference::new([t]);
        infer.push(t, CandidateKind::Lower, union, Vec::new());
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(db.name(mappings[&t]), "Int | Bytes");

        // Within a cycle, an upper union with another generic of it says nothing either, since the
        // cycle lets that generic be the one it bounds. No program has been found that gives these
        // (untyped lambdas, generic lambdas and generic functions cross-wired through `Maybe` in
        // two parameters, by hand, and infer-fuzz): such calls are rejected as `Cannot infer
        // generic`, or their generics have other candidates. Whatever's left, no mapping names a
        // generic of the cycle other than as that generic itself, here or once the call is solved
        // (`resolve_inference_with_bounds`)
        let (t, v) = (db.generic("T"), db.generic("V"));
        let cycles = [
            // `T ≤ V`, `V ≤ T | nil`
            (
                vec![(t, CandidateKind::Upper, v)],
                vec![(v, CandidateKind::Upper, vec![t, nil])],
            ),
            // `T ≤ Int | V`, `V ≤ T`
            (
                vec![(v, CandidateKind::Upper, t)],
                vec![(t, CandidateKind::Upper, vec![int, v])],
            ),
            // The same with `T ≥ Bytes`, which decides both
            (
                vec![
                    (v, CandidateKind::Upper, t),
                    (t, CandidateKind::Lower, bytes),
                ],
                vec![(t, CandidateKind::Upper, vec![int, v])],
            ),
        ];
        for (index, (bare, unions)) in cycles.into_iter().enumerate() {
            let mut infer = Inference::new([t, v]);
            for (generic, kind, ty) in bare {
                infer.push(generic, kind, ty, Vec::new());
            }
            for (generic, kind, members) in unions {
                let union = db.union(&members);
                infer.push(generic, kind, union, Vec::new());
            }
            for mappings in [
                resolve_inference(&mut db.arena, &db.builtins, &infer),
                resolve_inference_with_bounds(&mut db.arena, &db.builtins, &infer, &HashMap::new()),
            ] {
                // Only as the other generic itself, which a cycle with nothing else leaves it as
                for (generic, other) in [(t, v), (v, t)] {
                    if let Some(&ty) = mappings.get(&generic) {
                        let names = crate::free_generics(&db.arena, ty);
                        assert!(
                            ty == other || !names.contains(&t) && !names.contains(&v),
                            "cycle {index}: {}",
                            db.name(ty)
                        );
                    }
                }
                if index == 2 {
                    assert_eq!(db.name(mappings[&t]), "Bytes");
                    assert_eq!(db.name(mappings[&v]), "Bytes");
                }
            }
        }

        // A cycle through a compound candidate, as `T ≤ V` with `V ≥ (T, Int)`, is an infinite type,
        // which is left as is, and `T` isn't resolved through it, since it would name itself. No
        // instantiation satisfies it, so a call that gives it is rejected, as
        // `kv(fn(x) => (x, 1), fn(y) => y)` with `fn kv<T, V>(f: fn(a: T) -> V, g: fn(b: V) -> T)` is
        // (see `generic_inference_bounds_naming_the_generic_errors.rue`)
        let (t, v) = (db.generic("T"), db.generic("V"));
        let pair = db.pair(t, int);
        let mut infer = Inference::new([t, v]);
        infer.push(t, CandidateKind::Upper, v, Vec::new());
        infer.push(v, CandidateKind::Lower, pair, Vec::new());
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings.get(&v), Some(&pair));
        assert!(
            mappings
                .get(&t)
                .is_none_or(|&ty| !crate::free_generics(&db.arena, ty).contains(&t)),
            "{}",
            db.name(mappings[&t])
        );

        // An upper candidate isn't split, since `T ≤ Int | V` doesn't say `T ≤ Int`
        let (t, v) = (db.generic("T"), db.generic("V"));
        let union = db.union(&[int, v]);
        let mut infer = Inference::new([t, v]);
        infer.push(t, CandidateKind::Upper, union, Vec::new());
        infer.push(v, CandidateKind::Upper, t, Vec::new());
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_ne!(
            mappings.get(&t).map(|&ty| db.name(ty)).as_deref(),
            Some("Int")
        );
    }

    /// A type compared with itself is `Assign` without comparing it, unless it references a
    /// generic being inferred, which comparing it collects candidates for.
    #[test]
    fn compared_with_itself() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let t = db.generic("T");
        let union = db.union(&[t, int]);

        let mut infer = Inference::new([t]);
        assert_eq!(db.infer(union, union, &mut infer), Comparison::Assign);
        let held = candidates(&infer, t).held;
        let names: Vec<String> = held.iter().map(|&ty| db.name(ty)).collect();
        assert_eq!(names, ["Int"]);

        let mut infer = Inference::new([t]);
        assert_eq!(db.infer(int, int, &mut infer), Comparison::Assign);
        assert_eq!(candidates(&infer, t), InferenceCandidates::default());
    }

    /// `5 | (5, 5)` against `T | (T, T)` matches both members for `(5, 5)`, but the `(T, T)`
    /// member's `T ≥ 5` is implied by the `T ≥ 5` that `5` gives, so the `T` member's
    /// `T ≥ (5, 5)` isn't needed (#362's step 13h). Against `T | (T, nil)`, only the `T` member
    /// matches `(5, 5)`, so its `T ≥ (5, 5)` is kept.
    #[test]
    fn implied_members_leave_the_others_out() {
        let mut db = Db::new();
        let nil = db.builtins.nil;
        let five = db.literal(AtomSemantic::Int, &[5]);
        let fives = db.pair(five, five);
        let value = db.union(&[five, fives]);

        let t = db.generic("T");
        let pair = db.pair(t, t);
        let param = db.union(&[t, pair]);
        let mut infer = Inference::new([t]);
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);

        // Only an inference that's narrow leaves them out (see `CallSolver::solve_narrowed`)
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert!(db.name(mappings[&t]).contains('('));
        infer.narrow = true;
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(db.name(mappings[&t]), "Int");

        let t = db.generic("T");
        let pair = db.pair(t, nil);
        let param = db.union(&[t, pair]);
        let mut infer = Inference::new([t]);
        infer.narrow = true;
        let _ = db.infer(value, param, &mut infer);
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        let resolved = db.name(mappings[&t]);
        assert!(resolved.contains('('), "{resolved}");
    }

    /// Resolves the generics after comparing the value with the parameter, without narrowing and
    /// with it (`Inference::narrow`).
    fn resolve_both_ways(
        db: &mut Db,
        value: TypeId,
        param: TypeId,
        generics: &[TypeId],
    ) -> (Vec<String>, Vec<String>) {
        let mut infer = Inference::new(generics.iter().copied());
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);
        let wide = resolve_inference(&mut db.arena, &db.builtins, &infer);
        infer.narrow = true;
        let narrow = resolve_inference(&mut db.arena, &db.builtins, &infer);
        let names = |db: &mut Db, mappings: &HashMap<TypeId, TypeId>| {
            generics
                .iter()
                .map(|generic| db.name(mappings[generic]))
                .collect()
        };
        (names(db, &wide), names(db, &narrow))
    }

    #[test]
    fn implied_members_never_empty_a_generic() {
        // `((5, 5), ((5, 5), (5, nil)))` against
        // `((T, T) | (T, U), ((T, T) | (T, U), (T, U | nil)))`: `(T, T)` is implied in both
        // unions, and each would leave out the other's `U ≥ 5`, since it's outside of it. That
        // would leave `U` with only the held `nil`, so both are kept
        let mut db = Db::new();
        let nil = db.builtins.nil;
        let five = db.literal(AtomSemantic::Int, &[5]);
        let fives = db.pair(five, five);
        let last = db.pair(five, nil);
        let rest = db.pair(fives, last);
        let value = db.pair(fives, rest);

        let t = db.generic("T");
        let u = db.generic("U");
        let tt = db.pair(t, t);
        let tu = db.pair(t, u);
        let union = db.union(&[tt, tu]);
        let u_or_nil = db.union(&[u, nil]);
        let last = db.pair(t, u_or_nil);
        let rest = db.pair(union, last);
        let param = db.pair(union, rest);

        let (wide, narrow) = resolve_both_ways(&mut db, value, param, &[t, u]);
        assert_eq!(narrow[1], wide[1]);
        assert_eq!(narrow[1], "Int");
    }

    #[test]
    fn implied_members_keep_held_candidates() {
        // `((5, nil), (5, 7))` against `((T, U | nil) | (T, nil), (T, U | Int))`: both members
        // match `(5, nil)` and are implied, `(T, U | nil)` with a held `U ⊒ nil`. `U` has a held
        // `7` from outside of the union, but held candidates are never left out, so `U` is what
        // it is without narrowing whichever member comes first. Leaving them out made it
        // `nil | Int` in one order and `Int` in the other
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let five = db.literal(AtomSemantic::Int, &[5]);
        let seven = db.literal(AtomSemantic::Int, &[7]);
        let first = db.pair(five, nil);
        let second = db.pair(five, seven);
        let value = db.pair(first, second);

        for reversed in [false, true] {
            let t = db.generic("T");
            let u = db.generic("U");
            let u_or_nil = db.union(&[u, nil]);
            let held = db.pair(t, u_or_nil);
            let plain = db.pair(t, nil);
            let mut members = vec![held, plain];
            if reversed {
                members.reverse();
            }
            let union = db.union(&members);
            let u_or_int = db.union(&[u, int]);
            let second = db.pair(t, u_or_int);
            let param = db.pair(union, second);

            let (wide, narrow) = resolve_both_ways(&mut db, value, param, &[t, u]);
            assert_eq!(narrow[1], wide[1], "reversed: {reversed}");
        }
    }

    #[test]
    fn implied_members_upper_candidates() {
        // `(fn(x: Int | Bytes) -> nil, fn(x: 5) -> nil)` matches both members. The first's
        // `T ≤ Int | Bytes` is implied by the `T ≤ Int` from `(fn(x: Int) -> nil, nil)`, so the
        // second's `T ≤ 5` is left out, and `T` is `Int`
        let mut db = Db::new();
        let (int, bytes, nil) = (db.builtins.int, db.builtins.bytes, db.builtins.nil);
        let any = db.builtins.permissive_any;
        let five = db.literal(AtomSemantic::Int, &[5]);
        let int_or_bytes = db.union(&[int, bytes]);
        let takes_int = db.function(&[int], nil);
        let takes_wide = db.function(&[int_or_bytes], nil);
        let takes_five = db.function(&[five], nil);
        let first = db.pair(takes_int, nil);
        let second = db.pair(takes_wide, takes_five);
        let value = db.union(&[first, second]);

        let t = db.generic("T");
        let takes_t = db.function(&[t], nil);
        let first = db.pair(takes_t, any);
        let second = db.pair(any, takes_t);
        let param = db.union(&[first, second]);
        let mut infer = Inference::new([t]);
        infer.narrow = true;
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(db.name(mappings[&t]), "Int");
    }

    #[test]
    fn implied_members_only_leave_out_kinds_with_an_outside_candidate() {
        // `(5, 5)` matches both `(T, T)` and `(U, T)`, and `(T, T)` is implied, but `U` has no
        // lower candidate outside of the union, so `(U, T)`'s `U ≥ 5` is kept and `U` is `Int`
        // (`5` widened) rather than the `Int | Bytes` its upper candidate gives
        let mut db = Db::new();
        let (int, bytes, nil) = (db.builtins.int, db.builtins.bytes, db.builtins.nil);
        let five = db.literal(AtomSemantic::Int, &[5]);
        let fives = db.pair(five, five);
        let int_or_bytes = db.union(&[int, bytes]);
        let takes_wide = db.function(&[int_or_bytes], nil);
        let rest = db.pair(fives, nil);
        let rest = db.pair(takes_wide, rest);
        let value = db.pair(five, rest);

        let t = db.generic("T");
        let u = db.generic("U");
        let takes_u = db.function(&[u], nil);
        let tt = db.pair(t, t);
        let ut = db.pair(u, t);
        let union = db.union(&[tt, ut]);
        let rest = db.pair(union, nil);
        let rest = db.pair(takes_u, rest);
        let param = db.pair(t, rest);
        let mut infer = Inference::new([t, u]);
        infer.narrow = true;
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(db.name(mappings[&t]), "Int");
        assert_eq!(db.name(mappings[&u]), "Int");
    }

    #[test]
    fn implied_members_dont_depend_on_order() {
        // `T | (T, T) | (T, (T, T))` and the reverse, against `5 | (5, 5) | (5, (5, 5))`: each
        // union is decided from the `T ≥ 5` that `5` gives, which no union leaves out
        let mut db = Db::new();
        let five = db.literal(AtomSemantic::Int, &[5]);
        let fives = db.pair(five, five);
        let nested = db.pair(five, fives);
        let value = db.union(&[five, fives, nested]);

        for reversed in [false, true] {
            let t = db.generic("T");
            let pair = db.pair(t, t);
            let nested = db.pair(t, pair);
            let mut members = vec![t, pair, nested];
            if reversed {
                members.reverse();
            }
            let param = db.union(&members);
            let mut infer = Inference::new([t]);
            infer.narrow = true;
            assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);
            let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
            assert_eq!(db.name(mappings[&t]), "Int", "reversed: {reversed}");
        }
    }

    #[test]
    fn widens_covariant_candidates() {
        let mut db = Db::new();
        let (int, bytes32) = (db.builtins.int, db.builtins.bytes32);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        let value = db.pair(int, bytes32);
        let param = db.pair(t, t);
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(db.name(mappings[&t]), "Int | Bytes32");
    }

    #[test]
    fn contravariant() {
        let mut db = Db::new();
        let (bytes32, int) = (db.builtins.bytes32, db.builtins.int);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        let value = db.function(&[bytes32], int);
        let param = db.function(&[t], int);
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);
        assert_eq!(
            candidates(&infer, t),
            InferenceCandidates {
                lower: vec![],
                upper: vec![bytes32],
                held: vec![],
            }
        );

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], bytes32);
    }

    #[test]
    fn most_specific_upper_bound() {
        let mut db = Db::new();
        let (bytes, bytes32, int) = (db.builtins.bytes, db.builtins.bytes32, db.builtins.int);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        let takes_bytes = db.function(&[bytes], int);
        let takes_bytes32 = db.function(&[bytes32], int);
        let value = db.pair(takes_bytes, takes_bytes32);
        let takes_t = db.function(&[t], int);
        let param = db.pair(takes_t, takes_t);
        db.infer(value, param, &mut infer);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], bytes32);
    }

    #[test]
    fn nested_function_parameters_are_covariant() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // fn(f: fn(x: Int) -> Int) -> Int to fn(f: fn(x: T) -> Int) -> Int
        let inner_value = db.function(&[int], int);
        let value = db.function(&[inner_value], int);
        let inner_param = db.function(&[t], int);
        let param = db.function(&[inner_param], int);
        db.infer(value, param, &mut infer);

        assert_eq!(
            candidates(&infer, t),
            InferenceCandidates {
                lower: vec![int],
                upper: vec![],
                held: vec![],
            }
        );
    }

    #[test]
    fn mixed_variance_must_be_rechecked() {
        let mut db = Db::new();
        let (int, bytes32) = (db.builtins.int, db.builtins.bytes32);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // (Int, fn(x: Bytes32) -> Bytes32) to (T, fn(x: T) -> Bytes32)
        let takes_bytes32 = db.function(&[bytes32], bytes32);
        let value = db.pair(int, takes_bytes32);
        let takes_t = db.function(&[t], bytes32);
        let param = db.pair(t, takes_t);
        db.infer(value, param, &mut infer);

        assert_eq!(
            candidates(&infer, t),
            InferenceCandidates {
                lower: vec![int],
                upper: vec![bytes32],
                held: vec![],
            }
        );

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], int);

        let param = substitute_with_mappings(&mut db.arena, param, &mappings);
        assert_eq!(db.cmp(value, param), Comparison::Invalid);
    }

    #[test]
    fn rigid_function_generics_are_not_inferred() {
        let mut db = Db::new();
        let t = db.generic("T");
        let u = db.generic("U");
        let v = db.generic("V");
        let mut infer = Inference::new([t]);

        // fn<V>(p0: V) -> V to fn<U>(p0: U) -> T
        let identity = db.function_with(&[v], &[v], true, v);
        let param = db.function_with(&[u], &[u], true, t);
        db.infer(identity, param, &mut infer);

        // `T` can't be inferred as `U`, since it's only in scope of the parameter's type
        assert_eq!(candidates(&infer, t), InferenceCandidates::default());

        // But a function returned to where `fn<U>(p0: U) -> U` is expected can be instantiated
        // with `U`, since the expected type binds it for the whole value
        let mut infer = Inference::new([t]);
        let ret = db.function_with(&[], &[t], true, t);
        let expected = db.function_with(&[u], &[u], true, u);
        infer_from_expected(&mut db.arena, &db.builtins, ret, expected, &mut infer);
        assert_eq!(
            candidates(&infer, t),
            InferenceCandidates {
                lower: vec![u],
                upper: vec![u],
                held: vec![],
            }
        );
    }

    #[test]
    fn only_targets_are_inferred() {
        let mut db = Db::new();
        let (int, bytes32) = (db.builtins.int, db.builtins.bytes32);
        let t = db.generic("T");
        let u = db.generic("U");
        let mut infer = Inference::new([t]);

        // U isn't being inferred, so it's rigid
        assert_eq!(db.infer(int, u, &mut infer), Comparison::Invalid);
        assert!(infer.candidates(u).is_none());

        // The generic of a generic function type in a parameter isn't inferred either
        let takes_bytes32 = db.function(&[bytes32], bytes32);
        let identity_u = db.function_with(&[u], &[u], true, u);
        assert_eq!(
            db.infer(takes_bytes32, identity_u, &mut infer),
            Comparison::Invalid
        );
        assert_eq!(candidates(&infer, t), InferenceCandidates::default());

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert!(mappings.is_empty());
    }

    #[test]
    fn widens_literal_candidates() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // Each argument of `pick(1, 2)` contributes a candidate, and both widen to `Int`
        let one = db.literal(AtomSemantic::Int, &[1]);
        let two = db.literal(AtomSemantic::Int, &[2]);
        assert_eq!(db.infer(one, t, &mut infer), Comparison::Assign);
        assert_eq!(db.infer(two, t, &mut infer), Comparison::Assign);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], int);
    }

    #[test]
    fn widens_literals_to_base_type() {
        let mut db = Db::new();

        for (semantic, len, expected) in [
            (AtomSemantic::Bytes, 1, db.builtins.bytes),
            (AtomSemantic::Bytes, 32, db.builtins.bytes32),
            (AtomSemantic::String, 1, db.builtins.string),
            (AtomSemantic::PublicKey, 48, db.builtins.public_key),
            (AtomSemantic::Bool, 1, db.builtins.bool),
        ] {
            let t = db.generic("T");
            let mut infer = Inference::new([t]);
            let literal = db.literal(semantic, &vec![1; len]);
            db.infer(literal, t, &mut infer);

            let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
            assert_eq!(mappings[&t], expected);
        }
    }

    #[test]
    fn keeps_nil_candidate() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        db.infer(nil, t, &mut infer);
        db.infer(int, t, &mut infer);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(db.name(mappings[&t]), "nil | Int");
    }

    #[test]
    fn keeps_literal_required_by_upper_bound() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // (5, fn(x: 5) -> Int) to (T, fn(x: T) -> Int)
        let five = db.literal(AtomSemantic::Int, &[5]);
        let takes_five = db.function(&[five], int);
        let value = db.pair(five, takes_five);
        let takes_t = db.function(&[t], int);
        let param = db.pair(t, takes_t);
        db.infer(value, param, &mut infer);

        // Widening to `Int` would make the function argument invalid
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], five);

        let param = substitute_with_mappings(&mut db.arena, param, &mappings);
        assert_eq!(db.cmp(value, param), Comparison::Assign);
    }

    #[test]
    fn through_list() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        let list_t = db.list(t);
        let list_t = substitute(&mut db.arena, list_t);
        let one = db.pair(int, nil);
        assert_eq!(db.infer(one, list_t, &mut infer), Comparison::Assign);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], int);
    }

    #[test]
    fn keeps_literal_required_by_expected_type() {
        let mut db = Db::new();
        let t = db.generic("T");
        let (five, six) = (
            db.literal(AtomSemantic::Int, &[5]),
            db.literal(AtomSemantic::Int, &[6]),
        );

        // `let x: 5 = id(5);` keeps the literal
        let mut expected = Inference::new([t]);
        infer_from_expected(&mut db.arena, &db.builtins, t, five, &mut expected);

        let mut infer = Inference::new([t]);
        db.infer(five, t, &mut infer);
        add_expected_bounds(&mut db.arena, &db.builtins, &mut infer, &expected);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], five);

        // `let x: 5 = id(6);` doesn't satisfy the expected type, so it's left to be reported
        let mut infer = Inference::new([t]);
        db.infer(six, t, &mut infer);
        add_expected_bounds(&mut db.arena, &db.builtins, &mut infer, &expected);
        assert_eq!(candidates(&infer, t).upper, vec![]);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], db.builtins.int);
    }

    #[test]
    fn call_solver_uses_the_expected_type() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let five = db.literal(AtomSemantic::Int, &[5]);
        let (a, b, t) = (db.generic("A"), db.generic("B"), db.generic("T"));

        // A call like `let x: 5 = pair(5, make());` with `fn pair<A, B>(a: A, b: B) -> (A, B)`
        // and the expected type `(5, Int)`
        let ret = db.pair(a, b);
        let expected_type = db.pair(five, int);
        let mut expected = Inference::new([a, b]);
        infer_from_expected(
            &mut db.arena,
            &db.builtins,
            ret,
            expected_type,
            &mut expected,
        );
        let solver = CallSolver::new(&mut db.arena, &db.builtins, Some(expected));

        // Before any argument, both generics fall back to the expected type for the next argument,
        // but the arguments themselves infer nothing
        let mut infer = Inference::new([a, b]);
        let inferred = solver.solve_arguments(&mut db.arena, &db.builtins, &infer);
        assert!(inferred.is_empty());
        let mappings =
            solver.argument_mappings(&mut db.arena, &db.builtins, &infer, &inferred, true);
        assert_eq!(mappings[&a], five);
        assert_eq!(mappings[&b], int);

        // Once `5` is passed for `A`, the arguments alone widen it to `Int`, but an argument's
        // expected type keeps the expected bound, `5` (#353). A lambda's keeps what the arguments
        // infer
        db.infer(five, a, &mut infer);
        let inferred = solver.solve_arguments(&mut db.arena, &db.builtins, &infer);
        assert_eq!(inferred[&a], int);
        let mappings =
            solver.argument_mappings(&mut db.arena, &db.builtins, &infer, &inferred, true);
        assert_eq!(mappings[&a], five);
        assert_eq!(mappings[&b], int);
        let mappings =
            solver.argument_mappings(&mut db.arena, &db.builtins, &infer, &inferred, false);
        assert_eq!(mappings[&a], int);
        assert_eq!(mappings[&b], int);

        // The final solve adds the expected bounds to `A`, so the literal isn't widened
        let mappings = solver.solve(&mut db.arena, &db.builtins, infer.clone());
        assert_eq!(mappings[&a], five);
        assert_eq!(mappings[&b], int);

        // Without an expected type, nothing falls back, and the literal is widened
        let solver = CallSolver::new(&mut db.arena, &db.builtins, None);
        let mut infer = Inference::new([t]);
        db.infer(five, t, &mut infer);
        let inferred = solver.solve_arguments(&mut db.arena, &db.builtins, &infer);
        let mappings =
            solver.argument_mappings(&mut db.arena, &db.builtins, &infer, &inferred, true);
        assert_eq!(mappings[&t], int);
        assert_eq!(
            solver.solve(&mut db.arena, &db.builtins, infer.clone())[&t],
            int
        );
    }

    #[test]
    fn call_solver_infers_unmapped_generics_through_others_for_arguments() {
        let mut db = Db::new();
        let five = db.literal(AtomSemantic::Int, &[5]);
        let (a, b) = (db.generic("A"), db.generic("B"));

        // `let x: 5 = map(identity, map(...));`: `identity` gives `B` the candidate `A`, and the
        // expected type bounds `B`, so the next argument sees `A ≤ 5` through it (#331)
        let mut expected = Inference::new([a, b]);
        infer_from_expected(&mut db.arena, &db.builtins, b, five, &mut expected);
        let solver = CallSolver::new(&mut db.arena, &db.builtins, Some(expected));

        let mut infer = Inference::new([a, b]);
        infer.push(b, CandidateKind::Lower, a, Vec::new());
        let inferred = solver.solve_arguments(&mut db.arena, &db.builtins, &infer);
        let mappings =
            solver.argument_mappings(&mut db.arena, &db.builtins, &infer, &inferred, true);
        assert_eq!(mappings[&a], five);
    }

    /// Collects the bounds that `B` is expected to satisfy, as in `let x: <expected> = ...` for a
    /// call returning `B`, and adds them to the inference.
    fn add_expected(
        db: &mut Db,
        generics: &[TypeId],
        b: TypeId,
        ty: TypeId,
        infer: &mut Inference,
    ) {
        let mut expected = Inference::new(generics.iter().copied());
        infer_from_expected(&mut db.arena, &db.builtins, b, ty, &mut expected);
        add_expected_bounds(&mut db.arena, &db.builtins, infer, &expected);
    }

    #[test]
    fn passes_expected_bounds_through_other_generics() {
        let mut db = Db::new();
        let (a, b) = (db.generic("A"), db.generic("B"));
        let (five, six) = (
            db.literal(AtomSemantic::Int, &[5]),
            db.literal(AtomSemantic::Int, &[6]),
        );

        // `let x: 5 = map(identity, 5);` gives `A` the bound through `B`
        let mut infer = Inference::new([a, b]);
        assert_eq!(db.infer(a, b, &mut infer), Comparison::Assign);
        assert_eq!(db.infer(five, a, &mut infer), Comparison::Assign);
        add_expected(&mut db, &[a, b], b, five, &mut infer);
        assert_eq!(candidates(&infer, a).upper, vec![five]);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&a], five);
        assert_eq!(mappings[&b], five);

        // `let x: 5 = map(identity, 6);` doesn't satisfy it, through `A`
        let mut infer = Inference::new([a, b]);
        db.infer(a, b, &mut infer);
        db.infer(six, a, &mut infer);
        add_expected(&mut db, &[a, b], b, five, &mut infer);
        assert_eq!(candidates(&infer, a).upper, vec![]);
        assert_eq!(candidates(&infer, b).upper, vec![]);
    }

    #[test]
    fn passes_expected_bounds_into_compound_candidates() {
        let mut db = Db::new();
        let (a, b) = (db.generic("A"), db.generic("B"));
        let five = db.literal(AtomSemantic::Int, &[5]);

        // `B` is inferred from `(A, A)`, and expected to be `(5, 5)`
        let mut infer = Inference::new([a, b]);
        let pair = db.pair(a, a);
        db.infer(pair, b, &mut infer);
        db.infer(five, a, &mut infer);
        let expected = db.pair(five, five);
        add_expected(&mut db, &[a, b], b, expected, &mut infer);
        assert_eq!(candidates(&infer, a).upper, vec![five]);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&a], five);
    }

    #[test]
    fn passes_expected_bounds_through_cycles() {
        let mut db = Db::new();
        let (a, b) = (db.generic("A"), db.generic("B"));
        let five = db.literal(AtomSemantic::Int, &[5]);

        // `A` and `B` are inferred from each other, and only the expected type bounds them
        let mut infer = Inference::new([a, b]);
        db.infer(a, b, &mut infer);
        db.infer(b, a, &mut infer);
        add_expected(&mut db, &[a, b], b, five, &mut infer);
        assert_eq!(candidates(&infer, a).upper, vec![five]);
        assert_eq!(candidates(&infer, b).upper, vec![five]);
    }

    #[test]
    fn passes_union_bounds_through_cycles() {
        let mut db = Db::new();
        let (a, b) = (db.generic("A"), db.generic("B"));
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let five = db.literal(AtomSemantic::Int, &[5]);

        // `A` and `B` are inferred from each other and `5`, and expected to be `Int | nil`. Passing
        // the union around the cycle must end
        let mut infer = Inference::new([a, b]);
        db.infer(a, b, &mut infer);
        db.infer(b, a, &mut infer);
        db.infer(five, a, &mut infer);
        let expected = db.union(&[int, nil]);
        add_expected(&mut db, &[a, b], b, expected, &mut infer);
        assert_eq!(candidates(&infer, a).upper.len(), 1);
        assert_eq!(candidates(&infer, b).upper.len(), 1);

        // `B` is inferred from `(A, nil)`, where `A` and `C` are inferred from each other and `5`,
        // so the union inside the expected pair goes around that cycle
        let c = db.generic("C");
        let pair = db.pair(a, nil);
        let mut infer = Inference::new([a, b, c]);
        db.infer(pair, b, &mut infer);
        db.infer(c, a, &mut infer);
        db.infer(a, c, &mut infer);
        db.infer(five, a, &mut infer);
        let expected_pair = db.pair(expected, nil);
        let expected_pair = db.union(&[expected_pair, nil]);
        add_expected(&mut db, &[a, b, c], b, expected_pair, &mut infer);
        assert_eq!(candidates(&infer, b).upper.len(), 1);
        assert_eq!(candidates(&infer, a).upper.len(), 1);
        assert_eq!(candidates(&infer, c).upper.len(), 1);

        // If `A` is also inferred from `B`, it's an infinite type (`A` holds `(A, nil)`), whose
        // values aren't all `(Int | nil, nil)`, so the bound isn't added
        let mut infer = {
            let mut infer = Inference::new([a, b]);
            db.infer(pair, b, &mut infer);
            db.infer(b, a, &mut infer);
            db.infer(five, a, &mut infer);
            infer
        };
        add_expected(&mut db, &[a, b], b, expected_pair, &mut infer);
        assert_eq!(candidates(&infer, b).upper.len(), 0);
    }

    #[test]
    fn expands_lower_bounds_regardless_of_order() {
        let mut db = Db::new();
        let (a, b, t, u) = (
            db.generic("A"),
            db.generic("B"),
            db.generic("T"),
            db.generic("U"),
        );
        let nil = db.builtins.nil;
        let five = db.literal(AtomSemantic::Int, &[5]);

        // `B: [U, (T, nil)]`, `U: [A]`, `A: [T, 5]` and `T: [A]`, so `B`'s lower bounds expand to
        // `5 | (5, nil)` whichever of `U` and `T` is expanded first
        let mut infer = Inference::new([a, b, t, u]);
        db.infer(u, b, &mut infer);
        let pair = db.pair(t, nil);
        db.infer(pair, b, &mut infer);
        db.infer(a, u, &mut infer);
        db.infer(t, a, &mut infer);
        db.infer(five, a, &mut infer);
        db.infer(a, t, &mut infer);
        let five_nil = db.pair(five, nil);
        let expected = db.union(&[five, five_nil]);
        add_expected(&mut db, &[a, b, t, u], b, expected, &mut infer);
        assert_eq!(candidates(&infer, b).upper.len(), 1);
    }

    #[test]
    fn passes_upper_bounds_through_other_generics() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let (a, b) = (db.generic("A"), db.generic("B"));

        // `feed(identity, inc)`: `A` flows into `B`, which is bounded by `Int`
        let mut infer = Inference::new([a, b]);
        db.infer(a, b, &mut infer);
        infer_from_expected(&mut db.arena, &db.builtins, b, int, &mut infer);
        let mappings =
            resolve_inference_with_bounds(&mut db.arena, &db.builtins, &infer, &HashMap::new());
        assert_eq!(mappings[&a], int);
        assert_eq!(mappings[&b], int);

        // In a cycle, where `B` already has the bound, it still ends
        let mut infer = Inference::new([a, b]);
        db.infer(a, b, &mut infer);
        db.infer(b, a, &mut infer);
        infer_from_expected(&mut db.arena, &db.builtins, b, int, &mut infer);
        let mappings =
            resolve_inference_with_bounds(&mut db.arena, &db.builtins, &infer, &HashMap::new());
        assert_eq!(mappings[&a], int);
        assert_eq!(mappings[&b], int);
    }

    #[test]
    fn narrows_inferred_generics_when_passing_upper_bounds() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let (a, b) = (db.generic("A"), db.generic("B"));
        let five = db.literal(AtomSemantic::Int, &[5]);

        // `A` has the lower bound `5`, and `B`'s bound `5` is passed on to it, so it's `5` rather
        // than widened to `Int` (#337). `B`, which is inferred through it, is `5` too
        let mut infer = Inference::new([a, b]);
        db.infer(a, b, &mut infer);
        db.infer(five, a, &mut infer);
        infer_from_expected(&mut db.arena, &db.builtins, b, five, &mut infer);
        let mappings =
            resolve_inference_with_bounds(&mut db.arena, &db.builtins, &infer, &HashMap::new());
        assert_eq!(mappings[&a], five);
        assert_eq!(
            compare(&mut db.arena, &db.builtins, mappings[&b], five),
            Comparison::Assign
        );

        // `A` falls back to `5`, and `B`'s bound `Int` doesn't widen it, since `5` is the most
        // specific type that satisfies it
        let mut infer = Inference::new([a, b]);
        db.infer(a, b, &mut infer);
        infer_from_expected(&mut db.arena, &db.builtins, b, int, &mut infer);
        let fallback = HashMap::from([(a, five)]);
        let mappings =
            resolve_inference_with_bounds(&mut db.arena, &db.builtins, &infer, &fallback);
        assert_eq!(mappings[&a], five);
    }

    #[test]
    fn keeps_compound_lower_bounds_with_unconstrained_generics() {
        let mut db = Db::new();
        let (a, b) = (db.generic("A"), db.generic("B"));
        let (five, six) = (
            db.literal(AtomSemantic::Int, &[5]),
            db.literal(AtomSemantic::Int, &[6]),
        );

        // `B` is inferred from `(A, 6)`, where nothing bounds `A` from below, so `(A, 6)` is
        // checked as is against `(5, 5)` and doesn't satisfy it
        let mut infer = Inference::new([a, b]);
        let pair = db.pair(a, six);
        db.infer(pair, b, &mut infer);
        let expected = db.pair(five, five);
        add_expected(&mut db, &[a, b], b, expected, &mut infer);
        assert_eq!(candidates(&infer, a).upper, vec![]);
        assert_eq!(candidates(&infer, b).upper, vec![]);
    }

    /// The names of the generic's candidates of each kind, all of them and only those that hold.
    fn names(db: &mut Db, infer: &Inference, generic: TypeId) -> [(Vec<String>, Vec<String>); 3] {
        let all = candidates(infer, generic);
        let matched = infer.matched_candidates(generic).unwrap();
        let mut names = |types: &[TypeId]| types.iter().map(|&ty| db.name(ty)).collect::<Vec<_>>();
        [
            (names(&all.lower), names(&matched.lower)),
            (names(&all.upper), names(&matched.upper)),
            (names(&all.held), names(&matched.held)),
        ]
    }

    #[test]
    fn matched_candidates_leave_out_members_that_dont_match() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // A function matches the `T` member, so what the function's runtime structure gives `T`
        // through `(T, Int)` doesn't hold (#364)
        let function = db.function(&[int], int);
        let pair = db.pair(t, int);
        let param = db.union(&[t, pair, nil]);
        assert_eq!(db.infer(function, param, &mut infer), Comparison::Assign);

        let [(all, matched), _, _] = names(&mut db, &infer, t);
        assert!(all.len() > 1, "{all:?}");
        assert_eq!(matched, ["fn(p0: Int) -> Int"]);
    }

    #[test]
    fn matched_candidates_take_the_best_member() {
        let mut db = Db::new();
        let (int, bytes) = (db.builtins.int, db.builtins.bytes);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // `(T, Int)` only casts `Bytes` to `Int`, but `(Int, T)` is assignable
        let value = db.pair(int, bytes);
        let first = db.pair(t, int);
        let second = db.pair(int, t);
        let param = db.union(&[first, second]);
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);

        let [(all, matched), _, _] = names(&mut db, &infer, t);
        assert_eq!(all, ["Int", "Bytes"]);
        assert_eq!(matched, ["Bytes"]);
    }

    #[test]
    fn matched_candidates_keep_every_member_that_matches() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // `Int` matches `T`, and `nil` matches both members, so it's held
        let value = db.union(&[int, nil]);
        let param = db.union(&[t, nil]);
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);

        let [lower, upper, held] = names(&mut db, &infer, t);
        assert_eq!(lower, (vec!["Int".to_string()], vec!["Int".to_string()]));
        assert_eq!(upper, (vec![], vec![]));
        assert_eq!(held, (vec!["nil".to_string()], vec!["nil".to_string()]));
    }

    #[test]
    fn matched_candidates_keep_everything_when_no_member_matches() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // Neither member can hold the value, so the comparison fails regardless, and what it
        // collected is kept as it is
        let int_pair = db.pair(int, int);
        let value = db.pair(int, int_pair);
        let first = db.pair(t, nil);
        let second = db.pair(nil, t);
        let param = db.union(&[first, second]);
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Invalid);

        let [(all, matched), _, _] = names(&mut db, &infer, t);
        assert_eq!(all, ["Int", "(Int, Int)"]);
        assert_eq!(matched, all);
    }

    #[test]
    fn matched_candidates_leave_out_unions_nested_in_members_that_dont_match() {
        let mut db = Db::new();
        let (int, bytes, nil) = (db.builtins.int, db.builtins.bytes, db.builtins.nil);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // `Int` matches `T` in `T | nil`, but the member it's nested in only casts `Bytes`
        let value = db.pair(int, bytes);
        let optional = db.union(&[t, nil]);
        let first = db.pair(optional, int);
        let second = db.pair(int, t);
        let param = db.union(&[first, second]);
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);

        let [(all, matched), _, _] = names(&mut db, &infer, t);
        assert_eq!(all, ["Int", "Bytes"]);
        assert_eq!(matched, ["Bytes"]);
    }

    /// Whether each of the generic's lower candidates holds, in order.
    fn lower_matched(infer: &Inference, generic: TypeId) -> Vec<bool> {
        (0..candidates(infer, generic).lower.len())
            .map(|index| {
                infer
                    .branches(generic, CandidateKind::Lower, index)
                    .iter()
                    .all(|&branch| infer.matched(branch))
            })
            .collect()
    }

    #[test]
    fn matched_candidates_keep_the_fallback_when_no_member_holds_the_value() {
        let mut db = Db::new();
        let (int, atom) = (db.builtins.int, db.builtins.atom);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // No member is a function type, so the function's runtime structure is compared with the
        // union instead. What that gives `T` holds, and what comparing the function with `(T, T)`
        // on its own gave first doesn't
        let function = db.function(&[int], int);
        let pair = db.pair(t, t);
        let param = db.union(&[atom, pair]);
        assert_eq!(db.infer(function, param, &mut infer), Comparison::Assign);
        assert_eq!(lower_matched(&infer, t), [false, false, true, true]);
    }

    #[test]
    fn matched_candidates_leave_out_members_that_dont_match_an_opaque_generic() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let (t, u) = (db.generic("T"), db.generic("U"));
        let mut infer = Inference::new([t]);

        // `U` matches `T`, so what its runtime structure gives `T` through `(T, Int)` doesn't hold
        let pair = db.pair(t, int);
        let param = db.union(&[pair, t]);
        assert_eq!(db.infer(u, param, &mut infer), Comparison::Assign);

        let [(all, matched), _, _] = names(&mut db, &infer, t);
        assert!(all.len() > 1, "{all:?}");
        assert_eq!(matched, ["U"]);
    }

    #[test]
    fn matched_candidates_leave_out_upper_candidates_of_members_that_dont_match() {
        let mut db = Db::new();
        let (int, bytes) = (db.builtins.int, db.builtins.bytes);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // The first member only casts the return type, so `T ≤ Int` from its parameter doesn't
        // hold, and the second member, which is assignable, collects nothing
        let function = db.function(&[int], int);
        let first = db.function(&[t], bytes);
        let second = db.function(&[int], int);
        let param = db.union(&[first, second]);
        assert_eq!(db.infer(function, param, &mut infer), Comparison::Assign);

        let [_, (all, matched), _] = names(&mut db, &infer, t);
        assert_eq!(all, ["Int"]);
        assert_eq!(matched, Vec::<String>::new());
    }

    #[test]
    fn matched_candidates_keep_the_struct_cast_when_no_member_holds_the_struct() {
        let mut db = Db::new();
        let (int, bytes) = (db.builtins.int, db.builtins.bytes);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // Neither member holds the struct, but its inner type can be cast to the union as a whole.
        // What comparing the struct with `(T, T)` collected doesn't hold, and what the cast
        // collected does
        let int_pair = db.pair(int, int);
        let inner = db.union(&[int, int_pair]);
        let value = db.declare_struct("S", &[]);
        db.define(value, inner);
        let pair = db.pair(t, t);
        let param = db.union(&[bytes, pair]);
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Cast);
        assert_eq!(lower_matched(&infer, t), [false, false, true, true]);
    }

    #[test]
    fn passed_bounds_keep_the_members_they_were_collected_under() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let five = db.literal(AtomSemantic::Int, &[5]);
        let (a, b) = (db.generic("A"), db.generic("B"));
        let mut infer = Inference::new([a, b]);

        // `B ≤ A` was collected under the first member of a union, which didn't match
        let choice = infer.open_choice(None, 2);
        let unmatched = BranchId { choice, member: 0 };
        infer.record(unmatched, Comparison::Invalid);
        infer.record(BranchId { choice, member: 1 }, Comparison::Assign);
        infer.push(a, CandidateKind::Lower, b, vec![unmatched]);
        infer.push(a, CandidateKind::Lower, five, Vec::new());

        // So the bound that `A ≤ Int` passes on to `B` is only collected under it too
        let mut expected = Inference::new([a, b]);
        expected.push(a, CandidateKind::Upper, int, Vec::new());
        add_expected_bounds(&mut db.arena, &db.builtins, &mut infer, &expected);

        assert_eq!(candidates(&infer, b).upper, vec![int]);
        assert_eq!(infer.matched_candidates(b).unwrap().upper, vec![]);
        assert_eq!(candidates(&infer, a).lower, vec![b, five]);
        assert_eq!(infer.matched_candidates(a).unwrap().lower, vec![five]);
    }

    #[test]
    fn passed_bounds_keep_the_members_of_the_bound_being_passed() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let (a, b, c) = (db.generic("A"), db.generic("B"), db.generic("C"));
        let mut infer = Inference::new([a, b, c]);

        // `A ≤ (Int, Int)` was collected under a member that didn't match. It's passed on to `B`
        // through `B ≤ A`, and to `C` through `(C, Int) ≤ A`, so what they get only holds under
        // that member too
        let choice = infer.open_choice(None, 2);
        let unmatched = BranchId { choice, member: 0 };
        infer.record(unmatched, Comparison::Invalid);
        infer.record(BranchId { choice, member: 1 }, Comparison::Assign);
        let compound = db.pair(c, int);
        infer.push(a, CandidateKind::Lower, b, Vec::new());
        infer.push(a, CandidateKind::Lower, compound, Vec::new());

        let int_pair = db.pair(int, int);
        let existing = vec![(a, int_pair, vec![unmatched])];
        pass_upper_bounds(
            &mut db.arena,
            &db.builtins,
            &mut infer,
            Vec::new(),
            existing,
            |_| true,
        );

        assert_eq!(candidates(&infer, b).upper, vec![int_pair]);
        assert_eq!(infer.matched_candidates(b).unwrap().upper, vec![]);
        assert_eq!(candidates(&infer, c).upper, vec![int]);
        assert_eq!(infer.matched_candidates(c).unwrap().upper, vec![]);
    }

    #[test]
    fn same_bound_under_other_members_is_passed_on_again() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let (a, b) = (db.generic("A"), db.generic("B"));
        let mut infer = Inference::new([a, b]);

        // `A ≤ Int` was collected twice, once under a member that didn't match and once under none.
        // Both are passed on to `B` through `B ≤ A`, since skipping the second as the same bound
        // would leave `B` only the one that doesn't hold
        let choice = infer.open_choice(None, 2);
        let unmatched = BranchId { choice, member: 0 };
        infer.record(unmatched, Comparison::Invalid);
        infer.record(BranchId { choice, member: 1 }, Comparison::Assign);
        infer.push(a, CandidateKind::Lower, b, Vec::new());

        let existing = vec![(a, int, vec![unmatched]), (a, int, Vec::new())];
        pass_upper_bounds(
            &mut db.arena,
            &db.builtins,
            &mut infer,
            Vec::new(),
            existing,
            |_| true,
        );

        assert_eq!(candidates(&infer, b).upper, vec![int, int]);
        assert_eq!(infer.matched_candidates(b).unwrap().upper, vec![int]);
    }

    #[test]
    fn choices_leave_out_a_member_that_bounds_when_another_holds_without() {
        let mut choices = Choices::default();

        // Both members are assignable, but only the first bounded a generic being instantiated,
        // so its bounds aren't needed (`A ≥ nil` when comparing `nil` with `A | nil`)
        let choice = choices.open(None, 2);
        let bounded = BranchId { choice, member: 0 };
        let holding = BranchId { choice, member: 1 };
        choices.record(bounded, Comparison::Assign);
        choices.mark_bounded(bounded);
        choices.record(holding, Comparison::Assign);
        assert!(!choices.matched_so_far(bounded));
        assert!(choices.matched_so_far(holding));

        // One that only casts doesn't hold it, so the member that bounds is kept
        let choice = choices.open(None, 2);
        let bounded = BranchId { choice, member: 0 };
        let casting = BranchId { choice, member: 1 };
        choices.record(bounded, Comparison::Assign);
        choices.mark_bounded(bounded);
        choices.record(casting, Comparison::Cast);
        assert!(choices.matched_so_far(bounded));
        assert!(!choices.matched_so_far(casting));

        // A member that's still being compared holds, and so does what's nested in it
        let outer = choices.open(None, 2);
        let in_progress = BranchId {
            choice: outer,
            member: 0,
        };
        let inner = choices.open(Some(in_progress), 2);
        let matched = BranchId {
            choice: inner,
            member: 0,
        };
        choices.record(matched, Comparison::Assign);
        choices.record(
            BranchId {
                choice: inner,
                member: 1,
            },
            Comparison::Invalid,
        );
        assert!(choices.matched_so_far(matched));
        assert!(!choices.matched_so_far(BranchId {
            choice: inner,
            member: 1,
        }));
    }

    /// What's collected under a union member holds as far as the choices from an index on decide,
    /// which leaves out the ones it's nested in that are still being compared (see
    /// `ComparisonContext::summary_since`).
    #[test]
    fn choices_matched_since() {
        let mut choices = Choices::default();
        let outer = choices.open(None, 2);
        let outer_member = BranchId {
            choice: outer,
            member: 1,
        };
        choices.record(
            BranchId {
                choice: outer,
                member: 0,
            },
            Comparison::Assign,
        );

        let inner = choices.open(Some(outer_member), 2);
        let [missed, matched] = [0, 1].map(|member| BranchId {
            choice: inner,
            member,
        });
        choices.record(missed, Comparison::Invalid);
        choices.record(matched, Comparison::Assign);

        // Once the outer member gives `Cast`, it doesn't match, but from the inner choice on only
        // the inner member decides
        choices.record(outer_member, Comparison::Cast);
        assert!(!choices.matched_so_far(matched));
        assert!(choices.matched_since(matched, inner));
        assert!(!choices.matched_since(missed, inner));
        assert!(!choices.matched_since(matched, outer));

        // A branch from before the index holds
        assert!(choices.matched_since(outer_member, inner));
    }

    /// A comparison that's revisited is computed again in rounds, until what it puts on the
    /// instances its key refers to stops changing, and each round undoes the candidates the one
    /// before collected, so that they're collected once.
    #[test]
    fn rounds_collect_candidates_once() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let t = db.generic("T");

        // type Rec = fn<A>(x: A) -> (A, (Int, Rec));
        let rec = db.declare_alias("Rec", &[]);
        let a = db.generic("A");
        let rest = db.pair(int, rec);
        let ret = db.pair(a, rest);
        let function = db.function_with(&[a], &[a], true, ret);
        db.define(rec, function);

        // type R2 = (Int, (T, Again)); type Again = fn(x: Int) -> R2;
        let r2 = db.declare_alias("R2", &[]);
        let again = db.declare_alias("Again", &[]);
        let rest = db.pair(t, again);
        let inner = db.pair(int, rest);
        db.define(r2, inner);
        let function = db.function(&[int], r2);
        db.define(again, function);
        let target = db.function(&[int], r2);

        let mut infer = Inference::new([t]);
        assert_eq!(db.infer(rec, target, &mut infer), Comparison::Assign);
        assert_eq!(candidates(&infer, t).lower, vec![int]);
    }

    /// A bound that a revisit gets from a union member that didn't match is checked under an
    /// inference member that didn't match either, so the candidates it collects are left out of
    /// the matched ones, like those of the original.
    #[test]
    fn revisits_keep_unmatched_candidates_out() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let nil = db.builtins.nil;
        let t = db.generic("T");

        // type Rec = fn<A>(x: A) -> (A, Rec);
        let rec = db.declare_alias("Rec", &[]);
        let a = db.generic("A");
        let ret = db.pair(a, rec);
        let function = db.function_with(&[a], &[a], true, ret);
        db.define(rec, function);

        // type R2 = (Int, Again) | (T, nil); type Again = fn(x: Int) -> R2;
        let r2 = db.declare_alias("R2", &[]);
        let again = db.declare_alias("Again", &[]);
        let matched = db.pair(int, again);
        let unmatched = db.pair(t, nil);
        let union = db.union(&[matched, unmatched]);
        db.define(r2, union);
        let function = db.function(&[int], r2);
        db.define(again, function);
        let target = db.function(&[int], r2);

        let mut infer = Inference::new([t]);
        assert_eq!(db.infer(rec, target, &mut infer), Comparison::Assign);
        assert_eq!(candidates(&infer, t).lower, vec![int, int]);
        assert_eq!(infer.matched_candidates(t).unwrap().lower, Vec::new());
    }

    /// Restoring a snapshot removes the candidates and choices collected since, so that a
    /// comparison computed again collects them once.
    #[test]
    fn inference_restores_snapshot() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let (a, b) = (db.generic("A"), db.generic("B"));
        let mut infer = Inference::new([a, b]);
        infer.push(a, CandidateKind::Lower, int, Vec::new());

        let before = infer.clone();
        let snapshot = infer.snapshot();

        let choice = infer.open_choice(None, 2);
        let branch = BranchId { choice, member: 0 };
        infer.record(branch, Comparison::Assign);
        infer.push(a, CandidateKind::Lower, b, vec![branch]);
        infer.push(b, CandidateKind::Upper, int, vec![branch]);
        infer.push(b, CandidateKind::Held, int, Vec::new());

        infer.restore(&snapshot);
        assert_eq!(infer.candidates(a), before.candidates(a));

        // The union members are removed along with the candidates, so the next one is in step
        let choice = infer.open_choice(None, 2);
        let next = BranchId { choice, member: 1 };
        let index = infer.push(a, CandidateKind::Lower, b, vec![next]);
        assert_eq!(infer.branches(a, CandidateKind::Lower, index), [next]);
        infer.restore(&snapshot);

        assert_eq!(infer.candidates(b), before.candidates(b));
        assert_eq!(infer.choice_results(), before.choice_results());
        assert_eq!(infer.matched_candidates(a), before.matched_candidates(a));
    }

    #[test]
    fn bounds_passed_through_compound_lower_bounds_keep_their_members() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let (a, c) = (db.generic("A"), db.generic("C"));
        let mut infer = Inference::new([a, c]);

        // `(C, Int) ≤ A` was collected under a member that didn't match, so what `A ≤ (Int, Int)`
        // gives `C` through it only holds under that member
        let choice = infer.open_choice(None, 2);
        let unmatched = BranchId { choice, member: 0 };
        infer.record(unmatched, Comparison::Invalid);
        infer.record(BranchId { choice, member: 1 }, Comparison::Assign);
        let compound = db.pair(c, int);
        infer.push(a, CandidateKind::Lower, compound, vec![unmatched]);

        let int_pair = db.pair(int, int);
        let existing = vec![(a, int_pair, Vec::new())];
        pass_upper_bounds(
            &mut db.arena,
            &db.builtins,
            &mut infer,
            Vec::new(),
            existing,
            |_| true,
        );

        assert_eq!(candidates(&infer, c).upper, vec![int]);
        assert_eq!(infer.matched_candidates(c).unwrap().upper, vec![]);
    }

    #[test]
    fn upper_bounds_passed_through_other_generics_keep_their_members() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let (a, b) = (db.generic("A"), db.generic("B"));
        let mut infer = Inference::new([a, b]);

        let choice = infer.open_choice(None, 2);
        let unmatched = BranchId { choice, member: 0 };
        infer.record(unmatched, Comparison::Invalid);
        infer.record(BranchId { choice, member: 1 }, Comparison::Assign);
        infer.push(a, CandidateKind::Upper, int, vec![unmatched]);

        assert_eq!(upper_bounds(&infer), [(a, int, vec![unmatched])]);
    }

    #[test]
    fn appended_inferences_match_one_inference() {
        let mut db = Db::new();
        let (int, bytes, nil) = (db.builtins.int, db.builtins.bytes, db.builtins.nil);
        let t = db.generic("T");

        // Two arguments compared against params with nested unions, as if into one inference
        let optional = db.union(&[t, nil]);
        let first_param = db.pair(optional, int);
        let other = db.pair(int, t);
        let first_param = db.union(&[first_param, other]);
        let first_value = db.pair(int, bytes);
        let second_param = db.union(&[t, nil]);
        let second_value = db.union(&[bytes, nil]);

        // The one with a nested union goes second, so that its choices' parents need shifting
        let mut together = Inference::new([t]);
        db.infer(second_value, second_param, &mut together);
        db.infer(first_value, first_param, &mut together);

        let mut first = Inference::new([t]);
        db.infer(second_value, second_param, &mut first);
        let mut second = Inference::new([t]);
        db.infer(first_value, first_param, &mut second);

        // Appending to an empty inference, and appending one with no unions, change nothing else
        let mut appended = Inference::new([t]);
        appended.append(&first);
        appended.append(&second);
        appended.append(&Inference::new([t]));

        assert_eq!(candidates(&appended, t), candidates(&together, t));
        assert_eq!(
            appended.matched_candidates(t),
            together.matched_candidates(t)
        );

        // Each candidate holds under the same members as in the one inference
        let all = candidates(&together, t);
        for (kind, len) in [
            (CandidateKind::Lower, all.lower.len()),
            (CandidateKind::Upper, all.upper.len()),
            (CandidateKind::Held, all.held.len()),
        ] {
            for index in 0..len {
                assert_eq!(
                    appended.branches(t, kind, index),
                    together.branches(t, kind, index)
                );
            }
        }

        // The comparison with the nested union has a member whose candidate doesn't hold, in either
        assert_ne!(
            candidates(&together, t),
            together.matched_candidates(t).unwrap()
        );
    }

    #[test]
    fn lower_bounds_pass_into_compound_upper_bounds() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let (one, two) = (
            db.literal(AtomSemantic::Int, &[1]),
            db.literal(AtomSemantic::Int, &[2]),
        );
        let (a, b, c) = (db.generic("A"), db.generic("B"), db.generic("C"));
        let mut infer = Inference::new([a, b, c]);

        // `(1, 2) ≤ A ≤ (B, B)` under a member that didn't match, and `A ≤ C`, which is just a
        // generic
        let choice = infer.open_choice(None, 2);
        let unmatched = BranchId { choice, member: 0 };
        infer.record(unmatched, Comparison::Invalid);
        infer.record(BranchId { choice, member: 1 }, Comparison::Assign);
        let value = db.pair(one, two);
        let pair = db.pair(b, b);
        infer.push(a, CandidateKind::Lower, value, Vec::new());
        infer.push(a, CandidateKind::Upper, pair, vec![unmatched]);
        infer.push(a, CandidateKind::Upper, c, Vec::new());

        // Only the generics `accepts` allows get them
        let mut refused = infer.clone();
        pass_lower_bounds(&mut db.arena, &db.builtins, &mut refused, |_| false);
        assert_eq!(candidates(&refused, b).lower, vec![]);

        pass_lower_bounds(&mut db.arena, &db.builtins, &mut infer, |_| true);
        assert_eq!(candidates(&infer, b).lower, vec![one, two]);

        // Under the upper bound's members, and not through an upper bound that's just a generic
        assert_eq!(infer.matched_candidates(b).unwrap().lower, vec![]);
        assert_eq!(candidates(&infer, c).lower, vec![]);

        // Which resolves `B` once nothing else infers it
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&b], int);
    }

    #[test]
    fn retained_bounds_keep_the_members_they_were_collected_under() {
        let mut db = Db::new();
        let (int, bytes, nil) = (db.builtins.int, db.builtins.bytes, db.builtins.nil);
        let (t, u) = (db.generic("T"), db.generic("U"));
        let mut infer = Inference::new([t]);

        let choice = infer.open_choice(None, 3);
        let branch = |member| BranchId { choice, member };
        infer.push(t, CandidateKind::Lower, u, vec![branch(0)]);
        infer.push(t, CandidateKind::Lower, int, vec![branch(1)]);
        infer.push(t, CandidateKind::Upper, u, vec![branch(0)]);
        infer.push(t, CandidateKind::Upper, bytes, vec![branch(2)]);
        infer.push(t, CandidateKind::Held, nil, vec![branch(1)]);

        // The bounds on `U` are dropped, and the others keep their members. A bound was dropped,
        // so the held candidates go too, along with their members
        infer.retain_bounds(|ty| ty != u);
        assert_eq!(candidates(&infer, t).lower, vec![int]);
        assert_eq!(infer.branches(t, CandidateKind::Lower, 0), [branch(1)]);
        assert_eq!(candidates(&infer, t).upper, vec![bytes]);
        assert_eq!(infer.branches(t, CandidateKind::Upper, 0), [branch(2)]);
        assert_eq!(candidates(&infer, t).held, vec![]);

        // A held candidate added afterward has its own members
        infer.push(t, CandidateKind::Held, nil, Vec::new());
        assert_eq!(infer.branches(t, CandidateKind::Held, 0), []);
    }

    #[test]
    fn functions_only_add_themselves_to_unions_that_hold_them() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // `fn(p0: Int) -> Int` to `T | nil` only gives `T` the function, not its runtime structure
        let function = db.function(&[int], int);
        let param = db.union(&[t, nil]);
        assert_eq!(db.infer(function, param, &mut infer), Comparison::Assign);
        assert_eq!(candidates(&infer, t).lower.len(), 1);
        assert_eq!(
            db.name(candidates(&infer, t).lower[0]),
            "fn(p0: Int) -> Int"
        );
        assert_eq!(candidates(&infer, t).held, vec![]);
    }

    #[test]
    fn leaves_out_function_candidates_held_by_others() {
        let mut db = Db::new();
        let (int, bytes) = (db.builtins.int, db.builtins.bytes);
        let (t, v, w) = (db.generic("T"), db.generic("V"), db.generic("W"));
        let identity = db.function_with(&[v], &[v], true, v);
        let other_identity = db.function_with(&[w], &[w], true, w);
        let increment = db.function(&[int], int);
        let decrement = db.function(&[int], int);
        let reverse = db.function(&[bytes], bytes);

        let pick = |db: &mut Db, types: &[TypeId]| {
            let mut infer = Inference::new([t]);
            for &ty in types {
                assert_eq!(db.infer(ty, t, &mut infer), Comparison::Assign);
            }
            let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
            db.name(mappings[&t])
        };

        // A generic function next to one of its instances is left out, in either order
        assert_eq!(pick(&mut db, &[identity, increment]), "fn(p0: Int) -> Int");
        assert_eq!(pick(&mut db, &[increment, identity]), "fn(p0: Int) -> Int");

        // Of function types that are assignable to each other, the first is kept
        assert_eq!(pick(&mut db, &[increment, decrement]), "fn(p0: Int) -> Int");
        assert_eq!(
            pick(&mut db, &[identity, other_identity]),
            "fn<V>(p0: V) -> V"
        );
        assert_eq!(
            pick(&mut db, &[other_identity, identity]),
            "fn<W>(p0: W) -> W"
        );

        // Function types that aren't are kept as a union
        assert_eq!(
            pick(&mut db, &[increment, reverse]),
            "fn(p0: Int) -> Int | fn(p0: Bytes) -> Bytes"
        );
    }

    #[test]
    fn skips_union_members_held_by_others() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // `Int | nil` to `T | nil` only needs `T` to hold `Int`
        let value = db.union(&[int, nil]);
        let param = db.union(&[t, nil]);
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);
        assert_eq!(
            candidates(&infer, t),
            InferenceCandidates {
                lower: vec![int],
                upper: vec![],
                held: vec![nil],
            }
        );

        // A value that is only castable to `nil` (such as `0`) still has to be held by `T`
        let zero = db.literal(AtomSemantic::Int, &[]);
        assert_eq!(db.infer(zero, param, &mut infer), Comparison::Assign);
        assert_eq!(candidates(&infer, t).lower, vec![int, zero]);
    }

    #[test]
    fn held_union_members_are_a_last_resort() {
        let mut db = Db::new();
        let nil = db.builtins.nil;
        let t = db.generic("T");
        let mut infer = Inference::new([t]);

        // `nil` to `T | nil` doesn't constrain `T`, but it's better than nothing
        let param = db.union(&[t, nil]);
        assert_eq!(db.infer(nil, param, &mut infer), Comparison::Assign);
        assert_eq!(candidates(&infer, t).lower, vec![]);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&t], nil);
    }

    #[test]
    fn struct_contents_held_by_other_members() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let generic = db.generic("T");
        let boxed = db.declare_struct("Box", &[generic]);
        let inner = db.union(&[generic, nil]);
        db.define(boxed, inner);

        // `Box<Int>` to `Box<T>` infers `T` as `Int` rather than the `Int | nil` contents
        let t = db.generic("T");
        let mut infer = Inference::new([t]);
        let value = db.apply(boxed, &[(generic, int)]);
        let param = db.apply(boxed, &[(generic, t)]);
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);
        assert_eq!(candidates(&infer, t).lower, vec![int]);
    }

    #[test]
    fn nested_generic_held_by_other_members() {
        let mut db = Db::new();
        let (int, bytes32, nil) = (db.builtins.int, db.builtins.bytes32, db.builtins.nil);
        let generic = db.generic("T");
        let inner = db.union(&[generic, bytes32]);
        let maybe = db.alias("Maybe", &[generic], inner);

        // `Int | nil` to `Maybe<T> | nil` only needs `T` to hold `Int`, even though `nil` would
        // also reach `T` through the alias
        let t = db.generic("T");
        let mut infer = Inference::new([t]);
        let maybe_t = db.apply(maybe, &[(generic, t)]);
        let param = db.union(&[maybe_t, nil]);
        let value = db.union(&[int, nil]);
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);
        assert_eq!(
            candidates(&infer, t),
            InferenceCandidates {
                lower: vec![int],
                upper: vec![],
                held: vec![nil],
            }
        );
    }

    /// Infers `A` and `B` from passing `fn<U>(x: U) -> U` to `fn(a: A) -> B` and `Int` to `A`.
    fn infer_map_identity(db: &mut Db, a: TypeId, b: TypeId, infer: &mut Inference) {
        let int = db.builtins.int;
        let u = db.generic("U");
        let identity = db.function_with(&[u], &[u], true, u);
        let param = db.function(&[a], b);
        assert_eq!(db.infer(identity, param, infer), Comparison::Assign);
        assert_eq!(db.infer(int, a, infer), Comparison::Assign);
    }

    #[test]
    fn resolves_through_other_generics() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let (a, b) = (db.generic("A"), db.generic("B"));
        let mut infer = Inference::new([a, b]);
        infer_map_identity(&mut db, a, b, &mut infer);
        assert_eq!(candidates(&infer, b).lower, vec![a]);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&a], int);
        assert_eq!(mappings[&b], int);
    }

    #[test]
    fn resolves_compound_candidates_through_other_generics() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let (a, b) = (db.generic("A"), db.generic("B"));
        let mut infer = Inference::new([a, b]);

        // `(A, nil)` to `B` and `Int` to `A`
        let pair = db.pair(a, nil);
        assert_eq!(db.infer(pair, b, &mut infer), Comparison::Assign);
        assert_eq!(db.infer(int, a, &mut infer), Comparison::Assign);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(db.name(mappings[&b]), "(Int, nil)");
    }

    #[test]
    fn instantiates_generics_in_candidates() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let (a, b) = (db.generic("A"), db.generic("B"));
        let mut infer = Inference::new([a, b]);

        // `fn<T>(x: T) -> (T, T)` to `fn(a: A) -> B`, where `T` is instantiated as `A`
        let t = db.generic("T");
        let ret = db.pair(t, t);
        let duplicate = db.function_with(&[t], &[t], true, ret);
        let param = db.function(&[a], b);
        assert_eq!(db.infer(duplicate, param, &mut infer), Comparison::Assign);
        assert_eq!(db.name(candidates(&infer, b).lower[0]), "(A, A)");

        assert_eq!(db.infer(int, a, &mut infer), Comparison::Assign);
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(db.name(mappings[&b]), "(Int, Int)");
    }

    #[test]
    fn instantiates_generics_in_upper_candidates() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let a = db.generic("A");
        let mut infer = Inference::new([a]);

        // `fn<T>(x: (T, T)) -> T` to `fn(a: A) -> Int`, where `T` is instantiated as `Int`
        let t = db.generic("T");
        let pair = db.pair(t, t);
        let first = db.function_with(&[t], &[pair], true, t);
        let param = db.function(&[a], int);
        assert_eq!(db.infer(first, param, &mut infer), Comparison::Assign);
        assert_eq!(db.name(candidates(&infer, a).upper[0]), "(Int, Int)");
    }

    #[test]
    fn instantiates_generics_in_candidates_from_expected_type() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let (a, b) = (db.generic("A"), db.generic("B"));
        let mut infer = Inference::new([a, b]);

        // `fn(g: fn(x: A) -> B) -> Int` is expected to be `fn(g: fn<T>(x: T) -> (T, T)) -> Int`
        let t = db.generic("T");
        let ret = db.pair(t, t);
        let duplicate = db.function_with(&[t], &[t], true, ret);
        let expected = db.function(&[duplicate], int);
        let param = db.function(&[a], b);
        let ty = db.function(&[param], int);
        infer_from_expected(&mut db.arena, &db.builtins, ty, expected, &mut infer);
        assert_eq!(db.name(candidates(&infer, b).lower[0]), "(A, A)");
    }

    #[test]
    fn instantiates_upper_bounded_instances_in_lower_candidates_as_never() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let b = db.generic("B");
        let mut infer = Inference::new([b]);

        // `fn<T>(g: fn(x: T) -> nil) -> (T, T)` to `fn(g: fn(x: Int) -> nil) -> B`, where `T` is
        // only bounded from above, so its least instantiation is `Never` (#339)
        let t = db.generic("T");
        let ret = db.pair(t, t);
        let callback = db.function(&[t], nil);
        let function = db.function_with(&[t], &[callback], true, ret);
        let sink = db.function(&[int], nil);
        let param = db.function(&[sink], b);
        assert_eq!(db.infer(function, param, &mut infer), Comparison::Assign);
        assert_eq!(db.name(candidates(&infer, b).lower[0]), "(Never, Never)");
    }

    #[test]
    fn instantiates_lower_bounded_instances_in_upper_candidates_as_any() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let a = db.generic("A");
        let mut infer = Inference::new([a]);

        // `fn<T>(x: (T, T), y: T) -> nil` to `fn(a: A, b: Int) -> nil`, where `T` is only bounded
        // from below, so its greatest instantiation is `Any` (#339)
        let t = db.generic("T");
        let pair = db.pair(t, t);
        let function = db.function_with(&[t], &[pair, t], true, nil);
        let param = db.function(&[a, int], nil);
        assert_eq!(db.infer(function, param, &mut infer), Comparison::Assign);
        assert_eq!(db.name(candidates(&infer, a).upper[0]), "(Any, Any)");
    }

    #[test]
    fn instantiates_unconstrained_instances_in_lower_candidates_as_never() {
        let mut db = Db::new();
        let b = db.generic("B");
        let mut infer = Inference::new([b]);

        // `fn<T>() -> (T, T)` to `fn() -> B`, where nothing constrains `T` (#339)
        let t = db.generic("T");
        let ret = db.pair(t, t);
        let pairs = db.function_with(&[t], &[], true, ret);
        let param = db.function(&[], b);
        assert_eq!(db.infer(pairs, param, &mut infer), Comparison::Assign);
        assert_eq!(db.name(candidates(&infer, b).lower[0]), "(Never, Never)");
    }

    #[test]
    fn instantiates_unconstrained_instances_in_upper_candidates_as_any() {
        let mut db = Db::new();
        let nil = db.builtins.nil;
        let a = db.generic("A");
        let mut infer = Inference::new([a]);

        // `fn<T>(x: (T, T)) -> nil` to `fn(a: A) -> nil`, where nothing constrains `T` (#339)
        let t = db.generic("T");
        let pair = db.pair(t, t);
        let ignore = db.function_with(&[t], &[pair], true, nil);
        let param = db.function(&[a], nil);
        assert_eq!(db.infer(ignore, param, &mut infer), Comparison::Assign);
        assert_eq!(db.name(candidates(&infer, a).upper[0]), "(Any, Any)");
    }

    #[test]
    fn leaves_instances_bounded_by_rigid_generics_in_candidates() {
        let mut db = Db::new();
        let b = db.generic("B");
        let mut infer = Inference::new([b]);

        // `fn<T>(x: T) -> (T, T)` to `fn<U>(a: U) -> B`, where `T` is bounded by the rigid `U`
        let (t, u) = (db.generic("T"), db.generic("U"));
        let ret = db.pair(t, t);
        let duplicate = db.function_with(&[t], &[t], true, ret);
        let param = db.function_with(&[u], &[u], true, b);
        db.infer(duplicate, param, &mut infer);
        assert_eq!(db.name(candidates(&infer, b).lower[0]), "(T, T)");
    }

    #[test]
    fn instantiates_instances_in_held_candidates() {
        let mut db = Db::new();
        let (int, any) = (db.builtins.int, db.builtins.permissive_any);
        let b = db.generic("B");
        let mut infer = Inference::new([b]);

        // `fn<T>(x: T) -> (T, T)` to `fn(x: Int) -> B | (Any, Any)`, where `(Any, Any)` already
        // holds the return value, so `B` only holds `(T, T)` as a last resort
        let t = db.generic("T");
        let ret = db.pair(t, t);
        let duplicate = db.function_with(&[t], &[t], true, ret);
        let any_pair = db.pair(any, any);
        let union = db.union(&[b, any_pair]);
        let param = db.function(&[int], union);
        assert_eq!(db.infer(duplicate, param, &mut infer), Comparison::Assign);

        let held = &candidates(&infer, b).held;
        assert_eq!(held.len(), 1);
        assert_eq!(db.name(held[0]), "(Int, Int)");
    }

    #[test]
    fn instantiates_instances_in_lower_and_upper_candidates_separately() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let (a, b) = (db.generic("A"), db.generic("B"));
        let mut infer = Inference::new([a, b]);

        // `fn<T>(x: T, y: (T, T)) -> (T, T)` to `fn(x: Int, y: A) -> B`, where the same instance
        // is the least instantiation in `B`'s lower candidate and the greatest in `A`'s upper one
        let t = db.generic("T");
        let pair = db.pair(t, t);
        let function = db.function_with(&[t], &[t, pair], true, pair);
        let param = db.function(&[int, a], b);
        assert_eq!(db.infer(function, param, &mut infer), Comparison::Assign);

        assert_eq!(db.name(candidates(&infer, b).lower[0]), "(Int, Int)");
        assert_eq!(db.name(candidates(&infer, a).upper[0]), "(Any, Any)");
    }

    #[test]
    fn instantiates_each_instance_in_a_candidate() {
        let mut db = Db::new();
        let (int, bytes) = (db.builtins.int, db.builtins.bytes);
        let b = db.generic("B");
        let mut infer = Inference::new([b]);

        // `fn<T, U>(x: T, y: U) -> (T, U)` to `fn(x: Int, y: Bytes) -> B`
        let (t, u) = (db.generic("T"), db.generic("U"));
        let ret = db.pair(t, u);
        let function = db.function_with(&[t, u], &[t, u], true, ret);
        let param = db.function(&[int, bytes], b);
        assert_eq!(db.infer(function, param, &mut infer), Comparison::Assign);

        assert_eq!(db.name(candidates(&infer, b).lower[0]), "(Int, Bytes)");
    }

    #[test]
    fn resolves_cycles_between_generics() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let (a, b) = (db.generic("A"), db.generic("B"));

        // `B` to `A`, `A` to `B` and `Int` to `B`, with the generics in either order
        let mut results = Vec::new();

        for generics in [[a, b], [b, a]] {
            let mut infer = Inference::new(generics);
            assert_eq!(db.infer(b, a, &mut infer), Comparison::Assign);
            assert_eq!(db.infer(a, b, &mut infer), Comparison::Assign);
            assert_eq!(db.infer(int, b, &mut infer), Comparison::Assign);
            results.push(resolve_inference(&mut db.arena, &db.builtins, &infer));
        }

        assert_eq!(results[0][&a], int);
        assert_eq!(results[0][&b], int);
        assert_eq!(results[0], results[1]);
    }

    #[test]
    fn resolves_upper_candidates_through_other_generics() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let (a, b) = (db.generic("A"), db.generic("B"));
        let mut infer = Inference::new([a, b]);

        // `fn(x: B) -> nil` to `fn(x: A) -> nil` and `fn(x: Int) -> nil` to `fn(x: B) -> nil`
        let takes_b = db.function(&[b], nil);
        let takes_a = db.function(&[a], nil);
        let takes_int = db.function(&[int], nil);
        assert_eq!(db.infer(takes_b, takes_a, &mut infer), Comparison::Assign);
        assert_eq!(db.infer(takes_int, takes_b, &mut infer), Comparison::Assign);
        assert_eq!(candidates(&infer, a).upper, vec![b]);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&a], int);
        assert_eq!(mappings[&b], int);
    }

    #[test]
    fn resolves_held_candidates_through_other_generics() {
        let mut db = Db::new();
        let (int, nil, recursive_any) =
            (db.builtins.int, db.builtins.nil, db.builtins.recursive_any);
        let (a, b) = (db.generic("A"), db.generic("B"));
        let mut infer = Inference::new([a, b]);

        // `Int` to `B` and `(B, nil)` to `A | Any`, which already holds it
        assert_eq!(db.infer(int, b, &mut infer), Comparison::Assign);
        let pair = db.pair(b, nil);
        let param = db.union(&[a, recursive_any]);
        assert_eq!(db.infer(pair, param, &mut infer), Comparison::Assign);
        assert_eq!(candidates(&infer, a).held.len(), 1);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(db.name(mappings[&a]), "(Int, nil)");
    }

    #[test]
    fn compound_candidates_in_cycles_are_left_as_is() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let (a, b) = (db.generic("A"), db.generic("B"));

        // `Int` to `A`, `B` to `A` and `(A, nil)` to `B`, with the generics in either order.
        // Resolving `B` through `A` would make it an infinite type.
        let mut results = Vec::new();

        for generics in [[a, b], [b, a]] {
            let mut infer = Inference::new(generics);
            assert_eq!(db.infer(int, a, &mut infer), Comparison::Assign);
            assert_eq!(db.infer(b, a, &mut infer), Comparison::Assign);
            let pair = db.pair(a, nil);
            assert_eq!(db.infer(pair, b, &mut infer), Comparison::Assign);
            results.push(resolve_inference(&mut db.arena, &db.builtins, &infer));
        }

        for result in &results {
            assert_eq!(db.name(result[&b]), "(A, nil)");
        }
    }

    #[test]
    fn identical_inferences_need_the_same_order_choices_and_vars() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let (a, b) = (db.generic("A"), db.generic("B"));

        let mut infer = Inference::new([a, b]);
        assert_eq!(db.infer(int, a, &mut infer), Comparison::Assign);
        assert!(infer.is_identical(&infer.clone()));

        // The same generics and candidates in another order give results in another order, which
        // `IndexMap`'s own equality would ignore
        let mut reordered = Inference::new([b, a]);
        assert_eq!(db.infer(int, a, &mut reordered), Comparison::Assign);
        assert!(!infer.is_identical(&reordered));

        // A union that a value was compared against is recorded even when no candidate is
        let mut with_choice = Inference::new([a, b]);
        assert_eq!(db.infer(int, a, &mut with_choice), Comparison::Assign);
        let union = db.alloc(Type::Union(Union::new(vec![int, nil])));
        assert_eq!(db.infer(nil, union, &mut with_choice), Comparison::Assign);
        assert!(!infer.is_identical(&with_choice));

        // So is a var added along with the generics, even next to the same generic with the same
        // candidates that isn't one
        let var = db.generic("V");
        let mut added = Inference::new([a, b]);
        assert_eq!(db.infer(int, a, &mut added), Comparison::Assign);
        added.add_var(var, int);
        let mut plain = Inference::new([a, b, var]);
        assert_eq!(db.infer(int, a, &mut plain), Comparison::Assign);
        plain.push(var, CandidateKind::Upper, int, Vec::new());
        assert_eq!(added.generics, plain.generics);
        assert!(!added.is_identical(&plain));
        assert!(added.is_identical(&added.clone()));
    }

    #[test]
    fn resolves_through_fallback_types() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let (a, b) = (db.generic("A"), db.generic("B"));
        let mut infer = Inference::new([a, b]);

        // `A` to `B`, where only the fallback gives `A` a type
        assert_eq!(db.infer(a, b, &mut infer), Comparison::Assign);

        let fallback = HashMap::from([(a, int)]);
        let mappings =
            resolve_inference_with_fallback(&mut db.arena, &db.builtins, &infer, &fallback);
        assert_eq!(mappings[&a], int);
        assert_eq!(mappings[&b], int);
    }

    #[test]
    fn renamed_generics_compare_as_the_original() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let (a, renamed, b) = (db.generic("A"), db.generic("A"), db.generic("B"));
        let mut infer = Inference::new([renamed, b]);
        infer.rename(renamed, a);

        // `A` to the generic renamed from it, as in a recursive call, holds, and the renamed one
        // takes `A` as a candidate, as it's explicitly instantiated as `A` (#370)
        assert_eq!(db.infer(a, renamed, &mut infer), Comparison::Assign);
        assert_eq!(candidates(&infer, renamed).lower, vec![a]);
        assert_eq!(candidates(&infer, renamed).held, vec![]);

        // `(A, Int)` already fits the union's `(A, Int)` member, so it's only held for `B`
        let pair = db.pair(a, int);
        let member = db.pair(renamed, int);
        let param = db.union(&[member, b]);
        assert_eq!(db.infer(pair, param, &mut infer), Comparison::Assign);
        assert_eq!(candidates(&infer, b).lower, vec![]);
        let held = &candidates(&infer, b).held;
        assert_eq!(held.len(), 1);
        assert_eq!(db.name(held[0]), "(A, Int)");

        // A generic that wasn't renamed from `A` is still inferred from it
        let mut infer = Inference::new([renamed]);
        assert_eq!(db.infer(a, renamed, &mut infer), Comparison::Assign);
        assert_eq!(candidates(&infer, renamed).lower, vec![a]);
    }

    #[test]
    fn renamed_generics_take_the_original_in_a_union() {
        let mut db = Db::new();
        let (nil, any) = (db.builtins.nil, db.builtins.permissive_any);
        let (a, renamed) = (db.generic("A"), db.generic("A"));

        // `A | nil` to `A' | nil` gives `A'` the candidate `A`, and only holds `nil`, so `A'` is
        // `A`, rather than the held `nil` (#370)
        let mut infer = Inference::new([renamed]);
        infer.rename(renamed, a);
        let value = db.union(&[a, nil]);
        let param = db.union(&[renamed, nil]);
        assert_eq!(db.infer(value, param, &mut infer), Comparison::Assign);
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&renamed], a);

        // Next to a candidate that holds it, `A` adds nothing, so `A'` is `Any`, as before
        let mut infer = Inference::new([renamed]);
        infer.rename(renamed, a);
        assert_eq!(db.infer(any, renamed, &mut infer), Comparison::Assign);
        assert_eq!(db.infer(a, renamed, &mut infer), Comparison::Assign);
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&renamed], any);
    }

    #[test]
    fn renamed_generics_take_passed_bounds() {
        let mut db = Db::new();
        let (a, renamed, b) = (db.generic("A"), db.generic("A"), db.generic("B"));
        let five = db.literal(AtomSemantic::Int, &[5]);

        // `B`'s candidate is the generic renamed from `A`, which takes `B`'s bound like any other
        // generic being inferred (#361)
        let mut infer = Inference::new([renamed, b]);
        infer.rename(renamed, a);
        db.infer(renamed, b, &mut infer);
        add_expected(&mut db, &[renamed, b], b, five, &mut infer);
        assert_eq!(candidates(&infer, renamed).upper, vec![five]);

        // And it's resolved through
        let int = db.builtins.int;
        assert_eq!(db.infer(int, renamed, &mut infer), Comparison::Assign);
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&b], int);
    }

    #[test]
    fn renamed_generics_take_the_original_from_above() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let (a, renamed) = (db.generic("A"), db.generic("A"));
        let a_or_nil = db.union(&[a, nil]);

        // The type expected of `A'` is `A`, from the identity, and `A | nil`, so it's `A`, the
        // most specific (#378)
        let mut infer = Inference::new([renamed]);
        infer.rename(renamed, a);
        let renamed_or_nil = db.union(&[renamed, nil]);
        let ret = db.pair(renamed, renamed_or_nil);
        let expected = db.pair(a, a_or_nil);
        infer_from_expected(&mut db.arena, &db.builtins, ret, expected, &mut infer);
        assert!(candidates(&infer, renamed).upper.contains(&a));
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&renamed], a);

        // Next to `Int`, which it isn't assignable to, it's left out, so `A'` is `Int` as before
        let mut infer = Inference::new([renamed]);
        infer.rename(renamed, a);
        let ret = db.pair(renamed, renamed);
        let expected = db.pair(a, int);
        infer_from_expected(&mut db.arena, &db.builtins, ret, expected, &mut infer);
        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&renamed], int);
    }

    #[test]
    fn unconstrained_cycles_are_left_as_is() {
        let mut db = Db::new();
        let (a, b) = (db.generic("A"), db.generic("B"));
        let mut infer = Inference::new([a, b]);
        assert_eq!(db.infer(b, a, &mut infer), Comparison::Assign);
        assert_eq!(db.infer(a, b, &mut infer), Comparison::Assign);

        let mappings = resolve_inference(&mut db.arena, &db.builtins, &infer);
        assert_eq!(mappings[&a], b);
        assert_eq!(mappings[&b], a);
    }
}

mod substitution {
    use super::*;
    use crate::{function_type_generics, referenced_generics};

    #[test]
    fn apply() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let t = db.generic("T");
        let pair = db.pair(t, t);
        let twice = db.alias("Twice", &[t], pair);
        let applied = db.apply(twice, &[(t, int)]);

        let result = substitute(&mut db.arena, applied);
        let Type::Alias(alias) = db.arena[result].clone() else {
            panic!("expected an alias");
        };
        assert_eq!(alias.generics, vec![int]);
        assert_eq!(db.name(result), "Twice<Int>");
        assert_eq!(stringify(&mut db.arena, alias.inner), "(Int, Int)");
    }

    #[test]
    fn nested_generics() {
        let mut db = Db::new();
        let int = db.builtins.int;

        // type Twice<T> = (T, T); type Nested<U> = Twice<List<U>>;
        let t = db.generic("T");
        let pair = db.pair(t, t);
        let twice = db.alias("Twice", &[t], pair);
        let u = db.generic("U");
        let list_u = db.list(u);
        let twice_list = db.apply(twice, &[(t, list_u)]);
        let nested = db.alias("Nested", &[u], twice_list);

        let applied = db.apply(nested, &[(u, int)]);
        assert_eq!(db.name(applied), "Nested<Int>");

        let list_int = db.list(int);
        let expected = db.pair(list_int, list_int);
        assert_eq!(db.cmp(applied, expected), Comparison::Assign);
        assert_eq!(db.cmp(expected, applied), Comparison::Assign);
    }

    #[test]
    fn function_generics_are_shadowed() {
        let mut db = Db::new();
        let (int, bytes32) = (db.builtins.int, db.builtins.bytes32);
        let t = db.generic("T");
        let u = db.generic("U");
        let function = db.function_with(&[u], &[t], true, u);

        // `fn<U>(p0: T) -> U` with `T := Int` and `U := Bytes32`
        let mappings = HashMap::from_iter([(t, int), (u, bytes32)]);
        let result = substitute_with_mappings(&mut db.arena, function, &mappings);
        let Type::Function(function) = db.arena[result].clone() else {
            panic!("expected a function");
        };

        // The function's own generic is bound by it, so it isn't substituted
        assert_eq!(function.generics, vec![u]);
        assert_eq!(db.cmp(function.params[0].ty, int), Comparison::Assign);
        assert_eq!(function.ret, u);
    }

    #[test]
    fn function_generics_are_not_captured() {
        let mut db = Db::new();
        let t = db.generic("T");
        let u = db.generic("U");
        let function = db.function_with(&[u], &[u], true, t);

        // `fn<U>(p0: U) -> T` with `T := U` refers to the outer `U` in its return type
        let mappings = HashMap::from_iter([(t, u)]);
        let result = substitute_with_mappings(&mut db.arena, function, &mappings);
        let Type::Function(function) = db.arena[result].clone() else {
            panic!("expected a function");
        };

        assert_ne!(function.generics, vec![u]);
        assert_eq!(function.params[0].ty, function.generics[0]);
        assert_eq!(function.ret, u);

        // So it isn't the type of the identity function
        let v = db.generic("V");
        let identity = db.function_with(&[v], &[v], true, v);
        assert_eq!(db.cmp(identity, result), Comparison::Invalid);
    }

    #[test]
    fn recursive_function_generics_are_not_captured() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let any = db.alloc(Type::Any);

        // type P<T> = fn<U>(x: U) -> (T, P<U>);
        let t = db.generic("T");
        let u = db.generic("U");
        let alias = db.declare_alias("P", &[t]);
        let p_u = db.apply(alias, &[(t, u)]);
        let ret = db.pair(t, p_u);
        let function = db.function_with(&[u], &[u], true, ret);
        db.define(alias, function);
        let p_int = db.apply(alias, &[(t, int)]);

        // Calling the result of `P<Int>` returns a `P` of the argument's type
        let v = db.generic("V");
        let w = db.generic("W");
        let inner = db.pair(v, any);
        let inner = db.function_with(&[w], &[w], true, inner);
        let outer = db.pair(int, inner);
        let expected = db.function_with(&[v], &[v], true, outer);
        assert_eq!(db.cmp(p_int, expected), Comparison::Assign);

        // Not a `P` of the next argument's type
        let v = db.generic("V");
        let w = db.generic("W");
        let inner = db.pair(w, any);
        let inner = db.function_with(&[w], &[w], true, inner);
        let outer = db.pair(int, inner);
        let captured = db.function_with(&[v], &[v], true, outer);
        assert_eq!(db.cmp(p_int, captured), Comparison::Invalid);
    }

    #[test]
    fn referenced_generics_skip_bound_ones() {
        let mut db = Db::new();
        let t = db.generic("T");
        let u = db.generic("U");

        // `(T, fn<U>(p0: U) -> U)`
        let function = db.function_with(&[u], &[u], true, u);
        let pair = db.pair(t, function);

        assert_eq!(referenced_generics(&mut db.arena, pair), vec![t]);
    }

    #[test]
    fn function_type_generics_anywhere() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let (u, v, w) = (db.generic("U"), db.generic("V"), db.generic("W"));

        // `(fn<U>(p0: U) -> U, fn<W>(p0: fn<V>(p0: V) -> V) -> Int)`
        let first = db.function_with(&[u], &[u], true, u);
        let parameter = db.function_with(&[v], &[v], true, v);
        let second = db.function_with(&[w], &[parameter], true, int);
        let pair = db.pair(first, second);

        let mut generics = function_type_generics(&mut db.arena, pair);
        generics.sort_unstable();
        let mut expected = vec![u, v, w];
        expected.sort_unstable();
        assert_eq!(generics, expected);
    }

    #[test]
    fn substituted_function_generics_are_kept() {
        let mut db = Db::new();
        let int = db.builtins.int;

        // type P<T> = fn<U>(x: U) -> (T, P<U>);
        let t = db.generic("T");
        let u = db.generic("U");
        let alias = db.declare_alias("P", &[t]);
        let p_u = db.apply(alias, &[(t, u)]);
        let ret = db.pair(t, p_u);
        let function = db.function_with(&[u], &[u], true, ret);
        db.define(alias, function);
        let p_int = db.apply(alias, &[(t, int)]);

        // Each substitution of `P<Int>` renames the generic that the `P<U>` in its expansion binds,
        // but a substituted type keeps it, so every walk of that type sees the same one
        let substituted = substitute(&mut db.arena, p_int);
        let inner = inner_generics(&mut db, substituted);
        assert_eq!(inner, inner_generics(&mut db, substituted));
        assert!(function_type_generics(&mut db.arena, substituted).contains(&inner[0]));
        assert_ne!(
            inner_generics(&mut db, p_int),
            inner_generics(&mut db, p_int)
        );
    }

    /// The generics of the function type in the rest of the pair that a function type returns.
    fn inner_generics(db: &mut Db, ty: TypeId) -> Vec<TypeId> {
        let outer = extract_functions(&mut db.arena, ty, false);
        let ret = unwrap_semantic(&mut db.arena, outer[0].ret, true);
        let Type::Pair(pair) = db.arena[ret].clone() else {
            panic!("expected a pair");
        };
        extract_functions(&mut db.arena, pair.rest, false)[0]
            .generics
            .clone()
    }

    #[test]
    fn recursive_rigid_generics_get_the_summary() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let any = db.alloc(Type::Any);

        // type Q<T> = fn<V>(x: V) -> (T | Int, Q<V>);
        let t = db.generic("T");
        let v = db.generic("V");
        let q = db.declare_alias("Q", &[t]);
        let first = db.union(&[t, int]);
        let q_v = db.apply(q, &[(t, v)]);
        let ret = db.pair(first, q_v);
        let function = db.function_with(&[v], &[v], true, ret);
        db.define(q, function);
        let q_int = db.apply(q, &[(t, int)]);

        // type A0 = fn<U>(x: U) -> (Int, A1<U>);
        // type A1<U> = fn(x: Any) -> (first, A2<U>);
        // type A2<U> = fn(x: param) -> (Int, A1<U>);
        let mut a0 = |first: fn(TypeId) -> Option<TypeId>, param: fn(TypeId) -> Option<TypeId>| {
            let u = db.generic("U");
            let a1 = db.declare_alias("A1", &[u]);
            let a2 = db.declare_alias("A2", &[u]);
            let a1_u = db.apply(a1, &[(u, u)]);
            let a2_u = db.apply(a2, &[(u, u)]);
            let ret = db.pair(first(u).unwrap_or(int), a2_u);
            let function = db.function(&[any], ret);
            db.define(a1, function);
            let ret = db.pair(int, a1_u);
            let function = db.function(&[param(u).unwrap_or(any)], ret);
            db.define(a2, function);
            let ret = db.pair(int, a1_u);
            let a0 = db.function_with(&[u], &[u], true, ret);
            db.cmp(a0, q_int)
        };

        // `U` would have to hold each `V` that `Q` binds, but be assignable to the first one
        assert_eq!(a0(Some, Some), Comparison::Invalid);

        // Even if only the first `V` flows into it, since it's returned again for the next one
        assert_eq!(a0(Some, |_| None), Comparison::Invalid);

        // But if it's never returned, it can hold all of them
        assert_eq!(a0(|_| None, Some), Comparison::Assign);
    }

    #[test]
    fn recursive_shared_instances_get_the_summary() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let any = db.alloc(Type::Any);

        // type Q = fn<V>(x: V) -> (V | Int, Q);
        let v = db.generic("V");
        let q_alias = db.declare_alias("Q", &[]);
        let first = db.union(&[v, int]);
        let ret = db.pair(first, q_alias);
        let function = db.function_with(&[v], &[v], true, ret);
        db.define(q_alias, function);

        // type P<T> = fn<V>(x: V) -> (T | Int, P<V>);
        let t = db.generic("T");
        let v = db.generic("V");
        let p_alias = db.declare_alias("P", &[t]);
        let first = db.union(&[t, int]);
        let p_v = db.apply(p_alias, &[(t, v)]);
        let ret = db.pair(first, p_v);
        let function = db.function_with(&[v], &[v], true, ret);
        db.define(p_alias, function);
        let p_int = db.apply(p_alias, &[(t, int)]);

        // type A1<U> = fn(x: param) -> (U, A1<U>);
        // fn<U>(x: U) -> A1<U> compared to fn(x: Int) -> rhs
        let mut cmp = |rhs: TypeId, param: Option<TypeId>| {
            let u = db.generic("U");
            let a1 = db.declare_alias("A1", &[u]);
            let a1_u = db.apply(a1, &[(u, u)]);
            let ret = db.pair(u, a1_u);
            let function = db.function(&[param.unwrap_or(u)], ret);
            db.define(a1, function);
            let lhs = db.function_with(&[u], &[u], true, a1_u);
            let rhs = db.function(&[int], rhs);
            db.cmp(lhs, rhs)
        };

        // The `V` of one call would be returned as the `V` of the next
        assert_eq!(cmp(q_alias, None), Comparison::Invalid);

        // But if only an `Int` flows into `U`, it's assignable to each `V | Int`
        assert_eq!(cmp(p_int, Some(any)), Comparison::Assign);
    }

    #[test]
    fn recursion() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let t = db.generic("T");
        let list_t = db.list(t);

        let mappings = HashMap::from_iter([(t, int)]);
        let list_int = substitute_with_mappings(&mut db.arena, list_t, &mappings);
        assert_eq!(db.name(list_int), "List<Int>");

        let expected = db.list(int);
        assert_eq!(db.cmp(list_int, expected), Comparison::Assign);
        assert_eq!(db.cmp(expected, list_int), Comparison::Assign);
    }
}

mod subtraction {
    use super::*;

    fn sub(db: &mut Db, lhs: TypeId, rhs: TypeId) -> String {
        let result = subtract(&mut db.arena, &db.builtins, lhs, rhs);
        db.name(result)
    }

    fn literals(db: &mut Db) -> [TypeId; 3] {
        [1, 2, 3].map(|value| db.literal(AtomSemantic::Int, &[value]))
    }

    #[test]
    fn splits_first() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let [one, two, _] = literals(&mut db);
        let either = db.union(&[one, two]);
        let lhs = db.pair(either, int);
        let rhs = db.pair(one, int);

        assert_eq!(sub(&mut db, lhs, rhs), "(2, Int)");
    }

    #[test]
    fn splits_rest() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let [one, two, _] = literals(&mut db);
        let either = db.union(&[one, two]);
        let lhs = db.pair(int, either);
        let rhs = db.pair(int, one);

        assert_eq!(sub(&mut db, lhs, rhs), "(Int, 2)");
    }

    #[test]
    fn splits_nested_pairs() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let [one, two, _] = literals(&mut db);
        let either = db.union(&[one, two]);
        let inner = db.pair(either, nil);
        let lhs = db.pair(int, inner);
        let inner = db.pair(one, nil);
        let rhs = db.pair(int, inner);

        assert_eq!(sub(&mut db, lhs, rhs), "(Int, (2, nil))");
    }

    #[test]
    fn removes_pair_once_every_part_is_covered() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let [one, two, three] = literals(&mut db);
        let all = db.union(&[one, two, three]);
        let lhs = db.pair(all, int);
        let first = db.pair(one, int);
        let second = db.pair(two, int);
        let both = db.union(&[first, second]);
        let third = db.pair(three, int);
        let every = db.union(&[first, second, third]);

        assert_eq!(sub(&mut db, lhs, both), "(3, Int)");
        assert_eq!(sub(&mut db, lhs, every), "Never");
    }

    #[test]
    fn skips_target_pair_that_differs_in_both_parts() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let [one, two, three] = literals(&mut db);
        let either = db.union(&[one, two]);
        let lhs = db.pair(either, int);
        let other = db.pair(three, nil);
        let first = db.pair(one, int);
        let rhs = db.union(&[other, first]);

        assert_eq!(sub(&mut db, lhs, rhs), "(2, Int)");
    }

    #[test]
    fn removes_pair_when_a_part_is_emptied() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let [one, two, _] = literals(&mut db);
        let either = db.union(&[one, two]);
        let inner = db.pair(either, int);
        let lhs = db.pair(int, inner);
        let first = db.pair(one, int);
        let second = db.pair(two, int);
        let both = db.union(&[first, second]);
        let rhs = db.pair(int, both);

        assert_eq!(sub(&mut db, lhs, rhs), "Never");
    }

    #[test]
    fn splits_both_parts() {
        let mut db = Db::new();
        let [one, two, _] = literals(&mut db);
        let either = db.union(&[one, two]);
        let lhs = db.pair(either, either);
        let rhs = db.pair(one, one);

        assert_eq!(sub(&mut db, lhs, rhs), "(2, 1 | 2) | (1, 2)");
    }

    #[test]
    fn splits_every_combination() {
        let mut db = Db::new();
        let [one, two, _] = literals(&mut db);
        let either = db.union(&[one, two]);
        let lhs = db.pair(either, either);
        let pairs = [(one, one), (one, two), (two, one)].map(|(a, b)| db.pair(a, b));
        let rhs = db.union(&pairs);
        let all = db.pair(two, two);
        let every = db.union(&[rhs, all]);

        assert_eq!(sub(&mut db, lhs, rhs), "(2, 2)");
        assert_eq!(sub(&mut db, lhs, every), "Never");
    }

    #[test]
    fn keeps_pair_when_a_part_is_disjoint() {
        let mut db = Db::new();
        let [one, two, three] = literals(&mut db);
        let either = db.union(&[one, two]);
        let lhs = db.pair(either, either);
        let rhs = db.pair(three, one);
        let rest_disjoint = db.pair(one, three);

        assert_eq!(sub(&mut db, lhs, rhs), "(1 | 2, 1 | 2)");
        assert_eq!(sub(&mut db, lhs, rest_disjoint), "(1 | 2, 1 | 2)");
    }

    #[test]
    fn limits_split_pairs() {
        let mut db = Db::new();
        let values = (1..=24)
            .map(|value| db.literal(AtomSemantic::Int, &[value]))
            .collect::<Vec<_>>();
        let any_value = db.union(&values);
        let lhs = db.pair(any_value, any_value);
        let diagonal = values
            .iter()
            .map(|&value| db.pair(value, value))
            .collect::<Vec<_>>();
        let rhs = db.union(&diagonal);

        let result = subtract(&mut db.arena, &db.builtins, lhs, rhs);
        let Type::Union(union) = db.arena[result].clone() else {
            panic!("expected a union");
        };
        assert_eq!(union.types.len(), MAX_SPLIT_PAIRS);

        // Every value that fails the check is still in the result
        for &a in &values {
            for &b in &values {
                if a != b {
                    let pair = db.pair(a, b);
                    assert_eq!(db.cmp(pair, result), Comparison::Assign);
                }
            }
        }
    }

    #[test]
    fn splits_list() {
        let mut db = Db::new();
        let [one, two, _] = literals(&mut db);
        let either = db.union(&[one, two]);
        let list = db.list(either);
        let rhs = db.pair(one, list);
        let nil = db.builtins.nil;
        let pair = db.pair(two, list);
        let expected = db.union(&[nil, pair]);

        // The alias keeps its name, so the narrowing is only visible by comparing the result
        let result = subtract(&mut db.arena, &db.builtins, list, rhs);
        assert_eq!(db.cmp(result, expected), Comparison::Assign);
        assert_eq!(db.cmp(list, result), Comparison::Invalid);
    }

    #[test]
    fn narrows_struct() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let [one, two, _] = literals(&mut db);
        let either = db.union(&[one, two]);
        let inner = db.pair(either, int);
        let lhs = db.strukt("S", inner);
        let narrow = db.pair(two, int);
        let rhs = db.pair(one, int);

        let result = subtract(&mut db.arena, &db.builtins, lhs, rhs);
        let Type::Struct(ty) = db.arena[result].clone() else {
            panic!("expected a struct");
        };
        assert!(ty.narrowed);
        assert_eq!(db.cmp(ty.inner, narrow), Comparison::Assign);
        assert_eq!(db.cmp(result, lhs), Comparison::Assign);
        assert_eq!(db.cmp(lhs, result), Comparison::Invalid);
    }

    #[test]
    fn merges_struct_past_copy_limit() {
        const FIELDS: usize = 8;

        let mut db = Db::new();
        let nil = db.builtins.nil;
        let [one, two, _] = literals(&mut db);
        let either = db.union(&[one, two]);

        let nested = |db: &mut Db, parts: &[TypeId]| {
            parts
                .iter()
                .rev()
                .fold(nil, |rest, &first| db.pair(first, rest))
        };

        let inner = nested(&mut db, &[either; FIELDS]);
        let lhs = db.strukt("S", inner);
        let Type::Struct(mut ty) = db.arena[lhs].clone() else {
            unreachable!();
        };
        ty.fields = (0..FIELDS).map(|i| format!("f{i}")).collect();
        db.set(lhs, Type::Struct(ty));

        // Every combination with an even number of `2`s, which leaves more combinations than a
        // struct is split into
        let combinations = (0..1u32 << FIELDS).map(|bits| {
            (0..FIELDS)
                .map(|i| if bits >> i & 1 == 1 { two } else { one })
                .collect::<Vec<_>>()
        });
        let even = combinations
            .filter(|parts| parts.iter().filter(|&&part| part == two).count() % 2 == 0)
            .collect::<Vec<_>>();
        let even = even
            .iter()
            .map(|parts| nested(&mut db, parts))
            .collect::<Vec<_>>();
        let rhs = db.union(&even);

        // The pairs are merged into one struct with a single pair at each level, so its fields can
        // still be read, and it contains every combination that's left
        let result = subtract(&mut db.arena, &db.builtins, lhs, rhs);
        let Type::Struct(ty) = db.arena[result].clone() else {
            panic!("expected a struct, found {}", db.name(result));
        };
        let mut level = ty.inner;
        for _ in 0..FIELDS {
            let Type::Pair(pair) = db.arena[level] else {
                panic!("expected a pair, found {}", db.name(level));
            };
            level = pair.rest;
        }
        let mut odd = vec![one; FIELDS];
        odd[0] = two;
        let odd = nested(&mut db, &odd);
        assert_eq!(db.cmp(odd, ty.inner), Comparison::Assign);
    }

    #[test]
    fn splits_any() {
        let mut db = Db::new();
        let BuiltinTypes {
            int,
            nil,
            atom,
            permissive_any: any,
            ..
        } = db.builtins;
        let rhs = db.pair(int, any);
        let inner = db.pair(any, any);
        let pair = db.pair(inner, any);
        let expected = db.union(&[atom, pair]);
        let int_pair = db.pair(int, nil);

        // The first value of a remaining pair can't be an atom, since every atom is an `Int`
        let result = subtract(&mut db.arena, &db.builtins, any, rhs);
        assert_eq!(db.cmp(result, expected), Comparison::Assign);
        assert_eq!(db.cmp(atom, result), Comparison::Assign);
        assert_eq!(db.cmp(int_pair, result), Comparison::Invalid);
    }

    #[test]
    fn recursive_parts_terminate() {
        let mut db = Db::new();
        let [one, two, _] = literals(&mut db);

        // type L = 2 | (1, L)
        let l = db.declare_alias("L", &[]);
        let pair = db.pair(one, l);
        let inner = db.union(&[two, pair]);
        db.define(l, inner);

        // type R = 1 | (1, R)
        let r = db.declare_alias("R", &[]);
        let pair = db.pair(one, r);
        let inner = db.union(&[one, pair]);
        db.define(r, inner);

        // The rest of `(1, L)` leads back to `L - R`, so the pair is kept whole
        let result = subtract(&mut db.arena, &db.builtins, l, r);
        assert_eq!(db.cmp(l, result), Comparison::Assign);
    }
}
