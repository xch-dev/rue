//! Tests for runtime type checks (`check`) and type subtraction (`subtract`).
//!
//! The soundness invariant for a type guard `value is To` where `value: From` is:
//!
//! - For every value in `From`, the generated check passes if and only if the value is in `To`.
//! - Every value in `From` that isn't in `To` is in `subtract(From, To)` (the else branch type).

use std::{borrow::Cow, collections::HashMap};

use clvmr::{Allocator, NodePtr, SExp};
use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};
use rue_types::{
    Alias, Atom, AtomRestriction, AtomSemantic, BuiltinTypes, Check, CheckError, FunctionType,
    Generic, Pair, Struct, Type, TypeId, Union, check, stringify, subtract,
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

    fn atom(&mut self, semantic: AtomSemantic, restriction: Option<AtomRestriction>) -> TypeId {
        self.alloc(Type::Atom(Atom::new(semantic, restriction)))
    }

    fn int(&mut self, value: u8) -> TypeId {
        let bytes = if value == 0 { vec![] } else { vec![value] };
        self.atom(
            AtomSemantic::Int,
            Some(AtomRestriction::Value(Cow::Owned(bytes))),
        )
    }

    fn bytes_value(&mut self, value: &[u8]) -> TypeId {
        self.atom(
            AtomSemantic::Bytes,
            Some(AtomRestriction::Value(Cow::Owned(value.to_vec()))),
        )
    }

    fn bytes_length(&mut self, length: usize) -> TypeId {
        self.atom(AtomSemantic::Bytes, Some(AtomRestriction::Length(length)))
    }

    fn pair(&mut self, first: TypeId, rest: TypeId) -> TypeId {
        self.alloc(Type::Pair(Pair::new(first, rest)))
    }

    fn union(&mut self, types: Vec<TypeId>) -> TypeId {
        self.alloc(Type::Union(Union::new(types)))
    }

    fn alias(&mut self, inner: TypeId) -> TypeId {
        self.alloc(Type::Alias(Alias {
            name: None,
            inner,
            generics: vec![],
        }))
    }

    fn structure(&mut self, inner: TypeId) -> TypeId {
        let id = self.alloc(Type::Unresolved);
        self.arena[id] = Type::Struct(Struct {
            name: None,
            semantic: id,
            inner,
            generics: vec![],
            fields: IndexSet::new(),
            nil_terminated: true,
        });
        id
    }

    fn generic(&mut self) -> TypeId {
        self.alloc(Type::Generic(Generic { name: None }))
    }

    fn function(&mut self) -> TypeId {
        let ret = self.builtins.nil;
        self.alloc(Type::Function(FunctionType {
            generics: vec![],
            params: IndexMap::new(),
            nil_terminated: true,
            ret,
        }))
    }

    /// A recursive `List<T>` style type, without generics.
    fn list(&mut self, item: TypeId) -> TypeId {
        let list = self.alloc(Type::Unresolved);
        let pair = self.pair(item, list);
        let nil = self.builtins.nil;
        let inner = self.union(vec![nil, pair]);
        self.arena[list] = Type::Alias(Alias {
            name: None,
            inner,
            generics: vec![],
        });
        list
    }

    fn check(&mut self, from: TypeId, to: TypeId) -> Result<Check, CheckError> {
        check(&mut self.arena, &self.builtins, from, to)
    }

    fn subtract(&mut self, from: TypeId, to: TypeId) -> TypeId {
        subtract(&mut self.arena, &self.builtins, from, to)
    }

    fn name(&mut self, id: TypeId) -> String {
        stringify(&mut self.arena, id)
    }
}

fn value(bytes: &[u8]) -> Check {
    Check::Atom(AtomRestriction::Value(Cow::Owned(bytes.to_vec())))
}

fn length(length: usize) -> Check {
    Check::Atom(AtomRestriction::Length(length))
}

fn pair(first: Check, rest: Check) -> Check {
    Check::Pair(Box::new(first), Box::new(rest))
}

const IS_PAIR: Check = Check::IsPair {
    can_be_truthy: true,
};

const IS_ATOM: Check = Check::IsAtom {
    can_be_truthy: true,
};

#[test]
fn test_atom_checks() {
    let mut t = Types::new();
    let atom = t.builtins.atom;
    let any = t.builtins.permissive_any;
    let int = t.builtins.int;
    let bytes = t.builtins.bytes;
    let bytes32 = t.builtins.bytes32;
    let nil = t.builtins.nil;
    let v42 = t.int(42);
    let v34 = t.int(34);
    let v42_or_34 = t.union(vec![v42, v34]);

    // Value restrictions
    assert_eq!(t.check(atom, v42), Ok(value(&[42])));
    assert_eq!(
        t.check(atom, v42_or_34),
        Ok(Check::Or(vec![value(&[42]), value(&[34])]))
    );
    assert_eq!(t.check(v42_or_34, v42), Ok(value(&[42])));

    // Length restrictions
    assert_eq!(t.check(atom, bytes32), Ok(length(32)));
    assert_eq!(t.check(bytes, bytes32), Ok(length(32)));

    // Shape checks
    assert_eq!(t.check(any, atom), Ok(IS_ATOM));
    assert_eq!(
        t.check(any, v42),
        Ok(Check::And(vec![IS_ATOM, value(&[42])]))
    );

    // Unrestricted atoms, which can be cast between each other
    assert_eq!(t.check(int, bytes), Ok(Check::None));
    assert_eq!(t.check(bytes32, bytes), Ok(Check::None));
    assert_eq!(t.check(v42, int), Ok(Check::None));

    // Incompatible restrictions
    assert_eq!(t.check(bytes32, nil), Ok(Check::Impossible));
    assert_eq!(t.check(v42, v34), Ok(Check::Impossible));
    let int_pair = t.pair(int, int);
    assert_eq!(t.check(int, int_pair), Ok(Check::Impossible));
}

#[test]
fn test_atom_length_value_overlap() {
    let mut t = Types::new();
    let bytes32 = t.builtins.bytes32;
    let one = t.int(1);
    let value32 = t.bytes_value(&[7; 32]);
    let from = t.union(vec![bytes32, one]);
    let to = t.union(vec![value32, one]);

    // A value of the right length is in the target, so it must be checked for
    assert_eq!(
        t.check(from, to),
        Ok(Check::Or(vec![value(&[7; 32]), value(&[1])]))
    );
    assert_eq!(t.check(bytes32, value32), Ok(value(&[7; 32])));

    let else_type = t.subtract(from, to);
    assert_eq!(t.name(else_type), "Bytes32");
}

#[test]
fn test_nil_truthiness() {
    let mut t = Types::new();
    let nil = t.builtins.nil;
    let int = t.builtins.int;
    let zero_length = t.bytes_length(0);
    let pair = t.pair(int, nil);
    let from = t.union(vec![nil, pair]);
    let from_zero_length = t.union(vec![zero_length, pair]);
    let from_int = t.union(vec![int, pair]);

    // The only atom is nil, so truthiness can be used instead of `l`
    let not_truthy = Check::IsAtom {
        can_be_truthy: false,
    };
    assert_eq!(t.check(from, nil), Ok(not_truthy.clone()));
    assert_eq!(
        t.check(from_zero_length, nil),
        Ok(Check::And(vec![not_truthy, value(&[])]))
    );
    assert_eq!(
        t.check(from, pair),
        Ok(Check::IsPair {
            can_be_truthy: false
        })
    );

    // Here the atom can be truthy, so it can't
    assert_eq!(t.check(from_int, pair), Ok(IS_PAIR));
}

#[test]
fn test_pair_checks() {
    let mut t = Types::new();
    let any = t.builtins.permissive_any;
    let atom = t.builtins.atom;
    let nil = t.builtins.nil;
    let bytes32 = t.builtins.bytes32;
    let v42 = t.int(42);
    let v34 = t.int(34);

    let any_pair = t.pair(any, any);
    assert_eq!(t.check(any, any_pair), Ok(IS_PAIR));

    let atom_pair = t.pair(atom, atom);
    let value_pair = t.pair(v42, v34);
    assert_eq!(
        t.check(atom_pair, value_pair),
        Ok(pair(value(&[42]), value(&[34])))
    );

    // The type assumption shortcut: `nil` is enough to discriminate the variants
    let a = t.pair(bytes32, nil);
    let b = t.pair(nil, bytes32);
    let from = t.union(vec![a, b]);
    assert_eq!(t.check(from, b), Ok(pair(value(&[]), Check::None)));
    let else_type = t.subtract(from, b);
    assert_eq!(t.name(else_type), "(Bytes32, nil)");

    // Nested pairs
    let inner = t.pair(atom, nil);
    let outer = t.pair(atom, inner);
    let inner_target = t.pair(v42, nil);
    let outer_target = t.pair(v34, inner_target);
    assert_eq!(
        t.check(outer, outer_target),
        Ok(pair(value(&[34]), pair(value(&[42]), Check::None)))
    );
}

#[test]
fn test_pair_rest_includes_narrower_firsts() {
    // `x: (Bytes32, Bytes32) | (Bytes, nil)`, `x is (Bytes, nil)`
    let mut t = Types::new();
    let bytes = t.builtins.bytes;
    let bytes32 = t.builtins.bytes32;
    let nil = t.builtins.nil;
    let a = t.pair(bytes32, bytes32);
    let b = t.pair(bytes, nil);
    let from = t.union(vec![a, b]);

    assert_eq!(t.check(from, b), Ok(pair(Check::None, value(&[]))));
    let else_type = t.subtract(from, b);
    assert_eq!(t.name(else_type), "(Bytes32, Bytes32)");
}

#[test]
fn test_pair_rest_includes_overlapping_firsts() {
    // `x: (1 | 3, Bytes32) | (1 | 2, nil)`, `x is (1 | 2, nil)`
    let mut t = Types::new();
    let bytes32 = t.builtins.bytes32;
    let nil = t.builtins.nil;
    let one = t.int(1);
    let two = t.int(2);
    let three = t.int(3);
    let one_or_three = t.union(vec![one, three]);
    let one_or_two = t.union(vec![one, two]);
    let a = t.pair(one_or_three, bytes32);
    let b = t.pair(one_or_two, nil);
    let from = t.union(vec![a, b]);

    assert_eq!(
        t.check(from, b),
        Ok(pair(Check::Or(vec![value(&[1]), value(&[2])]), value(&[])))
    );
}

#[test]
fn test_pair_rest_condition() {
    // `c: (51, (Bytes32, (Int, nil))) | (Int, nil)`, `c is (Int, nil)`
    let mut t = Types::new();
    let int = t.builtins.int;
    let nil = t.builtins.nil;
    let bytes32 = t.builtins.bytes32;
    let fifty_one = t.int(51);
    let amount = t.pair(int, nil);
    let puzzle_hash = t.pair(bytes32, amount);
    let condition = t.pair(fifty_one, puzzle_hash);
    let from = t.union(vec![condition, amount]);

    assert_eq!(
        t.check(from, amount),
        Ok(pair(
            Check::None,
            Check::IsAtom {
                can_be_truthy: false
            }
        ))
    );
}

#[test]
fn test_target_union_member_without_check() {
    // `x: nil | (Int, nil) | (Int, 5)`, `x is (Int, 5) | (Int, nil | 5)`
    let mut t = Types::new();
    let nil = t.builtins.nil;
    let int = t.builtins.int;
    let five = t.int(5);
    let nil_or_five = t.union(vec![nil, five]);
    let a = t.pair(int, nil);
    let b = t.pair(int, five);
    let c = t.pair(int, nil_or_five);
    let from = t.union(vec![nil, a, b]);
    let to = t.union(vec![b, c]);

    assert_eq!(
        t.check(from, to),
        Ok(Check::IsPair {
            can_be_truthy: false
        })
    );
    let else_type = t.subtract(from, to);
    assert_eq!(t.name(else_type), "nil");

    // `x: nil | (1, nil) | (2, nil)`, `x is (1, nil) | (Int, nil)`
    let one = t.int(1);
    let two = t.int(2);
    let a = t.pair(one, nil);
    let b = t.pair(two, nil);
    let c = t.pair(int, nil);
    let from = t.union(vec![nil, a, b]);
    let to = t.union(vec![a, c]);

    assert_eq!(
        t.check(from, to),
        Ok(Check::IsPair {
            can_be_truthy: false
        })
    );
    let else_type = t.subtract(from, to);
    assert_eq!(t.name(else_type), "nil");

    // Target members that can't match shouldn't prevent another member from accepting everything
    let v42 = t.int(42);
    let impossible = t.pair(v42, v42);
    let from = t.union(vec![nil, c]);
    let to = t.union(vec![impossible, c]);
    assert_eq!(
        t.check(from, to),
        Ok(Check::IsPair {
            can_be_truthy: false
        })
    );
}

#[test]
fn test_structs_and_aliases() {
    let mut t = Types::new();
    let any = t.builtins.permissive_any;
    let int = t.builtins.int;
    let nil = t.builtins.nil;
    let bytes32 = t.builtins.bytes32;

    let inner = t.pair(int, nil);
    let structure = t.structure(inner);
    assert_eq!(
        t.check(any, structure),
        Ok(Check::And(vec![
            IS_PAIR,
            pair(IS_ATOM, Check::And(vec![IS_ATOM, value(&[])]))
        ]))
    );

    // Aliases are transparent
    let alias = t.alias(bytes32);
    let from = t.union(vec![alias, nil]);
    assert_eq!(t.check(from, alias), Ok(length(32)));
    assert_eq!(t.check(from, nil), Ok(value(&[])));

    // Subtracting from an alias keeps the alias around the remaining variants
    let alias = t.alias(from);
    let else_type = t.subtract(alias, nil);
    let Type::Alias(remaining) = t.arena[else_type].clone() else {
        panic!("expected alias, found {}", t.name(else_type));
    };
    assert_eq!(t.name(remaining.inner), "Bytes32");

    // Struct variants are repackaged separately from other variants
    let structure = t.structure(bytes32);
    let other = t.pair(int, int);
    let from = t.union(vec![structure, other, nil]);
    let else_type = t.subtract(from, nil);
    let Type::Union(remaining) = t.arena[else_type].clone() else {
        panic!("expected union, found {}", t.name(else_type));
    };
    assert_eq!(remaining.types.len(), 2);
    assert!(matches!(t.arena[remaining.types[0]], Type::Struct(_)));
    assert_eq!(t.name(remaining.types[1]), "(Int, Int)");
}

#[test]
fn test_any_and_generics() {
    let mut t = Types::new();
    let any = t.builtins.permissive_any;
    let int = t.builtins.int;
    let nil = t.builtins.nil;
    let generic = t.generic();

    // Checking against `Any` is always unnecessary
    assert_eq!(t.check(int, any), Ok(Check::None));

    // A generic value can be checked against a concrete type
    let generic_or_nil = t.union(vec![generic, nil]);
    assert_eq!(
        t.check(generic_or_nil, nil),
        Ok(Check::And(vec![IS_ATOM, value(&[])]))
    );

    // Checking a generic against itself is unnecessary
    assert_eq!(t.check(generic, generic), Ok(Check::None));
    let generic_pair = t.pair(generic, nil);
    let from = t.union(vec![generic_pair, nil]);
    assert_eq!(
        t.check(from, generic_pair),
        Ok(Check::IsPair {
            can_be_truthy: false
        })
    );

    // But a generic type can't be checked at runtime
    let int_or_nil = t.union(vec![int, nil]);
    assert_eq!(t.check(int_or_nil, generic), Err(CheckError::GenericType));
    assert_eq!(
        t.check(generic_or_nil, generic),
        Err(CheckError::GenericType)
    );

    // Even when it's nested
    let bytes = t.builtins.bytes;
    let five = t.int(5);
    let six = t.int(6);
    let a = t.pair(bytes, five);
    let b = t.pair(int, six);
    let from = t.union(vec![generic_pair, a]);
    let to = t.union(vec![generic_pair, b]);
    assert_eq!(t.check(from, to), Err(CheckError::GenericType));
}

#[test]
fn test_subtract_generics() {
    let mut t = Types::new();
    let any = t.builtins.recursive_any;
    let atom = t.builtins.atom;
    let int = t.builtins.int;
    let nil = t.builtins.nil;
    let generic = t.generic();

    // Nothing is known about a generic type, so it's kept as is rather than being widened
    let generic_or_nil = t.union(vec![generic, nil]);
    assert_eq!(t.subtract(generic_or_nil, nil), generic);
    let generic_or_int = t.union(vec![generic, int]);
    assert_eq!(t.subtract(generic_or_int, int), generic);
    assert_eq!(t.subtract(generic_or_nil, atom), generic);
    assert_eq!(t.subtract(generic, nil), generic);

    // It's only removed if the target covers every possible value
    assert_eq!(t.subtract(generic_or_nil, generic), nil);
    let never = t.subtract(generic_or_nil, any);
    assert_eq!(t.name(never), "Never");

    // Generic types nested in other types are kept as well
    let generic_pair = t.pair(generic, nil);
    let alias = t.alias(generic_or_nil);
    let from = t.union(vec![generic_pair, alias]);
    let result = t.subtract(from, nil);
    let generic_name = t.name(generic);
    assert_eq!(
        t.name(result),
        format!("{generic_name} | ({generic_name}, nil)")
    );
}

#[test]
fn test_recursive_types() {
    let mut t = Types::new();
    let int = t.builtins.int;
    let nil = t.builtins.nil;
    let bytes32 = t.builtins.bytes32;
    let list = t.list(int);
    let list_pair = t.pair(int, list);

    let not_truthy = Check::IsAtom {
        can_be_truthy: false,
    };
    assert_eq!(t.check(list, nil), Ok(not_truthy));
    assert_eq!(
        t.check(list, list_pair),
        Ok(Check::IsPair {
            can_be_truthy: false
        })
    );
    assert_eq!(t.check(list, list), Ok(Check::None));

    let from = t.union(vec![list, bytes32]);
    assert_eq!(
        t.check(from, list),
        Ok(Check::Or(vec![IS_PAIR, value(&[])]))
    );

    let else_type = t.subtract(list, nil);
    assert_eq!(t.name(else_type), "1 @ (Int, nil | <1>)");
}

#[test]
fn test_check_errors() {
    let mut t = Types::new();
    let any = t.builtins.permissive_any;
    let int = t.builtins.int;

    // Function types can't be checked at runtime
    let function = t.function();
    assert_eq!(t.check(any, function), Err(CheckError::FunctionType));

    // But a value of function type can be checked against other types
    assert_eq!(t.check(function, int), Ok(IS_ATOM));

    // Checking an arbitrary value against a recursive type would require an infinite check
    let list = t.list(int);
    assert_eq!(t.check(any, list), Err(CheckError::DepthExceeded));
}

/// A nil-terminated list of the given types, like `[A, B, C]`.
fn tuple(t: &mut Types, items: &[TypeId]) -> TypeId {
    items
        .iter()
        .rev()
        .fold(t.builtins.nil, |rest, &first| t.pair(first, rest))
}

/// A nil-terminated list of the given values.
fn list_value(a: &mut Allocator, items: &[NodePtr]) -> NodePtr {
    items.iter().rev().fold(NodePtr::NIL, |rest, &first| {
        a.new_pair(first, rest).unwrap()
    })
}

#[test]
fn test_check_depth_bounds_nesting() {
    let mut t = Types::new();
    let mut a = Allocator::new();
    let any = t.builtins.permissive_any;
    let int = t.builtins.int;
    let bytes32 = t.builtins.bytes32;

    // The depth limit applies to how deeply the check is nested, not to how many pairs it checks
    let ints = tuple(&mut t, &[int; 13]);
    let check = t.check(any, ints).unwrap();
    let values: Vec<NodePtr> = (0..14).map(|i| a.new_small_number(i).unwrap()).collect();
    let pair = a.new_pair(values[0], values[1]).unwrap();
    for (len, expected) in [(12, false), (13, true), (14, false)] {
        let value = list_value(&mut a, &values[..len]);
        assert_eq!(eval(&a, &check, value), Ok(expected), "{len} items");
    }
    let mut nested = values[..13].to_vec();
    nested[12] = pair;
    let value = list_value(&mut a, &nested);
    assert_eq!(eval(&a, &check, value), Ok(false));

    let ints = tuple(&mut t, &[int; 24]);
    assert!(t.check(any, ints).is_ok());

    // Each pair still adds a level of nesting
    let ints = tuple(&mut t, &[int; 25]);
    assert_eq!(t.check(any, ints), Err(CheckError::DepthExceeded));

    // `(Coin, (Coin, (Coin, (Coin, nil))))` where `Coin` is `[Bytes32, Bytes32, Int]`
    let inner = tuple(&mut t, &[bytes32, bytes32, int]);
    let coin = t.structure(inner);
    let coins = tuple(&mut t, &[coin; 4]);
    let check = t.check(any, coins).unwrap();
    let hash = a.new_atom(&[1; 32]).unwrap();
    let short = a.new_atom(&[1; 31]).unwrap();
    let valid_coin = list_value(&mut a, &[hash, hash, values[1]]);
    let invalid_coin = list_value(&mut a, &[hash, short, values[1]]);
    for (coins, expected) in [
        (vec![valid_coin; 4], true),
        (vec![valid_coin; 3], false),
        (vec![valid_coin; 5], false),
        (
            vec![valid_coin, valid_coin, valid_coin, invalid_coin],
            false,
        ),
    ] {
        let value = list_value(&mut a, &coins);
        assert_eq!(eval(&a, &check, value), Ok(expected));
    }

    // Recursive types still can't be checked from `Any`
    let tree = t.alloc(Type::Unresolved);
    let branch = t.pair(tree, tree);
    let nil = t.builtins.nil;
    let inner = t.union(vec![nil, branch]);
    t.arena[tree] = Type::Alias(Alias {
        name: None,
        inner,
        generics: vec![],
    });
    assert_eq!(t.check(any, tree), Err(CheckError::DepthExceeded));
    let coin_list = t.list(coin);
    assert_eq!(t.check(any, coin_list), Err(CheckError::DepthExceeded));
}

#[test]
fn test_simplify() {
    // `None` in a union means the union always passes
    assert_eq!(
        Check::Or(vec![value(&[1]), Check::None]).simplify(),
        Check::None
    );

    // Impossible checks are removed from unions
    assert_eq!(
        Check::Or(vec![Check::Impossible, value(&[1])]).simplify(),
        value(&[1])
    );
    assert_eq!(
        Check::Or(vec![Check::Impossible]).simplify(),
        Check::Impossible
    );
    assert_eq!(Check::Or(vec![]).simplify(), Check::Impossible);

    // Nested unions are flattened and deduplicated
    assert_eq!(
        Check::Or(vec![value(&[1]), Check::Or(vec![value(&[2]), value(&[1])])]).simplify(),
        Check::Or(vec![value(&[1]), value(&[2])])
    );

    // Pairs where either side is impossible are impossible
    assert_eq!(
        pair(Check::Impossible, Check::None).simplify(),
        Check::Impossible
    );
    assert_eq!(pair(Check::None, Check::None).simplify(), Check::None);

    // Intersections
    assert_eq!(Check::And(vec![]).simplify(), Check::None);
    assert_eq!(
        Check::And(vec![value(&[1]), value(&[2])]).simplify(),
        Check::Impossible
    );
    assert_eq!(
        Check::And(vec![length(1), value(&[2])]).simplify(),
        value(&[2])
    );
    assert_eq!(
        Check::And(vec![length(2), value(&[2])]).simplify(),
        Check::Impossible
    );
    assert_eq!(
        Check::And(vec![IS_ATOM, IS_PAIR]).simplify(),
        Check::Impossible
    );
    assert_eq!(
        Check::And(vec![value(&[1]), Check::And(vec![Check::None, IS_ATOM])]).simplify(),
        Check::And(vec![IS_ATOM, value(&[1])])
    );
}

/// Evaluates a check against a value, the same way the generated code would. Returns an error if
/// the generated code would raise (for example, by calling `strlen` on a pair).
fn eval(a: &Allocator, check: &Check, node: NodePtr) -> Result<bool, String> {
    let truthy = |node| match a.sexp(node) {
        SExp::Atom => !a.atom(node).as_ref().is_empty(),
        SExp::Pair(..) => true,
    };

    let is_pair = matches!(a.sexp(node), SExp::Pair(..));

    Ok(match check {
        Check::None => true,
        Check::Impossible => false,
        // When an atom can't be truthy, `l` is optimized to a truthiness check
        Check::IsAtom { can_be_truthy } => {
            if *can_be_truthy {
                !is_pair
            } else {
                !truthy(node)
            }
        }
        Check::IsPair { can_be_truthy } => {
            if *can_be_truthy {
                is_pair
            } else {
                truthy(node)
            }
        }
        Check::Atom(restriction) => {
            if is_pair {
                return Err(format!("atom check {restriction:?} on a pair"));
            }

            let atom = a.atom(node);

            match restriction {
                AtomRestriction::Value(value) => atom.as_ref() == value.as_ref(),
                AtomRestriction::Length(length) => atom.as_ref().len() == *length,
            }
        }
        Check::Pair(first, rest) => {
            let SExp::Pair(first_node, rest_node) = a.sexp(node) else {
                return Err("pair check on an atom".to_string());
            };

            eval(a, first, first_node)? && eval(a, rest, rest_node)?
        }
        Check::And(checks) => {
            for check in checks {
                if !eval(a, check, node)? {
                    return Ok(false);
                }
            }
            true
        }
        Check::Or(checks) => {
            for check in checks {
                if eval(a, check, node)? {
                    return Ok(true);
                }
            }
            false
        }
    })
}

/// Structurally determines whether a value is in a type (semantics are ignored). Generic types are
/// replaced with the type they're instantiated with.
fn contains(
    arena: &Arena<Type>,
    generics: &HashMap<TypeId, TypeId>,
    id: TypeId,
    a: &Allocator,
    node: NodePtr,
) -> bool {
    match &arena[id] {
        Type::Ref(id) => contains(arena, generics, *id, a, node),
        Type::Generic(_) => contains(arena, generics, generics[&id], a, node),
        Type::Alias(alias) => contains(arena, generics, alias.inner, a, node),
        Type::Struct(ty) => contains(arena, generics, ty.inner, a, node),
        Type::Union(ty) => ty
            .types
            .iter()
            .any(|&id| contains(arena, generics, id, a, node)),
        Type::Never => false,
        Type::Any => true,
        Type::Atom(atom) => {
            let SExp::Atom = a.sexp(node) else {
                return false;
            };

            let bytes = a.atom(node);

            match &atom.restriction {
                None => true,
                Some(AtomRestriction::Value(value)) => bytes.as_ref() == value.as_ref(),
                Some(AtomRestriction::Length(length)) => bytes.as_ref().len() == *length,
            }
        }
        Type::Pair(pair) => {
            let SExp::Pair(first, rest) = a.sexp(node) else {
                return false;
            };

            contains(arena, generics, pair.first, a, first)
                && contains(arena, generics, pair.rest, a, rest)
        }
        ty => panic!("unexpected type {ty:?}"),
    }
}

/// A small deterministic PRNG (`SplitMix64`), to avoid a dependency.
struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9e37_79b9_7f4a_7c15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        z ^ (z >> 31)
    }

    fn below(&mut self, n: usize) -> usize {
        (self.next() % n as u64) as usize
    }

    fn chance(&mut self, percent: usize) -> bool {
        self.below(100) < percent
    }
}

/// Atoms used to generate values, chosen so that they overlap with the restrictions below.
const ATOM_POOL: &[&[u8]] = &[
    &[],
    &[0],
    &[1],
    &[2],
    &[3],
    &[1, 2],
    &[0x80],
    &[7; 32],
    &[0; 32],
];

const VALUE_POOL: &[&[u8]] = &[&[], &[1], &[2], &[3], &[1, 2], &[7; 32]];

const LENGTH_POOL: &[usize] = &[0, 1, 2, 32];

const SEMANTIC_POOL: &[AtomSemantic] = &[
    AtomSemantic::Any,
    AtomSemantic::Bytes,
    AtomSemantic::Int,
    AtomSemantic::Bool,
];

struct Generator<'a> {
    t: &'a mut Types,
    rng: Rng,
    structs: Vec<TypeId>,
    /// A generic type and the type it's instantiated with, if generic leaves should be generated.
    generic: Option<(TypeId, TypeId)>,
}

impl Generator<'_> {
    fn random_atom(&mut self) -> TypeId {
        let semantic = SEMANTIC_POOL[self.rng.below(SEMANTIC_POOL.len())];

        let restriction = match self.rng.below(3) {
            0 => None,
            1 => Some(AtomRestriction::Value(Cow::Borrowed(
                VALUE_POOL[self.rng.below(VALUE_POOL.len())],
            ))),
            _ => Some(AtomRestriction::Length(
                LENGTH_POOL[self.rng.below(LENGTH_POOL.len())],
            )),
        };

        self.t.atom(semantic, restriction)
    }

    fn random_type(&mut self, depth: usize) -> TypeId {
        if depth >= 3 {
            return self.random_atom();
        }

        match self.rng.below(12) {
            0..=3 => self.random_atom(),
            4..=6 => {
                let first = self.random_type(depth + 1);
                let rest = self.random_type(depth + 1);
                self.t.pair(first, rest)
            }
            7..=8 => {
                let count = 2 + self.rng.below(2);
                let types = (0..count).map(|_| self.random_type(depth + 1)).collect();
                self.t.union(types)
            }
            9 => {
                let inner = self.random_type(depth + 1);
                self.t.alias(inner)
            }
            10 => {
                // Reuse structs sometimes, so that the same semantic appears in both types
                if !self.structs.is_empty() && self.rng.chance(50) {
                    self.structs[self.rng.below(self.structs.len())]
                } else {
                    let inner = self.random_type(depth + 1);
                    let id = self.t.structure(inner);
                    self.structs.push(id);
                    id
                }
            }
            _ => match self.rng.below(if self.generic.is_some() { 5 } else { 4 }) {
                0 => self.t.builtins.permissive_any,
                1 => self.t.builtins.nil,
                4 => self.generic.unwrap().0,
                _ => {
                    let item = self.random_type(depth + 1);
                    self.t.list(item)
                }
            },
        }
    }

    /// Generates a target type that is likely to be related to `from`.
    fn derive_type(&mut self, from: TypeId, depth: usize) -> TypeId {
        if depth >= 4 || self.rng.chance(15) {
            return self.random_type(depth.min(2));
        }

        match self.t.arena[from].clone() {
            Type::Ref(id) => self.derive_type(id, depth),
            Type::Union(ty) => {
                let mut types: Vec<TypeId> = ty
                    .types
                    .iter()
                    .filter(|_| self.rng.chance(60))
                    .copied()
                    .collect();

                for ty in &mut types {
                    if self.rng.chance(40) {
                        *ty = self.derive_type(*ty, depth + 1);
                    }
                }

                if types.is_empty() || self.rng.chance(30) {
                    types.push(self.random_type(2));
                }

                if types.len() == 1 {
                    types[0]
                } else {
                    self.t.union(types)
                }
            }
            Type::Pair(pair) => {
                let first = if self.rng.chance(50) {
                    self.derive_type(pair.first, depth + 1)
                } else {
                    pair.first
                };
                let rest = if self.rng.chance(50) {
                    self.derive_type(pair.rest, depth + 1)
                } else {
                    pair.rest
                };
                self.t.pair(first, rest)
            }
            Type::Alias(alias) => {
                if self.rng.chance(50) {
                    from
                } else {
                    self.derive_type(alias.inner, depth + 1)
                }
            }
            Type::Struct(ty) => {
                if self.rng.chance(50) {
                    from
                } else {
                    self.derive_type(ty.inner, depth + 1)
                }
            }
            _ => self.random_type(2),
        }
    }

    fn random_any_value(&mut self, a: &mut Allocator, depth: usize) -> NodePtr {
        if depth < 4 && self.rng.chance(30) {
            let first = self.random_any_value(a, depth + 1);
            let rest = self.random_any_value(a, depth + 1);
            a.new_pair(first, rest).unwrap()
        } else {
            a.new_atom(ATOM_POOL[self.rng.below(ATOM_POOL.len())])
                .unwrap()
        }
    }

    /// Generates a random value in a type, or `None` if it doesn't have any (within a depth limit).
    fn random_value(&mut self, a: &mut Allocator, id: TypeId, depth: usize) -> Option<NodePtr> {
        if depth > 8 {
            return None;
        }

        match self.t.arena[id].clone() {
            Type::Ref(id) => self.random_value(a, id, depth),
            Type::Generic(_) => self.random_value(a, self.generic.unwrap().1, depth),
            Type::Alias(alias) => self.random_value(a, alias.inner, depth),
            Type::Struct(ty) => self.random_value(a, ty.inner, depth),
            Type::Never => None,
            Type::Any => Some(self.random_any_value(a, depth)),
            Type::Union(ty) => {
                let mut types = ty.types.clone();

                while !types.is_empty() {
                    let ty = types.remove(self.rng.below(types.len()));

                    if let Some(value) = self.random_value(a, ty, depth + 1) {
                        return Some(value);
                    }
                }

                None
            }
            Type::Atom(atom) => Some(match atom.restriction {
                None => a
                    .new_atom(ATOM_POOL[self.rng.below(ATOM_POOL.len())])
                    .unwrap(),
                Some(AtomRestriction::Value(value)) => a.new_atom(&value).unwrap(),
                Some(AtomRestriction::Length(length)) => {
                    let candidates: Vec<&[u8]> = ATOM_POOL
                        .iter()
                        .copied()
                        .filter(|atom| atom.len() == length)
                        .collect();

                    if !candidates.is_empty() && self.rng.chance(70) {
                        a.new_atom(candidates[self.rng.below(candidates.len())])
                            .unwrap()
                    } else {
                        let bytes: Vec<u8> = (0..length).map(|_| self.rng.below(4) as u8).collect();
                        a.new_atom(&bytes).unwrap()
                    }
                }
            }),
            Type::Pair(pair) => {
                let first = self.random_value(a, pair.first, depth + 1)?;
                let rest = self.random_value(a, pair.rest, depth + 1)?;
                Some(a.new_pair(first, rest).unwrap())
            }
            ty => panic!("unexpected type {ty:?}"),
        }
    }
}

/// If `generics` is true, the types can contain a generic type, which is instantiated with a random
/// type. The check and else type must be correct for every instantiation.
fn run_property(seed: u64, iterations: usize, generics: bool) -> (Vec<String>, usize) {
    let mut failures = Vec::new();
    let mut generic_else_types = 0;

    for iteration in 0..iterations {
        let mut t = Types::new();
        let mut generator = Generator {
            t: &mut t,
            rng: Rng(seed.wrapping_add(iteration as u64)),
            structs: Vec::new(),
            generic: None,
        };

        if generics {
            let generic = generator.t.generic();
            let instantiation = generator.random_type(0);
            generator.generic = Some((generic, instantiation));
        }

        let from = generator.random_type(0);
        let to = generator.derive_type(from, 0);
        let generic = generator.generic;
        let instantiations: HashMap<TypeId, TypeId> = generic.into_iter().collect();

        let mut a = Allocator::new();
        let values: Vec<NodePtr> = (0..20)
            .filter_map(|_| generator.random_value(&mut a, from, 0))
            .collect();

        let mut rng = generator.rng;

        let Ok(check) = t.check(from, to) else {
            continue;
        };

        let else_type = t.subtract(from, to);

        let mut extra = Vec::new();

        // Also try values that are in both types, since they are more likely to find bugs
        for _ in 0..20 {
            let mut generator = Generator {
                t: &mut t,
                rng: Rng(rng.next()),
                structs: Vec::new(),
                generic,
            };

            if let Some(value) = generator.random_value(&mut a, to, 0) {
                extra.push(value);
            }
        }

        let mut from_name = t.name(from);
        let to_name = t.name(to);
        let else_name = t.name(else_type);

        if let Some((generic, instantiation)) = generic {
            let generic_name = t.name(generic);
            let instantiation_name = t.name(instantiation);
            from_name = format!("{from_name}` (where `{generic_name} = {instantiation_name}`)");

            if else_name.contains(&generic_name) {
                generic_else_types += 1;
            }
        }

        for value in values.into_iter().chain(extra) {
            if !contains(&t.arena, &instantiations, from, &a, value) {
                continue;
            }

            let expected = contains(&t.arena, &instantiations, to, &a, value);
            let actual = eval(&a, &check, value);
            let shown = display_value(&a, value);

            if actual != Ok(expected) {
                failures.push(format!(
                    "iteration {iteration}: `{shown}` in `{from_name}` is `{to_name}` should be {expected}, but check {check:?} yielded {actual:?}"
                ));
                break;
            }

            if !expected && !contains(&t.arena, &instantiations, else_type, &a, value) {
                failures.push(format!(
                    "iteration {iteration}: `{shown}` in `{from_name}` is not `{to_name}`, but else type `{else_name}` excludes it"
                ));
                break;
            }
        }
    }

    (failures, generic_else_types)
}

fn display_value(a: &Allocator, node: NodePtr) -> String {
    match a.sexp(node) {
        SExp::Atom => {
            let atom = a.atom(node);
            if atom.as_ref().is_empty() {
                "()".to_string()
            } else {
                format!("0x{}", hex::encode(atom.as_ref()))
            }
        }
        SExp::Pair(first, rest) => {
            format!("({} . {})", display_value(a, first), display_value(a, rest))
        }
    }
}

#[test]
fn test_check_property() {
    let (failures, _) = run_property(0x5eed, 20_000, false);
    assert_no_failures(&failures);
}

#[test]
fn test_check_property_generics() {
    let (failures, generic_else_types) = run_property(0x6e4e, 20_000, true);
    assert_no_failures(&failures);

    // Make sure generic types are actually kept in the else type, rather than being widened
    assert!(
        generic_else_types >= 500,
        "not enough else types with generics: {generic_else_types}"
    );
}

fn assert_no_failures(failures: &[String]) {
    assert!(
        failures.is_empty(),
        "{} failures, first few:\n{}",
        failures.len(),
        failures
            .iter()
            .take(10)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n")
    );
}

#[test]
fn test_property_coverage() {
    // Make sure the property test actually exercises interesting checks, so that it can't pass
    // vacuously if the generators stop producing overlapping types
    let mut kinds = HashMap::new();

    for iteration in 0..2_000 {
        let mut t = Types::new();
        let mut generator = Generator {
            t: &mut t,
            rng: Rng(0x5eed + iteration),
            structs: Vec::new(),
            generic: None,
        };
        let from = generator.random_type(0);
        let to = generator.derive_type(from, 0);

        let kind = match t.check(from, to) {
            Ok(Check::None) => "none",
            Ok(Check::Impossible) => "impossible",
            Ok(Check::Pair(..)) => "pair",
            Ok(Check::Or(..)) => "or",
            Ok(Check::And(..)) => "and",
            Ok(_) => "other",
            Err(_) => "error",
        };

        *kinds.entry(kind).or_insert(0) += 1;
    }

    for kind in ["none", "impossible", "pair", "or", "and"] {
        assert!(
            kinds.get(kind).copied().unwrap_or(0) >= 20,
            "not enough `{kind}` checks: {kinds:?}"
        );
    }
}
