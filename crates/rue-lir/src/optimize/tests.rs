use clvmr::{Allocator, ChiaDialect, NodePtr, run_program, serde::node_to_bytes};
use expect_test::{Expect, expect};
use id_arena::Arena;
use num_bigint::BigUint;
use rue_clvm::{disassemble_program, disassemble_value};

use crate::{ClvmTarget, CodegenOptions, Lir, LirId, codegen, optimize};

fn atom(arena: &mut Arena<Lir>, bytes: &[u8]) -> LirId {
    arena.alloc(Lir::Atom(bytes.to_vec()))
}

fn path(arena: &mut Arena<Lir>, path: u32) -> LirId {
    arena.alloc(Lir::Path(BigUint::from(path)))
}

fn raise(arena: &mut Arena<Lir>) -> LirId {
    arena.alloc(Lir::Raise(vec![]))
}

fn not(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    arena.alloc(Lir::Not(value))
}

fn compile(
    arena: &Arena<Lir>,
    allocator: &mut Allocator,
    lir: LirId,
    target: ClvmTarget,
    optimized: bool,
) -> NodePtr {
    codegen(
        arena,
        allocator,
        lir,
        CodegenOptions {
            optimize_static_pairs: optimized,
            target,
        },
    )
    .unwrap()
}

fn run(
    allocator: &mut Allocator,
    target: ClvmTarget,
    program: NodePtr,
    env: NodePtr,
) -> Option<Vec<u8>> {
    let dialect = ChiaDialect::new(target.flags());
    let output = run_program(allocator, &dialect, program, env, 11_000_000_000).ok()?;
    Some(node_to_bytes(allocator, output.1).unwrap())
}

// Optimizes the LIR, checks the code generated for the default target, and checks that the
// optimized and unoptimized programs behave the same for each of the given environments, for each
// target
#[allow(clippy::needless_pass_by_value)]
fn check(arena: &mut Arena<Lir>, lir: LirId, envs: &[&str], expect: Expect) {
    for target in ClvmTarget::ALL {
        let optimized = optimize(arena, lir, target);

        let mut allocator = Allocator::new();
        let unoptimized_ptr = compile(arena, &mut allocator, lir, target, false);
        let optimized_ptr = compile(arena, &mut allocator, optimized, target, true);

        if target == ClvmTarget::default() {
            expect.assert_eq(&disassemble_program(&allocator, optimized_ptr));
        }

        for env in envs {
            let env_ptr = rue_clvm::assemble(&mut allocator, env).unwrap();
            let expected = run(&mut allocator, target, unoptimized_ptr, env_ptr);
            let actual = run(&mut allocator, target, optimized_ptr, env_ptr);
            assert_eq!(
                expected,
                actual,
                "{} and {} differ for env {env} with {target:?}",
                disassemble_program(&allocator, unoptimized_ptr),
                disassemble_program(&allocator, optimized_ptr),
            );
        }
    }
}

const ENVS: &[&str] = &["()", "(())", "(5)", "(1)", "(0x00)", "((1 . 2))", "(5 7)"];

// Some rewrites assume that values match their static types, so these only contain
// environments where the first argument is an atom or a list respectively
// Environments with three arguments that are atoms, for the rewrites that assume the operands of
// `=` are
const THREE_ATOM_ENVS: &[&str] = &[
    "(() () ())",
    "(1 () ())",
    "(() 5 1)",
    "(5 1 7)",
    "(() 0x00 ())",
    "(0x00 () 3)",
];

const ATOM_ENVS: &[&str] = &["()", "(())", "(5)", "(1)", "(0x00)", "(5 7)"];

#[test]
fn test_not_ne_nil_is_not_value() {
    // (not (= x ())) must not be optimized to x
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let nil = atom(&mut arena, &[]);
    let eq = arena.alloc(Lir::Eq(x, nil));
    let lir = not(&mut arena, eq);
    check(&mut arena, lir, ENVS, expect!["(not (= 2 ()))"]);
}

#[test]
fn test_double_not() {
    // (not (not x)) is only x if x is already a boolean
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let inner = not(&mut arena, x);
    let lir = not(&mut arena, inner);
    check(&mut arena, lir, ENVS, expect!["(not (not 2))"]);

    let x = path(&mut arena, 2);
    let listp = arena.alloc(Lir::Listp(x));
    let inner = not(&mut arena, listp);
    let lir = not(&mut arena, inner);
    check(&mut arena, lir, ENVS, expect!["(l 2)"]);

    // Triple not only needs a single not
    let x = path(&mut arena, 2);
    let a = not(&mut arena, x);
    let b = not(&mut arena, a);
    let lir = not(&mut arena, b);
    check(&mut arena, lir, ENVS, expect!["(not 2)"]);
}

#[test]
fn test_not_eq_boolean_nil() {
    // (= (l x) ()) is (not (l x)), since (l x) is always an atom
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let listp = arena.alloc(Lir::Listp(x));
    let nil = atom(&mut arena, &[]);
    let eq = arena.alloc(Lir::Eq(listp, nil));
    let lir = not(&mut arena, eq);
    check(&mut arena, lir, ENVS, expect!["(l 2)"]);
}

#[test]
fn test_eq_nil_condition() {
    // In a condition, (= x ()) is (not x), assuming that x is an atom
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let nil = atom(&mut arena, &[]);
    let eq = arena.alloc(Lir::Eq(nil, x));
    let one = atom(&mut arena, &[1]);
    let two = atom(&mut arena, &[2]);
    let lir = arena.alloc(Lir::If(eq, one, two, false));
    check(&mut arena, lir, ATOM_ENVS, expect!["(i 2 (q . 2) (q . 1))"]);
}

#[test]
fn test_listp_condition() {
    // (l x) isn't x in a condition, since x may be an atom other than nil
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let listp = arena.alloc(Lir::Listp(x));
    let one = atom(&mut arena, &[1]);
    let two = atom(&mut arena, &[2]);
    let lir = arena.alloc(Lir::If(listp, one, two, false));
    check(&mut arena, lir, ENVS, expect!["(i (l 2) (q . 1) (q . 2))"]);
}

#[test]
fn test_nil_check_of_any_value() {
    // `x is nil` for a value that could be any atom or a pair is lowered to
    // `(a (i (not (l x)) (q 9 x ()) ()) 1)`, which is `(not x)`, since `not` is false for a pair
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let listp = arena.alloc(Lir::Listp(x));
    let is_atom = not(&mut arena, listp);
    let nil = atom(&mut arena, &[]);
    let eq = arena.alloc(Lir::Eq(x, nil));
    let is_nil = arena.alloc(Lir::If(is_atom, eq, nil, false));
    check(&mut arena, is_nil, ENVS, expect!["(not 2)"]);

    // With the operands of `=` the other way around
    let eq = arena.alloc(Lir::Eq(nil, x));
    let flipped = arena.alloc(Lir::If(is_atom, eq, nil, false));
    check(&mut arena, flipped, ENVS, expect!["(not 2)"]);

    // In a condition, the `not` flips the branches
    let one = atom(&mut arena, &[1]);
    let two = atom(&mut arena, &[2]);
    let lir = arena.alloc(Lir::If(is_nil, one, two, false));
    check(&mut arena, lir, ENVS, expect!["(i 2 (q . 2) (q . 1))"]);

    // The value is only evaluated once, and still fails when it did before
    let first = arena.alloc(Lir::First(x));
    let copy = arena.alloc(Lir::First(x));
    let listp = arena.alloc(Lir::Listp(first));
    let is_atom = not(&mut arena, listp);
    let eq = arena.alloc(Lir::Eq(copy, nil));
    let lir = arena.alloc(Lir::If(is_atom, eq, nil, false));
    check(&mut arena, lir, ENVS, expect!["(not 4)"]);
}

#[test]
fn test_nil_check_with_not() {
    // `(i (l x) () (not x))` is `(not x)` whether or not the `if` is inline, since `not` can't
    // fail
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let listp = arena.alloc(Lir::Listp(x));
    let nil = atom(&mut arena, &[]);
    let not_x = not(&mut arena, x);

    for inline in [false, true] {
        let lir = arena.alloc(Lir::If(listp, nil, not_x, inline));
        check(&mut arena, lir, ENVS, expect!["(not 2)"]);
    }

    // The value is a field, which is nil, another atom or a pair
    let first = arena.alloc(Lir::First(x));
    let listp = arena.alloc(Lir::Listp(first));
    let not_first = not(&mut arena, first);
    let lir = arena.alloc(Lir::If(listp, nil, not_first, false));
    check(
        &mut arena,
        lir,
        &["((()))", "((0x00))", "(((1 . 2)))", "((5))", "(5)", "()"],
        expect!["(not 4)"],
    );
}

#[test]
fn test_nil_check_keeps_other_checks() {
    // An inline `if` evaluates `(= x ())` for a pair too, which fails, so it isn't `(not x)`.
    // It's the `all` of both checks, where `(= x ())` is only `(not x)` for an atom, which the
    // optimizer assumes of the operands of `=` (see `opt_truthy`).
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let listp = arena.alloc(Lir::Listp(x));
    let nil = atom(&mut arena, &[]);
    let eq = arena.alloc(Lir::Eq(x, nil));
    let lir = arena.alloc(Lir::If(listp, nil, eq, true));
    check(
        &mut arena,
        lir,
        ATOM_ENVS,
        expect!["(all (not (l 2)) (not 2))"],
    );

    // With the operands of `=` the other way around too
    let eq = arena.alloc(Lir::Eq(nil, x));
    let lir = arena.alloc(Lir::If(listp, nil, eq, true));
    check(&mut arena, lir, ENVS, expect!["(i (l 2) () (= () 2))"]);

    // A different value is checked
    let y = path(&mut arena, 5);
    let eq = arena.alloc(Lir::Eq(y, nil));
    let lir = arena.alloc(Lir::If(listp, nil, eq, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(a (i (l 2) () (q 9 5 ())) 1)"],
    );

    // The pair isn't false
    let one = atom(&mut arena, &[1]);
    let not_x = not(&mut arena, x);
    let lir = arena.alloc(Lir::If(listp, one, not_x, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(a (i (l 2) (q 1 . 1) (q 32 2)) 1)"],
    );

    // The check is against another atom
    let eq = arena.alloc(Lir::Eq(x, one));
    let lir = arena.alloc(Lir::If(listp, nil, eq, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(a (i (l 2) () (q 9 2 (q . 1))) 1)"],
    );
}

#[test]
fn test_inline_if_of_a_bool_is_all_or_any() {
    // An inline `if` evaluates both branches, so `(i a 1 b)` is `(any a b)` if `b` is a bool,
    // which is what `a || b` is lowered to when `b` is evaluated either way
    let mut arena = Arena::new();
    let a = path(&mut arena, 2);
    let b = path(&mut arena, 5);
    let nil = atom(&mut arena, &[]);
    let one = atom(&mut arena, &[1]);
    let b_is_nil = arena.alloc(Lir::Eq(b, nil));
    let lir = arena.alloc(Lir::If(a, one, b_is_nil, true));
    check(&mut arena, lir, THREE_ATOM_ENVS, expect!["(any 2 (not 5))"]);

    // And `(i a b ())` is `(all a b)`
    let lir = arena.alloc(Lir::If(a, b_is_nil, nil, true));
    check(&mut arena, lir, THREE_ATOM_ENVS, expect!["(all 2 (not 5))"]);

    // With the constant in the other branch, the condition is negated
    let lir = arena.alloc(Lir::If(a, nil, b_is_nil, true));
    check(
        &mut arena,
        lir,
        THREE_ATOM_ENVS,
        expect!["(all (not 2) (not 5))"],
    );
    let lir = arena.alloc(Lir::If(a, b_is_nil, one, true));
    check(
        &mut arena,
        lir,
        THREE_ATOM_ENVS,
        expect!["(any (not 2) (not 5))"],
    );

    // A value that isn't known to be a bool is the result as it is
    let lir = arena.alloc(Lir::If(a, one, b, true));
    check(&mut arena, lir, THREE_ATOM_ENVS, expect!["(i 2 (q . 1) 5)"]);
    let lir = arena.alloc(Lir::If(a, b, nil, true));
    check(&mut arena, lir, THREE_ATOM_ENVS, expect!["(i 2 5 ())"]);

    // Unless only its truthiness is observed
    let two = atom(&mut arena, &[2]);
    let either = arena.alloc(Lir::If(a, one, b, true));
    let lir = arena.alloc(Lir::If(either, one, two, false));
    check(
        &mut arena,
        lir,
        THREE_ATOM_ENVS,
        expect!["(i (any 2 5) (q . 1) (q . 2))"],
    );
    let both = arena.alloc(Lir::If(a, b, nil, true));
    let lir = arena.alloc(Lir::If(both, one, two, false));
    check(
        &mut arena,
        lir,
        THREE_ATOM_ENVS,
        expect!["(i (all 2 5) (q . 1) (q . 2))"],
    );

    // Chains are flattened, since every operand is evaluated in order either way
    let c = path(&mut arena, 11);
    let c_is_nil = arena.alloc(Lir::Eq(c, nil));
    let first = arena.alloc(Lir::If(a, one, b_is_nil, true));
    let lir = arena.alloc(Lir::If(first, one, c_is_nil, true));
    check(
        &mut arena,
        lir,
        THREE_ATOM_ENVS,
        expect!["(any 2 (not 5) (not 11))"],
    );

    // A branch that fails is still evaluated when the condition decides the result
    let fails = arena.alloc(Lir::Eq(a, a));
    let raises = raise(&mut arena);
    let fails = arena.alloc(Lir::If(fails, nil, raises, false));
    let fails = arena.alloc(Lir::Eq(fails, nil));
    let lir = arena.alloc(Lir::If(a, one, fails, true));
    check(
        &mut arena,
        lir,
        ATOM_ENVS,
        expect!["(any 2 (not (a (i (= 2 2) () (q 8)) 1)))"],
    );

    // An `if` that isn't inline only evaluates the branch that's taken
    let lir = arena.alloc(Lir::If(a, one, b_is_nil, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(a (i 2 (q 1 . 1) (q 9 5 ())) 1)"],
    );
}

#[test]
fn test_if_branches_directly() {
    // `if a || b { () } else { 2 }`, where `b` is only evaluated if `a` is false, branches to the
    // result as soon as `a` decides it, since `()` is smaller than the `(q 1 . 1)` it replaces
    let mut arena = Arena::new();
    let a = path(&mut arena, 2);
    let b = path(&mut arena, 5);
    let nil = atom(&mut arena, &[]);
    let one = atom(&mut arena, &[1]);
    let two = atom(&mut arena, &[2]);
    let b_first = arena.alloc(Lir::First(b));
    let either = arena.alloc(Lir::If(a, one, b_first, false));
    let lir = arena.alloc(Lir::If(either, nil, two, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(a (i 2 () (q 3 9 () (q . 2))) 1)"],
    );

    // A chain branches once per operand
    let c = path(&mut arena, 11);
    let c_first = arena.alloc(Lir::First(c));
    let any = arena.alloc(Lir::If(either, one, c_first, false));
    let lir = arena.alloc(Lir::If(any, nil, two, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(a (i 2 () (q 2 (i 9 () (q 3 19 () (q . 2))) 1)) 1)"],
    );

    // `if a && b { 2 } else { () }` does the same for the branch that's taken when `a` is false
    let both = arena.alloc(Lir::If(a, b_first, nil, false));
    let lir = arena.alloc(Lir::If(both, two, nil, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(a (i 2 (q 3 9 (q . 2) ()) ()) 1)"],
    );

    // A branch that's larger than the constant would be repeated, so the condition is kept
    let sum = arena.alloc(Lir::Add(vec![a, b]));
    let lir = arena.alloc(Lir::If(either, sum, two, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(a (i (a (i 2 (q 1 . 1) (q . 9)) 1) (q 16 2 5) (q 1 . 2)) 1)"],
    );
    let lir = arena.alloc(Lir::If(both, two, one, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(i (a (i 2 (q . 9) ()) 1) (q . 2) (q . 1))"],
    );
}

#[test]
fn test_if_in_condition() {
    // Only the truthiness of each branch of an if in a condition is observed
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let y = path(&mut arena, 5);
    let inner = not(&mut arena, y);
    let truthy = not(&mut arena, inner);
    let nil = atom(&mut arena, &[]);
    let condition = arena.alloc(Lir::If(x, truthy, nil, false));
    let one = atom(&mut arena, &[1]);
    let two = atom(&mut arena, &[2]);
    let lir = arena.alloc(Lir::If(condition, one, two, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(i (a (i 2 (q . 5) ()) 1) (q . 1) (q . 2))"],
    );

    // A branch that fails is kept, and one whose truthiness is known becomes an atom
    let x = path(&mut arena, 2);
    let fails = raise(&mut arena);
    let one = atom(&mut arena, &[1]);
    let pair = arena.alloc(Lir::Cons(one, one));
    let condition = arena.alloc(Lir::If(x, fails, pair, false));
    let lir = arena.alloc(Lir::If(condition, one, two, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(a (i 2 (q 3 (x) (q . 1) (q . 2)) (q 1 . 1)) 1)"],
    );

    // So does a branch of an inline if, since atoms can't fail
    let x = path(&mut arena, 2);
    let pair = arena.alloc(Lir::Cons(one, one));
    let condition = arena.alloc(Lir::If(x, pair, nil, true));
    let lir = arena.alloc(Lir::If(condition, one, two, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(i (i 2 (q . 1) ()) (q . 1) (q . 2))"],
    );

    // A lazy if becomes inline once both of its branches are atoms
    let x = path(&mut arena, 2);
    let pair = arena.alloc(Lir::Cons(one, one));
    let falsy = not(&mut arena, one);
    let condition = arena.alloc(Lir::If(x, pair, falsy, false));
    let lir = arena.alloc(Lir::If(condition, one, two, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(i (i 2 (q . 1) ()) (q . 1) (q . 2))"],
    );
}

#[test]
fn test_quote_condition() {
    // A quoted pair or non-empty atom compiles to a pair, and a quoted nil to nil
    let mut arena = Arena::new();
    let one = atom(&mut arena, &[1]);
    let two = atom(&mut arena, &[2]);
    let x = path(&mut arena, 2);
    let pair = arena.alloc(Lir::Cons(one, x));
    let quoted = arena.alloc(Lir::Quote(pair));
    let lir = arena.alloc(Lir::If(quoted, one, two, false));
    check(&mut arena, lir, ENVS, expect!["(q . 1)"]);

    let five = atom(&mut arena, &[5]);
    let quoted = arena.alloc(Lir::Quote(five));
    let lir = arena.alloc(Lir::If(quoted, one, two, false));
    check(&mut arena, lir, ENVS, expect!["(q . 1)"]);

    let nil = atom(&mut arena, &[]);
    let quoted = arena.alloc(Lir::Quote(nil));
    let lir = arena.alloc(Lir::If(quoted, one, two, false));
    check(&mut arena, lir, ENVS, expect!["(q . 2)"]);
}

#[test]
fn test_inline_if_drops_pure_branch() {
    // Both branches of an inline if are evaluated, so the other one is only dropped if it can't fail
    let mut arena = Arena::new();
    let nil = atom(&mut arena, &[]);
    let x = path(&mut arena, 2);
    let one = atom(&mut arena, &[1]);
    let lir = arena.alloc(Lir::If(nil, one, x, true));
    check(&mut arena, lir, ENVS, expect!["2"]);

    let fails = raise(&mut arena);
    let lir = arena.alloc(Lir::If(nil, fails, x, true));
    check(&mut arena, lir, ENVS, expect!["(i () (x) 2)"]);

    // The same applies when the condition is known to be true
    let lir = arena.alloc(Lir::If(one, x, one, true));
    check(&mut arena, lir, ENVS, expect!["2"]);

    let lir = arena.alloc(Lir::If(one, x, fails, true));
    check(&mut arena, lir, ENVS, expect!["(i () (x) 2)"]);
}

#[test]
fn test_first_and_rest_of_pair() {
    // The other side of the pair is only dropped if it can't fail
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let one = atom(&mut arena, &[1]);
    let pair = arena.alloc(Lir::Cons(x, one));
    let lir = arena.alloc(Lir::First(pair));
    check(&mut arena, lir, ENVS, expect!["2"]);

    let pair = arena.alloc(Lir::Cons(one, x));
    let lir = arena.alloc(Lir::Rest(pair));
    check(&mut arena, lir, ENVS, expect!["2"]);

    let fails = raise(&mut arena);
    let pair = arena.alloc(Lir::Cons(x, fails));
    let lir = arena.alloc(Lir::First(pair));
    check(&mut arena, lir, ENVS, expect!["(f (c 2 (x)))"]);

    let pair = arena.alloc(Lir::Cons(fails, x));
    let lir = arena.alloc(Lir::Rest(pair));
    check(&mut arena, lir, ENVS, expect!["(r (c (x) 2))"]);
}

#[test]
fn test_div_and_mod_pair() {
    // A pair of the quotient and remainder of the same operands is a divmod
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let y = path(&mut arena, 5);
    let div = arena.alloc(Lir::Div(x, y));
    let other_x = path(&mut arena, 2);
    let modulo = arena.alloc(Lir::Mod(other_x, y));
    let lir = arena.alloc(Lir::Cons(div, modulo));
    check(
        &mut arena,
        lir,
        &[
            "(7 2)",
            "(-7 2)",
            "(7 0)",
            "(7 (1 . 2))",
            "((1 . 2) 2)",
            "(7)",
        ],
        expect!["(divmod 2 5)"],
    );

    // Different operands aren't combined
    let z = path(&mut arena, 11);
    let modulo = arena.alloc(Lir::Mod(x, z));
    let lir = arena.alloc(Lir::Cons(div, modulo));
    check(
        &mut arena,
        lir,
        &["(7 2 3)"],
        expect!["(c (/ 2 5) (% 2 11))"],
    );
}

#[test]
fn test_div_and_mod_of_matching_expressions() {
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let y = path(&mut arena, 5);
    let one = atom(&mut arena, &[1]);
    let envs = &[
        "(7 2)",
        "(-7 2)",
        "(7 0)",
        "((1 . 2) 2)",
        "((7 . 8) 2)",
        "(7)",
    ];

    // Separately built expressions with the same structure are combined
    let left = arena.alloc(Lir::Add(vec![x, one]));
    let div = arena.alloc(Lir::Div(left, y));
    let left = arena.alloc(Lir::Add(vec![x, one]));
    let modulo = arena.alloc(Lir::Mod(left, y));
    let lir = arena.alloc(Lir::Cons(div, modulo));
    check(&mut arena, lir, envs, expect!["(divmod (+ 2 (q . 1)) 5)"]);

    // Expressions that differ aren't, even if they're similar
    // The first and rest are taken of a run, so that they aren't folded into paths
    let program = path(&mut arena, 5);
    let run = arena.alloc(Lir::Run(program, x));
    let first = arena.alloc(Lir::First(run));
    let rest = arena.alloc(Lir::Rest(run));
    let div = arena.alloc(Lir::Div(first, y));
    let modulo = arena.alloc(Lir::Mod(rest, y));
    let lir = arena.alloc(Lir::Cons(div, modulo));
    check(
        &mut arena,
        lir,
        envs,
        expect!["(c (/ (f (a 5 2)) 5) (% (r (a 5 2)) 5))"],
    );

    let two_args = arena.alloc(Lir::Concat(vec![x, y]));
    let one_arg = arena.alloc(Lir::Concat(vec![x]));
    let div = arena.alloc(Lir::Div(two_args, y));
    let modulo = arena.alloc(Lir::Mod(one_arg, y));
    let lir = arena.alloc(Lir::Cons(div, modulo));
    check(
        &mut arena,
        lir,
        envs,
        expect!["(c (/ (concat 2 5) 5) (% (concat 2) 5))"],
    );
}

#[test]
fn test_listp_of_pair() {
    let mut arena = Arena::new();
    let one = atom(&mut arena, &[1]);
    let nil = atom(&mut arena, &[]);
    let pair = arena.alloc(Lir::Cons(one, nil));
    let lir = arena.alloc(Lir::Listp(pair));
    check(&mut arena, lir, ENVS, expect!["(q . 1)"]);

    // In a condition, a pair that may fail is still evaluated
    let x = path(&mut arena, 2);
    let pair = arena.alloc(Lir::Cons(x, nil));
    let listp = arena.alloc(Lir::Listp(pair));
    let two = atom(&mut arena, &[2]);
    let lir = arena.alloc(Lir::If(listp, one, two, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(i (c 2 ()) (q . 1) (q . 2))"],
    );
}

#[test]
fn test_nested_all_and_any() {
    let mut arena = Arena::new();
    let a = path(&mut arena, 2);
    let b = path(&mut arena, 5);
    let c = path(&mut arena, 11);
    let inner = arena.alloc(Lir::All(vec![b, c]));
    let lir = arena.alloc(Lir::All(vec![a, inner]));
    check(&mut arena, lir, ENVS, expect!["(all 2 5 11)"]);

    let inner = arena.alloc(Lir::Any(vec![b, c]));
    let lir = arena.alloc(Lir::Any(vec![a, inner]));
    check(&mut arena, lir, ENVS, expect!["(any 2 5 11)"]);

    // An any inside of an all isn't flattened
    let inner = arena.alloc(Lir::Any(vec![b, c]));
    let lir = arena.alloc(Lir::All(vec![a, inner]));
    check(&mut arena, lir, ENVS, expect!["(all 2 (any 5 11))"]);

    // A nested false value is only kept once
    let nil = atom(&mut arena, &[]);
    let inner = arena.alloc(Lir::All(vec![a, nil]));
    let lir = arena.alloc(Lir::All(vec![nil, inner]));
    check(&mut arena, lir, ENVS, expect!["(all () 2)"]);

    // A single value in a condition is used directly
    let single = arena.alloc(Lir::All(vec![a]));
    let one = atom(&mut arena, &[1]);
    let two = atom(&mut arena, &[2]);
    let lir = arena.alloc(Lir::If(single, one, two, false));
    check(&mut arena, lir, ENVS, expect!["(i 2 (q . 1) (q . 2))"]);

    let single = arena.alloc(Lir::Any(vec![a]));
    let lir = arena.alloc(Lir::If(single, one, two, false));
    check(&mut arena, lir, ENVS, expect!["(i 2 (q . 1) (q . 2))"]);

    // An empty any is false
    let empty = arena.alloc(Lir::Any(vec![]));
    let lir = arena.alloc(Lir::If(empty, one, two, false));
    check(&mut arena, lir, ENVS, expect!["(q . 2)"]);
}

#[test]
fn test_if_boolean_branches() {
    // (i c 1 ()) is c if c is always () or 1
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let y = path(&mut arena, 5);
    let gt = arena.alloc(Lir::Gt(x, y));
    let one = atom(&mut arena, &[1]);
    let nil = atom(&mut arena, &[]);
    let lir = arena.alloc(Lir::If(gt, one, nil, false));
    check(
        &mut arena,
        lir,
        &["(1 2)", "(2 1)", "((1 . 2) 1)"],
        expect!["(> 2 5)"],
    );

    // Otherwise it isn't, since the value isn't a boolean
    let lir = arena.alloc(Lir::If(x, one, nil, false));
    check(&mut arena, lir, ENVS, expect!["(i 2 (q . 1) ())"]);

    // A condition that's known is folded instead
    let empty = arena.alloc(Lir::All(vec![]));
    let lir = arena.alloc(Lir::If(empty, one, nil, false));
    check(&mut arena, lir, ENVS, expect!["(q . 1)"]);

    // (i c () 1) is always (not c)
    let lir = arena.alloc(Lir::If(x, nil, one, false));
    check(&mut arena, lir, ENVS, expect!["(not 2)"]);
}

#[test]
fn test_not_pure_pair() {
    let mut arena = Arena::new();
    let a = atom(&mut arena, &[1]);
    let b = atom(&mut arena, &[]);
    let pair = arena.alloc(Lir::Cons(a, b));
    let lir = not(&mut arena, pair);
    check(&mut arena, lir, ENVS, expect!["()"]);
}

#[test]
fn test_not_impure_pair() {
    // The pair is truthy, but evaluating it fails
    let mut arena = Arena::new();
    let a = raise(&mut arena);
    let b = atom(&mut arena, &[]);
    let pair = arena.alloc(Lir::Cons(a, b));
    let lir = not(&mut arena, pair);
    check(&mut arena, lir, ENVS, expect!["(not (c (x) ()))"]);

    // Paths can fail if they traverse into an atom
    let a = path(&mut arena, 5);
    let b = atom(&mut arena, &[]);
    let pair = arena.alloc(Lir::Cons(a, b));
    let lir = not(&mut arena, pair);
    check(&mut arena, lir, ENVS, expect!["(not (c 5 ()))"]);
}

#[test]
fn test_if_impure_pair() {
    let mut arena = Arena::new();
    let a = raise(&mut arena);
    let b = atom(&mut arena, &[]);
    let pair = arena.alloc(Lir::Cons(a, b));
    let one = atom(&mut arena, &[1]);
    let two = atom(&mut arena, &[2]);
    let lir = arena.alloc(Lir::If(pair, one, two, false));
    check(
        &mut arena,
        lir,
        ENVS,
        expect!["(i (c (x) ()) (q . 1) (q . 2))"],
    );

    let a = atom(&mut arena, &[1]);
    let b = atom(&mut arena, &[]);
    let pair = arena.alloc(Lir::Cons(a, b));
    let one = atom(&mut arena, &[1]);
    let two = atom(&mut arena, &[2]);
    let lir = arena.alloc(Lir::If(pair, one, two, false));
    check(&mut arena, lir, ENVS, expect!["(q . 1)"]);
}

#[test]
fn test_if_not_flips() {
    let mut arena = Arena::new();
    let x = path(&mut arena, 2);
    let cond = not(&mut arena, x);
    let one = atom(&mut arena, &[1]);
    let two = atom(&mut arena, &[2]);
    let lir = arena.alloc(Lir::If(cond, one, two, false));
    check(&mut arena, lir, ENVS, expect!["(i 2 (q . 2) (q . 1))"]);

    // A double not in a condition only matters for its truthiness
    let x = path(&mut arena, 2);
    let a = not(&mut arena, x);
    let cond = not(&mut arena, a);
    let one = atom(&mut arena, &[1]);
    let two = atom(&mut arena, &[2]);
    let lir = arena.alloc(Lir::If(cond, one, two, false));
    check(&mut arena, lir, ENVS, expect!["(i 2 (q . 1) (q . 2))"]);
}

#[test]
fn test_all_any_impure_pair() {
    let mut arena = Arena::new();
    let a = raise(&mut arena);
    let b = atom(&mut arena, &[]);
    let pair = arena.alloc(Lir::Cons(a, b));
    let x = path(&mut arena, 2);
    let lir = arena.alloc(Lir::All(vec![pair, x]));
    check(&mut arena, lir, ENVS, expect!["(all (c (x) ()) 2)"]);

    let a = raise(&mut arena);
    let b = atom(&mut arena, &[]);
    let pair = arena.alloc(Lir::Cons(a, b));
    let x = path(&mut arena, 2);
    let lir = arena.alloc(Lir::Any(vec![pair, x]));
    check(&mut arena, lir, ENVS, expect!["(any (c (x) ()) 2)"]);
}

#[test]
fn test_all_any_known() {
    let mut arena = Arena::new();
    let one = atom(&mut arena, &[1]);
    let a = atom(&mut arena, &[1]);
    let b = atom(&mut arena, &[]);
    let pair = arena.alloc(Lir::Cons(a, b));
    let x = path(&mut arena, 2);
    let lir = arena.alloc(Lir::All(vec![one, pair, x]));
    check(&mut arena, lir, ENVS, expect!["(all 2)"]);

    let nil = atom(&mut arena, &[]);
    let x = path(&mut arena, 2);
    let lir = arena.alloc(Lir::Any(vec![nil, x]));
    check(&mut arena, lir, ENVS, expect!["(any 2)"]);

    // The other arguments must still be evaluated, even though the result is known
    let nil = atom(&mut arena, &[]);
    let x = path(&mut arena, 5);
    let lir = arena.alloc(Lir::All(vec![nil, x]));
    check(&mut arena, lir, ENVS, expect!["(all () 5)"]);

    let one = atom(&mut arena, &[1]);
    let x = path(&mut arena, 5);
    let lir = arena.alloc(Lir::Any(vec![one, x]));
    check(&mut arena, lir, ENVS, expect!["(any (q . 1) 5)"]);

    // Double nots in arguments only matter for their truthiness
    let x = path(&mut arena, 2);
    let a = not(&mut arena, x);
    let b = not(&mut arena, a);
    let lir = arena.alloc(Lir::All(vec![b]));
    check(&mut arena, lir, ENVS, expect!["(all 2)"]);
}

#[test]
fn test_non_canonical_atoms_are_truthy() {
    let mut arena = Arena::new();
    for bytes in [&[0x00][..], &[0x00, 0x00], &[0x80]] {
        let value = atom(&mut arena, bytes);
        let lir = not(&mut arena, value);
        check(&mut arena, lir, ENVS, expect!["()"]);
    }
}

struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        self.0
    }

    fn below(&mut self, n: u64) -> u64 {
        self.next() % n
    }

    fn pick<T: Copy>(&mut self, items: &[T]) -> T {
        items[self.below(items.len() as u64) as usize]
    }
}

const FUZZ_ATOMS: &[&[u8]] = &[
    &[],
    &[1],
    &[2],
    &[0x00],
    &[0x00, 0x01],
    &[0xff],
    &[0x80],
    &[0x01, 0x00],
    // Longer than `LIMITS` allows for the dividend of `/` and `%`
    &[0x01; 257],
];

// These paths never share a parent, since opt_cons intentionally collapses a pair of sibling
// paths into their parent path, assuming that the parent is a pair (see its documentation)
// For the same reason, first and rest are never applied directly to a path
const FUZZ_PATHS: &[u32] = &[1, 2, 5, 11];

fn generate(arena: &mut Arena<Lir>, rng: &mut Rng, depth: u32) -> LirId {
    if depth == 0 || rng.below(5) == 0 {
        return if rng.below(2) == 0 {
            let bytes = rng.pick(FUZZ_ATOMS);
            atom(arena, bytes)
        } else {
            let p = rng.pick(FUZZ_PATHS);
            path(arena, p)
        };
    }

    let gen_args = |arena: &mut Arena<Lir>, rng: &mut Rng, min: u64| {
        let count = min + rng.below(3);
        (0..count)
            .map(|_| generate(arena, rng, depth - 1))
            .collect::<Vec<_>>()
    };

    let mut args = gen_args(arena, rng, 3);
    let mut next = || args.pop().unwrap();

    let lir = match rng.below(21) {
        0 => Lir::Not(next()),
        1 => eq(arena, next(), next()),
        2 => {
            let nil = atom(arena, &[]);
            if rng.below(2) == 0 {
                eq(arena, next(), nil)
            } else {
                eq(arena, nil, next())
            }
        }
        3 => Lir::Cons(next(), next()),
        4 => Lir::Listp(next()),
        // Whether a value which is always either nil or a pair has ended, the way a type guard
        // on a list is generated
        5 => {
            let pair = arena.alloc(Lir::Cons(next(), next()));
            let nil = atom(arena, &[]);
            let list = arena.alloc(Lir::If(next(), pair, nil, false));
            Lir::Not(list)
        }
        6 => Lir::If(next(), next(), next(), rng.below(2) == 0),
        7 => Lir::All(gen_args(arena, rng, 0)),
        8 => Lir::Any(gen_args(arena, rng, 0)),
        9 => Lir::Raise(vec![]),
        10 | 11 => {
            let mut value = next();
            while matches!(arena[value], Lir::Path(_)) {
                value = generate(arena, rng, depth - 1);
            }
            if rng.below(2) == 0 {
                Lir::First(value)
            } else {
                Lir::Rest(value)
            }
        }
        12 => Lir::Gt(next(), next()),
        13 => Lir::Add(gen_args(arena, rng, 1)),
        // The quoted program is itself optimized, so only quote atoms to keep the value the same
        14 => {
            let bytes = rng.pick(FUZZ_ATOMS);
            Lir::Quote(atom(arena, bytes))
        }
        15 => Lir::Sub(gen_args(arena, rng, 1)),
        16 => Lir::Mod(next(), next()),
        // Checks whether a value is odd
        17 => {
            let two = atom(arena, &[2]);
            let one = atom(arena, &[1]);
            let value = arena.alloc(Lir::Mod(next(), two));
            Lir::Eq(value, one)
        }
        // Runs a program in either a nil or the current environment
        18 => {
            let program = arena.alloc(Lir::Quote(next()));
            let env = if rng.below(2) == 0 {
                atom(arena, &[])
            } else {
                path(arena, 1)
            };
            Lir::Run(program, env)
        }
        // The quotient and remainder of the same operands, or of operands that are copies of each
        // other, or of operands that are different (but may happen to have the same structure)
        19 => {
            let left = next();
            let right = next();
            let div = arena.alloc(Lir::Div(left, right));
            let (mod_left, mod_right) = match rng.below(3) {
                0 => (left, right),
                1 => (copy(arena, left), copy(arena, right)),
                _ => (
                    generate(arena, rng, depth - 1),
                    generate(arena, rng, depth - 1),
                ),
            };
            let modulo = arena.alloc(Lir::Mod(mod_left, mod_right));
            Lir::Cons(div, modulo)
        }
        // A boolean (or other value) chosen by a condition
        20 => {
            let one = atom(arena, &[1]);
            let nil = atom(arena, &[]);
            let (then, otherwise) = if rng.below(2) == 0 {
                (one, nil)
            } else {
                (nil, one)
            };
            Lir::If(next(), then, otherwise, rng.below(2) == 0)
        }
        _ => unreachable!(),
    };

    arena.alloc(lir)
}

// Allocates a copy of the value, so that it has the same structure but different ids
fn copy(arena: &mut Arena<Lir>, value: LirId) -> LirId {
    let lir = match arena[value].clone() {
        Lir::Quote(value) => Lir::Quote(copy(arena, value)),
        Lir::First(value) => Lir::First(copy(arena, value)),
        Lir::Rest(value) => Lir::Rest(copy(arena, value)),
        Lir::Not(value) => Lir::Not(copy(arena, value)),
        Lir::Cons(a, b) => Lir::Cons(copy(arena, a), copy(arena, b)),
        Lir::Div(a, b) => Lir::Div(copy(arena, a), copy(arena, b)),
        Lir::Mod(a, b) => Lir::Mod(copy(arena, a), copy(arena, b)),
        Lir::Run(a, b) => Lir::Run(copy(arena, a), copy(arena, b)),
        Lir::Add(args) => Lir::Add(args.into_iter().map(|arg| copy(arena, arg)).collect()),
        Lir::Sub(args) => Lir::Sub(args.into_iter().map(|arg| copy(arena, arg)).collect()),
        lir => lir,
    };
    arena.alloc(lir)
}

// Rewrites in conditions assume that `=` is only used on atoms, as Rue's types ensure
// Either operand may become nil after it's optimized, so both are made atoms (+ fails on pairs)
fn eq(arena: &mut Arena<Lir>, left: LirId, right: LirId) -> Lir {
    let left = arena.alloc(Lir::Add(vec![left]));
    let right = arena.alloc(Lir::Add(vec![right]));
    Lir::Eq(left, right)
}

fn generate_env(allocator: &mut Allocator, rng: &mut Rng) -> NodePtr {
    let mut env = NodePtr::NIL;

    for _ in 0..rng.below(4) {
        let item = if rng.below(4) == 0 {
            let first = allocator.new_atom(rng.pick(FUZZ_ATOMS)).unwrap();
            let rest = allocator.new_atom(rng.pick(FUZZ_ATOMS)).unwrap();
            allocator.new_pair(first, rest).unwrap()
        } else {
            allocator.new_atom(rng.pick(FUZZ_ATOMS)).unwrap()
        };
        env = allocator.new_pair(item, env).unwrap();
    }

    env
}

// Generates random LIR trees and checks that the optimized program behaves exactly
// like the unoptimized one, with every error treated as equal
#[test]
fn fuzz_optimizer_differential() {
    let mut rng = Rng(0x9e37_79b9_7f4a_7c15);

    for _ in 0..20_000 {
        let mut arena = Arena::new();
        let lir = generate(&mut arena, &mut rng, 4);
        let target = rng.pick(&ClvmTarget::ALL);
        let optimized = optimize(&mut arena, lir, target);

        let mut allocator = Allocator::new();
        let unoptimized_ptr = compile(&arena, &mut allocator, lir, target, false);
        let optimized_ptr = compile(&arena, &mut allocator, optimized, target, true);

        for _ in 0..6 {
            let env = generate_env(&mut allocator, &mut rng);
            let expected = run(&mut allocator, target, unoptimized_ptr, env);
            let actual = run(&mut allocator, target, optimized_ptr, env);

            if expected != actual {
                let show = |allocator: &mut Allocator, value: Option<Vec<u8>>| {
                    value.map_or("error".to_string(), |bytes| {
                        let ptr = clvmr::serde::node_from_bytes(allocator, &bytes).unwrap();
                        disassemble_value(allocator, ptr)
                    })
                };
                let expected = show(&mut allocator, expected);
                let actual = show(&mut allocator, actual);
                panic!(
                    "optimizer changed behavior with {target:?} for env {}\nunoptimized: {} => {expected}\noptimized: {} => {actual}",
                    disassemble_value(&allocator, env),
                    disassemble_program(&allocator, unoptimized_ptr),
                    disassemble_program(&allocator, optimized_ptr),
                );
            }
        }
    }
}
