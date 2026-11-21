//! A `match` expression compiles to the `if` and `else if` chain of type guards that it's
//! equivalent to, so each one here is compared with the chain it should generate.

use rue_options::CompilerOptions;

use crate::test_utils::compile_with;

const TYPES: &str = "
struct A { kind = 1, value: Int }
struct B { kind = 2, value: Bytes }
struct C { kind = 3 }
type AB = A | B;
type U = AB | C;
type Tree = Int | (Tree, Tree);
fn make() -> U { C {} }
";

/// The program of `main` in the source, after the shared types.
fn program(main: &str, options: CompilerOptions) -> String {
    let source = format!("{TYPES}\n{main}");
    let mut compiled = compile_with(&source, |o| {
        *o = CompilerOptions {
            std: false,
            ..options
        };
    });
    compiled.main_program()
}

#[track_caller]
fn check(match_main: &str, if_main: &str) {
    for options in [CompilerOptions::default(), CompilerOptions::debug()] {
        assert_eq!(
            program(match_main, options),
            program(if_main, options),
            "{options:?}"
        );
    }
}

#[test]
fn struct_union() {
    check(
        "fn main(u: U) -> Int { match u { A => u.value, B => u.value.length, C => 0 } }",
        "fn main(u: U) -> Int { if u is A { u.value } else if u is B { u.value.length } else { 0 } }",
    );
}

#[test]
fn nested_union_arm() {
    check(
        "fn main(u: U) -> Int { match u { C => 0, AB => 1 } }",
        "fn main(u: U) -> Int { if u is C { 0 } else { 1 } }",
    );
}

#[test]
fn union_pattern() {
    check(
        "fn main(u: U) -> Int { match u { A | C => 0, B => u.value.length } }",
        "fn main(u: U) -> Int { if u is A | C { 0 } else { u.value.length } }",
    );
}

#[test]
fn wildcard() {
    check(
        "fn main(u: U) -> Int { match u { A => u.value, _ => 0 } }",
        "fn main(u: U) -> Int { if u is A { u.value } else { 0 } }",
    );
}

#[test]
fn literals() {
    check(
        "fn main(n: Int, s: String) -> Int {
            let a = match n { 0 => 1, 1 => 2, _ => n };
            let b = match s { \"x\" => 1, \"yz\" => 2, _ => 3 };
            a + b
        }",
        "fn main(n: Int, s: String) -> Int {
            let a = if n is 0 { 1 } else if n is 1 { 2 } else { n };
            let b = if s is \"x\" { 1 } else if s is \"yz\" { 2 } else { 3 };
            a + b
        }",
    );
}

#[test]
fn bool_and_nil() {
    check(
        "fn main(b: Bool, l: List<Int>) -> Int {
            let x = match b { true => 1, false => 2 };
            let y = match l { nil => 0, (Int, List<Int>) => l.first };
            x + y
        }",
        "fn main(b: Bool, l: List<Int>) -> Int {
            let x = if b is true { 1 } else { 2 };
            let y = if l is nil { 0 } else { l.first };
            x + y
        }",
    );
}

#[test]
fn any() {
    check(
        "fn main(x: Any) -> Int { match x { Bytes => 1, (Any, Any) => 2 } }",
        "fn main(x: Any) -> Int { if x is Bytes { 1 } else { 2 } }",
    );
}

#[test]
fn recursive_alias() {
    check(
        "fn main(t: Tree) -> Int { match t { Int => t, (Tree, Tree) => main(t.first) + main(t.rest) } }",
        "fn main(t: Tree) -> Int { if t is Int { t } else { main(t.first) + main(t.rest) } }",
    );
}

#[test]
fn field_scrutinee() {
    check(
        "fn main(p: (U, Int)) -> Int { match p.first { A => p.first.value, _ => p.rest } }",
        "fn main(p: (U, Int)) -> Int { if p.first is A { p.first.value } else { p.rest } }",
    );
}

#[test]
fn expression_scrutinee() {
    check(
        "fn main() -> Int { match make() { A => 1, B => 2, C => 3 } }",
        "fn main() -> Int { let u = make(); if u is A { 1 } else if u is B { 2 } else { 3 } }",
    );
}

#[test]
fn generic_wildcard() {
    check(
        "fn main(x: Int | nil) -> Int { f(x) } fn f<T>(x: T | nil) -> Int { match x { nil => 0, _ => 1 } }",
        "fn main(x: Int | nil) -> Int { f(x) } fn f<T>(x: T | nil) -> Int { if x is nil { 0 } else { 1 } }",
    );
}

/// Every `if` of the chain is inline, while only the first `if` of an `else if` chain written out
/// is, so the chain is nested in blocks instead.
#[test]
fn inline_match() {
    check(
        "fn main(u: U, n: Int) -> Int {
            let a = inline match u { A => 1, B => 2, C => 3 };
            let b = inline match n { 0 => 1, _ => n };
            a + b
        }",
        "fn main(u: U, n: Int) -> Int {
            let a = inline if u is A { 1 } else { inline if u is B { 2 } else { 3 } };
            let b = inline if n is 0 { 1 } else { n };
            a + b
        }",
    );
}

#[test]
fn inline_match_expression_scrutinee() {
    check(
        "fn main() -> Int { inline match make() { A | B => 1, C => 2 } }",
        "fn main() -> Int { let u = make(); inline if u is A | B { 1 } else { 2 } }",
    );
}

/// Every check of an `inline match` is evaluated, so each one checks the value's whole type, like
/// an `inline if` does, rather than only the values that the arms before it leave.
#[test]
fn inline_match_checks_whole_type() {
    check(
        "fn main(x: nil | (Int, Int)) -> Int { inline match x { nil => 0, (1, Int) => 1, _ => 2 } }",
        "fn main(x: nil | (Int, Int)) -> Int { inline if x is nil { 0 } else { inline if x is (1, Int) { 1 } else { 2 } } }",
    );
}

/// A `match` statement whose only arm that falls through is the last is an `if` statement that
/// returns early.
#[test]
fn statement_with_early_return() {
    check(
        "fn main(u: U) -> Int { match u { A => { return u.value; } _ => {} } 0 }",
        "fn main(u: U) -> Int { if u is A { return u.value; } 0 }",
    );
}

/// An `if` and `else` statement whose `else` branch falls through is the same.
#[test]
fn if_else_statement_with_early_return() {
    check(
        "fn main(u: U) -> Int { if u is A { return u.value; } else {} 0 }",
        "fn main(u: U) -> Int { if u is A { return u.value; } 0 }",
    );
}

/// A statement whose arms all return is the same as the `if` chain that's the function's value.
#[test]
fn statement_that_returns() {
    check(
        "fn main(u: U) -> Int { match u { A => { return u.value; } B => { return 1; } C => { return 2; } } }",
        "fn main(u: U) -> Int { if u is A { u.value } else if u is B { 1 } else { 2 } }",
    );
}

/// An arm with a condition is the branch for its type guard and the condition, and the arms after
/// it check the same values that it does.
#[test]
fn arm_condition() {
    check(
        "fn main(u: U) -> Int { match u { A if u.value > 10 => 1, A => u.value, B => 2, C => 3 } }",
        "fn main(u: U) -> Int { if u is A && u.value > 10 { 1 } else if u is A { u.value } else if u is B { 2 } else { 3 } }",
    );
}

/// The condition reads what the arm binds from the narrowed value.
#[test]
fn arm_condition_with_binding() {
    check(
        "fn main(u: U) -> Int { match u { A { value } if value > 10 => value, B as b if b.value.length > 1 => 1, _ => 2 } }",
        "fn main(u: U) -> Int { if u is A && u.value > 10 { u.value } else if u is B && u.value.length > 1 { 1 } else { 2 } }",
    );
}

/// An arm whose type doesn't need to be checked only checks its condition, including `_`.
#[test]
fn arm_condition_without_check() {
    check(
        "fn main(u: U, c: Bool) -> Int { match u { A => 1, B => 2, C if c => 3, _ if !c => 4, _ => 5 } }",
        "fn main(u: U, c: Bool) -> Int { if u is A { 1 } else if u is B { 2 } else if c { 3 } else if !c { 4 } else { 5 } }",
    );
}

/// A condition on an atom doesn't rely on the check for what it reads, like the right side of `&&`.
#[test]
fn arm_condition_on_atoms() {
    check(
        "fn main(n: Int, c: Bool) -> Int { match n { 0 if c => 1, 1 | 2 if n + 1 > 2 => 2, _ => n } }",
        "fn main(n: Int, c: Bool) -> Int { if n is 0 && c { 1 } else if n is 1 | 2 && n + 1 > 2 { 2 } else { n } }",
    );
}

/// A condition that's a type guard narrows the arm's value.
#[test]
fn arm_condition_type_guard() {
    check(
        "fn main(t: Tree) -> Int { match t { (Tree, Tree) if t.first is Int => t.first, (Tree, Tree) => 0, Int => t } }",
        "fn main(t: Tree) -> Int { if t is (Tree, Tree) && t.first is Int { t.first } else if t is (Tree, Tree) { 0 } else { t } }",
    );
}

/// A value that isn't a reference is bound once, and each condition reads the binding.
#[test]
fn arm_condition_expression_scrutinee() {
    check(
        "fn main(n: Int) -> Int { match make() { A as a if a.value > n => a.value, A => 1, _ => 2 } }",
        "fn main(n: Int) -> Int { let u = make(); if u is A && u.value > n { u.value } else if u is A { 1 } else { 2 } }",
    );
}

/// An arm of an `inline match` with a condition is an `inline if` on both.
#[test]
fn inline_arm_condition() {
    check(
        "fn main(n: Int, c: Bool) -> Int { inline match n { 0 if c => 1, 1 if !c => 2, _ => n } }",
        "fn main(n: Int, c: Bool) -> Int { inline if n is 0 && c { 1 } else { inline if n is 1 && !c { 2 } else { n } } }",
    );
}

/// An arm with a condition doesn't handle its type in an `inline match` either, and each check is
/// still of the value's whole type.
#[test]
fn inline_arm_condition_on_a_union() {
    check(
        "fn main(u: U, c: Bool) -> Int { inline match u { A if c => 1, A => 2, B => 3, C => 4 } }",
        "fn main(u: U, c: Bool) -> Int { inline if u is A && c { 1 } else { inline if u is A { 2 } else { inline if u is B { 3 } else { 4 } } } }",
    );
}

/// A statement whose arm with a condition returns is an `if` statement that returns early.
#[test]
fn statement_with_arm_condition() {
    check(
        "fn main(u: U) -> Int { match u { A if u.value > 10 => { return u.value; } _ => {} } 0 }",
        "fn main(u: U) -> Int { if u is A && u.value > 10 { return u.value; } 0 }",
    );
}
