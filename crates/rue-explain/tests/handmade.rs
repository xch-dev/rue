//! Programs written by hand in CLVM, with debug metadata built for them, for the cases that
//! compiled Rue doesn't produce: constants that differ between calls, code whose structure isn't
//! recognized, programs run from constants that no function describes, and no sidecar at all.

use std::collections::HashMap;

use clvmr::{Allocator, ChiaDialect, NodePtr, SExp, serde::node_to_bytes};
use expect_test::{Expect, expect};
use rue_clvm::{assemble, disassemble_value};
use rue_compiler::debug_metadata::{
    Annotation, Constraint, DebugMetadataBuilder, Function, Parameter,
};
use rue_explain::Explanation;

const MAX_COST: u64 = 11_000_000_000;

/// A function for the metadata: its name, and the names and paths of its parameters (everything
/// else in its environment is hidden).
struct Described {
    name: &'static str,
    parameters: Parameter,
}

fn name(builder: &mut DebugMetadataBuilder, name: &str, path: u32) -> Parameter {
    Parameter::Name {
        name: builder.string(name),
        path: path.into(),
        constraint: Constraint::Unknown,
    }
}

fn pair(first: Parameter, rest: Parameter) -> Parameter {
    Parameter::Pair(Box::new(first), Box::new(rest))
}

/// The nodes of the program that are the bodies of functions, found by their disassembly.
fn sidecar(
    allocator: &Allocator,
    program: NodePtr,
    functions: impl FnOnce(&mut DebugMetadataBuilder) -> Vec<(String, Described)>,
) -> Vec<u8> {
    let mut builder = DebugMetadataBuilder::default();
    let functions = functions(&mut builder);
    let mut indices = HashMap::new();

    for (body, function) in functions {
        let name = builder.string(function.name);
        let index = builder.function(Function {
            name,
            left_env: false,
            parameters: function.parameters,
        });
        indices.insert(body, index);
    }

    builder
        .encode(allocator, program, |_, node, _| Annotation {
            function: match allocator.sexp(node) {
                SExp::Pair(..) => indices
                    .get(&rue_clvm::disassemble_program(allocator, node))
                    .copied(),
                SExp::Atom => None,
            },
            ..Annotation::default()
        })
        .unwrap()
}

/// Explains the program, and checks that it covers every node and runs like the program does with
/// each solution.
fn check(
    program: &str,
    functions: impl FnOnce(&mut DebugMetadataBuilder) -> Vec<(String, Described)>,
    solutions: &[&str],
    expected: &Expect,
) {
    let mut allocator = Allocator::new();
    let ptr = assemble(&mut allocator, program).unwrap();
    let bytes = node_to_bytes(&allocator, ptr).unwrap();
    let metadata = sidecar(&allocator, ptr, functions);
    let explanation = Explanation::new(&bytes, Some(&metadata)).unwrap();
    assert_eq!(explanation.uncovered(), 0);

    let dialect = ChiaDialect::new(clvmr::ClvmFlags::empty());
    for solution in solutions {
        let env = assemble(&mut allocator, solution).unwrap();
        if let Err(error) = explanation.check(&mut allocator, &dialect, ptr, env, MAX_COST) {
            panic!("{solution}: {error}\n{}", explanation.render());
        }
    }

    expected.assert_eq(&explanation.render());
}

/// A function that adds its hidden first value to its parameter.
fn add_hidden(builder: &mut DebugMetadataBuilder) -> Vec<(String, Described)> {
    vec![(
        "(+ 2 5)".to_string(),
        Described {
            name: "add",
            parameters: pair(Parameter::Nil, pair(name(builder, "x", 5), Parameter::Nil)),
        },
    )]
}

#[test]
fn same_constant_is_known() {
    check(
        "(+ (a (q . (+ 2 5)) (c (q . 10) (c 2 ()))) (a (q . (+ 2 5)) (c (q . 10) (c 5 ()))))",
        add_hidden,
        &["(1 2)"],
        &expect![[r#"
            program(solution) {
                add(x: solution@2) + add(x: solution@5)
            }

            fn add(x) {
                // env@2 is always 10
                10 + x
            }
        "#]],
    );
}

#[test]
fn different_constants_are_shown() {
    check(
        "(+ (a (q . (+ 2 5)) (c (q . 10) (c 2 ()))) (a (q . (+ 2 5)) (c (q . 20) (c 2 ()))))",
        add_hidden,
        &["(1)"],
        &expect![[r#"
            program(solution) {
                add(x: solution@2, env@2: 10) + add(x: solution@2, env@2: 20)
            }

            fn add(x) {
                env@2 + x
            }
        "#]],
    );
}

#[test]
fn escaping_function_has_unknown_values() {
    // The function's code is returned as a value, so it could be called from anywhere
    check(
        "(c (a (q . (+ 2 5)) (c (q . 10) (c 2 ()))) (q . (+ 2 5)))",
        add_hidden,
        &["(1)"],
        &expect![[r#"
            program(solution) {
                (add(x: solution@2, env@2: 10), add)
            }

            fn add(x) {
                env@2 + x
            }
        "#]],
    );
}

#[test]
fn escaping_through_the_environment() {
    // `wrap` returns its whole environment, which includes the `add` that it was passed
    check(
        "(+ (a (q . (+ 2 5)) (c (q . 10) (c 2 ()))) (a (a (q . (f 1)) (c (q . (+ 2 5)) ())) (c (q . 20) (c 2 ()))))",
        |builder| {
            let mut functions = add_hidden(builder);
            functions.push((
                "(f 1)".to_string(),
                Described {
                    name: "wrap",
                    parameters: pair(Parameter::Nil, Parameter::Nil),
                },
            ));
            functions
        },
        &["(1)"],
        &expect![[r#"
            program(solution) {
                add(x: solution@2, env@2: 10) + wrap()(20, solution@2)
            }

            fn add(x) {
                env@2 + x
            }

            fn wrap() {
                // env@2 is always add
                env.first
            }
        "#]],
    );
}

#[test]
fn computed_values_are_shown_even_if_unread() {
    // The hidden value isn't read, but computing it can fail
    check(
        "(a (q . (+ 5 (q . 1))) (c (/ 2 5) (c 2 ())))",
        add_hidden_unread,
        &["(6 3)", "(6 ())"],
        &expect![[r#"
            program(solution) {
                increment(x: solution@2, env@2: solution@2 / solution@5)
            }

            fn increment(x) {
                x + 1
            }
        "#]],
    );
}

/// A function like `add`, which doesn't read its hidden first value.
fn add_hidden_unread(builder: &mut DebugMetadataBuilder) -> Vec<(String, Described)> {
    vec![(
        "(+ 5 (q . 1))".to_string(),
        Described {
            name: "increment",
            parameters: pair(Parameter::Nil, pair(name(builder, "x", 5), Parameter::Nil)),
        },
    )]
}

#[test]
fn raw_code_with_its_environment() {
    check(
        "(a (q . (100 2 5)) (c (q . 5) 1))",
        |_| Vec::new(),
        &["(1)"],
        &expect![[r#"
            program(solution) {
                clvm!((100 2 5), env = (5, solution))
            }
        "#]],
    );
}

#[test]
fn large_constants_are_clvm() {
    check(
        "(c (q 1 2 3) (q 1 2 3 4 5 6 7 8 9 10))",
        |_| Vec::new(),
        &["()"],
        &expect![[r#"
            program(solution) {
                ([1, 2, 3], clvm!(q . (q 2 3 4 5 6 7 8 9 10)))
            }
        "#]],
    );
}

#[test]
fn deeply_nested_programs() {
    // `(f (f ... 1))` run with a solution that's as deeply nested, a list built from
    // `(c (q . 1) ...)`, and lets of `if`s inside of `let`s, which are all too deep to recurse on
    // without growing the stack
    let depth = 20_000;
    let firsts = format!("{}01{}", "ff05ff".repeat(depth), "80".repeat(depth));
    let nested = format!("{}01{}", "ff".repeat(depth), "80".repeat(depth));
    let list = format!("{}80{}", "ff04ffff0101ff".repeat(depth), "80".repeat(depth));

    // `(a (q . 2) (c (a (i 2 (q . INNER) (q . 5)) 1) 1))`
    let mut lets = "01".to_string();
    for _ in 0..depth {
        lets = format!("ff02ffff0102ffff04ffff02ffff03ff02ffff01{lets}ffff010580ff0180ff018080");
    }

    for (hex, solution, output) in [
        (firsts, nested, Some("1")),
        (list, "80".to_string(), None),
        (lets, "ff0180".to_string(), None),
    ] {
        let mut allocator = Allocator::new();
        let bytes = hex::decode(hex).unwrap();
        let ptr = clvmr::serde::node_from_bytes(&mut allocator, &bytes).unwrap();
        let explanation = Explanation::new(&bytes, None).unwrap();
        assert_eq!(explanation.uncovered(), 0);
        assert!(!explanation.render().is_empty());

        let dialect = ChiaDialect::new(clvmr::ClvmFlags::empty());
        let env =
            clvmr::serde::node_from_bytes(&mut allocator, &hex::decode(solution).unwrap()).unwrap();
        explanation
            .check(&mut allocator, &dialect, ptr, env, MAX_COST)
            .unwrap();

        if let Some(output) = output {
            let actual = explanation
                .evaluate(&mut allocator, &dialect, env, MAX_COST)
                .unwrap();
            assert_eq!(disassemble_value(&allocator, actual), output);
        }
    }
}

#[test]
fn function_passed_as_different_hidden_values() {
    // `apply` is passed `add` once and `mul` once, so the `add` it runs isn't the `add` that's
    // called directly with 10
    check(
        "(c (a (q . (a 2 (c (q . 99) (c 5 ())))) (c (q . (+ 2 5)) (c 2 ()))) (c (a (q . (a 2 (c (q . 99) (c 5 ())))) (c (q . (* 2 5)) (c 2 ()))) (a (q . (+ 2 5)) (c (q . 10) (c 2 ())))))",
        |builder| {
            let mut functions = add_hidden(builder);
            functions.push((
                "(* 2 5)".to_string(),
                Described {
                    name: "mul",
                    parameters: pair(Parameter::Nil, pair(name(builder, "x", 5), Parameter::Nil)),
                },
            ));
            functions.push((
                "(a 2 (c (q . 99) (c 5 ())))".to_string(),
                Described {
                    name: "apply",
                    parameters: pair(Parameter::Nil, pair(name(builder, "y", 5), Parameter::Nil)),
                },
            ));
            functions
        },
        &["(1)"],
        &expect![[r#"
            program(solution) {
                [
                    apply(y: solution@2, env@2: add),
                    apply(y: solution@2, env@2: mul),
                    ...add(x: solution@2, env@2: 10),
                ]
            }

            fn apply(y) {
                env@2(99, y)
            }

            fn add(x) {
                env@2 + x
            }

            fn mul(x) {
                env@2 * x
            }
        "#]],
    );
}

#[test]
fn function_in_the_environment_of_an_unknown_program() {
    // The solution's program is run with `add` in its environment, and runs it with 3
    check(
        "(c (a 2 (c (q . (+ 2 5)) ())) (a (q . (+ 2 5)) (c (q . 10) (c 5 ()))))",
        add_hidden,
        &["((a 2 (q 7 3)) 1)"],
        &expect![[r#"
            program(solution) {
                (solution@2(add), add(x: solution@5, env@2: 10))
            }

            fn add(x) {
                env@2 + x
            }
        "#]],
    );
}

#[test]
fn statements_in_branches_are_kept() {
    // An `else` that asserts before its `if`, and a branch that binds a value before it raises
    check(
        "(c (a (i 2 (q . 1) (q 2 (i 5 (q 2 (i 11 (q . 1) (q . 2)) 1) (q 8)) 1)) 1) (a (i 2 (q . 1) (q 2 (q 8 2) (c (+ 5 5) 1))) 1))",
        |_| Vec::new(),
        &["(1 2 3)", "(() 2 3)", "(() 2 ())", "(() () 3)"],
        &expect![[r#"
            program(solution) {
                (if solution@2 { solution } else {
                    assert solution@5
                    if solution@11 {
                        solution
                    } else {
                        solution@2
                    }
                }, if solution@2 { solution } else {
                    let v1 = solution@5 + solution@5
                    raise(v1)
                })
            }
        "#]],
    );
}

#[test]
fn rest_of_a_pair_is_evaluated_first() {
    // Both values raise, and the rest's does first
    check(
        "(c (a (q . 2) (c (x (q . 1)) 1)) (a (q . 2) (c (x (q . 2)) 1)))",
        |_| Vec::new(),
        &["()"],
        &expect![[r#"
            program(solution) {
                let v1 = raise(2)
                let v2 = raise(1)
                (v2, v1)
            }
        "#]],
    );
}

#[test]
fn recursion_through_hidden_value() {
    // `count` is passed to itself, like a captured function is
    check(
        "(a (q . (a 2 (c 2 (c 5 ())))) (c (q . (a (i 5 (q . (+ (q . 1) (a 2 (c 2 (c (r 5) ()))))) ()) 1)) 1))",
        |builder| {
            vec![(
                "(a (i 5 (q 16 (q . 1) (a 2 (c 2 (c (r 5) ())))) ()) 1)".to_string(),
                Described {
                    name: "count",
                    parameters: pair(
                        Parameter::Nil,
                        pair(name(builder, "items", 5), Parameter::Nil),
                    ),
                },
            )]
        },
        &["((1 2 3))", "(())"],
        &expect![[r#"
            program(solution) {
                count(items: solution@2)
            }

            fn count(items) {
                // env@2 is always count
                if items {
                    1 + count(items: items.rest)
                } else {
                    nil
                }
            }
        "#]],
    );
}

#[test]
fn unrecognized_code_is_raw() {
    // An operator that isn't one of CLVM's, and an operator that's a pair
    check(
        "(c (100 2 5) ((q . 1) 2))",
        |_| Vec::new(),
        &["(1 2)"],
        &expect![[r#"
            program(solution) {
                (clvm!((100 2 5)), clvm!(((q . 1) 2)))
            }
        "#]],
    );
}

#[test]
fn atoms_of_zero_bytes_are_nil() {
    // As code, an atom of zero bytes doesn't read anything, and evaluates to nil like nil does
    check(
        "(c 0x00 (c 0x0000 (a (q . 0x00) 1)))",
        |_| Vec::new(),
        &["(1)"],
        &expect![[r#"
            program(solution) {
                [nil, nil, ...nil]
            }
        "#]],
    );
}

#[test]
fn program_from_a_constant_is_anonymous() {
    check(
        "(a (q . (a 2 (c (q . 3) ()))) (c (q . (* 2 2)) ()))",
        |_| Vec::new(),
        &["()"],
        &expect![[r#"
            program(solution) {
                anonymous#1(env: [3])
            }

            fn anonymous#1(env) {
                env@2 * env@2
            }
        "#]],
    );
}

#[test]
fn without_a_sidecar() {
    let mut allocator = Allocator::new();
    let ptr = assemble(&mut allocator, "(a (i 5 (q 16 2 (q . 1)) (q 8)) 1)").unwrap();
    let bytes = node_to_bytes(&allocator, ptr).unwrap();
    let explanation = Explanation::new(&bytes, None).unwrap();
    expect![[r#"
        program(solution) {
            assert solution@5
            solution@2 + 1
        }
    "#]]
    .assert_eq(&explanation.render());
}

#[test]
fn sidecar_must_be_for_the_program() {
    let mut allocator = Allocator::new();
    let ptr = assemble(&mut allocator, "(+ 2 5)").unwrap();
    let other = assemble(&mut allocator, "(* 2 5)").unwrap();
    let metadata = sidecar(&allocator, other, |_| Vec::new());
    let bytes = node_to_bytes(&allocator, ptr).unwrap();
    assert!(Explanation::new(&bytes, Some(&metadata)).is_err());
}
