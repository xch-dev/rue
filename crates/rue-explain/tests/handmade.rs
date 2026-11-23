//! Programs written by hand in CLVM, with debug metadata built for them, for the cases that
//! compiled Rue doesn't produce: constants that differ between calls, code whose structure isn't
//! recognized, programs run from constants that no function describes, and no sidecar at all.

use std::collections::HashMap;

use clvmr::{Allocator, ChiaDialect, NodePtr, SExp, run_program, serde::node_to_bytes};
use expect_test::{Expect, expect};
use rue_clvm::{assemble, disassemble_value};
use rue_compiler::debug_metadata::{
    Annotation, Constraint, DebugMetadataBuilder, Function, Parameter,
};
use rue_explain::{EvalFailure, Explanation};

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
    assert_eq!(explanation.uncovered(), Vec::<usize>::new());

    let dialect = ChiaDialect::new(clvmr::ClvmFlags::empty());
    for solution in solutions {
        let env = assemble(&mut allocator, solution).unwrap();
        let expected = run_program(&mut allocator, &dialect, ptr, env, MAX_COST);
        let actual = explanation.evaluate(&mut allocator, &dialect, env, MAX_COST);
        match (expected, actual) {
            (Ok(expected), Ok(actual)) => assert_eq!(
                disassemble_value(&allocator, actual),
                disassemble_value(&allocator, expected.1),
                "{solution}"
            ),
            (Err(_), Err(EvalFailure::Clvm(_))) => {}
            (expected, actual) => panic!("{solution}: {expected:?} vs {actual:?}"),
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
