//! Programs written by hand in CLVM, with debug metadata built for them, for the cases that
//! compiled Rue doesn't produce: constants that differ between calls, code whose structure isn't
//! recognized, programs run from constants that no function describes, and no sidecar at all.

use std::collections::HashMap;

use clvmr::{
    Allocator, ChiaDialect, NodePtr, SExp, error::EvalErr, reduction::Reduction, run_program,
    serde::node_to_bytes,
};
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

    for solution in solutions {
        let env = assemble(&mut allocator, solution).unwrap();
        if let Err(error) = verify(&mut allocator, &explanation, ptr, env) {
            panic!("{solution}: {error}\n{}", explanation.render());
        }
    }

    expected.assert_eq(&explanation.render());
}

/// Checks that the explanation gives the same result as the program with the solution, with the
/// usual cost limit, and with only as much as the program needs (which the explanation has a
/// multiple of).
fn verify(
    allocator: &mut Allocator,
    explanation: &Explanation,
    program: NodePtr,
    env: NodePtr,
) -> Result<(), String> {
    let dialect = ChiaDialect::new(clvmr::ClvmFlags::empty());
    let expected = run_program(allocator, &dialect, program, env, MAX_COST);
    explanation.check(allocator, &dialect, &expected, env, MAX_COST)?;

    if let Ok(output) = &expected {
        let cost = output.0.max(1);
        let expected = run_program(allocator, &dialect, program, env, cost);
        explanation.check(allocator, &dialect, &expected, env, cost)?;
    }

    Ok(())
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
fn different_constants_are_different_versions() {
    check(
        "(+ (a (q . (+ 2 5)) (c (q . 10) (c 2 ()))) (a (q . (+ 2 5)) (c (q . 20) (c 2 ()))))",
        add_hidden,
        &["(1)"],
        &expect![[r#"
            program(solution) {
                add(x: solution@2) + add#2(x: solution@2)
            }

            fn add(x) {
                // env@2 is always 10
                10 + x
            }

            fn add#2(x) {
                // env@2 is always 20
                20 + x
            }
        "#]],
    );
}

#[test]
fn function_used_as_a_value_knows_nothing() {
    // The function's code is returned as a value, so it could be called from anywhere, which is
    // the version of it that knows nothing about its environment
    check(
        "(c (a (q . (+ 2 5)) (c (q . 10) (c 2 ()))) (q . (+ 2 5)))",
        add_hidden,
        &["(1)"],
        &expect![[r#"
            program(solution) {
                (add#2(x: solution@2), add)
            }

            fn add(x) {
                env@2 + x
            }

            fn add#2(x) {
                // env@2 is always 10
                10 + x
            }
        "#]],
    );
}

#[test]
fn function_in_a_returned_environment() {
    // `wrap` returns its whole environment, which includes the `add` that it was passed, and the
    // call to `add` is still to the version for 10
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
                add#2(x: solution@2) + wrap()(20, solution@2)
            }

            fn add(x) {
                env@2 + x
            }

            fn add#2(x) {
                // env@2 is always 10
                10 + x
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

    // A constant that's as deeply nested, which is shown as CLVM
    let constant = format!("ff01{}80{}", "ff".repeat(depth), "80".repeat(depth));

    for (hex, solution, output, nested_too_deeply) in [
        (firsts, nested, Some("1"), false),
        (constant, "80".to_string(), None, false),
        (list, "80".to_string(), None, false),
        (lets, "ff0180".to_string(), None, true),
    ] {
        let mut allocator = Allocator::new();
        let bytes = hex::decode(hex).unwrap();
        let ptr = clvmr::serde::node_from_bytes(&mut allocator, &bytes).unwrap();
        let explanation = Explanation::new(&bytes, None).unwrap();
        assert_eq!(explanation.uncovered(), 0);
        assert!(!explanation.render().is_empty());

        let env =
            clvmr::serde::node_from_bytes(&mut allocator, &hex::decode(solution).unwrap()).unwrap();
        if let Some(output) = output {
            let dialect = ChiaDialect::new(clvmr::ClvmFlags::empty());
            let value = run_program(&mut allocator, &dialect, ptr, env, MAX_COST)
                .unwrap()
                .1;
            assert_eq!(disassemble_value(&allocator, value), output);
        }
        verify(&mut allocator, &explanation, ptr, env).unwrap();

        if nested_too_deeply {
            let rendered = explanation.render();
            assert!(
                rendered.contains("... // nested too deeply to show"),
                "{rendered}"
            );
            assert!(
                rendered.lines().count() < 1_000,
                "{}",
                rendered.lines().count()
            );
        }
    }
}

#[test]
fn function_passed_as_different_hidden_values() {
    // `apply` is passed `add` once and `mul` once, so there's a version of it for each, and the
    // `add` it runs with 99 isn't the `add` that's called directly with 10
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
                [apply(y: solution@2), apply#2(y: solution@2), ...add#3(x: solution@2)]
            }

            fn apply(y) {
                // env@2 is always add
                add#2(x: y)
            }

            fn apply#2(y) {
                // env@2 is always mul
                mul(x: y)
            }

            fn add(x) {
                env@2 + x
            }

            fn add#2(x) {
                // env@2 is always 99
                99 + x
            }

            fn add#3(x) {
                // env@2 is always 10
                10 + x
            }

            fn mul(x) {
                // env@2 is always 99
                99 * x
            }
        "#]],
    );
}

#[test]
fn function_in_the_environment_of_an_unknown_program() {
    // The solution's program is run with `add` in its environment, and runs it with 3, so the
    // value is the version of `add` that knows nothing
    check(
        "(c (a 2 (c (q . (+ 2 5)) ())) (a (q . (+ 2 5)) (c (q . 10) (c 5 ()))))",
        add_hidden,
        &["((a 2 (q 7 3)) 1)"],
        &expect![[r#"
            program(solution) {
                (solution@2(add), add#2(x: solution@5))
            }

            fn add(x) {
                env@2 + x
            }

            fn add#2(x) {
                // env@2 is always 10
                10 + x
            }
        "#]],
    );
}

#[test]
fn statements_in_branches_are_kept() {
    // An `else` that asserts before its `if`
    check(
        "(a (i 2 (q . 1) (q 2 (i 5 (q 2 (i 11 (q . 1) (q . 2)) 1) (q 8)) 1)) 1)",
        |_| Vec::new(),
        &["(1 2 3)", "(() 2 3)", "(() 2 ())", "(() () 3)"],
        &expect![[r#"
            program(solution) {
                if solution@2 {
                    solution
                } else {
                    assert solution@5
                    if solution@11 {
                        solution
                    } else {
                        solution@2
                    }
                }
            }
        "#]],
    );

    // A branch that binds a value before it raises
    check(
        "(a (i 2 (q . 1) (q 2 (q 8 2) (c (+ 5 5) 1))) 1)",
        |_| Vec::new(),
        &["(1 2)", "(() 2)"],
        &expect![[r#"
            program(solution) {
                if solution@2 {
                    solution
                } else {
                    let v1 = solution@5 + solution@5
                    raise(v1)
                }
            }
        "#]],
    );
}

#[test]
fn raw_code_running_a_function() {
    // The raw code runs `add`'s code with 20, which is shown as it is, and the call that's shown
    // passes 10
    check(
        "(c (100 (a (q . (+ 2 5)) (c (q . 20) (c 2 ())))) (a (q . (+ 2 5)) (c (q . 10) (c 2 ()))))",
        add_hidden,
        &["(1)"],
        &expect![[r#"
            program(solution) {
                (clvm!((100 (a (q 16 2 5) (c (q . 20) (c 2 ()))))), add(x: solution@2))
            }

            fn add(x) {
                // env@2 is always 10
                10 + x
            }
        "#]],
    );
}

#[test]
fn reading_inside_a_hidden_value() {
    // The body reads the first of its hidden value, which is a different constant in each call
    check(
        "(+ (a (q . (+ 4 5)) (c (q . (7)) (c 2 ()))) (a (q . (+ 4 5)) (c (q . (8)) (c 2 ()))))",
        |builder| {
            vec![(
                "(+ 4 5)".to_string(),
                Described {
                    name: "first",
                    parameters: pair(Parameter::Nil, pair(name(builder, "x", 5), Parameter::Nil)),
                },
            )]
        },
        &["(1)"],
        &expect![[r#"
            program(solution) {
                first(x: solution@2) + first#2(x: solution@2)
            }

            fn first(x) {
                // env@2 is always [7]
                7 + x
            }

            fn first#2(x) {
                // env@2 is always [8]
                8 + x
            }
        "#]],
    );
}

#[test]
fn environment_of_an_if_is_evaluated_before_its_condition() {
    check(
        "(a (i (a (q . 2) (c (x (q . 1)) 1)) (q . 1) ()) (c (x (q . 2)) 1))",
        |_| Vec::new(),
        &["()"],
        &expect![[r#"
            program(solution) {
                let v1 = raise(2)
                let v2 = raise(1)
                if v2 {
                    (v1, solution)
                } else {
                    nil
                }
            }
        "#]],
    );
}

/// Explains the program without a sidecar, checks it with the solution, and returns how long its
/// pseudocode is.
fn rendered_length(code: &str, solution: &str) -> usize {
    let mut allocator = Allocator::new();
    let ptr = assemble(&mut allocator, code).unwrap();
    let bytes = node_to_bytes(&allocator, ptr).unwrap();
    let explanation = Explanation::new(&bytes, None).unwrap();
    assert_eq!(explanation.uncovered(), 0);
    let env = assemble(&mut allocator, solution).unwrap();
    verify(&mut allocator, &explanation, ptr, env).unwrap();
    explanation.render().len()
}

#[test]
fn structures_built_from_copies_of_themselves() {
    // `(a (q . (a (q . ... (l 2)) (c (c 2 2) 1))) (c (c 2 2) 1))`, which doubles the first value of
    // the environment, is shown with a variable for each large part, rather than in full. A few
    // doublings would be large already, and many would take exponential time
    let doublings = |count: usize| {
        let mut code = "(l 2)".to_string();
        for _ in 0..count {
            code = format!("(a (q . {code}) (c (c 2 2) 1))");
        }
        code
    };
    assert!(rendered_length(&doublings(16), "(1)") < 10_000);
    assert!(rendered_length(&doublings(40), "(1)") < 50_000);

    // A structure of parts of the variable before it, each of which is a large structure, many
    // times over
    let tree = |leaf: &str| {
        let mut tree = leaf.to_string();
        for _ in 0..6 {
            tree = format!("(c {tree} {tree})");
        }
        format!("(c {tree} {leaf})")
    };
    let mut code = "(l 2)".to_string();
    for _ in 0..20 {
        code = format!("(a (q . {code}) (c 2 1))");
        code = format!("(a (q . {code}) (c {} 1))", tree("4"));
    }
    code = format!("(a (q . {code}) (c 2 1))");
    code = format!("(a (q . {code}) (c {} 1))", tree("2"));
    assert!(rendered_length(&code, "(1)") < 200_000);
}

#[test]
fn functions_in_bound_structures() {
    // A structure that's too large to show where it's used, with `add` at its start
    let mut large = "()".to_string();
    for _ in 0..70 {
        large = format!("(c (q . 1) {large})");
    }
    let large = format!("(c (q . (+ 2 5)) {large})");

    // `add` is called through the variable that the structure is bound to, which sees through to
    // it, and the structure is returned, which has the code of `add` in it
    check(
        &format!("(a (q . (c (a 4 (c (q . 10) (c 3 ()))) 2)) (c {large} 1))"),
        add_hidden,
        &["(1)"],
        &expect![[r#"
            program(solution) {
                let v1 = [
                    add,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                ]
                (add#2(x: solution), v1)
            }

            fn add(x) {
                env@2 + x
            }

            fn add#2(x) {
                // env@2 is always 10
                10 + x
            }
        "#]],
    );

    // The same, with the structure read through another variable first
    check(
        &format!("(a (q . (a (q . (c (a 4 (c (q . 10) (c 3 ()))) 2)) (c 2 1))) (c {large} 1))"),
        add_hidden,
        &["(1)"],
        &expect![[r#"
            program(solution) {
                let v1 = (
                    [
                        add,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                        1,
                    ],
                    solution,
                )
                let v2 = [
                    add,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                    1,
                ]
                (add#2(x: v1), v2)
            }

            fn add(x) {
                env@2 + x
            }

            fn add#2(x) {
                // env@2 is always 10
                10 + x
            }
        "#]],
    );
}

#[test]
fn closures_with_a_quoted_nil_capture_are_lists() {
    // `(q)` and `()` capture nil in the same way, but aren't the same value, so only the second is
    // shown as a closure
    let closure = |capture: &str| {
        format!("(c (q . 2) (c (q 1 + 2 5) (c (c (q . 4) (c {capture} (c (q . 1) ()))) ())))")
    };
    check(
        &format!("(c {} {})", closure("(q 1)"), closure("()")),
        add_hidden,
        &["()"],
        &expect![[r#"
            program(solution) {
                ([2, clvm!(q . (q 16 2 5)), [4, [1], 1]], add{env@2: nil})
            }

            fn add(x) {
                env@2 + x
            }
        "#]],
    );
}

#[test]
fn check_reports_differences() {
    let mut allocator = Allocator::new();
    let dialect = ChiaDialect::new(clvmr::ClvmFlags::empty());
    let explain = |allocator: &mut Allocator, program: &str| {
        let ptr = assemble(allocator, program).unwrap();
        Explanation::new(&node_to_bytes(allocator, ptr).unwrap(), None).unwrap()
    };

    let env = assemble(&mut allocator, "(1)").unwrap();
    let mut results = Vec::new();
    for program in ["(x (q . 1))", "(x (q . 2))", "(f 2)", "(q . 1)", "(q . 2)"] {
        let ptr = assemble(&mut allocator, program).unwrap();
        let result = run_program(&mut allocator, &dialect, ptr, env, MAX_COST);
        results.push((program, result));
    }

    // Each program matches itself, and none matches another
    for (program, _) in &results {
        let explanation = explain(&mut allocator, program);
        for (other, result) in &results {
            let checked = explanation.check(&mut allocator, &dialect, result, env, MAX_COST);
            assert_eq!(
                checked.is_ok(),
                program == other,
                "{program} against {other}"
            );
        }
    }

    // Running out of cost isn't the same as giving the result, but nothing is compared if the
    // program itself ran out
    let explanation = explain(
        &mut allocator,
        "(a (q 2 2 (c 2 ())) (c (q 2 2 (c 2 ())) ()))",
    );
    let one = allocator.one();
    let error = explanation
        .check(&mut allocator, &dialect, &Ok(Reduction(0, one)), env, 1_000)
        .unwrap_err();
    assert!(error.contains("ran out of cost"), "{error}");
    assert!(
        explanation
            .check(
                &mut allocator,
                &dialect,
                &Err(EvalErr::CostExceeded),
                env,
                1_000
            )
            .is_ok()
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

/// A function `apply(f, x)`, which runs `f` with 20 as its hidden value and `x`.
fn apply_hidden(builder: &mut DebugMetadataBuilder) -> Vec<(String, Described)> {
    let mut functions = add_hidden(builder);
    functions.push((
        "(a 2 (c (q . 20) (c 5 ())))".to_string(),
        Described {
            name: "apply",
            parameters: pair(
                name(builder, "f", 2),
                pair(name(builder, "x", 5), Parameter::Nil),
            ),
        },
    ));
    functions
}

#[test]
fn hidden_values_are_only_known_in_calls_that_pass_them() {
    // `add` is called with 10, and also passed as an argument to `apply`, which runs it with 20
    check(
        "(+ (a (q . (+ 2 5)) (c (q . 10) (c 2 ()))) (a (q . (a 2 (c (q . 20) (c 5 ())))) (c (q . (+ 2 5)) (c 2 ()))))",
        apply_hidden,
        &["(1)"],
        &expect![[r#"
            program(solution) {
                add#2(x: solution@2) + apply(f: add, x: solution@2)
            }

            fn add(x) {
                env@2 + x
            }

            fn add#2(x) {
                // env@2 is always 10
                10 + x
            }

            fn apply(f, x) {
                f(20, x)
            }
        "#]],
    );

    // An anonymous program is passed `add`, and runs it with 20
    check(
        "(+ (a (q . (+ 2 5)) (c (q . 10) (c 2 ()))) (a (q . (a 2 (c 5 (c 11 ())))) (c (q . (a 2 (c (q . 20) (c 5 ())))) (c (q . (+ 2 5)) (c 2 ())))))",
        add_hidden,
        &["(1)"],
        &expect![[r#"
            program(solution) {
                add#2(x: solution@2) + anonymous#1(env: [add, solution@2])
            }

            fn add(x) {
                env@2 + x
            }

            fn add#2(x) {
                // env@2 is always 10
                10 + x
            }

            fn anonymous#1(env) {
                env@2(20, env@5)
            }
        "#]],
    );

    // `g` passes its own hidden value to `add`, and is called with 10 and with 20
    check(
        "(+ (a (q . (+ 2 5)) (c (q . 10) (c 2 ()))) (+ (a (q . (a (q . (+ 2 5)) (c 2 (c 5 ())))) (c (q . 10) (c 2 ()))) (a (q . (a (q . (+ 2 5)) (c 2 (c 5 ())))) (c (q . 20) (c 2 ())))))",
        |builder| {
            let mut functions = add_hidden(builder);
            functions.push((
                "(a (q 16 2 5) (c 2 (c 5 ())))".to_string(),
                Described {
                    name: "g",
                    parameters: pair(Parameter::Nil, pair(name(builder, "y", 5), Parameter::Nil)),
                },
            ));
            functions
        },
        &["(1)"],
        &expect![[r#"
            program(solution) {
                add(x: solution@2) + (g(y: solution@2) + g#2(y: solution@2))
            }

            fn add(x) {
                // env@2 is always 10
                10 + x
            }

            fn add#2(x) {
                // env@2 is always 20
                20 + x
            }

            fn g(y) {
                // env@2 is always 10
                add(x: y)
            }

            fn g#2(y) {
                // env@2 is always 20
                add#2(x: y)
            }
        "#]],
    );
}

#[test]
fn versions_are_limited() {
    // `add` is called with ten different constants, and there are only versions for eight of them
    // (the last eight, since operands are decompiled from the last to the first), so the other
    // calls are to the version that knows nothing
    let calls = (1..=10)
        .map(|value| format!("(a (q . (+ 2 5)) (c (q . {value}) (c 2 ())))"))
        .collect::<Vec<_>>();
    check(
        &format!("(+ {})", calls.join(" ")),
        add_hidden,
        &["(1)"],
        &expect![[r#"
            program(solution) {
                add(x: solution@2, env@2: 1)
                    + add(x: solution@2, env@2: 2)
                    + add#2(x: solution@2)
                    + add#3(x: solution@2)
                    + add#4(x: solution@2)
                    + add#5(x: solution@2)
                    + add#6(x: solution@2)
                    + add#7(x: solution@2)
                    + add#8(x: solution@2)
                    + add#9(x: solution@2)
            }

            fn add(x) {
                env@2 + x
            }

            fn add#2(x) {
                // env@2 is always 3
                3 + x
            }

            fn add#3(x) {
                // env@2 is always 4
                4 + x
            }

            fn add#4(x) {
                // env@2 is always 5
                5 + x
            }

            fn add#5(x) {
                // env@2 is always 6
                6 + x
            }

            fn add#6(x) {
                // env@2 is always 7
                7 + x
            }

            fn add#7(x) {
                // env@2 is always 8
                8 + x
            }

            fn add#8(x) {
                // env@2 is always 9
                9 + x
            }

            fn add#9(x) {
                // env@2 is always 10
                10 + x
            }
        "#]],
    );
}

#[test]
fn functions_are_named_apart_from_values() {
    // A function named like a parameter of another function, and one named like a variable, and a
    // parameter named like a variable
    check(
        "(c (a (q . (+ 2 5)) (c (q . 10) (c 2 ()))) (c (a (q . (a (q . (* 2 2)) (c (+ 2 (q . 1)) ()))) (c 2 ())) (a (q . (* 2 (q . 3))) (c 2 ()))))",
        |builder| {
            let mut functions = add_hidden(builder);
            functions.push((
                "(a (q 18 2 2) (c (+ 2 (q . 1)) ()))".to_string(),
                Described {
                    name: "x",
                    parameters: pair(name(builder, "v1", 2), Parameter::Nil),
                },
            ));
            functions.push((
                "(* 2 (q . 3))".to_string(),
                Described {
                    name: "v1",
                    parameters: pair(name(builder, "y", 2), Parameter::Nil),
                },
            ));
            functions
        },
        &["(1)"],
        &expect![[r#"
            program(solution) {
                [add(x: solution@2), x#2(v1: solution@2), ...v1#2(y: solution@2)]
            }

            fn add(x) {
                // env@2 is always 10
                10 + x
            }

            fn x#2(v1) {
                let v2 = v1 + 1
                v2 * v2
            }

            fn v1#2(y) {
                y * 3
            }
        "#]],
    );
}

#[test]
fn large_values_are_shown_once() {
    // A large constant that's used three times, and a value read from a long path that's used
    // three times
    let large = format!("0x{}", "ab".repeat(100));
    check(
        &format!("(c (q . {large}) (c (q . {large}) (sha256 (q . {large}))))"),
        |_| Vec::new(),
        &["()"],
        &expect![[r#"
            const C1 = 0xabababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab

            program(solution) {
                [C1, C1, ...sha256(C1)]
            }
        "#]],
    );
    check(
        "(a (q . (+ 1 1 1)) 0x7fffffffffffffffffff)",
        |_| Vec::new(),
        &["()"],
        &expect![[r#"
            program(solution) {
                let v1 = solution@604462909807314587353087
                v1 + v1 + v1
            }
        "#]],
    );
}

/// A function `f(_, x)` with the body.
fn hidden_first(
    body: &'static str,
) -> impl FnOnce(&mut DebugMetadataBuilder) -> Vec<(String, Described)> {
    move |builder| {
        vec![(
            body.to_string(),
            Described {
                name: "f",
                parameters: pair(Parameter::Nil, pair(name(builder, "x", 5), Parameter::Nil)),
            },
        )]
    }
}

#[test]
fn calls_show_the_hidden_values_that_are_used() {
    // The body reads a part of its hidden value
    check(
        "(a (q . (+ 4 5)) (c 2 (c 5 ())))",
        hidden_first("(+ 4 5)"),
        &["((7) 8)"],
        &expect![[r#"
            program(solution) {
                f(x: solution@5, env@2: solution@2)
            }

            fn f(x) {
                env@4 + x
            }
        "#]],
    );

    // The body runs raw code with its whole environment
    check(
        "(a (q . (100 1)) (c 2 (c 5 ())))",
        hidden_first("(100 1)"),
        &["(7 8)"],
        &expect![[r#"
            program(solution) {
                f(x: solution@5, env@2: solution@2)
            }

            fn f(x) {
                clvm!((100 1))
            }
        "#]],
    );

    // The body runs a program with its whole environment
    check(
        "(a (q . (a 5 1)) (c 2 (c 5 ())))",
        hidden_first("(a 5 1)"),
        &["(7 (q . 9))"],
        &expect![[r#"
            program(solution) {
                f(x: solution@5, env@2: solution@2)
            }

            fn f(x) {
                x(...env)
            }
        "#]],
    );

    // The body doesn't use its hidden value, which isn't computed, so it isn't shown
    check(
        "(a (q . (+ 5 (q . 1))) (c 2 (c 5 ())))",
        hidden_first("(+ 5 (q . 1))"),
        &["(7 8)"],
        &expect![[r#"
            program(solution) {
                f(x: solution@5)
            }

            fn f(x) {
                x + 1
            }
        "#]],
    );
}

#[test]
fn versions_are_for_the_constants_of_each_part() {
    // `g` has two hidden values, and each call passes 7 for a different one
    check(
        "(c (a (q . (+ 2 5 11)) (c (q . 7) (c 2 (c 5 ())))) (a (q . (+ 2 5 11)) (c 2 (c (q . 7) (c 5 ())))))",
        |builder| {
            vec![(
                "(+ 2 5 11)".to_string(),
                Described {
                    name: "g",
                    parameters: pair(
                        Parameter::Nil,
                        pair(Parameter::Nil, pair(name(builder, "x", 11), Parameter::Nil)),
                    ),
                },
            )]
        },
        &["(1 2)"],
        &expect![[r#"
            program(solution) {
                (g(x: solution@5, env@5: solution@2), g#2(x: solution@5, env@2: solution@2))
            }

            fn g(x) {
                // env@2 is always 7
                7 + env@5 + x
            }

            fn g#2(x) {
                // env@5 is always 7
                env@2 + 7 + x
            }
        "#]],
    );
}

#[test]
fn functions_are_named_apart_from_generated_names() {
    // Functions named like a constant, a variable and the environment, none of which are names
    // that are used here
    let large = format!("0x{}", "ab".repeat(100));
    check(
        &format!(
            "(c (a (q . (c 2 (q . {large}))) (c (q . {large}) ())) (c (a (q . (* 2 (q . 3))) (c 2 ())) (a (q . (* 2 (q . 4))) (c 2 ()))))"
        ),
        |builder| {
            vec![
                (
                    format!("(c 2 (q . {large}))"),
                    Described {
                        name: "C1",
                        parameters: pair(name(builder, "y", 2), Parameter::Nil),
                    },
                ),
                (
                    "(* 2 (q . 3))".to_string(),
                    Described {
                        name: "v7",
                        parameters: pair(name(builder, "y", 2), Parameter::Nil),
                    },
                ),
                (
                    "(* 2 (q . 4))".to_string(),
                    Described {
                        name: "env",
                        parameters: pair(name(builder, "y", 2), Parameter::Nil),
                    },
                ),
            ]
        },
        &["(1)"],
        &expect![[r#"
            const C1 = 0xabababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab

            program(solution) {
                [C1#2(y: C1), v7#2(y: solution@2), ...env#2(y: solution@2)]
            }

            fn C1#2(y) {
                (y, C1)
            }

            fn v7#2(y) {
                y * 3
            }

            fn env#2(y) {
                y * 4
            }
        "#]],
    );
}

#[test]
fn computed_environments_across_parameters_are_bound() {
    // The environment is computed, and has both parameters in it, so it's bound first and each
    // parameter is shown as a part of it
    check(
        "(a (q . (+ 2 5)) (f 1))",
        |builder| {
            vec![(
                "(+ 2 5)".to_string(),
                Described {
                    name: "add",
                    parameters: pair(
                        name(builder, "a", 2),
                        pair(name(builder, "b", 5), Parameter::Nil),
                    ),
                },
            )]
        },
        &["((1 2))"],
        &expect![[r#"
            program(solution) {
                let v1 = solution.first
                add(a: v1.first, b: v1.rest.first)
            }

            fn add(a, b) {
                a + b
            }
        "#]],
    );
}

#[test]
fn functions_only_in_raw_code_are_shown() {
    // `add` is never called or used as a value, but it's in raw code
    check(
        "(c (100 (q . (+ 2 5))) (q . 1))",
        add_hidden,
        &["(1)"],
        &expect![[r#"
            program(solution) {
                (clvm!((100 (q 16 2 5))), 1)
            }

            fn add(x) {
                env@2 + x
            }
        "#]],
    );
}
