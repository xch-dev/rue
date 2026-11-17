use std::cell::Cell;
use std::collections::VecDeque;
use std::time::Duration;

use super::*;
use crate::generate::{legalize, parse_ty, removable_sites};
use crate::oracle::{Category, Compiled, Diag, Outcome, Run, classify, compile};
use crate::program::{Expr, ExprKind, Function, Let, Program, Strip, Ty};
use crate::report::cluster_key;
use crate::worker::{Connection, Explicit, Message};

#[test]
fn types_print_as_parsed() {
    for text in [
        "Int",
        "(T, T)",
        "T | nil",
        "List<(A, B)>",
        "Box<T | nil>",
        "fn(a: A) -> B",
        "fn(a: A, b: 5) -> (Int | nil)",
        "(fn(a: A) -> B) | nil",
        // A function type's return type is in parentheses when it's a union or a function
        "fn() -> (fn(a: T) -> T)",
    ] {
        let generics = ["A", "B", "T"];
        let ty = parse_ty(text, &generics);
        let names: Vec<String> = generics.iter().map(ToString::to_string).collect();
        assert_eq!(ty.print(&names), text);
    }
}

#[test]
fn generics_are_substituted() {
    let ty = parse_ty("fn(a: A) -> (B, A)", &["A", "B"]);
    assert_eq!(
        ty.subst(&[Ty::Int, Ty::opt(Ty::Lit(5))]).print(&[]),
        "fn(a: Int) -> (5 | nil, Int)"
    );
}

#[test]
fn generation_is_deterministic() {
    for seed in 0..20 {
        let first = Generator::new(seed).generate();
        let second = Generator::new(seed).generate();
        assert_eq!(
            first.program.print(&Variant::default()),
            second.program.print(&Variant::default())
        );
        assert_eq!(first.variants, second.variants);
    }
}

/// The generator is only useful if nearly all of its explicit programs compile, and if removing
/// generic arguments changes them. A broken explicit program is skipped rather than reported,
/// so a regression in the printer or the generator would otherwise only show up as fewer
/// programs checked.
#[test]
fn explicit_programs_compile() {
    let mut valid = 0;
    for seed in 0..40 {
        let generated = Generator::new(seed).generate();
        let explicit = generated.program.print(&Variant::default());
        if matches!(compile(&explicit), Compiled::Ok { .. }) {
            valid += 1;
        }
        for (_, variant) in &generated.variants {
            assert_ne!(generated.program.print(variant), explicit, "seed {seed}");
        }
    }
    assert!(valid >= 38, "only {valid} of 40 explicit programs compile");
}

// Programs built by hand, for the rules of which generic arguments can be removed.

fn function(name: &str, generics: &[&str], params: &[(&str, &str)], ret: &str) -> Function {
    Function {
        name: name.to_string(),
        generics: generics.iter().map(ToString::to_string).collect(),
        params: params
            .iter()
            .map(|(name, ty)| ((*name).to_string(), parse_ty(ty, generics)))
            .collect(),
        ret: parse_ty(ret, generics),
        recursion: None,
        lets: Vec::new(),
        body: Expr::new(ExprKind::Unchecked, parse_ty(ret, generics)),
    }
}

fn call(site: usize, function: &str, generic_args: Vec<Ty>, args: Vec<Expr>, ty: Ty) -> Expr {
    Expr::new(
        ExprKind::Call {
            site: Some(site),
            function: function.to_string(),
            generic_args,
            args,
        },
        ty,
    )
}

fn int(value: i64, ty: Ty) -> Expr {
    Expr::new(ExprKind::Int(value), ty)
}

fn nil(ty: Ty) -> Expr {
    Expr::new(ExprKind::Nil, ty)
}

/// A program whose `main` binds one value, with an optional annotation.
fn program(functions: Vec<Function>, annotation: Option<Ty>, value: Expr) -> Program {
    Program {
        functions,
        main: Function {
            name: "main".to_string(),
            generics: Vec::new(),
            params: Vec::new(),
            ret: Ty::Any,
            recursion: None,
            lets: vec![Let {
                name: "v0".to_string(),
                annotation,
                value,
            }],
            body: Expr::new(ExprKind::Var("v0".to_string()), Ty::Any),
        },
    }
}

fn unwrap() -> Function {
    function("unwrap", &["T"], &[("value", "T | nil")], "T")
}

fn masks(program: &Program) -> Vec<Vec<bool>> {
    removable_sites(program, &Variant::default())
        .into_values()
        .collect()
}

#[test]
fn nil_gives_no_type_to_an_optional_generic() {
    let opt = Ty::opt(Ty::Int);

    let from_nil = call(0, "unwrap", vec![Ty::Int], vec![nil(opt.clone())], Ty::Int);
    assert_eq!(
        masks(&program(vec![unwrap()], None, from_nil.clone())),
        [[false]]
    );

    // The expected type gives it one instead
    assert_eq!(
        masks(&program(vec![unwrap()], Some(Ty::Int), from_nil)),
        [[true]]
    );

    let from_value = call(0, "unwrap", vec![Ty::Int], vec![int(5, opt)], Ty::Int);
    assert_eq!(masks(&program(vec![unwrap()], None, from_value)), [[true]]);
}

#[test]
fn untyped_lambdas_give_no_type_to_their_parameters() {
    let compose = function(
        "compose",
        &["A", "B"],
        &[("function", "fn(a: A) -> B")],
        "fn(a: A) -> B",
    );
    let fn_ty = Ty::func(vec![Ty::Int], Ty::Int);
    let lambda = |typed| {
        Expr::new(
            ExprKind::Lambda {
                params: vec![("x".to_string(), Ty::Int)],
                typed,
                body: Box::new(int(1, Ty::Int)),
            },
            fn_ty.clone(),
        )
    };
    let compose_call = |typed| {
        call(
            0,
            "compose",
            vec![Ty::Int, Ty::Int],
            vec![lambda(typed)],
            fn_ty.clone(),
        )
    };

    assert_eq!(
        masks(&program(vec![compose.clone()], None, compose_call(false))),
        [[false, true]]
    );
    assert_eq!(
        masks(&program(vec![compose], None, compose_call(true))),
        [[true, true]]
    );
}

#[test]
fn struct_literal_arguments_are_kept() {
    let value = Expr::new(
        ExprKind::BoxLit {
            site: 0,
            arg: Ty::Int,
            value: Box::new(int(1, Ty::Int)),
        },
        Ty::boxed(Ty::Int),
    );
    assert_eq!(
        masks(&program(vec![], Some(Ty::boxed(Ty::Int)), value)),
        [[false]]
    );
}

#[test]
fn generics_with_nothing_to_infer_them_from_are_kept_as_placeholders() {
    // `B` is only in the return type, and the binding has no annotation
    let pick = function("pick", &["A", "B"], &[("value", "A")], "B");
    let value = call(
        0,
        "pick",
        vec![Ty::Int, Ty::Bool],
        vec![int(1, Ty::Int)],
        Ty::Bool,
    );
    let program = program(vec![pick], None, value);

    let variant = Variant {
        stripped: [(0, Strip::All)].into(),
    };
    assert_eq!(
        legalize(&program, &variant).stripped,
        [(0, Strip::Placeholders(vec![true, false]))].into()
    );
}

/// `first_of(pairs())`: `pairs`'s `T` comes from the expected type, which is `first_of`'s own
/// `T`, so nothing gives either a type. Keeping `first_of`'s arguments makes `pairs` removable.
#[test]
fn removed_calls_only_give_types_they_infer_from_arguments() {
    let first_of = function("first_of", &["T"], &[("pair", "(T, T)")], "T");
    let pairs = function("pairs", &["T"], &[], "(T, T)");
    let pair = Ty::pair(Ty::Int, Ty::Int);
    let value = call(
        0,
        "first_of",
        vec![Ty::Int],
        vec![call(1, "pairs", vec![Ty::Int], vec![], pair)],
        Ty::Int,
    );
    let program = program(vec![first_of, pairs], None, value);

    // With `pairs`'s arguments written out, `first_of`'s can be removed, and the other way round
    assert_eq!(masks(&program), [[true], [true]]);

    let both = Variant {
        stripped: [(0, Strip::All), (1, Strip::All)].into(),
    };
    assert_eq!(legalize(&program, &both).stripped, [(1, Strip::All)].into());
}

#[test]
fn shrinking_keeps_what_a_removed_generic_is_inferred_from() {
    // Shrinking would replace `maybe(5)` with `nil`, the simplest `Int | nil`, and the program
    // would still fail as long as the call's arguments are removed. But then nothing gives `T` a
    // type.
    let maybe = function("maybe", &[], &[("value", "Int")], "Int | nil");
    let opt = Ty::opt(Ty::Int);
    let argument = Expr::new(
        ExprKind::Call {
            site: None,
            function: "maybe".to_string(),
            generic_args: Vec::new(),
            args: vec![int(5, Ty::Int)],
        },
        opt,
    );
    let value = call(0, "unwrap", vec![Ty::Int], vec![argument], Ty::Int);
    let program = program(vec![unwrap(), maybe], None, value);
    let variant = Variant {
        stripped: [(0, Strip::All)].into(),
    };

    // Fails as long as the call's arguments are removed, and the program still compiles
    let (program, variant) = shrink::shrink(program, variant, 100, &mut |program, variant| {
        program.function("unwrap").is_some()
            && program.function("maybe").is_some()
            && program.exprs().iter().any(|expr| variant.strips(expr))
    });

    assert_eq!(legalize(&program, &variant), variant);
    let source = program.print(&variant);
    assert!(
        source.contains("unwrap(unchecked_cast::<Int | nil>(nil))"),
        "{source}"
    );
}

#[test]
fn shrinks_to_what_the_failure_needs() {
    // Fails while some call to `map` has its generic arguments removed
    let calls_map = |program: &Program, variant: &Variant| {
        program.exprs().iter().any(|expr| {
            matches!(&expr.kind, ExprKind::Call { function, .. } if function == "map")
                && variant.strips(expr)
        })
    };

    let generated = (0..500)
        .map(|seed| Generator::new(seed).generate())
        .find(|generated| {
            generated.program.functions.len() > 3
                && generated
                    .variants
                    .iter()
                    .any(|(_, variant)| calls_map(&generated.program, variant))
        })
        .unwrap();
    let (_, variant) = generated
        .variants
        .iter()
        .find(|(_, variant)| calls_map(&generated.program, variant))
        .unwrap()
        .clone();

    let checks = Cell::new(0);
    let (program, variant) = shrink::shrink(generated.program, variant, 2000, &mut |p, v| {
        checks.set(checks.get() + 1);
        calls_map(p, v)
    });

    assert!(calls_map(&program, &variant));
    assert_eq!(variant.stripped.len(), 1);
    // `map` and whatever its arguments need, and nothing that the call doesn't use
    assert!(program.functions.len() <= 3, "{}", program.print(&variant));
    assert_eq!(program.main.lets.len(), 1, "{}", program.print(&variant));
    // It reaches a fixed point rather than using up the budget
    assert!(checks.get() < 500, "took {} checks", checks.get());
}

#[test]
fn operands_of_fields_are_parenthesized() {
    let boxed = Expr::new(
        ExprKind::BoxLit {
            site: 0,
            arg: Ty::Int,
            value: Box::new(int(1, Ty::Int)),
        },
        Ty::boxed(Ty::Int),
    );
    let field = Expr::new(ExprKind::Field(Box::new(boxed)), Ty::Int);
    let cast = Expr::new(ExprKind::Unchecked, Ty::pair(Ty::Int, Ty::Int));
    let first = Expr::new(ExprKind::First(Box::new(cast)), Ty::Int);
    let value = Expr::new(
        ExprKind::Pair(Box::new(field), Box::new(first)),
        Ty::pair(Ty::Int, Ty::Int),
    );

    let source =
        program(vec![], Some(Ty::pair(Ty::Int, Ty::Int)), value).print(&Variant::default());
    assert!(
        source
            .contains("(Box::<Int> { value: 1 }).value, (unchecked_cast::<(Int, Int)>(nil)).first"),
        "{source}"
    );
    assert!(matches!(compile(&source), Compiled::Ok { .. }), "{source}");
}

// The oracle.

fn classify_sources(explicit: &str, stripped: &str) -> Outcome {
    let explicit = compile(explicit);
    let stripped_compiled = compile(stripped);
    classify(&explicit, &stripped_compiled, || {
        (explicit.clone(), stripped_compiled.clone())
    })
}

#[test]
fn classifies_results() {
    let explicit = "fn main() -> Int {\n    1\n}\n";

    let inferred = classify_sources(explicit, explicit);
    assert_eq!(inferred.category, Category::Inferred);
    assert!(inferred.same_clvm);

    let rejected = classify_sources(explicit, "fn main() -> Int {\n    (1, 2)\n}\n");
    assert_eq!(rejected.category, Category::Rejected);
    let diag = rejected.diag.unwrap();
    assert_eq!(diag.kind, "IncompatibleType");
    assert_eq!(
        diag.template(),
        "Cannot assign `_` to `_`, since they are incompatible"
    );

    let unsound = classify_sources(explicit, "fn main() -> Int {\n    2\n}\n");
    assert_eq!(unsound.category, Category::Unsound);

    let raises = classify_sources(explicit, "fn main() -> Int {\n    raise;\n}\n");
    assert_eq!(raises.category, Category::Unsound);
}

fn ok(clvm: &str, millis: u64) -> Compiled {
    Compiled::Ok {
        clvm: clvm.to_string(),
        run: Run::Output("1".to_string()),
        time: Duration::from_millis(millis),
    }
}

fn rejected(millis: u64) -> Compiled {
    Compiled::Rejected {
        diag: Diag {
            kind: "CannotInferGeneric".to_string(),
            message: "Cannot infer generic `T` for `f`".to_string(),
            line_col: "1:1".to_string(),
        },
        time: Duration::from_millis(millis),
    }
}

#[test]
fn slow_compiles_are_confirmed_by_compiling_again() {
    let explicit = ok("(q . 1)", 1);

    // Slow both times, and a slow rejection keeps its diagnostic
    let slow = classify(&explicit, &rejected(500), || {
        (ok("(q . 1)", 1), rejected(500))
    });
    assert_eq!(slow.category, Category::Slow);
    assert_eq!(slow.diag.unwrap().kind, "CannotInferGeneric");

    // Noise: fast the second time
    let noise = classify(&explicit, &rejected(500), || {
        (ok("(q . 1)", 1), rejected(5))
    });
    assert_eq!(noise.category, Category::Rejected);

    // Below the floor, however many times slower
    let fast = classify(&explicit, &ok("(q . 1)", 200), || unreachable!());
    assert_eq!(fast.category, Category::Inferred);
}

#[test]
fn panics_and_different_clvm_are_classified() {
    let explicit = ok("(q . 1)", 1);

    let panicked = Compiled::Panicked {
        message: "boom".to_string(),
    };
    let crashed = classify(&explicit, &panicked, || unreachable!());
    assert_eq!(crashed.category, Category::Crashed);
    assert_eq!(crashed.detail.as_deref(), Some("boom"));

    let different = classify(&explicit, &ok("(q 1)", 1), || unreachable!());
    assert_eq!(different.category, Category::Inferred);
    assert!(!different.same_clvm);
}

#[test]
fn clusters_are_keyed_by_category_and_diagnostic() {
    let Compiled::Rejected { diag, .. } = rejected(1) else {
        unreachable!()
    };
    let with_diag = Outcome {
        category: Category::Rejected,
        diag: Some(diag),
        same_clvm: false,
        detail: None,
        time_ms: 1.0,
    };
    assert_eq!(
        cluster_key(&with_diag),
        "Rejected: Cannot infer generic `_` for `_` (CannotInferGeneric)"
    );

    let crashed = Outcome::failed(
        Category::Crashed,
        "worker died: boom\nmore".to_string(),
        0.0,
    );
    assert_eq!(cluster_key(&crashed), "Crashed: worker died: boom");

    let timed_out = Outcome::failed(Category::Slow, "timed out after 10 s".to_string(), 1.0);
    assert_eq!(cluster_key(&timed_out), "Slow");
}

#[test]
fn shrinking_keeps_the_generic_and_function_a_rejection_names() {
    let Compiled::Rejected { diag, .. } = rejected(1) else {
        unreachable!()
    };
    let other_function = Diag {
        message: "Cannot infer generic `T` for `g`".to_string(),
        ..diag.clone()
    };
    let other_type = Diag {
        kind: "IncompatibleType".to_string(),
        message: "Cannot assign `Int` to `5`, since they are incompatible".to_string(),
        line_col: "1:1".to_string(),
    };
    let other_types = Diag {
        message: "Cannot assign `Bytes` to `nil`, since they are incompatible".to_string(),
        ..other_type.clone()
    };

    assert!(diag.same_failure(&diag));
    assert!(!diag.same_failure(&other_function));
    assert!(other_type.same_failure(&other_types));
}

// The driver, with a worker that follows a script.

/// Answers each command with the next list of messages, and records the commands and restarts.
struct Script {
    answers: VecDeque<VecDeque<Message>>,
    current: VecDeque<Message>,
    commands: Vec<String>,
    restarts: usize,
}

impl Script {
    fn new(answers: Vec<Vec<Message>>) -> Self {
        Self {
            answers: answers.into_iter().map(VecDeque::from).collect(),
            current: VecDeque::new(),
            commands: Vec::new(),
            restarts: 0,
        }
    }
}

impl Connection for Script {
    fn send(&mut self, command: &str) -> anyhow::Result<()> {
        self.commands.push(command.to_string());
        self.current = self.answers.pop_front().expect("no answer for the command");
        Ok(())
    }

    fn recv(&mut self, _timeout: Duration) -> Message {
        self.current.pop_front().expect("the script ran out")
    }

    fn restart(&mut self) -> anyhow::Result<()> {
        self.restarts += 1;
        Ok(())
    }
}

fn line(text: &str) -> Message {
    Message::Line(text.to_string())
}

fn valid() -> Message {
    line(&format!(
        "explicit {}",
        serde_json::to_string(&Explicit::Valid { time_ms: 1.0 }).unwrap()
    ))
}

fn variant_line(index: usize) -> Message {
    let outcome = classify(&ok("(q . 1)", 1), &ok("(q . 1)", 1), || unreachable!());
    line(&format!(
        "variant {}",
        serde_json::to_string(&(index, outcome)).unwrap()
    ))
}

#[test]
fn hung_and_dead_workers_are_recorded_and_the_rest_resumed() {
    let (seed, generated) = (0..100)
        .map(|seed| (seed, Generator::new(seed).generate()))
        .find(|(_, generated)| generated.variants.len() == 3)
        .unwrap();

    let mut script = Script::new(vec![
        vec![
            line("progress explicit"),
            valid(),
            line("progress 0"),
            Message::TimedOut,
        ],
        vec![
            line("progress explicit"),
            valid(),
            line("progress 1"),
            Message::Died("boom".to_string()),
        ],
        vec![
            line("progress explicit"),
            valid(),
            line("progress 2"),
            variant_line(2),
            line("done"),
        ],
    ]);

    let record = run_seed(&mut script, seed, &generated, Duration::from_secs(10)).unwrap();

    assert_eq!(
        script.commands,
        [
            format!("run {seed} 0"),
            format!("run {seed} 1"),
            format!("run {seed} 2")
        ]
    );
    assert_eq!(script.restarts, 2);
    let categories: Vec<Category> = record
        .variants
        .iter()
        .map(|variant| variant.outcome.category)
        .collect();
    assert_eq!(
        categories,
        [Category::Slow, Category::Crashed, Category::Inferred]
    );
    assert_eq!(
        record.variants[1].outcome.detail.as_deref(),
        Some("worker died: boom")
    );
}

#[test]
fn a_worker_that_dies_at_once_dies_on_the_explicit_program() {
    let generated = Generator::new(0).generate();
    let mut script = Script::new(vec![vec![Message::Died("startup".to_string())]]);

    let record = run_seed(&mut script, 0, &generated, Duration::from_secs(10)).unwrap();

    assert_eq!(record.explicit, Explicit::Died("startup".to_string()));
    assert!(record.variants.is_empty());
    assert_eq!(script.restarts, 1);
}
