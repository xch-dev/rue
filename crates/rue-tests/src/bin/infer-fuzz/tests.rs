use std::cell::Cell;
use std::collections::VecDeque;
use std::sync::Arc;
use std::time::Duration;

use super::*;
use crate::generate::{legalize, parse_ty, removable_sites};
use rue_diagnostic::{Diagnostic, DiagnosticKind, DiagnosticSeverity, Source, SourceKind, SrcLoc};

use crate::oracle::{self, Category, Compiled, Diag, Outcome, Run, classify, compile};
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
        probes: std::collections::BTreeSet::new(),
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
fn nil_gives_no_type() {
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

    // Nor does a call whose type is `nil`, nor `nil` for a bare generic
    let inner = call(
        1,
        "unwrap",
        vec![Ty::Nil],
        vec![nil(Ty::opt(Ty::Nil))],
        Ty::Nil,
    );
    let from_nil_call = call(0, "unwrap", vec![Ty::Int], vec![inner], Ty::Int);
    assert_eq!(
        removable_sites(
            &program(vec![unwrap()], None, from_nil_call),
            &Variant::default()
        )[&0],
        [false]
    );

    let either = function("either", &["T"], &[("first", "T"), ("second", "T")], "T");
    let nils = call(
        0,
        "either",
        vec![Ty::Nil],
        vec![nil(Ty::Nil), nil(Ty::Nil)],
        Ty::Nil,
    );
    assert_eq!(masks(&program(vec![either], None, nils)), [[false]]);

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

fn lambda(params: &[(&str, Ty)], typed: bool, body: Expr, ty: Ty) -> Expr {
    Expr::new(
        ExprKind::Lambda {
            params: params
                .iter()
                .map(|(name, ty)| ((*name).to_string(), ty.clone()))
                .collect(),
            typed,
            body: Box::new(body),
        },
        ty,
    )
}

#[test]
fn lambdas_give_their_return_type_only_what_their_body_does() {
    let produce = function("produce", &["T"], &[("make", "fn() -> T")], "T");
    let list = Ty::list(Ty::Int);
    let made = |body: Expr| {
        let ty = body.ty.clone();
        let make = lambda(&[], false, body, Ty::func(vec![], ty.clone()));
        call(0, "produce", vec![ty.clone()], vec![make], ty)
    };

    let from_nil = made(nil(list.clone()));
    assert_eq!(
        masks(&program(vec![produce.clone()], None, from_nil)),
        [[false]]
    );

    let items = Expr::new(ExprKind::List(vec![int(1, Ty::Int)]), list);
    assert_eq!(
        masks(&program(vec![produce.clone()], None, made(items))),
        [[true]]
    );

    // An untyped lambda's body is only looked at once the call gives its parameters a type:
    // `5` gives `A` one, and then `x` gives `B` one. `nil` gives `A` none, so `B` can only be
    // removed if `A` is kept.
    let map = function(
        "map",
        &["A", "B"],
        &[("function", "fn(a: A) -> B"), ("value", "A")],
        "B",
    );
    let fn_ty = Ty::func(vec![Ty::Int], Ty::Int);
    let mapped = |value: Expr| {
        let echo = lambda(
            &[("x", Ty::Int)],
            false,
            Expr::new(ExprKind::Var("x".to_string()), Ty::Int),
            fn_ty.clone(),
        );
        call(0, "map", vec![Ty::Int, Ty::Int], vec![echo, value], Ty::Int)
    };
    assert_eq!(
        masks(&program(vec![map.clone()], None, mapped(int(5, Ty::Int)))),
        [[true, true]]
    );
    let from_nil = program(vec![map], None, mapped(nil(Ty::opt(Ty::Int))));
    assert_eq!(masks(&from_nil), [[false, true]]);
    assert_eq!(
        legalize(&from_nil, &strip_all(&[0])).stripped,
        [(0, Strip::Placeholders(vec![false, true]))].into()
    );
}

/// `pick(fn(x: Int | nil) => nil)` infers `T` as `nil` from the body, despite the annotation,
/// so it gives `identity`'s `T` nothing. On its own, the annotation gives `pick`'s `T` a type.
#[test]
fn annotations_give_nested_calls_no_type() {
    let pick = function("pick", &["T"], &[("function", "fn(a: T) -> T")], "T");
    let identity = function("identity", &["T"], &[("value", "T")], "T");
    let opt = Ty::opt(Ty::Int);
    let typed = lambda(
        &[("x", opt.clone())],
        true,
        nil(opt.clone()),
        Ty::func(vec![opt.clone()], opt.clone()),
    );
    let picked = call(1, "pick", vec![opt.clone()], vec![typed], opt.clone());

    let alone = program(vec![pick.clone()], None, picked.clone());
    assert_eq!(masks(&alone), [[true]]);

    let nested = call(0, "identity", vec![opt.clone()], vec![picked], opt);
    let nested = program(vec![pick, identity], None, nested);
    assert_eq!(legalize(&nested, &strip_all(&[0, 1])), strip_all(&[1]));
}

#[test]
fn lambdas_give_a_bare_generic_no_type() {
    let identity = function("identity", &["T"], &[("value", "T")], "T");
    let fn_ty = Ty::func(vec![Ty::Int], Ty::Int);
    let untyped = || lambda(&[("x", Ty::Int)], false, int(1, Ty::Int), fn_ty.clone());

    let alone = call(
        0,
        "identity",
        vec![fn_ty.clone()],
        vec![untyped()],
        fn_ty.clone(),
    );
    assert_eq!(
        masks(&program(vec![identity.clone()], None, alone)),
        [[false]]
    );

    // Nor does a pair with one in it
    let pair_ty = Ty::pair(fn_ty.clone(), Ty::Int);
    let pair = Expr::new(
        ExprKind::Pair(Box::new(untyped()), Box::new(int(5, Ty::Int))),
        pair_ty.clone(),
    );
    let in_pair = call(0, "identity", vec![pair_ty.clone()], vec![pair], pair_ty);
    assert_eq!(masks(&program(vec![identity], None, in_pair)), [[false]]);
}

#[test]
fn generic_lambdas_get_types_through_the_call() {
    let fn_ty = Ty::func(vec![Ty::Int], Ty::Int);
    let generic_lambda = Expr::new(ExprKind::IdentityLambda, fn_ty.clone());

    let g = function("g", &["A"], &[("function", "fn(a: A) -> A")], "Int");
    let alone = call(0, "g", vec![Ty::Int], vec![generic_lambda.clone()], Ty::Int);
    assert_eq!(masks(&program(vec![g], None, alone)), [[false]]);

    let map = function(
        "map",
        &["A", "B"],
        &[("function", "fn(a: A) -> B"), ("value", "A")],
        "B",
    );
    let with_value = call(
        0,
        "map",
        vec![Ty::Int, Ty::Int],
        vec![generic_lambda, int(42, Ty::Int)],
        Ty::Int,
    );
    assert_eq!(masks(&program(vec![map], None, with_value)), [[true, true]]);
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

fn fn_ref(site: usize, function: &str, generic_args: Vec<Ty>, ty: Ty) -> Expr {
    Expr::new(
        ExprKind::FnRef {
            site: Some(site),
            function: function.to_string(),
            generic_args,
        },
        ty,
    )
}

fn strip_all(sites: &[usize]) -> Variant {
    Variant {
        stripped: sites.iter().map(|&site| (site, Strip::All)).collect(),
    }
}

#[test]
fn function_references_need_an_expected_type() {
    let duplicate = function("duplicate", &["T"], &[("value", "T")], "(T, T)");
    let ty = Ty::func(vec![Ty::Int], Ty::pair(Ty::Int, Ty::Int));
    let reference = fn_ref(0, "duplicate", vec![Ty::Int], ty.clone());

    assert_eq!(
        masks(&program(vec![duplicate.clone()], None, reference.clone())),
        [[false]]
    );
    assert_eq!(
        masks(&program(vec![duplicate], Some(ty), reference)),
        [[true]]
    );
}

/// In `map(duplicate, 42)`, `42` gives `A` a type, which gives `duplicate`'s `T` one, and so
/// `B` gets one. In `g(identity)`, nothing gives `A` a type, so only one of the two can have its
/// arguments removed.
#[test]
fn function_references_get_types_through_the_call() {
    let map = function(
        "map",
        &["A", "B"],
        &[("function", "fn(a: A) -> B"), ("value", "A")],
        "B",
    );
    let duplicate = function("duplicate", &["T"], &[("value", "T")], "(T, T)");
    let pair = Ty::pair(Ty::Int, Ty::Int);
    let value = call(
        1,
        "map",
        vec![Ty::Int, pair.clone()],
        vec![
            fn_ref(
                0,
                "duplicate",
                vec![Ty::Int],
                Ty::func(vec![Ty::Int], pair.clone()),
            ),
            int(42, Ty::Int),
        ],
        pair,
    );
    let mapped = program(vec![map, duplicate], None, value);
    assert_eq!(legalize(&mapped, &strip_all(&[0, 1])), strip_all(&[0, 1]));

    let identity = function("identity", &["T"], &[("value", "T")], "T");
    let g = function("g", &["A"], &[("function", "fn(a: A) -> A")], "Int");
    let fn_ty = Ty::func(vec![Ty::Int], Ty::Int);
    let value = call(
        1,
        "g",
        vec![Ty::Int],
        vec![fn_ref(0, "identity", vec![Ty::Int], fn_ty)],
        Ty::Int,
    );
    let passed = program(vec![identity, g], None, value);
    assert_eq!(legalize(&passed, &strip_all(&[0, 1])), strip_all(&[1]));
}

#[test]
fn lambda_bodies_and_unions_of_functions_give_no_expected_type() {
    let make = function("make", &["A", "B"], &[], "fn(a: A) -> B");
    let fn_ty = Ty::func(vec![Ty::Int], Ty::Int);
    let made = |ty: Ty| call(0, "make", vec![Ty::Int, Ty::Int], vec![], ty);

    assert_eq!(
        masks(&program(
            vec![make.clone()],
            Some(fn_ty.clone()),
            made(fn_ty.clone())
        )),
        [[true, true]]
    );

    // `F | nil`, whose value is the call itself
    let opt = Ty::opt(fn_ty.clone());
    assert_eq!(
        masks(&program(vec![make.clone()], Some(opt.clone()), made(opt))),
        [[false, false]]
    );

    // `List<F>`'s items
    let list = Ty::list(fn_ty.clone());
    let items = Expr::new(ExprKind::List(vec![made(fn_ty.clone())]), list.clone());
    assert_eq!(
        masks(&program(vec![make.clone()], Some(list), items)),
        [[false, false]]
    );

    // A lambda's body
    let outer = Ty::func(vec![Ty::Int], fn_ty.clone());
    let lambda = Expr::new(
        ExprKind::Lambda {
            params: vec![("x".to_string(), Ty::Int)],
            typed: true,
            body: Box::new(made(fn_ty)),
        },
        outer.clone(),
    );
    assert_eq!(
        masks(&program(vec![make], Some(outer), lambda)),
        [[false, false]]
    );
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
fn shrinking_leaves_simple_pairs_alone() {
    // Replacing `(1, 2)` with the simplest `(Int, Int)` gives the same pair, which must not
    // count as progress
    let mut f = function("f", &[], &[], "(Int, Int)");
    f.body = Expr::new(
        ExprKind::Pair(Box::new(int(1, Ty::Int)), Box::new(int(2, Ty::Int))),
        Ty::pair(Ty::Int, Ty::Int),
    );
    let program = program(vec![f], None, nil(Ty::Nil));

    let checks = Cell::new(0);
    shrink::shrink(program, Variant::default(), 1000, &mut |program, _| {
        checks.set(checks.get() + 1);
        program.function("f").is_some()
    });
    assert!(checks.get() < 20, "took {} checks", checks.get());
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
        fingerprint: String::new(),
        leak: None,
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
        fingerprint: String::new(),
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
        output: None,
        fingerprint: None,
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

#[test]
fn unsound_and_crashed_failures_must_stay_so_while_shrinking() {
    let expected = Run::Output("1".to_string());

    let unsound = Outcome::failed(Category::Unsound, String::new(), 1.0);
    assert!(!worker::same_failure(
        &unsound,
        &expected,
        &ok("(q . 1)", 1)
    ));
    let different = Compiled::Ok {
        clvm: "(q . 2)".to_string(),
        run: Run::Output("2".to_string()),
        time: Duration::from_millis(1),
        fingerprint: String::new(),
        leak: None,
    };
    assert!(worker::same_failure(&unsound, &expected, &different));

    let crashed = Outcome::failed(Category::Crashed, "boom".to_string(), 0.0);
    let panicked = |message: &str| Compiled::Panicked {
        message: message.to_string(),
    };
    assert!(worker::same_failure(&crashed, &expected, &panicked("boom")));
    assert!(!worker::same_failure(
        &crashed,
        &expected,
        &panicked("other")
    ));
}

#[test]
fn tags_and_shapes_describe_the_calls_whose_arguments_are_removed() {
    let cycle = function(
        "cycle",
        &["A", "B"],
        &[
            ("forward", "fn(a: A) -> B"),
            ("backward", "fn(b: B) -> A"),
            ("value", "A"),
        ],
        "B",
    );
    let fn_ty = Ty::func(vec![Ty::Lit(5)], Ty::Lit(5));
    let lambda = |typed| {
        Expr::new(
            ExprKind::Lambda {
                params: vec![("x".to_string(), Ty::Lit(5))],
                typed,
                body: Box::new(int(5, Ty::Lit(5))),
            },
            fn_ty.clone(),
        )
    };
    let value = call(
        0,
        "cycle",
        vec![Ty::Lit(5), Ty::Lit(5)],
        vec![lambda(false), lambda(true), int(5, Ty::Lit(5))],
        Ty::Lit(5),
    );
    let program = program(vec![cycle], Some(Ty::Lit(5)), value);
    let variant = strip_all(&[0]);

    assert_eq!(
        shrink::tags(&program, &variant),
        [
            "cycle of generics",
            "expected literal",
            "typed lambda",
            "untyped lambda"
        ]
    );
    assert_eq!(
        shrink::shape(&program, &variant),
        "cycle(lambda, typed lambda, lit) as let: literal"
    );
}

fn record(seed: u64, outcomes: &[(&str, Category, &[&str])]) -> report::SeedRecord {
    report::SeedRecord {
        seed,
        generator: 1,
        explicit_source: String::new(),
        explicit: Explicit::Valid {
            time_ms: 1.0,
            fingerprint: None,
        },
        variants: outcomes
            .iter()
            .map(|(name, category, tags)| report::VariantRecord {
                name: (*name).to_string(),
                source: String::new(),
                shape: String::new(),
                tags: tags.iter().map(ToString::to_string).collect(),
                outcome: Outcome::failed(*category, "detail".to_string(), 1.0),
            })
            .collect(),
    }
}

#[test]
fn shrink_jobs_sample_each_set_of_tags() {
    let mut records = Vec::new();
    for seed in 0..12 {
        records.push(record(
            seed,
            &[
                ("all", Category::Unsound, &[]),
                ("some", Category::Slow, &[]),
                ("one", Category::Crashed, &["a"]),
            ],
        ));
    }
    let clusters = report::clusters(&records);
    let jobs = report::shrink_jobs(&records, &clusters, 2);

    let count = |category| {
        jobs.iter()
            .filter(|(cluster, _)| clusters[*cluster].category == category)
            .count()
    };
    // Slow failures aren't shrunk, and at least 10 of the rare categories are
    assert_eq!(count(Category::Slow), 0);
    assert_eq!(count(Category::Unsound), 10);
    assert_eq!(count(Category::Crashed), 10);
}

#[test]
fn shrink_jobs_take_single_sites_first_and_each_set_of_tags_in_turn() {
    let records = vec![
        record(
            0,
            &[
                ("all", Category::Unsound, &["a"]),
                ("one", Category::Unsound, &["a"]),
            ],
        ),
        record(1, &[("all", Category::Unsound, &["b"])]),
    ];
    let clusters = report::clusters(&records);
    let mut jobs: Vec<(u64, usize)> = report::shrink_jobs(&records, &clusters, 0)
        .into_iter()
        .map(|(_, member)| member)
        .collect();
    jobs.truncate(2);

    // The `one` variant of the larger set, then the other set
    assert_eq!(jobs, [(0, 1), (1, 0)]);
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
        serde_json::to_string(&Explicit::Valid {
            time_ms: 1.0,
            fingerprint: None,
        })
        .unwrap()
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
            format!("run 1 {seed} 0"),
            format!("run 1 {seed} 1"),
            format!("run 1 {seed} 2")
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

#[test]
fn a_last_variant_that_hangs_ends_the_seed() {
    let (seed, generated) = (0..100)
        .map(|seed| (seed, Generator::new(seed).generate()))
        .find(|(_, generated)| generated.variants.len() == 3)
        .unwrap();

    let mut script = Script::new(vec![vec![
        line("progress explicit"),
        valid(),
        line("progress 0"),
        variant_line(0),
        line("progress 1"),
        variant_line(1),
        line("progress 2"),
        Message::TimedOut,
    ]]);

    let record = run_seed(&mut script, seed, &generated, Duration::from_secs(10)).unwrap();

    assert_eq!(script.commands, [format!("run 1 {seed} 0")]);
    assert_eq!(script.restarts, 1);
    assert_eq!(record.variants[2].outcome.category, Category::Slow);
}

#[test]
fn workers_resume_at_the_variant_they_are_given() {
    let seed = (0..100)
        .find(|&seed| Generator::new(seed).generate().variants.len() == 3)
        .unwrap();

    let mut out = Vec::new();
    worker::run_command(&mut out, 1, seed, 2).unwrap();
    let out = String::from_utf8(out).unwrap();

    let lines: Vec<&str> = out.lines().collect();
    let progress: Vec<&str> = lines
        .iter()
        .copied()
        .filter(|line| line.starts_with("progress"))
        .collect();
    assert_eq!(progress, ["progress explicit", "progress 2"]);
    assert!(
        lines
            .iter()
            .all(|line| !line.starts_with("variant [0,") && !line.starts_with("variant [1,")),
        "{out}"
    );
    assert_eq!(lines.last(), Some(&"done"));
}

// Comparing two runs.

fn compared(before: Category, after: Category) -> compare::Comparison {
    let old = record(0, &[("all", before, &[])]);
    let mut new = old.clone();
    new.variants[0].outcome.category = after;
    compare::compare(&[old], &[new])
}

fn kinds(comparison: &compare::Comparison) -> Vec<compare::ChangeKind> {
    comparison
        .changes
        .iter()
        .map(|change| change.kind)
        .collect()
}

#[test]
fn comparisons_report_variants_that_are_no_longer_inferred() {
    use compare::ChangeKind::{Regression, Status};

    assert_eq!(kinds(&compared(Category::Inferred, Category::Inferred)), []);
    assert_eq!(kinds(&compared(Category::Rejected, Category::Rejected)), []);
    assert_eq!(
        kinds(&compared(Category::Rejected, Category::Inferred)),
        [Status]
    );

    for worse in [
        Category::Rejected,
        Category::Unsound,
        Category::Slow,
        Category::Crashed,
    ] {
        assert_eq!(
            kinds(&compared(Category::Inferred, worse)),
            [Regression],
            "{worse:?}"
        );
    }

    // A rejection that becomes unsound is worse too, and so is a move between bad categories
    assert_eq!(
        kinds(&compared(Category::Rejected, Category::Unsound)),
        [Regression]
    );
    assert_eq!(
        kinds(&compared(Category::Unsound, Category::Crashed)),
        [Regression]
    );
    assert_eq!(
        kinds(&compared(Category::Unsound, Category::Rejected)),
        [Status]
    );

    // Staying unsound, slow or crashed isn't a regression in itself
    for bad in [Category::Unsound, Category::Slow, Category::Crashed] {
        assert_eq!(kinds(&compared(bad, bad)), [], "{bad:?}");
    }

    let comparison = compared(Category::Rejected, Category::Inferred);
    assert_eq!(
        comparison
            .transitions
            .get(&(Category::Rejected, Category::Inferred)),
        Some(&1)
    );
    assert!(comparison.report().contains(
        "Compared 1 variants: 1 changed, 0 regressed, 1 newly inferred, 0 not comparable, 2 \
         variants and explicit programs without fingerprints."
    ));
}

#[test]
fn comparisons_report_changed_outputs_and_diagnostics() {
    let mut old = record(
        0,
        &[
            ("all", Category::Inferred, &[]),
            ("one", Category::Rejected, &[]),
        ],
    );
    let Compiled::Rejected { diag, .. } = rejected(1) else {
        unreachable!()
    };
    old.variants[0].outcome.output = Some(Run::Output("1".to_string()));
    old.variants[1].outcome.diag = Some(diag.clone());

    // The same results aren't changes
    assert_eq!(compare::compare(&[old.clone()], &[old.clone()]).changes, []);

    let mut new = old.clone();
    new.variants[0].outcome.output = Some(Run::Output("2".to_string()));
    new.variants[1].outcome.diag = Some(Diag {
        message: "Cannot infer generic `U` for `f`".to_string(),
        ..diag
    });

    let comparison = compare::compare(std::slice::from_ref(&old), &[new]);
    assert_eq!(
        kinds(&comparison),
        [compare::ChangeKind::Output, compare::ChangeKind::Diagnostic]
    );
    let report = comparison.report();
    assert!(
        report.contains(r#"- seed 0 `all` [output]: "Inferred: 1" -> "Inferred: 2""#),
        "{report}"
    );
    assert!(
        report.contains(
            r#"- seed 0 `one` [diagnostic]: "Rejected: Cannot infer generic `T` for `f` at 1:1" -> "Rejected: Cannot infer generic `U` for `f` at 1:1""#
        ),
        "{report}"
    );

    // A run from before outputs were recorded has none to compare
    let mut unrecorded = old.clone();
    unrecorded.variants[0].outcome.output = None;
    assert_eq!(compare::compare(&[unrecorded], &[old]).changes, []);
}

#[test]
fn comparisons_report_changed_fingerprints() {
    let mut old = record(0, &[("all", Category::Inferred, &[])]);
    old.variants[0].outcome.fingerprint = Some("aaaa".to_string());
    old.explicit = Explicit::Valid {
        time_ms: 1.0,
        fingerprint: Some("eeee".to_string()),
    };

    let mut new = old.clone();
    new.variants[0].outcome.fingerprint = Some("bbbb".to_string());
    new.explicit = Explicit::Valid {
        time_ms: 2.0,
        fingerprint: Some("ffff".to_string()),
    };

    let comparison = compare::compare(std::slice::from_ref(&old), &[new]);
    assert_eq!(
        kinds(&comparison),
        [
            compare::ChangeKind::Fingerprint,
            compare::ChangeKind::Fingerprint
        ]
    );
    assert_eq!(comparison.regressions().count(), 0);
    let report = comparison.report();
    assert!(
        report.contains(r#"- seed 0 `explicit` [fingerprint]: "Valid #eeee" -> "Valid #ffff""#),
        "{report}"
    );
    assert!(
        report.contains(
            r#"- seed 0 `all` [fingerprint]: "Inferred: detail #aaaa" -> "Inferred: detail #bbbb""#
        ),
        "{report}"
    );

    // A run from before fingerprints were recorded has none to compare
    let mut unrecorded = old.clone();
    unrecorded.variants[0].outcome.fingerprint = None;
    unrecorded.explicit = Explicit::Valid {
        time_ms: 1.0,
        fingerprint: None,
    };
    let comparison = compare::compare(&[unrecorded], &[old.clone()]);
    assert_eq!(comparison.changes, []);
    assert_eq!(comparison.without_fingerprints, 2);
    assert_eq!(
        compare::compare(std::slice::from_ref(&old), &[old.clone()]).without_fingerprints,
        0
    );
}

#[test]
fn fingerprints_cover_every_diagnostic_and_the_clvm() {
    let warning = |line| {
        let source = format!(
            "fn main() -> Int {{\n{}    let x = 1;\n    2\n}}\n",
            "\n".repeat(line)
        );
        match compile(&source) {
            Compiled::Ok { fingerprint, .. } => fingerprint,
            other => panic!("{other:?}"),
        }
    };

    // The same program has the same fingerprint, and moving its warning changes it
    assert_eq!(warning(0), warning(0));
    assert_ne!(warning(0), warning(1));

    let clvm = |value| match compile(&format!("fn main() -> Int {{\n    {value}\n}}\n")) {
        Compiled::Ok { fingerprint, .. } => fingerprint,
        other => panic!("{other:?}"),
    };
    assert_ne!(clvm(1), clvm(2));

    // A rejected program's later warnings count too, not only its first error
    let rejected = |line| {
        let source = format!(
            "fn main() -> Int {{\n    let y: Int = \"a\";\n{}    let x = 1;\n    y\n}}\n",
            "\n".repeat(line)
        );
        match compile(&source) {
            Compiled::Rejected {
                diag, fingerprint, ..
            } => (diag, fingerprint),
            other => panic!("{other:?}"),
        }
    };
    let (first, before) = rejected(0);
    let (second, after) = rejected(1);
    assert_eq!(first, second);
    assert_ne!(before, after);

    // So do each diagnostic's message and severity
    let source = Source::new(
        Arc::from("fn main() {}"),
        SourceKind::File("main.rue".to_string()),
    );
    let diagnostic = |field: &str, severity| {
        Diagnostic::new(
            SrcLoc::new(source.clone(), 0..2),
            DiagnosticKind::UnknownField(field.to_string(), "T".to_string()),
            severity,
        )
    };
    let fingerprint = |diagnostics: &[Diagnostic]| oracle::fingerprint(diagnostics, None);
    let error = fingerprint(&[diagnostic("a", DiagnosticSeverity::Error)]);
    assert_eq!(
        error,
        fingerprint(&[diagnostic("a", DiagnosticSeverity::Error)])
    );
    assert_ne!(
        error,
        fingerprint(&[diagnostic("b", DiagnosticSeverity::Error)])
    );
    assert_ne!(
        error,
        fingerprint(&[diagnostic("a", DiagnosticSeverity::Warning)])
    );
}

#[test]
fn outcomes_record_the_stripped_programs_fingerprint() {
    let explicit = "fn main() -> Int {\n    2\n}\n";
    let stripped = "fn main() -> Int {\n    let x = 1;\n    2\n}\n";
    let outcome = classify_sources(explicit, stripped);
    assert_eq!(outcome.category, Category::Inferred);

    let fingerprint = |source| match compile(source) {
        Compiled::Ok { fingerprint, .. } => fingerprint,
        other => panic!("{other:?}"),
    };
    assert_eq!(outcome.fingerprint, Some(fingerprint(stripped)));
    assert_ne!(outcome.fingerprint, Some(fingerprint(explicit)));

    let rejected = classify_sources(explicit, "fn main() -> Int {\n    \"a\"\n}\n");
    assert_eq!(rejected.category, Category::Rejected);
    assert!(rejected.fingerprint.is_some());
}

#[test]
fn workers_send_fingerprints_for_the_explicit_program_and_each_variant() {
    let seed = (0..100)
        .find(|&seed| Generator::new(seed).generate().variants.len() == 3)
        .unwrap();

    let mut out = Vec::new();
    worker::run_command(&mut out, 1, seed, 0).unwrap();
    let out = String::from_utf8(out).unwrap();

    let explicit = out
        .lines()
        .find_map(|line| line.strip_prefix("explicit "))
        .unwrap();
    let explicit: Explicit = serde_json::from_str(explicit).unwrap();
    assert!(
        matches!(
            explicit,
            Explicit::Valid {
                fingerprint: Some(_),
                ..
            }
        ),
        "{explicit:?}"
    );

    let variants: Vec<Outcome> = out
        .lines()
        .filter_map(|line| line.strip_prefix("variant "))
        .map(|line| serde_json::from_str::<(usize, Outcome)>(line).unwrap().1)
        .collect();
    assert_eq!(variants.len(), 3);
    assert!(
        variants.iter().all(|outcome| outcome.fingerprint.is_some()),
        "{variants:?}"
    );
}

#[test]
fn comparisons_report_explicit_programs_that_no_longer_compile() {
    let old = record(0, &[("all", Category::Inferred, &[])]);
    let mut new = old.clone();
    new.explicit = Explicit::Panicked("boom".to_string());

    let comparison = compare::compare(&[old], &[new]);
    assert_eq!(kinds(&comparison), [compare::ChangeKind::Regression]);
    assert_eq!(comparison.changes[0].variant, "explicit");
}

#[test]
fn comparisons_count_the_same_rejected_explicit_program_as_without_a_fingerprint() {
    let Compiled::Rejected { diag, .. } = rejected(1) else {
        unreachable!()
    };
    let mut old = record(0, &[]);
    old.explicit = Explicit::Rejected(diag);

    let comparison = compare::compare(std::slice::from_ref(&old), &[old.clone()]);
    assert_eq!(comparison.changes, []);
    assert_eq!(comparison.without_fingerprints, 1);

    // A program with no sites has nothing to compare
    old.explicit = Explicit::NoSites;
    let comparison = compare::compare(std::slice::from_ref(&old), &[old.clone()]);
    assert_eq!(comparison.without_fingerprints, 0);
}

#[test]
fn comparisons_skip_variants_whose_programs_differ() {
    let old = record(0, &[("all", Category::Inferred, &[])]);
    let mut new = old.clone();
    new.variants[0].source = "fn main() -> Int {\n    1\n}\n".to_string();
    new.variants[0].outcome.category = Category::Rejected;

    // Neither side has a match
    let comparison = compare::compare(std::slice::from_ref(&old), &[new]);
    assert_eq!(comparison.changes, []);
    assert_eq!(comparison.not_comparable, 2);

    // A seed that's only in the first run
    let comparison = compare::compare(&[old], &[]);
    assert_eq!(comparison.not_comparable, 1);
}

#[test]
fn outcomes_from_before_outputs_were_recorded_still_load() {
    let json =
        r#"{"category":"Inferred","diag":null,"same_clvm":true,"detail":null,"time_ms":1.0}"#;
    let outcome: Outcome = serde_json::from_str(json).unwrap();
    assert_eq!(outcome.output, None);
}

#[test]
fn comparisons_report_crashes_that_change() {
    let mut old = record(0, &[("all", Category::Crashed, &[])]);
    old.variants[0].outcome.detail = Some("index out of bounds\nmore".to_string());
    let mut new = old.clone();

    new.variants[0].outcome.detail = Some("index out of bounds\nother".to_string());
    assert_eq!(
        kinds(&compare::compare(
            std::slice::from_ref(&old),
            std::slice::from_ref(&new)
        )),
        []
    );

    new.variants[0].outcome.detail = Some("worker died: stack overflow".to_string());
    let comparison = compare::compare(&[old], &[new]);
    assert_eq!(kinds(&comparison), [compare::ChangeKind::Diagnostic]);
    // Each side is one JSON string, whatever its text holds
    let report = comparison.report();
    assert!(
        report.contains(
            r#""Crashed: index out of bounds\nmore" -> "Crashed: worker died: stack overflow""#
        ),
        "{report}"
    );
}

#[test]
fn comparisons_report_explicit_programs_that_newly_compile() {
    let mut old = record(0, &[]);
    old.explicit = Explicit::TimedOut;
    let new = record(
        0,
        &[
            ("all", Category::Unsound, &[]),
            ("one", Category::Inferred, &[]),
        ],
    );

    let comparison = compare::compare(&[old], &[new]);
    assert_eq!(
        kinds(&comparison),
        [compare::ChangeKind::Status, compare::ChangeKind::Regression]
    );
    assert_eq!(comparison.changes[0].variant, "explicit");
    assert_eq!(comparison.changes[1].before, "none");
    assert_eq!(comparison.not_comparable, 2);
}

#[test]
fn comparisons_count_what_only_the_new_run_has() {
    let old = record(0, &[("all", Category::Inferred, &[])]);
    let mut new = old.clone();
    new.variants.push(new.variants[0].clone());
    new.variants[1].name = "some".to_string();
    let extra = record(1, &[("all", Category::Inferred, &[])]);

    let comparison = compare::compare(&[old], &[new, extra]);
    assert_eq!(comparison.changes, []);
    assert_eq!(comparison.not_comparable, 2);
}

#[test]
fn comparisons_load_runs_from_a_file_or_its_directory() {
    let dir = std::env::temp_dir().join(format!("infer-fuzz-load-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let record = record(3, &[("all", Category::Inferred, &[])]);
    let line = serde_json::to_string(&record).unwrap();
    std::fs::write(dir.join("results.jsonl"), format!("{line}\n\n{line}\n")).unwrap();

    assert_eq!(compare::load(&dir).unwrap().len(), 2);
    assert_eq!(compare::load(&dir.join("results.jsonl")).unwrap().len(), 2);

    std::fs::write(dir.join("results.jsonl"), format!("{line}\nnot json\n")).unwrap();
    let error = format!("{:#}", compare::load(&dir).unwrap_err());
    assert!(error.contains("line 2"), "{error}");

    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn comparisons_compare_explicit_panics_by_their_first_line() {
    let mut old = record(0, &[]);
    old.explicit = Explicit::Panicked("assertion failed\nleft: 1".to_string());
    let mut new = old.clone();
    new.explicit = Explicit::Panicked("assertion failed\nleft: 2".to_string());
    assert_eq!(
        compare::compare(std::slice::from_ref(&old), &[new.clone()]).changes,
        []
    );

    new.explicit = Explicit::Panicked("index out of bounds".to_string());
    assert_eq!(
        kinds(&compare::compare(&[old], &[new])),
        [compare::ChangeKind::Diagnostic]
    );
}

#[test]
fn slow_variants_are_described_by_their_timing() {
    let old = record(0, &[("all", Category::Inferred, &[])]);
    let mut new = old.clone();
    new.variants[0].outcome.category = Category::Slow;
    new.variants[0].outcome.output = Some(Run::Output("1".to_string()));
    new.variants[0].outcome.detail = Some("500.0 ms, explicit 1.0 ms".to_string());

    let comparison = compare::compare(&[old], &[new]);
    assert_eq!(
        comparison.changes[0].after,
        "Slow: 500.0 ms, explicit 1.0 ms"
    );
}

/// A hash of the programs and variants that generator version 1 prints for seeds `0..100`. Runs
/// from before a change to the generator must stay comparable, so this must never change: new
/// shapes go in a new version.
const VERSION_1_HASH: u64 = 0x76a7_95f7_9e3d_bec1;

fn programs_hash(version: u32) -> u64 {
    // FNV-1a, which doesn't depend on the standard library's hasher staying the same
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for seed in 0..100 {
        let generated = Generator::versioned(seed, version).generate();
        let mut text = generated.program.print(&Variant::default());
        for (name, variant) in &generated.variants {
            text.push_str(name);
            text.push_str(&generated.program.print(variant));
        }
        for byte in text.bytes() {
            hash ^= u64::from(byte);
            hash = hash.wrapping_mul(0x0100_0000_01b3);
        }
    }
    hash
}

#[test]
fn version_1_programs_never_change() {
    assert_eq!(programs_hash(1), VERSION_1_HASH, "{:#x}", programs_hash(1));
}

/// The same as `VERSION_1_HASH`, for generator version 2.
const VERSION_2_HASH: u64 = 0xccbd_89ce_e4c9_1ebb;

#[test]
fn version_2_programs_never_change() {
    assert_eq!(programs_hash(2), VERSION_2_HASH, "{:#x}", programs_hash(2));
}

#[test]
fn version_2_is_deterministic_and_its_explicit_programs_compile() {
    let mut valid = 0;
    for seed in 0..40 {
        let first = Generator::versioned(seed, 2).generate();
        let second = Generator::versioned(seed, 2).generate();
        let explicit = first.program.print(&Variant::default());
        assert_eq!(explicit, second.program.print(&Variant::default()));
        assert_eq!(first.variants, second.variants);
        if matches!(
            oracle::compile_with(&explicit, first.program.uses_std()),
            Compiled::Ok { .. }
        ) {
            valid += 1;
        }
    }
    assert!(valid >= 37, "only {valid} of 40 explicit programs compile");
}

#[test]
fn version_2_types_print_as_parsed() {
    for text in [
        "Maybe<T>",
        "Tree<(A, B)>",
        "Entry<A, List<B>>",
        "Box<T> | (T, T)",
        "(fn(a: Int) -> T) | (fn(a: Bytes) -> T)",
        "5 | 7",
        "Box<A> | Entry<A, B> | nil",
    ] {
        let generics = ["A", "B", "T"];
        let names: Vec<String> = generics.iter().map(ToString::to_string).collect();
        assert_eq!(parse_ty(text, &generics).print(&names), text);
    }

    let nested = Ty::GenFn(
        vec!["T".to_string()],
        vec![Ty::Rigid("T".to_string())],
        Box::new(Ty::GenFn(
            vec!["U".to_string()],
            vec![Ty::Rigid("U".to_string())],
            Box::new(Ty::pair(
                Ty::Rigid("T".to_string()),
                Ty::Rigid("U".to_string()),
            )),
        )),
    );
    assert_eq!(nested.print(&[]), "fn<T>(a: T) -> (fn<U>(a: U) -> (T, U))");
}

/// `let v0: fn<T>(a: T) -> (T, T) = make();` (#340) can't be written with explicit generic
/// arguments, so the explicit program wraps the call in a generic lambda.
#[test]
fn calls_given_a_generic_function_type_are_wrapped() {
    let make = function("make", &["A", "B"], &[], "fn(a: A) -> B");
    let t = || Ty::Rigid("T".to_string());
    let pair = Ty::pair(t(), t());
    let annotation = Ty::GenFn(vec!["T".to_string()], vec![t()], Box::new(pair.clone()));
    let call = Expr::new(
        ExprKind::Call {
            site: None,
            function: "make".to_string(),
            generic_args: vec![t(), pair],
            args: Vec::new(),
        },
        Ty::func(vec![t()], Ty::pair(t(), t())),
    );
    let wrapped = Expr::new(
        ExprKind::Wrapped {
            site: 0,
            layers: vec![(vec!["T".to_string()], vec![("w1".to_string(), t())])],
            call: Box::new(call),
        },
        annotation.clone(),
    );
    let mut program = program(vec![make], Some(annotation), wrapped);
    program.main.body = Expr::new(
        ExprKind::Apply(
            Box::new(Expr::new(ExprKind::Var("v0".to_string()), Ty::Any)),
            vec![int(1, Ty::Int)],
        ),
        Ty::Any,
    );

    let explicit = program.print(&Variant::default());
    assert!(
        explicit.contains("= fn<T>(w1: T) => make::<T, (T, T)>()(w1);"),
        "{explicit}"
    );
    assert!(
        matches!(compile(&explicit), Compiled::Ok { .. }),
        "{explicit}"
    );

    let variant = strip_all(&[0]);
    assert!(program.print(&variant).contains("= make();"));
    // The expected generic function type gives both of `make`'s generics a type
    assert_eq!(masks(&program), [[true, true]]);
    assert_eq!(legalize(&program, &variant), variant);
}

#[test]
fn a_value_for_a_union_gives_no_type() {
    let pair = Ty::pair(Ty::Int, Ty::Int);
    let both = function("both", &["T"], &[("value", "Box<T> | (T, T)")], "T");
    let value = call(
        0,
        "both",
        vec![Ty::Int],
        vec![Expr::new(
            ExprKind::Pair(Box::new(int(1, Ty::Int)), Box::new(int(2, Ty::Int))),
            pair.clone(),
        )],
        Ty::Int,
    );
    // `Box<T>` is a pair too, so the value could be either member's
    assert_eq!(
        masks(&program(vec![both.clone()], None, value.clone())),
        [[false]]
    );

    // The expected type still gives the return type's generic one
    assert_eq!(
        masks(&program(vec![both.clone()], Some(Ty::Int), value.clone())),
        [[true]]
    );
    let union = Ty::Union(vec![Ty::Lit(5), Ty::Lit(7)]);
    let five = Expr {
        ty: union.clone(),
        ..call(0, "both", vec![Ty::Lit(5)], vec![nil(Ty::Nil)], Ty::Lit(5))
    };
    assert_eq!(
        masks(&program(vec![both.clone()], Some(union), five)),
        [[true]]
    );

    // `("a", 3)` for `(A, Int) | (Bytes, A)` fits `A = Bytes` and `A = Int`
    let overlap = function("pick", &["A"], &[("value", "(A, Int) | (Bytes, A)")], "A");
    let value = call(
        0,
        "pick",
        vec![Ty::Bytes],
        vec![Expr::new(
            ExprKind::Pair(
                Box::new(Expr::new(ExprKind::Bytes, Ty::Bytes)),
                Box::new(int(3, Ty::Int)),
            ),
            Ty::pair(Ty::Bytes, Ty::Int),
        )],
        Ty::Bytes,
    );
    assert_eq!(masks(&program(vec![overlap], None, value)), [[false]]);

    // Nor does it with generics in only one member each
    let one = function("both", &["A", "B"], &[("value", "Box<A> | (B, B)")], "B");
    let value = call(
        0,
        "both",
        vec![Ty::Int, Ty::Int],
        vec![Expr::new(
            ExprKind::Pair(Box::new(int(1, Ty::Int)), Box::new(int(2, Ty::Int))),
            pair,
        )],
        Ty::Int,
    );
    assert_eq!(masks(&program(vec![one], None, value)), [[false, false]]);
}

#[test]
fn the_expected_variant_removes_only_what_the_expected_type_alone_gives() {
    // Over the seeds, every site the `expected` variant removes has nothing but the expected
    // type to be inferred from
    let mut checked = 0;
    for seed in 0..100 {
        let generated = Generator::versioned(seed, 2).generate();
        let Some((_, variant)) = generated
            .variants
            .iter()
            .find(|(name, _)| name == "expected")
        else {
            continue;
        };
        let without_expected =
            generate::removable_sites_with(&generated.program, &Variant::default(), false);
        for site in variant.stripped.keys() {
            assert!(without_expected[site].iter().all(|&removable| !removable));
            checked += 1;
        }
    }
    assert!(checked > 20, "only {checked} sites");
}

#[test]
fn programs_that_call_std_compile_with_it() {
    let (seed, generated) = (0..200)
        .map(|seed| (seed, Generator::versioned(seed, 2).generate()))
        .find(|(_, generated)| {
            generated.program.uses_std()
                && generated
                    .program
                    .print(&Variant::default())
                    .contains("merge_list::<")
        })
        .unwrap();
    let explicit = generated.program.print(&Variant::default());
    assert!(!explicit.contains("fn merge_list"), "seed {seed}");
    assert!(
        matches!(oracle::compile_with(&explicit, true), Compiled::Ok { .. }),
        "seed {seed}"
    );
    assert!(matches!(compile(&explicit), Compiled::Rejected { .. }));
}

fn std_seed() -> u64 {
    (0..200)
        .find(|&seed| Generator::versioned(seed, 2).generate().program.uses_std())
        .unwrap()
}

#[test]
fn workers_compile_version_2_programs_with_std() {
    let seed = std_seed();
    let mut out = Vec::new();
    worker::run_command(&mut out, 2, seed, 0).unwrap();
    let out = String::from_utf8(out).unwrap();
    let explicit = out
        .lines()
        .find(|line| line.starts_with("explicit "))
        .unwrap();
    assert!(explicit.contains("Valid"), "{out}");
}

#[test]
fn the_driver_sends_and_records_the_generator_version() {
    let (seed, generated) = (0..100)
        .map(|seed| (seed, Generator::versioned(seed, 2).generate()))
        .find(|(_, generated)| !generated.variants.is_empty())
        .unwrap();
    let mut answers = vec![line("progress explicit"), valid()];
    for index in 0..generated.variants.len() {
        answers.push(line(&format!("progress {index}")));
        answers.push(variant_line(index));
    }
    answers.push(line("done"));
    let mut script = Script::new(vec![answers]);

    let record = run_seed(&mut script, seed, &generated, Duration::from_secs(10)).unwrap();
    assert_eq!(script.commands, [format!("run 2 {seed} 0")]);
    assert_eq!(record.generator, 2);

    let shrunk = worker::Shrunk {
        explicit: String::new(),
        stripped: String::new(),
        shape: String::new(),
        tags: Vec::new(),
        message: String::new(),
    };
    let mut script = Script::new(vec![vec![line(&format!(
        "shrunk {}",
        serde_json::to_string(&shrunk).unwrap()
    ))]]);
    worker::shrink_on(&mut script, 2, seed, 1, Duration::from_secs(10)).unwrap();
    assert_eq!(
        script.commands,
        [format!("shrink 2 {seed} 1 {}", worker::SHRINK_BUDGET)]
    );
}

#[test]
fn outputs_list_every_shrunk_failure_and_count_expected_variants() {
    let records = vec![record(
        0,
        &[
            ("all", Category::Rejected, &[]),
            ("expected", Category::Inferred, &[]),
        ],
    )];
    let mut clusters = report::clusters(&records);
    let shrunk = worker::Shrunk {
        explicit: "fn main() -> Int {\n    1\n}\n".to_string(),
        stripped: "fn main() -> Int {\n    1\n}\n".to_string(),
        shape: "shape".to_string(),
        tags: vec!["tag".to_string()],
        message: "message".to_string(),
    };
    clusters[0].shrunk.push(((0, 0), shrunk));
    let report = report::report(&records, &clusters);
    assert!(
        report.contains("| expected | 1 | 0 | 0 | 0 | 0 |"),
        "{report}"
    );

    let dir = std::env::temp_dir().join(format!("infer-fuzz-outputs-{}", std::process::id()));
    report::write_outputs(&dir, &records, &clusters, &report).unwrap();
    let shrunk = std::fs::read_to_string(dir.join("shrunk.jsonl")).unwrap();
    let lines: Vec<&str> = shrunk.lines().collect();
    assert_eq!(lines.len(), 1);
    let line: serde_json::Value = serde_json::from_str(lines[0]).unwrap();
    assert_eq!(line["cluster"], 1);
    assert_eq!(line["seed"], 0);
    assert_eq!(line["variant"], "all");
    assert_eq!(line["shrunk"]["message"], "message");
    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn a_value_for_maybe_gives_what_it_would_for_its_inner_type() {
    // `(1, nil)` for `Maybe<(A, B)>` gives `A` a type, as it would for `(A, B)`, but not `B`
    let wrap = function("wrap", &["A", "B"], &[("value", "Maybe<(A, B)>")], "Int");
    let pair = Ty::pair(Ty::Int, Ty::Nil);
    let value = call(
        0,
        "wrap",
        vec![Ty::Int, Ty::Nil],
        vec![Expr::new(
            ExprKind::Pair(Box::new(int(1, Ty::Int)), Box::new(nil(Ty::Nil))),
            pair,
        )],
        Ty::Int,
    );
    assert_eq!(masks(&program(vec![wrap], None, value)), [[true, false]]);
}

/// A wrapped call is removed all or nothing: if anything can't be inferred, nothing is.
#[test]
fn wrapped_calls_with_a_generic_nothing_infers_keep_all_their_arguments() {
    // `C` is only in the parameter, and `nil` gives it nothing
    let make = function(
        "make",
        &["A", "B", "C"],
        &[("value", "C | nil")],
        "fn(a: A) -> B",
    );
    let t = || Ty::Rigid("T".to_string());
    let annotation = Ty::GenFn(vec!["T".to_string()], vec![t()], Box::new(t()));
    let call = Expr::new(
        ExprKind::Call {
            site: None,
            function: "make".to_string(),
            generic_args: vec![t(), t(), Ty::Int],
            args: vec![nil(Ty::opt(Ty::Int))],
        },
        Ty::func(vec![t()], t()),
    );
    let wrapped = Expr::new(
        ExprKind::Wrapped {
            site: 0,
            layers: vec![(vec!["T".to_string()], vec![("w1".to_string(), t())])],
            call: Box::new(call),
        },
        annotation.clone(),
    );
    let program = program(vec![make], Some(annotation), wrapped);
    assert_eq!(masks(&program), [[false, false, false]]);
    assert_eq!(legalize(&program, &strip_all(&[0])), Variant::default());
}

#[test]
fn shrinking_keeps_functions_from_the_standard_library() {
    let merge = function(
        "merge_list",
        &["T"],
        &[("a", "List<T>"), ("b", "List<T>")],
        "List<T>",
    );
    let list = Ty::list(Ty::Int);
    let items = |value| Expr::new(ExprKind::List(vec![int(value, Ty::Int)]), list.clone());
    let value = call(
        0,
        "merge_list",
        vec![Ty::Int],
        vec![items(1), items(2)],
        list.clone(),
    );
    let program = program(vec![merge], None, value);

    let (program, _) = shrink::shrink(program, strip_all(&[0]), 100, &mut |program, variant| {
        program.exprs().iter().any(|expr| variant.strips(expr))
    });
    assert_eq!(program.functions.len(), 1);
}

/// The leak oracle finds nothing in the snapshot tests that compile, whose types are what the
/// compiler means them to be, so it doesn't flag a generic that's in scope.
#[test]
fn leak_oracle_flags_nothing_in_the_snapshot_tests() {
    // Some of them are deep, as the stress tests are, so they're compiled with a larger stack
    std::thread::Builder::new()
        .stack_size(256 * 1024 * 1024)
        .spawn(check_snapshot_tests_for_leaks)
        .unwrap()
        .join()
        .unwrap();
}

fn check_snapshot_tests_for_leaks() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../tests");
    let mut stack = vec![root];
    let mut leaks = Vec::new();
    let mut checked = 0;

    while let Some(dir) = stack.pop() {
        for entry in std::fs::read_dir(&dir).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                stack.push(path);
            } else if path.extension().is_some_and(|ext| ext == "rue") {
                let source = std::fs::read_to_string(&path).unwrap();
                if let Compiled::Ok {
                    leak: Some(leak), ..
                } = oracle::compile_with(&source, true)
                {
                    leaks.push(format!("{}: {leak}", path.display()));
                }
                checked += 1;
            }
        }
    }

    assert!(checked > 100);
    assert_eq!(leaks, Vec::<String>::new());
}
