use super::*;
use crate::generate::parse_ty;
use crate::program::{ExprKind, Ty};

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

/// The generator is only useful if most of its explicit programs compile, and if removing
/// generic arguments changes them.
#[test]
fn most_explicit_programs_compile() {
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
    assert!(valid >= 24, "only {valid} of 40 explicit programs compile");
}

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

#[test]
fn shrinks_to_what_the_failure_needs() {
    // Fails while some call to `map` has its generic arguments removed
    let calls_map = |program: &program::Program, variant: &Variant| {
        program.exprs().iter().any(|expr| {
            matches!(&expr.kind, ExprKind::Call { function, site: Some(site), .. }
                if function == "map" && variant.stripped.contains_key(site))
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

    let (program, variant) =
        shrink::shrink(generated.program, variant, 2000, &mut |program, variant| {
            calls_map(program, variant)
        });

    assert!(calls_map(&program, &variant));
    assert_eq!(variant.stripped.len(), 1);
    // `map` and whatever its arguments need, and nothing that the call doesn't use
    assert!(program.functions.len() <= 3, "{}", program.print(&variant));
    assert!(program.main.lets.len() == 1, "{}", program.print(&variant));
}
