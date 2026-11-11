use std::sync::Arc;

use rue_diagnostic::{Diagnostic, DiagnosticSeverity, Lint, LintLevel, Source, SourceKind};
use rue_options::CompilerOptions;

use crate::{Compiler, File, FileTree};

const SOURCE: &str = "fn main() -> Int {\n    let unused = 1;\n    --42\n}\n";

fn compile(configure: impl FnOnce(&mut CompilerOptions)) -> (Vec<Diagnostic>, bool) {
    compile_source(SOURCE, configure)
}

fn compile_source(
    source: &str,
    configure: impl FnOnce(&mut CompilerOptions),
) -> (Vec<Diagnostic>, bool) {
    let mut options = CompilerOptions {
        std: false,
        ..CompilerOptions::default()
    };
    configure(&mut options);

    let mut ctx = Compiler::new(options);

    let tree = FileTree::File(File::new(
        &mut ctx,
        "main".to_string(),
        Source::new(Arc::from(source), SourceKind::File("main.rue".to_string())),
    ));

    tree.compile(&mut ctx);

    let has_errors = ctx.has_errors();
    (ctx.take_diagnostics(), has_errors)
}

fn severities(diagnostics: &[Diagnostic]) -> Vec<(Option<Lint>, DiagnosticSeverity)> {
    diagnostics
        .iter()
        .map(|diagnostic| (diagnostic.lint(), diagnostic.severity))
        .collect()
}

#[test]
fn lints_use_default_levels() {
    let (diagnostics, has_errors) = compile(|_| {});

    assert_eq!(
        severities(&diagnostics),
        vec![
            (Some(Lint::UnusedBinding), DiagnosticSeverity::Warning),
            (Some(Lint::DoubleNegation), DiagnosticSeverity::Warning),
        ]
    );
    assert!(!has_errors);
}

#[test]
fn allowed_lints_are_not_reported() {
    let (diagnostics, has_errors) =
        compile(|options| options.lints.set(Lint::UnusedBinding, LintLevel::Allow));

    assert_eq!(
        severities(&diagnostics),
        vec![(Some(Lint::DoubleNegation), DiagnosticSeverity::Warning)]
    );
    assert!(!has_errors);
}

#[test]
fn denied_lints_are_errors() {
    let (diagnostics, has_errors) =
        compile(|options| options.lints.set(Lint::DoubleNegation, LintLevel::Deny));

    assert_eq!(
        severities(&diagnostics),
        vec![
            (Some(Lint::UnusedBinding), DiagnosticSeverity::Warning),
            (Some(Lint::DoubleNegation), DiagnosticSeverity::Error),
        ]
    );
    assert!(has_errors);
}

#[test]
fn warned_lints_are_warnings() {
    let (diagnostics, has_errors) = compile(|options| {
        options.lints.set(Lint::DoubleNegation, LintLevel::Allow);
        options.lints.set(Lint::UnusedBinding, LintLevel::Warn);
    });

    assert_eq!(
        severities(&diagnostics),
        vec![(Some(Lint::UnusedBinding), DiagnosticSeverity::Warning)]
    );
    assert!(!has_errors);
}

#[test]
fn unnecessary_plus_only_applies_to_ints() {
    let (diagnostics, has_errors) = compile_source("fn main() -> Int {\n    +42\n}\n", |_| {});

    assert_eq!(
        severities(&diagnostics),
        vec![(Some(Lint::UnnecessaryPlus), DiagnosticSeverity::Warning)]
    );
    assert!(!has_errors);

    // `+` on anything else is a type error, so it isn't reported as having no effect
    let (diagnostics, has_errors) =
        compile_source("fn main() -> Int {\n    +\"hello\"\n}\n", |_| {});

    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.lint().is_none())
    );
    assert!(has_errors);
}

#[test]
fn constant_conditions_must_still_be_bools() {
    // `true` is always true, but it's a `Bool`, so it's only reported by the lint
    let (diagnostics, has_errors) =
        compile_source("fn main() -> Int {\n    assert true;\n    42\n}\n", |_| {});

    assert_eq!(
        severities(&diagnostics),
        vec![(Some(Lint::ConstantCondition), DiagnosticSeverity::Warning)]
    );
    assert!(!has_errors);

    // `1` is always true as well, but it isn't a `Bool`, so allowing the lint doesn't hide that
    let (diagnostics, has_errors) = compile_source(
        "fn main() -> Int {\n    assert 1;\n    42\n}\n",
        |options| {
            options.lints.set(Lint::ConstantCondition, LintLevel::Allow);
        },
    );

    assert_eq!(
        severities(&diagnostics),
        vec![(None, DiagnosticSeverity::Error)]
    );
    assert!(has_errors);
}

/// Renders each diagnostic as its severity, line and message, to check where lints are reported.
fn reports(source: &str, configure: impl FnOnce(&mut CompilerOptions)) -> (Vec<String>, bool) {
    let (diagnostics, has_errors) = compile_source(source, configure);

    let reports = diagnostics
        .iter()
        .map(|diagnostic| {
            format!(
                "{:?} {}: {}",
                diagnostic.severity,
                diagnostic.start().line + 1,
                diagnostic.kind
            )
        })
        .collect();

    (reports, has_errors)
}

#[test]
fn attributes_set_each_level() {
    let source = "\
fn main() -> Int {
    #[allow(unused_binding)]
    let a = 1;
    #[warn(unused_binding)]
    let b = 2;
    #[deny(unused_binding)]
    let c = 3;
    let d = 4;
    0
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Warning 5: Unused binding `b`",
            "Error 7: Unused binding `c`",
            "Warning 8: Unused binding `d`",
        ]
    );
    assert!(has_errors);
}

#[test]
fn nested_attributes_override_their_parents() {
    let source = "\
#[deny(unused_binding)]
mod outer {
    #[allow(unused_binding)]
    fn allowed() -> Int {
        let a = 1;
        #[warn(unused_binding)]
        let b = 2;
        3
    }

    fn denied() -> Int {
        let c = 1;
        2
    }

    export fn main() -> Int {
        allowed() + denied()
    }
}

fn main() -> Int {
    let d = 1;
    outer::main()
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Warning 7: Unused binding `b`",
            "Error 12: Unused binding `c`",
            "Warning 22: Unused binding `d`",
        ]
    );
    assert!(has_errors);
}

#[test]
fn attributes_override_project_levels() {
    let source = "\
#[warn(unused_binding)]
fn main() -> Int {
    let a = 1;
    --42
}
";

    let (reports, has_errors) = reports(source, |options| {
        options.lints.set(Lint::UnusedBinding, LintLevel::Allow);
        options.lints.set(Lint::DoubleNegation, LintLevel::Deny);
    });

    assert_eq!(
        reports,
        vec![
            "Warning 3: Unused binding `a`",
            "Error 4: Use of `--` in an expression will perform a double negation, but could be confused with the 'prefix decrement' operator in other languages",
        ]
    );
    assert!(has_errors);
}

#[test]
fn attributes_apply_to_items() {
    let source = "\
#[allow(unused_function, unused_parameter)]
fn helper(value: Int) -> Int {
    1
}

#[allow(unused_constant)]
const VALUE: Int = 1;

#[allow(unused_struct)]
struct Point {
    x: Int,
}

#[allow(unused_type_alias)]
type Number = Int;

fn unused() -> Int {
    1
}

fn main() -> Int {
    0
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(reports, vec!["Warning 17: Unused function `unused`"]);
    assert!(!has_errors);
}

#[test]
fn invalid_attributes_are_reported() {
    let source = "\
#[allow(unused_bindings, unused_function)]
fn helper() -> Int {
    1
}

#[expect(unused_function)]
fn other() -> Int {
    1
}

#[allow]
fn main() -> Int {
    let a = 1;
    #[allow(unused_value)]
    a
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Warning 1: Unknown lint `unused_bindings`",
            "Error 6: Unknown attribute `expect`",
            "Warning 7: Unused function `other`",
            "Error 11: Attribute `allow` expects a list of lint names",
            "Error 14: Attributes can only be used on items and statements",
        ]
    );
    assert!(has_errors);
}

#[test]
fn attributes_apply_to_statements_that_start_with_an_expression() {
    let source = "\
fn main() -> Int {
    let c = true;
    #[allow(inline_if_may_raise, constant_condition, unused_value)]
    inline if c { 1 } else { raise; };
    0
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert!(reports.is_empty(), "{reports:?}");
    assert!(!has_errors);
}

#[test]
fn attributes_after_a_parse_error_are_only_reported_once() {
    let (reports, has_errors) = reports("#[allow(unused_function)]\nlet x = 1;\n", |_| {});

    assert_eq!(reports, vec!["Error 2: Expected item, found `let`"]);
    assert!(has_errors);
}

#[test]
fn forbidden_lints_are_errors() {
    let (diagnostics, has_errors) =
        compile(|options| options.lints.set(Lint::DoubleNegation, LintLevel::Forbid));

    assert_eq!(
        severities(&diagnostics),
        vec![
            (Some(Lint::UnusedBinding), DiagnosticSeverity::Warning),
            (Some(Lint::DoubleNegation), DiagnosticSeverity::Error),
        ]
    );
    assert!(has_errors);
}

#[test]
fn attributes_cannot_change_lints_forbidden_by_the_project() {
    let source = "\
#[allow(unused_binding)]
mod outer {
    #[warn(unused_binding, double_negation)]
    export fn main() -> Int {
        #[deny(unused_binding)]
        let a = 1;
        #[forbid(unused_binding)]
        let b = 2;
        --42
    }
}

fn main() -> Int {
    outer::main()
}
";

    let (reports, has_errors) = reports(source, |options| {
        options.lints.set(Lint::UnusedBinding, LintLevel::Forbid);
    });

    assert_eq!(
        reports,
        vec![
            "Error 1: Cannot set lint `unused_binding` to `allow`, since it's forbidden by the project",
            "Error 3: Cannot set lint `unused_binding` to `warn`, since it's forbidden by the project",
            "Error 5: Cannot set lint `unused_binding` to `deny`, since it's forbidden by the project",
            "Error 6: Unused binding `a`",
            "Error 8: Unused binding `b`",
            "Warning 9: Use of `--` in an expression will perform a double negation, but could be confused with the 'prefix decrement' operator in other languages",
        ]
    );
    assert!(has_errors);
}

#[test]
fn attributes_cannot_change_lints_forbidden_by_an_enclosing_attribute() {
    let source = "\
#[forbid(unused_binding)]
mod outer {
    #[allow(unused_function)]
    mod inner {
        #[allow(unused_binding)]
        fn helper() -> Int {
            #[warn(unused_binding)]
            let a = 1;
            2
        }
    }

    #[allow(unused_binding)]
    export fn main() -> Int {
        let b = 1;
        2
    }
}

#[allow(unused_binding)]
fn main() -> Int {
    let c = 1;
    #[forbid(unused_binding)]
    let d = 2;
    outer::main()
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Error 5: Cannot set lint `unused_binding` to `allow`, since it's forbidden by the attribute on line 1",
            "Error 7: Cannot set lint `unused_binding` to `warn`, since it's forbidden by the attribute on line 1",
            "Error 8: Unused binding `a`",
            "Error 13: Cannot set lint `unused_binding` to `allow`, since it's forbidden by the attribute on line 1",
            "Error 15: Unused binding `b`",
            "Error 24: Unused binding `d`",
        ]
    );
    assert!(has_errors);
}

#[test]
fn forbidden_lint_errors_point_to_the_innermost_forbid() {
    let source = "\
#[allow(unused_binding)]
mod outer {
    #[forbid(unused_binding)]
    export fn main() -> Int {
        #[allow(unused_binding)]
        let a = 1;
        2
    }
}

fn main() -> Int {
    outer::main()
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Error 5: Cannot set lint `unused_binding` to `allow`, since it's forbidden by the attribute on line 3",
            "Error 6: Unused binding `a`",
        ]
    );
    assert!(has_errors);
}
