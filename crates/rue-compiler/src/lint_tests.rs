use rue_diagnostic::{
    Applicability, Diagnostic, DiagnosticSeverity, Lint, LintGroup, LintLevel, LintName,
    apply_edits,
};
use rue_options::CompilerOptions;

use crate::test_utils::compile_with;

const SOURCE: &str = "fn main() -> Int {\n    let unused = 1;\n    --42\n}\n";

fn compile(configure: impl FnOnce(&mut CompilerOptions)) -> (Vec<Diagnostic>, bool) {
    compile_source(SOURCE, configure)
}

fn compile_source(
    source: &str,
    configure: impl FnOnce(&mut CompilerOptions),
) -> (Vec<Diagnostic>, bool) {
    let diagnostics = compile_with(source, configure).diagnostics;
    let has_errors = diagnostics
        .iter()
        .any(|diagnostic| diagnostic.severity == DiagnosticSeverity::Error);
    (diagnostics, has_errors)
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

#[warning(unused_function)]
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
            "Error 6: Unknown attribute `warning`",
            "Warning 7: Unused function `other`",
            "Error 11: Attribute `allow` expects a list of lint names",
            "Warning 13: Binding `a` is returned directly after it's declared",
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
fn stacked_attributes_apply_to_statements_that_start_with_an_expression() {
    let source = "\
fn main() -> Int {
    let c = true;
    #[allow(inline_if_may_raise)]
    #[allow(constant_condition)] #[allow(unused_value)]
    inline if c { 1 } else { raise; };
    0
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert!(reports.is_empty(), "{reports:?}");
    assert!(!has_errors);
}

#[test]
fn stacked_attributes_before_a_trailing_expression_are_reported() {
    let source = "\
fn main() -> Int {
    let a = 1;
    #[allow(unused_value)]
    #[allow(unused_binding)]
    a
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Warning 2: Binding `a` is returned directly after it's declared",
            "Error 3: Attributes can only be used on items and statements",
            "Error 4: Attributes can only be used on items and statements",
        ]
    );
    assert!(has_errors);
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

#[test]
fn stacked_attributes_cannot_change_a_lint_forbidden_by_an_earlier_one() {
    let source = "\
#[forbid(unused_binding)]
#[allow(unused_binding)]
fn main() -> Int {
    let a = 1;
    2
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Error 2: Cannot set lint `unused_binding` to `allow`, since it's forbidden by the attribute on line 1",
            "Error 4: Unused binding `a`",
        ]
    );
    assert!(has_errors);
}

#[test]
fn stacked_attributes_apply_to_the_same_item() {
    let source = "\
#[allow(unused_function)]
#[allow(unused_parameter)]
fn helper(value: Int) -> Int {
    1
}

#[allow(unused_function)] #[allow(unused_parameter)]
fn other(value: Int) -> Int {
    1
}

fn main() -> Int {
    #[allow(unused_binding)]
    #[deny(unused_binding)]
    let a = 1;
    #[deny(unused_binding)] #[allow(unused_binding)]
    let b = 2;
    0
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(reports, vec!["Error 15: Unused binding `a`"]);
    assert!(has_errors);
}

#[test]
fn groups_set_the_level_of_each_lint_in_them() {
    let (reports, has_errors) = reports(SOURCE, |options| {
        options
            .lints
            .apply(&[(LintName::Group(LintGroup::Unused), LintLevel::Deny)]);
    });

    assert_eq!(
        reports,
        vec![
            "Error 2: Unused binding `unused`",
            "Warning 3: Use of `--` in an expression will perform a double negation, but could be confused with the 'prefix decrement' operator in other languages",
        ]
    );
    assert!(has_errors);
}

#[test]
fn groups_in_attributes() {
    let source = "\
#[allow(unused)]
fn helper(value: Int) -> Int {
    let a = 1;
    #[deny(unused_binding)]
    let b = 2;
    3
}

#[allow(unused_binding)]
fn other() -> Int {
    let c = 1;
    #[warn(unused)]
    let d = 2;
    3
}

// On the same item or statement, the later attribute wins, whether it names a lint or a group
#[allow(unused)]
#[deny(unused_binding)]
fn main() -> Int {
    let e = 1;
    #[deny(unused_binding)]
    #[allow(unused)]
    let f = 2;
    other()
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Error 5: Unused binding `b`",
            "Warning 13: Unused binding `d`",
            "Error 21: Unused binding `e`",
        ]
    );
    assert!(has_errors);
}

#[test]
fn group_attributes_override_project_levels() {
    let source = "\
#[warn(unused)]
fn main() -> Int {
    let a = 1;
    #[allow(unused)]
    let b = 2;
    0
}
";

    let (reports, has_errors) = reports(source, |options| {
        options.lints.apply(&[
            (LintName::Group(LintGroup::Unused), LintLevel::Deny),
            (LintName::Lint(Lint::UnusedBinding), LintLevel::Allow),
        ]);
    });

    assert_eq!(reports, vec!["Warning 3: Unused binding `a`"]);
    assert!(!has_errors);
}

#[test]
fn unknown_group_names_are_reported() {
    let source = "\
#[allow(unuse)]
fn main() -> Int {
    let a = 1;
    0
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Warning 1: Unknown lint `unuse`",
            "Warning 3: Unused binding `a`",
        ]
    );
    assert!(!has_errors);
}

#[test]
fn group_attributes_cannot_change_forbidden_lints() {
    let source = "\
#[allow(unused)]
fn helper(value: Int) -> Int {
    let a = 1;
    2
}

#[forbid(unused)]
fn main() -> Int {
    #[allow(unused_binding)]
    let b = 1;
    #[forbid(unused_binding)]
    #[warn(unused)]
    let c = 2;
    0
}
";

    let (reports, has_errors) = reports(source, |options| {
        options.lints.set(Lint::UnusedBinding, LintLevel::Forbid);
    });

    // A group sets the lints in it that aren't forbidden, and reports the first one that is
    assert_eq!(
        reports,
        vec![
            "Error 1: Cannot set lint group `unused` to `allow`, since `unused_binding` is forbidden by the project",
            "Error 3: Unused binding `a`",
            "Error 9: Cannot set lint `unused_binding` to `allow`, since it's forbidden by the attribute on line 7",
            "Error 10: Unused binding `b`",
            "Error 12: Cannot set lint group `unused` to `warn`, since `unused_function` is forbidden by the attribute on line 7",
            "Error 13: Unused binding `c`",
        ]
    );
    assert!(has_errors);
}

#[test]
fn group_attributes_enable_lints_the_project_allows() {
    let source = "\
#[warn(unused)]
mod helpers {
    fn helper(value: Int) -> Int {
        1
    }
}

fn main() -> Int {
    #[deny(unused)]
    let a = 1;
    let b = 2;
    0
}
";

    // The unused pass is skipped when none of its lints are enabled, so it must still run when
    // they're only enabled by a group attribute
    let (reports, has_errors) = reports(source, |options| {
        options
            .lints
            .apply(&[(LintName::Group(LintGroup::Unused), LintLevel::Allow)]);
    });

    assert_eq!(
        reports,
        vec![
            "Warning 3: Unused function `helper`",
            "Warning 3: Unused parameter `value`",
            "Error 10: Unused binding `a`",
        ]
    );
    assert!(has_errors);
}

#[test]
fn group_attributes_cannot_change_lints_forbidden_on_the_same_item() {
    let source = "\
#[forbid(unused_binding)]
#[allow(unused)]
fn helper(value: Int) -> Int {
    let a = 1;
    2
}

fn main() -> Int {
    0
}
";

    let (reports, has_errors) = reports(source, |_| {});

    // The rest of the group is still allowed
    assert_eq!(
        reports,
        vec![
            "Error 2: Cannot set lint group `unused` to `allow`, since `unused_binding` is forbidden by the attribute on line 1",
            "Error 4: Unused binding `a`",
        ]
    );
    assert!(has_errors);
}

#[test]
fn expectations_are_reported_if_unfulfilled() {
    let source = "\
#[expect(unused_function)]
fn helper() -> Int {
    1
}

#[expect(unused_function, unused_binding)]
fn main() -> Int {
    #[expect(unused_binding)]
    let a = 1;
    #[expect(double_negation)]
    let b = 2;
    --b
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Warning 6: Expected lint `unused_function` to be reported here, but it never was",
            "Warning 6: Expected lint `unused_binding` to be reported here, but it never was",
            "Warning 10: Expected lint `double_negation` to be reported here, but it never was",
            "Warning 12: Use of `--` in an expression will perform a double negation, but could be confused with the 'prefix decrement' operator in other languages",
        ]
    );
    assert!(!has_errors);
}

#[test]
fn nested_expectations_are_fulfilled_by_the_innermost() {
    let source = "\
#[expect(unused_binding)]
fn first() -> Int {
    #[expect(unused_binding)]
    let a = 1;
    0
}

#[expect(unused_binding)]
fn second() -> Int {
    let b = 1;
    #[expect(unused_binding)]
    let c = 2;
    0
}

#[expect(unused_binding)]
fn third() -> Int {
    #[warn(unused_binding)]
    let d = 1;
    0
}

#[expect(unused_binding)]
fn fourth() -> Int {
    #[allow(unused_binding)]
    let e = 1;
    0
}

fn main() -> Int {
    first() + second() + third() + fourth()
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Warning 1: Expected lint `unused_binding` to be reported here, but it never was",
            "Warning 16: Expected lint `unused_binding` to be reported here, but it never was",
            "Warning 19: Unused binding `d`",
            "Warning 23: Expected lint `unused_binding` to be reported here, but it never was",
        ]
    );
    assert!(!has_errors);
}

#[test]
fn expectations_apply_to_lints_allowed_by_the_project() {
    let source = "\
fn main() -> Int {
    #[expect(unused_binding)]
    let a = 1;
    #[expect(unused_binding)]
    let b = 2;
    let c = 3;
    #[expect(double_negation)]
    let d = --b;
    d
}
";

    let (reports, has_errors) = reports(source, |options| {
        options.lints.set(Lint::UnusedBinding, LintLevel::Allow);
        options.lints.set(Lint::DoubleNegation, LintLevel::Allow);
    });

    assert_eq!(
        reports,
        vec![
            "Warning 4: Expected lint `unused_binding` to be reported here, but it never was",
            "Warning 7: Binding `d` is returned directly after it's declared",
        ]
    );
    assert!(!has_errors);
}

#[test]
fn unfulfilled_expectations_can_be_configured() {
    let source = "\
#[allow(unfulfilled_lint_expectation)]
fn helper() -> Int {
    #[expect(unused_binding)]
    let a = 1;
    a
}

fn main() -> Int {
    #[expect(unused_binding)]
    let b = 1;
    helper() + b
}
";

    let (reports, has_errors) = reports(source, |options| {
        options
            .lints
            .set(Lint::UnfulfilledLintExpectation, LintLevel::Deny);
    });

    assert_eq!(
        reports,
        vec![
            "Warning 3: Binding `a` is returned directly after it's declared",
            "Error 9: Expected lint `unused_binding` to be reported here, but it never was",
        ]
    );
    assert!(has_errors);
}

#[test]
fn duplicate_expectations_are_fulfilled_together() {
    let source = "\
fn main() -> Int {
    #[expect(unused_binding, unused_binding)]
    let a = 1;
    #[expect(unused_binding)]
    #[expect(unused_binding)]
    let b = 2;
    #[expect(unused_binding)]
    #[allow(unused_binding)]
    let c = 3;
    #[expect(unused_binding, unused_binding)]
    let d = 4;
    #[expect(unused_binding)]
    #[warn(unused_binding)]
    #[expect(unused_binding)]
    let e = 5;
    d
}
";

    let (reports, has_errors) = reports(source, |_| {});

    // The `allow` and `warn` override the `expect` before them, so those are never fulfilled
    assert_eq!(
        reports,
        vec![
            "Warning 7: Expected lint `unused_binding` to be reported here, but it never was",
            "Warning 10: Expected lint `unused_binding` to be reported here, but it never was",
            "Warning 10: Expected lint `unused_binding` to be reported here, but it never was",
            "Warning 12: Expected lint `unused_binding` to be reported here, but it never was",
        ]
    );
    assert!(!has_errors);
}

#[test]
fn unfulfilled_lint_expectation_cannot_be_expected() {
    let source = "\
#[expect(unfulfilled_lint_expectation)]
fn helper() -> Int {
    #[expect(unused_binding)]
    let a = 1;
    a
}

#[allow(unfulfilled_lint_expectation)]
#[expect(unfulfilled_lint_expectation)]
fn main() -> Int {
    helper()
}
";

    let (reports, has_errors) = reports(source, |_| {});

    // The expectation is always reported, and doesn't allow the lint inside of it, but it can
    // still be allowed by another attribute
    assert_eq!(
        reports,
        vec![
            "Warning 1: Lint `unfulfilled_lint_expectation` can't be expected, so this is always reported",
            "Warning 3: Binding `a` is returned directly after it's declared",
            "Warning 3: Expected lint `unused_binding` to be reported here, but it never was",
        ]
    );
    assert!(!has_errors);
}

#[test]
fn expectations_are_not_checked_if_compilation_fails() {
    let source = "\
fn main() -> Int {
    #[expect(unnecessary_cast)]
    let a = 1 as Int;
    #[expect(unused_binding)]
    let b = 2;
    a + missing
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(reports, vec!["Error 6: Undeclared symbol `missing`"]);
    assert!(has_errors);
}

#[test]
fn invalid_expect_attributes_are_reported() {
    let source = "\
#[expect]
fn helper() -> Int {
    1
}

#[expect(unused_bindings)]
fn main() -> Int {
    #[expect(unused_binding)]
    let a = 1;
    #[expect(unused_value)]
    a
}
";

    let (reports, has_errors) = reports(source, |_| {});

    // As in Rust, invalid attributes are errors, which stop expectations from being checked
    assert_eq!(
        reports,
        vec![
            "Error 1: Attribute `expect` expects a list of lint names",
            "Warning 2: Unused function `helper`",
            "Warning 6: Unknown lint `unused_bindings`",
            "Warning 8: Binding `a` is returned directly after it's declared",
            "Error 10: Attributes can only be used on items and statements",
        ]
    );
    assert!(has_errors);
}

#[test]
fn group_expectations_are_fulfilled_by_any_lint_in_them() {
    let source = "\
#[expect(unused)]
fn helper(value: Int) -> Int {
    1
}

#[expect(unused)]
fn other() -> Int {
    1
}

fn main() -> Int {
    #[expect(unused, unused_binding)]
    let a = 1;
    helper(1) + other()
}
";

    let (reports, has_errors) = reports(source, |_| {});

    // Expectations on the same statement are fulfilled together, whether they name a group or a
    // lint in it
    assert_eq!(
        reports,
        vec![
            "Warning 6: Expected a lint in group `unused` to be reported here, but none of them were"
        ]
    );
    assert!(!has_errors);
}

#[test]
fn expectations_cannot_change_forbidden_lints() {
    let source = "\
#[forbid(unused_binding)]
fn main() -> Int {
    #[expect(unused_binding)]
    let a = 1;
    #[expect(unused)]
    let b = 2;
    0
}
";

    let (reports, has_errors) = reports(source, |_| {});

    // The group's expectation isn't checked, since the attribute is already an error
    assert_eq!(
        reports,
        vec![
            "Error 3: Cannot set lint `unused_binding` to `expect`, since it's forbidden by the attribute on line 1",
            "Error 4: Unused binding `a`",
            "Error 5: Cannot set lint group `unused` to `expect`, since `unused_binding` is forbidden by the attribute on line 1",
            "Error 6: Unused binding `b`",
        ]
    );
    assert!(has_errors);
}

#[test]
fn other_attributes_on_an_expectation_set_the_level_of_unfulfilled_expectations() {
    let source = "\
fn main() -> Int {
    #[allow(unfulfilled_lint_expectation)]
    #[expect(unused_binding)]
    let a = 1;
    #[deny(unfulfilled_lint_expectation)]
    #[expect(unused_binding)]
    let b = 2;
    a + b
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec!["Error 6: Expected lint `unused_binding` to be reported here, but it never was"]
    );
    assert!(has_errors);
}

#[test]
fn only_errors_from_lints_allow_expectations_to_be_checked() {
    let denied = "\
#[deny(unused_binding)]
fn helper() -> Int {
    let a = 1;
    0
}

fn main() -> Int {
    #[expect(unused_binding)]
    let b = 1;
    helper() + b
}
";

    let (denied_reports, has_errors) = reports(denied, |_| {});

    assert_eq!(
        denied_reports,
        vec![
            "Error 3: Unused binding `a`",
            "Warning 8: Expected lint `unused_binding` to be reported here, but it never was",
        ]
    );
    assert!(has_errors);

    let forbidden = "\
#[forbid(unused_binding)]
fn helper() -> Int {
    #[allow(unused_binding)]
    let a = 1;
    a
}

fn main() -> Int {
    #[expect(unused_binding)]
    let b = 1;
    helper() + b
}
";

    let (reports, has_errors) = reports(forbidden, |_| {});

    assert_eq!(
        reports,
        vec![
            "Warning 3: Binding `a` is returned directly after it's declared",
            "Error 3: Cannot set lint `unused_binding` to `allow`, since it's forbidden by the attribute on line 1",
        ]
    );
    assert!(has_errors);
}

#[test]
fn inner_attributes_apply_to_the_whole_file() {
    let source = "\
// The file's header comment
#![deny(unused_binding)]
#![allow(unused_function)]

fn unused() -> Int {
    let a = 1;
    2
}

#[warn(unused_binding)]
fn main() -> Int {
    let b = 1;
    #[allow(unused_binding)]
    let c = 2;
    0
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Error 6: Unused binding `a`",
            "Warning 12: Unused binding `b`",
        ]
    );
    assert!(has_errors);
}

#[test]
fn inner_attributes_override_project_levels() {
    let source = "\
#![warn(unused_binding)]

fn main() -> Int {
    let a = 1;
    0
}
";

    let (reports, has_errors) = reports(source, |options| {
        options.lints.set(Lint::UnusedBinding, LintLevel::Allow);
    });

    assert_eq!(reports, vec!["Warning 4: Unused binding `a`"]);
    assert!(!has_errors);
}

#[test]
fn inner_attributes_apply_to_the_whole_module() {
    let source = "\
#![deny(unused_binding)]

#[warn(unused_binding)]
mod outer {
    #![allow(unused_binding)]

    export fn main() -> Int {
        let a = 1;
        #[deny(unused_binding)]
        let b = 2;
        0
    }
}

fn main() -> Int {
    let c = 1;
    outer::main()
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Error 10: Unused binding `b`",
            "Error 16: Unused binding `c`",
        ]
    );
    assert!(has_errors);
}

#[test]
fn misplaced_inner_attributes_are_reported() {
    let source = "\
fn helper() -> Int {
    1
}

#![allow(unused_function)]
fn other() -> Int {
    #![allow(unused_binding)]
    let a = 1;
    2
}

#[allow(unused_function)] #![allow(unused_binding)]
fn third() -> Int {
    1
}

#![allow(unused_function)]
mod inner {
    export fn main() -> Int {
        0
    }
}

fn main() -> Int {
    helper() + inner::main()
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Error 5: Inner attributes can only be used at the start of a file or module",
            "Warning 6: Unused function `other`",
            "Error 7: Inner attributes can only be used at the start of a file or module",
            "Warning 8: Unused binding `a`",
            "Error 12: Inner attributes can only be used at the start of a file or module",
            "Error 17: Inner attributes can only be used at the start of a file or module",
        ]
    );
    assert!(has_errors);
}

#[test]
fn misplaced_inner_attributes_are_still_checked() {
    let source = "\
fn main() -> Int {
    #![allw(unused_binding)]
    #![allow(unused_bindng)]
    let a = 1;
    0
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Error 2: Inner attributes can only be used at the start of a file or module",
            "Error 2: Unknown attribute `allw`",
            "Error 3: Inner attributes can only be used at the start of a file or module",
            "Warning 3: Unknown lint `unused_bindng`",
            "Warning 4: Unused binding `a`",
        ]
    );
    assert!(has_errors);
}

#[test]
fn inner_attributes_after_a_parse_error_are_only_reported_once() {
    let (reports, has_errors) = reports(
        "fn main() -> Int {\n    0\n}\n#![allow(unused_function)]\nlet x = 1;\n",
        |_| {},
    );

    assert_eq!(reports, vec!["Error 5: Expected item, found `let`"]);
    assert!(has_errors);
}

#[test]
fn attributes_after_a_parse_error_in_a_module_are_only_reported_once() {
    let source = "\
mod m {
    export fn main() -> Int {
        0
    }

    #![allow(unused_binding)]
    #[allow(unused_binding)]
    let x = 1;
}

fn main() -> Int {
    m::main()
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(reports, vec!["Error 8: Expected item, found `let`"]);
    assert!(has_errors);
}

#[test]
fn misplaced_outer_attributes_are_still_checked() {
    let source = "\
fn main() -> Int {
    let a = 1;
    #[alow(unused_value)] #[allow(unused_bindng)]
    a
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Warning 2: Binding `a` is returned directly after it's declared",
            "Error 3: Attributes can only be used on items and statements",
            "Error 3: Unknown attribute `alow`",
            "Error 3: Attributes can only be used on items and statements",
            "Warning 3: Unknown lint `unused_bindng`",
        ]
    );
    assert!(has_errors);
}

#[test]
fn inner_attributes_can_forbid_lints() {
    let source = "\
#![allow(unused_binding)]
#![forbid(unused_binding)]
#![allow(unused_binding)]

#[allow(unused_binding)]
fn main() -> Int {
    let a = 1;
    0
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Error 3: Cannot set lint `unused_binding` to `allow`, since it's forbidden by the attribute on line 2",
            "Error 5: Cannot set lint `unused_binding` to `allow`, since it's forbidden by the attribute on line 2",
            "Error 7: Unused binding `a`",
        ]
    );
    assert!(has_errors);
}

#[test]
fn inner_attributes_cant_change_lints_forbidden_outside_of_them() {
    let source = "\
#[forbid(unused_binding)]
mod m {
    #![allow(unused_binding)]

    export fn main() -> Int {
        let a = 1;
        0
    }
}

fn main() -> Int {
    m::main()
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Error 3: Cannot set lint `unused_binding` to `allow`, since it's forbidden by the attribute on line 1",
            "Error 6: Unused binding `a`",
        ]
    );
    assert!(has_errors);
}

#[test]
fn inner_attributes_cant_change_lints_forbidden_by_the_project() {
    let (reports, has_errors) = reports("#![allow(unused_function)]\n", |options| {
        options.lints.set(Lint::UnusedFunction, LintLevel::Forbid);
    });

    assert_eq!(
        reports,
        vec![
            "Error 1: Cannot set lint `unused_function` to `allow`, since it's forbidden by the project"
        ]
    );
    assert!(has_errors);
}

#[test]
fn inner_attributes_can_expect_lints() {
    let source = "\
#![expect(unused_binding)]

#[expect(unused_binding)]
mod m {
    #![expect(unused_binding)]

    export fn main() -> Int {
        let a = 1;
        0
    }
}

fn main() -> Int {
    m::main()
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec!["Warning 1: Expected lint `unused_binding` to be reported here, but it never was"]
    );
    assert!(!has_errors);
}

#[test]
fn inner_attributes_can_set_groups() {
    let source = "\
#![allow(unused)]

fn unused(value: Int) -> Int {
    let a = 1;
    0
}

mod m {
    #![expect(unused)]

    fn helper() -> Int {
        1
    }
}

fn main() -> Int {
    0
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert!(reports.is_empty(), "{reports:?}");
    assert!(!has_errors);
}

#[test]
fn hir_passes_visit_every_expression() {
    let source = "\
struct Point {
    x: Int = if true { 1 } else { 2 },
}

const VALUE: Int = const { if false { 1 } else { 2 } };

fn main() -> Int {
    let a = Point {};
    let b = Point {};
    let f = fn() => if true { 1 } else { 2 };
    let c = if true { 1 } else { 2 };
    a.x + b.x + f() + VALUE + c + (+1)
}
";

    let (reports, has_errors) = reports(source, |_| {});

    // The struct field default is used twice, but it's only reported once
    assert_eq!(
        reports,
        vec![
            "Warning 2: Condition always evaluates to `true`",
            "Warning 5: Condition always evaluates to `false`",
            "Warning 10: Condition always evaluates to `true`",
            "Warning 11: Condition always evaluates to `true`",
            "Warning 12: Unnecessary `+` operator, since it has no effect",
        ]
    );
    assert!(!has_errors);
}

#[test]
fn nested_casts_are_linted_separately() {
    let source = "\
fn main(value: Int) -> Int {
    (value as Int) as Int
}
";

    let (reports, has_errors) = reports(source, |_| {});

    // Each cast has a HIR node of its own, so both are reported
    assert_eq!(
        reports,
        vec![
            "Warning 2: Unnecessary cast from `Int` to `Int`, since they are compatible with each other",
            "Warning 2: Unnecessary cast from `Int` to `Int`, since they are compatible with each other",
        ]
    );
    assert!(!has_errors);
}

#[test]
fn returns_in_function_bodies_are_lints() {
    let source = "\
fn main() -> Int {
    #[allow(unnecessary_return)]
    return 1;
}

fn early() -> Int {
    return 1;
    2
}

fn block() -> Int {
    {
        return 1;
    }
}

test fn tests() {
    assert main() == early();
    assert block() == 1;
}
";

    let (reports, has_errors) = reports(source, |_| {});

    // A `return` in an expression block would only exit the block, so it's still an error
    assert_eq!(
        reports,
        vec![
            "Warning 7: Unnecessary `return` statement, since the function body can end in an expression instead",
            "Warning 8: Unreachable code",
            "Error 13: Cannot use a `return` statement here, end the block with an expression instead",
        ]
    );
    assert!(has_errors);
}

#[test]
fn unknown_lints_are_lints() {
    let source = "\
#[allow(typo)]
fn main() -> Int {
    #[allow(unknown_lint, other_typo)]
    let _a = 1;
    0
}
";

    let (reports, has_errors) = reports(source, |_| {});

    // The attribute that allows `unknown_lint` also allows it for the other names in itself
    assert_eq!(reports, vec!["Warning 1: Unknown lint `typo`"]);
    assert!(!has_errors);

    let (reports, has_errors) = reports_with_level(source, Lint::UnknownLint, LintLevel::Deny);

    assert_eq!(reports, vec!["Error 1: Unknown lint `typo`"]);
    assert!(has_errors);
}

fn reports_with_level(source: &str, lint: Lint, level: LintLevel) -> (Vec<String>, bool) {
    reports(source, |options| options.lints.set(lint, level))
}

/// Applies every suggestion that can be applied automatically.
fn fix(source: &str) -> String {
    let (diagnostics, _) = compile_source(source, |_| {});

    let edits = diagnostics
        .iter()
        .flat_map(|diagnostic| &diagnostic.suggestions)
        .filter(|suggestion| suggestion.applicability == Applicability::MachineApplicable)
        .flat_map(|suggestion| &suggestion.edits)
        .collect::<Vec<_>>();

    apply_edits(source, &edits)
}

#[test]
fn suggestions_fix_lints() {
    let source = "\
fn main() -> Int {
    let value = +42;
    double(value) as Int
}

fn double(value: Int) -> Int {
    let doubled = value * (+2);
    return doubled;
}

fn early(value: Int) -> Int {
    return value;
    0
}
";

    let fixed = fix(source);

    // The `return` in `early` isn't the end of the body, so removing it would change the result
    assert_eq!(
        fixed,
        "\
fn main() -> Int {
    let value = 42;
    double(value)
}

fn double(value: Int) -> Int {
    let doubled = value * 2;
    doubled
}

fn early(value: Int) -> Int {
    return value;
    0
}
"
    );

    let (reports, has_errors) = reports(&fixed, |_| {});

    assert_eq!(
        reports,
        vec![
            "Warning 7: Binding `doubled` is returned directly after it's declared",
            "Warning 11: Unused function `early`",
            "Warning 12: Unnecessary `return` statement, since the function body can end in an expression instead",
            "Warning 13: Unreachable code",
        ]
    );
    assert!(!has_errors);
}

#[test]
fn unreachable_code_is_reported_once_at_its_start() {
    let source = "\
fn main() -> Int {
    allowed() + expected() + nested() + unused()
}

fn allowed() -> Int {
    raise \"x\";
    #[allow(unreachable_code)]
    let a = 1;
    a
}

fn expected() -> Int {
    raise \"x\";
    #[expect(unreachable_code)]
    let a = 1;
    a
}

fn nested() -> Int {
    raise \"x\";
    let a = {
        raise \"y\";
        1
    };
    a
}

fn unused() -> Int {
    let _a = {
        raise \"x\";
    };
    1
}
";

    let (reports, has_errors) = reports(source, |_| {});

    // The block inside the unreachable code isn't reported separately, and an unused binding is
    // never evaluated, so the code after it is reachable
    assert_eq!(
        reports,
        vec![
            "Warning 7: Binding `a` is returned directly after it's declared",
            "Warning 14: Binding `a` is returned directly after it's declared",
            "Warning 21: Unreachable code",
            "Warning 21: Binding `a` is returned directly after it's declared",
        ]
    );
    assert!(!has_errors);
}

#[test]
fn returns_with_attributes_are_not_fixed() {
    let source = "\
fn main() -> Int {
    let value = 1;
    #[allow(unused_value)]
    return value;
}
";

    // The attribute can't be kept on an expression, and removing it would change what's reported
    assert_eq!(fix(source), source);
}

#[test]
fn style_suggestions_fix_lints() {
    let source = "\
struct Point {
    x: Int,
    y: Int,
}

fn main(flag: Bool, value: Int | nil) -> Int {
    if value is nil {
        raise;
    }

    if flag == false {
        raise;
    }

    let x = (value + 1);
    let point = Point { x: x, y: 2 };
    let positive = if point.x > 0 { false } else { true };
    let checked = flag && if positive || flag { true } else { false };

    let result = if checked {
        point.y
    } else {
        if positive { 1 } else { 2 }
    };
    result
}
";

    // Suggestions that overlap are applied on the next pass, as they would be by an editor
    let fixed = fix(&fix(source));

    assert_eq!(
        fixed,
        "\
struct Point {
    x: Int,
    y: Int,
}

fn main(flag: Bool, value: Int | nil) -> Int {
    assert !(value is nil);

    assert flag;

    let x = value + 1;
    let point = Point { x, y: 2 };
    let positive = !(point.x > 0);
    let checked = flag && (positive || flag);

    if checked {
        point.y
    } else if positive { 1 } else { 2 }
}
"
    );

    let (reports, has_errors) = reports(&fixed, |_| {});

    assert_eq!(reports, Vec::<String>::new());
    assert!(!has_errors);
}

#[test]
fn removal_suggestions_fix_lints() {
    let source = "\
inline const ONCE: Int = 1;

mod helpers {
    export fn inner() -> Int {
        1
    }

    export fn outer() -> Int {
        inner() + 1
    }
}

fn main(value: Int) -> Int {
    inline let doubled = value * 2;
    debug value;
    doubled + ONCE + helpers::outer()
}
";

    let (diagnostics, _) = compile_source(source, |options| {
        options.lints.set(Lint::DebugStatement, LintLevel::Warn);
    });

    let edits = diagnostics
        .iter()
        .flat_map(|diagnostic| &diagnostic.suggestions)
        .filter(|suggestion| suggestion.applicability == Applicability::MachineApplicable)
        .flat_map(|suggestion| &suggestion.edits)
        .collect::<Vec<_>>();

    let fixed = apply_edits(source, &edits);

    assert_eq!(
        fixed,
        "\
inline const ONCE: Int = 1;

mod helpers {
    fn inner() -> Int {
        1
    }

    export fn outer() -> Int {
        inner() + 1
    }
}

fn main(value: Int) -> Int {
    inline let doubled = value * 2;
    doubled + ONCE + helpers::outer()
}
"
    );

    let (reports, has_errors) = reports(&fixed, |options| {
        options.lints.set(Lint::DebugStatement, LintLevel::Warn);
    });

    assert_eq!(reports, Vec::<String>::new());
    assert!(!has_errors);
}

#[test]
fn negations_are_parenthesized_as_operands() {
    let source = "\
fn main(flag: Bool, a: Bool, b: Bool) -> Bool {
    let compared = flag && !(a || b) == false;
    let flipped = flag && (a == b) == false;
    let needless = flag && if !(!a || b) { false } else { true };
    let cast = (flag == false) as Int;
    let guarded = (if flag { false } else { true }) is Bool;
    compared && flipped && needless && cast == 1 && guarded
}
";

    // Removing the comparison or the `if` can't let the operators around it split up what's left
    // of it, including a postfix operator such as `as`, which binds more tightly than `!`
    assert_eq!(
        fix(source),
        "\
fn main(flag: Bool, a: Bool, b: Bool) -> Bool {
    let compared = flag && (a || b);
    let flipped = flag && (a != b);
    let needless = flag && (!a || b);
    let cast = (!flag) as Int;
    let guarded = (!flag) is Bool;
    compared && flipped && needless && cast == 1 && guarded
}
"
    );
}

#[test]
fn conditions_keep_parentheses_around_struct_initializers() {
    let source = "\
struct Point {
    x: Int,
    s: String,
}

fn main(a: Int, s: String) -> Int {
    if (Point { x: a, s }.x == 1) == true {
        return 1;
    }

    // A brace in a string isn't a struct initializer, and strings don't have escapes
    if (s == \"{\") == true {
        return 8;
    }

    if (\"\\\" != Point { x: a, s }.s) == false {
        return 10;
    }

    if (Point { x: a, s }.x == 5 == true) {
        return 5;
    }

    if (Point { x: a, s }.x != 6 == false) {
        return 6;
    }

    if (a != 7) == false {
        return 7;
    }

    let compared = if (Point { x: a, s }.x == 2) == true { 1 } else { 2 };
    let needless = if if (Point { x: a, s }.x == 3) { true } else { false } { 1 } else { 2 };
    let negated = if !(Point { x: a, s }.x == 4) == false { 1 } else { 2 };
    compared + needless + negated
}
";

    // A struct initializer can't be written in a condition without parentheses
    let fixed = fix(source);

    assert_eq!(
        fixed,
        "\
struct Point {
    x: Int,
    s: String,
}

fn main(a: Int, s: String) -> Int {
    if (Point { x: a, s }.x == 1) {
        return 1;
    }

    // A brace in a string isn't a struct initializer, and strings don't have escapes
    if s == \"{\" {
        return 8;
    }

    if (\"\\\" == Point { x: a, s }.s) {
        return 10;
    }

    if (Point { x: a, s }.x == 5) {
        return 5;
    }

    if (Point { x: a, s }.x == 6) {
        return 6;
    }

    if a == 7 {
        return 7;
    }

    let compared = if (Point { x: a, s }.x == 2) { 1 } else { 2 };
    let needless = if (Point { x: a, s }.x == 3) { 1 } else { 2 };
    let negated = if (Point { x: a, s }.x == 4) { 1 } else { 2 };
    compared + needless + negated
}
"
    );

    let (reports, has_errors) = reports(&fixed, |_| {});
    assert_eq!(reports, Vec::<String>::new());
    assert!(!has_errors);
}

#[test]
fn comments_are_kept_by_suggestions() {
    let source = "\
inline // inline
const ONCE: Int = 1;

fn main(a: Int, flag: Bool) -> Int {
    inline // inline
    let doubled = a * 2;

    if (a == 1 // note
    ) == true {
        return 1;
    }

    if (a /* x /* { */ */ == 2) == true {
        return 2;
    }

    if (a == 3 // note
    ) == false {
        return 3;
    }

    if (!flag // note
    ) == false {
        return 4;
    }

    let cast = a /* keep */ as Int;
    let plus = + /* keep */ 1;
    doubled + ONCE + cast + plus + early(a)
}

fn early(a: Int) -> Int {
    return /* keep */ a;
}
";

    // Fixes that keep every comment are applied, but the rest would remove one, so they aren't
    let fixed = fix(source);

    assert_eq!(
        fixed,
        "\
inline // inline
const ONCE: Int = 1;

fn main(a: Int, flag: Bool) -> Int {
    inline // inline
    let doubled = a * 2;

    if (a == 1 // note
    ) {
        return 1;
    }

    if a /* x /* { */ */ == 2 {
        return 2;
    }

    if (a == 3 // note
    ) == false {
        return 3;
    }

    if (!flag // note
    ) == false {
        return 4;
    }

    let cast = a /* keep */ as Int;
    let plus = + /* keep */ 1;
    doubled + ONCE + cast + plus + early(a)
}

fn early(a: Int) -> Int {
    return /* keep */ a;
}
"
    );

    assert_eq!(fix(&fixed), fixed);

    let (reports, has_errors) = reports(&fixed, |_| {});

    assert_eq!(
        reports,
        vec![
            "Warning 8: Unnecessary parentheses",
            "Warning 17: Unnecessary comparison with `false`",
            "Warning 22: Unnecessary comparison with `false`",
            "Warning 27: Unnecessary cast from `Int` to `Int`, since they are compatible with each other",
            "Warning 28: Unnecessary `+` operator, since it has no effect",
            "Warning 33: Unnecessary `return` statement, since the function body can end in an expression instead",
        ]
    );
    assert!(!has_errors);
}

#[test]
fn suggestions_dont_join_words() {
    let source = "\
fn main(a: Int) -> Int {
    if a > 1 {
        return+a;
    }

    0
}
";

    // Removing the `+` would otherwise join `return` and `a` into one name
    assert_eq!(
        fix(source),
        "\
fn main(a: Int) -> Int {
    if a > 1 {
        return a;
    }

    0
}
"
    );
    let source = "\
fn main(a: Int) -> Int {
    assert(a > 0);

    if a > 1 {
        return(a);
    }

    0
}
";

    // The parentheses are only reported if the code still parses without them, which it only
    // does once the keywords are separated from the expressions
    assert_eq!(
        fix(source),
        "\
fn main(a: Int) -> Int {
    assert a > 0;

    if a > 1 {
        return a;
    }

    0
}
"
    );
}

#[test]
fn imports_are_used_if_any_namespace_is_used() {
    let source = |main: &str| {
        format!(
            "\
import module::X;

mod module {{
    export const X: Int = 42;
    export type X = 42;
}}

{main}
"
        )
    };

    // Only the type is used, so the import is still needed
    let (type_reports, has_errors) = reports(&source("fn main() -> X {\n    42\n}"), |_| {});

    assert_eq!(type_reports, vec!["Warning 4: Unused constant `X`"]);
    assert!(!has_errors);

    // Neither is used, so the import is only reported once
    let (reports, has_errors) = reports(&source("fn main() -> Int {\n    42\n}"), |_| {});

    assert_eq!(
        reports,
        vec![
            "Warning 1: Unused import `X`",
            "Warning 4: Unused constant `X`",
            "Warning 5: Unused type alias `X`",
        ]
    );
    assert!(!has_errors);
}

#[test]
fn types_used_by_unused_functions_are_unused() {
    let source = "\
struct Foo {
    a: Int,
}

fn main() -> Int {
    1
}

fn unused() -> Int {
    let value: Foo = Foo { a: 1 };
    value.a
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Warning 1: Unused struct `Foo`",
            "Warning 9: Unused function `unused`",
        ]
    );
    assert!(!has_errors);
}

#[test]
fn casts_and_guards_of_unresolved_values_are_not_reported() {
    for source in [
        "fn main() -> Int {\n    nope as Int\n}\n",
        "fn main() -> Int {\n    if nope is Int {\n        1\n    } else {\n        2\n    }\n}\n",
        // The unresolved type can also be inside of another type
        "fn main() -> Int {\n    let pair = (nope, 1) as (Int, Int);\n    pair.first\n}\n",
        "fn main() -> Int {\n    if (nope, 1) is (Int, Int) {\n        1\n    } else {\n        2\n    }\n}\n",
    ] {
        let (diagnostics, has_errors) = compile_source(source, |_| {});

        assert!(
            diagnostics
                .iter()
                .all(|diagnostic| diagnostic.lint().is_none()),
            "{source}"
        );
        assert!(has_errors);
    }
}

#[test]
fn imports_used_by_unused_functions_are_unused() {
    let source = "\
import module::Value;

mod module {
    export type Value = Int;
}

fn main() -> Int {
    1
}

fn unused() -> Value {
    1
}
";

    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Warning 1: Unused import `Value`",
            "Warning 4: Unused type alias `Value`",
            "Warning 11: Unused function `unused`",
        ]
    );
    assert!(!has_errors);
}

#[test]
fn types_in_the_signature_of_a_live_function_are_used() {
    let source = "\
import module::Value;

mod module {
    export struct Value {
        inner: Int,
    }
}

fn main(_value: Value) -> Int {
    1
}
";

    // The parameter is unused, but the signature still needs its type
    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(reports, Vec::<String>::new());
    assert!(!has_errors);
}

#[test]
fn generics_are_used_if_anything_references_them() {
    let source = "\
struct Unused<T> {
    value: T,
}

struct UnusedGeneric<T> {
    value: Int,
}

fn main() -> Int {
    let value = UnusedGeneric::<Int> { value: 1 };
    value.value
}
";

    // The generic of an unused struct is still used by it, so only the struct is reported
    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Warning 1: Unused struct `Unused`",
            "Warning 5: Unused generic type `T`",
        ]
    );
    assert!(!has_errors);
}

#[test]
fn imports_are_used_where_they_are_used_by_live_code() {
    let source = "\
mod module {
    export fn helper() -> Int {
        1
    }
}

mod live {
    import module::helper;

    export fn run() -> Int {
        helper()
    }
}

mod dead {
    import module::helper;

    export fn unused() -> Int {
        helper()
    }
}

import live::run;

struct Foo {
    a: Int,
}

struct Bar {
    a: Int,
}

fn main() -> Int {
    let _probe: Foo = Foo { a: 1 };
    run()
}

fn _helper() -> Bar {
    Bar { a: 1 }
}
";

    // The `helper` imported into `dead` is live through `live`, but `dead` only uses it from an
    // unused function. `Foo` and `Bar` are only used by `_probe` and `_helper`, which aren't
    // reported, so they're kept along with what they need.
    let (reports, has_errors) = reports(source, |options| {
        options.lints.set(Lint::UnusedStructField, LintLevel::Allow);
    });

    assert_eq!(
        reports,
        vec![
            "Warning 16: Unused import `helper`",
            "Warning 18: Unused function `unused`",
        ]
    );
    assert!(!has_errors);
}

#[test]
fn unused_code_is_reported_all_at_once() {
    let source = "\
import m::inner;
import inner::f;
import m::helper;

mod m {
    export mod inner {
        export fn f() -> Int {
            1
        }
    }

    export fn helper() -> Int {
        1
    }
}

struct Point {
    x: Int,
    y: Int,
}

fn main() -> Int {
    let (_q, _r) = (helper(), 1);
    1
}

fn dead(p: Point) -> Int {
    p.x
}
";

    // Both imports in the chain are reported together, `helper` is kept by the bindings
    // destructured from it, and the fields of an unused struct aren't reported along with it
    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Warning 1: Unused import `inner`",
            "Warning 2: Unused import `f`",
            "Warning 7: Unused function `f`",
            "Warning 17: Unused struct `Point`",
            "Warning 27: Unused function `dead`",
        ]
    );
    assert!(!has_errors);
}

#[test]
fn shared_import_paths_and_bindings_inside_values() {
    let source = "\
import m::inner;
import inner::f;
import inner::g;

mod m {
    export mod inner {
        export fn f() -> Int {
            1
        }

        export fn g() -> Int {
            2
        }
    }

    export fn helper() -> Int {
        1
    }
}

import m::helper;

fn main() -> Int {
    let (a, b) = {
        let _z = 1;
        (helper(), 2)
    };
    let (_q, _r) = (g(), 1);
    1
}
";

    // The path to `inner` is resolved once, but both imports that use it count as using `inner`.
    // A `_` binding inside of a value doesn't keep the value, so `helper` is still unused.
    let (reports, has_errors) = reports(source, |_| {});

    assert_eq!(
        reports,
        vec![
            "Warning 2: Unused import `f`",
            "Warning 7: Unused function `f`",
            "Warning 16: Unused function `helper`",
            "Warning 21: Unused import `helper`",
            "Warning 24: Unused binding `a`",
            "Warning 24: Unused binding `b`",
        ]
    );
    assert!(!has_errors);
}
