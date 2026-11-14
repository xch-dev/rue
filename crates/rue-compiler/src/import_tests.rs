//! Imports: which declaration an imported name refers to, how targeted and glob imports interact,
//! and the errors for imports that can't be resolved.

use crate::test_utils::compile;

#[test]
fn named_imports_refer_to_the_exported_declaration() {
    let compiled = compile(
        "
        import inner::value;

        mod inner {
            export fn value() -> Int {
                42
            }
        }

        fn main() -> Int {
            value()
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("value"), "inner");
}

#[test]
fn named_imports_bring_in_a_symbol_and_a_type_with_the_same_name() {
    let compiled = compile(
        "
        import inner::Value;

        mod inner {
            export type Value = Int;
            export const Value: Value = 1;
        }

        fn main() -> Value {
            Value
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("Value"), "inner");
    assert_eq!(compiled.type_origin("Value"), "inner");
}

#[test]
fn private_items_cannot_be_imported() {
    let compiled = compile(
        "
        import inner::secret;
        import inner::Secret;
        export inner::{hidden};

        mod inner {
            fn secret() -> Int {
                42
            }

            type Secret = Int;
            fn hidden() {}
        }

        fn main() -> Secret {
            secret()
        }
        ",
    );

    // The errors are the same as for a path to the items, and the imports still resolve, so
    // the uses aren't also reported
    assert_eq!(
        compiled.errors(),
        [
            "2:23 PrivateSymbol(\"secret\", \"inner\")",
            "3:23 PrivateType(\"Secret\", \"inner\")",
            "4:24 PrivateSymbol(\"hidden\", \"inner\")",
        ]
    );
    assert_eq!(compiled.symbol_origin("secret"), "inner");
    assert_eq!(compiled.type_origin("Secret"), "inner");
}

#[test]
fn private_items_are_not_reported_when_a_public_item_has_the_same_name() {
    let compiled = compile(
        "
        import inner::Value;
        import inner::Other;

        mod inner {
            type Value = Int;
            export const Value: Int = 1;

            const Other: Int = 2;
            export type Other = Int;
        }

        fn main() -> Value {
            Value
        }

        fn other() -> Other {
            Other
        }
        ",
    );

    // Types are declared and imported before symbols, so the private type `Value` is imported
    // before the public constant is declared, and it stays imported without an error. The
    // private constant `Other` is declared when the public type is already known, so only the
    // type is imported
    assert_eq!(compiled.errors(), ["18:13 ExpectedSymbol(\"Other\")"]);
    assert_eq!(compiled.symbol_origin("Value"), "inner");
    assert_eq!(compiled.type_origin("Value"), "inner");
    assert_eq!(compiled.type_origin("Other"), "inner");
}

#[test]
fn missing_items_and_modules_are_reported() {
    let compiled = compile(
        "
        import module::inner::*;
        import module::inner::missing;
        import module::missing;

        mod module {}
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "2:24 UndeclaredSymbol(\"inner\")",
            "3:24 UndeclaredSymbol(\"inner\")",
            "4:24 UnresolvedImport(\"missing\")",
        ]
    );
}

#[test]
fn items_cannot_be_imported_through_a_private_module() {
    let compiled = compile(
        "
        import outer::hidden::VALUE;
        import outer::hidden::OTHER;
        import outer::hidden::deeper::*;
        import outer::hidden;
        import outer::{hidden::THIRD};

        mod outer {
            mod hidden {
                export const VALUE: Int = 1;
                export const OTHER: Int = 2;
                export const THIRD: Int = 4;

                export mod deeper {
                    export const DEEP: Int = 3;
                }
            }
        }

        fn main() -> Int {
            VALUE + OTHER + DEEP + THIRD + hidden::VALUE
        }
        ",
    );

    // Each import through the private module is reported, including those that share the
    // resolved path, and the imports still resolve. Importing the module itself is reported
    // like any other private item.
    assert_eq!(
        compiled.errors(),
        [
            "2:23 PrivateSymbol(\"hidden\", \"outer\")",
            "3:23 PrivateSymbol(\"hidden\", \"outer\")",
            "4:23 PrivateSymbol(\"hidden\", \"outer\")",
            "5:23 PrivateSymbol(\"hidden\", \"outer\")",
            "6:24 PrivateSymbol(\"hidden\", \"outer\")",
        ]
    );
    assert_eq!(compiled.symbol_origin("VALUE"), "outer::hidden");
    assert_eq!(compiled.symbol_origin("DEEP"), "outer::hidden::deeper");
}

#[test]
fn imports_are_private_unless_exported() {
    let compiled = compile(
        "
        mod outer {
            import inner::*;
            import inner::NAMED;
            export inner::EXPORTED;

            mod inner {
                export const GLOBBED: Int = 1;
                export const NAMED: Int = 2;
                export const EXPORTED: Int = 3;
            }

            export fn sum() -> Int {
                GLOBBED + NAMED + EXPORTED
            }
        }

        fn main() -> Int {
            outer::sum() + outer::GLOBBED + outer::NAMED + outer::EXPORTED
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "19:35 PrivateSymbol(\"GLOBBED\", \"outer\")",
            "19:52 PrivateSymbol(\"NAMED\", \"outer\")",
        ]
    );
    assert_eq!(compiled.symbol_origin("outer::EXPORTED"), "outer::inner");
}

#[test]
fn exporting_an_item_that_is_also_imported_privately_makes_it_public() {
    let compiled = compile(
        "
        mod outer {
            import inner::VALUE;
            export inner::VALUE;
            import inner::*;
            export inner::*;

            mod inner {
                export const VALUE: Int = 1;
                export const OTHER: Int = 2;
                export type Number = Int;
            }
        }

        fn main() -> outer::Number {
            outer::VALUE + outer::OTHER
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("outer::VALUE"), "outer::inner");
    assert_eq!(compiled.symbol_origin("outer::OTHER"), "outer::inner");
    assert_eq!(compiled.type_origin("outer::Number"), "outer::inner");
}

#[test]
fn items_cannot_be_imported_from_a_non_module() {
    let compiled = compile(
        "
        import VALUE::inner;

        const VALUE: Int = 1;
        ",
    );

    assert_eq!(compiled.errors(), ["2:16 SubpathNotSupported(\"VALUE\")"]);
}

#[test]
fn glob_imports_bring_in_only_exported_items() {
    let compiled = compile(
        "
        import inner::*;

        mod inner {
            export const PUBLIC: Int = 1;
            const PRIVATE: Int = 2;
            export type Number = Int;
        }

        fn main() -> Number {
            PUBLIC + PRIVATE
        }
        ",
    );

    assert_eq!(compiled.errors(), ["11:22 UndeclaredSymbol(\"PRIVATE\")"]);
    assert_eq!(compiled.symbol_origin("PUBLIC"), "inner");
    assert_eq!(compiled.type_origin("Number"), "inner");
}

/// A module with a glob import of `globbed`, which exports `VALUE` and `Choice` among other
/// things, and a targeted import of the same names from `targeted`, in either order.
fn targeted_and_glob(glob_first: bool) -> String {
    let glob = "import globbed::*;";
    let targeted = "import targeted::{Choice, VALUE};";
    let (first, second) = if glob_first {
        (glob, targeted)
    } else {
        (targeted, glob)
    };

    format!(
        "
        mod targeted {{
            export const VALUE: Int = 1;
            export type Choice = Int;
        }}

        mod globbed {{
            export const VALUE: Int = 2;
            export const GLOB_ONLY: Int = 3;
            export type Choice = Bytes;
        }}

        mod user {{
            {first}
            {second}

            export fn result(value: Choice) -> Int {{
                VALUE + GLOB_ONLY + value
            }}
        }}

        fn main() -> Int {{
            user::result(1)
        }}
        "
    )
}

#[test]
fn targeted_imports_take_precedence_over_glob_imports() {
    for glob_first in [true, false] {
        let compiled = compile(&targeted_and_glob(glob_first));

        compiled.assert_no_errors();
        assert_eq!(
            compiled.symbol_origin("user::VALUE"),
            "targeted",
            "glob first: {glob_first}"
        );
        assert_eq!(
            compiled.type_origin("user::Choice"),
            "targeted",
            "glob first: {glob_first}"
        );
        assert_eq!(compiled.symbol_origin("user::GLOB_ONLY"), "globbed");
    }
}

#[test]
fn targeted_imports_take_precedence_over_glob_imports_that_resolve_first() {
    // `targeted::VALUE` only exists once the glob export in `targeted` has been resolved, which
    // is after the glob import of `globbed`, since imports are resolved in order of their paths
    let compiled = compile(
        "
        mod targeted {
            export zzz::*;
        }

        mod zzz {
            export const VALUE: Int = 1;
        }

        mod globbed {
            export const VALUE: Int = 2;
        }

        mod user {
            import targeted::VALUE;
            import globbed::*;

            export fn result() -> Int {
                VALUE
            }
        }

        fn main() -> Int {
            user::result()
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("user::VALUE"), "zzz");
}

#[test]
fn colliding_glob_imports_are_ambiguous_when_used() {
    for (first, second) in [("alpha", "beta"), ("beta", "alpha")] {
        let compiled = compile(&format!(
            "
            mod alpha {{
                export const VALUE: Int = 1;
                export const UNUSED: Int = 1;
                export type Number = Int;
            }}

            mod beta {{
                export const VALUE: Int = 2;
                export const UNUSED: Int = 2;
                export type Number = Bytes;
            }}

            mod user {{
                import {first}::*;
                import {second}::*;

                export fn result(value: Number) -> Int {{
                    VALUE + value
                }}
            }}

            fn main() -> Int {{
                user::result(1)
            }}
            "
        ));

        // Each use is reported, whichever glob is written first, but not the unused name
        assert_eq!(
            compiled.errors(),
            [
                "18:41 AmbiguousImport(\"Number\")",
                "19:21 AmbiguousImport(\"VALUE\")",
            ],
            "{first} first"
        );
    }
}

#[test]
fn ambiguous_glob_imports_are_reported_through_paths_and_imports() {
    let compiled = compile(
        "
        import outer::both::VALUE;
        import outer::both::inner::NESTED;

        mod alpha {
            export const VALUE: Int = 1;
            export mod inner {
                export const NESTED: Int = 3;
            }
        }

        mod beta {
            export const VALUE: Int = 2;
            export mod inner {
                export const NESTED: Int = 4;
            }
        }

        mod outer {
            export mod both {
                export alpha::*;
                export beta::*;
            }
        }

        fn main() -> Int {
            outer::both::VALUE + VALUE + NESTED
        }
        ",
    );

    // The named imports are reported rather than their uses
    assert_eq!(
        compiled.errors(),
        [
            "2:29 AmbiguousImport(\"VALUE\")",
            "3:29 AmbiguousImport(\"inner\")",
            "27:26 AmbiguousImport(\"VALUE\")",
        ]
    );
}

#[test]
fn ambiguous_glob_imports_are_reported_through_chains_of_globs() {
    let compiled = compile(
        "
        mod alpha {
            export const VALUE: Int = 1;
        }

        mod beta {
            export const VALUE: Int = 2;
        }

        mod both {
            export alpha::*;
            export beta::*;
        }

        mod chained {
            export both::*;
        }

        mod user {
            import chained::*;

            export fn result() -> Int {
                VALUE
            }
        }

        fn main() -> Int {
            user::result() + chained::VALUE
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "23:17 AmbiguousImport(\"VALUE\")",
            "28:39 AmbiguousImport(\"VALUE\")",
        ]
    );
}

#[test]
fn ambiguous_glob_imports_are_reported_through_mutual_glob_exports() {
    let compiled = compile(
        "
        mod alpha {
            export const VALUE: Int = 1;
        }

        mod beta {
            export const VALUE: Int = 2;
        }

        mod first {
            export alpha::*;
            export beta::*;
            export second::*;
        }

        mod second {
            export first::*;
        }

        fn main() -> Int {
            first::VALUE + second::VALUE
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "21:20 AmbiguousImport(\"VALUE\")",
            "21:36 AmbiguousImport(\"VALUE\")",
        ]
    );
}

#[test]
fn named_imports_from_an_enclosing_scope_are_checked_for_ambiguity() {
    let compiled = compile(
        "
        import alpha::*;
        import beta::*;

        mod alpha {
            export const VALUE: Int = 1;
        }

        mod beta {
            export const VALUE: Int = 2;
        }

        mod inner {
            import VALUE;

            export fn result() -> Int {
                VALUE
            }
        }

        fn main() -> Int {
            inner::result()
        }
        ",
    );

    assert_eq!(compiled.errors(), ["14:20 AmbiguousImport(\"VALUE\")"]);
}

#[test]
fn glob_imports_of_a_symbol_and_a_type_with_the_same_name_are_not_ambiguous() {
    let compiled = compile(
        "
        import alpha::*;
        import beta::*;

        mod alpha {
            export const Value: Int = 1;
        }

        mod beta {
            export type Value = Int;
        }

        fn main() -> Value {
            Value
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("Value"), "alpha");
    assert_eq!(compiled.type_origin("Value"), "beta");
}

#[test]
fn named_imports_take_precedence_over_ambiguous_glob_imports() {
    let compiled = compile(
        "
        mod alpha {
            export const VALUE: Int = 1;
        }

        mod beta {
            export const VALUE: Int = 2;
        }

        import alpha::*;
        import beta::*;
        import beta::VALUE;

        fn main() -> Int {
            VALUE
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("VALUE"), "beta");
}

#[test]
fn glob_imports_of_the_same_item_are_not_ambiguous() {
    let compiled = compile(
        "
        mod alpha {
            export const VALUE: Int = 1;
        }

        mod beta {
            export alpha::VALUE;
        }

        import alpha::*;
        import beta::*;

        fn main() -> Int {
            VALUE
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("VALUE"), "alpha");
}

#[test]
fn local_declarations_take_precedence_over_glob_imports() {
    let compiled = compile(
        "
        import inner::*;

        mod inner {
            export const VALUE: Int = 1;
        }

        const VALUE: Int = 2;

        fn main() -> Int {
            VALUE
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("VALUE"), "(root)");
}

#[test]
fn named_imports_of_a_declared_name_are_duplicates() {
    let compiled = compile(
        "
        import inner::VALUE;
        import inner::Number;

        mod inner {
            export const VALUE: Int = 1;
            export type Number = Int;
        }

        const VALUE: Int = 2;
        type Number = Int;

        fn main() -> Number {
            VALUE
        }
        ",
    );

    // The local declarations are used, and the imports are reported rather than unused
    assert_eq!(
        compiled.errors(),
        [
            "2:23 DuplicateSymbol(\"VALUE\")",
            "3:23 DuplicateType(\"Number\")",
        ]
    );
    assert_eq!(
        compiled.reference_origins("main", "VALUE"),
        ["`VALUE`: (root)"]
    );
    assert_eq!(compiled.type_origin("Number"), "(root)");
    assert!(
        !compiled
            .warnings()
            .iter()
            .any(|w| w.ends_with("UnusedImport")),
        "{:?}",
        compiled.warnings()
    );
}

#[test]
fn named_imports_of_a_name_declared_in_the_other_namespace_are_not_duplicates() {
    let compiled = compile(
        "
        import inner::Value;

        mod inner {
            export const Value: Int = 1;
        }

        type Value = Int;

        fn main() -> Value {
            Value
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("Value"), "inner");
    assert_eq!(compiled.type_origin("Value"), "(root)");
}

#[test]
fn named_imports_of_different_items_with_the_same_name_are_duplicates() {
    let compiled = compile(
        "
        import alpha::VALUE;
        import beta::VALUE;
        import alpha::VALUE;

        mod alpha {
            export const VALUE: Int = 1;
        }

        mod beta {
            export const VALUE: Int = 2;
        }

        fn main() -> Int {
            VALUE
        }
        ",
    );

    // Imports are resolved in order of their paths, so the first `alpha` import is used and
    // importing the same item again isn't a duplicate
    assert_eq!(compiled.errors(), ["3:22 DuplicateSymbol(\"VALUE\")"]);
    assert_eq!(compiled.symbol_origin("VALUE"), "alpha");
}

#[test]
fn glob_exports_re_export_items_through_modules() {
    let compiled = compile(
        "
        import outer::*;

        mod outer {
            mod inner {
                export fn hello() -> Int {
                    1
                }
            }

            export inner::*;
        }

        fn main() -> Int {
            hello()
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("hello"), "outer::inner");
    assert_eq!(compiled.symbol_origin("outer::hello"), "outer::inner");
}

#[test]
fn mutual_glob_exports_resolve() {
    let compiled = compile(
        "
        mod a {
            export b::*;

            export fn a() -> Int {
                4
            }
        }

        mod b {
            export a::*;

            export fn b() -> Int {
                5
            }
        }

        fn main() -> Int {
            a::b() + b::a()
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("a::b"), "b");
    assert_eq!(compiled.symbol_origin("b::a"), "a");
}

#[test]
fn imports_resolve_through_items_imported_later() {
    let compiled = compile(
        "
        import first::value;

        mod first {
            export second::value;
        }

        mod second {
            export fn value() -> Int {
                1
            }
        }

        fn main() -> Int {
            value()
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("value"), "second");
}

#[test]
fn super_only_refers_to_directory_modules() {
    // Inline modules aren't parents for `super` (since fc8b077a), so this is unresolved
    let compiled = compile(
        "
        const TOP: Int = 1;

        mod inner {
            import super::TOP;
        }
        ",
    );

    assert_eq!(compiled.errors(), ["5:20 UnresolvedSuper"]);
}

#[test]
fn super_errors_are_reported() {
    let compiled = compile(
        "
        import super;
        import super::super;
        import super::x;
        import a::{super};

        mod a {
            mod b {}

            import b::super::c;
        }
        ",
    );

    // An import whose path ends in `super` imports nothing, so it's only reported once, and
    // `super` after a name is skipped, so the last import looks for `b::c`
    assert_eq!(
        compiled.errors(),
        [
            "2:16 SuperAtEnd",
            "3:16 UnresolvedSuper",
            "3:23 SuperAtEnd",
            "4:16 UnresolvedSuper",
            "4:23 UnresolvedImport(\"x\")",
            "5:20 SuperAtEnd",
            "10:23 SuperAfterNamedImport",
            "10:30 UnresolvedImport(\"c\")",
        ]
    );
}

#[test]
fn glob_imports_that_bring_in_nothing_are_unused() {
    let compiled = compile(
        "
        import inner::*;

        mod inner {}

        fn main() -> Int {
            1
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.warnings(), ["2:23 UnusedImport"]);
}
