//! Imports: which declaration an imported name refers to, how targeted and glob imports interact,
//! and the errors for imports that can't be resolved.

use std::fmt::Write;

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

    // The errors are the same as for a path to the items. The items aren't imported, but their
    // uses aren't reported again
    assert_eq!(
        compiled.errors(),
        [
            "2:23 PrivateSymbol(\"secret\", \"inner\")",
            "3:23 PrivateType(\"Secret\", \"inner\")",
            "4:24 PrivateSymbol(\"hidden\", \"inner\")",
        ]
    );
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

    // Only the public item with each name is imported, so the private type `Value` and the
    // private constant `Other` can't be used. Types are declared before symbols are filled in,
    // so this checks that the private type isn't imported before the public constant is.
    assert_eq!(
        compiled.errors(),
        [
            "13:22 ExpectedType(\"Value\")",
            "18:13 ExpectedSymbol(\"Other\")",
        ]
    );
    assert_eq!(compiled.symbol_origin("Value"), "inner");
    assert_eq!(compiled.type_origin("Other"), "inner");
}

#[test]
fn private_items_are_not_imported_before_a_reexported_public_item_with_the_same_name() {
    let compiled = compile(
        "
        import inner::Value;

        mod inner {
            type Value = Int;
            export nested::*;

            mod nested {
                export const Value: Int = 1;
            }
        }

        fn main() -> Value {
            Value
        }
        ",
    );

    // The public constant only reaches `inner` once the glob import is resolved, after the
    // private type has been seen, which still isn't imported
    assert_eq!(compiled.errors(), ["13:22 ExpectedType(\"Value\")"]);
    assert_eq!(compiled.symbol_origin("Value"), "inner::nested");
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
    // like any other private item, and like one it isn't imported, but its use isn't reported
    // again.
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
    let glob = "import super::globbed::*;";
    let targeted = "import super::targeted::{Choice, VALUE};";
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
            export super::zzz::*;
        }

        mod zzz {
            export const VALUE: Int = 1;
        }

        mod globbed {
            export const VALUE: Int = 2;
        }

        mod user {
            import super::targeted::VALUE;
            import super::globbed::*;

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
                import super::{first}::*;
                import super::{second}::*;

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
                export super::super::alpha::*;
                export super::super::beta::*;
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
            export super::alpha::*;
            export super::beta::*;
        }

        mod chained {
            export super::both::*;
        }

        mod user {
            import super::chained::*;

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
            export super::alpha::*;
            export super::beta::*;
            export super::second::*;
        }

        mod second {
            export super::first::*;
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
fn named_imports_from_a_parent_module_are_checked_for_ambiguity() {
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
            import super::VALUE;

            export fn result() -> Int {
                VALUE
            }
        }

        fn main() -> Int {
            inner::result()
        }
        ",
    );

    assert_eq!(compiled.errors(), ["14:27 AmbiguousImport(\"VALUE\")"]);
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
            export super::alpha::VALUE;
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
            export super::b::*;

            export fn a() -> Int {
                4
            }
        }

        mod b {
            export super::a::*;

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
            export super::second::value;
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
fn super_refers_to_the_module_an_inline_module_is_in() {
    // `super` from an inline module is the module it's in, whose private items it can use, and
    // a single file has no module above it
    let compiled = compile(
        "
        const TOP: Int = 1;

        mod inner {
            import super::TOP;
            import super::super::x;

            export fn value() -> Int {
                TOP + super::TOP
            }
        }

        fn main() -> Int {
            inner::value()
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        ["6:27 UnresolvedSuper", "6:34 UnresolvedImport(\"x\")"]
    );
    assert_eq!(compiled.symbol_origin("inner::TOP"), "(root)");
    assert_eq!(
        compiled.reference_origins("inner::value", "TOP"),
        ["`TOP`: (root)", "`super::TOP`: (root)"]
    );
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

#[test]
fn imports_from_a_parent_module_find_what_it_imports() {
    // `import super::V;` and `import super::m::W;` find what `inner` imports, rather than the
    // items at the top level, even though `inner`'s imports are resolved after them
    let compiled = compile(
        "
        mod a {
            export const V: Int = 1;
            export type T = Int;
        }

        mod z {
            export mod m {
                export const W: Int = 2;
            }
        }

        const V: Int = 3;
        type T = Bytes;

        mod m {
            export const W: Int = 4;
        }

        mod inner {
            import super::a::V;
            import super::a::T;
            import super::z::m;

            export mod deeper {
                import super::V;
                import super::T;
                import super::m::W;

                export fn value(value: T) -> Int {
                    V + W + value
                }
            }
        }

        fn main() -> Int {
            inner::deeper::value(0) + V + m::W
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("inner::deeper::V"), "a");
    assert_eq!(compiled.type_origin("inner::deeper::T"), "a");
    assert_eq!(compiled.symbol_origin("inner::deeper::W"), "z::m");
    // The imports in `inner` are used through the ones in `deeper`
    assert_eq!(compiled.warnings(), ["14:14 UnusedTypeAlias"]);
}

#[test]
fn reexports_from_a_parent_module_find_what_it_glob_imports() {
    let compiled = compile(
        "
        mod a {
            export const V: Int = 1;
        }

        const V: Int = 2;

        mod inner {
            export super::a::*;

            export mod deeper {
                export super::V;
            }
        }

        fn main() -> Int {
            inner::deeper::V + V
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("inner::deeper::V"), "a");
    assert_eq!(compiled.warnings(), Vec::<String>::new());
}

#[test]
fn imports_from_a_parent_module_find_only_the_namespaces_it_has() {
    // `inner` only has a type named `V`, so the value `V` is imported from the top level by a
    // separate import of the same name, which isn't a duplicate
    let compiled = compile(
        "
        mod a {
            export const OTHER: Int = 1;
            export type V = Int;
        }

        const V: Int = 2;

        mod inner {
            import super::a::*;
            import super::a::V;

            export mod deeper {
                import super::V;
                import super::super::V;
                import super::OTHER;

                export fn value(value: V) -> Int {
                    V + value + OTHER
                }
            }
        }

        fn main() -> Int {
            inner::deeper::value(0)
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("inner::deeper::V"), "(root)");
    assert_eq!(compiled.type_origin("inner::deeper::V"), "a");
}

#[test]
fn chains_of_imports_from_parent_modules_resolve() {
    let compiled = compile(
        "
        mod a {
            export const V: Int = 1;
        }

        const V: Int = 2;

        mod first {
            import super::a::*;

            export mod second {
                import super::V;

                export mod third {
                    import super::V;

                    export fn value() -> Int {
                        V
                    }
                }
            }
        }

        fn main() -> Int {
            first::second::third::value() + V
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("first::second::third::V"), "a");
}

#[test]
fn local_modules_take_precedence_over_imported_modules() {
    let compiled = compile(
        "
        import m::helper;
        import m::*;

        mod m {
            export mod helper {
                export const VALUE: Int = 1;
            }

            export mod other {
                export const VALUE: Int = 2;
            }
        }

        mod helper {
            export const VALUE: Int = 3;
        }

        mod other {
            export const VALUE: Int = 4;
        }

        fn main() -> Int {
            helper::VALUE + other::VALUE
        }
        ",
    );

    // Like any named import of a declared name, the import of `helper` is the duplicate. The
    // glob import is shadowed.
    assert_eq!(compiled.errors(), ["2:19 DuplicateSymbol(\"helper\")"]);
    assert_eq!(compiled.symbol_origin("helper::VALUE"), "helper");
    assert_eq!(compiled.symbol_origin("other::VALUE"), "other");
}

#[test]
fn imported_modules_and_local_values_with_the_same_name_are_both_bound() {
    // Modules have their own namespace, so an imported module doesn't clash with a local
    // function or constant
    let compiled = compile(
        "
        import m::helper;
        import m::*;

        mod m {
            export mod helper {
                export const VALUE: Int = 1;
            }

            export mod value {
                export const VALUE: Int = 2;
            }
        }

        fn helper() -> Int {
            3
        }

        const value: Int = 4;

        fn main() -> Int {
            helper() + value + helper::VALUE + value::VALUE
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(
        compiled.reference_origins("main", "helper"),
        ["`helper`: (root)"]
    );
    assert_eq!(
        compiled.reference_origins("main", "value"),
        ["`value`: (root)"]
    );
    assert_eq!(compiled.symbol_origin("helper::VALUE"), "m::helper");
    assert_eq!(compiled.symbol_origin("value::VALUE"), "m::value");
}

#[test]
fn reexports_of_a_local_declaration_ignore_enclosing_modules_with_the_name() {
    let compiled = compile(
        "
        mod thing {}

        mod inner {
            export thing;

            fn thing() -> Int {
                1
            }
        }

        fn main() -> Int {
            inner::thing()
        }
        ",
    );

    // The function is private, so it can't be re-exported, but the name still refers to it
    assert_eq!(compiled.errors(), ["5:20 PrivateReexport(\"thing\")"]);
    assert_eq!(compiled.symbol_origin("inner::thing"), "inner");
}

#[test]
fn uses_of_a_private_import_are_not_reported_again() {
    let compiled = compile(
        "
        import inner::secret;
        import inner::Secret;

        mod inner {
            fn secret() -> Int {
                1
            }

            type Secret = Int;
        }

        mod nested {
            export fn value() -> Int {
                super::secret()
            }
        }

        fn main() -> Secret {
            secret() + nested::value()
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "2:23 PrivateSymbol(\"secret\", \"inner\")",
            "3:23 PrivateType(\"Secret\", \"inner\")",
        ]
    );
}

#[test]
fn private_imports_do_not_hide_a_declaration_with_the_same_name() {
    let compiled = compile(
        "
        import inner::secret;

        mod inner {
            fn secret() -> Int {
                1
            }
        }

        fn secret() -> Int {
            2
        }

        fn main() -> Int {
            secret()
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        ["2:23 PrivateSymbol(\"secret\", \"inner\")"]
    );
    assert_eq!(
        compiled.reference_origins("main", "secret"),
        ["`secret`: (root)"]
    );
}

#[test]
fn imports_from_each_parent_module_find_what_it_imports() {
    // `third` imports `V` from `second` and `OTHER` from `first`, which each import them
    let compiled = compile(
        "
        mod a {
            export const OTHER: Int = 1;
        }

        mod m {
            export const V: Int = 5;
        }

        const V: Int = 2;

        mod first {
            import super::a::*;

            export mod second {
                import super::super::m::V;

                export mod third {
                    import super::V;
                    import super::super::OTHER;

                    export fn value() -> Int {
                        V + OTHER
                    }
                }
            }
        }

        fn main() -> Int {
            first::second::third::value() + V + m::V
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("first::second::third::V"), "m");
    assert_eq!(compiled.symbol_origin("first::second::third::OTHER"), "a");
}

#[test]
fn imports_from_a_parent_module_find_what_it_imports_from_a_child() {
    // `inner` gets `V` from `deeper` through a glob import, and `user` imports it from `inner`.
    // Only what `inner` has is looked at, so the top-level `V` can't be found by mistake.
    let compiled = compile(
        "
        mod a {
            export const V: Int = 1;
        }

        const V: Int = 7;

        mod inner {
            import deeper::*;

            export mod deeper {
                export super::super::a::V;
            }

            export mod user {
                import super::V;

                export fn f() -> Int {
                    V
                }
            }

            export fn g() -> Int {
                V
            }
        }

        fn main() -> Int {
            inner::user::f() + inner::g() + V
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("inner::user::V"), "a");
    assert_eq!(compiled.symbol_origin("inner::V"), "a");
}

#[test]
fn super_paths_name_the_parent_module_rather_than_a_glob_imported_one() {
    // `super::z::*` brings a module named `y` into `inner`, but `super::y` is the top-level `y`
    let compiled = compile(
        "
        mod y {
            export const V: Int = 7;
        }

        mod z {
            export mod y {
                export const V: Int = 1;
            }
        }

        mod inner {
            import super::z::*;
            import super::y::*;

            export fn f() -> Int {
                V + y::V
            }
        }

        fn main() -> Int {
            inner::f() + y::V
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("inner::V"), "y");
    assert_eq!(
        compiled.reference_origins("inner::f", "V"),
        ["`V`: y", "`y::V`: z::y"]
    );
}

#[test]
fn glob_imports_that_bring_in_their_own_module_name_are_not_ambiguous() {
    // Like the prelude's `export tree_hash::*;`: the glob import brings in a function with the
    // name of the module it imports from
    let compiled = compile(
        "
        mod helpers {
            export fn helpers() -> Int {
                1
            }
        }

        mod other {
            export fn other() -> Int {
                2
            }
        }

        mod inner {
            import super::helpers::*;
            import super::other::*;

            export fn f() -> Int {
                helpers() + other()
            }
        }

        fn main() -> Int {
            inner::f()
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("inner::helpers"), "helpers");
}

#[test]
fn private_imports_in_a_nested_module_do_not_fall_back_to_the_parent() {
    // The use in `nested` doesn't find the top-level function, and isn't reported again
    let compiled = compile(
        "
        mod inner {
            fn secret() -> Int {
                42
            }
        }

        fn secret() -> Int {
            1
        }

        mod nested {
            import super::inner::secret;

            export fn value() -> Int {
                secret()
            }
        }

        fn main() -> Int {
            nested::value() + secret()
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        ["13:34 PrivateSymbol(\"secret\", \"inner\")"]
    );
    assert_eq!(
        compiled.reference_origins("nested::value", "secret"),
        Vec::<String>::new()
    );
}

#[test]
fn functions_and_modules_can_have_the_same_name() {
    // Modules have their own namespace, so neither is a duplicate, whichever is written first
    let compiled = compile(
        "
        fn helper() -> Int {
            helper::VALUE
        }

        mod helper {
            export const VALUE: Int = 1;
        }

        mod other {
            export const VALUE: Int = 2;
        }

        const other: Int = other::VALUE;

        fn main() -> Int {
            helper() + other
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(
        compiled.reference_origins("main", "helper"),
        ["`helper`: (root)"]
    );
    assert_eq!(compiled.symbol_origin("helper::VALUE"), "helper");
}

#[test]
fn modules_with_the_same_name_are_duplicates() {
    let compiled = compile(
        "
        mod helper {}

        mod helper {}
        ",
    );

    assert_eq!(compiled.errors(), ["4:13 DuplicateSymbol(\"helper\")"]);
}

#[test]
fn type_imports_from_a_parent_module_find_what_it_imports_from_a_child() {
    let compiled = compile(
        "
        mod a {
            export type V = Int;
        }

        type V = Bytes;

        mod inner {
            import deeper::*;

            export mod deeper {
                export super::super::a::V;
            }

            export mod user {
                import super::V;

                export fn f(value: V) -> V {
                    value
                }
            }
        }

        fn main() -> Int {
            inner::user::f(1)
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.type_origin("inner::user::V"), "a");
}

#[test]
fn import_paths_wait_for_a_glob_import_that_brings_in_their_first_segment() {
    // `import q::V` waits for `import super::y::*`, which brings in `y`'s `q`, so it doesn't find
    // the top-level `q`, which isn't visible in `inner` anyway
    let compiled = compile(
        "
        mod q {
            export const V: Int = 7;
        }

        mod y {
            export mod q {
                export const V: Int = 1;
            }
        }

        mod inner {
            import q::V;
            import super::y::*;

            export fn f() -> Int {
                V
            }
        }

        fn main() -> Int {
            inner::f() + q::V
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("inner::V"), "y::q");
}

#[test]
fn import_paths_use_what_another_import_in_the_scope_brings_in() {
    // `import m::X` finds the `m` that `import super::m::m` brings in, as a use of `m::X` there
    // would, and `super::m` is still the top-level `m`. Paths through `super` aren't shared with
    // the other imports in the scope, which name a different `m`.
    let compiled = compile(
        "
        mod m {
            export mod m {
                export const X: Int = 1;
            }

            export const X: Int = 2;
        }

        mod inner {
            import super::m::m;
            import m::X;

            export fn f() -> Int {
                X + m::X
            }

            export mod deeper {
                import super::m::X;

                export fn g() -> Int {
                    X
                }
            }
        }

        fn main() -> Int {
            inner::f() + inner::deeper::g() + m::X
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("inner::X"), "m::m");
    assert_eq!(compiled.symbol_origin("inner::deeper::X"), "m::m");
}

#[test]
fn import_paths_use_what_a_glob_import_in_the_scope_brings_in() {
    let compiled = compile(
        "
        mod m {
            export mod m {
                export const Y: Int = 1;
            }

            export const Y: Int = 2;
        }

        mod inner {
            import super::m::*;
            import m::Y;

            export fn f() -> Int {
                Y + m::Y
            }
        }

        fn main() -> Int {
            inner::f() + m::Y
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("inner::Y"), "m::m");
}

#[test]
fn super_glob_imports_are_not_ambiguous_with_a_module_other_globs_bring_in() {
    // Both glob imports bring in `inner::m` as `m`, but `super::m` is the top-level `m`, so the
    // glob import of it isn't affected by them
    let compiled = compile(
        "
        mod inner {
            export mod m {
                export const Y: Int = 2;
            }
        }

        mod m {
            export super::inner::m;
            export const Y: Int = 1;
        }

        mod n {
            export super::inner::m;
        }

        mod s {
            import super::m::*;
            import super::n::*;

            export fn f() -> Int {
                Y + m::Y
            }
        }

        fn main() -> Int {
            s::f()
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("s::Y"), "m");
    assert_eq!(compiled.symbol_origin("s::m::Y"), "inner::m");
}

#[test]
fn private_type_imports_in_a_nested_module_do_not_fall_back_to_the_parent() {
    let compiled = compile(
        "
        mod inner {
            type Secret = Int;
        }

        type Secret = Bytes;

        mod nested {
            import super::inner::Secret;

            export fn value() -> Secret {
                1
            }
        }

        fn main() -> Int {
            nested::value()
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        ["9:34 PrivateType(\"Secret\", \"inner\")"]
    );
}

#[test]
fn qualified_uses_of_a_private_import_are_still_reported() {
    // Only a name looked up from the scope stops at the blocked name, so `other::secret` is
    // reported as missing from `other`
    let compiled = compile(
        "
        import inner::secret;

        mod inner {
            fn secret() -> Int {
                42
            }
        }

        mod other {}

        fn main() -> Int {
            other::secret()
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "2:23 PrivateSymbol(\"secret\", \"inner\")",
            "13:20 UndeclaredSymbol(\"secret\")",
        ]
    );
}

#[test]
fn glob_imports_of_items_imported_back_resolve() {
    // `n` re-exports the `m` that `s`'s glob import brought in, and `import super::n::*` brings it
    // back into `s`, which isn't another item with the name
    let compiled = compile(
        "
        mod m {
            export mod m {
                export const Y: Int = 2;
            }

            export const Y: Int = 1;
        }

        mod n {
            export super::s::m;
            export const Z: Int = 3;
        }

        mod s {
            export super::m::*;
            import super::n::*;

            export fn f() -> Int {
                Y + Z
            }
        }

        fn main() -> Int {
            s::f()
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("s::Y"), "m");
}

#[test]
fn named_imports_bring_in_a_module_and_a_value_with_the_same_name() {
    let compiled = compile(
        "
        import m::helper;

        mod m {
            export mod helper {
                export const VALUE: Int = 1;
            }

            export fn helper() -> Int {
                2
            }
        }

        fn main() -> Int {
            helper() + helper::VALUE
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("helper"), "m");
    assert_eq!(compiled.symbol_origin("helper::VALUE"), "m::helper");
}

#[test]
fn reexports_bring_in_a_module_and_a_value_with_the_same_name() {
    let compiled = compile(
        "
        import outer::helper;

        mod inner {
            export mod helper {
                export const VALUE: Int = 1;
            }

            export fn helper() -> Int {
                2
            }
        }

        mod outer {
            export super::inner::helper;
        }

        fn main() -> Int {
            helper() + helper::VALUE + outer::helper() + outer::helper::VALUE
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("outer::helper"), "inner");
    assert_eq!(
        compiled.symbol_origin("outer::helper::VALUE"),
        "inner::helper"
    );
}

#[test]
fn glob_imports_bring_in_a_module_and_a_value_with_the_same_name() {
    let compiled = compile(
        "
        import m::*;

        mod m {
            export mod helper {
                export const VALUE: Int = 1;
            }

            export fn helper() -> Int {
                2
            }
        }

        fn main() -> Int {
            helper() + helper::VALUE
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("helper"), "m");
    assert_eq!(compiled.symbol_origin("helper::VALUE"), "m::helper");
}

#[test]
fn imports_used_only_through_one_namespace_are_used() {
    // The import brings in both, and using either one uses it
    let compiled = compile(
        "
        import m::helper;

        mod m {
            export mod helper {
                export const VALUE: Int = 1;
            }

            export fn helper() -> Int {
                2
            }
        }

        fn main() -> Int {
            helper::VALUE
        }
        ",
    );

    compiled.assert_no_errors();
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
fn private_module_imports_in_a_nested_module_do_not_fall_back_to_the_parent() {
    let compiled = compile(
        "
        mod inner {
            mod secret {
                export const VALUE: Int = 1;
            }
        }

        mod secret {
            export const VALUE: Int = 2;
        }

        mod nested {
            import super::inner::secret;

            export fn value() -> Int {
                secret::VALUE
            }
        }

        fn main() -> Int {
            nested::value() + secret::VALUE
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        ["13:34 PrivateSymbol(\"secret\", \"inner\")"]
    );
    assert_eq!(
        compiled.reference_origins("nested::value", "VALUE"),
        Vec::<String>::new()
    );
}

#[test]
fn private_imports_are_not_reported_again_through_another_namespace() {
    // The private module `x` and the private function `y` are blocked, so neither is reported as
    // the wrong kind of item at the uses
    let compiled = compile(
        "
        mod a {
            mod x {
                export const Y: Int = 5;
            }

            fn y() -> Int {
                1
            }

            export fn z() -> Int {
                x::Y + y()
            }
        }

        fn x() -> Int {
            1
        }

        mod y {
            export const Y: Int = 7;
        }

        mod inner {
            import super::a::x;
            import super::a::y;

            export fn q() -> Int {
                x::Y + y()
            }
        }

        fn main() -> Int {
            inner::q() + a::z() + x() + y::Y
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "25:30 PrivateSymbol(\"x\", \"a\")",
            "26:30 PrivateSymbol(\"y\", \"a\")",
        ]
    );
}

#[test]
fn module_imports_from_a_parent_module_find_what_it_imports_from_a_child() {
    let compiled = compile(
        "
        mod a {
            export mod V {
                export const X: Int = 1;
            }
        }

        mod V {
            export const X: Int = 7;
        }

        mod inner {
            import deeper::*;

            export mod deeper {
                export super::super::a::V;
            }

            export mod user {
                import super::V;

                export fn f() -> Int {
                    V::X
                }
            }

            export fn g() -> Int {
                V::X
            }
        }

        fn main() -> Int {
            inner::user::f() + inner::g() + V::X
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("inner::user::V::X"), "a::V");
    assert_eq!(compiled.symbol_origin("inner::V::X"), "a::V");
}

#[test]
fn glob_exports_of_items_that_are_already_imported_export_them() {
    // The glob export finds the items the glob import already brought in, and exports them
    let compiled = compile(
        "
        mod m {
            export mod helper {
                export const X: Int = 1;
            }

            export const VALUE: Int = 2;
        }

        mod outer {
            import super::m::*;
            export super::m::*;
        }

        fn main() -> Int {
            outer::helper::X + outer::VALUE
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("outer::helper::X"), "m::helper");
    assert_eq!(compiled.symbol_origin("outer::VALUE"), "m");
}

#[test]
fn private_imports_are_not_reported_again_at_a_use_of_the_wrong_kind() {
    // `m` is a private module and `f` a private function, so these uses would be errors even if
    // the imports had worked, but the imports have already been reported
    let compiled = compile(
        "
        mod a {
            mod m {
                export const X: Int = 1;
            }

            fn f() -> Int {
                2
            }
        }

        import a::m;
        import a::f;

        fn main() -> Int {
            m() + f::Y
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "12:19 PrivateSymbol(\"m\", \"a\")",
            "13:19 PrivateSymbol(\"f\", \"a\")",
        ]
    );
}

#[test]
fn inline_modules_do_not_see_the_items_of_the_module_they_are_in() {
    // Neither the file's constant nor its other module can be used by name from `inner`
    let compiled = compile(
        "
        const TOP: Int = 1;

        mod other {
            export const VALUE: Int = 2;
        }

        mod inner {
            import other::VALUE;

            export fn value() -> Int {
                TOP + other::VALUE
            }
        }

        fn main() -> Int {
            inner::value() + TOP + other::VALUE
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "9:20 UndeclaredSymbol(\"other\")",
            "12:17 UndeclaredSymbol(\"TOP\")",
            "12:23 UndeclaredSymbol(\"other\")",
        ]
    );
}

#[test]
fn super_reaches_the_private_items_of_enclosing_modules() {
    let compiled = compile(
        "
        const TOP: Int = 1;

        mod outer {
            const MIDDLE: Int = 2;

            export mod inner {
                export fn value() -> Int {
                    super::MIDDLE + super::super::TOP
                }
            }
        }

        fn main() -> Int {
            outer::inner::value()
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(
        compiled.reference_origins("outer::inner::value", "MIDDLE"),
        ["`super::MIDDLE`: outer"]
    );
    assert_eq!(
        compiled.reference_origins("outer::inner::value", "TOP"),
        ["`super::super::TOP`: (root)"]
    );
}

#[test]
fn glob_imports_of_super_bring_in_private_items() {
    let compiled = compile(
        "
        const TOP: Int = 1;
        type Number = Int;

        mod inner {
            import super::*;

            export fn value(number: Number) -> Int {
                TOP + number
            }
        }

        fn main() -> Int {
            inner::value(2)
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("inner::TOP"), "(root)");
    assert_eq!(compiled.type_origin("inner::Number"), "(root)");
}

#[test]
fn named_imports_of_super_reach_the_private_items_of_the_file() {
    let compiled = compile(
        "
        fn helper() -> Int {
            1
        }

        mod inner {
            import super::helper;

            export fn value() -> Int {
                helper()
            }
        }

        fn main() -> Int {
            inner::value()
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("inner::helper"), "(root)");
}

#[test]
fn super_imports_of_a_blocked_name_are_not_reported_again() {
    let compiled = compile(
        "
        mod m {
            const X: Int = 1;
        }

        import m::X;

        mod a {
            import super::X;

            export fn f() -> Int {
                super::X + X
            }
        }

        fn main() -> Int {
            a::f() + X
        }
        ",
    );

    assert_eq!(compiled.errors(), ["6:19 PrivateSymbol(\"X\", \"m\")"]);
}

#[test]
fn types_in_inline_modules_use_super() {
    let compiled = compile(
        "
        type T = Int;

        mod a {
            export type U = super::T;

            export struct S {
                x: super::T,
            }
        }

        fn main() -> a::U {
            a::S { x: 1 }.x
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.type_origin("a::U"), "a");
}

#[test]
fn super_import_paths_through_a_non_module_are_reported() {
    // `X` is a constant in the file, so it can't have a path after it. `sha256` is only in the
    // builtins, which `super::` doesn't look in.
    let compiled = compile(
        "
        const X: Int = 1;

        mod a {
            import super::X::Y;
            import super::sha256::x;
        }

        fn main() -> Int {
            X
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "5:27 SubpathNotSupported(\"X\")",
            "6:27 UndeclaredSymbol(\"sha256\")",
        ]
    );
}

#[test]
fn super_paths_still_need_items_inside_other_modules_to_be_exported() {
    let compiled = compile(
        "
        mod m {
            const P: Int = 1;
            type Q = Int;
        }

        mod a {
            export fn f() -> super::m::Q {
                super::m::P
            }
        }

        fn main() -> Int {
            a::f()
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "8:40 PrivateType(\"Q\", \"m\")",
            "9:27 PrivateSymbol(\"P\", \"m\")"
        ]
    );
}

#[test]
fn named_imports_only_shadow_glob_imports_in_their_namespaces() {
    // `import a::x` only brings in a function, so the glob import still brings in the module `x`
    let compiled = compile(
        "
        mod a {
            export fn x() -> Int {
                1
            }
        }

        mod b {
            export mod x {
                export const Y: Int = 5;
            }

            export fn x() -> Int {
                2
            }
        }

        import a::x;
        import b::*;

        fn main() -> Int {
            x() + x::Y
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("x"), "a");
    assert_eq!(compiled.symbol_origin("x::Y"), "b::x");
}

#[test]
fn many_glob_imports_that_wait_for_each_other_resolve_quickly() {
    // Each glob import could bring in the module that another one's path names, so they all wait
    // for each other. Finding which ones to answer used to take time quartic in how many there
    // are, so this took minutes. It's compiled on another thread, so that a regression fails the
    // test rather than hanging it.
    const COUNT: usize = 300;

    /// Removes the directory when the test ends, even if it fails.
    struct TempDir(std::path::PathBuf);

    impl Drop for TempDir {
        fn drop(&mut self) {
            std::fs::remove_dir_all(&self.0).ok();
        }
    }

    let dir =
        TempDir(std::env::temp_dir().join(format!("rue-waiting-globs-{}", std::process::id())));
    std::fs::create_dir_all(&dir.0).unwrap();

    let mut main = String::new();

    for i in 0..COUNT {
        writeln!(main, "import f{i}::*;").unwrap();
        std::fs::write(
            dir.0.join(format!("f{i}.rue")),
            format!("export const V{i}: Int = {i};\n"),
        )
        .unwrap();
    }

    writeln!(main, "fn main() -> Int {{\n    V0 + V{}\n}}", COUNT - 1).unwrap();
    std::fs::write(dir.0.join("main.rue"), main).unwrap();

    let (sender, receiver) = std::sync::mpsc::channel();
    let path = dir.0.clone();

    std::thread::spawn(move || {
        let mut ctx = crate::Compiler::new(rue_options::CompilerOptions {
            std: false,
            ..rue_options::CompilerOptions::default()
        });
        crate::FileTree::compile_path(&mut ctx, &path, &mut std::collections::HashMap::new())
            .unwrap();

        let errors = ctx
            .take_diagnostics()
            .into_iter()
            .filter(|diagnostic| diagnostic.severity == rue_diagnostic::DiagnosticSeverity::Error)
            .map(|diagnostic| diagnostic.kind.to_string())
            .collect::<Vec<_>>();

        sender.send(errors).ok();
    });

    let errors = receiver
        .recv_timeout(std::time::Duration::from_secs(10))
        .expect("resolving the imports took more than 10 seconds, or panicked");

    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn glob_imports_of_super_bring_in_blocked_names_as_blocked() {
    let compiled = compile(
        "
        import m::Q;

        mod m {
            const Q: Int = 1;

            export fn use_q() -> Int {
                Q
            }
        }

        mod b {
            import super::*;

            export fn f() -> Int {
                Q + m::use_q()
            }
        }

        fn main() -> Int {
            b::f()
        }
        ",
    );

    assert_eq!(compiled.errors(), ["2:19 PrivateSymbol(\"Q\", \"m\")"]);
}

#[test]
fn reexports_of_private_items_through_super_are_reported() {
    let compiled = compile(
        "
        const SECRET: Int = 1;
        type Secret = Int;
        mod hidden {}

        export mod a {
            export super::SECRET;
            export super::Secret;
            export super::hidden;

            export mod b {
                export super::SECRET;
            }
        }

        fn main() -> Int {
            a::SECRET + a::b::SECRET
        }
        ",
    );

    // Each re-export is reported once, where it's private. The name is still bound, so its uses,
    // and re-exports of the binding, aren't reported again.
    assert_eq!(
        compiled.errors(),
        [
            "7:27 PrivateReexport(\"SECRET\")",
            "8:27 PrivateReexport(\"Secret\")",
            "9:27 PrivateReexport(\"hidden\")",
        ]
    );
}

#[test]
fn reexports_of_private_local_items_are_reported() {
    let compiled = compile(
        "
        mod m {
            const VALUE: Int = 1;
            type Value = Int;
            export VALUE;
            export Value;
        }

        fn main() -> Int {
            m::VALUE
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "5:20 PrivateReexport(\"VALUE\")",
            "6:20 PrivateReexport(\"Value\")",
        ]
    );
}

#[test]
fn reexports_of_private_imports_are_reported() {
    let compiled = compile(
        "
        mod inner {
            export const VALUE: Int = 1;
            export const OTHER: Int = 2;
        }

        import inner::OTHER;

        mod m {
            import super::inner::VALUE;
            export VALUE;
        }

        export mod n {
            export super::OTHER;
        }

        fn main() -> Int {
            m::VALUE + n::OTHER
        }
        ",
    );

    // An import's binding is private unless it's exported, as in Rust, even if the item it
    // brings in is public where it's declared
    assert_eq!(
        compiled.errors(),
        [
            "11:20 PrivateReexport(\"VALUE\")",
            "15:27 PrivateReexport(\"OTHER\")",
        ]
    );
}

#[test]
fn reexports_of_public_items_are_not_reported() {
    let compiled = compile(
        "
        export const VALUE: Int = 1;
        export inner::OTHER;
        const SECRET: Int = 3;

        mod inner {
            export const OTHER: Int = 2;
        }

        // The parent's re-export is resolved after this one, which still sees it
        export mod a {
            export super::OTHER;
            export super::VALUE;
            import super::SECRET;
            export LOCAL;

            export const LOCAL: Int = SECRET;
        }

        mod b {
            import super::inner::OTHER;
            export super::a::OTHER;
        }

        fn main() -> Int {
            a::OTHER + a::VALUE + a::LOCAL + b::OTHER
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("a::OTHER"), "inner");
}

#[test]
fn glob_reexports_through_super_only_export_public_items() {
    let compiled = compile(
        "
        const SECRET: Int = 1;
        export const PUBLIC: Int = 2;
        export inner::LATER;

        mod inner {
            export const LATER: Int = 3;
        }

        export mod a {
            export super::*;

            export fn secret() -> Int {
                SECRET
            }
        }

        fn main() -> Int {
            a::PUBLIC + a::LATER + a::secret() + a::SECRET
        }
        ",
    );

    // The private item is brought into `a` without being re-exported, so it isn't reported there
    assert_eq!(
        compiled.errors(),
        ["19:53 PrivateSymbol(\"SECRET\", \"a\")"]
    );
}
