//! Imports: which declaration an imported name refers to, how targeted and glob imports interact,
//! and the errors for imports that can't be resolved.

use std::fmt::Write;

use crate::test_utils::{Compiled, compile};

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
fn project_refers_to_the_file_of_a_single_file_program() {
    // `project` is the outermost module, the file here, from anywhere in it, so it reaches the
    // file's private items like `super` does from the modules directly in it. An item with the
    // same name in a module doesn't shadow it.
    let compiled = compile(
        "
        const TOP: Int = 1;

        mod outer {
            const TOP: Int = 2;

            export mod inner {
                import project::TOP;
                import project::outer::ONE;

                export fn value() -> Int {
                    TOP + project::TOP + super::TOP + ONE
                }
            }

            export const ONE: Int = 3;
        }

        fn main() -> Int {
            outer::inner::value() + project::TOP
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("outer::inner::TOP"), "(root)");
    assert_eq!(
        compiled.reference_origins("outer::inner::value", "TOP"),
        [
            "`TOP`: (root)",
            "`project::TOP`: (root)",
            "`super::TOP`: outer"
        ]
    );
    assert_eq!(
        compiled.reference_origins("main", "TOP"),
        ["`project::TOP`: (root)"]
    );
}

#[test]
fn project_paths_at_the_top_of_a_single_file_do_not_reach_the_prelude() {
    // At the top level of a single file, `project` names the scope the code is in, but it's still
    // only the file's own items (and imports): the prelude around the file isn't searched
    let compiled = compile(
        "
        import project::LOCAL;
        export project::helper;

        type Alias = project::Int;
        type Local = project::Number;
        type Number = Int;

        const LOCAL: Int = 1;
        const HASH: Bytes32 = project::sha256(1);
        const VALUE: Local = project::LOCAL + project::helper();

        export fn helper() -> Int {
            1
        }

        fn main() -> Int {
            VALUE
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "5:31 UndeclaredType(\"Int\")",
            "10:40 UndeclaredSymbol(\"sha256\")",
        ]
    );
    assert_eq!(compiled.type_origin("Local"), "(root)");
}

#[test]
fn project_imports_at_the_top_of_a_single_file_do_not_reach_the_prelude() {
    // An import through `project` at the top of the file doesn't find the prelude's items either.
    // Like any failed import, it binds its name to an error item in the file, and `project` names
    // the file, so a later use through `project` isn't reported again, nor is one without it.
    let compiled = compile(
        "
        import project::Int;
        import project::sha256;

        type Alias = project::Int;

        const HASH: Bytes32 = project::sha256(1) + sha256(2);

        fn main() -> Int {
            1
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "2:25 UnresolvedImport(\"Int\")",
            "3:25 UnresolvedImport(\"sha256\")",
        ]
    );
}

#[test]
fn failed_imports_at_the_root_bind_their_name_through_project() {
    // A name that a failed import binds to an error item in the root is found by imports and paths
    // through `project` from a nested module too, like through `super`, so only the failure is
    // reported
    let compiled = compile(
        "
        import nope::X;

        mod m {
            export mod n {
                import project::X;

                export fn f() -> Int {
                    X + project::X
                }
            }
        }

        fn main() -> Int {
            m::n::f() + project::X
        }
        ",
    );

    assert_eq!(compiled.errors(), ["2:16 UndeclaredSymbol(\"nope\")"]);
}

#[test]
fn glob_imports_of_project_at_the_top_of_a_single_file_do_not_bring_in_the_prelude() {
    // `import project::*` at the top of the file imports the file into itself, which brings in
    // nothing new, and in particular nothing from the prelude, which is still found as usual
    let compiled = compile(
        "
        import project::*;

        const LOCAL: Int = 1;

        fn main() -> Int {
            LOCAL
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(
        compiled.reference_origins("main", "LOCAL"),
        ["`LOCAL`: (root)"]
    );
}

#[test]
fn project_paths_into_other_modules_need_their_items_to_be_exported() {
    // Through `project`, an item inside another module is reached the same way as through `super`
    // or the module's name, so it has to be exported, even from a module that the path is in
    let compiled = compile(
        "
        mod a {
            const HIDDEN: Int = 1;

            mod b {
                export fn value() -> Int {
                    project::a::HIDDEN + super::super::a::HIDDEN + super::HIDDEN
                }
            }
        }

        fn main() -> Int {
            project::a::b::value()
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "7:33 PrivateSymbol(\"HIDDEN\", \"a\")",
            "7:59 PrivateSymbol(\"HIDDEN\", \"a\")",
            "13:25 PrivateSymbol(\"b\", \"a\")",
        ]
    );
}

#[test]
fn glob_and_exported_imports_of_project_bring_in_private_items() {
    // `project::*` brings in the root's private items, like `super::*` does from a module in it,
    // and `export project::...` re-exports a public one (a private one can't be re-exported, see
    // `reexports_of_private_items_through_project_are_reported`)
    let compiled = compile(
        "
        const TOP: Int = 1;
        export const SHARED: Int = 2;

        mod outer {
            export mod inner {
                import project::*;
                export project::SHARED;

                export fn value() -> Int {
                    TOP + main()
                }
            }
        }

        fn main() -> Int {
            outer::inner::SHARED
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_origin("outer::inner::SHARED"), "(root)");
}

#[test]
fn project_errors_are_reported() {
    let compiled = compile(
        "
        import project;
        import project::super::x;
        import project::{project};
        import a::project::b;

        mod a {
            export mod b {}
        }

        fn main() -> Int {
            project + project::super::main() + a::project::b + super::project::main()
        }
        ",
    );

    // An import that ends in `project` imports nothing, and a `project` after the start of a path
    // is skipped in an import, like `super` after a name
    assert_eq!(
        compiled.errors(),
        [
            "2:16 ProjectAtEnd",
            "3:25 UnresolvedSuper",
            "3:32 UnresolvedImport(\"x\")",
            "4:26 ProjectNotAtStart",
            "5:19 ProjectNotAtStart",
            "12:13 ProjectAtEnd",
            "12:32 UnresolvedSuper",
            "12:51 ProjectNotAtStart",
            "12:64 UnresolvedSuper",
            "12:71 ProjectNotAtStart",
        ]
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

/// Glob imports in `user` of `alpha` and `beta`, which both bring in `source`'s items: one
/// module with named imports (and `named_items`), and the other with a glob import, of `other`
/// instead if `collide` is true, which has a different `X`. The named imports are resolved
/// before the glob import, and the names of the two modules are in either order, so that either
/// of `user`'s glob imports is resolved first and binds the names. `user` has `user_items` after
/// its glob imports, which are exported if `exported` is true.
fn overlapping_globs(
    named_first: bool,
    named_items: &str,
    collide: bool,
    exported: bool,
    user_items: &str,
) -> String {
    let (named, globbed) = if named_first {
        ("alpha", "beta")
    } else {
        ("beta", "alpha")
    };
    let globbed_source = if collide { "other" } else { "source" };
    let keyword = if exported { "export" } else { "import" };

    format!(
        "
        mod source {{
            export const X: Int = 1;

            export mod m {{
                export const V: Int = 2;
            }}
        }}

        mod {named} {{
            export super::source::{{X, m}};
            {named_items}
        }}

        mod {globbed} {{
            export super::{globbed_source}::*;
        }}

        mod other {{
            export const X: Int = 4;
        }}

        mod user {{
            {keyword} super::alpha::*;
            {keyword} super::beta::*;
            {user_items}
        }}

        fn main() -> Int {{
            user::value()
        }}
        "
    )
}

/// The warnings about unused imports. `source`'s items are only used in some of the tests.
fn unused_imports(compiled: &Compiled) -> Vec<String> {
    compiled
        .warnings()
        .into_iter()
        .filter(|warning| warning.ends_with("UnusedImport"))
        .collect()
}

#[test]
fn glob_imports_of_the_same_item_are_both_used() {
    for named_first in [true, false] {
        let compiled = compile(&overlapping_globs(
            named_first,
            "",
            false,
            false,
            "export fn value() -> Int { X }",
        ));

        compiled.assert_no_errors();
        assert_eq!(
            unused_imports(&compiled),
            Vec::<String>::new(),
            "named first: {named_first}"
        );
    }
}

#[test]
fn glob_imports_of_the_same_unused_item_are_both_unused() {
    for named_first in [true, false] {
        let compiled = compile(&overlapping_globs(
            named_first,
            "",
            false,
            false,
            "export fn value() -> Int { 1 }",
        ));

        compiled.assert_no_errors();
        assert_eq!(
            unused_imports(&compiled),
            ["24:34 UnusedImport", "25:33 UnusedImport"],
            "named first: {named_first}"
        );
    }
}

#[test]
fn glob_imports_are_only_used_by_the_names_they_bring_in() {
    // `user` only uses `Y`, which only `alpha`'s or `beta`'s named module brings in, so the other
    // glob import is unused, even though it shares the binding of `X` with it
    for named_first in [true, false] {
        let compiled = compile(&overlapping_globs(
            named_first,
            "export const Y: Int = 3;",
            false,
            false,
            "export fn value() -> Int { Y }",
        ));
        let globbed = if named_first { "25:33" } else { "24:34" };

        compiled.assert_no_errors();
        assert_eq!(
            unused_imports(&compiled),
            [format!("{globbed} UnusedImport")],
            "named first: {named_first}"
        );
    }
}

#[test]
fn glob_imports_of_the_same_item_are_both_used_through_an_import_path() {
    // `import m::V;` uses `m` through whichever glob import bound it, while the imports are being
    // resolved
    for named_first in [true, false] {
        let compiled = compile(&overlapping_globs(
            named_first,
            "",
            false,
            false,
            "import m::V;\n\nexport fn value() -> Int { V }",
        ));

        compiled.assert_no_errors();
        assert_eq!(
            unused_imports(&compiled),
            Vec::<String>::new(),
            "named first: {named_first}"
        );
    }
}

#[test]
fn exported_glob_imports_of_the_same_item_both_bring_it_in() {
    for named_first in [true, false] {
        let compiled = compile(&overlapping_globs(
            named_first,
            "",
            false,
            true,
            "export fn value() -> Int { X }",
        ));

        compiled.assert_no_errors();
        assert_eq!(
            unused_imports(&compiled),
            Vec::<String>::new(),
            "named first: {named_first}"
        );
    }
}

#[test]
fn colliding_glob_imports_are_both_used() {
    for named_first in [true, false] {
        let compiled = compile(&overlapping_globs(
            named_first,
            "",
            true,
            false,
            "export fn value() -> Int { X }",
        ));

        assert_eq!(
            compiled.errors(),
            ["26:40 AmbiguousImport(\"X\")"],
            "named first: {named_first}"
        );
        assert_eq!(
            unused_imports(&compiled),
            Vec::<String>::new(),
            "named first: {named_first}"
        );
    }
}

#[test]
fn colliding_unused_glob_imports_are_both_unused() {
    for named_first in [true, false] {
        let compiled = compile(&overlapping_globs(
            named_first,
            "",
            true,
            false,
            "export fn value() -> Int { 1 }",
        ));

        compiled.assert_no_errors();
        assert_eq!(
            unused_imports(&compiled),
            ["24:34 UnusedImport", "25:33 UnusedImport"],
            "named first: {named_first}"
        );
    }
}

/// Two modules each export a different `V`, which `both` glob exports from both of them, and
/// `user` has `user_items`, in which `{first}` and `{second}` are replaced with the modules'
/// names. The first module's `V` has the value `first_value`. The names are `alpha` and `beta` if
/// `first_binds`, and otherwise `zeta` and `beta`, so that either glob import binds `V`, since
/// that depends on the modules' names.
fn colliding_glob_items(first_binds: bool, first_value: &str, user_items: &str) -> String {
    let first = if first_binds { "alpha" } else { "zeta" };
    let user_items = user_items
        .replace("{first}", first)
        .replace("{second}", "beta");

    format!(
        "
        mod {first} {{ export const V: Int = {first_value}; }}
        mod beta {{ export const V: Int = 2; }}
        mod both {{ export super::{first}::*; export super::beta::*; }}
        mod user {{ {user_items} }}
        fn main() -> Int {{ user::value() }}
        "
    )
}

/// The warnings about unused constants.
fn unused_constants(compiled: &Compiled) -> Vec<String> {
    compiled
        .warnings()
        .into_iter()
        .filter(|warning| warning.ends_with("UnusedConstant"))
        .collect()
}

#[test]
fn ambiguous_uses_of_colliding_glob_items_use_all_of_them() {
    // Whichever glob import bound `V`, its use is a use of both `V`s, so neither is reported as
    // unused, whether it's used directly, through a chain of glob imports, a named import, a
    // glob import of a named import, or a path
    for user_items in [
        "import super::{first}::*; import super::{second}::*; export fn value() -> Int { V }",
        "import super::both::*; export fn value() -> Int { V }",
        "import super::both::V; export fn value() -> Int { V }",
        "mod mid { export super::super::both::V; } import mid::*; export fn value() -> Int { V }",
        "export fn value() -> Int { super::both::V }",
    ] {
        for first_binds in [true, false] {
            let compiled = compile(&colliding_glob_items(first_binds, "1", user_items));
            let errors = compiled.errors();

            assert!(
                errors.len() == 1 && errors[0].ends_with("AmbiguousImport(\"V\")"),
                "{user_items}, first binds: {first_binds}: {errors:?}"
            );
            assert_eq!(
                unused_constants(&compiled),
                Vec::<String>::new(),
                "{user_items}, first binds: {first_binds}"
            );
        }
    }
}

#[test]
fn ambiguous_uses_through_a_glob_import_that_collides_again_use_all_of_them() {
    // `user` brings in a third `V`, which collides with the ambiguous `V` that `both` exports.
    // Whichever of the three the name is bound to, the use counts as a use of all of them.
    for third in ["aaa", "zzz"] {
        for first_binds in [true, false] {
            let source = colliding_glob_items(
                first_binds,
                "1",
                &format!(
                    "import super::{third}::*; import super::both::*; export fn value() -> Int {{ V }}"
                ),
            );
            let compiled = compile(&format!(
                "{source}mod {third} {{ export const V: Int = 3; }}"
            ));
            let errors = compiled.errors();

            assert!(
                errors.len() == 1 && errors[0].ends_with("AmbiguousImport(\"V\")"),
                "third: {third}, first binds: {first_binds}: {errors:?}"
            );
            assert_eq!(
                unused_constants(&compiled),
                Vec::<String>::new(),
                "third: {third}, first binds: {first_binds}"
            );
        }
    }
}

#[test]
fn ambiguous_uses_through_chains_resolved_in_later_passes_use_all_of_them() {
    // `both` only finds that `V` is ambiguous once the chain of glob imports to `beta` is resolved,
    // after the chain of named imports has already passed on `V`, so what else it could refer to
    // is passed on in later passes
    for first in ["alpha", "zeta"] {
        let compiled = compile(&format!(
            "
            mod n1 {{ export super::n2::V; }}
            mod n2 {{ export super::n3::V; }}
            mod n3 {{ export super::n4::V; }}
            mod n4 {{ export super::both::V; }}
            mod {first} {{ export const V: Int = 1; }}
            mod beta {{ export const V: Int = 2; }}
            mod g1 {{ export super::g2::*; }}
            mod g2 {{ export super::g3::*; }}
            mod g3 {{ export super::g4::*; }}
            mod g4 {{ export super::g5::*; }}
            mod g5 {{ export super::beta::*; }}
            mod both {{ export super::{first}::*; export super::g1::*; }}
            mod user {{ import super::n1::V; export fn value() -> Int {{ V }} }}
            mod globbed {{ import super::n1::*; export fn value() -> Int {{ V }} }}
            fn main() -> Int {{ user::value() + globbed::value() }}
            "
        ));
        let errors = compiled.errors();

        assert!(
            errors.len() == 1 && errors[0].ends_with("AmbiguousImport(\"V\")"),
            "first: {first}: {errors:?}"
        );
        assert_eq!(
            unused_constants(&compiled),
            Vec::<String>::new(),
            "first: {first}"
        );
    }
}

#[test]
fn unused_colliding_glob_items_are_all_unused() {
    for first_binds in [true, false] {
        let compiled = compile(&colliding_glob_items(
            first_binds,
            "1",
            "import super::{first}::*; import super::{second}::*; export fn value() -> Int { 1 }",
        ));
        let first = if first_binds { "2:34" } else { "2:33" };

        compiled.assert_no_errors();
        assert_eq!(
            unused_constants(&compiled),
            [
                format!("{first} UnusedConstant"),
                "3:33 UnusedConstant".to_string()
            ],
            "first binds: {first_binds}"
        );
    }
}

#[test]
fn ambiguous_uses_of_colliding_glob_items_are_not_recursive_through_the_others() {
    // The first module's `V` calls the function that uses `V`, which is only recursive if the
    // name is bound to it. The use counts as a use of the other `V` too, but isn't a reference to
    // it.
    for first_binds in [true, false] {
        let compiled = compile(&colliding_glob_items(
            first_binds,
            "super::user::value()",
            "import super::{first}::*; import super::{second}::*; export fn value() -> Int { V }",
        ));
        let recursive = compiled
            .errors()
            .into_iter()
            .filter(|error| error.contains("Recursive"))
            .collect::<Vec<_>>();

        if first_binds {
            assert_eq!(recursive, ["2:34 RecursiveConstant(\"V\")"]);
        } else {
            assert_eq!(recursive, Vec::<String>::new());
        }

        assert_eq!(
            unused_constants(&compiled),
            Vec::<String>::new(),
            "first binds: {first_binds}"
        );
    }
}

/// Two modules each export a module `m` with a different `X`, `T` and `n::Y`, which `both` glob
/// exports from both of them, so `both::m` is ambiguous, and `user` has `user_items`. The names
/// are `alpha` and `beta` if `first_binds`, and otherwise `omega` and `beta`, so that either glob
/// import binds `m`. `alpha` and `omega` have the same length, so the diagnostics are at the same
/// places.
fn colliding_glob_modules(first_binds: bool, user_items: &str) -> String {
    let first = if first_binds { "alpha" } else { "omega" };

    format!(
        "
        mod {first} {{ export mod m {{ export const X: Int = 1; export type T = Int; export mod n {{ export const Y: Int = 1; }} }} }}
        mod beta {{ export mod m {{ export const X: Int = 2; export type T = Int; export mod n {{ export const Y: Int = 2; }} }} }}
        mod both {{ export super::{first}::*; export super::beta::*; }}
        mod user {{ {user_items} }}
        fn main() -> Int {{ user::value() }}
        "
    )
}

/// The warnings about unused constants and type aliases.
fn unused_items(compiled: &Compiled) -> Vec<String> {
    compiled
        .warnings()
        .into_iter()
        .filter(|warning| {
            warning.ends_with("UnusedConstant") || warning.ends_with("UnusedTypeAlias")
        })
        .collect()
}

/// Asserts that every error is about an ambiguous name, and that there's at least one.
fn assert_only_ambiguous(compiled: &Compiled, context: &str) {
    let errors = compiled.errors();

    assert!(
        !errors.is_empty() && errors.iter().all(|error| error.contains("AmbiguousImport")),
        "{context}: {errors:?}"
    );
}

#[test]
fn members_of_ambiguous_modules_are_used_in_each_module() {
    // Whichever glob import bound `m`, the rest of a path through it is resolved in each module it
    // could refer to, so neither module's members are reported as unused, whether they're used
    // through expression and type paths, named or glob imports through the path, an import of the
    // ambiguous module itself, or a re-export of its members
    for user_items in [
        "export fn value() -> Int { let t: super::both::m::T = super::both::m::X; t + super::both::m::n::Y }",
        "import super::both::m::X; import super::both::m::T; import super::both::m::n::Y; export fn value() -> Int { let t: T = X; t + Y }",
        "import super::both::m::*; export fn value() -> Int { let t: T = X; t + n::Y }",
        "import super::both::m::n::*; export fn value() -> Int { let t: super::both::m::T = super::both::m::X; t + Y }",
        "import super::both::m; export fn value() -> Int { let t: m::T = m::X; t + m::n::Y }",
        "import super::both::*; export fn value() -> Int { let t: m::T = m::X; t + m::n::Y }",
        "mod mid { export super::super::both::m::*; } export fn value() -> Int { let t: mid::T = mid::X; t + mid::n::Y }",
        "mod mid { export super::super::both::m::X; export super::super::both::m::T; export super::super::both::m::n; } import mid::*; export fn value() -> Int { let t: T = X; t + n::Y }",
    ] {
        for first_binds in [true, false] {
            let compiled = compile(&colliding_glob_modules(first_binds, user_items));
            let context = format!("{user_items}, first binds: {first_binds}");

            assert_only_ambiguous(&compiled, &context);
            assert_eq!(unused_items(&compiled), Vec::<String>::new(), "{context}");
        }
    }
}

#[test]
fn members_of_ambiguous_modules_used_by_neither_are_unused_in_each() {
    for first_binds in [true, false] {
        let compiled = compile(&colliding_glob_modules(
            first_binds,
            "import super::both::m::X; export fn value() -> Int { 1 + super::both::m::X }",
        ));

        assert_only_ambiguous(&compiled, &format!("first binds: {first_binds}"));
        assert_eq!(
            unused_items(&compiled),
            [
                "2:73 UnusedTypeAlias",
                "2:110 UnusedConstant",
                "3:72 UnusedTypeAlias",
                "3:109 UnusedConstant",
            ],
            "first binds: {first_binds}"
        );
    }
}

#[test]
fn members_of_ambiguous_modules_exported_from_a_file_are_exported_from_each_module() {
    // The file exports what `both::m::X` names, which counts as exporting the `X` in each module
    // that `m` could refer to, whether it's exported by name or through a glob export. A glob
    // export exports `ONLY` too, which only `beta`'s `m` has, even when `m` is bound to the first
    // module's, unless the file has its own `ONLY`, but exporting `X` by name doesn't.
    for (export, expected) in [
        ("export both::m::X;", vec!["3:81 UnusedConstant"]),
        ("export both::m::*;", vec![]),
        // The file's own `ONLY` hides `beta`'s, which the glob export never exports
        (
            "export both::m::*; const ONLY: Int = 4;",
            vec!["3:81 UnusedConstant", "5:42 UnusedConstant"],
        ),
        // A glob import that isn't exported doesn't export anything
        (
            "import both::m::*;",
            vec![
                "2:57 UnusedConstant",
                "3:56 UnusedConstant",
                "3:81 UnusedConstant",
            ],
        ),
    ] {
        for first_binds in [true, false] {
            let first = if first_binds { "alpha" } else { "omega" };
            let compiled = compile(&format!(
                "
                mod {first} {{ export mod m {{ export const X: Int = 1; }} }}
                mod beta {{ export mod m {{ export const X: Int = 2; export const ONLY: Int = 3; }} }}
                mod both {{ export super::{first}::*; export super::beta::*; }}
                {export}
                fn main() -> Int {{ 1 }}
                "
            ));
            let context = format!("{export}, first binds: {first_binds}");

            assert_only_ambiguous(&compiled, &context);
            assert_eq!(unused_items(&compiled), expected, "{context}");
        }
    }
}

#[test]
fn members_of_modules_ambiguous_at_several_segments_are_used_in_each_module() {
    // `both::m` could be `{outer}::m` or `gamma::m`, and in `{outer}::m`, `n` could be
    // `{first}::n` or `beta::n`, so `both::m::n::Y` could be any of the three `Y`s, whichever
    // glob imports bound the names
    for outer in ["aaa", "zzz"] {
        for first in ["alpha", "zeta"] {
            for user_items in [
                "export fn value() -> Int { super::both::m::n::Y }",
                "import super::both::m::n::Y; export fn value() -> Int { Y }",
                "import super::both::m::n::*; export fn value() -> Int { Y }",
                "import super::both::m::*; export fn value() -> Int { n::Y }",
            ] {
                let compiled = compile(&format!(
                    "
                    mod {first} {{ export mod n {{ export const Y: Int = 1; }} }}
                    mod beta {{ export mod n {{ export const Y: Int = 2; }} }}
                    mod {outer} {{ export mod m {{ export super::super::{first}::*; export super::super::beta::*; }} }}
                    mod gamma {{ export mod m {{ export mod n {{ export const Y: Int = 3; }} }} }}
                    mod both {{ export super::{outer}::*; export super::gamma::*; }}
                    mod user {{ {user_items} }}
                    fn main() -> Int {{ user::value() }}
                    "
                ));
                let context = format!("{user_items}, outer: {outer}, first: {first}");

                assert_only_ambiguous(&compiled, &context);
                assert_eq!(unused_items(&compiled), Vec::<String>::new(), "{context}");
            }
        }
    }
}

#[test]
fn members_of_only_one_ambiguous_module_are_used_whichever_module_is_bound() {
    // Only `beta`'s `m` has `ONLY`. When the first module's `m` is bound, the path fails, but
    // `ONLY` could still be what it refers to, so it isn't reported as unused in either order.
    // `X` is in both, and `MISSING` is in neither, which is only reported as undeclared.
    for first_binds in [true, false] {
        let first = if first_binds { "alpha" } else { "zeta" };
        let compiled = compile(&format!(
            "
            mod {first} {{ export mod m {{ export const X: Int = 1; }} }}
            mod beta {{ export mod m {{ export const X: Int = 2; export const ONLY: Int = 3; }} }}
            mod both {{ export super::{first}::*; export super::beta::*; }}
            fn main() -> Int {{ both::m::X + both::m::ONLY + both::m::MISSING }}
            "
        ));
        let errors = compiled.errors();

        assert!(
            errors
                .iter()
                .any(|error| error.ends_with("UndeclaredSymbol(\"MISSING\")")),
            "first binds: {first_binds}: {errors:?}"
        );
        assert_eq!(
            errors
                .iter()
                .any(|error| error.ends_with("UndeclaredSymbol(\"ONLY\")")),
            first_binds,
            "first binds: {first_binds}: {errors:?}"
        );
        assert_eq!(
            unused_items(&compiled),
            Vec::<String>::new(),
            "first binds: {first_binds}"
        );
    }
}

/// Two modules each export a module `m`, which `both` glob exports from both of them, so `both::m`
/// is ambiguous. The first one's `m` has `first_items`, and `beta`'s has `second_items`. The rest
/// is `rest`, where `{first}` is replaced with the first module's name, which is `alpha` if
/// `first_binds`, and otherwise `omega`, so that either glob import binds `m`.
fn colliding_modules(
    first_binds: bool,
    first_items: &str,
    second_items: &str,
    rest: &str,
) -> String {
    let first = if first_binds { "alpha" } else { "omega" };
    let rest = rest.replace("{first}", first);

    format!(
        "
        mod {first} {{ export mod m {{ {first_items} }} }}
        mod beta {{ export mod m {{ {second_items} }} }}
        mod both {{ export super::{first}::*; export super::beta::*; }}
        {rest}
        "
    )
}

#[test]
fn names_that_only_another_ambiguous_module_has_are_used_through_glob_imports() {
    // Only `beta`'s `m` has `ONLY`, so a glob import of `both::m` only brings it in when the name
    // is bound to that one. Otherwise, the use of `ONLY` finds the top-level one, or a failed
    // import's error item, or nothing. Either way, it could refer to `beta`'s `ONLY` too, so that
    // isn't reported as unused in either order, whether the glob import is direct or through a
    // re-export, and neither is the glob import.
    for rest in [
        "const ONLY: Int = 3; mod user { import super::both::m::*; export fn value() -> Int { ONLY } } fn main() -> Int { user::value() + ONLY }",
        "mod user { import super::nowhere::ONLY; import super::both::m::*; export fn value() -> Int { ONLY } } mod nowhere {} fn main() -> Int { user::value() }",
        "mod user { import super::both::m::*; export fn value() -> Int { ONLY } } fn main() -> Int { user::value() }",
        "mod mid { export super::both::m::*; } mod user { import super::mid::*; export fn value() -> Int { ONLY } } fn main() -> Int { user::value() }",
        "mod mid { export super::both::m::*; } fn main() -> Int { mid::ONLY }",
    ] {
        for first_binds in [true, false] {
            let compiled = compile(&colliding_modules(
                first_binds,
                "export const X: Int = 1;",
                "export const ONLY: Int = 2;",
                rest,
            ));
            let context = format!("{rest}, first binds: {first_binds}");

            assert_eq!(
                unused_items(&compiled),
                ["2:49 UnusedConstant"],
                "{context}"
            );
            assert_eq!(unused_imports(&compiled), Vec::<String>::new(), "{context}");
        }
    }
}

#[test]
fn names_hidden_by_failed_imports_could_be_what_glob_imports_through_ambiguous_modules_bring_in() {
    // The import of `X` fails in its path, and its error item hides what the glob import brings
    // in. When `m` is bound to the first module's, that's the first module's `X`, and `beta`'s
    // `m`, the other module it could name, is empty. When it's bound to `beta`'s, the glob
    // import brings in nothing, but the first module's `X` is what it could bring in. Either
    // way, the use could refer to the first module's `X`, and the glob import is used.
    for first_binds in [true, false] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "export const X: Int = 1;",
            "",
            "mod nowhere {} mod user { import super::nowhere::n::X; import super::both::m::*; export fn value() -> Int { X } } fn main() -> Int { user::value() }",
        ));
        let context = format!("first binds: {first_binds}");

        assert_eq!(unused_items(&compiled), Vec::<String>::new(), "{context}");
        assert_eq!(unused_imports(&compiled), Vec::<String>::new(), "{context}");
    }
}

#[test]
fn names_bound_by_declarations_and_named_imports_hide_what_glob_imports_could_bring_in() {
    // A declaration or named import of `ONLY` hides what the glob import brings in, whichever
    // `m` is bound, so `beta`'s `ONLY` and the glob import are unused in either order
    for (rest, glob) in [
        (
            "mod user { import super::other::ONLY; import super::both::m::*; export fn value() -> Int { ONLY } } mod other { export const ONLY: Int = 9; } fn main() -> Int { user::value() }",
            "5:70 UnusedImport",
        ),
        (
            "mod user { const ONLY: Int = 9; import super::both::m::*; export fn value() -> Int { ONLY } } fn main() -> Int { user::value() }",
            "5:64 UnusedImport",
        ),
    ] {
        for first_binds in [true, false] {
            let compiled = compile(&colliding_modules(
                first_binds,
                "export const X: Int = 1;",
                "export const ONLY: Int = 2;",
                rest,
            ));
            let context = format!("{rest}, first binds: {first_binds}");

            assert_eq!(
                unused_items(&compiled),
                ["2:49 UnusedConstant", "3:48 UnusedConstant"],
                "{context}"
            );
            assert_eq!(unused_imports(&compiled), [glob], "{context}");
        }
    }
}

#[test]
fn exported_glob_imports_whose_alternatives_are_all_hidden_are_empty() {
    // `r` declares `X`, which hides everything `src`'s glob import could bring in, so `r`'s
    // glob export of `src` brings in nothing, whichever `m` is bound, and is reported
    for first_binds in [true, false] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "export const X: Int = 1;",
            "export const X: Int = 2;",
            "mod src { export super::both::m::*; } mod r { export super::src::*; export const X: Int = 3; } fn main() -> Int { r::X }",
        ));

        assert_eq!(
            unused_imports(&compiled),
            ["5:74 UnusedImport"],
            "first binds: {first_binds}"
        );
    }
}

#[test]
fn paths_through_ambiguous_modules_prefer_the_expected_kind_in_each() {
    // `beta`'s `m` has both a constant and a type alias `X`. A use of `both::m::X` as a value
    // could refer to its constant, but never its type alias, and a use as a type the other way
    // around, whichever `m` is bound
    for (first_items, main, expected) in [
        (
            "export const X: Int = 1;",
            "fn main() -> Int { both::m::X }",
            "3:72 UnusedTypeAlias",
        ),
        (
            "export type X = Int;",
            "fn main() -> Int { let x: both::m::X = 1; x }",
            "3:48 UnusedConstant",
        ),
    ] {
        for first_binds in [true, false] {
            let compiled = compile(&colliding_modules(
                first_binds,
                first_items,
                "export const X: Int = 2; export type X = Int;",
                main,
            ));

            assert_eq!(
                unused_items(&compiled),
                [expected],
                "{main}, first binds: {first_binds}"
            );
        }
    }
}

#[test]
fn named_imports_through_ambiguous_modules_hide_other_kinds_they_could_bring_in() {
    // The first module's `m` has a type alias `Y` and `beta`'s a function `Y`, so `import
    // both::m::Y` brings in one or the other, and could bring in either. Both hide what `u2`'s
    // glob export of `both::m` could bring in, so nothing uses either `Y`, in either order, and
    // the import is unused. (The glob export never brings anything in either, since it's bound
    // to the same `m` as the import, but since the import might not bring in a `Y` of each kind,
    // the glob export counts as possibly bringing in the one it hides, so it isn't reported as
    // bringing in nothing, in either order.)
    for first_binds in [true, false] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "export type Y = Int;",
            "export fn Y() -> Int { 1 }",
            "mod u2 { export super::both::m::*; import super::both::m::Y; } import u2::*; fn main() -> Int { 1 }",
        ));
        let context = format!("first binds: {first_binds}");
        let warnings = compiled
            .warnings()
            .into_iter()
            .filter(|warning| warning.contains(" Unused"))
            .collect::<Vec<_>>();

        assert_eq!(
            warnings,
            [
                "2:48 UnusedTypeAlias",
                "3:45 UnusedFunction",
                "5:67 UnusedImport",
                "5:83 UnusedImport",
            ],
            "{context}"
        );
    }
}

#[test]
fn named_imports_through_ambiguous_modules_are_used_through_other_kinds() {
    // The import brings in the first module's type alias `Y` when that `m` is bound, but could
    // bring in `beta`'s function `Y`, which the call could refer to, so the import and the
    // function are used in either order. (When the type alias is brought in, the call finds it
    // and reports it as a type, so it's only used in that order, which the errors differ in too.)
    for first_binds in [true, false] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "export type Y = Int;",
            "export fn Y() -> Int { 1 }",
            "mod user { import super::both::m::Y; export fn value() -> Int { Y() } } fn main() -> Int { user::value() }",
        ));
        let context = format!("first binds: {first_binds}");

        assert!(
            !unused_items(&compiled).contains(&"3:47 UnusedFunction".to_string()),
            "{context}"
        );
        assert_eq!(unused_imports(&compiled), Vec::<String>::new(), "{context}");
    }
}

#[test]
fn named_imports_that_fail_do_not_hide_what_glob_imports_could_bring_in() {
    // Found by renaming modules at random. `u`'s import of `Y` fails in either order, since the
    // `n` it's bound to has no `Y`, though another `n` its path could name does. Its error item
    // only fills `Y` when the glob import doesn't bring one in, which it does in one order (from
    // `{b}`'s `m`), so what the glob import could bring in is used in either order.
    let template = "
        mod {a} { export mod m { export const X: Int = 1; export const Y: Int = 2; export type T = Int; export mod n { export const X: Int = 3; export const Y: Int = 4; export type T = Int; } } }
        mod {b} { export mod m { export const X: Int = 5; export const Y: Int = 6; export mod n { export const X: Int = 7; } } }
        mod {c} { export mod m { export mod n { export const X: Int = 8; export const Y: Int = 9; } } }
        mod {d} { export mod m { export const X: Int = 10; export type T = Int; export mod n { export const X: Int = 11; export type T = Int; } export super::super::{c}::m::*; } }
        mod {e} { export super::{d}::*; export super::{c}::*; export super::{b}::*; }
        mod {f} { export super::{c}::*; export super::{b}::*; export super::{e}::*; }
        mod {g} { export super::{f}::*; export super::{c}::*; export super::{a}::*; }
        mod u { import super::{e}::m::*; import super::{f}::m::n::Y; export fn value() -> Int { super::{b}::m::X + X + super::{e}::m::n::X + Y } }
        fn main() -> Int { u::value() }
    ";
    let orders = [
        ["hx", "dx", "hk", "bk", "ck", "dz", "fz"],
        ["dx", "dz", "fz", "hx", "hk", "hj", "ck"],
    ];
    let mut results = Vec::new();

    for names in orders {
        let source = ["a", "b", "c", "d", "e", "f", "g"]
            .into_iter()
            .zip(names)
            .fold(template.to_string(), |source, (key, name)| {
                source.replace(&format!("{{{key}}}"), name)
            });
        let compiled = compile(&source);

        results.push((compiled.errors(), unused_items(&compiled)));
    }

    assert_eq!(results[0], results[1]);
    assert!(
        !results[0].1.contains(&"3:71 UnusedConstant".to_string()),
        "{results:?}"
    );
}

#[test]
fn named_imports_that_might_bring_in_nothing_do_not_hide_glob_imports() {
    // The import of `Y` brings in the first module's type alias or `beta`'s constant. With the
    // constant, it hides `other`'s constant `Y` that the glob import brings in; with the type
    // alias, the use finds `other`'s. Either way, the use could refer to either constant, so
    // both constants and both imports are used (the type alias isn't).
    for first_binds in [true, false] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "export type Y = Int;",
            "export const Y: Int = 2;",
            "mod other { export const Y: Int = 5; } mod user { import super::both::m::Y; import super::other::*; export fn value() -> Int { Y } } fn main() -> Int { user::value() }",
        ));
        let context = format!("first binds: {first_binds}");

        assert_eq!(
            unused_items(&compiled),
            ["2:48 UnusedTypeAlias"],
            "{context}"
        );
        assert_eq!(unused_imports(&compiled), Vec::<String>::new(), "{context}");
    }
}

#[test]
fn hiding_by_duplicate_named_imports_does_not_depend_on_which_binds() {
    // Found by renaming modules at random. `u` imports `T` twice, once through an unambiguous
    // path and once through the ambiguous `{f}::m`, where it might bring in nothing. Which of
    // them binds `T` depends on module names, but the first always brings it in, so `T` is
    // hidden from what the glob imports could bring in either way, and `{h}`'s glob import
    // (which could only bring in a `T`) is unused in either order.
    let template = "
        mod {a} { export mod m { export const Y: Int = 1; export mod n { export const X: Int = 2; export const Y: Int = 3; export type T = Int; } } }
        mod {b} { export mod m { export const X: Int = 4; export const Y: Int = 5; export type T = Int; } }
        mod {c} { export mod m { export const Y: Int = 6; export type T = Int; } }
        mod {d} { export mod m { export const X: Int = 7; export const Y: Int = 8; export mod n { export type T = Int; } export super::super::{b}::m::*; } }
        mod {e} { export super::{d}::*; export super::{b}::*; export super::{a}::*; }
        mod {f} { export super::{c}::*; export super::{e}::*; export super::{b}::*; }
        mod {g} { export super::{d}::*; }
        mod {h} { export super::{f}::m::*; }
        mod u { import super::{g}::m::T; import super::{f}::m::T; import super::{e}::*; import super::{d}::*; import super::{h}::*; export fn value() -> Int { { let t: T = 1; t } + { let t: T = 1; t } + m::Y + m::Y + m::X } }
        fn main() -> Int { u::value() }
    ";
    let orders = [
        ["fj", "bj", "hj", "gz", "cj", "fk", "bx", "fz"],
        ["cj", "fz", "hj", "bj", "bx", "fj", "fk", "gz"],
    ];
    let mut results = Vec::new();

    for names in orders {
        let source = ["a", "b", "c", "d", "e", "f", "g", "h"]
            .into_iter()
            .zip(names)
            .fold(template.to_string(), |source, (key, name)| {
                source.replace(&format!("{{{key}}}"), name)
            });
        let compiled = compile(&source);

        results.push((compiled.errors(), compiled.warnings()));
    }

    assert_eq!(results[0], results[1]);
    assert!(
        results[0].1.contains(&"10:125 UnusedImport".to_string()),
        "{results:?}"
    );
}

#[test]
fn members_of_ambiguous_modules_used_from_outside_are_needed_exports_in_each() {
    // A use of `both::m::X` from outside of either `m`, by path or by import, needs the `X` in
    // each `m` it could refer to to be exported, whichever is bound. `Z` is only used inside of
    // its module, so its export is still reported.
    for user in [
        "fn main() -> Int { both::m::X + alpha_or_omega() }",
        "import both::m::X; fn main() -> Int { X + alpha_or_omega() }",
    ] {
        for first_binds in [true, false] {
            let compiled = compile(&colliding_modules(
                first_binds,
                "export const X: Int = 1; export const Z: Int = 2; export fn f() -> Int { X + Z }",
                "export const X: Int = 3; export fn f() -> Int { X }",
                &format!(
                    "fn alpha_or_omega() -> Int {{ {{first}}::m::f() + beta::m::f() }} {user}"
                ),
            ));
            let context = format!("{user}, first binds: {first_binds}");
            let exports = compiled
                .warnings()
                .into_iter()
                .filter(|warning| warning.ends_with("UnusedExport"))
                .collect::<Vec<_>>();

            assert_eq!(exports, ["2:74 UnusedExport"], "{context}");
        }
    }
}

#[test]
fn paths_through_ambiguous_modules_fall_back_to_the_other_kind_in_each() {
    // `beta`'s `m` only has a type alias `X`, which a use of `both::m::X` as a value finds when
    // that `m` is bound, and reports as a type. When the first module's `m` is bound, the path
    // finds its constant, but could refer to `beta`'s type alias too.
    for first_binds in [true, false] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "export const X: Int = 1;",
            "export type X = Int;",
            "fn main() -> Int { both::m::X }",
        ));

        assert_eq!(
            unused_items(&compiled),
            Vec::<String>::new(),
            "first binds: {first_binds}"
        );
    }
}

#[test]
fn imports_that_other_modules_name_paths_through_are_used() {
    // `beta`'s `m` has `n` through a private import, which `both::m::n` only finds when that `m`
    // is bound (and reports as private). Either way, the import's path could name it through
    // that import, which is used.
    for first_binds in [true, false] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "export mod n { export const Y: Int = 1; }",
            "import super::super::src::n;",
            "mod src { export mod n { export const Y: Int = 2; } } mod user { import super::both::m::n::Y; export fn value() -> Int { Y } } fn main() -> Int { user::value() }",
        ));

        assert_eq!(
            unused_imports(&compiled),
            Vec::<String>::new(),
            "first binds: {first_binds}"
        );
    }
}

#[test]
fn named_imports_through_ambiguous_modules_only_use_what_the_others_export() {
    // `beta`'s `m` has a private `X`, which an import can't bring in, so it's unused whichever
    // `m` is bound. When it's `beta`'s, the import fails as private, but it could still bring in
    // the first module's `X`, which its use could refer to, so that's used in either order.
    for first_binds in [true, false] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "export const X: Int = 1;",
            "const X: Int = 2;",
            "mod user { import super::both::m::X; export fn value() -> Int { X } } fn main() -> Int { user::value() }",
        ));

        assert_eq!(
            unused_items(&compiled),
            ["3:41 UnusedConstant"],
            "first binds: {first_binds}"
        );
    }

    // `beta`'s `m` only has `ONLY` through a private glob import of `both2::k`, which an import
    // can't bring in either, so neither `k`'s `ONLY` is used in any order
    for first_binds in [true, false] {
        for second_k in ["kk", "zz"] {
            let compiled = compile(&colliding_modules(
                first_binds,
                "export const ONLY: Int = 1;",
                "import super::super::both2::k::*;",
                &format!(
                    "mod kq {{ export mod k {{ export const ONLY: Int = 3; }} }} mod {second_k} {{ export mod k {{ export const ONLY: Int = 4; }} }} mod both2 {{ export super::kq::*; export super::{second_k}::*; }} mod user {{ import super::both::m::ONLY; export fn value() -> Int {{ ONLY }} }} fn main() -> Int {{ user::value() }}"
                ),
            ));

            assert_eq!(
                unused_items(&compiled),
                ["5:46 UnusedConstant", "5:102 UnusedConstant"],
                "first binds: {first_binds}, second k: {second_k}"
            );
        }
    }
}

#[test]
fn glob_imports_through_ambiguous_modules_are_used_through_names_another_glob_import_binds() {
    // Only the first module's `m` has `n`, which `user`'s first glob import brings in too. When
    // `both::m` is bound to the first module's `m`, both glob imports bring in the same `n`;
    // otherwise only the first one does, but the second could. Either way, both are used.
    for first_binds in [true, false] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "export mod n { export const Y: Int = 1; }",
            "export const Z: Int = 2;",
            "mod user { import super::{first}::m::*; import super::both::m::*; export fn value() -> Int { n::Y } } fn main() -> Int { user::value() }",
        ));
        let context = format!("first binds: {first_binds}");

        assert_eq!(
            unused_items(&compiled),
            ["3:48 UnusedConstant"],
            "{context}"
        );
        assert_eq!(unused_imports(&compiled), Vec::<String>::new(), "{context}");
    }
}

#[test]
fn glob_imports_through_ambiguous_modules_that_could_bring_in_unused_names_are_unused() {
    // The first module's `m` is empty, so the glob import brings in nothing when `m` is bound to
    // it. It's reported as unused in either order when nothing it could bring in is used, and not
    // reported when something is.
    for (body, unused) in [("1", true), ("X", false)] {
        for first_binds in [true, false] {
            let compiled = compile(&colliding_modules(
                first_binds,
                "",
                "export const X: Int = 2;",
                &format!(
                    "mod user {{ import super::both::m::*; export fn value() -> Int {{ {body} }} }} fn main() -> Int {{ user::value() }}"
                ),
            ));
            let context = format!("body: {body}, first binds: {first_binds}");
            let expected = if unused {
                vec!["5:43 UnusedImport"]
            } else {
                vec![]
            };

            assert_eq!(unused_imports(&compiled), expected, "{context}");
        }
    }
}

#[test]
fn glob_imports_that_could_bring_in_the_same_module_are_both_used() {
    // `both::m` could be the same `m` that the first glob import brings in, and the uses of it
    // count as uses through both glob imports, whichever `m` is bound
    for first_binds in [true, false] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "export mod n { export const Y: Int = 1; }",
            "export mod n { export const Y: Int = 2; }",
            "mod user { import super::{first}::m::*; import super::both::m::*; export fn value() -> Int { n::Y } } fn main() -> Int { user::value() }",
        ));
        let context = format!("first binds: {first_binds}");

        assert_eq!(unused_items(&compiled), Vec::<String>::new(), "{context}");
        assert_eq!(unused_imports(&compiled), Vec::<String>::new(), "{context}");
    }
}

#[test]
fn only_the_first_named_import_of_an_item_is_used() {
    // Two named imports bring in the same `X`. Whichever is resolved first binds the name, which
    // depends on module names, but the first one in the source is the one that's used, so the
    // other is reported as unused in either order, and both are if the name isn't used.
    for (first, second) in [("ma", "mz"), ("mz", "ma")] {
        for (body, expected) in [
            ("X", vec!["5:64 UnusedImport"]),
            ("1", vec!["5:43 UnusedImport", "5:64 UnusedImport"]),
        ] {
            let compiled = compile(&format!(
                "
                mod src {{ export const X: Int = 1; }}
                mod {first} {{ export super::src::X; }}
                mod {second} {{ export super::src::X; }}
                mod u {{ import super::{first}::X; import super::{second}::X; export fn value() -> Int {{ {body} }} }}
                fn main() -> Int {{ u::value() }}
                "
            ));
            let context = format!("first: {first}, body: {body}");

            compiled.assert_no_errors();
            assert_eq!(unused_imports(&compiled), expected, "{context}");
        }
    }
}

#[test]
fn named_imports_that_also_bring_in_another_item_are_used_for_it() {
    // Both imports bring in the same constant `X`, but only the second brings in a type `X`, so
    // it's still used for that when it's used as a type, and only unused when it isn't
    for (first, second) in [("ma", "mz"), ("mz", "ma")] {
        for (body, expected) in [
            ("let x: X = X; x", vec![]),
            ("X", vec!["4:60 UnusedTypeAlias", "5:64 UnusedImport"]),
        ] {
            let compiled = compile(&format!(
                "
                mod src {{ export const X: Int = 1; }}
                mod {first} {{ export super::src::X; }}
                mod {second} {{ export super::src::X; export type X = Int; }}
                mod u {{ import super::{first}::X; import super::{second}::X; export fn value() -> Int {{ {body} }} }}
                fn main() -> Int {{ u::value() }}
                "
            ));
            let context = format!("first: {first}, body: {body}");
            let warnings = compiled
                .warnings()
                .into_iter()
                .filter(|warning| warning.contains(" Unused"))
                .collect::<Vec<_>>();

            compiled.assert_no_errors();
            assert_eq!(warnings, expected, "{context}");
        }
    }
}

#[test]
fn only_the_first_of_several_named_imports_of_an_item_is_used() {
    // Three named imports of the same `X`, with their modules named in every order, so that each
    // of them binds the name in some order. The first one in the source is always the one that's
    // used.
    let names = ["ma", "mm", "mz"];

    for order in [
        [0, 1, 2],
        [0, 2, 1],
        [1, 0, 2],
        [1, 2, 0],
        [2, 0, 1],
        [2, 1, 0],
    ] {
        let [a, b, c] = order.map(|index| names[index]);

        for (body, expected) in [
            ("X", vec!["6:64 UnusedImport", "6:85 UnusedImport"]),
            (
                "1",
                vec![
                    "6:43 UnusedImport",
                    "6:64 UnusedImport",
                    "6:85 UnusedImport",
                ],
            ),
        ] {
            let compiled = compile(&format!(
                "
                mod src {{ export const X: Int = 1; }}
                mod {a} {{ export super::src::X; }}
                mod {b} {{ export super::src::X; }}
                mod {c} {{ export super::src::X; }}
                mod u {{ import super::{a}::X; import super::{b}::X; import super::{c}::X; export fn value() -> Int {{ {body} }} }}
                fn main() -> Int {{ u::value() }}
                "
            ));
            let context = format!("{a} {b} {c}, body: {body}");

            compiled.assert_no_errors();
            assert_eq!(unused_imports(&compiled), expected, "{context}");
        }
    }
}

#[test]
fn only_the_first_exported_import_of_an_item_is_used() {
    // Both re-exports of `X` are exported, so the first one in the source is the one that's used,
    // and the other is reported, whichever binds the name
    for (first, second) in [("ma", "mz"), ("mz", "ma")] {
        let compiled = compile(&format!(
            "
            mod src {{ export const X: Int = 1; }}
            mod {first} {{ export super::src::X; }}
            mod {second} {{ export super::src::X; }}
            mod re {{ export super::{second}::X; export super::{first}::X; }}
            fn main() -> Int {{ re::X }}
            "
        ));

        compiled.assert_no_errors();
        assert_eq!(
            unused_imports(&compiled),
            ["5:61 UnusedImport"],
            "first: {first}"
        );
    }
}

#[test]
fn named_imports_that_find_an_item_through_another_are_the_unused_ones() {
    // `import X;` finds `X` through `import super::src::X;`, which it needs, so it's the one
    // that's reported, even though it comes first
    let compiled = compile(
        "
        mod src { export const X: Int = 1; }
        mod user { import X; import super::src::X; export fn value() -> Int { X } }
        fn main() -> Int { user::value() }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(unused_imports(&compiled), ["3:27 UnusedImport"]);
}

#[test]
fn private_named_imports_of_an_exported_item_are_unused() {
    // The exported import is the one that's used, even when it comes after the private one and
    // whichever binds the name, so the private one is reported as unused, and so is an exact
    // duplicate
    for (first, second) in [("ma", "mz"), ("mz", "ma")] {
        let compiled = compile(&format!(
            "
            mod src {{ export const X: Int = 1; }}
            mod {first} {{ export super::src::X; }}
            mod {second} {{ export super::src::X; }}
            mod re {{ import super::{second}::X; export super::{first}::X; }}
            mod dup {{ import super::src::X; import super::src::X; export fn value() -> Int {{ X }} }}
            fn main() -> Int {{ re::X + dup::value() }}
            "
        ));
        let context = format!("first: {first}");

        compiled.assert_no_errors();
        assert_eq!(
            unused_imports(&compiled),
            ["5:40 UnusedImport", "6:64 UnusedImport"],
            "{context}"
        );
    }
}

#[test]
fn named_imports_of_the_same_item_through_ambiguous_modules_use_every_alternative() {
    // `beta` sorts before `both` and `omega`, so `import super::beta::m::X;` binds `X`. When
    // `both::m` is bound to `beta`'s `m`, the other import finds the same `X`, and what else its
    // path could refer to is recorded on the binding, so `omega`'s `X` is used too, and the
    // other import is the unused one. When `m` is bound to the first module's, the imports bring
    // in different items, which is reported as a duplicate. That import fails in that order
    // only, so it's out of scope, like an import whose path fails: the first module's `X` is
    // unused, and the failed import isn't reported.
    for (first_binds, errors, unused, imports) in [
        (
            true,
            vec!["5:66 AmbiguousImport(\"m\")", "5:69 DuplicateSymbol(\"X\")"],
            vec!["2:49 UnusedConstant"],
            vec![],
        ),
        (
            false,
            vec!["5:66 AmbiguousImport(\"m\")"],
            vec![],
            vec!["5:69 UnusedImport"],
        ),
    ] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "export const X: Int = 1;",
            "export const X: Int = 2;",
            "mod user { import super::beta::m::X; import super::both::m::X; export fn value() -> Int { X } } fn main() -> Int { user::value() }",
        ));
        let context = format!("first binds: {first_binds}");

        assert_eq!(compiled.errors(), errors, "{context}");
        assert_eq!(unused_items(&compiled), unused, "{context}");
        assert_eq!(unused_imports(&compiled), imports, "{context}");
    }
}

#[test]
fn glob_re_exports_only_pass_on_what_their_module_does_not_hide() {
    // `relay2` has its own `X`, by declaration or named import, which hides the first module's
    // `X` that `relay`'s glob export could bring in, so `user`'s use of `X` never refers to it,
    // whichever `m` is bound. (With the declared `X`, nothing imports `other`'s, so it's unused
    // too.)
    for (relay2, other) in [
        ("export const X: Int = 5;", vec!["5:138 UnusedConstant"]),
        ("export super::other::X;", vec![]),
    ] {
        for first_binds in [true, false] {
            let compiled = compile(&colliding_modules(
                first_binds,
                "export const X: Int = 1;",
                "export const Y: Int = 2;",
                &format!(
                    "mod relay {{ export super::both::m::*; }} mod relay2 {{ export super::relay::*; {relay2} }} mod other {{ export const X: Int = 6; }} mod user {{ import super::relay2::*; export fn value() -> Int {{ X }} }} fn main() -> Int {{ user::value() }}"
                ),
            ));
            let context = format!("{relay2}, first binds: {first_binds}");
            let mut expected = vec!["2:49 UnusedConstant", "3:48 UnusedConstant"];
            expected.extend(other.iter().copied());

            assert_eq!(unused_items(&compiled), expected, "{context}");
        }
    }
}

#[test]
fn exported_glob_imports_through_ambiguous_modules_pass_on_what_they_could_bring_in() {
    // `beta`'s `m` glob-exports `both2::k`, where only `kq`'s `k` has `ONLY`, so `user`'s glob
    // import of `both::m` could bring in `ONLY` in some orders, and its use could refer to it in
    // every order of both ambiguous names. Only the other `k`'s `Z` is unused.
    for first_binds in [true, false] {
        for second_k in ["kk", "zz"] {
            let compiled = compile(&colliding_modules(
                first_binds,
                "export const X: Int = 1;",
                "export const X: Int = 2; export super::super::both2::k::*;",
                &format!(
                    "mod kq {{ export mod k {{ export const ONLY: Int = 3; }} }} mod {second_k} {{ export mod k {{ export const Z: Int = 4; }} }} mod both2 {{ export super::kq::*; export super::{second_k}::*; }} mod user {{ import super::both::m::*; export fn value() -> Int {{ X + ONLY }} }} fn main() -> Int {{ user::value() }}"
                ),
            ));
            let context = format!("first binds: {first_binds}, second k: {second_k}");

            assert_eq!(
                unused_items(&compiled),
                ["5:102 UnusedConstant"],
                "{context}"
            );
            assert_eq!(unused_imports(&compiled), Vec::<String>::new(), "{context}");
        }
    }
}

#[test]
fn private_glob_imports_through_ambiguous_modules_pass_nothing_on() {
    // `beta`'s `m` glob-imports `both2::k` privately, where only `kq`'s `k` has `ONLY`, so
    // `user`'s glob import of `both::m` can't bring in `ONLY`, which is unused in every order of
    // both ambiguous names (`kk` sorts before `kq` and `zz` after it). Neither can it bring in
    // `beta`'s private `HIDDEN`.
    for first_binds in [true, false] {
        for second_k in ["kk", "zz"] {
            let compiled = compile(&colliding_modules(
                first_binds,
                "export const X: Int = 1;",
                "export const X: Int = 2; const HIDDEN: Int = 5; import super::super::both2::k::*;",
                &format!(
                    "mod kq {{ export mod k {{ export const ONLY: Int = 3; }} }} mod {second_k} {{ export mod k {{ export const Z: Int = 4; }} }} mod both2 {{ export super::kq::*; export super::{second_k}::*; }} mod user {{ import super::both::m::*; export fn value() -> Int {{ X + ONLY + HIDDEN }} }} fn main() -> Int {{ user::value() }}"
                ),
            ));
            let context = format!("first binds: {first_binds}, second k: {second_k}");

            assert_eq!(
                unused_items(&compiled),
                [
                    "3:66 UnusedConstant",
                    "5:46 UnusedConstant",
                    "5:102 UnusedConstant"
                ],
                "{context}"
            );
        }
    }
}

#[test]
fn names_hidden_by_named_imports_resolved_in_later_passes_are_not_passed_on() {
    // `{user}`'s named import of `A` only resolves once `kk`'s glob import has, a pass later, and
    // then hides the first module's `A` that its glob export of `both::m` could bring in. So
    // nothing can refer to that `A`, which is unused however the modules are named, even though
    // the name was still free in the passes before.
    for (user, first_binds) in [("aa", true), ("zz", true), ("aa", false), ("zz", false)] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "export const A: Int = 1;",
            "export const B: Int = 2;",
            &format!(
                "mod kk {{ export super::k5::*; }} mod k5 {{ export mod c1 {{ export const A: Int = 3; }} }} mod t {{ export const A: Int = 4; }} mod {user} {{ import super::kk::c1::A; export super::both::m::*; export fn f() -> Int {{ A }} }} import {user}::*; import t::*; fn main() -> Int {{ A + f() }}"
            ),
        ));

        assert_eq!(
            unused_items(&compiled),
            ["2:49 UnusedConstant", "3:48 UnusedConstant"],
            "user: {user}, first binds: {first_binds}"
        );
    }
}

#[test]
fn imports_used_only_by_hidden_paths_through_ambiguous_modules_are_unused() {
    // `beta`'s `m` exports `k` by name, from a chain of glob imports that resolves a few passes
    // later, which hides the `k` that its glob import of `both2::n` could bring in. So that glob
    // import is never used, whichever module either ambiguous name is bound to.
    for first_binds in [true, false] {
        for second_n in ["nb", "zb"] {
            let compiled = compile(&colliding_modules(
                first_binds,
                "export mod k { export const X: Int = 1; }",
                "export super::super::s1::k; import super::super::both2::n::*;",
                &format!(
                    "mod qr {{ export mod k {{ export const X: Int = 3; }} }} mod s1 {{ export super::s2::*; }} mod s2 {{ export super::s3::*; }} mod s3 {{ export super::qr::*; }} mod nc {{ export mod n {{ export mod k {{ export const X: Int = 4; }} export const Z: Int = 1; }} }} mod {second_n} {{ export mod n {{ export const Z: Int = 2; }} }} mod both2 {{ export super::nc::*; export super::{second_n}::*; }} import both::m::k::X; fn main() -> Int {{ X }}"
                ),
            ));
            let context = format!("first binds: {first_binds}, second n: {second_n}");

            assert_eq!(
                unused_imports(&compiled),
                ["3:94 UnusedImport"],
                "{context}"
            );
        }
    }
}

#[test]
fn glob_imports_of_parent_modules_pass_on_what_their_private_glob_imports_could_bring_in() {
    // `c` glob-imports its parent `p`, which privately glob-imports `both::m`, so `c`'s use of
    // `ONLY` could refer to `beta`'s, which only `beta`'s `m` has, whichever `m` is bound. (`p`'s
    // glob import is reported as it would be without the ambiguity: a use through a child's
    // `super::*` doesn't count for it.)
    for first_binds in [true, false] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "export const X: Int = 1;",
            "export const ONLY: Int = 2;",
            "mod p { import super::both::m::*; export mod c { import super::*; export fn value() -> Int { ONLY } } } fn main() -> Int { p::c::value() }",
        ));
        let context = format!("first binds: {first_binds}");

        assert_eq!(
            unused_items(&compiled),
            ["2:49 UnusedConstant"],
            "{context}"
        );
        assert_eq!(
            unused_imports(&compiled),
            ["5:40 UnusedImport"],
            "{context}"
        );
    }
}

#[test]
fn private_items_of_ambiguous_modules_are_used_through_paths() {
    // `beta`'s `m` has a private `X` and `T`, which a path through `both::m` finds (and reports as
    // private) when that `m` is bound, so it could refer to them either way
    for first_binds in [true, false] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "export const X: Int = 1; export type T = Int;",
            "const X: Int = 2; type T = Int;",
            "fn main() -> Int { let t: both::m::T = both::m::X; t }",
        ));

        assert_eq!(
            unused_items(&compiled),
            Vec::<String>::new(),
            "first binds: {first_binds}"
        );
    }
}

#[test]
fn private_items_of_other_modules_hide_what_their_glob_imports_could_bring_in() {
    // The first module's `m` has a private `X`, which hides the `X`s its glob export of `both2::k`
    // could bring in, so `user`'s glob import of `both::m` never brings them in, whichever `m` or
    // `k` is bound, and they're unused
    for first_binds in [true, false] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "export super::super::both2::k::*; const X: Int = 5; export const Y: Int = X;",
            "export const X: Int = 3; export const Y: Int = 3;",
            "mod ka { export mod k { export const X: Int = 1; } } mod kb { export mod k { export const X: Int = 2; } } mod both2 { export super::ka::*; export super::kb::*; } mod user { import super::both::m::*; import super::beta::m::*; export fn value() -> Int { X } } fn main() -> Int { user::value() }",
        ));
        // The first module's own `X` and `Y` and `beta`'s `Y` are unused too
        assert_eq!(
            unused_items(&compiled),
            [
                "2:76 UnusedConstant",
                "2:101 UnusedConstant",
                "3:73 UnusedConstant",
                "5:46 UnusedConstant",
                "5:99 UnusedConstant",
            ],
            "first binds: {first_binds}"
        );
    }
}

#[test]
fn uses_of_members_of_ambiguous_modules_are_not_recursive_through_the_others() {
    // The first module's `X` calls the function that uses `both::m::X`, which is only recursive if
    // `m` is bound to the first module's. The use counts as a use of the other `X` too, but isn't
    // a reference to it.
    for first_binds in [true, false] {
        let first = if first_binds { "alpha" } else { "zeta" };
        let compiled = compile(&format!(
            "
            mod {first} {{ export mod m {{ export const X: Int = super::super::value(); }} }}
            mod beta {{ export mod m {{ export const X: Int = 2; }} }}
            mod both {{ export super::{first}::*; export super::beta::*; }}
            fn value() -> Int {{ both::m::X }}
            fn main() -> Int {{ value() }}
            "
        ));
        let recursive = compiled
            .errors()
            .into_iter()
            .filter(|error| error.contains("Recursive"))
            .count();

        assert_eq!(recursive > 0, first_binds, "first binds: {first_binds}");
        assert_eq!(
            unused_items(&compiled),
            Vec::<String>::new(),
            "first binds: {first_binds}"
        );
    }
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
    // The error item of an import that isn't exported is only found from the module, so
    // `other::secret` is reported as missing from `other`
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
    // The names of the private module `x` and the private function `y` are bound to error items,
    // so neither is reported as the wrong kind of item at the uses
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
fn super_imports_of_a_failed_name_are_not_reported_again() {
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
fn glob_imports_of_super_bring_in_the_error_items_of_failed_names() {
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
            let value: m::Value = m::VALUE;
            value + 1
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

    // The imports aren't also reported as unused, and the uses of the names aren't reported again
    assert_eq!(compiled.warnings(), Vec::<String>::new());
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

#[test]
fn reexports_only_export_the_namespaces_that_are_public() {
    let compiled = compile(
        "
        export const X: Int = 1;
        type X = Int;
        type Y = Int;
        export inner::Y;

        mod inner {
            export const Y: Int = 2;
        }

        export mod a {
            export super::X;
            export super::Y;
        }

        export mod b {
            export const Z: Int = 3;
            type Z = Int;
            export Z;
        }

        fn main() -> Int {
            a::X + a::Y + b::Z
        }

        fn types(x: a::X, y: a::Y, z: b::Z) {}
        ",
    );

    // Only the public value with each name is re-exported, and nothing is reported for the
    // private types. `a`'s import is resolved before the file's `export inner::Y`, so it first
    // finds only the private type `Y`, which checks that re-exports are only reported once every
    // import is resolved.
    assert_eq!(
        compiled.errors(),
        [
            "26:24 PrivateType(\"X\", \"a\")",
            "26:33 PrivateType(\"Y\", \"a\")",
            "26:42 PrivateType(\"Z\", \"b\")",
        ]
    );
}

#[test]
fn imports_of_reported_reexports_are_not_reported_again() {
    let compiled = compile(
        "
        const SECRET: Int = 1;
        type Secret = Int;

        mod hidden {
            export const X: Int = 2;
        }

        mod z {
            export super::SECRET;
            export super::Secret;
            export super::hidden;
        }

        mod user {
            import super::z::SECRET;
            import super::z::Secret;
            import super::z::hidden::X;

            export fn get() -> Int {
                let value: Secret = SECRET + X;
                value + 1
            }
        }

        mod outer {
            mod m {
                export const Y: Int = 3;
            }

            export mod foo {
                export super::m;
            }
        }

        // The module `m` is only brought in by the glob import once its re-export is reported, so
        // the paths that name it only resolve then, and the segment after it is reported
        import outer::foo::*;
        import m::Y;
        import m::n::Z;

        fn main() -> Int {
            user::get() + Y
        }
        ",
    );

    // The imports through `z` are resolved before the re-exports are reported, when the items are
    // still private there, so this checks that they're resolved again once they're exported
    assert_eq!(
        compiled.errors(),
        [
            "10:27 PrivateReexport(\"SECRET\")",
            "11:27 PrivateReexport(\"Secret\")",
            "12:27 PrivateReexport(\"hidden\")",
            "32:31 PrivateReexport(\"m\")",
            "40:19 UndeclaredSymbol(\"n\")",
        ]
    );
}

#[test]
fn errors_in_other_imports_are_reported_once_after_reexports_are_reported() {
    let compiled = compile(
        "
        import nothing::Y;
        import X::Z;
        import other::P;
        import other::deep::Q;
        import other::T;

        mod other {
            const P: Int = 1;
            type T = Int;

            mod deep {
                export const Q: Int = 2;
            }
        }

        const X: Int = 1;

        export mod b {
            export super::X;
        }

        mod c {
            import super::b::X;

            export fn f() -> Int {
                X
            }
        }

        fn main() -> Int {
            c::f()
        }
        ",
    );

    // The imports are resolved again after the re-export is reported, which still reports the
    // errors in the other imports, but only once
    assert_eq!(
        compiled.errors(),
        [
            "2:16 UndeclaredSymbol(\"nothing\")",
            "3:16 SubpathNotSupported(\"X\")",
            "4:23 PrivateSymbol(\"P\", \"other\")",
            "5:23 PrivateSymbol(\"deep\", \"other\")",
            "6:23 PrivateType(\"T\", \"other\")",
            "20:27 PrivateReexport(\"X\")",
        ]
    );
}

#[test]
fn chains_of_private_reexports_are_reported_at_their_start() {
    let compiled = compile(
        "
        const X: Int = 1;

        export mod a {
            export super::X;

            export mod b {
                export super::X;

                export mod c {
                    export super::X;
                }
            }
        }

        export mod p {
            const Y: Int = 2;
            export Y;

            export mod c {
                export mod d {
                    export super::Y;
                }

                export super::Y;
            }
        }

        export mod q {
            export mod c {
                export super::Z;

                export mod d {
                    export super::Z;
                }
            }

            export Z;
            const Z: Int = 3;
        }

        fn main() -> Int {
            a::b::c::X + p::c::d::Y + q::c::d::Z
        }
        ",
    );

    // Each re-export after the first finds what the one before it brought in, which is exported
    // once the first is reported
    assert_eq!(
        compiled.errors(),
        [
            "5:27 PrivateReexport(\"X\")",
            "18:20 PrivateReexport(\"Y\")",
            "38:20 PrivateReexport(\"Z\")",
        ]
    );
}

#[test]
fn private_reexports_of_the_same_name_in_one_module_are_all_reported() {
    let compiled = compile(
        "
        export mod m {
            const X: Int = 1;
            const Y: Int = 2;
            export X;
            export X;
            export Y;

            export mod c {
                export super::X;
            }
        }

        fn main() -> Int {
            m::X + m::Y + m::c::X
        }
        ",
    );

    // The imports in `m` don't wait for each other, so both imports of `X` are reported, and `c`
    // waits for them
    assert_eq!(
        compiled.errors(),
        [
            "5:20 PrivateReexport(\"X\")",
            "6:20 PrivateReexport(\"X\")",
            "7:20 PrivateReexport(\"Y\")",
        ]
    );
}

#[test]
fn reexports_of_names_declared_in_the_module_are_reported_as_duplicates() {
    let compiled = compile(
        "
        const X: Int = 1;

        export mod m {
            export const X: Int = 2;
            export super::X;
        }

        fn main() -> Int {
            m::X
        }
        ",
    );

    assert_eq!(compiled.errors(), ["6:27 DuplicateSymbol(\"X\")"]);
}

#[test]
fn glob_reexports_through_super_dont_export_private_items_another_glob_bound() {
    let compiled = compile(
        "
        const SECRET: Int = 1;

        export mod a {
            import super::*;
            export super::*;

            export fn secret() -> Int {
                SECRET
            }
        }

        fn main() -> Int {
            a::secret() + a::SECRET
        }
        ",
    );

    // The private glob import binds the name first, and the exported one shares its binding,
    // which it still doesn't export
    assert_eq!(
        compiled.errors(),
        ["14:30 PrivateSymbol(\"SECRET\", \"a\")"]
    );
}

#[test]
fn imports_through_the_names_of_reported_reexports_resolve() {
    let compiled = compile(
        "
        const SECRET: Int = 1;

        mod z {
            export super::SECRET;
        }

        mod user {
            import super::z::SECRET;

            mod named {
                import super::SECRET;

                export fn f() -> Int {
                    SECRET
                }
            }

            mod globbed {
                import super::*;

                export fn f() -> Int {
                    SECRET
                }
            }

            export fn g() -> Int {
                named::f() + globbed::f()
            }
        }

        fn main() -> Int {
            user::g()
        }
        ",
    );

    // The import in `user` finds the private item, but the re-export is reported (and exports it)
    // before any failure is final, so it and the imports through `super` resolve.
    assert_eq!(compiled.errors(), ["5:27 PrivateReexport(\"SECRET\")"]);
    assert_eq!(
        compiled.reference_origins("user::named::f", "SECRET"),
        ["`SECRET`: (root)"]
    );
    assert_eq!(
        compiled.reference_origins("user::globbed::f", "SECRET"),
        ["`SECRET`: (root)"]
    );
}

#[test]
fn failures_past_imports_that_resolve_after_reexports_are_reported() {
    let compiled = compile(
        "
        mod m {
            export const A: Int = 1;
        }

        mod z {
            export super::m;
        }

        mod g {
            export super::z::*;
        }

        mod user {
            import super::z::m;
            import m::MISSING;
            import super::g::m::A;
            import A::B;

            export fn f() -> Int {
                MISSING + A + B
            }
        }

        fn main() -> Int {
            user::f()
        }
        ",
    );

    // Before the re-export is reported, `m` is private, so the imports of `m` and `A` fail. The
    // re-export is reported before any failure is final, so the imports of `m` and `A` resolve,
    // and nothing explains why the others still fail, so they're reported.
    assert_eq!(
        compiled.errors(),
        [
            "7:27 PrivateReexport(\"m\")",
            "16:23 UnresolvedImport(\"MISSING\")",
            "18:20 SubpathNotSupported(\"A\")",
        ]
    );
}

#[test]
fn uses_of_names_whose_imports_resolve_after_reexports_are_reported() {
    let compiled = compile(
        "
        mod p {
            export mod z {
                export super::M;
                type M = Int;
            }

            mod M {
                export const V: Int = 1;
            }
        }

        import p::z::M;

        fn main() -> Int {
            let a: M = 2;
            a
        }
        ",
    );

    // Before the re-export is reported, the import finds only private items. Once it's reported,
    // the import brings in the module, so the private type `M` isn't reported, and the use of `M`
    // as a type is reported like it would be if the import had never failed.
    assert_eq!(
        compiled.errors(),
        ["4:31 PrivateReexport(\"M\")", "16:20 ExpectedType(\"M\")",]
    );
}

#[test]
fn super_imports_of_a_name_whose_import_found_a_private_type_are_not_reported_again() {
    let compiled = compile(
        "
        mod m {
            type T = Int;
        }

        import m::T;

        mod a {
            import super::T;

            export fn f(x: T) -> super::T {
                x
            }
        }

        fn main() -> Int {
            let x: T = 1;
            a::f(x)
        }
        ",
    );

    assert_eq!(compiled.errors(), ["6:19 PrivateType(\"T\", \"m\")"]);
}

#[test]
fn imports_that_find_a_public_item_after_reexports_are_reported_are_not_reported_as_private() {
    for module in ["y", "zz"] {
        let compiled = compile(&format!(
            "
            import {module}::X;

            mod z {{
                fn X() -> Int {{
                    1
                }}

                export X;
            }}

            mod {module} {{
                export super::z::X;
                type X = Int;
            }}

            fn main() -> Int {{
                X()
            }}
            "
        ));

        // Once the re-export in `z` is reported, the import may first find only the private type
        // `X`, depending on the order the imports are resolved in, before the re-export of the
        // public function in `{module}`, which is then what it imports
        assert_eq!(
            compiled.errors(),
            ["9:24 PrivateReexport(\"X\")"],
            "module {module}"
        );
    }
}

#[test]
fn reexports_of_private_items_through_project_are_reported() {
    let compiled = compile(
        "
        const SECRET: Int = 1;
        const OTHER: Int = 2;
        export project::OTHER;

        export mod a {
            export super::SECRET;

            export mod b {
                export project::SECRET;
                export project::a::SECRET;
            }
        }

        fn main() -> Int {
            OTHER + a::SECRET + a::b::SECRET
        }
        ",
    );

    // `project` names the file, so the first re-export is of its own private item. `b`'s re-export
    // through `project` finds the file's private item too, but its re-export through `a` finds what
    // `a` brought in, which is exported once `a` is reported.
    assert_eq!(
        compiled.errors(),
        [
            "4:25 PrivateReexport(\"OTHER\")",
            "7:27 PrivateReexport(\"SECRET\")",
            "10:33 PrivateReexport(\"SECRET\")",
        ]
    );
}

#[test]
fn chains_of_private_reexports_through_project_globs_are_reported_at_their_start() {
    let compiled = compile(
        "
        const Y: Int = 1;
        export Y;

        export mod p {
            export mod a {
                export project::*;

                export mod d {
                    export super::Y;
                }
            }
        }

        fn main() -> Int {
            p::a::d::Y + p::a::Y
        }
        ",
    );

    // `a`'s glob import of `project` exports `Y` once the root's re-export is reported, so `d`
    // waits for it
    assert_eq!(compiled.errors(), ["3:16 PrivateReexport(\"Y\")"]);
}

#[test]
fn chains_of_private_reexports_through_globs_beside_private_globs_are_reported_at_their_start() {
    let compiled = compile(
        "
        export mod p {
            const Y: Int = 1;
            export Y;

            export mod a {
                import super::*;
                export super::*;

                export mod d {
                    export super::Y;
                }
            }
        }

        fn main() -> Int {
            p::a::d::Y + p::a::Y
        }
        ",
    );

    // The binding of `Y` in `a` is recorded on the glob import that doesn't export it, but the one
    // that does exports it once `p`'s re-export is reported, so `d` waits for it
    assert_eq!(compiled.errors(), ["4:20 PrivateReexport(\"Y\")"]);
}
