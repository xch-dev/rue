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

/// Two modules each export a module `m`, which `both` glob exports from both of them, so `both::m`
/// is ambiguous. The first one's `m` has `first_items`, and `beta`'s has `second_items`. The rest
/// is `rest`. The first module is `alpha` if `first_binds`, and otherwise `omega`, so that either
/// glob import binds `m` (and the names have the same length, so the warnings are at the same
/// places).
fn colliding_modules(
    first_binds: bool,
    first_items: &str,
    second_items: &str,
    rest: &str,
) -> String {
    let first = if first_binds { "alpha" } else { "omega" };

    format!(
        "
        mod {first} {{ export mod m {{ {first_items} }} }}
        mod beta {{ export mod m {{ {second_items} }} }}
        mod both {{ export super::{first}::*; export super::beta::*; }}
        {rest}
        "
    )
}

/// The warnings about unused items (rather than imports).
fn unused_items(compiled: &Compiled) -> Vec<String> {
    compiled
        .warnings()
        .into_iter()
        .filter(|warning| warning.contains(" Unused") && !warning.ends_with("UnusedImport"))
        .collect()
}

#[test]
fn paths_through_ambiguous_modules_use_everything_in_each() {
    // Whichever glob import bound `m`, a path or import through it counts everything in each
    // module it could refer to as used, including what's in the modules inside of them, so none of
    // it is reported, and neither is the import. That includes `ONLY`, which only `beta`'s `m`
    // has, and `T`, which only the first module's has (the path to each fails when `m` is bound to
    // the other), and `UNUSED`, which nothing uses.
    let first_items = "export const X: Int = 1; export type T = Int; export const UNUSED: Int = 3; export mod n { export const Y: Int = 1; }";
    let second_items = "export const X: Int = 2; export const ONLY: Int = 4; export mod n { export const Y: Int = 2; export fn g() -> Int { 1 } }";

    for user in [
        "fn main() -> Int { both::m::X }",
        "fn main() -> Int { let t: both::m::T = 1; t }",
        "fn main() -> Int { both::m::n::Y }",
        "fn main() -> Int { both::m::ONLY }",
        "import both::m::X; fn main() -> Int { X }",
        "import both::m::*; fn main() -> Int { X }",
        "import both::m::n::Y; fn main() -> Int { Y }",
        "import both::*; fn main() -> Int { m::X }",
        "mod mid { export super::both::m::*; } fn main() -> Int { mid::X }",
    ] {
        for first_binds in [true, false] {
            let compiled = compile(&colliding_modules(
                first_binds,
                first_items,
                second_items,
                user,
            ));
            let context = format!("{user}, first binds: {first_binds}");

            assert!(
                compiled
                    .errors()
                    .iter()
                    .all(|error| error.contains("AmbiguousImport") || error.contains("Undeclared")),
                "{context}: {:?}",
                compiled.errors()
            );
            assert_eq!(unused_items(&compiled), Vec::<String>::new(), "{context}");
            assert_eq!(unused_imports(&compiled), Vec::<String>::new(), "{context}");
        }
    }
}

#[test]
fn imports_through_ambiguous_modules_are_used() {
    // An import through `both::m` is used even if nothing uses what it brings in, in either order
    for user in [
        "import both::m::X; fn main() -> Int { 1 }",
        "import both::m::*; fn main() -> Int { 1 }",
    ] {
        for first_binds in [true, false] {
            let compiled = compile(&colliding_modules(
                first_binds,
                "export const X: Int = 1;",
                "export const X: Int = 2;",
                user,
            ));
            let context = format!("{user}, first binds: {first_binds}");

            assert_eq!(unused_items(&compiled), Vec::<String>::new(), "{context}");
            assert_eq!(unused_imports(&compiled), Vec::<String>::new(), "{context}");
        }
    }
}

#[test]
fn ambiguous_modules_that_nothing_goes_through_are_unused_in_each() {
    // Nothing goes through `both::m`, so what's in each `m` is still reported, in either order, and
    // so is what's in a module that a path through an unambiguous name goes to
    for first_binds in [true, false] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "export const X: Int = 1;",
            "export const X: Int = 2;",
            "mod other { export const Y: Int = 3; export const Z: Int = 4; } fn main() -> Int { other::Y }",
        ));

        assert_eq!(
            unused_items(&compiled),
            [
                "2:49 UnusedConstant",
                "3:48 UnusedConstant",
                "5:59 UnusedConstant",
            ],
            "first binds: {first_binds}"
        );
    }
}

#[test]
fn what_ambiguous_modules_only_import_privately_is_still_unused() {
    // The first module's `m` privately glob-imports `lib`, which a path through `both::m` can't
    // reach, so `lib`'s items are still reported in either order, and so is the glob import
    for first_binds in [true, false] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "import super::super::lib::*; export const X: Int = 1;",
            "export const X: Int = 2;",
            "mod lib { export const DEAD: Int = 1; export mod sub { export const DEEP: Int = 2; } } fn main() -> Int { both::m::X }",
        ));
        let context = format!("first binds: {first_binds}");

        assert_eq!(
            unused_items(&compiled),
            ["5:32 UnusedConstant", "5:77 UnusedConstant"],
            "{context}"
        );
        assert_eq!(
            unused_imports(&compiled),
            ["2:62 UnusedImport"],
            "{context}"
        );
    }
}

#[test]
fn glob_imports_through_ambiguous_modules_that_bring_in_nothing_are_used() {
    // The first module's `m` is empty, so the glob import brings in nothing when `m` is bound to
    // it, but it goes through an ambiguous name, so it's used in either order
    for first_binds in [true, false] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "",
            "export const X: Int = 2;",
            "mod user { export super::both::m::*; } fn main() -> Int { 1 }",
        ));

        assert_eq!(
            unused_imports(&compiled),
            Vec::<String>::new(),
            "first binds: {first_binds}"
        );
    }
}

#[test]
fn paths_through_ambiguous_modules_at_several_segments_use_everything_in_each() {
    // `both::m` could be `{outer}::m` or `gamma::m`, and in `{outer}::m`, `n` could be
    // `{first}::n` or `beta::n`, so every `Y` is used, whichever glob imports bound the names
    for outer in ["aaa", "zzz"] {
        for first in ["alpha", "omega"] {
            let compiled = compile(&format!(
                "
                mod {first} {{ export mod n {{ export const Y: Int = 1; }} }}
                mod beta {{ export mod n {{ export const Y: Int = 2; }} }}
                mod {outer} {{ export mod m {{ export super::super::{first}::*; export super::super::beta::*; }} }}
                mod gamma {{ export mod m {{ export mod n {{ export const Y: Int = 3; }} }} }}
                mod both {{ export super::{outer}::*; export super::gamma::*; }}
                fn main() -> Int {{ both::m::n::Y }}
                "
            ));

            assert_eq!(
                unused_items(&compiled),
                Vec::<String>::new(),
                "outer: {outer}, first: {first}"
            );
        }
    }
}

#[test]
fn uses_through_ambiguous_modules_are_not_recursive_through_the_others() {
    // The first module's `X` calls the function that uses `both::m::X`, which is only recursive if
    // `m` is bound to the first module's. Everything in the other `m` counts as used, but isn't
    // referenced.
    for first_binds in [true, false] {
        let compiled = compile(&colliding_modules(
            first_binds,
            "export const X: Int = super::super::value();",
            "export const X: Int = 2;",
            "fn value() -> Int { both::m::X } fn main() -> Int { value() }",
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
