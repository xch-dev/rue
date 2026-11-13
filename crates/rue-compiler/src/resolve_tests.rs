//! Name resolution: which declaration each name refers to, and the errors for names that don't
//! refer to anything usable.

use crate::test_utils::compile;

#[test]
fn undeclared_symbol_is_reported_where_it_is_used() {
    let compiled = compile("fn main() -> Int {\n    missing\n}\n");

    assert_eq!(compiled.errors(), ["2:5 UndeclaredSymbol(\"missing\")"]);
}

#[test]
fn undeclared_type_is_reported_where_it_is_used() {
    let compiled = compile("fn main(value: Missing) -> Int {\n    1\n}\n");

    assert_eq!(compiled.errors(), ["1:16 UndeclaredType(\"Missing\")"]);
}

#[test]
fn every_undeclared_name_is_reported() {
    let compiled = compile("fn main() -> Int {\n    first + second\n}\n");

    assert_eq!(
        compiled.errors(),
        [
            "2:5 UndeclaredSymbol(\"first\")",
            "2:13 UndeclaredSymbol(\"second\")",
        ]
    );
}

#[test]
fn items_can_be_used_before_they_are_declared() {
    let mut compiled = compile(
        "
        fn main() -> Int {
            later(LIMIT)
        }

        fn later(value: Int) -> Int {
            value
        }

        const LIMIT: Int = 10;
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(compiled.symbol_type_name("later"), "fn(value: Int) -> Int");
    assert_eq!(compiled.reference_types("main", "LIMIT"), ["`LIMIT`: Int"]);
}

#[test]
fn bindings_shadow_parameters_from_the_next_statement() {
    let mut compiled = compile(
        "
        fn main(value: Int) -> Bool {
            let value = value > 0;
            value
        }
        ",
    );

    compiled.assert_no_errors();

    // The value of the binding still refers to the parameter, and the body to the binding
    assert_eq!(
        compiled.reference_types("main", "value"),
        ["`value`: Int", "`value`: Bool"]
    );
}

#[test]
fn bindings_are_not_visible_outside_of_their_block() {
    let compiled = compile(
        "
        fn main() -> Int {
            let outer = {
                let inner = 1;
                inner
            };
            outer + inner
        }
        ",
    );

    assert_eq!(compiled.errors(), ["7:21 UndeclaredSymbol(\"inner\")"]);
}

#[test]
fn bindings_are_not_visible_in_their_own_value() {
    let compiled = compile(
        "
        fn main() -> Int {
            let value = value + 1;
            value
        }
        ",
    );

    assert_eq!(compiled.errors(), ["3:25 UndeclaredSymbol(\"value\")"]);
}

#[test]
fn duplicate_items_are_reported() {
    let compiled = compile(
        "
        fn main() -> Int {
            1
        }

        fn main() -> Int {
            2
        }

        type Alias = Int;
        type Alias = Bytes;
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "6:12 DuplicateSymbol(\"main\")",
            "11:14 DuplicateType(\"Alias\")",
        ]
    );
}

#[test]
fn symbols_and_types_have_separate_namespaces() {
    let mut compiled = compile(
        "
        type value = Int;

        fn value(input: value) -> value {
            input
        }

        fn main() -> Int {
            value(1)
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(
        compiled.symbol_type_name("value"),
        "fn(input: value) -> value"
    );
}

#[test]
fn types_cannot_be_used_as_values() {
    let compiled = compile(
        "
        type Alias = Int;

        fn main() -> Int {
            Alias
        }
        ",
    );

    assert_eq!(compiled.errors(), ["5:13 ExpectedSymbol(\"Alias\")"]);
}

#[test]
fn values_cannot_be_used_as_types() {
    let compiled = compile(
        "
        const VALUE: Int = 1;

        fn main(value: VALUE) -> Int {
            1
        }
        ",
    );

    assert_eq!(compiled.errors(), ["4:24 ExpectedType(\"VALUE\")"]);
}

#[test]
fn modules_cannot_be_used_as_values() {
    let compiled = compile(
        "
        mod inner {}

        fn main() -> Int {
            inner
        }
        ",
    );

    assert_eq!(compiled.errors(), ["5:13 CannotReferenceModule(\"inner\")"]);
}

#[test]
fn only_modules_have_paths() {
    let compiled = compile(
        "
        const VALUE: Int = 1;

        fn main() -> Int {
            VALUE::inner
        }
        ",
    );

    // The error names the symbol that has no paths, but is reported at the segment after it
    assert_eq!(compiled.errors(), ["5:20 SubpathNotSupported(\"VALUE\")"]);
}

#[test]
fn paths_resolve_exported_items_of_modules() {
    let mut compiled = compile(
        "
        const VALUE: Int = 0;

        mod outer {
            export const VALUE: Int = 1;

            export mod inner {
                export const VALUE: Int = 42;
                export type Number = Int;
            }
        }

        fn main() -> outer::inner::Number {
            outer::inner::VALUE + outer::VALUE + VALUE
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(
        compiled.reference_origins("main", "VALUE"),
        [
            "`outer::inner::VALUE`: outer::inner",
            "`outer::VALUE`: outer",
            "`VALUE`: (root)",
        ]
    );
    assert_eq!(
        compiled.reference_types("main", "VALUE"),
        [
            "`outer::inner::VALUE`: Int",
            "`outer::VALUE`: Int",
            "`VALUE`: Int",
        ]
    );
}

#[test]
fn private_items_cannot_be_referenced_through_a_path() {
    let compiled = compile(
        "
        mod private {
            type Type = nil;

            fn function() -> Type {
                nil
            }
        }

        fn main() -> private::Type {
            private::function()
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "10:31 PrivateType(\"Type\", \"private\")",
            "11:22 PrivateSymbol(\"function\", \"private\")",
        ]
    );
}

#[test]
fn private_modules_cannot_be_referenced_through_a_path() {
    let compiled = compile(
        "
        mod outer {
            mod hidden {
                export const VALUE: Int = 1;
            }
        }

        fn main() -> Int {
            outer::hidden::VALUE
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        ["9:20 PrivateSymbol(\"hidden\", \"outer\")"]
    );
}

#[test]
fn modules_can_use_the_items_of_their_parents() {
    let compiled = compile(
        "
        const TOP: Int = 1;

        mod inner {
            export fn value() -> Int {
                TOP
            }
        }

        fn main() -> Int {
            inner::value()
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(
        compiled.reference_origins("inner::value", "TOP"),
        ["`TOP`: (root)"]
    );
}

#[test]
fn module_items_shadow_the_items_of_their_parents() {
    let compiled = compile(
        "
        const VALUE: Int = 1;

        mod inner {
            const VALUE: Int = 2;

            export fn value() -> Int {
                VALUE
            }
        }

        fn main() -> Int {
            inner::value() + VALUE
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(
        compiled.reference_origins("inner::value", "VALUE"),
        ["`VALUE`: inner"]
    );
    assert_eq!(
        compiled.reference_origins("main", "VALUE"),
        ["`VALUE`: (root)"]
    );
}

#[test]
fn bindings_shadow_items() {
    let compiled = compile(
        "
        const VALUE: Int = 1;

        fn main() -> Int {
            let before = VALUE;
            let VALUE = 2;
            before + VALUE
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(
        compiled.reference_origins("main", "VALUE"),
        ["`VALUE`: (root)", "`VALUE`: local"]
    );
}

#[test]
fn generic_parameters_are_types_in_their_function() {
    let mut compiled = compile(
        "
        fn identity<T>(value: T) -> T {
            value
        }

        fn main() -> Int {
            identity(1)
        }
        ",
    );

    compiled.assert_no_errors();
    assert_eq!(
        compiled.symbol_type_name("identity"),
        "fn<T>(value: T) -> T"
    );
}

#[test]
fn generic_parameters_are_not_visible_outside_of_their_function() {
    let compiled = compile(
        "
        fn identity<T>(value: T) -> T {
            value
        }

        fn main(value: T) -> Int {
            1
        }
        ",
    );

    assert_eq!(compiled.errors(), ["6:24 UndeclaredType(\"T\")"]);
}

#[test]
fn constants_cannot_refer_to_themselves() {
    let compiled = compile(
        "
        const A: Int = B + 1;
        const B: Int = A + 1;

        fn main() -> Int {
            A
        }
        ",
    );

    assert_eq!(
        compiled.errors(),
        [
            "2:15 RecursiveConstant(\"A\")",
            "3:15 RecursiveConstant(\"B\")",
        ]
    );
}
