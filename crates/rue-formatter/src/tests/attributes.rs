use super::*;

#[test]
fn attributes_go_on_their_own_lines() {
    check(
        "#[allow(unused_function)]   fn helper()->Int{1}\nfn main()->Int{#[ allow( unused_binding ) ] let x=1;#[deny(unused_value)]1+2;\n#[allow(constant_condition)] if true{return 1;}\n0}",
        expect![[r#"
            #[allow(unused_function)]
            fn helper() -> Int {
                1
            }

            fn main() -> Int {
                #[allow(unused_binding)]
                let x = 1;
                #[deny(unused_value)]
                1 + 2;
                #[allow(constant_condition)]
                if true {
                    return 1;
                }
                0
            }
        "#]],
    );
}

#[test]
fn long_attributes_wrap_inside_the_brackets() {
    check(
        "#[allow(unused_function, unused_parameter, unused_generic_type, unnecessary_cast, unnecessary_guard)]\nfn helper<T>(a: Int) -> Int { 1 }",
        expect![[r#"
            #[allow(
                unused_function,
                unused_parameter,
                unused_generic_type,
                unnecessary_cast,
                unnecessary_guard,
            )]
            fn helper<T>(a: Int) -> Int {
                1
            }
        "#]],
    );
}

#[test]
fn attributes_are_ignored_when_sorting_imports() {
    check(
        "import b::x;\n#[allow(unused_import)]\nimport c::*;\n#[allow(unused_import)] // why\nimport a::y;",
        expect![[r#"
            #[allow(unused_import)] // why
            import a::y;
            import b::x;
            #[allow(unused_import)]
            import c::*;
        "#]],
    );
}

#[test]
fn comments_between_attributes_are_preserved() {
    check(
        "fn main() -> Int {\n    #[allow(unused_binding)] /* why */ #[deny(unused_value)]\n    let x = 1;\n    0\n}",
        expect![[r#"
            fn main() -> Int {
                #[allow(unused_binding)] /* why */ #[deny(unused_value)]
                let x = 1;
                0
            }
        "#]],
    );
}

#[test]
fn inner_attributes_stay_above_reordered_imports() {
    check(
        "// The file's header\n\n#![allow(unused_import)]   #![deny( unused_binding )]\nimport b::x;\nimport a::{z, y};\nfn main()->Int{0}",
        expect![[r#"
            // The file's header

            #![allow(unused_import)]
            #![deny(unused_binding)]

            import a::{y, z};
            import b::x;

            fn main() -> Int {
                0
            }
        "#]],
    );
}

#[test]
fn inner_attributes_stay_below_the_header_comment() {
    check(
        "// The file's header\n#![allow(unused_import)] // why\n// The import\nimport b::x;\nimport a::y;",
        expect![[r#"
            // The file's header
            #![allow(unused_import)] // why

            import a::y;
            // The import
            import b::x;
        "#]],
    );
}

#[test]
fn files_with_only_inner_attributes() {
    check(
        "  #![allow(unused_function)]  ",
        expect![[r#"
        #![allow(unused_function)]
    "#]],
    );
}

#[test]
fn inner_attributes_at_the_start_of_modules() {
    check(
        "mod a {\n#![allow(unused_function)] // why\nfn f()->Int{1}\n}\nmod b { #![allow(unused_function)] }\nmod c { #![allow(unused_function)] #![deny(unused_binding)] import x::y; }",
        expect![[r#"
            mod a {
                #![allow(unused_function)] // why

                fn f() -> Int {
                    1
                }
            }

            mod b {
                #![allow(unused_function)]
            }

            mod c {
                #![allow(unused_function)]
                #![deny(unused_binding)]

                import x::y;
            }
        "#]],
    );
}

#[test]
fn misplaced_inner_attributes_are_not_moved() {
    check(
        "import b::x;\n#![allow(unused_import)]\nimport a::y;\nfn main()->Int{#![allow(unused_binding)] let x=1;0}",
        expect![[r#"
            import b::x;

            #![allow(unused_import)]
            import a::y;

            fn main() -> Int {
                #![allow(unused_binding)]
                let x = 1;
                0
            }
        "#]],
    );
}

#[test]
fn misplaced_inner_attributes_on_modules_stay_attached() {
    check(
        "fn g()->Int{1}\n#![allow(unused_function)]\nmod m { fn f()->Int{1} }",
        expect![[r#"
            fn g() -> Int {
                1
            }

            #![allow(unused_function)]
            mod m {
                fn f() -> Int {
                    1
                }
            }
        "#]],
    );
}

#[test]
fn long_inner_attributes_wrap_inside_the_brackets() {
    check(
        "#![allow(unused_function, unused_parameter, unused_generic_type, unnecessary_cast, unnecessary_guard)]\nmod m {\n#[allow(unused_function, unused_parameter, unused_generic_type, unnecessary_cast, unnecessary_guard)]\nmod n { #![allow(unused_function, unused_parameter, unused_generic_type, unnecessary_cast, unnecessary_guard)] }\n}",
        expect![[r#"
            #![allow(
                unused_function,
                unused_parameter,
                unused_generic_type,
                unnecessary_cast,
                unnecessary_guard,
            )]

            mod m {
                #[allow(
                    unused_function,
                    unused_parameter,
                    unused_generic_type,
                    unnecessary_cast,
                    unnecessary_guard,
                )]
                mod n {
                    #![allow(
                        unused_function,
                        unused_parameter,
                        unused_generic_type,
                        unnecessary_cast,
                        unnecessary_guard,
                    )]
                }
            }
        "#]],
    );
}
