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
