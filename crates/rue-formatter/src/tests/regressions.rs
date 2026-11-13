use super::*;

#[test]
fn nested_modules_format_mixed_item_kinds() {
    check(
        "mod outer{const Z:Int=2;mod inner{type Pair<T>=(T,T);fn make(x:Int){x}}struct Value{item:Int}}",
        expect![[r#"
            mod outer {
                const Z: Int = 2;
                mod inner {
                    type Pair<T> = (T, T);
                    fn make(x: Int) {
                        x
                    }
                }
                struct Value {
                    item: Int,
                }
            }
        "#]],
    );
}

#[test]
fn item_modifier_combinations_have_canonical_spacing() {
    check(
        "export inline const VALUE:Int=1; export test fn verify(){assert true;} export extern fn host(x:Int)->Int from \"host.hex\";",
        expect![[r#"
            export inline const VALUE: Int = 1;

            export test fn verify() {
                assert true;
            }

            export extern fn host(x: Int) -> Int from "host.hex";
        "#]],
    );
}

#[test]
fn nested_destructuring_bindings_format_recursively() {
    check(
        "fn unpack(value:Any){let ([first,...rest],{left:right,...tail})=value;[first,right,...rest]}",
        expect![[r#"
            fn unpack(value: Any) {
                let ([first, ...rest], { left: right, ...tail }) = value;
                [first, right, ...rest]
            }
        "#]],
    );
}

#[test]
fn struct_defaults_spreads_and_initializers_are_distinct() {
    check(
        "struct Config<T>{value:T=default,count=1,...rest:T} fn make(value:Int){Config::<Int>{count:2,value}}",
        expect![[r#"
            struct Config<T> {
                value: T = default,
                count = 1,
                ...rest: T,
            }

            fn make(value: Int) {
                Config::<Int> { count: 2, value }
            }
        "#]],
    );
}

#[test]
fn pair_and_group_expressions_keep_their_shape() {
    check(
        "fn pairs(a:Int,b:Int){let grouped=(a+b);let pair=(a,b,);[(grouped),pair]}",
        expect![[r#"
            fn pairs(a: Int, b: Int) {
                let grouped = (a + b);
                let pair = (a, b);
                [(grouped), pair]
            }
        "#]],
    );
}

#[test]
fn postfix_chains_format_after_struct_and_list_expressions() {
    check(
        "fn chain(value:Int){Wrapper{value}.field(value).next as Int is Int}",
        expect![[r#"
            fn chain(value: Int) {
                Wrapper { value }.field(value).next as Int is Int
            }
        "#]],
    );
}

#[test]
fn prefix_operators_remain_unambiguous_next_to_binary_operators() {
    check(
        "fn operators(a:Int,b:Int){a+-b;a--b;a*!b;!!a==!b}",
        expect![[r#"
            fn operators(a: Int, b: Int) {
                a + -b;
                a - -b;
                a * !b;
                !!a == !b
            }
        "#]],
    );
}

#[test]
fn lambdas_support_generics_spreads_and_destructuring() {
    check(
        "fn make(){fn<T>(...[first,...rest]:[T,...T]):T=>first}",
        expect![[r#"
            fn make() {
                fn<T>(...[first, ...rest]: [T, ...T]): T => first
            }
        "#]],
    );
}

#[test]
fn nested_function_pair_and_union_types_format_consistently() {
    check(
        "type Handler<T>=fn(value:T,rest:List<T>)->(T|nil,fn(item:T)->T);",
        expect![[r#"
            type Handler<T> = fn(value: T, rest: List<T>) -> (T | nil, fn(item: T) -> T);
        "#]],
    );
}

#[test]
fn empty_and_value_statements_keep_required_semicolons() {
    check(
        "fn statements(value:Int){inline let copy=value;debug copy;assert true;raise;return;}",
        expect![[r#"
            fn statements(value: Int) {
                inline let copy = value;
                debug copy;
                assert true;
                raise;
                return;
            }
        "#]],
    );
}

#[test]
fn absolute_generic_paths_and_shift_expressions_do_not_conflict() {
    check(
        "fn paths<T>(value:Int){::root::Type::<T>::make(value);value<<2>1}",
        expect![[r#"
            fn paths<T>(value: Int) {
                ::root::Type::<T>::make(value);
                value << 2 > 1
            }
        "#]],
    );
}

/// Each layout choice holds a flat and a broken variant of its contents, so without sharing
/// them the document doubled in size with each level of nesting. The depth is low enough that
/// a regression fails the time check rather than hanging the test.
#[test]
fn deeply_nested_expressions_format_quickly() {
    let depth = 24;
    for (open, close) in [("f(", ")"), ("[", "]"), ("(", ")"), ("f(1 == ", ")")] {
        let source = format!(
            "fn f(x: Any) -> Any {{ x }}\nfn main() -> Any {{ {}1{} }}",
            open.repeat(depth),
            close.repeat(depth)
        );
        let started = std::time::Instant::now();
        let result = format_source(&source, &FormatOptions::default());
        assert!(result.is_ok(), "{open}...{close}: {result:?}");
        assert!(
            started.elapsed() < std::time::Duration::from_secs(5),
            "{open}...{close} took {:?}",
            started.elapsed()
        );
    }
}
