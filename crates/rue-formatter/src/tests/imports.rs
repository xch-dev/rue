use super::*;

#[test]
fn imports_are_hoisted_grouped_and_sorted() {
    check(
        "const VALUE:Int=1; import zebra; import beta::{zeta,alpha};\n\nimport delta; import charlie; fn main(){}",
        expect![[r#"
            import beta::{alpha, zeta};
            import zebra;

            import charlie;
            import delta;

            const VALUE: Int = 1;

            fn main() {}
        "#]],
    );
    check(
        "const VALUE:Int=1;\n// zebra\nimport zebra;\nimport alpha;",
        expect![[r#"
            import alpha;
            // zebra
            import zebra;

            const VALUE: Int = 1;
        "#]],
    );
}

#[test]
fn nested_import_paths_are_sorted_without_crossing_groups() {
    check(
        "import root::{zeta::{two,one},alpha,super::thing};\nimport beta::*;\n\nexport zebra::{last,first};\nexport alpha;",
        expect![[r#"
            import beta::*;
            import root::{alpha, super::thing, zeta::{one, two}};

            export alpha;
            export zebra::{first, last};
        "#]],
    );
}

#[test]
fn import_comments_move_with_their_imports() {
    check(
        "// zeta docs\nimport zeta;\n// alpha docs\nimport alpha;\n\n// beta export\nexport beta;",
        expect![[r#"
            // alpha docs
            import alpha;
            // zeta docs
            import zeta;

            // beta export
            export beta;
        "#]],
    );
}

#[test]
fn trailing_comments_stay_with_sorted_imports() {
    check(
        "import zeta; // zeta tail\n// alpha docs\nimport alpha; // alpha tail",
        expect![[r#"
            // alpha docs
            import alpha; // alpha tail
            import zeta; // zeta tail
        "#]],
    );
    check(
        "import b::b; // b\nimport a::a; // a\n\nimport super::shared::*;",
        expect![[r#"
            import a::a; // a
            import b::b; // b

            import super::shared::*;
        "#]],
    );
    check(
        "import a::a; // a\nimport super::shared::*;\nimport b::b; // b\n\n",
        expect![[r#"
            import a::a; // a
            import b::b; // b
            import super::shared::*;
        "#]],
    );
}

#[test]
fn nested_import_comments_have_stable_owners() {
    check(
        "// root docs\nimport root::{zeta, // zeta tail\n// alpha docs\nalpha};",
        expect![[r#"
            // root docs
            import root::{
                // alpha docs
                alpha,
                zeta, // zeta tail
            };
        "#]],
    );
}

#[test]
fn nested_group_banners_stay_with_the_opener() {
    check(
        "import root::{\n// group banner\n// group banner\n\n// zeta docs\nzeta,\n// alpha docs\nalpha};",
        expect![[r#"
            import root::{
                // group banner
                // group banner

                // alpha docs
                alpha,
                // zeta docs
                zeta,
            };
        "#]],
    );
}

#[test]
fn duplicate_identical_comments_are_preserved_per_import() {
    check(
        "// docs\nimport zeta;\n// docs\nimport alpha;",
        expect![[r#"
            // docs
            import alpha;
            // docs
            import zeta;
        "#]],
    );
}

#[test]
fn file_header_is_not_first_import_documentation() {
    check(
        "// file header\n\n// zeta docs\nimport zeta;\n// alpha docs\nimport alpha;",
        expect![[r#"
            // file header

            // alpha docs
            import alpha;
            // zeta docs
            import zeta;
        "#]],
    );
}

#[test]
fn absolute_and_super_import_paths_sort_canonically() {
    check(
        "import ::zeta::item;\nimport root::super::thing;\nimport ::alpha::item;",
        expect![[r#"
            import ::alpha::item;
            import ::zeta::item;
            import root::super::thing;
        "#]],
    );
}

#[test]
fn import_and_export_of_the_same_path_have_stable_order() {
    check(
        "import shared::item; // import\nexport shared::item; // export",
        expect![[r#"
            export shared::item; // export
            import shared::item; // import
        "#]],
    );
}

#[test]
fn block_comments_move_with_sorted_imports() {
    check(
        "/* zeta docs */ import zeta; /* zeta tail */\n/* alpha docs */ import alpha; /* alpha tail */",
        expect![[r#"
            /* alpha docs */ import alpha; /* alpha tail */
            /* zeta docs */ import zeta; /* zeta tail */
        "#]],
    );
}

#[test]
fn nested_comments_before_commas_move_with_their_paths() {
    check(
        "import root::{zeta /* zeta comma */,alpha /* alpha comma */};",
        expect![[r#"
            import root::{alpha, /* alpha comma */  zeta /* zeta comma */ ,};
        "#]],
    );
}

#[test]
fn comments_on_group_delimiters_remain_dangling() {
    check(
        "import root::{ // opening\nzeta,\nalpha\n/* closing */};",
        expect![[r#"
            import root::{ // opening
                alpha,
                zeta,
                /* closing */
            };
        "#]],
    );
}

#[test]
fn empty_and_single_path_import_groups_are_stable() {
    check(
        "import root::{};\nimport other::{item,};",
        expect![[r#"
            import other::{item};
            import root::{};
        "#]],
    );
}

#[test]
fn blank_lines_define_import_sorting_groups() {
    check(
        "import zeta;\nimport beta;\n\nimport alpha;\nimport gamma;",
        expect![[r#"
            import beta;
            import zeta;

            import alpha;
            import gamma;
        "#]],
    );
}

#[test]
fn deeply_nested_import_groups_sort_recursively() {
    check(
        "import root::{zeta::{three::{c,a,b},one},alpha::{last,first},middle};",
        expect![[r#"
            import root::{alpha::{first, last}, middle, zeta::{one, three::{a, b, c}}};
        "#]],
    );
}

#[test]
fn duplicate_import_paths_keep_distinct_comment_owners() {
    check(
        "// first\nimport same::item;\n// second\nimport same::item;",
        expect![[r#"
            // first
            import same::item;
            // second
            import same::item;
        "#]],
    );
}

#[test]
fn comments_that_end_a_line_before_the_closing_brace_break_the_group() {
    // Found by the property tests: the closing brace was put inside the line comment
    check(
        "import a::{b, c // line\n};\nimport d::{e // line\n};\nimport f::{g /* multi\nline */};",
        expect![[r#"
            import a::{
                b,
                c, // line
            };
            import d::{
                e, // line
            };
            import f::{
                g, /* multi
            line */
            };
        "#]],
    );
}
