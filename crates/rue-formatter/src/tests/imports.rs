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
            import root::{alpha, /* alpha comma */ zeta /* zeta comma */,};
        "#]],
    );
    // An inline comment before a comma is followed by the comma, and one after a comma by a single
    // space
    check(
        "import a::{b, c /* x */,};\nimport x::{a, /* c */ b};\nimport z::{\n    c /* x */,\n    b, /* y */\n    a,\n};",
        expect![[r#"
            import a::{b, c /* x */,};
            import x::{a, /* c */ b};
            import z::{
                a,
                b, /* y */
                c /* x */,
            };
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
fn imports_are_sorted_by_their_sorted_groups() {
    // Found by the property tests: imports were sorted by their source text, so `o::{s, n}` went
    // after `o::{p}`, and then before it once its group was sorted
    check(
        "import o::{s, n};\nimport o::{p};\nimport x::{y::{z, a}, y::{m}};",
        expect![[r#"
            import o::{n, s};
            import o::{p};
            import x::{y::{a, z}, y::{m}};
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
fn comments_followed_by_a_blank_line_at_the_start_of_an_import_group() {
    // Found by the property tests: this panicked with an index out of bounds
    check(
        "import a::{\n// line\n\nb};\nimport c::{\n/* multi\nline */\n\nd};",
        expect![[r#"
            import a::{
                // line

                b,
            };
            import c::{
                /* multi
            line */

                d,
            };
        "#]],
    );
}

#[test]
fn multiline_comments_before_the_closing_brace_after_a_trailing_comment() {
    // Found by the property tests: each format added another blank line before `/* multi`
    check(
        "import a::{b /* trailing */\n/* multi\nline */\n};",
        expect![[r#"
            import a::{
                b, /* trailing */
                /* multi
            line */
            };
        "#]],
    );
}

#[test]
fn comments_before_the_first_path_start_its_line() {
    // Found by the property tests: on the same line as `{`, the comment would belong to it
    check(
        "import a::{\n/* block */ b};",
        expect![[r#"
            import a::{
                /* block */ b,
            };
        "#]],
    );
}

#[test]
fn comments_before_a_path_sorted_to_the_start_of_a_group() {
    // Found by the property tests: comments separated from the first path by a blank line belong
    // to `{`, so the blank lines are removed when sorting makes their path the first one
    check(
        "import a::{f,\n// own-line comment\n\ne};\nimport b::{z,\n/* first */\n\n// second\ny};",
        expect![[r#"
            import a::{
                // own-line comment
                e,
                f,
            };
            import b::{
                /* first */
                // second
                y,
                z,
            };
        "#]],
    );
    // And a comment before the first path stays on its own line when sorting moves the path after
    // another one, where it would otherwise follow that path's comma
    check(
        "import m::{\n/* block */lx,endkkh};",
        expect![[r#"
            import m::{
                endkkh,
                /* block */ lx,
            };
        "#]],
    );
}

#[test]
fn comments_inside_paths_in_nested_groups() {
    // Found by the property tests: a comment inside a path that's inside an unnamed group was
    // checked against either the path or the group, whichever came first in a hash map
    check(
        "import a::{{b/* block */::c}};\nimport d::{{::/* block */e}};\nimport f::{{g// line\n::h}};",
        expect![[r#"
            import a::{{b /* block */ ::c}};
            import d::{{:: /* block */ e}};
            import f::{
                {
                    g // line
                    ::h,
                },
            };
        "#]],
    );
}

#[test]
fn comments_before_the_closing_brace_after_sorting() {
    // Found by the property tests: the line break before `// own-line` was kept with `b`'s
    // trailing comment, so once sorting made `c` the last path, the comment followed it
    check(
        "import a::{c, b // trailing\n// own-line\n};",
        expect![[r#"
            import a::{
                b, // trailing
                c,
                // own-line
            };
        "#]],
    );
    // The blank line after `c`'s trailing comment and the line before the closing comment were
    // both kept, giving two blank lines
    check(
        "import a::{c, // line\n\n// own-line\nb\n/* multi\nline */\n};",
        expect![[r#"
            import a::{
                // own-line
                b,
                c, // line

                /* multi
            line */
            };
        "#]],
    );
    check(
        "import a::{\n    c, // trailing\n\n    // own-line\n    b,\n    // closing\n};",
        expect![[r#"
            import a::{
                // own-line
                b,
                c, // trailing

                // closing
            };
        "#]],
    );
}

#[test]
fn comments_moved_after_a_trailing_comment_by_sorting_stay_on_the_next_line() {
    // The line break before `// own` and the one after `// t` were both kept, adding a blank line
    check(
        "import a::{a, // t\nz,\n// own\nb};",
        expect![[r#"
            import a::{
                a, // t
                // own
                b,
                z,
            };
        "#]],
    );
}

#[test]
fn sorting_paths_keeps_no_blank_line_after_a_trailing_comment() {
    // Found by the property tests: the blank line before `/* multi` stayed after `// line` once
    // sorting moved `c` between them, and the next format removed it
    check(
        "import d::{c, a, // line\n\n/* multi\nline */\nd};",
        expect![[r#"
            import d::{
                a, // line
                c,
                /* multi
            line */
                d,
            };
        "#]],
    );
    // Without sorting, the blank line stays before the comment
    check(
        "import a::{b, // line\n\n// own-line\nc};",
        expect![[r#"
            import a::{
                b, // line

                // own-line
                c,
            };
        "#]],
    );
}

#[test]
fn sorting_moves_no_blank_lines_into_an_import_group() {
    // Found by the property tests: the blank line after `line */` started a new group of imports
    // on the second format, which added another blank line before the comment
    check(
        "import b;\n/* multi\nline */\n\nimport k;\nimport h; /* trailing */\nimport z;\nimport a; // trailing\n\n// doc\nimport q;",
        expect![[r#"
            import b;

            import a; // trailing
            import h; /* trailing */
            /* multi
            line */
            import k;
            import z;

            // doc
            import q;
        "#]],
    );
}

#[test]
fn comments_moved_after_a_trailing_comment_by_sorting_add_no_blank_line() {
    // Found by the property tests: `// doc` started a line of its own after the line ended by
    // `// trailing`, which left a blank line between them that the next format kept
    check(
        "import b;\n// doc\nimport k;\nimport h; // trailing\n\nfn main() {}",
        expect![[r#"
            import b;
            import h; // trailing
            // doc
            import k;

            fn main() {}
        "#]],
    );
}

#[test]
fn comments_that_end_a_line_before_the_closing_brace_break_the_group() {
    // Found by the property tests: the closing brace was put inside the line comment
    check(
        "import a::{b, c // line\n};\nimport d::{e // line\n};\nimport f::{g /* multi\nline */};\nimport h::{i, j::{k // line\n}};\nimport l::{m, n /* block */\n};",
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
            import h::{
                i,
                j::{
                    k, // line
                },
            };
            import l::{
                m,
                n, /* block */
            };
        "#]],
    );
}

#[test]
fn sorting_adds_no_blank_line_before_closing_comments() {
    // The line break after `// t` and the one before `// closing` were both kept once sorting
    // made `b` the last path, adding a blank line
    check(
        "import x::{b, // t\na\n// closing\n};",
        expect![[r#"
            import x::{
                a,
                b, // t
                // closing
            };
        "#]],
    );
    // Found by the property tests: the blank line after `/* block */` and the one before
    // `// line comment` were both kept, giving two blank lines
    check(
        "import a::{{ivar,/* block */  \n\n// own-line comment\nhe\n\n\n // line comment\n}};",
        expect![[r#"
            import a::{
                {
                    // own-line comment
                    he,
                    ivar, /* block */

                    // line comment
                },
            };
        "#]],
    );
}

#[test]
fn sorting_keeps_comments_at_the_end_of_the_file_on_their_own_line() {
    // The line break after `// t` and the one before `// footer` were both kept, adding a blank
    // line
    check(
        "import b; // t\nimport a;\n// footer",
        expect![[r#"
            import a;
            import b; // t
            // footer
        "#]],
    );
    // The line break before `// footer` was kept with `// t`, so once sorting moved it away, the
    // footer followed `import b;` on the same line
    check(
        "import b;\nimport a; // t\n// footer",
        expect![[r#"
            import a; // t
            import b;
            // footer
        "#]],
    );
}
