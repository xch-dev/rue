//! An index (`value[index]`) or slice (`value[start..end]`) compiles to the `substr` call that it
//! stands for, so each one here is compared with the call it should generate.

use std::path::PathBuf;

use clvmr::Allocator;
use rue_clvm::disassemble_program;
use rue_diagnostic::SourceKind;
use rue_options::CompilerOptions;

use crate::test_utils::compile_with;

const ITEMS: &str = "
struct Cursor { data: Bytes, offset: Int }
const N: Int = 3;
const M: Int = N + 2;
fn double(value: Int) -> Int { value * 2 }
";

/// The program of `main` with the given parameters and body, after the shared items.
fn program(parameters: &str, body: &str, options: CompilerOptions) -> String {
    let source = format!("{ITEMS}\nfn main({parameters}) -> Bytes {{ {body} }}");
    let mut compiled = compile_with(&source, |o| {
        *o = CompilerOptions {
            std: false,
            ..options
        };
    });
    compiled.assert_no_errors();

    let mut allocator = Allocator::new();
    let kind = SourceKind::File("main.rue".to_string());
    let main = compiled
        .tree
        .main(&mut compiled.ctx, &mut allocator, &kind, PathBuf::from("."))
        .unwrap()
        .expect("the source has a `main` function");
    disassemble_program(&allocator, main)
}

#[track_caller]
fn check(parameters: &str, index: &str, substr: &str) {
    for options in [CompilerOptions::default(), CompilerOptions::debug()] {
        assert_eq!(
            program(parameters, index, options),
            program(parameters, substr, options),
            "{options:?}"
        );
    }
}

#[test]
fn literal_index() {
    check("a: Bytes", "a[0]", "substr(a, 0, 1)");
    check("a: Bytes", "a[7]", "substr(a, 7, 8)");
    check("a: Bytes", "a[-1]", "substr(a, -1, 0)");
    check("a: Bytes", "a[(2 as Int)]", "substr(a, 2, 3)");
}

/// A variable, parameter or field is repeated for the end, as it would be written by hand.
#[test]
fn variable_index() {
    check("a: Bytes, i: Int", "a[i]", "substr(a, i, i + 1)");
    check(
        "c: Cursor",
        "c.data[c.offset]",
        "substr(c.data, c.offset, c.offset + 1)",
    );
    check(
        "a: Bytes, i: Int",
        "let j = i * 2; a[j]",
        "let j = i * 2; substr(a, j, j + 1)",
    );
}

/// A literal that's added to or subtracted from a variable is folded into the end.
#[test]
fn offset_index() {
    check("a: Bytes, i: Int", "a[i + 1]", "substr(a, i + 1, i + 2)");
    check("a: Bytes, i: Int", "a[1 + i]", "substr(a, 1 + i, i + 2)");
    check("a: Bytes, i: Int", "a[i - 1]", "substr(a, i - 1, i)");
    check("a: Bytes, i: Int", "a[i - 3]", "substr(a, i - 3, i - 2)");
    check("a: Bytes, i: Int", "a[i + -1]", "substr(a, i + -1, i)");
    check(
        "c: Cursor",
        "c.data[c.offset + 2]",
        "substr(c.data, c.offset + 2, c.offset + 3)",
    );
}

/// Anything else is bound, so that it's only evaluated once.
#[test]
fn computed_index() {
    check(
        "a: Bytes, i: Int",
        "a[double(i)]",
        "let index = double(i); substr(a, index, index + 1)",
    );
    check(
        "a: Bytes, i: Int",
        "a[i * 2]",
        "let index = i * 2; substr(a, index, index + 1)",
    );
    check(
        "a: Bytes",
        "a[a.length - 1]",
        "let index = a.length - 1; substr(a, index, index + 1)",
    );
    check(
        "a: Bytes, i: Int, j: Int",
        "a[i + j]",
        "let index = i + j; substr(a, index, index + 1)",
    );
}

/// The end of a constant index is a literal, whatever the constant is made of.
#[test]
fn constant_index() {
    check("a: Bytes", "a[N]", "substr(a, N, 4)");
    check("a: Bytes", "a[M]", "substr(a, M, 6)");
    check("a: Bytes", "a[N + 1]", "substr(a, N + 1, 5)");
    check("a: Bytes", "a[M - N]", "substr(a, M - N, 3)");
    check("a: Bytes", "a[-N + 4]", "substr(a, -N + 4, 2)");
    check(
        "a: Bytes",
        "a[const { double(N) }]",
        "substr(a, const { double(N) }, 7)",
    );
    check(
        "a: Bytes",
        "a[N] + a[N]",
        "substr(a, N, 4) + substr(a, N, 4)",
    );
}

#[test]
fn slices() {
    check("a: Bytes, i: Int, j: Int", "a[i..j]", "substr(a, i, j)");
    check("a: Bytes, i: Int", "a[i..]", "substr(a, i)");
    check("a: Bytes, i: Int", "a[..i]", "substr(a, 0, i)");
    check("a: Bytes", "a[1..3]", "substr(a, 1, 3)");
    check("a: Bytes", "a[N..M]", "substr(a, N, M)");
    check(
        "a: Bytes, i: Int",
        "a[double(i)..double(i) + 1]",
        "substr(a, double(i), double(i) + 1)",
    );
}

#[test]
fn folded_operands() {
    check("", "\"hello\"[1]", "substr(\"hello\", 1, 2)");
    check("", "\"hello\"[1..3]", "substr(\"hello\", 1, 3)");
    check("", "\"hello\"[..2]", "substr(\"hello\", 0, 2)");
    check("", "\"hello\"[3..]", "substr(\"hello\", 3)");
    check("", "\"hello\"[N]", "substr(\"hello\", N, 4)");
    check("", "\"hello\"[9]", "substr(\"hello\", 9, 10)");
}

#[test]
fn operand_types() {
    check(
        "s: String, h: Bytes32, k: PublicKey, o: Bytes | nil",
        "s[0] + h[..4] + (k as Bytes)[1] + o[2..]",
        "substr(s, 0, 1) + substr(h, 0, 4) + substr(k as Bytes, 1, 2) + substr(o, 2)",
    );
}

#[test]
fn nested() {
    check(
        "a: Bytes, i: Int",
        "a[1..][i] + (a + a)[i..][..2] + a[a[0] as Int]",
        "substr(substr(a, 1), i, i + 1) + substr(substr(a + a, i), 0, 2) \
         + { let index = substr(a, 0, 1) as Int; substr(a, index, index + 1) }",
    );
}
