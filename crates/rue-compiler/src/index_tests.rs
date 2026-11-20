//! An index (`value[index]`) or slice (`value[start..end]`) compiles to the `substr` call that it
//! stands for, so each one here is compared with the call it should generate.

use std::fmt::Write;

use rue_options::CompilerOptions;

use crate::test_utils::{compile, compile_with};

const ITEMS: &str = "
struct Cursor { data: Bytes, offset: Int }
const N: Int = 3;
const M: Int = N + 2;
const K: Int = double(N) + 1;
const HELLO_N: Bytes = 'hello'[N];
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
    compiled.main_program()
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

/// Compares what the optimizer makes of both, which is the same even if they're written
/// differently.
#[track_caller]
fn check_optimized(parameters: &str, index: &str, substr: &str) {
    let options = CompilerOptions::default();
    assert_eq!(
        program(parameters, index, options),
        program(parameters, substr, options)
    );
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

/// The end of a constant index is a constant too, whatever the index is made of, so one isn't
/// added to it at runtime.
#[test]
fn constant_index() {
    check("a: Bytes", "a[N]", "substr(a, N, 4)");
    check("a: Bytes", "a[M]", "substr(a, M, 6)");
    check("a: Bytes", "a[K]", "substr(a, K, 8)");
    check("a: Bytes", "a[N + 1]", "substr(a, N + 1, 5)");
    check("a: Bytes", "a[M - N]", "substr(a, M - N, 3)");
    check("a: Bytes", "a[-N + 4]", "substr(a, -N + 4, 2)");
    check("a: Bytes", "a[N * 2]", "substr(a, N * 2, 7)");
    check("a: Bytes", "a[M / 2]", "substr(a, M / 2, 3)");
    check("a: Bytes", "a[M % 2]", "substr(a, M % 2, 2)");
    check("a: Bytes", "a[N + 32 / 2]", "substr(a, N + 32 / 2, 20)");
    check("a: Bytes", "a[32 / 2 + N]", "substr(a, 32 / 2 + N, 20)");
    check("a: Bytes", "a[32 / 2 - N]", "substr(a, 32 / 2 - N, 14)");
    check(
        "a: Bytes",
        "a[N + \"ab\".length]",
        "substr(a, N + \"ab\".length, 6)",
    );
    check("a: Bytes", "a[{ N }]", "substr(a, { N }, 4)");
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

/// The same goes for an index that's made of literals with operators.
#[test]
fn literal_operator_index() {
    check("a: Bytes", "a[2 * 3]", "substr(a, 2 * 3, 7)");
    check("a: Bytes", "a[32 / 2]", "substr(a, 32 / 2, 17)");
    check("a: Bytes", "a[1 << 2]", "substr(a, 1 << 2, 5)");
    check(
        "a: Bytes",
        "a[\"ab\".length]",
        "substr(a, \"ab\".length, 3)",
    );
    check("a: Bytes", "a[0x05 as Int]", "substr(a, 0x05 as Int, 6)");
    // Bytes are a signed integer
    check("a: Bytes", "a[0xFF as Int]", "substr(a, 0xFF as Int, 0)");
    check(
        "a: Bytes",
        "a[0x0080 as Int]",
        "substr(a, 0x0080 as Int, 129)",
    );

    check_optimized("a: Bytes", "a[2 * 3]", "substr(a, 6, 7)");
    check_optimized("a: Bytes", "a[32 / 2]", "substr(a, 16, 17)");
    check_optimized("", "\"hello\"[2 * 2]", "\"o\"");
    check_optimized("", "\"hello\"[8 / 2]", "\"o\"");
}

/// A constant that's added to or subtracted from a variable is folded into the end.
#[test]
fn constant_offset_index() {
    check("a: Bytes, i: Int", "a[i + N]", "substr(a, i + N, i + 4)");
    check("a: Bytes, i: Int", "a[N + i]", "substr(a, N + i, i + 4)");
    check("a: Bytes, i: Int", "a[i - N]", "substr(a, i - N, i - 2)");
    check(
        "a: Bytes, i: Int",
        "a[i + M / 2]",
        "substr(a, i + M / 2, i + 3)",
    );
    check(
        "a: Bytes, i: Int",
        "a[i + 2 * 3]",
        "substr(a, i + 2 * 3, i + 7)",
    );
}

/// A constant index that can't be evaluated when compiling is left to fail at runtime, as it
/// does in the call, rather than being an error.
#[test]
fn failing_constant_index() {
    check("a: Bytes", "a[1 / 0]", "substr(a, 1 / 0, 1 / 0 + 1)");
    check("a: Bytes", "a[N / 0]", "substr(a, N / 0, N / 0 + 1)");
    check(
        "a: Bytes, i: Int",
        "a[i + N / 0]",
        "substr(a, i + N / 0, i + (N / 0 + 1))",
    );
}

/// The same goes for an index that costs too much to evaluate when compiling.
#[test]
fn costly_constant_index() {
    let source = |body: &str| {
        format!(
            "fn count(n: Int) -> Int {{ if n == 0 {{ 0 }} else {{ 1 + count(n - 1) }} }}\n\
             const K: Int = count(1000);\n\
             fn main(a: Bytes) -> Bytes {{ {body} }}\n"
        )
    };
    let program = |body: &str| {
        compile_with(&source(body), |o| {
            o.std = false;
            o.const_eval_max_cost = 10_000;
        })
        .main_program()
    };

    assert_eq!(program("a[K]"), program("substr(a, K, K + 1)"));
}

/// A constant index inside of a const expression or a constant is evaluated with it, and one
/// that fails is still an error of the const expression that's written.
#[test]
fn constant_index_in_const() {
    check_optimized("", "const { \"hello\"[N + 1] }", "\"o\"");
    check_optimized("", "HELLO_N", "\"l\"");

    let source = "const N: Int = 3;\nfn main() -> Bytes { const { \"hello\"[N / 0] } }\n";
    assert_eq!(
        compile(source).errors(),
        [r#"2:22 ConstEvalFailed("Division by zero")"#]
    );
}

/// A constant index has a constant end wherever it's written.
#[test]
fn constant_index_in_functions() {
    let source = |body: &str| {
        format!(
            "const N: Int = 3;\n\
             fn generic<T>(a: Bytes, value: T) -> (Bytes, T) {{ ({body}, value) }}\n\
             fn main(a: Bytes) -> Bytes {{\n    \
             let lambda = fn(b: Bytes) => {body} + b;\n    \
             generic(a, 1).first + lambda(a)\n}}\n"
        )
    };

    assert_eq!(
        compile(&source("a[N * 2]")).main_program(),
        compile(&source("substr(a, N * 2, 7)")).main_program()
    );
}

/// A constant can be declared after the code that indexes with it.
#[test]
fn constant_declared_later() {
    let source = "
fn main(a: Bytes) -> Bytes { a[LATER] }
const LATER: Int = EVEN_LATER + 1;
const EVEN_LATER: Int = const { 1 + 1 };
";
    assert_eq!(compile(source).main_program(), "(substr 2 (q . 3) (q . 4))");
}

/// A long chain of constants is followed without overflowing the stack.
#[test]
fn long_constant_chain() {
    let mut source = String::from("const C0: Int = 1;\n");
    for i in 1..20_000 {
        writeln!(source, "const C{i}: Int = C{};", i - 1).unwrap();
    }
    source.push_str("fn main(a: Bytes) -> Bytes { a[C19999] }\n");

    compile(&source).assert_no_errors();
}

/// An `inline` binding stands for its value wherever it's used, so it's repeated like any other
/// binding, as it is in the call.
#[test]
fn inline_index() {
    check(
        "a: Bytes, i: Int",
        "inline let j = double(i); a[j]",
        "inline let j = double(i); substr(a, j, j + 1)",
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
